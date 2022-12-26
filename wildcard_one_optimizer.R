#Load Packages I need
library(tidyverse)
library(worldfootballR)
library(rvest)
library(xml2)
library(httr)
library(stringi)
library(jsonlite)
library(janitor)
library(fastDummies)

#read in epl team data I need with fbref
#league table
epl_league <- fb_big5_advanced_season_stats(season_end_year = 2023, stat_type = 'standard', team_or_player = 'team')
epl_league <- epl_league %>% 
  filter(Comp == 'Premier League')

#player stats
epl_players <- fb_big5_advanced_season_stats(season_end_year = 2023, stat_type = 'standard', team_or_player = 'player')
epl_players <- epl_players %>% 
  filter(Comp == 'Premier League')


#fetch xGA for each team
epl_xGA <- epl_league %>% 
  filter(Team_or_Opponent == 'opponent') %>% 
  select(Squad,xG_Expected, xG_Per)

#soccer scoring follows poissant dist... can use this to predict shutout probability
#build quick loop using poissant dist to get chance of each team getting shutout
epl_xGA$shutout_prob <- unlist(map(epl_xGA$xG_Per,~dpois(0,.x)))
epl_xGA$proj_shutouts <- epl_xGA$shutout_prob*38


#need to also calculate chances of 2 or more goals 
epl_xGA$two_or_more <- unlist(map(epl_xGA$xG_Per,~dpois(2,.x) + dpois(3,.x) + dpois(4,.x)))

#rename certain squads to matchup with fantasy pricing team names
epl_xGA$short_name <- c('ARS','AVL','BOU','BRE','BHA','CHE','CRY','EVE','FUL', 'LEE','LEI',
                        'LIV','MCI','MUN','NEW','NFO', 'SOU','TOT', 'WHU','WOL')

#Rename squads to full
epl_players$Squad[epl_players$Squad=='Arsenal'] = 'ARS'
epl_players$Squad[epl_players$Squad=='Aston Villa'] = 'AVL'
epl_players$Squad[epl_players$Squad=='Brentford'] = 'BRE'
epl_players$Squad[epl_players$Squad=='Brighton'] = 'BHA'
epl_players$Squad[epl_players$Squad=='Chelsea'] = 'CHE'
epl_players$Squad[epl_players$Squad=='Crystal Palace'] = 'CRY'
epl_players$Squad[epl_players$Squad=='Everton'] = 'EVE'
epl_players$Squad[epl_players$Squad=='Leeds United'] = 'LEE'
epl_players$Squad[epl_players$Squad=='Leicester City'] = 'LEI'
epl_players$Squad[epl_players$Squad=='Liverpool'] = 'LIV'
epl_players$Squad[epl_players$Squad=='Manchester City'] = 'MCI'
epl_players$Squad[epl_players$Squad=='Manchester Utd'] = 'MUN'
epl_players$Squad[epl_players$Squad=='Newcastle Utd'] = 'NEW'
epl_players$Squad[epl_players$Squad=='Southampton'] = 'SOU'
epl_players$Squad[epl_players$Squad=='Tottenham'] = 'TOT'
epl_players$Squad[epl_players$Squad=='West Ham'] = 'WHU'
epl_players$Squad[epl_players$Squad=='Wolves'] = 'WOL'
epl_players$Squad[epl_players$Squad=="Nott'ham Forest"] = 'NFO'
epl_players$Squad[epl_players$Squad=='Fulham'] = 'FUL'
epl_players$Squad[epl_players$Squad=='Bournemouth'] = 'BOU'                                    


#Create rowmatch in epl_table to merge with player prices
epl_players$RowMatch <- rep(1:nrow(epl_players))



### Load Pricing Data ###

epl_api <- 'https://fantasy.premierleague.com/api/bootstrap-static/' %>% 
  GET(config = httr::config(ssl_verifypeer = FALSE))

epl_api <-fromJSON(rawToChar(epl_api$content))
fantasy_stats <- epl_api$elements
fantasy_teams <- epl_api$teams %>% 
  select(id,name,short_name)

#select only columns I need lol
fantasy_stats <- merge(fantasy_stats, fantasy_teams, by.x = 'team', by.y = 'id', all.x = TRUE)

fantasy_stats <- fantasy_stats %>% 
  select(web_name,first_name,second_name, name, short_name, element_type, now_cost, minutes,goals_scored,assists ,clean_sheets,yellow_cards, red_cards, saves, bonus)
#Add position column using element_type
fantasy_stats <- fantasy_stats %>% 
  mutate(Position = ifelse(element_type==1,'GKP',
                           ifelse(element_type==2,'DEF',
                                  ifelse(element_type==3,'MID','FWD'))))

#divide cost by 10 cuz it's bugging me
fantasy_stats$now_cost <- fantasy_stats$now_cost/10

#Match player names in prices to epl table... def not gonna be perfect. This returns row number or row numbers we have last naem match
fantasy_stats$RowMatch <- map2(fantasy_stats$web_name,fantasy_stats$short_name,~ which(grepl(.x, epl_players$Player) & .y == epl_players$Squad)[1])

mean(is.na(fantasy_stats$RowMatch))
#48% of players still don't have row match

#some it's because of name differences
#357 / 638 means most priced players don't even have fbref stats in (maybe they haven't seen field?)

#fetch players that do have price linked
cleaned_players <- fantasy_stats %>% 
  filter(RowMatch >0)

#Merge with inner join
final_players = merge(epl_players, cleaned_players, by = 'RowMatch')


missing_players <- epl_players %>% 
  filter(!Player %in% final_players$Player)



#addressn big names in missing players
#manually add tomas soucek cuz his first name was also weird
fantasy_stats$RowMatch[fantasy_stats$web_name == 'Soucek'] <- 444
fantasy_stats$RowMatch[fantasy_stats$web_name == 'N.Williams'] <- 373
fantasy_stats$RowMatch[fantasy_stats$web_name == 'Van Dijk'] <- 283
fantasy_stats$RowMatch[fantasy_stats$web_name == 'Fabianski'] <- 434
fantasy_stats$RowMatch[fantasy_stats$web_name == 'Gündogan'] <- 294


# Time ot optimize!

#merge xGA onto them
final_player_stats <- merge(final_players,epl_xGA,by.x = 'Squad', by.y = 'short_name', all.x = TRUE)

#stats need to be used to predict remaining 24 games... they hve played 14 games. Do 24/14 = 1.71. Use this to predict remaining szn
#use xG, xA, shutout probability using xGA, use their actual minutes they played, actual yellow and red cards
final_player_stats <- final_player_stats %>% 
  mutate(Proj_Pnts = ifelse(Position == 'DEF' | Position == 'GKP',
                            (xG_Expected.x*6 + xAG_Expected*3 + shutout_prob*Mins_Per_90_Playing*4 +
                              Starts_Playing*1 + Mins_Per_90_Playing*1 - CrdY*1 - CrdR*3 - two_or_more*Mins_Per_90_Playing*2+
                              saves*(1/3) + bonus)*1.71,
                            ifelse(Position == 'MID',
                                   (xG_Expected.x*5 + xAG_Expected*3 + shutout_prob*Mins_Per_90_Playing*1 +
                                     Starts_Playing*1 + Mins_Per_90_Playing*1 - CrdY*1 - CrdR*3 + bonus)*1.71,
                                   (xG_Expected.x*4 + xAG_Expected*3 + Starts_Playing*1 + 
                                     Mins_Per_90_Playing*1 - CrdY*1 - CrdR*3 + bonus)*1.71)))

#For my optimization- I need to extract player name, team, price, projected points

final_players_projections <- final_player_stats %>% 
  select(web_name,short_name,Position,now_cost, Proj_Pnts)

#make POS column dummy variables for the optimization to work
#add a column with 1 values just to help 15 player constraint
final_players_projections <- dummy_cols(final_players_projections, select_columns = 'Position')
final_players_projections$Player_Count <- 1

#drop NA points and players we know more info on
final_players_projections <- drop_na(final_players_projections)

#DROP Arsenal players cuz it tried to give me 6!
final_players_projections$Proj_Pnts[final_players_projections$web_name == 'White' & final_players_projections$short_name== 'ARS'] <- 0
final_players_projections$Proj_Pnts[final_players_projections$web_name == 'Saliba'] <- 0
final_players_projections$Proj_Pnts[final_players_projections$web_name == 'Saka'] <- 0
final_players_projections$Proj_Pnts[final_players_projections$web_name == 'Schär'] <- 0



library(lpSolve)

#create constraints
cons<- c(final_players_projections$now_cost,final_players_projections$Position_GKP, final_players_projections$Position_DEF,final_players_projections$Position_FWD, final_players_projections$Player_Count)
#create matrix and double it for both sides of constraints
con <- matrix(cons, nrow=5, byrow=TRUE)
allcons <- rbind(con, con)
#set max and mins
maxrhs <- c(99.3,2,5,3,15) 
minrhs <- c(0,2,5,3,14)
maxrel <- c("<=","<=","<=","<=","<=")
minrel <- c(">=", ">=",">=",">=",">=")
#all final variables
obj <- final_players_projections$Proj_Pnts
rel <- c(maxrel,minrel)
rhs <- c(maxrhs, minrhs)
mylp<- lp("max",obj,allcons,rel,rhs,all.bin=TRUE)
#creating table just for the players that are in optimal solution
my_lineup <- final_players_projections[which(mylp$solution==1),]
sum(my_lineup$now_cost)


#dropped 2 West Ham players cuz i needed to... plus Zouma is injured
final_lineup <- my_lineup[!(my_lineup$web_name=='Bowen' | my_lineup$web_name=='Zouma'),]
added_players <- final_players_projections[final_players_projections$web_name=='Martinelli' | final_players_projections$web_name=='Dalot',]

final_lineup <- rbind(final_lineup,added_players)
