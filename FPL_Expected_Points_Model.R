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
epl_league <- fb_big5_advanced_season_stats(season_end_year = 2022, stat_type = 'standard', team_or_player = 'team')
epl_league <- epl_league %>% 
  filter(Comp == 'Premier League')

epl_table = load_fb_big5_advanced_season_stats(team_or_player = 'player', stat_type = 'standard', season_end_year = 2022)
epl_table = epl_table %>% 
  filter(Comp == 'Premier League')

#Now I have it filtered down to EPL players and have their key xG/xA assists, in total and per 90
#I have several questions I can answer using 2020/2021 data to test on how good it was at predicting 2021/2022 data

### Key Tests to Run ###
# What is best predictor of goals scored for future season? Is it their xG amount or actual goal amount? Is it their xGper 90 times their previous season minutes played
# Same thing with assists, do i use assists or xA?
# For minutes played/starts, do I just use their starts and minutes played? Or is there a better way to calculate their future starts and minutes
#v^^^ maybe I can use the minutes pct for position group they are in. Ideally I want players who took up a big chunk of their team's positon group
# For saves, use their 2021 save stats? Or use fbref goalie stats to come up with better prediction of saves (ex saves to shot ratio)
# I will tackle team defense with different data

# First Q- What is best predictor or goals scored for future season?
# Pull my 2020/2021 data to use to check with 21/22 data
epl_table_20 = load_fb_big5_advanced_season_stats(team_or_player = 'player', stat_type = 'standard', season_end_year = 2021)
epl_table_20 = epl_table_20 %>% 
  filter(Comp == 'Premier League')

#Check how good goals does to predict 21/22 goals
#Merge 2020 table onto 21/22 table
combined_epl = merge(epl_table, epl_table_20, by = 'Player')
goal_diff <- sqrt(mean((combined_epl$Gls.x - combined_epl$Gls.y)^2))
xG_diff <- sqrt(mean((combined_epl$Gls.x - combined_epl$xG_Expected.y)^2))
#xG is slightly better with avg of 2.7 goals off instead of 2.8

# Let's check assists
assist_diff <- sqrt(mean((combined_epl$Ast.x - combined_epl$Ast.y)^2))
xA_diff <- sqrt(mean((combined_epl$Ast.x - combined_epl$xA_Expected.y)^2))
#Again xA slightly better with avg of 1.87 off instead of 2.2 off

#How does this hold with new players to the EPL? I can at least check with players coming from other big5 leagues
#Merge whole big5 table2020 from 2020 onto 21/22 epl players
euro_players <- load_fb_big5_advanced_season_stats(team_or_player = 'player', stat_type = 'standard', season_end_year = 2021)
newcomers = merge(epl_table, euro_players, by = 'Player')
newcomers = newcomers %>% 
  filter(Comp.y != 'Premier League')

assist_diff_new <- sqrt(mean((newcomers$Ast.x - newcomers$Ast.y)^2)) #2.86 off
xA_diff_new <- sqrt(mean((newcomers$Ast.x - newcomers$xA_Expected.y)^2)) #2.18 off

goal_diff_new <- sqrt(mean((newcomers$Gls.x - newcomers$Gls.y)^2)) # 5.3 off
xG_diff_new <- sqrt(mean((newcomers$Gls.x - newcomers$xG_Expected.y)^2)) # 4.6 off

# are newcomers from europe underperofrming?
newcomers$Ast.x - newcomers$xA_Expected.y

#I also need to get championship players, ex Fulham and their squad


#Ok let's just build the expected points end product to get it working, using xG and xA... then I can work from backwards to finetune it
#Pull FPL player list from FPL site... probs gonna need to webscrape

#Join team xGA info to each player
epl_xGA <- epl_league %>% 
  filter(Team_or_Opponent == 'opponent') %>% 
  select(Squad,xG_Expected, xG_Per)

#get data from champ squads too
#ugh just gonna rbind data cuz its being a pain
#fulham xgper90 .96 | bournmouth xgoer90 1.02 | nott xgper90 1.22
#check previous years to find best multiplier to nerf champ teams
#brentfrod .87 1.29 haha | watford 1 1.75 xg per | norwich 1.14 2.03
#1.48 + 1.75 + 1.78 (the three multipliers) and average them = 1.67
#multiply newly promoted sides by 1.67
epl_xGA[nrow(epl_xGA)+1,] <- c('Fulham', 60.8, 1.6)
epl_xGA[nrow(epl_xGA)+1,] <- c('Bournemouth', 64.6, 1.7)
epl_xGA[nrow(epl_xGA)+1,] <- c("Nott'm Forest", 77.5, 2.04)
epl_xGA$xG_Per <- as.numeric(epl_xGA$xG_Per)
#build quick loop using poissant dist to get chance of each team getting shutout
epl_xGA$shutout_prob <- unlist(map(epl_xGA$xG_Per,~dpois(0,.x)))
epl_xGA$proj_shutouts <- epl_xGA$shutout_prob*38

dpois(2,.71) + dpois(3,.71)
#need to also calculate chances of 2 or more goals 
epl_xGA$two_or_more <- unlist(map(epl_xGA$xG_Per,~dpois(2,.x) + dpois(3,.x) + dpois(4,.x)))
  
#rename certain squads to matchup with fantasy pricing team names
epl_xGA$short_name <- c('ARS','AVL','BRE','BHA','BUR','CHE','CRY','EVE','LEE','LEI',
                        'LIV','MCI','MUN','NEW','NOR', 'SOU','TOT','WAT', 'WHU','WOL','FUL',
                        'BOU','NFO','NFO')

#Rename squads to full
epl_table$Squad[epl_table$Squad=='Arsenal'] = 'ARS'
epl_table$Squad[epl_table$Squad=='Aston Villa'] = 'AVL'
epl_table$Squad[epl_table$Squad=='Brentford'] = 'BRE'
epl_table$Squad[epl_table$Squad=='Brighton'] = 'BHA'
epl_table$Squad[epl_table$Squad=='Chelsea'] = 'CHE'
epl_table$Squad[epl_table$Squad=='Crystal Palace'] = 'CRY'
epl_table$Squad[epl_table$Squad=='Everton'] = 'EVE'
epl_table$Squad[epl_table$Squad=='Leeds United'] = 'LEE'
epl_table$Squad[epl_table$Squad=='Leicester City'] = 'LEI'
epl_table$Squad[epl_table$Squad=='Liverpool'] = 'LIV'
epl_table$Squad[epl_table$Squad=='Manchester City'] = 'MCI'
epl_table$Squad[epl_table$Squad=='Manchester Utd'] = 'MUN'
epl_table$Squad[epl_table$Squad=='Newcastle Utd'] = 'NEW'
epl_table$Squad[epl_table$Squad=='Southampton'] = 'SOU'
epl_table$Squad[epl_table$Squad=='Tottenham'] = 'TOT'
epl_table$Squad[epl_table$Squad=='West Ham'] = 'WHU'
epl_table$Squad[epl_table$Squad=='Wolves'] = 'WOL'


#Create rowmatch in epl_table to merge with player prices
epl_table$RowMatch <- rep(1:nrow(epl_table))

#Clean player_prices so it only has players with ONE value in their name
cleaned_players <- player_prices %>% 
  filter(RowMatch>0)

#Merge with inner join
final_players = merge(epl_table, cleaned_players, by = 'RowMatch')

#Now I can do the math to assign point values 
final_players <- final_players %>% 
  mutate(Projected_Points_xG = xG_Expected*5+ xA_Expected*3 + shutout_prob*minutesP90*4 +
           saves*(1/3) + starts*1 + minutes/60*1 + bonuspoints - yellowcards*1 - redcards*3 -
           (xGA/2)*(minutes/3420),
Projected_Points_Goals = Gls*5 + Ast*3)

#Ok so I have 344 out of the 519
#oooof, more than 150 players missing
#Uploading the three championship sides will help a little

#try and scrape champ data I guess, maybe look at that epl

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

#ok merge again I guess


#try and get the url of every player in epl
team_urls <- tm_league_team_urls(country_name = "England", start_year = 2022)

data_list <- list()
for (team in 1:length(team_urls)) {
  data_list[[team]] <- tm_squad_stats(team_urls[team])
}

tm_urls <- tm_squad_stats("https://www.transfermarkt.com/afc-bournemouth/startseite/verein/989/saison_id/2022")
tm_urls <- tm_team_player_urls("https://www.transfermarkt.com/afc-bournemouth/startseite/verein/989/saison_id/2022")
tm_players <- player_dictionary_mapping()

epl_players_2022 <- bind_rows(data_list)
#link fbref url to player list
which(tm_urls$player_url[1] == tm_players$UrlTmarkt)
tm_players[c('UrlFBref', 'UrlTmarkt')]

#merge player_urls onto tm_urls with LEFT join
epl_players_2022 <- merge(epl_players_2022, tm_players[c('UrlFBref', 'UrlTmarkt')],
                          by.x = 'player_url', by.y = 'UrlTmarkt', all.x = TRUE
)

sum(is.na(epl_players_2022$UrlFBref))
#70 players don't have an fbref url :\
#let's return the list of those 70 players
bad_players <- epl_players_2022 %>% 
  filter(is.na(UrlFBref))


#Match player names in prices to epl table... def not gonna be perfect. This returns row number or row numbers we have last naem match
fantasy_stats$RowMatch <- map2(fantasy_stats$web_name,fantasy_stats$short_name,~ which(grepl(.x, epl_table$Player) & .y == epl_table$Squad)[1])

#Create rowmatch in epl_table to merge with player prices
epl_table$RowMatch <- rep(1:nrow(epl_table))

#Clean player_prices so it only has players with ONE value in their name
cleaned_players <- fantasy_stats %>% 
  filter(RowMatch>0)

#Merge with inner join
final_players = merge(epl_table, cleaned_players, by = 'RowMatch')

#get players NOT in final_players
left_out_players <- epl_players_2022 %>% 
  filter(! UrlFBref %in% final_players$Url)

#239 players missing final stats... let's make a dent in this

#start with retreieving player data for euro big 5 using fbref urls we have on file
#this should get us players like haaland, etc, along with players who switched teams... raheem sterling
euro_players <- load_fb_big5_advanced_season_stats(team_or_player = 'player', stat_type = 'standard', season_end_year = 2022)
big_5_players <- euro_players %>% 
  filter(Url %in% left_out_players$UrlFBref)
# 101 big 5 players! That's huge!
#See how many we can get prices on
#All euro players and players thats wtiched teams should match up, as long as spelling isn't weird

big_5_prices <- fantasy_stats[is.na(fantasy_stats$RowMatch),]
big_5_prices$RowMatch<- map(big_5_prices$web_name,~ which(grepl(.x, big_5_players$Player))[1])

which(grepl('Kovacic', 	'Mateo Kovačić'))
#Create rowmatch in epl_table to merge with player prices
big_5_players$RowMatch <- rep(1:nrow(big_5_players))

#Clean player_prices so it only has players with ONE value in their name
big_5_prices <- big_5_prices %>% 
  filter(RowMatch>0)

new_adds <- merge(big_5_players, big_5_prices, by = 'RowMatch')
#these are all peeps on new teams this season ^^^  
#42 players match on name... yikes... so we had 101 url matches, is there another way to match players up?

#Fetch remaining left out players
left_out_players <- left_out_players %>% 
  filter(! UrlFBref %in% new_adds$Url)

#Plays like Gundogan just are'nt in cuz of spelling, so might do weird spellings manually? Or match up some other way if their team is same
#Then we will need to get three promoted sides involved... will probs iterate through their urls and retrieve basic stat info?
#Some players are in final clean set but appears they aren't because of name differences like Arsenal's Gabriel

meh <- fb_player_season_stats('https://fbref.com/en/players/20b104bc/Adam-Smith', stat_type = 'standard')

#let's manually do the three promoted teams instead of this lol
promoted <- fantasy_stats %>% 
  filter(name %in% c('Bournemouth', "Nott'm Forest", 'Fulham'))

get_season_team_stats(country = 'ENG', gender = 'M', season_end_year = '2022', tier = '2nd', stat_type = 'standard')


#fetch all of the player urls for nott bourn and ful
new_promos <- epl_players_2022 %>% 
  filter(team_name %in% c('AFC Bournemouth', 'Nottingham Forest', 'Fulham FC'))

#call me crazy but gonna try and scrape new promo stats cuz it feels easier than sifting through urls
mean(is.na(new_promos$UrlFBref))

bourn <- read_html('https://fbref.com/en/squads/4ba7cbea/Bournemouth-Stats') %>% 
  html_table(fill = TRUE)

fulham <- read_html('https://fbref.com/en/squads/fd962109/Fulham-Stats') %>% 
  html_table(fill = TRUE)
fulham <- fulham[[1]] %>% 
  row_to_names(1)
nott <- read_html('https://fbref.com/en/squads/e4a775cb/Nottingham-Forest-Stats') %>% 
  html_table(fill = TRUE)
nott <- nott[[1]] %>% 
  row_to_names(1)

bourn <- bourn[[1]] %>% 
  row_to_names(1)

new_promos <- bind_rows(bourn,fulham,nott)

#tag prices onto these dataframes!
new_promo_prices <- fantasy_stats %>% 
  filter(name %in% c('Bournemouth', "Nott'm Forest", 'Fulham'))

new_promos$RowMatch <- rep(1:nrow(new_promos))



new_promo_prices$RowMatch<- map(new_promo_prices$web_name,~ which(grepl(.x, new_promos$Player))[1])

#Clean player_prices so it only has players with ONE value in their name
new_promo_prices <- new_promo_prices %>% 
  filter(RowMatch>0)

promoted_projections <- merge(new_promos, new_promo_prices, by = 'RowMatch')  

#Gonna use real goals and assists for new promoted cuz that's what I have available 
#Need to calculate the 

#Players with no rowmatch
missing_players <- fantasy_stats %>% 
  filter(is.na(RowMatch))

#adjust after taking into account euro matches like haaland
missing_players <- missing_players %>% 
  filter(! web_name %in% new_adds$web_name)

#adjust aftter new promo sides
missing_players <- missing_players %>% 
  filter(! web_name %in% promoted_projections$web_name)

#Only 86 missing players! Not bad- tha's like 4 per team
#we are gonna try to match based on first name and squad
#this should give us matches for gundogan and soucek, van dijk, etc

missing_players$RowMatch <- map2(missing_players$first_name,missing_players$short_name,~ which(grepl(.x, epl_table$Player) & .y == epl_table$Squad))

#that did pretty well! 

#manually add tomas soucek cuz his first name was also weird
missing_players$RowMatch[missing_players$web_name == 'Soucek'] <- 517
missing_players$RowMatch[missing_players$web_name == 'A.Armstrong'] <- 417
missing_players$RowMatch[missing_players$web_name == 'Fabio Silva'] <- 544
missing_players$RowMatch[missing_players$web_name == 'Fabianski'] <- 506
missing_players$RowMatch[missing_players$web_name == 'Gündogan'] <- 315


#merge these players with epl_table
new_adds2 <- merge(epl_table, missing_players, by = 'RowMatch')

#get new missing player list
missing_players <- missing_players %>% 
  filter(is.na(RowMatch > 0))

#lets see if they have fbref urls or not
missing_players$RowMatch <- map2(missing_players$web_name,missing_players$name,~ which(grepl(.x, epl_players_2022$player_name) & grepl(.y, epl_players_2022$team_name)))

epl_players_2022$RowMatch <- rep(1:nrow(epl_players_2022))

last_players <- merge(epl_players_2022, missing_players, by = 'RowMatch')
#ok only 9 players have urls there... not the move


#Let's see how much less teams score in expected goals on newly promoted sides

#2019/2020

#2020/2021
#total goals not xg 
# leeds goals 1.67 1.63 x.976 | wba 1.67 .92 x.55 | ful 1.39 .71 x.51

#2021/22 
#Norwich goals for 1.72 .95 x.55 | Brentford 1.65 1.38 x.836 | Wat 1.47 1.18 x.803
#All 3 teams underperformed xG- Nor 13 Bretn 4 Wat 11 #to be fiar most teams underperformed
#multiplier of .73

#convert to numeric
#convetr minutes column to deal with commas
promoted_projections$Min <- gsub(",", "",promoted_projections$Min)

promoted_projections[,7:21] <- as.numeric(unlist(promoted_projections[,7:21]))

unlist(promoted_projections[,7:21])

#add new column that is player pct of total minutes played: minutes/(46*90)
promoted_projections <- promoted_projections %>% 
  mutate(MinutesPct = Min/4140)

#attach xGA for each of the three teams
promoted_projections <- merge(promoted_projections,epl_xGA, by.x= 'name', by.y = 'Squad', all.x = TRUE)

#set three forward values  to xG and xA
#mitrovic xgper90 .84
#Solanke .71 xgper90 | .09
#Johnson .3 xgper90 | xA .15
promoted_projections$Gls...16[promoted_projections$web_name == 'Mitrović']<- .84
promoted_projections$Gls...16[promoted_projections$web_name == 'Johnson']<- .3
promoted_projections$Gls...16[promoted_projections$web_name == 'Solanke']<- .71
promoted_projections$Ast...17[promoted_projections$web_name == 'Mitrović']<- .12
promoted_projections$Ast...17[promoted_projections$web_name == 'Johnson']<- .15
promoted_projections$Ast...17[promoted_projections$web_name == 'Solanke']<- .09


#calculate points for champ sides! Use .73 multiplier for assists and goals
promoted_projections <- promoted_projections %>% 
  mutate(Proj_Pnts = ifelse(Position == 'DEF' | Position == 'GKP',
    38*MinutesPct*Gls...16*6*.73 + 38*MinutesPct*Ast...17*3*.73 + shutout_prob*38*MinutesPct*4 +
      Starts/1.21*1 + MinutesPct*38*1 - CrdY*1/1.21 - CrdR*3/1.21 - two_or_more*38*MinutesPct*2,
    ifelse(Position == 'MID',
    38*MinutesPct*Gls...16*5*.55 + 38*MinutesPct*Ast...17*3*.55 + shutout_prob*38*MinutesPct*1 +
      Starts/1.21*1 + MinutesPct*38*1 - CrdY*1/1.21 - CrdR*3/1.21,
    38*MinutesPct*Gls...16*4*.55 + 38*MinutesPct*Ast...17*3*.55 + Starts/1.21*1 + 
      MinutesPct*38*1 - CrdY*1/1.21 - CrdR*3/1.21)))

promoted_projections$Proj_Pnts <- round(promoted_projections$Proj_Pnts,4)


#Now let's do same thing for rest of players!
final_players_projections <- bind_rows(final_players,new_adds, new_adds2)

#merge xGA onto them
final_players_projections <- merge(final_players_projections,epl_xGA, by= 'short_name', all.x = TRUE)

final_players_projections <- final_players_projections %>% 
  mutate(Proj_Pnts = ifelse(Position == 'DEF' | Position == 'GKP',
    xG_Expected.x*6 + xA_Expected*3 + shutout_prob*Mins_Per_90_Playing*4 +
    Starts_Playing*1 + Mins_Per_90_Playing*1 - CrdY*1 - CrdR*3 - two_or_more*Mins_Per_90_Playing*2+
      saves*(1/3),
    ifelse(Position == 'MID',
           xG_Expected.x*5 + xA_Expected*3 + shutout_prob*Mins_Per_90_Playing*1 +
             Starts_Playing*1 + Mins_Per_90_Playing*1 - CrdY*1 - CrdR*3,
           xG_Expected.x*4 + xA_Expected*3 + Starts_Playing*1 + 
             Mins_Per_90_Playing*1 - CrdY*1 - CrdR*3)))

#For my optimization- I need to extract player name, team, price, projected points

final_players_major <- final_players_projections %>% 
  select(web_name,short_name,Position,now_cost, Proj_Pnts)

final_players_minor <- promoted_projections %>% 
  select(web_name, short_name,Position,now_cost, Proj_Pnts)

FPL_Season_Projections <- rbind(final_players_major,final_players_minor)

#make POS column dummy variables for the optimization to work
#add a column with 1 values just to help 15 player constraint
FPL_Season_Projections <- dummy_cols(FPL_Season_Projections, select_columns = 'Position')
FPL_Season_Projections$Player_Count <- 1

#drop NA points and players we know more info on
FPL_Season_Projections <- drop_na(FPL_Season_Projections)
FPL_Season_Projections$Proj_Pnts[FPL_Season_Projections$web_name == 'Raphinha'] <- 0
FPL_Season_Projections$Proj_Pnts[FPL_Season_Projections$web_name == 'Wilson' & FPL_Season_Projections$short_name== 'FUL'] <- 0
FPL_Season_Projections$Proj_Pnts[FPL_Season_Projections$web_name == 'Mitrović'] <- 0
FPL_Season_Projections$Proj_Pnts[FPL_Season_Projections$web_name == 'Laporte'] <- 0
FPL_Season_Projections$Proj_Pnts[FPL_Season_Projections$web_name == 'Awoniyi'] <- 0
FPL_Season_Projections$Proj_Pnts[FPL_Season_Projections$web_name == 'Bowen'] <- 0


#honestly don't ove that my 3 forwards are all newly promoted- and all expected to score in top 20
# of all scorers next season... let's dig more into these star strikers, compare with other newly 
#promoted forwards like Pukki, Toney, etc

library(lpSolve)

#create constraints
cons<- c(FPL_Season_Projections$now_cost,FPL_Season_Projections$Position_GKP, FPL_Season_Projections$Position_DEF,FPL_Season_Projections$Position_FWD, FPL_Season_Projections$Player_Count)
#create matrix and double it for both sides of constraints
con <- matrix(cons, nrow=5, byrow=TRUE)
allcons <- rbind(con, con)
#set max and mins
maxrhs <- c(95.5,1,5,3,14) 
minrhs <- c(0,1,5,3,14)
maxrel <- c("<=","<=","<=","<=","<=")
minrel <- c(">=", ">=",">=",">=",">=")
#all final variables
obj <- FPL_Season_Projections$Proj_Pnts
rel <- c(maxrel,minrel)
rhs <- c(maxrhs, minrhs)
mylp<- lp("max",obj,allcons,rel,rhs,all.bin=TRUE)
#creating table just for the players that are in optimal solution
my_lineup <- FPL_Season_Projections[which(mylp$solution==1),]
sum(optsolution$now_cost)
