library(plotly)
library(tidyverse)
library(httr)
library(rvest)
library(devtools)
library(jsonlite)
library(rtweet)
library(reactable)
library(glue)
library(ballr)
library(stringr)
library(shiny)

#2019-2020 NBA Player Stats
nba <-read_html("https://www.basketball-reference.com/leagues/NBA_2020_per_game.html") 
nba_table <- html_table(nba)
player_stats <- data.frame(nba_table)
player_stats <- player_stats %>% mutate_at(vars(Rk,Age,G,GS,MP,FG,FGA,FG.,X3P,X3PA,X3P.,X2P,eFG.,FT,FTA,FT.,ORB,DRB,TRB,AST,STL,BLK,TOV,PF,PTS),funs(as.numeric)) %>% drop_na

#Alternative Way to get data from same website(USING ballr package)
playerdata <- NBAPerGameStatistics(season = 2020)

#2019-2020 Nba Players (Distinct) Since traded players have multiple rows
playerdist <- playerdata%>% distinct(playerdist,player,.keep_all = TRUE)

#2019-2020 NBA League Averages
nbaleagueavg <- read_html("https://www.basketball-reference.com/leagues/NBA_stats_per_game.html") %>% 
  html_table %>% 
  data.frame %>% 
  drop_na
names(nbaleagueavg) <- nbaleagueavg[1,]
nbaleagueavg<-nbaleagueavg %>%  mutate_at(vars(Rk,Age,Wt,G,MP,FG,FGA,`3P`,`3PA`,FT,FTA,ORB,DRB,TRB,AST,STL,BLK,TOV,PF,PTS,`FG%`,`3P%`,`FT`,Pace,`eFG%`,`TOV%`,`ORB%`,`ORtg`),funs(as.numeric)) 
nbaleagueavg <- nbaleagueavg[2,]

#APIs

#SportsDataIO API for photo access

##Inner join the data to the distinct player data set to correspond the photos
r <- GET("https://api.sportsdata.io/v3/nba/scores/json/Players?key=8d1e9146298b4771a541957a3c12409e")
json <- content(r, "text", encoding = "UTF-8")
active <-fromJSON(json, simplifyMatrix = FALSE)
active <- active %>%  rename(player = YahooName)
lol <- playerdist %>% inner_join(active)

#Twitter API
twitter_app <- oauth_app("twitter",
                         key = "KFxa563fmYmeFPmzWjOPMOXtY",
                         secret = Sys.getenv("twitterkey")
)
#creating token
twitter_token <- create_token(
  app = "mytwitterapp",
  consumer_key = "KFxa563fmYmeFPmzWjOPMOXtY",
  consumer_secret = Sys.getenv('twitterkey'),
  access_token = Sys.getenv("token"),
  access_secret = Sys.getenv("token_secret"),
  set_renv = TRUE
)

