#Paul Gallagher - Big Data Bowl 2023
library(dplyr)
library(stringr)
library(gridExtra)
library(reshape)
library(readr)
library(ggplot2)
library(caret)
library(pgirmess)
library(xgboost)
library(DiagrammeR)
library(DataExplorer)

#Load Data
plays <- read.csv("plays.csv")
pffScoutingData <- read.csv("pffScoutingData.csv")
players <- read.csv("players.csv")

#Section 1 -- Cleaning & Merging
all_weeks <- list.files("/Users/paulgallagher/Documents/Big Data Bowl - 2023/nfl-big-data-bowl-2023", pattern = 'week', full.names = TRUE)%>% 
  lapply(read_csv) %>% 
  bind_rows 

#Join in PFF Scouting Data, Nfl Play Data, Player Data
main_df <- merge(all_weeks,pffScoutingData, by = c('playId','nflId','gameId'))
main_df <- merge(main_df,plays, by = c('playId','gameId'))
main_df <- merge(main_df,players, by = 'nflId')

#Section 2 ----- Define Play Type
#Only want to include regular drop backs, remove penalties, designed roll outs, unknown, QB runs
main_df <- main_df[main_df$dropBackType != 'DESIGNED_RUN',]
main_df <- main_df[main_df$dropBackType != 'DESIGNED_ROLLOUT_RIGHT',]
main_df <- main_df[main_df$dropBackType != 'DESIGNED_ROLLOUT_LEFT',]
main_df <- main_df[main_df$dropBackType != 'NA',]
main_df <- main_df[main_df$dropBackType != 'UNKNOWN',]

main_df <- main_df[order(main_df$gameId,main_df$nflId,main_df$playId,main_df$frameId),]

#Remove scrambles where they is play action
exclude_plays <- main_df %>% filter(pff_positionLinedUp == 'QB' & 
                                      (event == 'run' | (event == 'play_action' & 
                                                           (dropBackType == 'SCRAMBLE' | 
                                                              dropBackType == 'SCRAMBLE_ROLLOUT_RIGHT' |
                                                              dropBackType == 'SCRAMBLE_ROLLOUT_LEFT')))) %>%
                group_by(playId,gameId)
exclude_plays <- exclude_plays[,c("playId","gameId")]
main_df <- anti_join(main_df,exclude_plays,by = c("playId","gameId")) 

#Remove Penalties
main_df <- main_df[is.na(main_df$foulNFLId1),]

#Outlier Clean Up
main_df <- main_df[main_df$playId != 2699 & main_df$gameId != 2021091204,] #3 players coordinates are in the back field at snap
main_df <- main_df[main_df$playId != 1191 & main_df$gameId != 2021102400,] #Outlier, drop back only 2 yards