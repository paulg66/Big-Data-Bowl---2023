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

#Section 1 - Model Outputs
#Player & Team Outputs
#Rushers
Pass_Rushers_Rankings <- Pass_Rushers_Final %>% group_by(displayName,team) %>% 
  summarise(sum_dPZs = sum(dPZs), rush_attempts = n(), median_dPZs = median(dPZs)) %>% filter(rush_attempts >= 50) %>%
  arrange(desc(median_dPZs))
Pass_Rushers_Rankings$sum_dPZs <- round(Pass_Rushers_Rankings$sum_dPZs,3)
Pass_Rushers_Rankings$median_dPZs <- round(Pass_Rushers_Rankings$median_dPZs,3)

Team_Rush_Rankings <- Pass_Rushers_Final %>% group_by(team) %>% 
  summarise(sum_dPZs = sum(dPZs), rush_attempts = n(), median_dPZs = median(dPZs)) %>% arrange(desc(sum_dPZs))

#Blockers
Pass_Blockers_Rankings <- Pass_Blockers %>% group_by(displayName,team,pff_positionLinedUp) %>% 
  summarise(sum_dPZs = sum(dPZs), snaps = n(), median_dPZs = median(dPZs)) %>% filter(snaps > 50) %>% arrange(median_dPZs)
Pass_Blockers_Rankings$sum_dPZs <- round(Pass_Blockers_Rankings$sum_dPZs,3)
Pass_Blockers_Rankings$median_dPZs <- round(Pass_Blockers_Rankings$median_dPZs,3)


Team_Blockers_Rankings <- Pass_Blockers %>% group_by(team)  %>% 
  summarise(sum_dPZs = sum(dPZs), snaps = n(), median_dPZs = median(dPZs)) %>% arrange(sum_dPZs)

#Animated Plot
sample_play <- Pass_Rushers %>% filter(gameId == '2021103104', playId == 342, team == 'PHI')
sample_play <- sample_play[c(1:157),]
sample_play<- merge(sample_play,players[,c('nflId','displayName'),])
ggplot(sample_play,aes(x = frameId, y = Set_Point_Max_Delta-2, group = displayName, color = displayName)) +
  geom_line() + geom_point() + scale_y_reverse() + geom_hline(yintercept = 0) +
  geom_text(aes(15,0,label = 'Pressure Zone Start',vjust = -1),color = 'black') + ggtitle('Pass Rusher Path to Pressure Zone') +
  ylab('Yards to Pressure Zone') + transition_reveal(frameId)