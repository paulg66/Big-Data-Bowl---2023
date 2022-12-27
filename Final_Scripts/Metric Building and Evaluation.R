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

#Section 1 - Build Metric
#Path to QB

#Check if play action
play_action <- data.frame(pffScoutingData %>% filter(pff_blockType == 'PA') %>% select('gameId','playId')) %>% distinct()
play_action$is_play_action <- 1

#Play Start & End----
play_end <- main_df %>% filter(event %in% c('pass_forward','qb_sack','qb_strip_sack','autoevent_passforward','autoevent_passinterrupted'))
play_end <- play_end[order(play_end$playId,play_end$gameId,play_end$frameId,decreasing = TRUE),]
play_end <- unique(play_end[,c('playId','gameId','frameId')])
play_end <- play_end %>% group_by(playId,gameId) %>% summarise(frameId = max(frameId))
names(play_end) <- c('playId','gameId','play_end_frameId')

play_start <- main_df %>% filter(event == 'ball_snap')
play_start <- unique(play_start[,c('playId','gameId','frameId')])
names(play_start) <- c('playId','gameId','play_start_frameId')


#Determine set point, when QB throws----
QB_Set_Point <- main_df %>% filter(main_df$pff_positionLinedUp == 'QB' & main_df$dropBackType == 'TRADITIONAL')
#Snap
snap <- main_df %>% filter(officialPosition == 'QB' & event == 'ball_snap') %>% group_by(gameId,nflId,playId) %>% summarize(snapFrameId = max(frameId))
QB_Set_Point <- merge(QB_Set_Point,play_start)
#Drop Back
drop_start <- QB_Set_Point %>% filter(officialPosition == 'QB' & s >= 0.10 & (QB_Set_Point$frameId > QB_Set_Point$play_start_frameId)) %>% group_by(gameId,nflId,playId) %>% summarize(dropStartFrameId = min(frameId))
QB_Set_Point <- merge(QB_Set_Point,drop_start)
#Play End
QB_Set_Point <- merge(QB_Set_Point,play_end)

#If moving > 0.10 at snap, will create -ve acceleration on first frame. Make large negative value to keep acceleration +ve
QB_Set_Point$s <- ifelse(QB_Set_Point$event == 'ball_snap', -999, QB_Set_Point$s)

QB_Set_Point <- QB_Set_Point[order(QB_Set_Point$gameId,QB_Set_Point$nflId,QB_Set_Point$playId,QB_Set_Point$frameId),]
QB_Set_Point <- QB_Set_Point[(QB_Set_Point$frameId >= QB_Set_Point$dropStartFrameId) & (QB_Set_Point$frameId > QB_Set_Point$play_start_frameId),]
QB_Set_Point <- mutate(QB_Set_Point, a_direction = s-lag(s))
QB_Set_Point<-mutate(QB_Set_Point, s_direction = a_direction>lag(a_direction) & a_direction > 0)

peak_velocity <- QB_Set_Point %>% filter(a_direction < 0 & (frameId-dropStartFrameId) >= 3 & abs(a_direction) >=0.10 ) %>% group_by(gameId,nflId,playId) %>% summarise(set_point_frameId = min(frameId)-1)
QB_Set_Point <- merge(QB_Set_Point,peak_velocity)
set_point <- QB_Set_Point[(QB_Set_Point$set_point_frameId < QB_Set_Point$frameId & QB_Set_Point$s_direction == TRUE) | (QB_Set_Point$frameId == QB_Set_Point$play_end_frameId),c('nflId','playId','gameId','frameId','x','y')] %>%
  group_by(nflId,playId,gameId) %>% summarise(frameId = min(frameId)-1)
QB_Set_Point <- QB_Set_Point[,c('nflId','playId','gameId','frameId','x','y')]
QB_Set_Point <- merge(QB_Set_Point,set_point)
names(QB_Set_Point) <- c('nflId_QB','playId','gameId','frameId','x_set_point','y_set_point')


#Pass Rushers----
Yards_to_Set_Point <- 2
Pass_Rushers <-  main_df %>% filter(main_df$pff_role == "Pass Rush" & main_df$pff_positionLinedUp %in%
                                      c("DRT","DLT","LE","LILB","LEO","REO","ROLB","MLB","RE","LOLB","RILB","RLB","NLT","NT","LLB","NRT" ))
Pass_Rushers <- Pass_Rushers[,c("nflId","playId","gameId","frameId","jerseyNumber","team","playDirection","x","y","s","a","event","pff_positionLinedUp",
                                "pff_hit","pff_hurry","pff_sack","passResult","dropBackType","playResult")]

Pass_Rushers <- merge(Pass_Rushers,QB_Set_Point[,c('nflId_QB','playId','gameId','x_set_point','y_set_point')])


#Remove before snap and after play end event (pass forward or sack)
Pass_Rushers <- merge(Pass_Rushers,play_end, by = c('playId','gameId'))
Pass_Rushers <- merge(Pass_Rushers,play_start, by = c('playId','gameId'))
Pass_Rushers <- Pass_Rushers[Pass_Rushers$frameId <= Pass_Rushers$play_end_frameId,]
Pass_Rushers <- Pass_Rushers[Pass_Rushers$frameId >= Pass_Rushers$play_start_frameId,]

Pass_Rushers$Set_Point_Max_Delta <- pmax(abs(Pass_Rushers$x - Pass_Rushers$x_set_point),abs(Pass_Rushers$y - Pass_Rushers$y_set_point))
Pass_Rushers <- Pass_Rushers[order(Pass_Rushers$gameId,Pass_Rushers$playId,Pass_Rushers$nflId,Pass_Rushers$frameId),]

#Determine if player in the pressure box before throw --> 2 yards
Pass_Rushers$frame_trigger <- ifelse(Pass_Rushers$frameId == Pass_Rushers$play_start_frameId,'snap',ifelse(Pass_Rushers$frameId == Pass_Rushers$play_end_frameId,'playEnd',
                                                                                                           ifelse(Pass_Rushers$Set_Point_Max_Delta <= Yards_to_Set_Point,'pressure','')))

#Summarize when a player generated pressure or not
Pass_Rushers_Pressure <- Pass_Rushers %>% group_by(playId,gameId,nflId) %>% filter(frame_trigger == 'pressure') %>%
  summarise(frameId = min(frameId))
Pass_Rushers_Pressure$Frame_Status <- 'Pressure_Start'

Pass_Rushers <- merge(Pass_Rushers,Pass_Rushers_Pressure, by = c('playId','gameId','nflId','frameId'),all.x = TRUE)
Pass_Rushers <- Pass_Rushers[order(Pass_Rushers$gameId,Pass_Rushers$playId,Pass_Rushers$nflId,Pass_Rushers$frameId),]
Pass_Rushers$Frame_Status <- ifelse(Pass_Rushers$frame_trigger == 'playEnd','playEnd',ifelse(Pass_Rushers$frame_trigger == 'snap','snap',Pass_Rushers$Frame_Status))

Pass_Rushers_Summary <- Pass_Rushers[is.na(Pass_Rushers$Frame_Status) == FALSE,]
Pass_Rushers_Delta <- Pass_Rushers_Summary[,c('playId','gameId','nflId','Set_Point_Max_Delta','Frame_Status'),]
names(Pass_Rushers_Delta) <- c('playId','gameId','nflId','value','variable')
Pass_Rushers_Delta <- cast(Pass_Rushers_Delta,playId + gameId + nflId ~ variable,fun.aggregate = sum)
Pass_Rushers_Delta <- merge(Pass_Rushers_Delta,Pass_Rushers_Summary %>% filter(Frame_Status != 'snap') %>% 
                              group_by(playId,gameId,nflId,play_start_frameId) %>% summarise(Delta_End = min(frameId)))
Pass_Rushers_Delta$Seconds <- (Pass_Rushers_Delta$Delta_End - Pass_Rushers_Delta$play_start_frameId)*0.1
Pass_Rushers_Delta$Distance <- ifelse(Pass_Rushers_Delta$Pressure_Start >0 ,Pass_Rushers_Delta$snap - Pass_Rushers_Delta$Pressure_Start,
                                      Pass_Rushers_Delta$snap - Pass_Rushers_Delta$playEnd)
Pass_Rushers_Delta$Percent_to_Pressure_Zone <- ifelse(Pass_Rushers_Delta$Pressure_Start >0 ,1,Pass_Rushers_Delta$Distance / (Pass_Rushers_Delta$snap-Yards_to_Set_Point))
Pass_Rushers_Delta$Percent_to_Pressure_Zone <- ifelse(Pass_Rushers_Delta$Percent_to_Pressure_Zone > 1,1,Pass_Rushers_Delta$Percent_to_Pressure_Zone)
Pass_Rushers_Delta$Percent_to_Pressure_Zone_per_s <- Pass_Rushers_Delta$Percent_to_Pressure_Zone / Pass_Rushers_Delta$Seconds

Pass_Rushers_Final <- merge(Pass_Rushers_Delta,Pass_Rushers_Summary[Pass_Rushers_Summary$event == 'ball_snap',
                                                                    c('playId','gameId','nflId','pff_positionLinedUp','jerseyNumber','team',
                                                                      'pff_hurry','pff_sack','pff_hit','passResult','playResult')])



#Pass Blockers----
Pass_Blockers <-  main_df %>% filter(main_df$pff_role == "Pass Block" & main_df$pff_positionLinedUp %in%
                                       c("LT","LG","C","RG","RT"))
Pass_Blockers <- unique(Pass_Blockers[,c("nflId","playId","gameId","playDirection","jerseyNumber","team","pff_positionLinedUp",
                                         "pff_beatenByDefender","pff_hitAllowed","pff_hurryAllowed",      
                                         "pff_sackAllowed","pff_nflIdBlockedPlayer","pff_blockType","passResult","dropBackType")])
Pass_Blockers <- Pass_Blockers[order(Pass_Blockers$gameId,Pass_Blockers$playId,Pass_Blockers$nflId),]

#Add how many blockers to pass rush data
#Count Blockers that are OL
Blocker_Count_OL <- main_df[main_df$pff_role == 'Pass Block' & main_df$pff_positionLinedUp %in%c("LT","LG","C","RG","RT"),
                            c('gameId','playId','nflId', 'pff_nflIdBlockedPlayer')] %>% 
  distinct() %>% group_by(gameId,playId, pff_nflIdBlockedPlayer) %>% summarise(Blockers = n())
Blocker_Count_OL <- Blocker_Count_OL[complete.cases(Blocker_Count_OL$pff_nflIdBlockedPlayer),]
names(Blocker_Count_OL) <- c("gameId","playId","nflId","Blockers_OL")

#Count Total Blockers
Blocker_Count_All <- main_df[main_df$pff_role == 'Pass Block',c('gameId','playId','nflId', 'pff_nflIdBlockedPlayer')] %>% 
  distinct() %>% group_by(gameId,playId, pff_nflIdBlockedPlayer) %>% summarise(Blockers = n())
Blocker_Count_All <- Blocker_Count_All[complete.cases(Blocker_Count_All$pff_nflIdBlockedPlayer),]
names(Blocker_Count_All) <- c("gameId","playId","nflId","Blockers_All")

Pass_Rushers_Final <- merge(Pass_Rushers_Final,Blocker_Count_OL, all.x = TRUE)
Pass_Rushers_Final <- merge(Pass_Rushers_Final,Blocker_Count_All, all.x = TRUE)
Pass_Rushers_Final[is.na(Pass_Rushers_Final$Blockers_OL),'Blockers_OL'] <- 0
Pass_Rushers_Final[is.na(Pass_Rushers_Final$Blockers_All),'Blockers_All'] <- 0

#Check if blocked by an least 1 OL, remove players blocked by only RB,TE,WR
Pass_Rushers_Final <- merge(Pass_Rushers_Final,unique(Pass_Blockers[,c('pff_nflIdBlockedPlayer','gameId','playId')]),
                            by.x = c('playId','gameId','nflId'), by.y = c('playId','gameId','pff_nflIdBlockedPlayer'))

#Add Play Action to Pass Rusher Data
Pass_Rushers_Final <- merge(Pass_Rushers_Final,play_action, all.x = TRUE)
Pass_Rushers_Final[is.na(Pass_Rushers_Final$is_play_action),'is_play_action'] <- 0

#Add Rusher to Blocker Data
Pass_Blockers <- merge(Pass_Blockers,Pass_Rushers_Final[,c('playId','gameId','nflId','Percent_to_Pressure_Zone_per_s','Blockers_OL','Blockers_All')],
                       by.x = c('playId','gameId','pff_nflIdBlockedPlayer'), by.y = c('playId','gameId','nflId'))

#Section 2 - Evaluate Metric
#Summary / Distribution
summary(Pass_Rushers_Final$Percent_to_Pressure_Zone_per_s)
hist_plot <- hist(Pass_Rushers_Final$Percent_to_Pressure_Zone_per_s, breaks = 20,main = '%PZ/s Histogram', 
                  xlab = 'Percent_to_Pressure_Zone_per_s',xaxt="n")
axis(side=1, at=hist_plot$breaks, labels=paste0(100*hist_plot$breaks, "%"))

#Position / Team Breakdown
#Does it work? Are the effects linear?
#How am I defining success? Other metrics to pair them up against
Pass_Rushers_Final %>% group_by(pff_hurry) %>% summarise(median(Percent_to_Pressure_Zone_per_s),n())
Pass_Rushers_Final %>% group_by(pff_hit) %>% summarise(median(Percent_to_Pressure_Zone_per_s),n())
Pass_Rushers_Final %>% group_by(pff_sack) %>% summarise(median(Percent_to_Pressure_Zone_per_s),n())

Pass_Rushers_Final %>% group_by(passResult) %>% summarise(median(Percent_to_Pressure_Zone_per_s),n())
Pass_Rushers_Final %>% group_by(pff_positionLinedUp) %>% summarise(median(Percent_to_Pressure_Zone_per_s),n())
Pass_Blockers %>% group_by(pff_positionLinedUp) %>% summarise(median(Percent_to_Pressure_Zone_per_s),n())
