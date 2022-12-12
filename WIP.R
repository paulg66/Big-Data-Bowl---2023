#Paul Gallagher - Big Data Bowl 2023
library(dplyr)
library(stringr)
library(gridExtra)
library(reshape)
library(readr)
library(ggplot2)
library(caret)
library(pgirmess)

#Load Data
#plays <- read.csv("plays.csv")
#pffScoutingData <- read.csv("pffScoutingData.csv")
#players <- read.csv("players.csv")

#Section 1 -- Cleaning & Merging
all_weeks <- list.files("/Users/paulgallagher/Documents/Big Data Bowl - 2023/nfl-big-data-bowl-2023", pattern = 'week', full.names = TRUE)%>% 
  lapply(read_csv) %>% 
  bind_rows 

#First intial of first name and full last name of player, to find pass catcher from play description
#players$RefName <- paste(str_sub(players$displayName,0,1),
#                         str_sub(players$displayName,str_locate(players$displayName," ")[,1]+1), sep = '.')

#Parse out receiver from play description
#str_sub(test_string,str_locate(test_string,"to")[,2]+2)
#plays$RefName <- str_trim(str_sub(str_sub(plays$playDescription,str_locate(plays$playDescription,"to")[,2]+2),1,
#                                  str_locate(str_sub(plays$playDescription,str_locate(plays$playDescription,"to")[,2]+2)," ")[1]))

#plays$RefName <- str_sub(plays$playDescription,str_locate(plays$playDescription,"to")[,2]+2)
#Tryo using nflFastR to get who caught the ball

#Join in PFF Scouting Data, Nfl Play Data, Player Data
main_df <- merge(all_weeks,pffScoutingData, by = c('playId','nflId','gameId'))
main_df <- merge(main_df,plays, by = c('playId','gameId'))
main_df <- merge(main_df,players, by = 'nflId')

#Section 2 ----- Define Play Type
#Only want to include regular drop backs, remove penalties, screens, designed roll outs, unknown, QB runs, blitzes
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

#-------------------------------------------------
#Section 3 - Time to Throw
#The goal here is to see the time difference between the snap and throw, with the theory that longer passes need
#more time to throw
#Clean pocket throws only, ignore scrambles.
time_to_throw <- main_df %>% filter((main_df$event == 'ball_snap' | main_df$event == 'pass_forward') &
                                      main_df$pff_positionLinedUp == 'QB' & main_df$passResult == "C"
                                    & main_df$dropBackType == 'TRADITIONAL')
time_to_throw <- time_to_throw %>% group_by(playId,gameId,nflId) %>% summarize(snap = min(frameId)*0.1,throw = max(frameId)*0.1, yards = max(playResult))
time_to_throw$seconds_to_throw <- time_to_throw$throw - time_to_throw$snap
#ToDO: Convert time and calculate difference, plot time difference to yards gained, check about air years vs total yards?
#Total receiver distance traveled
#Could use EPA here instead of yards gained?

#Section 4 - Path to QB

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
Blocker_Count_OL <- main_df[main_df$pff_role == 'Pass Block'  & main_df$pff_blockType == 'PP' &
                              main_df$pff_positionLinedUp %in%c("LT","LG","C","RG","RT"),
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


#Other variables impact on Metric

#Section 5 - Evaluate Metric
#Summary / Distribution
summary(Pass_Rushers_Final$Percent_to_Pressure_Zone_per_s)
hist(Pass_Rushers_Final$Percent_to_Pressure_Zone_per_s, breaks = 20)

#Position / Team Breakdown
#Does it work? Are the effects linear?
#How am I defining succes? Other metrics to pair them up against
Pass_Rushers_Final %>% group_by(pff_hurry) %>% summarise(median(Percent_to_Pressure_Zone_per_s),n())
Pass_Rushers_Final %>% group_by(pff_hit) %>% summarise(median(Percent_to_Pressure_Zone_per_s),n())
Pass_Rushers_Final %>% group_by(pff_sack) %>% summarise(median(Percent_to_Pressure_Zone_per_s),n())

Pass_Rushers_Final %>% group_by(passResult) %>% summarise(median(Percent_to_Pressure_Zone_per_s),n())
Pass_Rushers_Final %>% group_by(pff_positionLinedUp) %>% summarise(median(Percent_to_Pressure_Zone_per_s),n())
Pass_Blockers %>% group_by(pff_positionLinedUp) %>% summarise(median(Percent_to_Pressure_Zone_per_s),n())


#Position Comparison
pos_count <- table(Pass_Rushers_Final$pff_positionLinedUp)
ggplot(subset(Pass_Rushers_Final, pff_positionLinedUp %in% names(pos_count[pos_count > 400])),aes(pff_positionLinedUp,Percent_to_Pressure_Zone_per_s,fill = as.factor(pff_positionLinedUp))) + 
  geom_boxplot() + ggtitle('%PZ/s vs Position')
#Show Impact off adding the per second metric vs pff_hurry

#What impacts a rusher's change of pressuring the QB
#1. Number of Blockers
Pass_Rushers_ML <- Pass_Rushers_Final[,c("pff_positionLinedUp","Blockers_OL","Blockers_All",'is_play_action',"Percent_to_Pressure_Zone_per_s"),]
Pass_Blockers_ML <- Pass_Blockers[,c("pff_positionLinedUp","Blockers_OL","Blockers_All","pff_blockType","Percent_to_Pressure_Zone_per_s"),]
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(Percent_to_Pressure_Zone_per_s~., data=Pass_Rushers_ML, method="lmStepAIC", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)
#2. Play Action
pos_count <- table(Pass_Blockers_ML$pff_blockType)
ggplot(subset(Pass_Blockers_ML, pff_blockType %in% names(pos_count[pos_count > 250])),aes(pff_blockType,Percent_to_Pressure_Zone_per_s,
                                                                                          fill = as.factor(pff_blockType))) + geom_boxplot() + ggtitle('%PZ/s vs Block Type')
#3. Position?
pairwise.t.test(Pass_Blockers$Percent_to_Pressure_Zone_per_s,Pass_Blockers$pff_positionLinedUp,p.adjust.method = "bonf")
kruskalmc(Percent_to_Pressure_Zone_per_s ~ pff_positionLinedUp, data = Pass_Blockers)

pairwise.t.test(subset(Pass_Rushers_Final, pff_positionLinedUp %in% names(pos_count[pos_count > 400]))$Percent_to_Pressure_Zone_per_s,subset(Pass_Rushers_Final, pff_positionLinedUp %in% names(pos_count[pos_count > 400]))$pff_positionLinedUp,p.adjust.method = "bonf")
kruskalmc(Percent_to_Pressure_Zone_per_s ~ pff_positionLinedUp, data = subset(Pass_Rushers_Final, pff_positionLinedUp %in% names(pos_count[pos_count > 400])))

temp_test <- Pass_Rushers_Final %>% group_by(nflId,team) %>% summarise(median = mean(Percent_to_Pressure_Zone_per_s)*mean(Blockers),n(),Blockers = mean(Blockers))
temp_test <- Pass_Blockers %>% group_by(nflId,team) %>% summarise(median = mean(Percent_to_Pressure_Zone_per_s),per = mean(Percent_to_Pressure_Zone_per_s)* mean(Blockers),n(),Blockers = mean(Blockers))

temp_test <- temp_test[temp_test$`n()` >= 100,]
temp_test <- merge(temp_test,players[,c('nflId','officialPosition','displayName')])
temp_test <- temp_test[order(temp_test$median, decreasing = FALSE),]

#Goal - Plot blocker, rusher and QB set point. Find a way to animate plot to show x & y over time
ggplot() + geom_point(data = test_path, aes(frameId,y),colour = 'blue') + geom_point(data = test_path, aes(frameId,y_set_point),colour = 'green') + geom_point(data = rush_test, aes(frameId,y),colour = 'maroon')
ggplot(TB_temp[TB_temp$playId == 97 & TB_temp$frameId > 5,],aes(frameId,s)) + geom_point()
ggplot(QB_Set_Point[QB_Set_Point$playId == 4106 & QB_Set_Point$nflId == 43290 ,],aes(frameId,s)) + geom_point()

grid.arrange(ggplot(TB_temp[TB_temp$playId == 434 & TB_temp$frameId > 5,],aes(frameId,x)) + geom_point(aes(color = event),show.legend = FALSE), 
             ggplot(TB_temp[TB_temp$playId == 434 & TB_temp$frameId > 5,],aes(frameId,s)) + geom_point(aes(color = event),show.legend = FALSE), nrow=2)

grid.arrange(ggplot(TB_temp[TB_temp$playId == 1267 & TB_temp$pff_positionLinedUp == 'QB',],aes(frameId,x)) + geom_point(aes(color = event),show.legend = FALSE),
             ggplot(TB_temp[TB_temp$playId == 1267 & TB_temp$pff_positionLinedUp == 'QB',],aes(frameId,s)) + geom_point(aes(color = event),show.legend = FALSE), 
             ggplot(TB_temp[TB_temp$playId == 1267 & TB_temp$pff_positionLinedUp == 'QB',],aes(frameId,a)) + geom_point(aes(color = event),show.legend = FALSE), nrow=3)

#What do I need to calculate my metric
#1. Time to throw - Done
#2. Play time if sacked - Todo
#3. Time from snap to scramble - Todo
#4. X & Y QB set point - Done, but can be improved. Issues with setup and scrambles. Can I used when they slow done, max X?
#5. X & Y at snap for OL and pass rushers - Todo
#6. Block types (standard pass pro, chip block) - Todo
#7 Percent traveled to set point min(dX,dY)

#Setpoint - Find local min 
#Snap Frame ID per play
snap <- main_df %>% filter(officialPosition == 'QB' & event == 'ball_snap') %>% group_by(playId,gameId) %>% summarize(snapFrameId = max(frameId))
TB_temp <- main_df %>% filter(gameId == '2021091905',nflId == 25511)
TB_temp <- merge(TB_temp,snap)
TB_temp <- TB_temp[TB_temp$frameId >= TB_temp$snapFrameId,]
TB_temp <- TB_temp[order(TB_temp$playId,TB_temp$frameId),]
TB_temp <- mutate(TB_temp, a_direction = s-lag(s))
set_point <- TB_temp %>% filter(a_direction < 0 & (frameId-snapFrameId) >3) %>% group_by(gameId,playId) %>% summarise(set_point_frameId = min(frameId)-1)


