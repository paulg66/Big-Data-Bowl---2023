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

#Section 1 - Build Model
set.seed(99)
control <- trainControl(method="repeatedcv", number=10, repeats=3)

#Linear
modelLinear <- train(Percent_to_Pressure_Zone_per_s ~ Blockers_All + is_play_action, data=Pass_Rushers_ML, method="lmStepAIC", preProcess="scale", trControl=control)
#Random Forest
modelRF <- train(Percent_to_Pressure_Zone_per_s ~ Blockers_All + is_play_action, data=Pass_Rushers_ML, method="rf", preProcess="scale", trControl=control)
#Extreme Gradient Boost
modelxgbTree <- train(Percent_to_Pressure_Zone_per_s ~ Blockers_All + is_play_action, data=Pass_Rushers_ML, method="xgbTree", preProcess="scale", trControl=control)
modelxgbLinear <- train(Percent_to_Pressure_Zone_per_s ~ Blockers_All + is_play_action, data=Pass_Rushers_ML, method="xgbLinear", preProcess="scale", trControl=control)

#Model Comparisons
results <- resamples(list(Linear=modelLinear, Random_Forest=modelRF, xGBoostLinear = modelxgbLinear, xGBoostTree = modelxgbTree))
summary(results)
bwplot(results)
dotplot(results)

#Join Modelled Data
Pass_Rushers_RF <- data.frame(predict(modelRF))
Pass_Rushers_Final$xPZs <- Pass_Rushers_RF$predict.modelRF
Pass_Rushers_Final$dPZs <- Pass_Rushers_Final$Percent_to_Pressure_Zone_per_s - Pass_Rushers_Final$xPZs
Pass_Rushers_Final <- merge(Pass_Rushers_Final,players[,c('nflId','displayName'),])
Pass_Rushers_Final <- merge(Pass_Rushers_Final,distinct(plays[,c("gameId","playId","down","yardsToGo"),]))

Pass_Blockers <- merge(Pass_Blockers,Pass_Rushers_Final[,c('nflId','playId','gameId','dPZs')],
                       by.x = c('playId','gameId','pff_nflIdBlockedPlayer'), by.y = c('playId','gameId','nflId'))
Pass_Blockers <- merge(Pass_Blockers,players[,c('nflId','displayName'),])