library(readr)
library(caret)
library(tidyverse)
library(ranger)
library(e1071)

#### LOAD TRAIN/TEST DATASETS FOR BUILDINGS ####

## inTraining
inTraining_B0_lat <- read_rds("inTraining_B0_lat.rds")
inTraining_B1_lat <- read_rds("inTraining_B1_lat.rds")
inTraining_B2_lat <- read_rds("inTraining_B2_lat.rds")
inTraining_B0_long <- read_rds("inTraining_B0_long.rds")
inTraining_B1_long<- read_rds("inTraining_B1_long.rds")
inTraining_B2_long <- read_rds("inTraining_B2_long.rds")
inTraining_B0_floor <- read_rds("inTraining_B0_floor.rds")
inTraining_B1_floor <- read_rds("inTraining_B1_floor.rds")
inTraining_B2_floor <- read_rds("inTraining_B2_floor.rds")
inTraining_B0_floor

## traning
training_B0_lat <- read_rds("training_B0_lat.rds")
training_B0_lat <- training_B0_lat %>% 
  select(starts_with("WAP"), LATITUDE)
training_B1_lat <- read_rds("training_B1_lat.rds")
training_B1_lat <- training_B1_lat %>% 
  select(starts_with("WAP"), LATITUDE)
training_B2_lat <- read_rds("training_B2_lat.rds")
training_B2_lat <- training_B2_lat %>% 
  select(starts_with("WAP"), LATITUDE)
training_B0_long <- read_rds("training_B0_long.rds")
training_B0_long <- training_B0_long %>% 
  select(starts_with("WAP"), LONGITUDE)
training_B1_long<- read_rds("training_B1_long.rds")
training_B1_long <- training_B1_long %>% 
  select(starts_with("WAP"), LONGITUDE)
training_B2_long <- read_rds("training_B2_long.rds")
training_B2_long <- training_B2_long %>% 
  select(starts_with("WAP"), LONGITUDE)
training_B0_floor <- read_rds("training_B0_floor.rds")
training_B0_floor <- training_B0_floor %>% 
  select(starts_with("WAP"), FLOOR)
training_B1_floor <- read_rds("training_B1_floor.rds")
training_B1_floor <- training_B1_floor_rf_factor %>% 
  select(starts_with("WAP"), FLOOR)
training_B2_floor <- read_rds("training_B2_floor.rds")
training_B2_floor <- training_B2_floor %>% 
  select(starts_with("WAP"), FLOOR)

## testing
testing_B0_lat <- read_rds("testing_B0_lat.rds")
testing_B0_lat <- testing_B0_lat %>% 
  select(starts_with("WAP"), LATITUDE)
testing_B1_lat <- read_rds("testing_B1_lat.rds")
testing_B1_lat <- testing_B1_lat %>% 
  select(starts_with("WAP"), LATITUDE)
testing_B2_lat <- read_rds("testing_B2_lat.rds")
testing_B2_lat <- testing_B2_lat %>% 
  select(starts_with("WAP"), LATITUDE)
testing_B0_long <- read_rds("testing_B0_long.rds")
testing_B0_long <- testing_B0_long %>% 
  select(starts_with("WAP"), LONGITUDE)
testing_B1_long <- read_rds("testing_B1_long.rds")
testing_B1_long <- testing_B1_long %>% 
  select(starts_with("WAP"), LONGITUDE)
testing_B2_long <- read_rds("testing_B2_long.rds")
testing_B2_long <- testing_B2_long %>% 
  select(starts_with("WAP"), LONGITUDE)
testing_B0_floor <- read_rds("testing_B0_floor.rds")
testing_B0_floor <- testing_B0_floor %>% 
  select(starts_with("WAP"), FLOOR)
testing_B1_floor <- read_rds("testing_B1_floor.rds")
testing_B1_floor <- testing_B1_floor %>% 
  select(starts_with("WAP"), FLOOR)
testing_B2_floor <- read_rds("testing_B2_floor.rds")
testing_B2_floor <- testing_B2_floor %>% 
  select(starts_with("WAP"), FLOOR)

#10 fold cross validation----
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 1, 
                           verboseIter = TRUE)

#### TRAIN B0, B1, B2 for lat/long/floor ####

# predict the location of a user in a building (long/lat/floor) based on WAP's

## LATITUDE----

Fit_lat_B0_rf <- train(LATITUDE ~., 
                    data = training_B0_lat, 
                    method = "ranger",
                    trControl=fitControl, 
                    preProcess = c("zv", "medianImpute"))
Fit_lat_B0_rf
saveRDS(Fit_lat_B0_rf, file = "RF_Fit_lat_B0.rds")
# mtry  splitrule   RMSE       Rsquared   MAE
# 196   extratrees   2.447808  0.9944988  1.552795


#Fit_lat_B0 postresample----
postResample(pred = predict(object = Fit_lat_B0_rf, 
                            newdata = testing_B0_lat), 
             obs = testing_B0_lat$LATITUDE)
#   RMSE      Rsquared    MAE 
#   2.2772682 0.9953698 1.3147164 

Fit_lat_B1_rf <- train(LATITUDE~., 
                    data = training_B1_lat, 
                    method = "ranger", 
                    trControl=fitControl, 
                    preProcess = c("zv", "medianImpute"))
Fit_lat_B1_rf
saveRDS(Fit_lat_B1_rf, file = "RF_Fit_lat_B1.rds")
#mtry  splitrule   RMSE       Rsquared   MAE       
#104   extratrees   3.951866  0.9882743  2.396714

#Fit_lat_B1 postresample----
postResample(pred = predict(object = Fit_lat_B1_rf, 
                            newdata = testing_B1_lat), 
             obs = testing_B1_lat$LATITUDE)
#  RMSE      Rsquared  MAE 
#  3.5814652 0.9902493 2.1510434

Fit_lat_B2_rf <- train(LATITUDE~., 
                    data = training_B2_lat, 
                    method = "ranger", 
                    trControl=fitControl, 
                    preProcess = c("zv", "medianImpute"))
Fit_lat_B2_rf
saveRDS(Fit_lat_B2_rf, file = "RF_Fit_lat_B2.rds")

#mtry  splitrule   RMSE       Rsquared   MAE  
#199   extratrees   3.510307  0.9849831   2.059200

#Fit_lat_B2 postresample----
postResample(pred = predict(object = Fit_lat_B2_rf, 
                            newdata = testing_B2_lat), 
             obs = testing_B2_lat$LATITUDE)
#   RMSE      Rsquared  MAE 
#   3.3257597 0.9865359 1.9903211

## LONGITUDE----
Fit_long_B0_rf <- train(LONGITUDE~., 
                     data = training_B0_long, 
                     method = "ranger", 
                     trControl=fitControl, 
                     preProcess = c("zv", "medianImpute"))
Fit_long_B0_rf
saveRDS(Fit_long_B0_rf, file = "RF_Fit_long_B0.rds")

#mtry  splitrule   RMSE       Rsquared   MAE  
#99   extratrees   2.945877  0.9865375  1.870254

#Fit_long_B0 postresample----
postResample(pred = predict(object = Fit_long_B0_rf, 
                            newdata = testing_B0_long), 
             obs = testing_B0_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#   2.6748828 0.9888481 1.7395135    

Fit_long_B1_rf <- train(LONGITUDE~., 
                     data = training_B1_long, 
                     method = "ranger", 
                     trControl=fitControl, 
                     preProcess = c("zv", "medianImpute"))
Fit_long_B1_rf
saveRDS(Fit_long_B1_rf, file = "RF_Fit_long_B1.rds")
#mtry  splitrule   RMSE       Rsquared   MAE  
#206   extratrees   4.178683  0.9929922   2.579351

#Fit_long_B1 postresample----
postResample(pred = predict(object = Fit_long_B1_rf, 
                            newdata = testing_B1_long), 
             obs = testing_B1_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#4.3462200 0.9923514 2.4202011  

Fit_long_B2_rf <- train(LONGITUDE~., 
                     data = training_B2_long, 
                     method = "ranger", 
                     trControl=fitControl, 
                     preProcess = c("zv", "medianImpute"))
Fit_long_B2_rf
saveRDS(Fit_long_B2_rf, file = "RF_Fit_long_B2.rds")
#mtry  splitrule   RMSE       Rsquared   MAE  
#199   extratrees   4.863235  0.9743902   2.681420 

#Fit_long_B2 postresample----
postResample(pred = predict(object = Fit_long_B2_rf, 
                            newdata = testing_B2_long), 
             obs = testing_B2_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#4.4375054 0.9786499 2.4247264  

## FLOOR----
#B0_Floor not set to factor----
#Fit_floor_B0 <- train(FLOOR~., 
#                      data = training_B0_floor, 
 #                     method = "kknn", 
  #                    trControl=fitControl, 
   #                   tuneLength = 5,
    #                  verboseIter = TRUE,
     #                 preProcess = c("zv", "medianImpute"))
#Fit_floor_B0
#saveRDS(Fit_floor_B0, file = "KNN_Fit_floor_B0.rds")

#B0_Floor set to factor----
training_B0_floor_rf_factor <- training_B0_floor %>% 
  mutate(FLOOR = as.factor(FLOOR))
Fit_floor_B0_rf_factor <- train(FLOOR~., 
                      data = training_B0_floor_rf_factor, 
                      method = "ranger", 
                      trControl=fitControl, 
                      preProcess = c("zv", "medianImpute"))
Fit_floor_B0_rf_factor
saveRDS(Fit_floor_B0_rf_factor, file = "RF_Fit_floor_B0_factor.rds")

#  mtry  splitrule   Accuracy   Kappa 
#98   extratrees  0.9972075  0.9962597

training_B1_floor_rf_factor <- training_B1_floor %>% 
  mutate(FLOOR = as.factor(FLOOR))
Fit_floor_B1_rf_factor <- train(FLOOR~., 
                      data = training_B1_floor_rf_factor, 
                      method = "ranger", 
                      preProcess = c("zv", "medianImpute"))
Fit_floor_B1_rf_factor
saveRDS(Fit_floor_B1_rf_factor, file = "RF_Fit_floor_B1_factor.rds")
#mtry  splitrule   Accuracy   Kappa    
# 98   extratrees  0.9966053  0.9954529  

training_B2_floor_rf_factor <- training_B2_floor %>% 
  mutate(FLOOR = as.factor(FLOOR))
Fit_floor_B2__rf_factor <- train(FLOOR~., 
                      data = training_B2_floor_rf_factor, 
                      method = "ranger", 
                      trControl=fitControl,
                      preProcess = c("zv", "medianImpute"))
Fit_floor_B2_rf_factor <- Fit_floor_B2__rf_factor
saveRDS(Fit_floor_B2_rf_factor, file = "RF_Fit_floor_B2_factor.rds")
##  mtry  splitrule   Accuracy   Kappa 
# 101   extratrees  0.9983088  0.9978414