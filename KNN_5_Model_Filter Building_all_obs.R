library(readr)
library(caret)
library(tidyverse)
library(ranger)
library(e1071)

# Load data 7500 sample & filtered per building, no set to factors----
Modeldata 
B0 <- read_rds("B0_Data.rds")
B1 <- read_rds("B1_Data.rds")
B2 <- read_rds("B2_Data.rds")

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
training_B1_floor <- training_B0_floor %>% 
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
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

#### TRAIN B0, B1, B2 for lat/long/floor ####

# predict the location of a user in a building (long/lat/floor) based on WAP's

## LATITUDE----

Fit_lat_B0 <- train(LATITUDE ~., 
                  data = training_B0_lat, 
                  method = "kknn", 
                  trControl=fitControl, 
                  tuneLength = 5,
                  verboseIter = TRUE,
                  preProcess = c("zv", "medianImpute"))
Fit_lat_B0
saveRDS(Fit_lat_B0, file = "KNN_Fit_lat_B0.rds")
#kmax   RMSE      Rsquared   MAE     
#5      5.840343  0.9680237  2.806226

#Fit_lat_B0 postresample----
postResample(pred = predict(object = Fit_lat_B0, 
                            newdata = testing_B0_lat), 
                            obs = testing_B0_lat$LATITUDE)
#   RMSE      Rsquared    MAE 
#   5.982871  0.967413    3.168045 

Fit_lat_B1 <- train(LATITUDE~., 
                    data = training_B1_lat, 
                    method = "kknn", 
                    trControl=fitControl, 
                    tuneLength = 5,
                    verboseIter = TRUE,
                    preProcess = c("zv", "medianImpute"))
Fit_lat_B1
saveRDS(Fit_lat_B1, file = "KNN_Fit_lat_B1.rds")
#k    RMSE       Rsquared   MAE      
#5    6.506556  0.9679517  3.369182

#Fit_lat_B1 postresample----
postResample(pred = predict(object = Fit_lat_B1, 
                            newdata = testing_B1_lat), 
             obs = testing_B1_lat$LATITUDE)
#  RMSE      Rsquared  MAE 
#kmax   RMSE      Rsquared   MAE     
#5      5.8600480 0.9735459 2.9833834

Fit_lat_B2 <- train(LATITUDE~., 
                    data = training_B2_lat, 
                    method = "kknn", 
                    trControl=fitControl, 
                    tuneLength = 5,
                    verboseIter = TRUE,
                    preProcess = c("zv", "medianImpute"))
Fit_lat_B2
saveRDS(Fit_lat_B2, file = "KNN_Fit_lat_B2.rds")
#k   RMSE       Rsquared   MAE      
#5   5.645028  0.9601617  2.843604

#Fit_lat_B2 postresample----
postResample(pred = predict(object = Fit_lat_B2, 
                            newdata = testing_B2_lat), 
             obs = testing_B2_lat$LATITUDE)
#   RMSE      Rsquared  MAE 
#   4.9402155 0.9682882 2.5529108 0 

## LONGITUDE----
Fit_long_B0 <- train(LONGITUDE~., 
                    data = training_B0_long, 
                    method = "kknn", 
                    trControl=fitControl, 
                    tuneLength = 5,
                    verboseIter = TRUE,
                    preProcess = c("zv", "medianImpute"))
Fit_long_B0
saveRDS(Fit_long_B0, file = "KNN_Fit_long_B0.rds")
#k    RMSE       Rsquared   MAE      
#5    5.962481  0.9436714  3.075533

#Fit_long_B0 postresample----
postResample(pred = predict(object = Fit_long_B0, 
                            newdata = testing_B0_long), 
             obs = testing_B0_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#   5.6387300 0.9480333 3.0377195  

Fit_long_B1 <- train(LONGITUDE~., 
                    data = training_B1_long, 
                    method = "kknn", 
                    trControl=fitControl, 
                    tuneLength = 5,
                    verboseIter = TRUE,
                    preProcess = c("zv", "medianImpute"))
Fit_long_B1
saveRDS(Fit_long_B1, file = "KNN_Fit_long_B1.rds")
#k    RMSE       Rsquared   MAE      
#5    7.441228  0.9775019  3.678046

#Fit_long_B1 postresample----
postResample(pred = predict(object = Fit_long_B1, 
                            newdata = testing_B1_long), 
             obs = testing_B1_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#   6.6183878 0.9819367 3.3280642

Fit_long_B2 <- train(LONGITUDE~., 
                    data = training_B2_long, 
                    method = "kknn", 
                    trControl=fitControl, 
                    tuneLength = 5,
                    verboseIter = TRUE,
                    preProcess = c("zv", "medianImpute"))
Fit_long_B2
saveRDS(Fit_long_B2, file = "KNN_Fit_long_B2.rds")
#k   RMSE       Rsquared   MAE      
#5    7.577559  0.9354733  3.559215   

#Fit_long_B2 postresample----
postResample(pred = predict(object = Fit_long_B2, 
                            newdata = testing_B2_long), 
             obs = testing_B2_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#   7.3556032 0.9404916 3.3675497   

## FLOOR----
training_B0_floor_factor <- training_B0_floor %>% 
  mutate(FLOOR = as.factor(FLOOR))

Fit_floor_B0 <- train(FLOOR~., 
                     data = training_B0_floor_factor, 
                     method = "kknn", 
                     trControl=fitControl, 
                     tuneLength = 5,
                     verboseIter = TRUE,
                     preProcess = c("zv", "medianImpute"))
Fit_floor_B0
saveRDS(Fit_floor_B0, file = "KNN_Fit_floor_B0.rds")
#k   Accuracy   Kappa     
#13    0.9223176  0.8961545   

training_B1_floor_factor <- training_B1_floor %>% 
  mutate(FLOOR = as.factor(FLOOR))
Fit_floor_B1 <- train(FLOOR~., 
                     data = training_B1_floor_factor, 
                     method = "kknn", 
                     trControl=fitControl,tuneLength = 5,
                     verboseIter = TRUE,
                     preProcess = c("zv", "medianImpute"))
Fit_floor_B1
saveRDS(Fit_floor_B1, file = "KNN_Fit_floor_B1.rds")
#k   Accuracy   Kappa     
#7   0.9157089  0.8873319   

training_B2_floor_factor <- training_B2_floor %>% 
  mutate(FLOOR = as.factor(FLOOR))
Fit_floor_B2 <- train(FLOOR~., 
                     data = training_B2_floor_factor, 
                     method = "kknn", 
                     trControl=fitControl, 
                     tuneLength = 5,
                     verboseIter = TRUE,
                     preProcess = c("zv", "medianImpute"))
Fit_floor_B2
saveRDS(Fit_floor_B2, file = "KNN_Fit_floor_B2.rds")
#k    Accuracy   Kappa     
#7    0.9603923  0.9494419   



