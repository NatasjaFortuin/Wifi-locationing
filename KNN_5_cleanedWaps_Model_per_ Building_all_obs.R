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
training_B1_floor <- training_B1_floor %>% 
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
#5    4.363868  0.9820019  1.845249

#Fit_lat_B0 postresample----
postResample(pred = predict(object = Fit_lat_B0, 
                            newdata = testing_B0_lat), 
             obs = testing_B0_lat$LATITUDE)
#   RMSE      Rsquared    MAE 
#   4.2222506 0.9838355   1.9643230 

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
#5    4.710868  0.9832066  1.933948

#Fit_lat_B1 postresample----
postResample(pred = predict(object = Fit_lat_B1, 
                            newdata = testing_B1_lat), 
             obs = testing_B1_lat$LATITUDE)
#  RMSE      Rsquared  MAE 
#  4.4818124 0.9847978 2.0615879 

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
#5  4.275687  0.9768972  1.890219

#Fit_lat_B2 postresample----
postResample(pred = predict(object = Fit_lat_B2, 
                            newdata = testing_B2_lat), 
             obs = testing_B2_lat$LATITUDE)
#   RMSE      Rsquared  MAE 
#   3.6507010 0.9833021 1.8463397

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
#5    3.697805  0.9781877  1.809231

#Fit_long_B0 postresample----
postResample(pred = predict(object = Fit_long_B0, 
                            newdata = testing_B0_long), 
             obs = testing_B0_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#   3.2452671 0.9828383 1.6137860  

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
#5    5.041457  0.9897686  1.998913

#Fit_long_B1 postresample----
postResample(pred = predict(object = Fit_long_B1, 
                            newdata = testing_B1_long), 
             obs = testing_B1_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#   5.6309285 0.9870586 1.9145439

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
#5  4.692082  0.9750738  1.995312   

#Fit_long_B2 postresample----
postResample(pred = predict(object = Fit_long_B2, 
                            newdata = testing_B2_long), 
             obs = testing_B2_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#   5.1232674 0.9713132 2.1970896   

## FLOOR----
#B0_Floor not set to factor----
Fit_floor_B0 <- train(FLOOR~., 
                      data = training_B0_floor, 
                      method = "kknn", 
                      trControl=fitControl, 
                      tuneLength = 5,
                      verboseIter = TRUE,
                      preProcess = c("zv", "medianImpute"))
Fit_floor_B0
saveRDS(Fit_floor_B0, file = "KNN_Fit_floor_B0.rds")
#k   Accuracy   Kappa       MAE
#5    0.141775  0.9828638  0.03481947

#B0_Floor set to factor----
training_B0_floor_factor <- training_B0_floor %>% 
  mutate(FLOOR = as.factor(FLOOR))
Fit_floor_B0_factor <- train(FLOOR~., 
                      data = training_B0_floor_factor, 
                      method = "kknn", 
                      trControl=fitControl, 
                      tuneLength = 5,
                      verboseIter = TRUE,
                      preProcess = c("zv", "medianImpute"))
Fit_floor_B0_factor
saveRDS(Fit_floor_B0_factor, file = "KNN_Fit_floor_B0_factor.rds")

#kmax  Accuracy   Kappa    
#5    0.9814624  0.975189

training_B1_floor_factor <- training_B1_floor %>% 
  mutate(FLOOR = as.factor(FLOOR))
Fit_floor_B1_factor <- train(FLOOR~., 
                      data = training_B1_floor_factor, 
                      method = "kknn", 
                      trControl=fitControl,tuneLength = 5,
                      verboseIter = TRUE,
                      preProcess = c("zv"))
Fit_floor_B1_factor
saveRDS(Fit_floor_B1_factor, file = "KNN_Fit_floor_B1_factor.rds")
#k   Accuracy   Kappa     
#5    0.9971583  0.9961736 

training_B2_floor_factor <- training_B2_floor %>% 
  mutate(FLOOR = as.factor(FLOOR))
Fit_floor_B2_factor <- train(FLOOR~., 
                      data = training_B2_floor_factor, 
                      method = "kknn", 
                      trControl=fitControl, 
                      tuneLength = 5,
                      verboseIter = TRUE,
                      preProcess = c("zv", "medianImpute"))
Fit_floor_B2_factor
saveRDS(Fit_floor_B2_factor, file = "KNN_Fit_floor_B2_factor.rds")
#k    Accuracy   Kappa     
#5    0.9826571  0.9778669