library(readr)
library(caret)
library(tidyverse)
library(ranger)
library(e1071)

# Load data 7500 sample & filtered per building, no set to factors----
ModelData <- read_rds("ModelData.rds")
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
#k   RMSE       Rsquared   MAE      
#5   6.460450  0.9588620  3.660657

#Fit_lat_B0 postresample----
postResample(pred = predict(object = Fit_lat_B0, 
                            newdata = testing_B0_lat), 
                            obs = testing_B0_lat$LATITUDE)
#   RMSE     Rsquared MAE 
#   6.7003103 0.9609583 4.0250448

#Predict Output----
predictions_KNNB0Lat= predict(Fit_lat_B0, testing_B0_lat[, 1:520])

#Evaluate predictions----
table(predictions_KNNB0Lat)
str(predictions_KNNB0Lat)

#Confusion matrix----
####REVIEW#### 
confusionMatrix(predictions_KNNB0Lat[, 521])

Fit_lat_B1 <- train(LATITUDE~., 
                    data = training_B1_lat, 
                    method = "kknn", 
                    trControl=fitControl, 
                    tuneLength = 5,
                    verboseIter = TRUE,
                    preProcess = c("zv", "medianImpute"))
Fit_lat_B1
saveRDS(Fit_lat_B1, file = "KNN_Fit_lat_B1.rds")
#k   RMSE       Rsquared   MAE      
#5   8.241445  0.9482391  4.979434

#Fit_lat_B1 postresample----
postResample(pred = predict(object = Fit_lat_B1, 
                            newdata = testing_B1_lat), 
             obs = testing_B1_lat$LATITUDE)
#   RMSE     Rsquared MAE 
#   6.7663285 0.9645302 4.4480253

#Predict Output----
predictions_KNNB1Lat= predict(Fit_lat_B1, testing_B1_lat[, 1:520])

#Evaluate predictions----
table(predictions_KNNB1Lat)
str(predictions_KNNB1Lat)

#Confusion matrix----
####REVIEW#### 
confusionMatrix(predictions_KNNB1Lat, testing_B1_lat[, 521])

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
#5   7.088801  0.9365169  3.989857

#Fit_lat_B2 postresample----
postResample(pred = predict(object = Fit_lat_B2, 
                            newdata = testing_B2_lat), 
             obs = testing_B2_lat$LATITUDE)
#   RMSE      Rsquared  MAE 
#   6.0978035 0.9547484 3.4121850 

#Predict Output----
predictions_KNNB2Lat= predict(Fit_lat_B2, testing_B2_lat[, 1:520])

#Evaluate predictions----
table(predictions_KNNB2Lat)
str(predictions_KNNB2Lat)

#Confusion matrix----
####REVIEW#### 
confusionMatrix(predictions_KNNB2Lat[,521])

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
#k   RMSE       Rsquared   MAE      
#5   7.311088  0.9158382  4.482322

#Fit_long_B0 postresample----
postResample(pred = predict(object = Fit_long_B0, 
                            newdata = testing_B0_long), 
             obs = testing_B0_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#   6.5256682 0.9337054 4.1474910 

#Predict Output----
predictions_KNNB0Long= predict(Fit_long_B0, testing_B0_long[, 1:520])

#Evaluate predictions----
table(predictions_KNNB0Long)
str(predictions_KNNB0Long)

#Confusion matrix----
####REVIEW#### 
confusionMatrix(predictions_KNNB0Long, testing_B0_long[,521])

Fit_long_B1 <- train(LONGITUDE~., 
                    data = training_B1_long, 
                    method = "kknn", 
                    trControl=fitControl, 
                    tuneLength = 5,
                    verboseIter = TRUE,
                    preProcess = c("zv", "medianImpute"))
Fit_long_B1
saveRDS(Fit_long_B1, file = "KNN_Fit_long_B1.rds")
#k  RMSE       Rsquared   MAE      
#5  8.538631  0.9698672  5.201769

#Fit_long_B1 postresample----
postResample(pred = predict(object = Fit_long_B1, 
                            newdata = testing_B1_long), 
             obs = testing_B1_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#   7.890551 0.974103 5.055423

#Predict Output----
predictions_KNNB1Long= predict(Fit_long_B1, testing_B1_long[, 1:520])

#Evaluate predictions----
table(predictions_KNNB1Long)
str(predictions_KNNB1Long)

#Confusion matrix----
####REVIEW#### 
confusionMatrix(predictions_KNNB1Long, testing_B1_long[, 521])

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
#5   9.455415  0.8973102  5.114687

#Fit_long_B2 postresample----
postResample(pred = predict(object = Fit_long_B2, 
                            newdata = testing_B2_long), 
             obs = testing_B2_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#   9.2788440 0.9006733 4.9395021

#Predict Output----
predictions_KNNB2Long= predict(Fit_long_B2, testing_B2_long[, 1:520])

#Evaluate predictions----
table(predictions_KNNB2Long)
str(predictions_KNNB2Long)

#Confusion matrix----
####REVIEW#### 
confusionMatrix(predictions_KNNB2Long, testing_B2_long[,521])

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
#5   0.8679156  0.8235844

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
#5   0.867244  0.8226709

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
#k   Accuracy   Kappa     
#5   0.9276070  0.9073784

