library(readr)
library(caret)
library(tidyverse)
library(kknn)


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

#tuneGrid----
tuneGrid <- expand.grid(kmax = 1:11,  # allows to test a range of k values
                       distance = 1:10,  # allows to test a range of distance values
                       kernel = 'triangular')

#### TRAIN B0, B1, B2 for lat/long/floor ####

# predict the location of a user in a building (long/lat/floor) based on WAP's

## LATITUDE----

Fit_lat_B0 <- train(LATITUDE ~., 
                    data = training_B0_lat, 
                    method = "kknn", 
                    trControl=fitControl, 
                    tuneLength = 5,
                    tuneGrid = tuneGrid,
                    verboseIter = TRUE,
                    preProcess = c("zv"))

Fit_lat_B0
saveRDS(Fit_lat_B0, file = "KNN_Fit_lat_B0.rds")
#kmax Distance RMSE      Rsquared   MAE     
#1     1        2.906570  0.9920867  1.111332

#Fit_lat_B0 postresample----
postResample(pred = predict(object = Fit_lat_B0, 
                            newdata = testing_B0_lat), 
             obs = testing_B0_lat$LATITUDE)
#   RMSE      Rsquared    MAE 
#   2.7840031 0.9929337 1.0855267

Fit_lat_B1 <- train(LATITUDE~., 
                    data = training_B1_lat, 
                    method = "kknn", 
                    trControl=fitControl, 
                    tuneLength = 5,
                    tuneGrid = tuneGrid,
                    verboseIter = TRUE,
                    preProcess = c("zv"))
Fit_lat_B1
saveRDS(Fit_lat_B1, file = "KNN_Fit_lat_B1.rds")
#k    Distance RMSE        Rsquared    MAE      
# 1     1        4.594654  0.9841134  1.345218

#Fit_lat_B1 postresample----
postResample(pred = predict(object = Fit_lat_B1, 
                            newdata = testing_B1_lat), 
             obs = testing_B1_lat$LATITUDE)
#  RMSE      Rsquared  MAE 
#  4.8582787 0.9819695 2.0469997

Fit_lat_B2 <- train(LATITUDE~., 
                    data = training_B2_lat, 
                    method = "kknn", 
                    trControl=fitControl, 
                    tuneLength = 11,
                    verboseIter = TRUE,
                    preProcess = c("zv"))
Fit_lat_B2
 saveRDS(Fit_lat_B2, file = "KNN_Fit_lat_B2.rds")
#k   RMSE       Rsquared   MAE      
#5  3.957463  0.9798979  1.735383

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
                     tuneLength = 11,
                     verboseIter = TRUE,
                     preProcess = c("zv"))
   Fit_long_B0
saveRDS(Fit_long_B0, file = "KNN_Fit_long_B0.rds")
#k    RMSE       Rsquared   MAE      
#5    3.478721  0.9809065  1.700386

#Fit_long_B0 postresample----
postResample(pred = predict(object = Fit_long_B0, 
                            newdata = testing_B0_long), 
             obs = testing_B0_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#   3.2758884 0.9825139 1.5674880   

Fit_long_B1 <- train(LONGITUDE~., 
                     data = training_B1_long, 
                     method = "kknn", 
                     trControl=fitControl, 
                     tuneLength = 5,
                     tuneGrid = tuneGrid,
                     verboseIter = TRUE,
                     preProcess = c("zv"))
Fit_long_B1
saveRDS(Fit_long_B1, file = "KNN_Fit_long_B1.rds")
#k    RMSE       Rsquared   MAE      
#5   5.200595  0.9886392  2.093923

#Fit_long_B1 postresample----
postResample(pred = predict(object = Fit_long_B1, 
                            newdata = testing_B1_long), 
             obs = testing_B1_long$LONGITUDE)
#   Kmax  Distance    RMSE      Rsquared    MAE 
#    1     1        5.042662  0.9897146  1.479346 

Fit_long_B2 <- train(LONGITUDE~., 
                     data = training_B2_long, 
                     method = "kknn", 
                     trControl=fitControl, 
                     tuneLength = 11,
                     verboseIter = TRUE,
                     preProcess = c("zv"))
  Fit_long_B2
saveRDS(Fit_long_B2, file = "KNN_Fit_long_B2.rds")
#k   RMSE       Rsquared   MAE      
#5    4.904144  0.9730170  1.937362  

#Fit_long_B2 postresample----
postResample(pred = predict(object = Fit_long_B2, 
                            newdata = testing_B2_long), 
             obs = testing_B2_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#   4.9858825 0.9731629 1.7948391   

## FLOOR----

#B0_Floor set to factor----
training_B0_floor_factor <- training_B0_floor %>% 
  mutate(FLOOR = as.character(FLOOR))
Fit_floor_B0_factor <- train(FLOOR~., 
                      data = training_B0_floor_factor, 
                      method = "kknn", 
                      trControl=fitControl, 
                      tuneLength = 11,
                      verboseIter = TRUE,
                      preProcess = c("zv"))
Fit_floor_B0_factor
saveRDS(Fit_floor_B0_factor, file = "KNN_Fit_floor_B0_factor.rds")

#kmax  Accuracy   Kappa    
#5    0.9828625  0.9770594

training_B1_floor_factor <- training_B1_floor %>% 
  mutate(FLOOR = as.character(FLOOR))
Fit_floor_B1_factor <- train(FLOOR~., 
                      data = training_B1_floor_factor, 
                      method = "kknn", 
                      trControl=fitControl,
                      tuneLength = 5,
                      tuneGrid = tuneGrid,
                      verboseIter = TRUE,
                      preProcess = c("zv"))
Fit_floor_B1_factor
saveRDS(Fit_floor_B1_factor, file = "KNN_Fit_floor_B1_factor.rds")
#kmax  distance  Accuracy   Kappa    
#1     1        0.9989664  0.9986084

training_B2_floor_factor <- training_B2_floor %>% 
  mutate(FLOOR = as.character(FLOOR))
Fit_floor_B2_factor <- train(FLOOR~., 
                      data = training_B2_floor_factor, 
                      method = "kknn", 
                      trControl=fitControl, 
                      tuneLength = 11,
                      verboseIter = TRUE,
                      preProcess = c("zv"))
Fit_floor_B2_factor
saveRDS(Fit_floor_B2_factor, file = "KNN_Fit_floor_B2_factor.rds")
#k    Accuracy   Kappa     
#5    0.9925034  0.9899095