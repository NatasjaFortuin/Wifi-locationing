library(readr)
library(caret)
library(lattice)
library(ggplot2)
library(tidyverse)
library(gmodels)
library(vcd)
library(grid)

#### KNN ####

## load saved KNN models----

KNNB0Lat <- read_rds("KNN_Fit_lat_B0.rds")
KNNB1Lat <- read_rds("KNN_Fit_lat_B1.rds")
KNNB2Lat <- read_rds("KNN_Fit_lat_B2.rds")

KNNB0Long <- read_rds("KNN_Fit_long_B0.rds")
KNNB1Long <- read_rds("KNN_Fit_long_B1.rds")
KNNB2Long <- read_rds("KNN_Fit_long_B2.rds")

KNNB0Floor <- read_rds("KNN_Fit_floor_B0_factor.rds")
KNNB1Floor<- read_rds("KNN_Fit_floor_B1_factor.rds")
KNNB2Floor <- read_rds("KNN_Fit_floor_B2_factor.rds")

# load training data----

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

# load test data----

Test_B0_lat <- read_rds("testing_B0_lat.rds")
Test_B1_lat <- read_rds("testing_B1_lat.rds")
Test_B2_lat <- read_rds("testing_B2_lat.rds")
Test_B0_long <- read_rds("testing_B0_long.rds")
Test_B1_long <- read_rds("testing_B1_long.rds")
Test_B2_long <- read_rds("testing_B2_long.rds")
Test_B0_Floor <- read_rds("testing_B0_floor.rds")
Test_B1_Floor <- read_rds("testing_B1_floor.rds")
Test_B1_Floor <- Test_B1_Floor %>% 
  mutate(FLOOR = as.factor(FLOOR))
Test_B2_Floor <- read_rds("testing_B2_floor.rds")

#### PREDICT & CHECK KPI  on test data ####

# B0----
## Lat----
predictions_KNNB0Lat= predict(object = KNNB0Lat, newdata = Test_B0_lat)

# Lat KPI----
postResample(pred = predictions_KNNB0Lat, obs = Test_B0_lat$LATITUDE)
#   RMSE      Rsquared       MAE 
#   4.2003490 0.9840869 2.0675697

error_KNNB0Lat <- predictions_KNNB0Lat - Test_B0_lat$LATITUDE 
rmse_KNNB0Lat <- sqrt(mean(error_KNNB0Lat^2))
rmse_KNNB0Lat
rsquared_KNNB0Lat <- 1 - (sum(error_KNNB0Lat^2) / 
                  sum((Test_B0_lat$LATITUDE-mean(Test_B0_lat$LATITUDE))^2))
rsquared_KNNB0Lat <- rsquared_KNNB0Lat * 100
rsquared_KNNB0Lat
MAE_KNN_B0Lat <- MAE(predictions_KNNB0Lat, Test_B0_lat$LATITUDE)

## Long----
predictions_KNNB0Long= predict(object = KNNB0Long, newdata = Test_B0_long)

# Long KPI----
postResample(pred = predictions_KNNB0Long, obs = Test_B0_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#   3.2452671 0.9828383 1.6137860

error_KNNB0Long <- predictions_KNNB0Long - Test_B0_long$LONGITUDE 
rmse_KNNB0Long <- sqrt(mean(error_KNNB0Long^2))
rmse_KNNB0Long
rsquared_KNNB0Long <- 1 - (sum(error_KNNB0Long^2) / 
                  sum((Test_B0_long$LONGITUDE-mean(Test_B0_long$LONGITUDE))^2))
rsquared_KNNB0Long <- rsquared_KNNB0Long * 100
rsquared_KNNB0Long
MAE_KNN_B0Long <- MAE(predictions_KNNB0Long, Test_B0_long$LONGITUDE)

## Floor----
predictions_KNNB0Floor= predict(object = KNNB0Floor, newdata = Test_B0_Floor)

#Confusion matrix & KPI----
CF_B0_Floor <- confusionMatrix(data = predictions_KNNB0Floor ,Test_B0_Floor$FLOOR)
CF_B0_Floor
table_CF_B0_Floor <- table(predictions_KNNB0Floor, Test_B0_Floor$FLOOR)
accuracy_KNNB0Floor <- (sum(diag(table_CF_B0_Floor))) / sum(table_CF_B0_Floor)
accuracy_KNNB0Floor <- accuracy_KNNB0Floor * 100
accuracy_KNNB0Floor
CF_B0_Floor <- confusionMatrix(table_CF_B0_Floor)
CF_B0_Floor

# B1----
## Lat----
predictions_KNNB1Lat= predict(object = KNNB1Lat, newdata = Test_B1_lat)

# Lat KPI----
postResample(pred = predictions_KNNB1Lat, obs = Test_B1_lat$LATITUDE)
#    RMSE     Rsquared  MAE 
#   5.5894955 0.9765521 1.9777480 

error_KNNB1Lat <- predictions_KNNB0Lat - Test_B0_lat$LATITUDE 
rmse_KNNB1Lat <- sqrt(mean(error_KNNB1Lat^2))
rmse_KNNB1Lat
rsquared_KNNB1Lat <- 1 - (sum(error_KNNB1Lat^2) / 
                            sum((Test_B1_lat$LATITUDE-mean(Test_B1_lat$LATITUDE))^2))
rsquared_KNNB1Lat <- rsquared_KNNB1Lat * 100
rsquared_KNNB1Lat
MAE_KNN_B1Lat <- MAE(predictions_KNNB1Lat, Test_B1_lat$LATITUDE)

## Long----
predictions_KNNB1Long= predict(KNNB1Long, Test_B1_long)

# Long KPI----
postResample(pred = predictions_KNNB1Long, obs = Test_B1_long$LONGITUDE)
# RMSE      Rsquared  MAE 
# 3.746984 0.994300 1.248799 

error_KNNB1Long <- predictions_KNNB1Long - Test_B1_long$LONGITUDE 
rmse_KNNB1Long <- sqrt(mean(error_KNNB1Long^2))
rmse_KNNB1Long
rsquared_KNNB1Long <- 1 - (sum(error_KNNB1Long^2) / 
                             sum((Test_B1_long$LONGITUDE-mean(Test_B1_long$LONGITUDE))^2))
rsquared_KNNB1Long <- rsquared_KNNB1Long * 100
rsquared_KNNB1Long
MAE_KNN_B1Long <- MAE(predictions_KNNB1Long, Test_B1_long$LONGITUDE)

## Floor----
predictions_KNNB1Floor= predict(object = KNNB1Floor, newdata = Test_B1_Floor)

#Confusion matrix & KPI----
CF_B1_Floor <- confusionMatrix(data = predictions_KNNB1Floor ,Test_B1_Floor$FLOOR)
CF_B1_Floor
table_CF_B1_Floor <- table(predictions_KNNB1Floor, Test_B1_Floor$FLOOR)
accuracy_KNNB1Floor <- (sum(diag(table_CF_B1_Floor))) / sum(table_CF_B1_Floor)
accuracy_KNNB1Floor <- accuracy_KNNB1Floor * 100
accuracy_KNNB1Floor
CF_B1_Floor <- confusionMatrix(table_CF_B1_Floor)
CF_B1_Floor

# B2----
## Lat----
predictions_KNNB2Lat= predict(KNNB2Lat, Test_B2_lat)

#Lat KPI----
postResample(pred = predictions_KNNB2Lat, obs = Test_B2_lat$LATITUDE)
#RMSE       Rsquared       MAE 
#3.6507010 0.9833021 1.8463397  

error_KNNB2Lat <- predictions_KNNB2Lat - Test_B2_lat$LATITUDE 
rmse_KNNB2Lat <- sqrt(mean(error_KNNB2Lat^2))
rmse_KNNB2Lat
rsquared_KNNB2Lat <- 1 - (sum(error_KNNB2Lat^2) / 
                            sum((Test_B2_lat$LATITUDE-mean(Test_B2_lat$LATITUDE))^2))
rsquared_KNNB2Lat <- rsquared_KNNB2Lat * 100
rsquared_KNNB2Lat
MAE_KNN_B2Lat <- MAE(predictions_KNNB2Lat, Test_B2_lat$LATITUDE)

## Long----
predictions_KNNB2Long= predict(KNNB2Long, Test_B2_long)

#Long KPI----
postResample(pred = predictions_KNNB2Long, obs = Test_B2_long$LONGITUDE)
# RMSE       Rsquared MAE 
# 4.2692812 0.9798539 1.8419951  

error_KNNB2long <-predictions_KNNB2Long - Test_B2_long$LONGITUDE 
rmse_KNNB2Long <- sqrt(mean(error_KNNB2long^2))
rmse_KNNB2Long
rsquared_KNNB2Long <- 1 - (sum(error_KNNB2long^2) / 
                            sum((Test_B2_long$LONGITUDE-mean(Test_B2_long$LONGITUDE))^2))
rsquared_KNNB2Long <- rsquared_KNNB2Long * 100
rsquared_KNNB2Long
MAE_KNN_B2Long <- MAE(predictions_KNNB2Long, Test_B2_long$LONGITUDE)

## Floor----
predictions_KNNB2Floor= predict(object = KNNB2Floor, newdata = Test_B2_Floor)

#Confusion matrix & KPI----
CF_B2_Floor <- confusionMatrix(data = predictions_KNNB2Floor ,Test_B2_Floor$FLOOR)
CF_B2_Floor
table_CF_B2_Floor <- table(predictions_KNNB2Floor, Test_B2_Floor$FLOOR)
accuracy_KNNB2Floor <- (sum(diag(table_CF_B2_Floor))) / sum(table_CF_B2_Floor)
accuracy_KNNB2Floor <- accuracy_KNNB2Floor * 100
accuracy_KNNB2Floor
CF_B1_Floor <- confusionMatrix(table_CF_B1_Floor)
CF_B1_Floor

# CREATE DF's for KPI check & PLOTS----
## LATITUDE

## All Lat KPI's per Floor----
Combi_StatSum_Lat <- data.frame( RMSE = c(rmse_KNNB0Lat, 
                                            rmse_KNNB1Lat, 
                                            rmse_KNNB2Lat),
                                  RSQ = c(rsquared_KNNB0Lat, 
                                            rsquared_KNNB1Lat, 
                                            rsquared_KNNB2Lat),
                                  MAE = c(MAE_KNN_B0Lat, 
                                          MAE_KNN_B1Lat, 
                                           MAE_KNN_B2Lat),
                                  row.names = c("B0","B1","B2"))

## All Long KPI's per Floor----
Combi_StatSum_Long <- data.frame( RMSE = c(rmse_KNNB0Long, 
                                          rmse_KNNB1Long, 
                                          rmse_KNNB2Long),
                                 RSQ = c(rsquared_KNNB0Long, 
                                         rsquared_KNNB1Long, 
                                         rsquared_KNNB2Long),
                                 MAE = c(MAE_KNN_B0Long, 
                                         MAE_KNN_B1Long, 
                                         MAE_KNN_B2Long),
                                row.names = c("B0","B1","B2"))

## KPI RESULTS Lat & Long----
Combi_StatSum_Lat
#   RMSE      RSQ      MAE
#B0 4.200349 98.38193 2.067570
#B1 4.200349 98.62613 1.977748
#B2 3.650701 98.32693 1.846340

Combi_StatSum_Long
#   RMSE      RSQ      MAE
#B0 3.245267 98.27749 1.613786
#B1 3.746984 99.42940 1.248799
#B2 4.269281 97.92495 1.841995

## KPI RESULTS Floor----
Combi_StatSum_Floor_acc <- data.frame(acc = c(
                      accuracy_KNNB0Floor,
                      accuracy_KNNB1Floor,
                      accuracy_KNNB2Floor),
                      row.names = c("B0","B1","B2"))

Combi_StatSum_Floor_kappa <- data.frame(kappa = c(kappa_KNNB0Floor$Unweighted, 
                                                 kappa_KNNB1Floor$Unweighted, 
                                                 kappa_KNNB2Floor$Unweighted))
Combi_StatSum_Floor_acc #B1 has a low accuracy, needs check
acc
#B0 98.09160
#B1 36.22964
#B2 98.94157
Combi_StatSum_Floor_kappa
#kappa


#### PLOT CF as Crosstable #### 
CrossTable(table_CF_B0_Floor, prop.chisq = FALSE, dnn = c('predicted', 'actual'))
CrossTable(table_CF_B1_Floor, prop.chisq = FALSE, dnn = c('predicted', 'actual'))
CrossTable(table_CF_B2_Floor, prop.chisq = FALSE, dnn = c('predicted', 'actual'))
kappa_KNNB0Floor <- Kappa(table_CF_B0_Floor)
kappa_KNNB0Floor
kappa_KNNB1Floor <- Kappa(table_CF_B1_Floor)
kappa_KNNB1Floor
kappa_KNNB2Floor <- Kappa(table_CF_B2_Floor)
kappa_KNNB2Floor


#### IF MORE MODELS COMPARE THEM WITH RESAMPLING ####

#In order to use resamples on your three trained models you should use the 
#following format:
  
#ModelData <- resamples(list(KNN = KNNB0Lat, SVM = ----, RF = -----))

#Summary(ModelData)

#Here is an example of the output showing the respective metrics for each model:
  
  