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

RFB0Lat <- read_rds("RF_Fit_lat_B0.rds")
RFB1Lat <- read_rds("RF_Fit_lat_B1.rds")
RFB2Lat <- read_rds("RF_Fit_lat_B2.rds")

RFB0Long <- read_rds("RF_Fit_long_B0.rds")
RFB1Long <- read_rds("RF_Fit_long_B1.rds")
RFB2Long <- read_rds("RF_Fit_long_B2.rds")

RFB0Floor <- read_rds("RF_Fit_floor_B0_factor.rds")
RFB1Floor<- read_rds("RF_Fit_floor_B1_factor.rds")
RFB2Floor<- read_rds("RF_Fit_floor_B2_factor.rds")

# load test data----

Test_B0_lat <- read_rds("testing_B0_lat.rds")
Test_B1_lat <- read_rds("testing_B1_lat.rds")
Test_B2_lat <- read_rds("testing_B2_lat.rds")
Test_B0_long <- read_rds("testing_B0_long.rds")
Test_B1_long <- read_rds("testing_B1_long.rds")
Test_B2_long <- read_rds("testing_B2_long.rds")
Test_B0_Floor <- read_rds("testing_B0_floor.rds")
Test_B1_Floor <- read_rds("testing_B1_floor.rds")
Test_B2_Floor <- read_rds("testing_B2_floor.rds")

#### PREDICT & CHECK KPI  on test data ####

# B0----
## Lat----
predictions_RFB0Lat= predict(object = RFB0Lat, newdata = Test_B0_lat)

# Lat KPI----
postResample(pred = predictions_RFB0Lat, obs = Test_B0_lat$LATITUDE)
#   RMSE      Rsquared       MAE 
# 2.2772682 0.9953698 1.3147164

error_RFB0Lat <- predictions_RFB0Lat - Test_B0_lat$LATITUDE 
rmse_RFB0Lat <- sqrt(mean(error_RFB0Lat^2))
rmse_RFB0Lat
rsquared_RFB0Lat <- 1 - (sum(error_RFB0Lat^2) / 
                  sum((Test_B0_lat$LATITUDE-mean(Test_B0_lat$LATITUDE))^2))
rsquared_RFB0Lat <- rsquared_RFB0Lat * 100
rsquared_RFB0Lat
MAE_RF_B0Lat <- MAE(predictions_RFB0Lat, Test_B0_lat$LATITUDE)

## Long----
predictions_RFB0Long= predict(object = RFB0Long, newdata = Test_B0_long)

# Long KPI----
postResample(pred = predictions_RFB0Long, obs = Test_B0_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#  2.6748828 0.9888481 1.7395135 

error_RFB0Long <- predictions_RFB0Long - Test_B0_long$LONGITUDE 
rmse_RFB0Long <- sqrt(mean(error_KNNB0Long^2))
rmse_RFB0Long
rsquared_RFB0Long <- 1 - (sum(error_RFB0Long^2) / 
                  sum((Test_B0_long$LONGITUDE-mean(Test_B0_long$LONGITUDE))^2))
rsquared_RFB0Long <- rsquared_RFB0Long * 100
rsquared_RFB0Long
MAE_RF_B0Long <- MAE(predictions_RFB0Long, Test_B0_long$LONGITUDE)

## Floor----
predictions_RFB0Floor= predict(object = RFB0Floor, newdata = Test_B0_Floor)

#Confusion matrix & KPI----
RF_CF_B0_Floor <- confusionMatrix(RFB0Floor)
RF_CF_B0_Floor
table_RF_CF_B0_Floor <- table(predictions_RFB0Floor, Test_B0_Floor$FLOOR)
accuracy_RFB0Floor <- (sum(diag(table_RF_CF_B0_Floor))) / sum(table_RF_CF_B0_Floor)
accuracy_RFB0Floor <- accuracy_RFB0Floor * 100
accuracy_RFB0Floor
RF_CF_B0_Floor <- confusionMatrix(table_RF_CF_B0_Floor)
RF_CF_B0_Floor

# B1----
## Lat----
predictions_RFB1Lat= predict(object = RFB1Lat, newdata = Test_B1_lat)

# Lat KPI----
postResample(pred = predictions_RFB1Lat, obs = Test_B1_lat$LATITUDE)
#    RMSE     Rsquared  MAE 
# 3.5814652 0.9902493 2.1510434    

error_RFB1Lat <- predictions_RFB0Lat - Test_B0_lat$LATITUDE 
rmse_RFB1Lat <- sqrt(mean(error_RFB1Lat^2))
rmse_RFB1Lat
rsquared_RFB1Lat <- 1 - (sum(error_RFB1Lat^2) / 
                            sum((Test_B1_lat$LATITUDE-mean(Test_B1_lat$LATITUDE))^2))
rsquared_RFB1Lat <- rsquared_RFB1Lat * 100
rsquared_RFB1Lat
MAE_RF_B1Lat <- MAE(predictions_RFB1Lat, Test_B1_lat$LATITUDE)

## Long----
predictions_RFB1Long= predict(RFB1Long, Test_B1_long)

# Long KPI----
postResample(pred = predictions_RFB1Long, obs = Test_B1_long$LONGITUDE)
# RMSE      Rsquared  MAE 
#4.3462200 0.9923514 2.4202011

error_RFB1Long <- predictions_RFB1Long - Test_B1_long$LONGITUDE 
rmse_RFB1Long <- sqrt(mean(error_RFB1Long^2))
rmse_RFB1Long
rsquared_RFB1Long <- 1 - (sum(error_RFB1Long^2) / 
                             sum((Test_B1_long$LONGITUDE-mean(Test_B1_long$LONGITUDE))^2))
rsquared_RFB1Long <- rsquared_RFB1Long * 100
rsquared_RFB1Long
MAE_RF_B1Long <- MAE(predictions_RFB1Long, Test_B1_long$LONGITUDE)

## Floor----
predictions_RFB1Floor= predict(object = RFB1Floor, newdata = Test_B1_Floor)

#Confusion matrix & KPI----
RF_CF_B1_Floor <- confusionMatrix(RFB1Floor)
RF_CF_B1_Floor
table_RF_CF_B1_Floor <- table(predictions_RFB1Floor, Test_B1_Floor$FLOOR)
accuracy_RFB1Floor <- (sum(diag(table_RF_CF_B1_Floor))) / sum(table_RF_CF_B1_Floor)
accuracy_RFB1Floor <- accuracy_RFB1Floor * 100
accuracy_RFB1Floor
RF_CF_B1_Floor <- confusionMatrix(table_RF_CF_B1_Floor)
RF_CF_B1_Floor

# B2----
## Lat----
predictions_RFB2Lat= predict(RFB2Lat, Test_B2_lat)

#Lat KPI----
postResample(pred = predictions_RFB2Lat, obs = Test_B2_lat$LATITUDE)
#RMSE       Rsquared       MAE 
#3.3257597 0.9865359 1.9903211  

error_RFB2Lat <- predictions_RFB2Lat - Test_B2_lat$LATITUDE 
rmse_RFB2Lat <- sqrt(mean(error_RFB2Lat^2))
rmse_RFB2Lat
rsquared_RFB2Lat <- 1 - (sum(error_RFB2Lat^2) / 
                            sum((Test_B2_lat$LATITUDE-mean(Test_B2_lat$LATITUDE))^2))
rsquared_RFB2Lat <- rsquared_RFB2Lat * 100
rsquared_RFB2Lat
MAE_RF_B2Lat <- MAE(predictions_RFB2Lat, Test_B2_lat$LATITUDE)

## Long----
predictions_RFB2Long= predict(RFB2Long, Test_B2_long)

#Long KPI----
postResample(pred = predictions_RFB2Long, obs = Test_B2_long$LONGITUDE)
# RMSE       Rsquared MAE 
# 

error_RFB2long <-predictions_RFB2Long - Test_B2_long$LONGITUDE 
rmse_RFB2Long <- sqrt(mean(error_RFB2long^2))
rmse_RFB2Long
rsquared_RFB2Long <- 1 - (sum(error_RFB2long^2) / 
                            sum((Test_B2_long$LONGITUDE-mean(Test_B2_long$LONGITUDE))^2))
rsquared_RFB2Long <- rsquared_RFB2Long * 100
rsquared_RFB2Long
MAE_RF_B2Long <- MAE(predictions_RFB2Long, Test_B2_long$LONGITUDE)

## Floor----
predictions_RFB2Floor= predict(object = RFB2Floor, newdata = Test_B2_Floor)

#Confusion matrix & KPI----
RF_CF_B2_Floor <- confusionMatrix(RFB2Floor)
RF_CF_B2_Floor
table_RF_CF_B2_Floor <- table(predictions_RFB2Floor, Test_B2_Floor$FLOOR)
accuracy_RFB2Floor <- (sum(diag(table_RF_CF_B2_Floor))) / sum(table_RF_CF_B2_Floor)
accuracy_RFB2Floor <- accuracy_KNNB2Floor * 100
accuracy_RFB2Floor
RF_CF_B1_Floor <- confusionMatrix(table_RF_CF_B1_Floor)
RF_CF_B1_Floor

# CREATE DF's for KPI check & PLOTS----
## LATITUDE

## All Lat KPI's per Floor----
Combi_StatSum_Lat <- data.frame( RMSE = c(rmse_RFB0Lat, 
                                            rmse_RFB1Lat, 
                                            rmse_RFB2Lat),
                                  RSQ = c(rsquared_RFB0Lat, 
                                            rsquared_RFB1Lat, 
                                            rsquared_RFB2Lat),
                                  MAE = c(MAE_RF_B0Lat, 
                                          MAE_RF_B1Lat, 
                                           MAE_RF_B2Lat),
                                  row.names = c("B0","B1","B2"))

## All Long KPI's per Floor----
Combi_StatSum_Long <- data.frame( RMSE = c(rmse_RFB0Long, 
                                          rmse_RFB1Long, 
                                          rmse_RFB2Long),
                                 RSQ = c(rsquared_RFB0Long, 
                                         rsquared_RFB1Long, 
                                         rsquared_RFB2Long),
                                 MAE = c(MAE_RF_B0Long, 
                                         MAE_RF_B1Long, 
                                         MAE_RF_B2Long),
                                row.names = c("B0","B1","B2"))

## KPI RESULTS Lat & Long----
Combi_StatSum_Lat
#   RMSE      RSQ      MAE
#B0 2.277268 99.52439 1.314716
#B1 2.277268 99.59617 2.151043
#B2 3.325760 98.61151 1.990321

Combi_StatSum_Long
#   RMSE      RSQ      MAE
#B0 3.245267 98.82977 1.739514
#B1 4.346220 99.23230 2.420201
#B2 4.437505 97.79701 2.424726

## KPI RESULTS Floor----
RF_Combi_StatSum_Floor_acc <- data.frame(acc = c(
                      accuracy_RFB0Floor,
                      accuracy_RFB1Floor,
                      accuracy_RFB2Floor),
                      row.names = c("B0","B1","B2"))

##### REVIEW#####
RF_Combi_StatSum_Floor_kappa <- data.frame(kappa = c(kappa_RFB0Floor$Unweighted, 
                                                 kappa_RFB1Floor$Unweighted, 
                                                 kappa_RFB2Floor$Unweighted))
RF_Combi_StatSum_Floor_acc #B1 has a low accuracy, needs check
#acc
#B0   99.77099
#B1   28.70442
#B2 9894.15749

RF_Combi_StatSum_Floor_kappa
#kappa


#### PLOT CF as Crosstable #### 
CrossTable(table_RF_CF_B0_Floor, prop.chisq = FALSE, dnn = c('predicted', 'actual'))
CrossTable(table_RF_CF_B1_Floor, prop.chisq = FALSE, dnn = c('predicted', 'actual'))
CrossTable(table_RF_CF_B2_Floor, prop.chisq = FALSE, dnn = c('predicted', 'actual'))
kappa_RFB0Floor <- Kappa(table_RF_CF_B0_Floor)
kappa_RFB0Floor
kappa_RFB1Floor <- Kappa(table_RF_CF_B1_Floor)
kappa_RFB1Floor
kappa_RFB2Floor <- Kappa(table_RF_CF_B2_Floor)
kappa_RFB2Floor


#### IF MORE MODELS COMPARE THEM WITH RESAMPLING ####

#In order to use resamples on your three trained models you should use the 
#following format:
  
#ModelData <- resamples(list(KNN = KNNB0Lat, SVM = ----, RF = -----))

#Summary(ModelData)

#Here is an example of the output showing the respective metrics for each model:
  
  