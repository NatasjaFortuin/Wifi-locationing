library(readr)
library(caret)
library(tidyverse)
library(ranger)
library(gmodels)

# Load data----
trainingData <- read_csv("Data etc/trainingData.csv")
validationData <- read_csv("Data etc/validationData.csv")

#How much of the values are not detected and therefore 100----
# Remove columns (WAP) where all the values = 100 (WAP was not detected)
uniquelength <- sapply(trainingData,function(x) length(unique(x)))
trainingData <- subset(trainingData, select=uniquelength>1)
nrow(trainingData) #19937 rows
ncol(trainingData) #removed are all columns (55) with only WAP100. Left = 474

uniquelength_v <- sapply(validationData,function(x) length(unique(x)))
validationData <- subset(validationData, select=uniquelength>1)
nrow(validationData) #1111 rows
ncol(validationData) #removed are all columns (55) with only WAP100. Left = 474

# Remove rows (WAP) where all the values = 100 (WAP was not detected)----
keep <- apply(trainingData[,1:465], 1, function(x) length(unique(x[!is.na(x)])) != 1)
trainingData <- trainingData[keep, ]
nrow(trainingData) #19861 85 rows removed

keep <- apply(validationData[,1:465], 1, function(x) length(unique(x[!is.na(x)])) != 1)
validationData <- validationData[keep, ]
nrow(validationData) #1111
ncol(validationData) #474

# Change WAP values so that no signal is 0 and highest signal is 104----
trainingData[trainingData == 100] <- -105 #if 100 than make it -105
trainingData[,1:465] <- trainingData[,1:465] + 105  #make all WAP's positive 
#by adding +105
summary(trainingData$WAP111) #check conversion of WAP's to positives
nrow(trainingData)

validationData[validationData == 100] <- -105 # if 100 than make it -105
validationData[,1:465] <- validationData[,1:465] + 105  #make all WAP's positive 
#by adding +105
summary(validationData$WAP108) #check conversion of WAP's to positives

# Converting data types----
trainingData$FLOOR <- as.factor(trainingData$FLOOR)
trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)
validationData$FLOOR <- as.factor(validationData$FLOOR)
validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
ncol(trainingData)
nrow(trainingData)
ncol(validationData)
nrow(validationData)

# subset full data per building
B0 <- subset(trainingData, BUILDINGID == 0)
saveRDS(B0, file = "Final_B0_Data.rds")
B1 <- subset(trainingData, BUILDINGID == 1)
saveRDS(B1, file = "Final_B1_Data.rds")
B2 <- subset(trainingData, BUILDINGID == 2)
saveRDS(B2, file = "Final_B2_Data.rds")

B0_v <- subset(validationData, BUILDINGID == 0)
saveRDS(B0, file = "Final_B0_Data_v.rds")
B1_v <- subset(validationData, BUILDINGID == 1)
saveRDS(B1, file = "Final_B1_Data_v.rds")
B2_v <- subset(validationData, BUILDINGID == 2)
saveRDS(B2, file = "Final_B2_Data_v.rds")

#Create lat/long/floor sets for validationdata----
B0_lat_v <- B0_v %>% 
  select(starts_with("WAP"), LATITUDE)
B1_lat_v <- B1_v %>% 
  select(starts_with("WAP"), LATITUDE)
B2_lat_v <- B2_v %>% 
  select(starts_with("WAP"), LATITUDE)

B0_long_v <- B0_v %>% 
  select(starts_with("WAP"), LONGITUDE)
B1_long_v <- B1_v %>% 
  select(starts_with("WAP"), LONGITUDE)
B2_long_v <- B2_v %>% 
  select(starts_with("WAP"), LONGITUDE)

B0_floor_v <- B0_v %>% 
  select(starts_with("WAP"), FLOOR)
B1_floor_v <- B1_v %>% 
  select(starts_with("WAP"), FLOOR)
B2_floor_v <- B2_v %>% 
  select(starts_with("WAP"), FLOOR)

# Load trained KNN model
KNNB0Lat <- read_rds("KNN_Fit_lat_B0.rds")
KNNB1Lat <- read_rds("KNN_Fit_lat_B1.rds")
KNNB2Lat <- read_rds("KNN_Fit_lat_B2.rds")

KNNB0Long <- read_rds("KNN_Fit_long_B0.rds")
KNNB1Long <- read_rds("KNN_Fit_long_B1.rds")
KNNB2Long <- read_rds("KNN_Fit_long_B2.rds")

KNNB0Floor <- read_rds("KNN_Fit_floor_B0_factor.rds")
KNNB1Floor<- read_rds("KNN_Fit_floor_B1_factor.rds")
KNNB2Floor <- read_rds("KNN_Fit_floor_B2_factor.rds")

#### PREDICT ####
## B0
FP_KNNB0Lat= predict(object = KNNB0Lat, newdata = B0_lat_v)

# Lat KPI----
postResample(pred = FP_KNNB0Lat, obs = B0_lat_v$LATITUDE)
#   RMSE      Rsquared   MAE 
# 12.0127158  0.8582513  6.6947009

error_FP_KNNB0Lat <- FP_KNNB0Lat - B0_lat_v$LATITUDE 
rmse_FP_KNNB0Lat <- sqrt(mean(error_FP_KNNB0Lat^2))
rmse_FP_KNNB0Lat
rsquared_FP_KNNB0Lat <- 1 - (sum(error_FP_KNNB0Lat^2) / 
                            sum((B0_lat_v$LATITUDE-mean(B0_lat_v$LATITUDE))^2))
rsquared_FP_KNNB0Lat <- rsquared_FP_KNNB0Lat * 100
rsquared_FP_KNNB0Lat
MAE_FP_KNN_B0Lat <- MAE(FP_KNNB0Lat, B0_lat_v$LATITUDE)
MAE_FP_KNN_B0Lat

## B0----
FP_KNNB0Long= predict(object = KNNB0Long, newdata = B0_long_v)

# Long KPI----
postResample(pred = FP_KNNB0Long, obs = B0_long_v$LONGITUDE)
#   RMSE      Rsquared   MAE 
# 9.3569221 0.8751301   5.6989023

error_FP_KNNB0Long <- FP_KNNB0Long - B0_long_v$LONGITUDE 
rmse_FP_KNNB0Long <- sqrt(mean(error_FP_KNNB0Long^2))
rmse_FP_KNNB0Long
rsquared_FP_KNNB0Long <- 1 - (sum(error_FP_KNNB0Long^2) / 
                               sum((B0_long_v$LONGITUDE-mean(B0_long_v$LONGITUDE))^2))
rsquared_FP_KNNB0Long <- rsquared_FP_KNNB0Long * 100
rsquared_FP_KNNB0Long
MAE_FP_KNN_B0Long <- MAE(FP_KNNB0Long, B0_long_v$LONGITUDE)
MAE_FP_KNN_B0Long

FP_KNNB0Floor= predict(object = KNNB0Floor, newdata = B0_floor_v)

#Floor KPI----
FP_CF_B0_Floor <- confusionMatrix(FP_KNNB0Floor, B0_floor_v$FLOOR)
FP_CF_B0_Floor
table_FP_CF_B0_Floor <- table(FP_KNNB0Floor, B0_floor_v$FLOOR)
accuracy_FP_KNNB0Floor <- (sum(diag(table_FP_CF_B0_Floor))) / sum(table_FP_CF_B0_Floor)
accuracy_FP_KNNB0Floor <- accuracy_FP_KNNB0Floor * 100
accuracy_FP_KNNB0Floor
FP_CF_B0_Floor <- confusionMatrix(table_FP_CF_B0_Floor)
FP_CF_B0_Floor

#B1----

## Latitude
FP_KNNB1Lat= predict(object = KNNB1Lat, newdata = B1_lat_v)

# Lat KPI----
postResample(pred = FP_KNNB1Lat, obs = B1_lat_v$LATITUDE)
#   RMSE      Rsquared   MAE 
# 13.6616805  0.8534788  7.9349223 

error_FP_KNNB1Lat <- FP_KNNB1Lat - B1_lat_v$LATITUDE 
rmse_FP_KNNB1Lat <- sqrt(mean(error_FP_KNNB1Lat^2))
rmse_FP_KNNB1Lat
rsquared_FP_KNNB1Lat <- 1 - (sum(error_FP_KNNB1Lat^2) / 
                               sum((B1_lat_v$LATITUDE-mean(B1_lat_v$LATITUDE))^2))
rsquared_FP_KNNB1Lat <- rsquared_FP_KNNB1Lat * 100
rsquared_FP_KNNB1Lat
MAE_FP_KNN_B1Lat <- MAE(FP_KNNB1Lat, B1_lat_v$LATITUDE)
MAE_FP_KNN_B1Lat

## Longitude
FP_KNNB1Long= predict(object = KNNB1Long, newdata = B1_long_v)

# Long KPI----
postResample(pred = FP_KNNB1Long, obs = B1_long_v$LONGITUDE)
#   RMSE      Rsquared   MAE 
# 12.6285945  0.9273226  7.9146620

error_FP_KNNB1Long <- FP_KNNB1Long - B1_long_v$LONGITUDE 
rmse_FP_KNNB1Long <- sqrt(mean(error_FP_KNNB1Long^2))
rmse_FP_KNNB1Long
rsquared_FP_KNNB1Long <- 1 - (sum(error_FP_KNNB1Long^2) / 
                                sum((B1_long_v$LONGITUDE-mean(B1_long_v$LONGITUDE))^2))
rsquared_FP_KNNB1Long <- rsquared_FP_KNNB1Long * 100
rsquared_FP_KNNB1Long
MAE_FP_KNN_B1Long <- MAE(FP_KNNB1Long, B1_long_v$LONGITUDE)
MAE_FP_KNN_B1Long

## Floor
FP_KNNB1Floor= predict(object = KNNB1Floor, newdata = B1_floor_v)

#Floor KPI----
FP_CF_B1_Floor <- confusionMatrix(FP_KNNB1Floor, B1_floor_v$FLOOR)
FP_CF_B1_Floor
table_FP_CF_B1_Floor <- table(FP_KNNB1Floor, B1_floor_v$FLOOR)
accuracy_FP_KNNB1Floor <- (sum(diag(table_FP_CF_B1_Floor))) / sum(table_FP_CF_B1_Floor)
accuracy_FP_KNNB1Floor <- accuracy_FP_KNNB1Floor * 100
accuracy_FP_KNNB1Floor
FP_CF_B1_Floor <- confusionMatrix(table_FP_CF_B1_Floor)
FP_CF_B1_Floor

#B2----
## Latitude
FP_KNNB2Lat= predict(object = KNNB2Lat, newdata = B2_lat_v)

# Lat KPI----
postResample(pred = FP_KNNB2Lat, obs = B2_lat_v$LATITUDE)
#   RMSE      Rsquared   MAE 
# 13.8167746  0.7834094  8.5473528  

error_FP_KNNB2Lat <- FP_KNNB2Lat - B2_lat_v$LATITUDE 
rmse_FP_KNNB2Lat <- sqrt(mean(error_FP_KNNB2Lat^2))
rmse_FP_KNNB2Lat
rsquared_FP_KNNB2Lat <- 1 - (sum(error_FP_KNNB2Lat^2) / 
                               sum((B2_lat_v$LATITUDE-mean(B2_lat_v$LATITUDE))^2))
rsquared_FP_KNNB2Lat <- rsquared_FP_KNNB2Lat * 100
rsquared_FP_KNNB2Lat
MAE_FP_KNN_B2Lat <- MAE(FP_KNNB2Lat, B2_lat_v$LATITUDE)
MAE_FP_KNN_B2Lat

## Longitude
FP_KNNB2Long= predict(object = KNNB2Long, newdata = B2_long_v)

# Long KPI----
postResample(pred = FP_KNNB2Long, obs = B2_long_v$LONGITUDE)
#   RMSE      Rsquared   MAE 
# 12.8153956  0.8364717  8.3325801

error_FP_KNNB2Long <- FP_KNNB2Long - B2_long_v$LONGITUDE 
rmse_FP_KNNB2Long <- sqrt(mean(error_FP_KNNB2Long^2))
rmse_FP_KNNB2Long
rsquared_FP_KNNB2Long <- 1 - (sum(error_FP_KNNB2Long^2) / 
                                sum((B2_long_v$LONGITUDE-mean(B2_long_v$LONGITUDE))^2))
rsquared_FP_KNNB2Long <- rsquared_FP_KNNB2Long * 100
rsquared_FP_KNNB2Long
MAE_FP_KNN_B2Long <- MAE(FP_KNNB2Long, B2_long_v$LONGITUDE)
MAE_FP_KNN_B2Long

## Floor
FP_KNNB2Floor= predict(object = KNNB2Floor, newdata = B2_floor_v)

#Floor KPI----
FP_CF_B2_Floor <- confusionMatrix(FP_KNNB2Floor, B2_floor_v$FLOOR)
FP_CF_B2_Floor
table_FP_CF_B2_Floor <- table(FP_KNNB2Floor, B2_floor_v$FLOOR)
accuracy_FP_KNNB2Floor <- (sum(diag(table_FP_CF_B2_Floor))) / sum(table_FP_CF_B2_Floor)
accuracy_FP_KNNB2Floor <- accuracy_FP_KNNB2Floor * 100
accuracy_FP_KNNB2Floor
FP_CF_B2_Floor <- confusionMatrix(table_FP_CF_B2_Floor)
FP_CF_B2_Floor

# CREATE DF's for KPI check & PLOTS----
## LATITUDE

## All Lat KPI's per Floor----
FP_Combi_StatSum_Lat <- data.frame( RMSE = c(rmse_FP_KNNB0Lat, 
                                          rmse_FP_KNNB1Lat, 
                                          rmse_FP_KNNB2Lat),
                                 RSQ = c(rsquared_FP_KNNB0Lat, 
                                         rsquared_FP_KNNB1Lat, 
                                         rsquared_FP_KNNB2Lat),
                                 MAE = c(MAE_FP_KNN_B0Lat, 
                                         MAE_FP_KNN_B1Lat, 
                                         MAE_FP_KNN_B2Lat),
                                 row.names = c("B0","B1","B2"))

## All Long KPI's per Floor----
FP_Combi_StatSum_Long <- data.frame( RMSE = c(rmse_FP_KNNB0Long, 
                                           rmse_FP_KNNB1Long, 
                                           rmse_FP_KNNB2Long),
                                  RSQ = c(rsquared_FP_KNNB0Long, 
                                          rsquared_FP_KNNB1Long, 
                                          rsquared_FP_KNNB2Long),
                                  MAE = c(MAE_FP_KNN_B0Long, 
                                          MAE_FP_KNN_B1Long, 
                                          MAE_FP_KNN_B2Long),
                                  row.names = c("B0","B1","B2"))

## KPI RESULTS Lat & Long----
FP_Combi_StatSum_Lat
#   RMSE      RSQ      MAE
#B0 12.01272 85.79767 6.694701
#B1 13.66168 84.82729 7.934922
#B2 13.81677 76.50689 8.547353

FP_Combi_StatSum_Long
#   RMSE      RSQ      MAE
#B0  9.356922 87.15947 5.698902
#B1 12.628595 92.37601 7.914662
#B2 12.815396 83.32801 8.332580

## KPI RESULTS Floor----
FP_Combi_StatSum_Floor_acc <- data.frame(acc = c(
  accuracy_FP_KNNB0Floor,
  accuracy_FP_KNNB1Floor,
  accuracy_FP_KNNB2Floor),
  row.names = c("B0","B1","B2"))

FP_Combi_StatSum_Floor_kappa <- data.frame(kappa = c(kappa_FP_KNNB0Floor$Unweighted,
                                                  kappa_FP_KNNB1Floor$Unweighted, 
                                                  kappa_FP_KNNB2Floor$Unweighted))
Combi_StatSum_Floor_acc #B1 has a low accuracy, needs check
acc
#B0 98.09160
#B1 36.22964
#B2 98.94157

kappa_FP_KNNB0Floor <- Kappa(table_FP_CF_B0_Floor)
kappa_FP_KNNB0Floor
kappa_FP_KNNB1Floor <- Kappa(table_FP_CF_B1_Floor)
kappa_FP_KNNB1Floor
kappa_FP_KNNB2Floor <- Kappa(table_FP_CF_B2_Floor)
kappa_FP_KNNB2Floor

FP_Combi_StatSum_Floor_kappa
#kappa


#### PLOT CF as Crosstable #### 
CrossTable(table_CF_B0_Floor, prop.chisq = FALSE, dnn = c('predicted', 'actual'))
CrossTable(table_CF_B1_Floor, prop.chisq = FALSE, dnn = c('predicted', 'actual'))
CrossTable(table_CF_B2_Floor, prop.chisq = FALSE, dnn = c('predicted', 'actual'))



#### IF MORE MODELS COMPARE THEM WITH RESAMPLING ####

#In order to use resamples on your three trained models you should use the 
#following format:

#ModelData <- resamples(list(KNN = KNNB0Lat, SVM = ----, RF = -----))

#Summary(ModelData)

#Here is an example of the output showing the respective metrics for each model: