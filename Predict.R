library(readr)
library(caret)
library(tidyverse)
library(ranger)
library(gmodels)
library(vcd)

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
trainingData$FLOOR <- as.character(trainingData$FLOOR)
trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)
validationData$FLOOR <- as.character(validationData$FLOOR)
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
## B0----
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
FP_KNNB0Floor

#Floor KPI----
FP_CF_B0_Floor <- confusionMatrix(FP_KNNB0Floor, B0_floor_v$FLOOR)
FP_CF_B0_Floor
table_FP_CF_B0_Floor <- table(FP_KNNB0Floor, B0_floor_v$FLOOR)
table_FP_CF_B0_Floor
accuracy_FP_KNNB0Floor <- (sum(diag(table_FP_CF_B0_Floor))) / sum(table_FP_CF_B0_Floor)
accuracy_FP_KNNB0Floor <- accuracy_FP_KNNB0Floor * 100
accuracy_FP_KNNB0Floor

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
table_FP_CF_B1_Floor <- table(FP_KNNB1Floor, B1_floor_v$FLOOR)
accuracy_FP_KNNB1Floor <- (sum(diag(table_FP_CF_B1_Floor))) / sum(table_FP_CF_B1_Floor)
accuracy_FP_KNNB1Floor <- accuracy_FP_KNNB1Floor * 100
accuracy_FP_KNNB1Floor

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

# CREATE DF's for KPI check & PLOTS----
## LATITUDE

## All Lat KPI's per Building----
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

## All Long KPI's per Building----
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
FP_Combi_StatSum_Floor_acc
#acc
#B0 80.78358
#B1 71.00977
#B2 83.95522

kappa_FP_B0Floor <- Kappa(FP_CF_B0_Floor)
CF_FP_B0Floor <- table_FP_CF_B0_Floor
kappa_FP_B1Floor <- Kappa(FP_CF_B1_Floor)
CF_FP_B1Floor <- table_FP_CF_B1_Floor
kappa_FP_B2Floor <- Kappa(FP_CF_B2_Floor)
CF_FP_B2Floor <- table_FP_CF_B2_Floor

CF_FP_B0Floor
kappa_FP_B0Floor

Combi_StatSum_Floor_kappa <- data.frame(Kappa= c(CF_FP_B0Floor, 
                                      CF_FP_B1Floor, 
                                      CF_FP_B2Floor))


Combi_StatSum_Floor_kappa

#### PLOT CF as Crosstable #### 
CrossTable(table_FP_CF_B0_Floor, prop.chisq = FALSE, dnn = c('predicted', 'actual'))
CrossTable(table_FP_CF_B1_Floor, prop.chisq = FALSE, dnn = c('predicted', 'actual'))
CrossTable(table_FP_CF_B2_Floor, prop.chisq = FALSE, dnn = c('predicted', 'actual'))

#### Create df with predicted values ####
df_pred_KNN_B0_lat <- tibble(FP_KNNB0Lat)
df_pred_KNN_B0_long <- tibble(FP_KNNB0Long)
df_pred_KNN_B1_lat <- tibble(FP_KNNB1Lat)
df_pred_KNN_B1_long <- tibble(FP_KNNB1Long)
df_pred_KNN_B2_lat <- tibble(FP_KNNB2Lat)
df_pred_KNN_B2_long <- tibble(FP_KNNB2Long)

df_pred_KNN_B0_floor <- tibble(FP_KNNB0Floor)
df_pred_KNN_B1_floor <- tibble(FP_KNNB1Floor)
df_pred_KNN_B2_floor <- tibble(FP_KNNB2Floor)

PB0_lat_v <- bind_cols(B0_lat_v, df_pred_KNN_B0_lat)
PB1_lat_v <- bind_cols(B1_lat_v, df_pred_KNN_B1_lat)
PB2_lat_v <- bind_cols(B2_lat_v, df_pred_KNN_B2_lat)

PB0_long_v <- bind_cols(B0_long_v, df_pred_KNN_B0_long)
PB1_long_v <- bind_cols(B1_long_v, df_pred_KNN_B1_long)
PB2_long_v <- bind_cols(B2_long_v, df_pred_KNN_B2_long)
view(df_pred_KNN_B0_floor)

#x = cos(df_pred_KNN_B0_lat) * cos(df_pred_KNN_B0_long)
#y = cos(df_pred_KNN_B0_lat) * sin(df_pred_KNN_B0_long) 
#z = sin(df_pred_KNN_B0_lat)
#xyz <- bind_cols(x, y, z)

df_long_lat_floor_B0 <- bind_cols(x = df_pred_KNN_B0_lat, 
                                  y = df_pred_KNN_B0_long,
                                  z = df_pred_KNN_B0_floor)
df_long_lat_floor_B1 <- bind_cols(x = df_pred_KNN_B1_lat, 
                            y = df_pred_KNN_B1_long,
                            z = df_pred_KNN_B1_floor)
df_long_lat_floor_B2 <- bind_cols(x = df_pred_KNN_B2_lat, 
                            y = df_pred_KNN_B2_long,
                            z = df_pred_KNN_B2_floor)

#### PLOTS ####

# Predicted long/lat per building----
B0Plot <- plot(df_long_lat_B0$FP_KNNB0Long, 
     df_long_lat_B0$FP_KNNB0Lat)

B1Plot <- plot(df_long_lat_B1$FP_KNNB1Long, 
               df_long_lat_B1$FP_KNNB1Lat)

B2Plot <- plot(df_long_lat_B2$FP_KNNB2Long, 
               df_long_lat_B2$FP_KNNB2Lat)


#### 3D scatterplot of floors per building ####
#### Subset Building_Floor ####
# B0----
BB0Floor0 <- subset(df_long_lat_floor_B0, FP_KNNB0Floor == 0)
BB0Floor1 <- subset(df_long_lat_floor_B0, FP_KNNB0Floor == 1)
BB0Floor2 <- subset(df_long_lat_floor_B0, FP_KNNB0Floor == 2)
BB0Floor3 <- subset(df_long_lat_floor_B0, FP_KNNB0Floor == 3)

# B1----
BB1Floor0 <- subset(df_long_lat_floor_B1, FP_KNNB1Floor == 0)
BB1Floor1 <- subset(df_long_lat_floor_B1, FP_KNNB1Floor == 1)
BB1Floor2 <- subset(df_long_lat_floor_B1, FP_KNNB1Floor == 2)
BB1Floor3 <- subset(df_long_lat_floor_B1, FP_KNNB1Floor == 3)

# B2----
BB2Floor0 <- subset(df_long_lat_floor_B2, FP_KNNB2Floor == 0)
BB2Floor1 <- subset(df_long_lat_floor_B2, FP_KNNB2Floor == 1)
BB2Floor2 <- subset(df_long_lat_floor_B2, FP_KNNB2Floor == 2)
BB2Floor3 <- subset(df_long_lat_floor_B2, FP_KNNB2Floor == 3)
BB2Floor4 <- subset(df_long_lat_floor_B2, FP_KNNB2Floor == 4)

## BB0----
BB0Floor0$z <- 0
BB0Floor1$z <- 1
BB0Floor2$z <- 2
BB0Floor3$z <- 3

PPPlotBB0 <- rbind(BB0Floor0,BB0Floor1)
PPPlotBB0 <- rbind(PPPlotBB0,BB0Floor2)
PPPlotBB0 <- rbind(PPPlotBB0,BB0Floor3)
str(PPPlotBB0)
head(PPPlotBB0)

PPPlotBB0 <- PPPlotBB0[,1:4]
z <- PPPlotBB0$z
x <- PPPlotBB0$FP_KNNB0Long
y <- PPPlotBB0$FP_KNNB0Lat
scatterplot3d::scatterplot3d(x, y, z, pch = 20, angle = 45, 
                           main = "Building 0")

# BB1----
BB1Floor0$z <- 0
BB1Floor1$z <- 1
BB1Floor2$z <- 2
BB1Floor3$z <- 3

PPPlotBB1 <- rbind(BB1Floor0,BB1Floor1)
PPPlotBB1 <- rbind(PPPlotBB1,BB1Floor2)
PPPlotBB1 <- rbind(PPPlotBB1,BB1Floor3)
PPPlotBB1 <- PPPlotBB1[,1:4]
a <- PPPlotBB1$z
b <- PPPlotBB1$FP_KNNB1Long
c <- PPPlotBB1$FP_KNNB1Lat
scatterplot3d::scatterplot3d(b, c, a, pch = 20, angle = 45, 
                             main = "Building 1")
# 
# BB2----
BB2Floor0$z <- 0
BB2Floor1$z <- 1
BB2Floor2$z <- 2
BB2Floor3$z <- 3
BB2Floor4$z <- 4

PPPlotBB2 <- rbind(BB2Floor0,BB2Floor1)
PPPlotBB2 <- rbind(PPPlotBB2,BB2Floor2)
PPPlotBB2 <- rbind(PPPlotBB2,BB2Floor3)
PPPlotBB2 <- rbind(PPPlotBB2,BB2Floor4)
PPPlotBB2 <- PPPlotBB2[,1:4]
a <- PPPlotBB2$z
b <- PPPlotBB2$FP_KNNB2Long
c <- PPPlotBB2$FP_KNNB2Lat
scatterplot3d::scatterplot3d(b, c, a, pch = 20, angle = 45, 
                             main = "Building 2")