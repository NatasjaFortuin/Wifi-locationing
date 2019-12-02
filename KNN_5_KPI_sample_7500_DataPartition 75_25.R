library(readr)
library(caret)
library(lattice)
library(ggplot2)
library(tidyverse)
library(gmodels)

#### KNN ####

## load saved KNN models----

KNNB0Lat <- read_rds("KNN_Fit_lat_B0.rds")
KNNB1Lat <- read_rds("KNN_Fit_lat_B1.rds")
KNNB2Lat <- read_rds("KNN_Fit_lat_B2.rds")

KNNB0Long <- read_rds("KNN_Fit_long_B0.rds")
KNNB1Long <- read_rds("KNN_Fit_long_B1.rds")
KNNB2Long <- read_rds("KNN_Fit_long_B2.rds")

KNNB0Floor <- read_rds("KNN_Fit_floor_B0.rds")
KNNB1Floor<- read_rds("KNN_Fit_floor_B1.rds")
KNNB2Floor <- read_rds("KNN_Fit_floor_B2.rds")

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
training_B1_floor <- training_B0_floor %>% 
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
Test_B2_Floor <- read_rds("testing_B2_floor.rds")

#### PREDICT & CHECK KPI  on test data ####

# B0----
## Lat----
predictions_KNNB0Lat= predict(object = KNNB0Lat, newdata = Test_B0_lat)

# Lat KPI----
postResample(pred = predictions_KNNB0Lat, obs = Test_B0_lat$LATITUDE)
#    RMSE  Rsquared       MAE 
#6.7003103 0.9609583 4.0250448 

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
#    RMSE  Rsquared       MAE 
# 6.5256682 0.9337054 4.1474910 

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

KNNB0Floor
#kmax  Accuracy   Kappa    
#5    0.8679156  0.8235844

#Confusion matrix & KPI----
CF_B0_Floor <- confusionMatrix(KNNB0Floor)
table_CF_B0_Floor <- table(predictions_KNNB0Floor, Test_B0_Floor$FLOOR)
CF_BO_Floor
accuracy_KNNB0Floor <- (sum(diag(CF_B0_Floor))) / sum(CF_B0_Floor)
accuracy_KNNB0Floor <- accuracy_KNNB0Floor * 100
accuracy_KNNB0Floor
CF_BO_Floor <- confusionMatrix(CF_B0_Floor)

#predictions_KNNB0 - B0 noticeble errors, needs check 
# 0/1 = 12
# 1/0 = 5, 1/2 = 4
# 2/1 = 8, 2/3 = 13
# 3/2 = 15

#           PREDICTIONS_KNNB0Floor   
#     A         0   1   2   3 floors
#     C     0  79  12   1   0
#     T     1   5 120   4   0
#     U     2   0   8 116  13
#     AL    3   0   1  15 119

#Overall Statistics
#Accuracy : 0.8803          
#95% CI : (0.8484, 0.9076)
#Kappa : 0.839  

# B1----
## Lat----
predictions_KNNB1Lat= predict(object = KNNB1Lat, newdata = Test_B1_lat)

# Lat KPI----
postResample(pred = predictions_KNNB1Lat, obs = Test_B1_lat$LATITUDE)
#    RMSE  Rsquared       MAE 
# 6.7663285 0.9645302 4.4480253

error_KNNB1Lat <- predictions_KNNB0Lat - Test_B0_lat$LATITUDE 
rmse_KNNB1Lat <- sqrt(mean(error_KNNB1Lat^2))
rmse_KNNB1Lat
rsquared_KNNB1Lat <- 1 - (sum(error_KNNB1Lat^2) / 
                            sum((Test_B1_lat$LATITUDE-mean(Test_B1_lat$LATITUDE))^2))
rsquared_KNNB1Lat <- rsquared_KNNB1Lat * 100
rsquared_KNNB1Lat
MAE_KNN_B1Lat <- MAE(predictions_KNNB1Lat, Test_B1_lat$LATITUDE)

## Long----
predictions_KNNB1Long= predict(Fit_long_B1, Test_B1_long)

# Long KPI----
postResample(pred = predictions_KNNB1Long, obs = Test_B1_long$LONGITUDE)
#   MSE Rsquared      MAE 
#7.890551 0.974103 5.055423 

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

KNNB1Floor
#kmax  Accuracy   Kappa    
#5    0.867244  0.8226709

#Confusion matrix & KPI----
CF_B1_Floor <- confusionMatrix(KNNB1Floor)
CF_B1_Floor
table_CF_B1_Floor <- table(predictions_KNNB1Floor, Test_B1_Floor$FLOOR)
accuracy_KNNB1Floor <- (sum(diag(CF_B1_Floor))) / sum(CF_B1_Floor)
accuracy_KNNB1Floor <- accuracy_KNNB1Floor * 100
accuracy_KNNB1Floor

#predictions_KNNB1 - Floor noticeble errors, needs check 
# 0/1 = 77, 0/2 = 95, 0/3 = 55
# 1/0 = 14
# 2/1 = 20, 2/3 = 19
#PREDICTED B1 -   0   1   2   3 floors
#A            0 114  77  95  55
#C            1  14  38   6   0
#T            2   0  20  25  19
#UAL          3   0   3   9  12

#Overall Statistics
#Accuracy : 0.3881         
#95% CI : (0.3446, 0.433)
#Kappa : 0.1698   

# B2----
## Lat----
predictions_KNNB2Lat= predict(Fit_lat_B2, Test_B2_lat)

#Lat KPI----
postResample(pred = predictions_KNNB2Lat, obs = Test_B2_lat$LATITUDE)
#RMSE       Rsquared       MAE 
#6.0978035  0.9547484 3.4121850 

error_KNNB2Lat <- predictions_KNNB2Lat - Test_B2_lat$LATITUDE 
rmse_KNNB2Lat <- sqrt(mean(error_KNNB2Lat^2))
rmse_KNNB2Lat
rsquared_KNNB2Lat <- 1 - (sum(error_KNNB2Lat^2) / 
                            sum((Test_B2_lat$LATITUDE-mean(Test_B2_lat$LATITUDE))^2))
rsquared_KNNB2Lat <- rsquared_KNNB2Lat * 100
rsquared_KNNB2Lat
MAE_KNN_B2Lat <- MAE(predictions_KNNB2Lat, Test_B2_lat$LATITUDE)

## Long----
predictions_KNNB2Long= predict(Fit_long_B2, Test_B2_long)

#Long KPI----
postResample(pred = predictions_KNNB2Long, obs = Test_B2_long$LONGITUDE)
#RMSE       Rsquared       MAE 
#9.2788440  0.9006733 4.9395021  

error_KNNB2long <-predictions_KNNB2Long - Test_B2_long$LONGITUDE 
rmse_KNNB2Long <- sqrt(mean(error_KNNB2long^2))
rmse_KNNB2Long
rsquared_KNNB2Long <- 1 - (sum(error_KNNB2long^2) / 
                            sum((Test_B2_lat$LONGITUDE-mean(Test_B2_long$LONGITUDE))^2))
rsquared_KNNB2Long <- rsquared_KNNB2Long * 100
rsquared_KNNB2Long
MAE_KNN_B2Long <- MAE(predictions_KNNB2Long, Test_B2_long$LONGITUDE)

## Floor----
predictions_KNNB2Floor= predict(object = KNNB2Floor, newdata = Test_B2_Floor)

KNNB2Floor
#kmax  Accuracy   Kappa    
#5    0.9276070  0.9073784

#Confusion matrix & KPI----
CF_B2_Floor <- confusionMatrix(KNNB1Floor)
CF_B2_Floor
table_CF_B2_Floor <- table(predictions_KNNB2Floor, Test_B2_Floor$FLOOR)
accuracy_KNNB2Floor <- (sum(diag(CF_B2_Floor))) / sum(CF_B2_Floor)
accuracy_KNNB2Floor <- accuracy_KNNB2Floor * 100
accuracy_KNNB2Floor

#predictions_KNNB2 - B2 noticeble errors, needs check 
# 0/2 = 10, 0/3 = 3
# 1/2 = 8
# 3/2 = 14, 3/4 = 6
# 4/3 = 3
#PREDICTED B2 -      0   1   2   3   4 floors
#           A    0 180  10   1   3   0
#           C    1   3 179   8   3   0
#           T    2   0   1 127   3   2
#           U    3   0   4  14 251   6
#           AL   4   0   0   0   3  93

#Overall Statistics
#Accuracy : 0.9315          
#95% CI : (0.9129, 0.9472)
#Kappa : 0.9122   


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
#RMSE         RSQ      MAE
#B0 6.700310  96.02481 4.025045
#B1 6.700310  96.47526 4.448025
#B2 6.097803  95.41785 3.412185

Combi_StatSum_Long
#RMSE         RSQ      MAE
#B0 6.525668 93.29991 4.147491
#B1 7.890551 97.40207 5.055423
#B2 9.278844 90.12502 4.939502

## All accuracy Floor metrics----
Combi_StatSum_Floor <- data.frame(acc = c(
                      accuracy_KNNB0Floor,
                      accuracy_KNNB1Floor,
                      accuracy_KNNB2Floor),
                      row.names = c("B0","B1","B2"))

Combi_StatSum_Floor #B1 has a low accuracy, needs check

## ACCURACY RESULTS Floor per building----

#accuracy metrics per building
#B0 88.03245
#B1 38.80903
#B2 93.15376

Combi_StatSum_Floor

#### PLOT CF as Crosstable #### 
CrossTable(table_CF_B0_Floor, prop.chisq = FALSE, dnn = c('predicted', 'actual'))
CrossTable(table_CF_B1_Floor, prop.chisq = FALSE, dnn = c('predicted', 'actual'))
CrossTable(table_CF_B2_Floor, prop.chisq = FALSE, dnn = c('predicted', 'actual'))


