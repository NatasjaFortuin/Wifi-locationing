library(readr)
library(caret)
library(ggplot2)

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

## Floor----
predictions_KNNB0Floor= predict(object = KNNB0Floor, newdata = Test_B0_Floor)

KNNB0Floor
#kmax  Accuracy   Kappa    
#5    0.8679156  0.8235844

#Confusion matrix & KPI----
CF_B0_Floor <- table(predictions_KNNB0Floor, Test_B0_Floor$FLOOR)
accuracy_KNNB0Floor <- (sum(diag(CF_B0_Floor))) / sum(CF_B0_Floor)
accuracy_KNNB0Floor <- accuracy_KNNB0Floor * 100
accuracy_KNNB0Floor

# B1----
## Lat----
predictions_KNNB1Lat= predict(object = KNNB1Lat, newdata = Test_B1_lat)

# Lat KPI----
postResample(pred = predictions_KNNB1Lat, obs = Test_B1_lat$LATITUDE)
#    RMSE  Rsquared       MAE 
# 6.7663285 0.9645302 4.4480253

## Long----
predictions_KNNB1Long= predict(Fit_long_B1, Test_B1_long)

# Long KPI----
postResample(pred = predictions_KNNB1Long, obs = Test_B1_long$LONGITUDE)
#   MSE Rsquared      MAE 
#7.890551 0.974103 5.055423 

## Floor----
predictions_KNNB1Floor= predict(object = KNNB1Floor, newdata = Test_B1_Floor)

KNNB1Floor
#kmax  Accuracy   Kappa    
#5    0.867244  0.8226709

#Confusion matrix & KPI----
CF_B1_Floor <- table(predictions_KNNB1Floor, Test_B1_Floor$FLOOR)
accuracy_KNNB1Floor <- (sum(diag(CF_B1_Floor))) / sum(CF_B1_Floor)
accuracy_KNNB1Floor <- accuracy_KNNB1Floor * 100
accuracy_KNNB1Floor
confusionMatrix(CF_B1_Floor)

# B2----
## Lat----
predictions_KNNB2Lat= predict(Fit_lat_B2, Test_B2_lat)

#Lat KPI----
postResample(pred = predictions_KNNB2Lat, obs = Test_B2_lat$LATITUDE)
#RMSE       Rsquared       MAE 
#6.0978035  0.9547484 3.4121850 

## Long----
predictions_KNNB2Long= predict(Fit_long_B2, Test_B2_long)

#Long KPI----
postResample(pred = predictions_KNNB2Long, obs = Test_B2_long$LONGITUDE)
#RMSE       Rsquared       MAE 
#9.2788440  0.9006733 4.9395021  

## Floor----
predictions_KNNB2Floor= predict(object = KNNB2Floor, newdata = Test_B2_Floor)

KNNB2Floor
#kmax  Accuracy   Kappa    
#5    0.9276070  0.9073784

#Confusion matrix & KPI----
CF_B2_Floor <- table(predictions_KNNB2Floor, Test_B2_Floor$FLOOR)
accuracy_KNNB2Floor <- (sum(diag(CF_B2_Floor))) / sum(CF_B2_Floor)
accuracy_KNNB2Floor <- accuracy_KNNB2Floor * 100
accuracy_KNNB2Floor
confusionMatrix(CF_B2_Floor)
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

#Build dataframe with predictions

