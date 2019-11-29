library(readr)
library(caret)
library(lattice)
library(ggplot2)
library(dplyr)

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
CF_B2_Floor <- table(predictions_KNNB2Floor, Test_B2_Floor$FLOOR)
accuracy_KNNB2Floor <- (sum(diag(CF_B2_Floor))) / sum(CF_B2_Floor)
accuracy_KNNB2Floor <- accuracy_KNNB2Floor * 100
accuracy_KNNB2Floor
confusionMatrix(CF_B2_Floor)

#Create df B2 for ConfusionMatrix----
df_CF_B2_Floor <- tibble(table_CF_B2_Floor)

#### REVIEW #### 
#hoe maak ik hier drie kolommen RMSE, RSQUARED, MAE van voor vergelijking
#en kun je die op een zinvolle manier plotten?

# Create df with summary stats for all buildings & predictions----

StatSum_Lat <- tibble( 
                      rmse_KNNB0Lat,
                      rmse_KNNB1Lat,
                      rmse_KNNB2Lat,
                      rsquared_KNNB0Lat,
                      rsquared_KNNB1Lat,
                      rsquared_KNNB2Lat,
                      MAE_KNN_B0Lat,
                      MAE_KNN_B1Lat,
                      MAE_KNN_B2Lat)

View(StatSum_Lat)
#### REVIEW #### 3 column, 3 rows with the values
Combi_StatSum_Lat_RMSE <- bind_cols(by = c(rmse_KNNB0Lat, 
                                           rmse_KNNB1Lat, 
                                           rmse_KNNB2Lat))
Combi_StatSum_Lat_RSQ <- bind_cols(by = c(rsquared_KNNB0Lat, 
                                          rsquared_KNNB1Lat, 
                                          rsquared_KNNB2Lat))
Combi_StatSum_Lat_MAE <- bind_cols(by=c(MAE_KNN_B0Lat, 
                                        MAE_KNN_B1Lat, 
                                        MAE_KNN_B2Lat))

Combi_StatSum_Lat <- bind_cols(Combi_StatSum_Lat_RMSE, Combi_StatSum_Lat_RSQ)
Combi_StatSum_Lat <- bind_cols(Combi_StatSum_Lat, Combi_StatSum_Lat_MAE)
#### REVIEW####
Combi_StatSum_Lat <-  rename(Combi_StatSum_Lat, by=RMSE, by1=RSQ, by2=MAE)
head(Combi_StatSum_Lat)

StatSum_Long <- tibble(
                      rmse_KNNB0Long,
                      rmse_KNNB1Long,
                      rmse_KNNB2Long,
                      rsquared_KNNB0Long,
                      rsquared_KNNB1Long,
                      rsquared_KNNB2Long,
                      MAE_KNN_B0Long,
                      MAE_KNN_B1Long,
                      MAE_KNN_B2Long)
View(StatSum_Long)

Combi_StatSum_Long_RMSE <- bind_cols(by = c(rmse_KNNB0Long, 
                                           rmse_KNNB1Long, 
                                           rmse_KNNB2Long))
Combi_StatSum_Long_RSQ <- bind_cols(by = c(rsquared_KNNB0Long, 
                                          rsquared_KNNB1Long, 
                                          rsquared_KNNB2Long))
Combi_StatSum_Long_MAE <- bind_cols(by=c(MAE_KNN_B0Long, 
                                        MAE_KNN_B1Long, 
                                        MAE_KNN_B2Long))

Combi_StatSum_Long <- bind_cols(Combi_StatSum_Long_RMSE, Combi_StatSum_Long_RSQ)
Combi_StatSum_Long <- bind_cols(Combi_StatSum_Long, Combi_StatSum_Long_MAE)

StatSum_Floor <- tibble(
                      accuracy_KNNB0Floor,
                      accuracy_KNNB1Floor,
                      accuracy_KNNB2Floor)
head(StatSum_Floor)
Combi_StatSum_Floor <- bind_cols(by=c(accuracy_KNNB0Floor, accuracy_KNNB1Floor))

#### REVIEW #### krijg de 3e variabele er niet bij??
Combi_StatSum_Floor <- bind_cols(by=c(Combi_StatSum_Floor, accuracy_KNNB2Floor))

Combi_StatSum_Floor
View(StatSum_Floor)

#### REVIEW #### can we create tibble although B2 has 5 floors?----
StatSum_CF_Floor <- tibble(
                     CF_B0_Floor,
                     CF_B1_Floor,
                     CF_B2_Floor)

#### PLOTS #### I would like to plot rmse, rsquared, MAE, accuracy for 
# Lat, Long, Floor per metric in 1 plot and together per B0, B1, B2 with...

#StatSum_Lat
#StatSum_Long
#StatSum_Floor

#Something like this...

Plot_StatSum_Lat <- StatSum_Lat %>% 
  ggplot(aes(x = metrics, y = values)) + 
  geom_col(aes(fill = metrics)) +
  geom_text(aes(fill = metrics, label = round(values, digits = 3)), colour = "black") +
  coord_flip() +
  labs(x = "Metrics for each Building",
       y = "RMSE",
       title = "LATITUDE") +
  theme_light() +
  scale_fill_brewer(palette = "GnBu") +
  theme(legend.position="none")
Plot_StatSum_Lat

### REVIEW hoe plot je een confusion matrix?? #### 
df_CF_B2_Floor
ggplot(data =  df_CF_B2_Floor, mapping = aes(x = 
              c("0", "1", "2", "3", "4"), y = ????)) +
  geom_tile(aes(fill = value), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f",value)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "steelblue")
  





