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

x = cos(df_pred_KNN_B0_lat) * cos(df_pred_KNN_B0_long)
y = cos(df_pred_KNN_B0_lat) * sin(df_pred_KNN_B0_long) 
z = sin(df_pred_KNN_B0_lat)

df_long_lat <- bind_cols(x = df_pred_KNN_B0_lat, 
                y = df_pred_KNN_B0_long)
view(df_long_lat)
#### PLOTS ####
# Locations at which users logged in 
# Red colour is outside the room, black inside
Plot_long_lat <- ggplot(df_long_lat) +
Plot_long_lat + geom_point(mapping = aes(FP_KNNB0Long), FP_KNNB0Lat)) +
  xlim(0, 400) +
  ylim(0, 300) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle ("Predicted long/lat B0")
  
  
  ggtitle ("Predictions")
Plot_long_lat



# Training log in locations----
ggplot() +
  geom_point(data = trainingData, aes(x = LONGITUDE, y = LATITUDE, colour = "Training dataset")) +
  #geom_point(data = testData, aes(x = LONGITUDE, y = LATITUDE, colour = "Test dataset")) +
  ggtitle("Log In Locations (Training set)") 

# Subset data per building----
BB0 <- subset(trainingData, BUILDINGID == 0)
BB1 <- subset(trainingData, BUILDINGID == 1)
BB2 <- subset(trainingData, BUILDINGID == 2)

# Plot Users per building----
# BB0----
plotForUSer<-function(builNum) {
  builId<- paste("B.", builNum, sep = " " )
  plotTitle<- paste("Building", builNum, ": Which users account for floor location", 
                    sep=" ")
  
  message(paste("building Id:", builId))
  message(paste("Plot title:", plotTitle))
  
  
  BB0%>% 
    ggplot()+
    geom_point(aes(x=LONGITUDE, y= LATITUDE, color=USERID)) + 
    facet_grid(. ~ FLOOR) + 
    labs(title=plotTitle) + 
    theme_linedraw(base_size = 11, base_family = "") + 
    theme(plot.title = element_text(hjust = 0.5, face="bold"))
}

plotForUSer("0")

# BB1----
plotForUSer<-function(builNum) {
  builId<- paste("B.", builNum, sep = " " )
  plotTitle<- paste("Building", builNum, ": Which users account for floor location", 
                    sep=" ")
  
  message(paste("building Id:", builId))
  message(paste("Plot title:", plotTitle))
  
  
  BB1 %>% 
    ggplot()+
    geom_point(aes(x=LONGITUDE, y= LATITUDE, color=USERID)) + 
    facet_grid(. ~ FLOOR) + 
    labs(title=plotTitle) + 
    theme_linedraw(base_size = 11, base_family = "") + 
    theme(plot.title = element_text(hjust = 0.5, face="bold"))
}

plotForUSer("1")

# BB2----
plotForUSer<-function(builNum) {
  builId<- paste("B.", builNum, sep = " " )
  plotTitle<- paste("Building", builNum, ": Which users account for floor location", 
                    sep=" ")
  
  message(paste("building Id:", builId))
  message(paste("Plot title:", plotTitle))
  
  
  BB2 %>% 
    ggplot()+
    geom_point(aes(x=LONGITUDE, y= LATITUDE, color=USERID)) + 
    facet_grid(. ~ FLOOR) + 
    labs(title=plotTitle) + 
    theme_linedraw(base_size = 11, base_family = "") + 
    theme(plot.title = element_text(hjust = 0.5, face="bold"))
}

plotForUSer("2")

#Plot number of locations by User----
plot(trainingData$USERID,
     type ="h",
     xlab="USER NUMBER", ylab="frequency",
     main="Number of locations by User",
     col="turquoise3")

#Plot number of locations by Phone id----
plot(trainingData$PHONEID,
     type ="h",
     xlab="PHONE ID", ylab="frequency",
     lwd = 5,
     main="Number of locations by Phone",
     col="turquoise3")

#Inspect location registrations on relative position 1 = in, 2= out----
BB0----
  BB0%>%
  filter(FLOOR==0)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # about 20 outside room

BB0%>%
  filter(FLOOR==1)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # 19-29 outside room

BB0%>%
  filter(FLOOR==2)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # 19-20 outside room

BB0%>%
  filter(FLOOR==3)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # 19-30 outside room

#BB1----
BB1%>%
  filter(FLOOR==0)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # mostly 20 outside room

BB1%>%
  filter(FLOOR==1)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # high counts inside room

BB1%>%
  filter(FLOOR==2)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # low inside, 10-20 outside

BB1%>%
  filter(FLOOR==3)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # low inside,about 20 outside

#BB2----
BB2%>%
  filter(FLOOR==0)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # also inside but mostly outside

BB2%>%
  filter(FLOOR==1)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # relatively high counts inside room

BB2%>%
  filter(FLOOR==2)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # 10-20 in- and outside

BB2%>%
  filter(FLOOR==3)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # lower inside,about 20 outside

BB2%>%
  filter(FLOOR==4)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # 10 only outside

# Remove columns (WAP) where all the values = 100 (WAP was not detected)----
uniquelength <- sapply(BB0,function(x) length(unique(x)))
BB0 <- subset(BB0, select=uniquelength>1)
uniquelength <- sapply(BB1,function(x) length(unique(x)))
BB1 <- subset(BB1, select=uniquelength>1)
uniquelength <- sapply(BB2,function(x) length(unique(x)))
BB2 <- subset(BB2, select=uniquelength>1)

# Remove rows (WAP) where all the values = 0 (WAP was not detected)
keep <- apply(BB0[,1:200], 1, function(x) length(unique(x[!is.na(x)])) != 1)
BB0[keep, ]
uniquelength <- sapply(BB1,function(x) length(unique(x)))
BB1 <- subset(BB1, select=uniquelength>1)
uniquelength <- sapply(BB2,function(x) length(unique(x)))
BB2 <- subset(BB2, select=uniquelength>1)

#### Subset Building_Floor ####
# B0----
BB0Floor0 <- subset(BB0, FLOOR == 0)
BB0Floor1 <- subset(BB0, FLOOR == 1)
BB0Floor2 <- subset(BB0, FLOOR == 2)
BB0Floor3 <- subset(BB0, FLOOR == 3)

# B1----
BB1Floor0 <- subset(BB1, FLOOR == 0)
BB1Floor1 <- subset(BB1, FLOOR == 1)
BB1Floor2 <- subset(BB1, FLOOR == 2)
BB1Floor3 <- subset(BB1, FLOOR == 3)

# B2----
BB2Floor0 <- subset(BB2, FLOOR == 0)
BB2Floor1 <- subset(BB2, FLOOR == 1)
BB2Floor2 <- subset(BB2, FLOOR == 2)
BB2Floor3 <- subset(BB2, FLOOR == 3)
BB2Floor4 <- subset(BB2, FLOOR == 4)

#Remove columns (WAP) where all the values = 100 (WAP was not detected)----
#uniquelength <- sapply(BB0Floor0,function(x) length(unique(x)))
#BB0Floor0 <- subset(BB0Floor0, select=uniquelength>1)
#str(BB0Floor0)
#Remove rows (WAP) where all the values = 100 (WAP was not detected)----
#keep <- apply(BB0Floor0[,1:125], 1, function(x) length(unique(x[!is.na(x)])) != 1)
#BB0Floor0[keep, ]

# Check userid/phone per building----
unique(BB0$USERID) # 2 different user IDs -> 11 1
unique(BB0$PHONEID) # 2 different phone IDs -> 13 14
unique(BB1$USERID) # 12 different user IDs -> 2  4  7  8  9 16 10 11 13 14 17 18
unique(BB1$PHONEID) # 11 different phone IDs -> 23 18  6  1 14  8 13 17  7 22 10
unique(BB2$USERID) # 16 different user IDs -> 2 11  3  5  6  7  8  9 10 12 13 14 15 16 17 18
unique(BB2$PHONEID) # 15 different phone IDs -> 23 13 16  3 19  6  1 14  8 24 17  7 11 22 10

#### 3D scatterplot of floors per building ####
## BB0----
BB0Floor0$z <- 0
BB0Floor1$z <- 1
BB0Floor2$z <- 2
BB0Floor3$z <- 3

PPPlotBB0 <- rbind(BB0Floor0,BB0Floor1)
PPPlotBB0 <- rbind(PPPlotBB0,BB0Floor2)
PPPlotBB0 <- rbind(PPPlotBB0,BB0Floor3)

PPPlotBB0 <- PPPlotBB0[,201:209]
z <- PPPlotBB0$z
x <- PPPlotBB0$LONGITUDE
y <- PPPlotBB0$LATITUDE
scatterplot3d::scatterplot3d(x, y, z, pch = 20, angle = 45, 
                             color = PPPlotBB0$RELATIVEPOSITION, main = "Building 0 Log In points")

# BB1----
BB1Floor0$z <- 0
BB1Floor1$z <- 1
BB1Floor2$z <- 2
BB1Floor3$z <- 3

PPPlotBB1 <- rbind(BB1Floor0,BB1Floor1)
PPPlotBB1 <- rbind(PPPlotBB1,BB1Floor2)
PPPlotBB1 <- rbind(PPPlotBB1,BB1Floor3)
PPPlotBB1
PPPlotBB1 <- PPPlotBB1[,208:216]
a <- PPPlotBB1$z
b <- PPPlotBB1$LONGITUDE
c <- PPPlotBB1$LATITUDE
scatterplot3d::scatterplot3d(a, b, c, angle = 60, pch = PPPlotBB1$z, 
                             color = PPPlotBB1$RELATIVEPOSITION, 
                             main = "Building 1 Log In points" )
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
PPPlotBB2 <- PPPlotBB2[,204:212]
c <- PPPlotBB2$z
a <- PPPlotBB2$LONGITUDE
b <- PPPlotBB2$LATITUDE
scatterplot3d::scatterplot3d(a, b, c, angle = 20, pch = PPPlotBB2$z, 
                             color = PPPlotBB2$RELATIVEPOSITION)
BB0 #5,249 rows, 208 columns
BB1 #5,196 rows, 215 columns
BB2 #9,492 rows, 211 columns
View(BB2)

# Check where is the highest signal strength----
which(BB0[,1:200] == 30)
which(BB0[,1:200] == 30, arr.ind=TRUE)
which(BB1[,1:207] == 30)
which(BB1[,1:207] == 30, arr.ind=TRUE)
which(BB2[,1:203] == 30)
which(BB2[,1:203] == 30, arr.ind=TRUE)

# ~~~~Signal Strenghts ~~~~~~~~~----

#Good Signal----
Signal0_60 <- which(apply(trainingData[, 1:465], 1, function(x) 
  length(which(x > 60))) > 0)

GoodSignal <- trainingData[Signal0_60, ]

Plot_GoodSignal <- ggplot(GoodSignal, aes(GoodSignal$LONGITUDE, 
                                          GoodSignal$LATITUDE))
Plot_GoodSignal + 
  geom_point(colour = as.factor(GoodSignal$RELATIVEPOSITION)) +
  ggtitle("Log in locations where WAP signal was high") +
  xlab("Longitude") +
  ylab("Latitude")

# Remove columns (WAP) where all the values = 0 (WAP was not detected)
uniquelength <- sapply(GoodSignal,function(x) length(unique(x)))
GoodSignal <- subset(GoodSignal, select=uniquelength>1)

# Remove rows (WAP) where all the values = 0 (WAP was not detected)
keep <- apply(GoodSignal[,1:183], 1, function(x) length(unique(x[!is.na(x)])) != 1)
GoodSignal[keep, ]

Plot_GoodSignal <- ggplot(GoodSignal, aes(GoodSignal$LONGITUDE, 
                                          GoodSignal$LATITUDE))
Plot_GoodSignal + 
  geom_point(colour = as.factor(GoodSignal$RELATIVEPOSITION)) +
  ggtitle("Log in locations where WAP signal was high") +
  xlab("Longitude") +
  ylab("Latitude")

# Medium Signal----
def <- which(apply(trainingData[, 1:465], 1, function(x) length(which(x > 20 & x < 60))) > 0)
Mediumsignal <- trainingData[def, ]

ms <- ggplot(Mediumsignal, aes(Mediumsignal$LONGITUDE, Mediumsignal$LATITUDE))
ms + geom_point(colour = as.factor(Mediumsignal$RELATIVEPOSITION))

# Remove columns (WAP) where all the values = 0 (WAP was not detected)
uniquelength <- sapply(Mediumsignal,function(x) length(unique(x)))
Mediumsignal <- subset(Mediumsignal, select=uniquelength>1)

# Remove rows (WAP) where all the values = 0 (WAP was not detected)
keep <- apply(Mediumsignal[,1:183], 1, function(x) length(unique(x[!is.na(x)])) != 1)
Mediumsignal[keep, ]

# Bad Signal----
ghi <- which(apply(trainingData[, 1:465], 1, function(x) length(which(x > 0 & x < 20))) > 0)
BadSignal <- trainingData[ghi, ]

bs <- ggplot(BadSignal, aes(BadSignal$LONGITUDE, BadSignal$LATITUDE))
bs + geom_point(colour = as.factor(BadSignal$RELATIVEPOSITION))

