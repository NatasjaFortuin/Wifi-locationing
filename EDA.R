library(readr)
library(caret)
library(tidyverse)
library(ranger)
library(e1071)
library(lubridate)


# Load data----
trainingData <- read_csv("Data etc/trainingData.csv")
View(trainingData)
validationData <- read_csv("Data etc/validationData.csv")

# Check for NA's----
is.na(trainingData)
sum(is.na(trainingData))

#### Explore data ####----
str(trainingData)
dim(trainingData)

Workfile <- trainingData
head(Workfile)
view(Workfile)

## filter per building

# filter full data per building
B0 <- filter(Workfile, BUILDINGID == 0)
B1 <- filter(Workfile, BUILDINGID == 1)
B2 <- filter(Workfile, BUILDINGID == 2)

## Convert TIMESTAMP to ymd_hms_UTC----

#as_datetime(1371713733) = "2013-06-20 07:35:33 UTC"
Workfile <- Workfile %>% 
  mutate(as_datetime(TIMESTAMP))
view(Workfile)
saveRDS(Workfile, file = "ToFilterData.rds")
#### REVIEW #### is timescope correct?----

## Summary & plotting----

# summary Longitude -  Timestamp
summary(Workfile[,521:529])
summary(Workfile$`as_datetime(TIMESTAMP)`)

# plot longitude/latitude
plot(Workfile$LONGITUDE, Workfile$LATITUDE)
plot(validationData$LONGITUDE, validationData$LATITUDE)

#### PREPARE 1ST MODEL ####
set.seed(7)

#create a 10% sample of the data----
Subset1<- Workfile[sample(1:nrow(Workfile), 2000,replace=FALSE),]

# define an 75%/25% train/test split of the dataset----
inTraining_long<- createDataPartition(Subset1$LONGITUDE, p = .75, list = FALSE)
training_long <- Subset1[inTraining_long,]
testing_long <- Subset1[-inTraining_long,]

#10 fold cross validation----
#####fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

Fit_long <- train(LONGITUDE~., 
                data = training_long, 
                method = "knn", 
                trControl=fitControl, 
                tuneLength = 3,
                verboseIter = TRUE,
preProcess = c("zv", "medianImpute"))
Fit_long

#Fit_long result summary
#k-Nearest Neighbors, 1502 samples, 529 predictor
#Pre-processing: median imputation (525), remove (122) 
#Resampling: Cross-Validated (10 fold, repeated 1 times) 
#Summary of sample sizes: 1352, 1353, 1352, 1351, 1351, 1352, ... 
#Resampling results across tuning parameters:
  
#  k  RMSE      Rsquared   MAE     
#  5  13.25943  0.9885509  7.446795
#  7  15.20129  0.9849412  8.750156
#  9  16.84683  0.9814590  9.967150

#RMSE was used to select the optimal model using the smallest value.
#The final value used for the model was k = 5.

#### REVIEW... postresample----
postResample(pred = predict(object = Fit_long, newdata = testing_long), 
             obs = testing_long$LONGITUDE)

#Predict Output----
predicted= predict(Fit_long, testing_long)
print(predicted)
str(predicted)
 

#   RMSE     Rsquared MAE 
#   380.911  0.6624   210.75 

#Predict Output----
predicted= predict(Fit_long, testing_long)

