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
  mutate(DateTime = as_datetime(TIMESTAMP))
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

Corplot_BuildLong <- cor(Workfile$BUILDINGID, Workfile$LONGITUDE)
print(Corplot_BuildLong) #[1] 0.9583744
plot(Corplot_BuildLong)

Corplot_BuildFloor <- cor(Workfile$BUILDINGID, Workfile$FLOOR)
print(Corplot_BuildFloor) #[1] 0.1147338
plot(Corplot_BuildFloor)

# calculate correlation matrix for Building variables----
correlationMatrix <- cor(Workfile[,521:524])

# summarize the correlation matrix----
print(correlationMatrix)
#LONGITUDE   LATITUDE       FLOOR BUILDINGID
#LONGITUDE   1.00000000 -0.8603265  0.08083591  0.9583744
#LATITUDE   -0.86032650  1.0000000 -0.10168548 -0.8812688
#FLOOR       0.08083591 -0.1016855  1.00000000  0.1147338
#BUILDINGID  0.95837436 -0.8812688  0.11473377  1.0000000

# find attributes that are highly corrected (ideally >0.75) 
# absolute correlations>0.75 can best be removed

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.90)

# print indexes of highly correlated attributes
print(highlyCorrelated) #[1] 4
plot(highlyCorrelated)

#### PREPARE 1ST MODEL ####
set.seed(7)

#create a 10% sample of the data----
Subset1<- Workfile[sample(1:nrow(Workfile), 4000,replace=FALSE),]

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
#k-Nearest Neighbors 3001 samples 529 predictor
#Pre-processing: median imputation (547), remove (103) 
#Resampling: Cross-Validated (5 fold, repeated 1 times) 
#Summary of sample sizes: 2401, 2400, 2400, 2401, 2402 
#Resampling results across tuning parameters:
  
#  k  RMSE       Rsquared   MAE     
#5   9.723456  0.9937327  5.047958
#7  10.547970  0.9927043  5.772798
#9  11.584433  0.9913227  6.622565

#RMSE was used to select the optimal model using the smallest value.
#The final value used for the model was k = 5.

#### REVIEW... postresample----
postResample(pred = predict(object = Fit_long, newdata = testing_long), 
             obs = testing_long$LONGITUDE)
#   RMSE      Rsquared  MAE 
#   8.8423037 0.9948784 4.5223290 

#Predict Output----
predicted= predict(Fit_long, testing_long)
print(predicted)
str(predicted)

#Predict Output----
predicted= predict(Fit_long, testing_long)

