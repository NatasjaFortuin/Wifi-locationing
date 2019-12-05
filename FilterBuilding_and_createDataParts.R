library(readr)
library(caret)
library(tidyverse)
library(ranger)
library(e1071)
library(lubridate)

# Load dat from saved GE ready Modeldata----
Modeldata <- read_rds("ToFilterData.rds")

# Sample data for train/test 
#### PREPARE 1ST MODEL ####
set.seed(7)

# filter full data per building
B0 <- filter(Modeldata, BUILDINGID == 0)
saveRDS(B0, file = "B0_Data.rds")
B1 <- filter(Modeldata, BUILDINGID == 1)
saveRDS(B1, file = "B1_Data.rds")
B2 <- filter(Modeldata, BUILDINGID == 2)
saveRDS(B2, file = "B2_Data.rds")

#### CREATE TRAIN/TEST DATASETS FOR BUILDINGS ####

# B0$Latitude- define an 75%/25% train/test split of the dataset----
inTraining_B0_lat<- createDataPartition(B0$LATITUDE, p = .75, list = FALSE)
saveRDS(inTraining_B0_lat, file = "inTraining_B0_lat.rds")
training_B0_lat <- B0[inTraining_B0_lat,]
saveRDS(training_B0_lat, file = "training_B0_lat.rds")
testing_B0_lat <- B0[-inTraining_B0_lat,]
saveRDS(testing_B0_lat, file = "testing_B0_lat.rds")

# B0$Longitude- define an 75%/25% train/test split of the dataset----
inTraining_B0_long<- createDataPartition(B0$LONGITUDE, p = .75, list = FALSE)
saveRDS(inTraining_B0_long, file = "inTraining_B0_long.rds")
training_B0_long <- B0[inTraining_B0_long,]
saveRDS(training_B0_long, file = "training_B0_long.rds")
testing_B0_long <- B0[-inTraining_B0_long,]
saveRDS(testing_B0_long, file = "testing_B0_long.rds")

# B0$Floor- define an 75%/25% train/test split of the dataset----
inTraining_B0_floor<- createDataPartition(B0$FLOOR, p = .75, list = FALSE)
saveRDS(inTraining_B0_floor, file = "inTraining_B0_floor.rds")
training_B0_floor <- B0[inTraining_B0_floor,]
saveRDS(training_B0_floor, file = "training_B0_floor.rds")
testing_B0_floor <- B0[-inTraining_B0_floor,]
saveRDS(testing_B0_floor, file = "testing_B0_floor.rds")

# B1$Latitude- define an 75%/25% train/test split of the dataset----
inTraining_B1_lat<- createDataPartition(B1$LATITUDE, p = .75, list = FALSE)
saveRDS(inTraining_B1_lat, file = "inTraining_B1_lat.rds")
training_B1_lat <- B1[inTraining_B1_lat,]
saveRDS(training_B1_lat, file = "training_B1_lat.rds")
testing_B1_lat <- B1[-inTraining_B1_lat,]
saveRDS(testing_B1_lat, file = "testing_B1_lat.rds")

# B1$Longitude- define an 75%/25% train/test split of the dataset----
inTraining_B1_long<- createDataPartition(B1$LONGITUDE, p = .75, list = FALSE)
saveRDS(inTraining_B1_long, file = "inTraining_B1_long.rds")
training_B1_long <- B1[inTraining_B1_long,]
saveRDS(training_B1_long, file = "training_B1_long.rds")
testing_B1_long <- B1[-inTraining_B1_long,]
saveRDS(testing_B1_long, file = "testing_B1_long.rds")

# B1$Floor- define an 75%/25% train/test split of the dataset----
inTraining_B1_floor<- createDataPartition(B1$FLOOR, p = .75, list = FALSE)
saveRDS(inTraining_B1_floor, file = "inTraining_B1_floor.rds")
training_B1_floor <- B1[inTraining_B1_floor,]
saveRDS(training_B1_floor, file = "training_B1_floor.rds")
testing_B1_floor <- B1[-inTraining_B1_floor,]
saveRDS(testing_B1_floor, file = "testing_B1_floor.rds")

# B2$Latitude- define an 75%/25% train/test split of the dataset----
inTraining_B2_lat<- createDataPartition(B2$LATITUDE, p = .75, list = FALSE)
saveRDS(inTraining_B2_lat, file = "inTraining_B2_lat.rds")
training_B2_lat <- B2[inTraining_B2_lat,]
saveRDS(training_B2_lat, file = "training_B2_lat.rds")
testing_B2_lat <- B2[-inTraining_B2_lat,]
saveRDS(testing_B2_lat, file = "testing_B2_lat.rds")

# B2$Longitude- define an 75%/25% train/test split of the dataset----
inTraining_B2_long<- createDataPartition(B2$LONGITUDE, p = .75, list = FALSE)
saveRDS(inTraining_B2_long, file = "inTraining_B2_long.rds")
training_B2_long <- B2[inTraining_B2_long,]
saveRDS(training_B2_long, file = "training_B2_long.rds")
testing_B2_long <- B2[-inTraining_B2_long,]
saveRDS(testing_B2_long, file = "testing_B2_long.rds")

# B2$Floor- define an 75%/25% train/test split of the dataset----
inTraining_B2_floor<- createDataPartition(B2$FLOOR, p = .75, list = FALSE)
saveRDS(inTraining_B2_floor, file = "inTraining_B2_floor.rds")
training_B2_floor <- B2[inTraining_B2_floor,]
saveRDS(training_B2_floor, file = "training_B2_floor.rds")
testing_B2_floor <- B2[-inTraining_B2_floor,]
saveRDS(testing_B2_floor, file = "testing_B2_floor.rds")

#10 fold cross validation----
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
