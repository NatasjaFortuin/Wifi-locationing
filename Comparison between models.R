library(readr)
library(caret)

# Load saved KNN models----
KNNB0Lat <- read_rds("KNN_Fit_lat_B0.rds")
KNNB1Lat <- read_rds("KNN_Fit_lat_B1.rds")
KNNB2Lat <- read_rds("KNN_Fit_lat_B2.rds")

KNNB0Long <- read_rds("KNN_Fit_long_B0.rds")
KNNB1Long <- read_rds("KNN_Fit_long_B1.rds")
KNNB2Long <- read_rds("KNN_Fit_long_B2.rds")

KNNB0Floor <- read_rds("KNN_Fit_floor_B0.rds")
KNNB1Floor<- read_rds("KNN_Fit_floor_B1.rds")
KNNB2Floor <- read_rds("KNN_Fit_floor_B2.rds")

# Load validation data for predictions----
validationData <- read_csv("Data etc/validationData.csv")


#pred_knn_B0 <- predict(KNNB0Lat, "validation data")
#conf.matrix.knn.floor.2 <- table(pred.knn.floor.2, build.2.floor.v$build.2.v.FLOOR)
#accuracy.knn.floor.2 <- (sum(diag(conf.matrix.knn.floor.2))) / sum(conf.matrix.knn.floor.2)
#accuracy.knn.floor.2 <- accuracy.knn.floor.2 * 100
#accuracy.knn.floor.2