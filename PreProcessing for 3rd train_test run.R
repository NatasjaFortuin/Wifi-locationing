library(readr)
library(caret)
library(dplyr)

# Load data----
trainingData <- read_csv("Data etc/trainingData.csv")
View(trainingData)

#How much of the values are not detected and therefore 100----
# Remove columns (WAP) where all the values = 100 (WAP was not detected)
# Training dataset
uniquelength <- sapply(trainingData,function(x) length(unique(x)))
trainingData <- subset(trainingData, select=uniquelength>1)

# Change WAP values so that no signal is 0 and highest signal is 91
# Training Data
trainingData[trainingData == 100] <- -91
trainingData[,1:465] <- trainingData[,1:465] + 91
str(trainingData)

# Remove rows (WAP) where all the values = 100 (WAP was not detected)
# Training dataset
keep <- apply(trainingData[,1:465], 1, function(x) length(unique(x[!is.na(x)])) != 1)
trainingData[keep, ]
dim(trainingData)
str(trainingData)
view(trainingData)

# Converting data types
# Training dataset
trainingData$FLOOR <- as.factor(trainingData$FLOOR)
trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)
trainingData$RELATIVEPOSITION <- as.factor(trainingData$RELATIVEPOSITION)
trainingData$USERID <- as.factor(trainingData$USERID)
trainingData$PHONEID <- as.factor(trainingData$PHONEID)

# Check distribution of signal strength
# traning data
x <- trainingData[,1:465]
x <- stack(x)

x <-x[-grep(0, x$values),]
hist(x$values, xlab = "WAP strength", 
     main = "Distribution of WAPs signal stength (Training set)", col = "red")

ggplot() +
  geom_histogram(data = x, aes(values), fill = "red", alpha = 1, binwidth = 5) +
  ggtitle("Distribution of WAPs signal strength (Training)") +
  xlab("WAP strength")

# Filter data per building----
BB0 <- filter(trainingData, BUILDINGID == 0)
BB1 <- filter(trainingData, BUILDINGID == 1)
BB2 <- filter(trainingData, BUILDINGID == 2)
View(BB0)




BB1_100 <- BB1 %>% select(contains("WAP")) %>% arrange(desc(WAP001:WAP520))
BB1_100
              
