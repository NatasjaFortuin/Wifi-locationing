library(readr)
library(caret)
library(gmodels)

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
Test_B0_floor <- read_rds("testing_B0_floor.rds")
Test_B1_Floor <- read_rds("testing_B1_floor.rds")
Test_B2_Floor <- read_rds("testing_B2_floor.rds")

# load trained models for predictions----
## KNN----
Fit_lat_B0 <- read_rds("KNN_Fit_lat_B0.rds")
Fit_lat_B1 <- read_rds("KNN_Fit_lat_B1.rds")
Fit_lat_B2 <- read_rds("KNN_Fit_lat_B2.rds")
Fit_long_B0 <- read_rds("KNN_Fit_long_B0.rds")
Fit_long_B1 <- read_rds("KNN_Fit_long_B1.rds")
Fit_long_B2 <- read_rds("KNN_Fit_long_B2.rds")
Fit_floor_B0 <- read_rds("KNN_Fit_floor_B0.rds")
Fit_floor_B1 <- read_rds("KNN_Fit_floor_B1.rds")
Fit_floor_B2 <- read_rds("KNN_Fit_floor_B2.rds")

#### PREDICT ####

