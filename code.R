
# This piece of code runs fine on R 4.1.0

if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(hms)) install.packages("hms", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")

library(e1071)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(hms)
library(caret)
library(gam)

#https://www.kaggle.com/anikannal/solar-power-generation-data/download

download.file("https://github.com/mikdvdv/solar_power_generation_archive/blob/master/archive.zip?raw=true", 
              paste(getwd(), "/archive.zip", sep = ""))
# Unzipping Plant_2_Weather_Sensor_Data.csv works fine, but we get a warning for some reason, so we suppress warnings for a while. The rest of the files unzip with no issue.
defaultW <- getOption("warn")
options(warn = -1)
# Unzipping the archive
unzip("archive.zip", exdir = paste(getwd(), "/data", sep = ""))
# Enable warnings
options(warn = defaultW)
# Deleting transitory variable
rm(defaultW)

# we want results to be reproducible
set.seed(1735)

# load data for first plant
p_one_gd <- read.csv("data/Plant_1_Generation_Data.csv")
p_one_wsd <- read.csv("data/Plant_1_Weather_Sensor_Data.csv")
# transform titles of columns to lower case
names(p_one_gd) <- tolower(names(p_one_gd))
names(p_one_wsd) <- tolower(names(p_one_wsd))
# transform date_time from character type to date type.
p_one_gd$date_time <- dmy_hm(p_one_gd$date_time)
p_one_wsd$date_time <- ymd_hms(p_one_wsd$date_time)
# we have plant_id variable in p_one_gd. The variable source_key is not informative since it has only one value over the whole data set.
p_one_wsd <- p_one_wsd %>%
  select(-plant_id, -source_key)
# joining first plant's power generation data and weather sensor data
p_one <- inner_join(p_one_gd, p_one_wsd, by = "date_time")

# transforming date_time feature into two separate features - p_date and p_time
p_one <- p_one %>%
  mutate(p_date = date(date_time),
         p_time = as_hms(date_time)) %>%
  relocate(p_date, p_time, .before = plant_id) %>%
  select(-date_time)

# transform source_key so we can approach it easier
p_one$source_key <- as.numeric(as.factor(p_one$source_key))

# Arranging rows to ease our job
p_one <- p_one %>%
  arrange(source_key, p_date, p_time)

# load data for second plant
p_two_gd <- read.csv("data/Plant_2_Generation_Data.csv")
p_two_wsd <- read.csv("data/Plant_2_Weather_Sensor_Data.csv")
# transform titles of columns to lower case
names(p_two_gd) <- tolower(names(p_two_gd))
names(p_two_wsd) <- tolower(names(p_two_wsd))
# transform date_time from character type to date type.
p_two_gd$date_time <- ymd_hms(p_two_gd$date_time)
p_two_wsd$date_time <- ymd_hms(p_two_wsd$date_time)
# we have plant_id variable in p_two_gd. The variable source_key is not informative since it has only one value over the whole data set.
p_two_wsd <- p_two_wsd %>%
  select(-plant_id, -source_key)
# joining second plant's power generation data and weather sensor data
p_two <- inner_join(p_two_gd, p_two_wsd, by = "date_time")

# transforming date_time feature into two separate features - p_date and p_time
p_two <- p_two %>%
  mutate(p_date = date(date_time),
         p_time = as_hms(date_time)) %>%
  relocate(p_date, p_time, .before = plant_id) %>%
  select(-date_time)

# Transforming source_key like we did with p_one, only this time we add a constant to merge it with p_one
p_two$source_key <- as.numeric(as.factor(p_two$source_key)) + 
  length(unique(p_one$source_key))

# Arranging rows to ease our job
p_two <- p_two %>%
  arrange(source_key, p_date, p_time)

# Deleting redundant variables
rm(p_one_gd, p_one_wsd, p_two_gd, p_two_wsd)

# Merging two data sets into one
plants <- rbind(p_one, p_two)
rm(p_one, p_two)
# Setting plant_id as a factor. We need factors for classification algorithms
plants$plant_id <- as.factor(plants$plant_id)
# Splitting data on training and test set
test_index <- createDataPartition(y = plants$plant_id, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- plants[-test_index,]
test_set <- plants[test_index,]
rm(plants, test_index)

##### Functions' definitions

# This function cuts troublesome observations out of a data frame. malf_list is a result of a sapply() function, so it is a list of vectors. Every vector represents something like source_key, for example. The name stands for "cut malfunction".
cut_malf <- function(malf_list, data_set){
  # Creating a variable
  malf_vec <- c(0)
  # Transforming a list of vectors in one huge vector
  for (xx in 1:length(malf_list)){
    malf_vec <- c(malf_vec, malf_list[[xx]])
  }
  # We need to delete the first value from the vector, since it had been created to create the vector itself.
  malf_vec <- malf_vec[2: length(malf_vec)]
  # Deleting broken observations from the data set.
  data_set %>%
    slice(which(!malf_vec))
}

# This function deletes observations with total_yield drop. The name stands for "delete total yield drop"
del_ttl_yld_drp <- function(data_set){
  # Creating a list of vectors to tackle total_yield drop. Every vector matches with source_key.
  malf_list1 <- sapply(unique(data_set$source_key), function(xx){
    # Taking the initial total_yield
    init_yield <- head(data_set %>%
                         filter(source_key == xx), 1)$total_yield
    
    # Comparing total_yield with initial yield we have got earlier
    data_set %>%
      filter(source_key == xx) %>%
      mutate(malf = ifelse(total_yield < init_yield, 1, 0)) %>%
      pull(malf)
  })
  # Cut observations with wrong total_yield. And return clean data set as a result of a function
  cut_malf(malf_list1, data_set)
}

# This function creates a variable showing a switch from zero to some number or vice versa in the ac_power feature. The name stands for "add switch variable based on ac_power"
add_swtch_vr_ac <- function(data_set){
  # Creating a feature representing switches from zero to non-zero number in ac_power. First non-zero observations after zeros get 1, the rest of observations get 0.
  data_set <- data_set %>%
    # Creating a feature showing whether ac_power has zero or non-zero value.
    mutate(swtch_f_z_ac = ifelse(ac_power == 0, 1, 2)) %>%
    # Comparing each observation with its previous one. If the two observations are the same - the sum will result in an even number (either two or four). If they are not - the sum going to be an odd number (three). Then we divide the sum by two and take the reminder
    mutate(swtch_f_z_ac = (swtch_f_z_ac + c(1, swtch_f_z_ac[1:(n()-1)])) %% 2) %>%
    # If current ac_power == 0 and in the previous observation ac_power has a non-zero value, the line above returns "1". We don't need that. So we set zeros to observations with ac_power == 0.
    mutate(swtch_f_z_ac = ifelse(ac_power == 0, 0, swtch_f_z_ac)) %>%
    relocate(swtch_f_z_ac, .after = ac_power)
  # Creating a feature representing switches from a non-zero value to zero in ac_power. Last non-zero observations before zeros get 1, the rest of observations get 0.
  data_set <- data_set %>%
    # Creating a feature showing weather ac_power has zero or non-zero value.
    mutate(swtch_f_n_ac = ifelse(ac_power == 0, 1, 2)) %>%
    # Comparing each observation with its next one. If the two observations are the same - the sum will result in an even number (either two or four). If they are not - the sum going to be an odd number (three). Then we divide the sum by two and take the reminder
    mutate(swtch_f_n_ac = (swtch_f_n_ac + c(swtch_f_n_ac[2:(n())], 0)) %% 2) %>%
    # If current ac_power == 0 and in the next observation ac_power has a non-zero value, the line above returns "1". We don't need that. So we set zeros to observations with ac_power == 0.
    mutate(swtch_f_n_ac = ifelse(ac_power == 0, 0, swtch_f_n_ac)) %>%
    relocate(swtch_f_n_ac, .after = ac_power)
  # Combine switches from zero to non-zero with switches from non-zero to zero.
  data_set %>%
    mutate(swtch_ac = swtch_f_n_ac + swtch_f_z_ac) %>%
    relocate(swtch_ac, .after = ac_power) %>%
    select(-swtch_f_n_ac, -swtch_f_z_ac)
}

# Finding mode. https://www.tutorialspoint.com/r/r_mean_median_mode.htm .According to https://courses.edx.org/courses/course-v1:HarvardX+PH125.9x+1T2021/discussion/forum/e3f44cffce1cc3ed3b72f06e629fd7b3a5d895c8/threads/60ae7dfce136290450b23d5b I can do that.
getmode <- function(v) {
  # Getting unique values from the vector v
  uniqv <- unique(v)
  # Getting the most common value
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# This function bins irradiation feature and computes mode of ac_power and dc_power for each bin
get_pwr_md_irr <- function(data_set, n_bins){
  # Binning irradiation feature, so we can find the mode of ac_power and dc_power for each bin
  data_set <- data_set %>%
    mutate(irrad_bin = round(irradiation * n_bins))
  
  # Finding the mode of ac_power and dc_power
  data_set %>%
    # Deleting zeros, so they don't spoil the mode. We don't need them anyway
    slice(which(ac_power != 0 & dc_power != 0)) %>% 
    group_by(irrad_bin) %>%
    summarise(irrad_mode_ac = getmode(ac_power), irrad_mode_dc = getmode(dc_power))
}

cut_swtch_malf <- function(data_set, irr_bin, n_bins){
  # Binning irradiation feature, so we can join the data set with irr_bin
  data_set <- data_set %>%
    mutate(irrad_bin = round(irradiation * n_bins))
  # Adding mode of ac_power and dc_power to the data set and find the distance between ac_power and the mode of ac_power. Same for dc_power
  data_set <- left_join(data_set, irr_bin, by = "irrad_bin") %>%
    mutate(mode_dist_ac = irrad_mode_ac - ac_power,
           mode_dist_dc = irrad_mode_dc - dc_power)
  # Cutting off observations where c_power significantly differs from its mode given irradiation and the observation has been made right before or after the panel didn't work
  data_set %>%
    filter(irradiation > 0) %>%
    # We measure the significance of the difference as sqrt() function because when irradiation is low, 200 is a lot, but when irradiation is high, 200 is ok. So we need a curve, and sqrt() fits nice. The multiplier for sqrt() is arbitrary, has been chosen by trial and error
    slice(which(!(abs(mode_dist_ac) > (sqrt(irradiation) * 400) & swtch_ac != 0))) %>%
    # Deleting transitory features. 
    select(-mode_dist_ac, -mode_dist_dc, -irrad_mode_ac, -irrad_mode_dc)
}
### Definitions of functions to cut the rest of the outliers.
## This function bins ac_power and computes mode of module_temperature for each bin
#get_mdl_tmpr_md_ac <- function(data_set, n_bins_rev){
#  # Binning ac_power feature, so we can find mode of module_temperature, which will allow us to detect module_temperature anomalies
#  data_set <- data_set %>%
#    mutate(ac_bin = round(ac_power / n_bins_rev))
#  
#  # Finding mode of module_temperature
#  data_set %>%
#    group_by(ac_bin) %>%
#    summarise(mod_temp_mode_ac = getmode(module_temperature))
#}

#cut_temp_malf <- function(data_set, irr_bin, ac_p_bin, n_bins, n_bins_rev){
## Binning irradiation feature, so we can find mode of ac_power and dc_power for each bin
#data_set <- data_set %>%
#  mutate(irrad_bin = round(irradiation * n_bins))
## Binning ac_power feature, so we can find mode of module_temperature, which will allow us to detect module_temperature anomalies
#data_set <- data_set %>%
#  mutate(ac_bin = round(ac_power / n_bins_rev))

#  # Adding mode of ac_power and dc_power to the data set and find the distance between ac_power and the mode of ac_power. Same for dc_power
#  data_set <- left_join(data_set, irr_bin, by = "irrad_bin") %>%
#    mutate(mode_dist_ac = irrad_mode_ac - ac_power,
#           mode_dist_dc = irrad_mode_dc - dc_power)
#  
#  # Finding the distance between module_temperature and the mode of module_temperature
#  data_set <- left_join(data_set, ac_p_bin, by = "ac_bin") %>%
#    mutate(mt_mode_dist_ac = mod_temp_mode_ac - module_temperature)
#  
#  # Cutting off observations where module_temperature differs from its mode more than 10, given ac_power, and where ac_power significantly differs from its mode given irradiation. We measure significance of the difference as sqrt() function, because when irradiation is low, 200 is a lot but when irradiation is high 200 is ok. So we need a curve, and sqrt() fits nice. The multiplier for sqrt() is arbitrary. 
#  data_set %>%
#    filter(irradiation > 0) %>%
#    slice(which(!(abs(mode_dist_ac) > (sqrt(irradiation) * 400) & abs(mt_mode_dist_ac) >= 10))) %>%
#    # Deleting transitory features. 
#    select(-mode_dist_ac, -mode_dist_dc, -irrad_mode_ac, -irrad_mode_dc, -mt_mode_dist_ac) 
#}
###

##### Algorithm's implementation
## Data preparation
# Deleting total yield drops
train_set <- del_ttl_yld_drp(train_set)
# Marking peripheral observations
train_set <- add_swtch_vr_ac(train_set)

# This number defines the number(not exact) of irradiation bins.
num_irr_bin <- 18
# Getting ac_power mode for each irradiation bin
irr_bin <- get_pwr_md_irr(train_set, num_irr_bin)
# Cutting peripheral observations with low productivity
train_set <- cut_swtch_malf(train_set, irr_bin, num_irr_bin)

# Cleaning the test_set
test_set <- add_swtch_vr_ac(test_set)
test_set <- del_ttl_yld_drp(test_set)
test_set <- cut_swtch_malf(test_set, irr_bin, num_irr_bin)

# Functions to cut the rest of the outliers. You should uncomment functions' definitions
#num_ac_p_bin <- 200
#ac_p_bin <- get_mdl_tmpr_md_ac(train_set, num_ac_p_bin)
#train_set <- cut_temp_malf(train_set, irr_bin, ac_p_bin, num_irr_bin, num_ac_p_bin)
#test_set <- cut_temp_malf(test_set, irr_bin, ac_p_bin, num_irr_bin, num_ac_p_bin)

# Cutting redundant features
train_set <- train_set %>%
  select(plant_id, dc_power, irradiation)
test_set <- test_set %>%
  select(plant_id, dc_power, irradiation)

## Models' tuning

# gam tuning
#grid <- expand.grid(span = seq(0.1, 0.9, by = 0.1), degree = c(0:1))
#model_gam <- train(plant_id ~ ., data = g_train_set, method = "gamLoess", 
#                   tuneGrid = grid)
#model_gam$bestTune
#ggplot(model_gam)

# knn tuning
#model_knn <- train(plant_id ~ ., data = train_set, method = "knn",
#                   tuneGrid = data.frame(k = seq(1, 7, 2)))
#model_knn$bestTune
#ggplot(model_knn)

## Models' training
# lda
model_lda <- train(plant_id ~ ., data = train_set, method = "lda")

# knn
model_knn <- train(plant_id ~ ., data = train_set, method = "knn",
                   tuneGrid = data.frame(k = 3))

# Creating a subset of data, since gamLoess is computationally expensive
g_train_index <- createDataPartition(y = train_set$plant_id, times = 1, p = 0.05, 
                                     list = FALSE)
g_train_set <- train_set[g_train_index,]

# gamLoess
grid <- expand.grid(span = 0.5, degree = 1)
model_gam <- train(plant_id ~ ., data = g_train_set, method = "gamLoess", 
                   tuneGrid = grid)

## Prediction
y_hat_lda <- predict(model_lda, test_set, type = "raw")
y_hat_knn <- predict(model_knn, test_set, type = "raw")
y_hat_gam <- predict(model_gam, test_set, type = "raw")

tibble(lda = confusionMatrix(y_hat_lda, test_set$plant_id)$overall["Accuracy"],
       knn = confusionMatrix(y_hat_knn, test_set$plant_id)$overall["Accuracy"],
       gam = confusionMatrix(y_hat_gam, test_set$plant_id)$overall["Accuracy"])