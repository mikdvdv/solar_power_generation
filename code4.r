library(lubridate)
library(ggplot2)
library(tidyverse)
library(hms)

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
# we have plant_id variable in p_one_gd. The variable source_key is not 
#informative, since it has only one value over the whole data set.
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
# we have plant_id variable in p_two_gd. The variable source_key is not 
#informative, since it has only one value over the whole data set.
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

# Transforming source_key like we did with p_one, only this time we add a constant just in case we want to merge two data sets
p_two$source_key <- as.numeric(as.factor(p_two$source_key)) + 
  length(unique(p_one$source_key))

# Arranging rows to ease our job
p_two <- p_two %>%
  arrange(source_key, p_date, p_time)

# Deleting redundant variables
rm(p_one_gd, p_one_wsd, p_two_gd, p_two_wsd)

# Finding mode. https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# This function cuts troublesome observations out of a data frame. malf_list is a result of a sapply() function, so it is a list of vectors. Every vector represents something like source_key for example. The name stands for "cut malfunction"
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

# This function creates a variable showing a switch from zero to some number or vice versa in the ac_power feature
add_swtch_vr_ac <- function(data_set){
  # Creating a feature representing switches from zero to non-zero number in ac_power. First non-zero observations after zeros get 1, the rest of observations get 0.
  data_set <- data_set %>%
    # Creating a feature showing weather ac_power has zero or non-zero value.
    mutate(swtch_f_z_ac = ifelse(ac_power == 0, 1, 2)) %>%
    # Comparing each observation with it's previous one. If the two observations are the same - the sum will result in an even number (either two or four). If they are not - the sum going to be an odd number (three).
    mutate(swtch_f_z_ac = (swtch_f_z_ac + c(1, swtch_f_z_ac[1:(n()-1)])) %% 2) %>%
    # If current ac_power == 0 and in the previous observation ac_power has a non-zero value, the line above returns one. We don't need that. So we set zeros to observations with ac_power == 0.
    mutate(swtch_f_z_ac = ifelse(ac_power == 0, 0, swtch_f_z_ac)) %>%
    relocate(swtch_f_z_ac, .after = ac_power)
  # Creating a feature representing switches from non-zero value to zero in ac_power. Last non-zero observations before zeros get 1, the rest of observations get 0.
  data_set <- data_set %>%
    # Creating a feature showing weather ac_power has zero or non-zero value.
    mutate(swtch_f_n_ac = ifelse(ac_power == 0, 1, 2)) %>%
    # Comparing each observation with it's next one. If the two observations are the same - the sum will result in an even number (either two or four). If they are not - the sum going to be an odd number (three).
    mutate(swtch_f_n_ac = (swtch_f_n_ac + c(swtch_f_n_ac[2:(n())], 0)) %% 2) %>%
    # If current ac_power == 0 and in the next observation ac_power has a non-zero value, the line above returns one. We don't need that. So we set zeros to observations with ac_power == 0.
    mutate(swtch_f_n_ac = ifelse(ac_power == 0, 0, swtch_f_n_ac)) %>%
    relocate(swtch_f_n_ac, .after = ac_power)
  # Combine switches from zero to non-zero with switches from non-zero to zero.
  data_set %>%
    mutate(swtch_ac = swtch_f_n_ac + swtch_f_z_ac) %>%
    relocate(swtch_ac, .after = ac_power) %>%
    select(-swtch_f_n_ac, -swtch_f_z_ac)
}

# This function deletes observations with total_yield drop
del_ttl_yld_drp <- function(data_set){
  # Creating a list of vectors to tackle total_yield drop. Every vector represents source_key.
  malf_list1 <- sapply(unique(data_set$source_key), function(xx){
    # Getting value of the total_yield feature at 00:00:00 2020-05-15. Which is the time first observations had been made at.
    init_yield <- data_set %>%
      filter(source_key == xx & p_date == ymd("2020-05-15") & p_time == hms(0)) %>%
      pull(total_yield)
    # Comparing total_yield with initial yield we have got earlier
    data_set %>%
      filter(source_key == xx) %>%
      mutate(malf = ifelse(total_yield < init_yield, 1, 0)) %>%
      pull(malf)
  })
  # Cut observations with wrong total_yield. And return clean data set as a result of a function
  cut_malf(malf_list1, data_set)
}

# This function bins irradiation feature and computes mode of ac_power and dc_power for each bin
get_pwr_md_irr <- function(data_set, n_bins){
  # Binning irradiation feature, so we can find mode of ac_power and dc_power for each bin
  data_set <- data_set %>%
    mutate(irrad_bin = round(irradiation * n_bins))
  
  # Finding mode of ac_power and dc_power
  data_set %>%
    # Delete zeros, so they don't spoil the mode. We don't need them anyway
    slice(which(ac_power != 0 & dc_power != 0)) %>% 
    group_by(irrad_bin) %>%
    summarise(irrad_mode_ac = getmode(ac_power), irrad_mode_dc = getmode(dc_power))
}

# This function bins ac_power and computes mode of module_temperature for each bin
get_mdl_tmpr_md_ac <- function(data_set, n_bins_rev){
  # Binning ac_power feature, so we can find mode of module_temperature, which will allow us to detect module_temperature anomalies
  data_set <- data_set %>%
    mutate(ac_bin = round(ac_power / n_bins_rev))
  
  # Finding mode of module_temperature
  data_set %>%
    group_by(ac_bin) %>%
    summarise(mod_temp_mode_ac = getmode(module_temperature))
}

cut_temp_malf <- function(data_set, irr_bin, ac_p_bin){
  # Adding mode of ac_power and dc_power to the data set and find the distance between ac_power and the mode of ac_power. Same for dc_power
  data_set <- left_join(data_set, irr_bin, by = "irrad_bin") %>%
    mutate(mode_dist_ac = irrad_mode_ac - ac_power,
           mode_dist_dc = irrad_mode_dc - dc_power)
  
  # Finding the distance between module_temperature and the mode of module_temperature
  data_set <- left_join(data_set, ac_p_bin, by = "ac_bin") %>%
    mutate(mt_mode_dist_ac = mod_temp_mode_ac - module_temperature)
  
  # Cutting off observations where module_temperature differs from its mode more than 10, given ac_power, and where ac_power significantly differs from its mode given irradiation. We measure significance of the difference as sqrt() function, because when irradiation is low, 200 is a lot but when irradiation is high 200 is ok. So we need a curve, and sqrt() fits nice. The multiplier for sqrt() is arbitrary. 
  data_set %>%
    filter(irradiation > 0) %>%
    slice(which(!(abs(mode_dist_ac) > (sqrt(irradiation) * 400) & abs(mt_mode_dist_ac) >= 10))) %>%
    # Deleting transitory features. 
    select(-mode_dist_ac, -mode_dist_dc, -irrad_mode_ac, -irrad_mode_dc, -mt_mode_dist_ac) 
}

cut_swtch_malf <- function(data_set, irr_bin){
  # Adding mode of ac_power and dc_power to the data set and find the distance between ac_power and the mode of ac_power. Same for dc_power
  data_set <- left_join(data_set, irr_bin, by = "irrad_bin") %>%
    mutate(mode_dist_ac = irrad_mode_ac - ac_power,
           mode_dist_dc = irrad_mode_dc - dc_power)
  
  # Cutting off observations where c_power significantly differs from its mode given irradiation and the observation has been made right before or after the panel didn't work
  data_set %>%
    filter(irradiation > 0) %>%
    slice(which(!(abs(mode_dist_ac) > (sqrt(irradiation) * 400) & swtch_ac != 0))) %>%
    # Deleting transitory features. 
    select(-mode_dist_ac, -mode_dist_dc, -irrad_mode_ac, -irrad_mode_dc)
}

###################

# Set numbers of bins for binning features.
num_irr_bin <- 18
num_ac_p_bin <- 200

p_two_clean <- add_swtch_vr_ac(p_two)
p_two_clean <- del_ttl_yld_drp(p_two_clean)
irr_bin <- get_pwr_md_irr(p_two_clean, num_irr_bin)
ac_p_bin <- get_mdl_tmpr_md_ac(p_two_clean, num_ac_p_bin)

# Binning irradiation feature, so we can find mode of ac_power and dc_power for each bin
p_two_clean <- p_two_clean %>%
  mutate(irrad_bin = round(irradiation * num_irr_bin))

# Binning ac_power feature, so we can find mode of module_temperature, which will allow us to detect module_temperature anomalies
p_two_clean <- p_two_clean %>%
  mutate(ac_bin = round(ac_power / num_ac_p_bin))

p_two_clean <- cut_temp_malf(p_two_clean, irr_bin, ac_p_bin)
p_two_clean <- cut_swtch_malf(p_two_clean, irr_bin)

#ggplot(p_two, aes(irradiation, ac_power)) + geom_point()
