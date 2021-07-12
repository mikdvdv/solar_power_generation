# The data has come from 
    # https://www.kaggle.com/anikannal/solar-power-generation-data

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

# merging data from two power plants into one data set.
plants <- rbind(p_one, p_two)

# removing transitory variables
rm(list = ls()[which(ls() != "plants")])

# we want source_key to be easy to reach
plants$source_key <- as.numeric(as.factor(plants$source_key))

# Since the data set is small, I want validation set to be 10% of the data set.
    # we have 34 days of observation, so hold out set would be the 3 last days 

# Assumption: old panels don't work as good as new ones.
range(plants$total_yield)
median(plants$daily_yield)
# The oldest panel works approximately 793015.7 days
(max(plants$total_yield) / median(plants$daily_yield))/365


#########################################
#-------- I have to make this block look nice changing variable names.

# is there any difference between ac and dc power? 
ggplot(p_one_gd, aes(ac_power, dc_power)) + geom_point()
# almost none

# ok let's see generated power over time
ggplot(p_one_gd, aes(date_time, dc_power)) + geom_point()

# how similar the panels are in terms of efficiency 
p_one_gd %>%
  group_by(date_time) %>%
  summarise(DC_sd = sd(dc_power)) %>%
  ggplot(aes(date_time, DC_sd)) + geom_point()
# the difference is obvious. the grater the radiation the greater the difference
    # you can see it as peaks at noon

# are there some panels under performing?
p_one_gd %>%
  group_by(source_key) %>%
  summarise(performance = sum(dc_power + ac_power)) %>%
  ggplot(aes(source_key, performance)) + geom_point()
# we have two outliers

p_two_gd$source_key <- as.factor(p_two_gd$source_key)
p_two_gd %>%
  mutate(meas_day = floor_date(date_time, unit = "day")) %>%
  #aggregate(by = list(p_two_gd$source_key, p_two_gd$meas_day), FUN = sum, data = .) %>%
  aggregate(cbind(dc_power, ac_power) ~ source_key + meas_day, FUN = sum, data = .) %>%
  mutate(performance = dc_power + ac_power) %>%
  ggplot(aes(meas_day, performance, color = source_key)) + geom_point() + 
    geom_path()
# so we have two panels consistently under performing. Moreover, these two
    # panels had got performance drop at June 14 for some reason. It's hard to tell 
    # something about the rest of the panels. It is too much of them on the plot
    # let's take subset of source_key 
p_one_gd %>%
  filter(as.numeric(source_key) < 6) %>%
  mutate(meas_day = floor_date(date_time, unit = "day")) %>%
  group_by(source_key, meas_day) %>%
  summarise(performance = sum(dc_power + ac_power)) %>%
  ggplot(aes(meas_day, performance, color = source_key)) + geom_point() + 
  geom_path()
# we can see that some panels work better then others. Maybe it's because
    # panels have different angle to the sun, or maybe variation is the norm,
    # when it comes to solar panels manufacturing.


#-------- The end of 'make it beautiful' block
###############################################


###############################################
# ------- Tackle NA's

aj <- anti_join(p_one_gd, p_one_wsd)

p_one_wsd %>%
  filter(date_time < ymd_hms("2020-06-03 16:00:00") & date_time > ymd_hms("2020-06-03 12:00:00") )
# in p_one_wsd the value for 2020-06-03 14:00:00 is missing

aj <- anti_join(p_one_wsd, p_one_gd)

missing_dates <- aj$date_time
  
p_one_gd %>%
  filter(date_time %in% aj$date_time) # ymd_hms(c("2020-05-20 22:00:00", "2020-05-20 22:15:00"))) just to check it works that way
# in p_one_gd there are missing data for 25 time points

p_one <- inner_join(p_one_gd, p_one_wsd, by = "date_time")
p_two <- inner_join(p_two_gd, p_two_wsd, by = "date_time")

# how many observations are missing? Well, we take a measurement every 15 minutes, so 4 measurements an hour, we have 24 hours in a day, 34 days in data set and 22 inverters
4 * 24 * 34 * 22 # observations for each power plant should be

# -------
###############################################

###############################################
#-------- Total yield issue

# I don't think the total_yield is legit. Look at the chart
hist(p_one_gd$total_yield) # doesn't seem even (or consistent)

# nah, it's legit
p_one %>%
  filter(p_date == ymd("2020-05-15"), p_time == as_hms(0)) %>%
  ggplot(aes(total_yield)) + geom_histogram()

max(p_one$daily_yield)
(max(p_one_gd$total_yield) / max(p_one_gd$daily_yield))/ 365
(min(p_one_gd$total_yield) / max(p_one_gd$daily_yield))/ 365

# Let alone 600+ years of work
(max(p_two_gd$total_yield) / max(p_two_gd$daily_yield)) / 365

p_one_gd$source_key <- as.factor(p_one_gd$source_key)

head(p_one_gd %>%
  filter(as.numeric(source_key) == 1, ac_power > 0))

head(p_one_gd %>%
       filter(ac_power > 0, date_time == ymd("2020-05-15")), 10)



#--------
###############################################

p_two %>%
  filter(p_time == as_hms(60 * 60 * 24 - 15 * 60) & as.numeric(source_key) < 6) %>%
  ggplot(aes(p_date, daily_yield, color = source_key)) + geom_point() +
  geom_path()
# Maybe some panels were down during daytime, that's why the lines on the plot don't move along

# Or maybe some panels don't generate power during some periods. Lets find it out
p_two %>%
  filter(daily_yield > 0) %>%
  summarise(n = n(), unq = length(unique(daily_yield)), 
            per = (n - unq) * 100 / n)
# 44.91796 % of non-zero values are not unique. Probably sometimes panels don't generate power while being set on(online)


##############################################################
# Better idea how to find the number of observations repeating themselves

yield_vec <- p_two %>%
  filter(irradiation > 0) %>% # daily_yield
  arrange(source_key, p_date, p_time) %>%
  pull(daily_yield)

yield_vec_skew <- c(0, yield_vec)
# comparing each non-zero daily_yield with it's predecessor
rep_ind <- c(yield_vec, 0) == yield_vec_skew

# We don't need the first element because we create it to skew the yield_vec
rep_ind <- rep_ind[2:length(rep_ind)]

sum(as.numeric(rep_ind))
# 5013 observations are the same as observation before. The total number of observations is 38723

# let's take a look at this observations
head(p_two %>%
        filter(irradiation > 0) %>% 
        arrange(source_key, p_date, p_time) %>%
        slice(which(rep_ind)))
# The sun is shining, module_temperature grows like on steroids and the module doesn't generate the power. This is a malfunction


# Now we need to eliminate malfunction data from the data set
p_two_clean <- p_two %>%
 # mutate(daily_yield = ifelse((irradiation == 0 & p_time < hms(60 * 60 * 12)), 0, daily_yield)) %>% # no need for that
  filter(irradiation > 0) %>% 
  arrange(source_key, p_date, p_time) %>%
  slice(which(!rep_ind))

# New idea how to solve total_yield drop
p_two_clean <- p_two_clean %>%
  mutate(l_source_key = c(1, source_key)[1:n()],
         l_daily_yield = c(0, daily_yield)[1:n()]) %>%
  mutate(sk_comp = source_key == l_source_key,
         dy_comp = daily_yield < l_daily_yield) %>%
  slice(which(!(sk_comp & dy_comp))) %>%
  select(-l_source_key, -l_daily_yield, -sk_comp, -dy_comp)

# Creating cumulative daily yield
dyc1 <- p_two_clean$daily_yield
dyc2 <- c(0, dyc1[1:(length(dyc1) - 1)])
dyc <- dyc1 - dyc2
#dyc[which(p_two_clean$daily_yield == 0)] <- 0

p_two_clean <- p_two_clean %>%
  mutate(daily_yield_c = dyc) %>%
  relocate(daily_yield_c, .after = daily_yield)

# Plotting irradiation against daily_yield_c to see how clean the data is
ggplot(p_two_clean, aes(irradiation, daily_yield_c)) + geom_point()
# A lot of anomalies, let's look at them
head(p_two_clean %>%
       filter(daily_yield_c < -1000))
# We will take the first row from there to take a closer look
head(p_two_clean %>%
       filter(p_date == ymd("2020-05-17") & p_time > hms(60 * 60 * 11) &
                source_key == "4UPUqMRk7TRMgml"), 15)
# In the same rows daily_yield_c goes negative, total_yield drops. total_yield only goes up by design. This is a malfunction. We should delete this rows. This way we will not only eliminate some negative values but also extremely large positive ones from daily_yield_c

# The idea how to solve total_yield drop. We need to put this chunk of code before we create dyc
slv_drp_vctrs <- p_two_clean %>%
  mutate(l_source_key = c(1, source_key)[1:n()],
         l_daily_yield = c(0, daily_yield)[1:n()]) %>%
  mutate(sk_comp = source_key == l_source_key,
         dy_comp = daily_yield < l_daily_yield) %>%
  slice(which(!(sk_comp & dy_comp))) %>%
  select(-l_source_key, -l_daily_yield, -sk_comp, -dy_comp)



###########################################################
###########################################################
# We don't need the code below
malf_list <- sapply(unique(p_two_clean$source_key), function(xx){
  p_two_clean %>%
    filter(source_key == xx) %>%
    mutate(init_indx = 1:n()) %>%
    arrange(total_yield) %>%
    mutate(arrgd_indx = 1:n()) %>%
    mutate(malf = init_indx > arrgd_indx) %>%
    arrange(p_date, p_time) %>%
    pull(malf)
})
str(malf_list[[1]]) # This is how we going to use it

malf_vec <- c(0)
for (xx in 1:length(malf_list)){
  malf_vec <- c(malf_vec, malf_list[[xx]])
}
malf_vec <- malf_vec[2: length(malf_vec)]

p_two_clean %>%
  slice(which(!malf_vec)) %>%
  ggplot(aes(irradiation, daily_yield_c)) + geom_point()
# looks a bit better

p_two_clean <- p_two_clean %>%
  slice(which(!malf_vec))
# let's continue the research
head(p_two_clean %>%
       filter(daily_yield_c < -1000))
# We will take the first row from there to take a closer look
head(p_two_clean %>%
       filter(p_date == ymd("2020-06-06") & p_time > hms(60 * 60 * 3) &
                source_key == "4UPUqMRk7TRMgml"), 15)

head(p_two_clean %>%
       filter((p_date == ymd("2020-06-05") & p_time > hms(60 * 60 * 18) &
                source_key == "4UPUqMRk7TRMgml") | 
       (p_date == ymd("2020-06-06") & p_time > hms(60 * 60 * 3) &
       source_key == "4UPUqMRk7TRMgml")), 15)


# I have an idea to clean daily_yield before we derive daily_yield_c

# First we set zeros to daily_yield when it's early in the morning
p_two_pre_clean <- p_two %>%
  mutate(daily_yield = ifelse((irradiation == 0 & p_time < hms(60 * 60 * 12)), 0, daily_yield)) %>%
  arrange(source_key, p_date, p_time)

# Some dark magic
#clean5_total_yield <- as.character(p_two_pre_clean$total_yield)
#clean5_total_yield <- as.numeric(clean5_total_yield)
#p_two_pre_clean <- p_two_pre_clean %>%
#  mutate(total_yield = clean5_total_yield)
# doesn't work

# Trying another dark magic
ty <- p_two_pre_clean$total_yield
ty2 <- ty
any(!(ty == ty2))

ty2 <- c(0, ty2)[1:length(ty2)]
any(!(ty[1:(length(ty) - 1)] == ty2[2:length(ty2)]))

length(ty2) == nrow(p_two_pre_clean)
any(!(p_two_pre_clean$total_yield[1:(length(p_two_pre_clean$total_yield) - 1)] == ty2[2:length(ty2)]))

p_two_pre_clean$l_total_yield <- ty2

# The idea how to solve total_yield drop. We need to put this chunk of code before we create dyc
p_two_pre_clean <- p_two_pre_clean %>%
  mutate(l_source_key = c(1, source_key)[1:n()] ) %>%#,
    #     l_total_yield = ty2) %>%
     #    l_total_yield = c(0, total_yield)[1:n()]) %>%
  mutate(sk_comp = source_key == l_source_key,
         ty_comp = total_yield < l_total_yield) #%>%
 # slice(which(!(sk_comp & ty_comp)))# %>%
#  select(-l_source_key, -l_daily_yield, -sk_comp, -dy_comp)

any(!(p_two_pre_clean$l_total_yield == ty2))

# Than we compute daily_yield_c
dyc1 <- p_two_pre_clean$daily_yield
dyc2 <- c(0, dyc1[1:(length(dyc1) - 1)])
dyc <- dyc1 - dyc2

# Add new feature to the temporary data set
p_two_pre_clean <- p_two_pre_clean %>%
  mutate(daily_yield_c = dyc) %>%
  relocate(daily_yield_c, .after = daily_yield)

yield_vec2 <- p_two_pre_clean %>%
  filter(irradiation > 0) %>% 
  arrange(source_key, p_date, p_time) %>%
  pull(daily_yield)

yield_vec_skew2 <- c(0, yield_vec2)
# comparing each non-zero daily_yield with it's predecessor
rep_ind2 <- c(yield_vec2, 0) == yield_vec_skew2

# We don't need the first element because we create it to skew the yield_vec
rep_ind2 <- rep_ind2[2:length(rep_ind2)]

# And transform the data set the way we need
p_two_clean2 <- p_two_pre_clean %>%
  filter(irradiation > 0) %>% 
  arrange(source_key, p_date, p_time) %>%
  slice(which(!rep_ind2))

ggplot(p_two_clean2, aes(irradiation, daily_yield_c)) + geom_point()

head(p_two_clean2 %>%
       filter(daily_yield_c < -1000))

head(p_two_clean2 %>%
       filter(p_date == ymd("2020-06-02") & p_time > hms(60 * 60 * 15) &
                source_key == 23), 15)





head(p_two %>%
       filter(p_date == ymd("2020-05-20") & p_time > hms(60 * 60 * 6)))

# Plotting irradiation against ac_power + dc_power to see how clean the data is
ggplot(p_two_clean, aes(irradiation, ac_power + dc_power)) + geom_point()
# The trend is clear, but we still have some anomalies

#------------
##############################################################


# This is the first try of the code block above. Why I have chosen ac_power - no idea
p_two %>%
  filter(ac_power > 0) %>%
  summarise(n = n(), unq = length(unique(daily_yield)), 
            dy_0 = length(which(daily_yield == 0)), per = (n - unq - dy_0) * 100 / n)
# 7.397927 % of non-zero values are not unique. Probably sometimes panels don't generate power while being set on(online)


# ok let's see generated power against sun radiation