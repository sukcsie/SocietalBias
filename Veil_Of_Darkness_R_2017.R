rm(list = ls())

## The analysis here is from this tutorial: https://openpolicing.stanford.edu/tutorials/

## Libraries to include
library(tidyverse)
library(lubridate)

# For Veil of Darkness
library(lutz)
library(suncalc)
library(splines)

## Load the data
setwd("~/Documents/CS_Research/Data")
stops <- read_csv("df_ca_clean_2017.csv")
pop <- read_csv("df_cen_clean.csv")
agg_stops <- read_csv("df_agg_ca_clean_2017.csv")

# getting rid of X1 because it is the same as the index
stops <- stops[-1]

# Basics

# Looking at the dataset columns
colnames(stops)
# Looking at the number of rows
nrow(stops)
# Looking at the minimum and maximum stop datas
# These should be 01-01-2017, and 12-31-2017
min(stops$date)
max(stops$date)
# We only want to look at vehicular stops for veil of darkness analysis, so we will filter this type of stop out
stops <- stops %>% filter(type == "vehicular")
# Looking at the new number of rows we have
stops %>% nrow()
# Looking at the new number of rows we have by city
# The cities we should have are: Bakersfield, Long Beach, Los Angeles, Oakland and San Jose
stops %>% 
  count(city)
# Looking at the number of stops by race
# At this point we should have black, hispanic and white
stops %>% 
  count(subject_race)
# Looking at the proportion of stops by race
stops %>% 
  count(subject_race) %>% 
  mutate(prop = n / sum(n))

# Proportion of the population that is each race
pop$black_pop_prop = pop$black_pop / pop$total_pop
pop$hispanic_pop_prop = pop$hispanic_pop / pop$total_pop
pop$white_pop_prop = pop$white_pop / pop$total_pop

stops_and_pop <- agg_stops %>% 
  left_join(
    pop,
    by = "city"
  )
stops_and_pop['black_per_cap'] <- stops_and_pop$black/stops_and_pop$black_pop
stops_and_pop['hispanic_per_cap'] <- stops_and_pop$hispanic/stops_and_pop$hispanic_pop
stops_and_pop['white_per_cap'] <- stops_and_pop$white/stops_and_pop$white_pop
print(stops_and_pop[,c(1,13,14,15)])


# Take the unweighted avg
black_stop_rate_mean = mean(as.numeric(stops_and_pop$black_per_cap))
hispanic_stop_rate_mean = mean(as.numeric(stops_and_pop$hispanic_per_cap))
white_stop_rate_mean = mean(as.numeric(stops_and_pop$white_per_cap))

# Getting the white, black and hispanic population for all of the cities 
white_pop_sum = sum(stops_and_pop['white_pop'])
hispanic_pop_sum = sum(stops_and_pop['hispanic_pop'])
black_pop_sum = sum(stops_and_pop['black_pop'])

# Calculating confidence interval between black and white stops:
# I did this by hand, there might be a built in function that does it as well
# The formula is from this website: https://online.stat.psu.edu/stat100/lesson/9/9.3, and https://stattrek.com/estimation/difference-in-proportions.aspx
# z = 2.58 for a 99% Confidence interval 

lower_bound = (white_stop_rate_mean - black_stop_rate_mean) - 2.58*sqrt(((white_stop_rate_mean * (1 - white_stop_rate_mean))/white_pop_sum)+ ((black_stop_rate_mean * (1 - black_stop_rate_mean))/black_pop_sum))
upper_bound = (white_stop_rate_mean - black_stop_rate_mean) + 2.58*sqrt(((white_stop_rate_mean * (1 - white_stop_rate_mean))/white_pop_sum)+ ((black_stop_rate_mean * (1 - black_stop_rate_mean))/black_pop_sum))

cat('The 99% confidence interval for the difference between the proportion of white and black drivers stopped is is ',lower_bound,',',upper_bound)


# Calculating confidence interval between hispanic and white stops:
# I did this by hand, there might be a built in function that does it as well
# The formula is from this website: https://online.stat.psu.edu/stat100/lesson/9/9.3, and https://stattrek.com/estimation/difference-in-proportions.aspx
# z = 2.58 for a 99% Confidence interval 

lower_bound = (white_stop_rate_mean - hispanic_stop_rate_mean) - 2.58*sqrt(((white_stop_rate_mean * (1 - white_stop_rate_mean))/white_pop_sum)+ ((hispanic_stop_rate_mean * (1 - hispanic_stop_rate_mean))/hispanic_pop_sum))
upper_bound = (white_stop_rate_mean - hispanic_stop_rate_mean) + 2.58*sqrt(((white_stop_rate_mean * (1 - white_stop_rate_mean))/white_pop_sum)+ ((hispanic_stop_rate_mean * (1 - hispanic_stop_rate_mean))/hispanic_pop_sum))

cat('The confidence interval for the difference between the proportion of white and hispanic drivers stopped is is ',lower_bound,',',upper_bound)

# How much more often black drivers are stoped than white drivers
# dividing the unweighted average stop rate for black drivers by the unweighted average stop rate for white drivers
print(black_stop_rate_mean/white_stop_rate_mean)

# How much more often hispanic drivers are stoped than white drivers
# dividing the unweighted average stop rate for hispanic drivers by the unweighted average stop rate for white drivers
print(hispanic_stop_rate_mean/white_stop_rate_mean)

# Veil of Darkness Analysis
# In the tutorial they get the data by city, and each city has a different sunset time
# I did all the cleaning once for each city and then concatinated them all at the end to get the final data set
# Other than those changes, it follows the tutorial exactly

# Bakersfield: 35.3733° N, 119.0187° W
# For Bakersfield
center_lat = 35.3733
center_lng = -119.0187
tz <- lutz::tz_lookup_coords(center_lat, center_lng, warn = F)

# Helper function
time_to_minute <- function(time) {
  hour(hms(time)) * 60 + minute(hms(time))
}

stops_b <- stops %>% filter(city == "Bakersfield")
# Compute sunset time for each date in our dataset
sunset_times <- 
  stops_b %>%
  mutate(
    lat = center_lat,
    lon = center_lng
  ) %>% 
  select(date, lat, lon) %>%
  distinct() %>%
  getSunlightTimes(
    data = ., 
    keep = c("sunset", "dusk"), 
    tz = tz
  ) %>% 
  mutate_at(vars("sunset", "dusk"), ~format(., "%H:%M:%S")) %>% 
  mutate(
    sunset_minute = time_to_minute(sunset),
    dusk_minute = time_to_minute(dusk),
    date = ymd(str_sub(date, 1, 10))
  ) %>% 
  select(date, sunset, dusk, ends_with("minute"))

sunset_times %>% 
  filter(dusk == min(dusk) | dusk == max(dusk))

vod_stops_b <- 
  stops_b %>% 
  left_join(
    sunset_times,
    by = "date"
  ) %>% 
  mutate(
    minute = time_to_minute(time),
    minutes_after_dark = minute - dusk_minute,
    is_dark = minute > dusk_minute,
    min_dusk_minute = min(dusk_minute),
    max_dusk_minute = max(dusk_minute),
    is_black = subject_race == "black"
  ) %>% 
  filter(
    # Filter to get only the intertwilight period
    minute >= min_dusk_minute,
    minute <= max_dusk_minute,
    # Remove ambigous period between sunset and dusk
    !(minute > sunset_minute & minute < dusk_minute),
    # Compare only white and black drivers
    subject_race %in% c("black", "white")
  )

vod_stops_b %>% nrow()

# an example of one 15 minute period
vod_stops_b %>% 
  filter(time > hm("18:30"), time < hm("18:45")) %>% 
  group_by(is_dark) %>% 
  summarize(prop_black = mean(is_black))

# Los Angeles: 34.0522° N, 118.2437° W
# For Los Angeles
center_lat = 34.0522
center_lng = -118.2437
tz <- lutz::tz_lookup_coords(center_lat, center_lng, warn = F)

# Helper function
time_to_minute <- function(time) {
  hour(hms(time)) * 60 + minute(hms(time))
}

stops_la <- stops %>% filter(city == "Los Angeles")
# Compute sunset time for each date in our dataset
sunset_times <- 
  stops_la %>%
  mutate(
    lat = center_lat,
    lon = center_lng
  ) %>% 
  select(date, lat, lon) %>%
  distinct() %>%
  getSunlightTimes(
    data = ., 
    keep = c("sunset", "dusk"), 
    tz = tz
  ) %>% 
  mutate_at(vars("sunset", "dusk"), ~format(., "%H:%M:%S")) %>% 
  mutate(
    sunset_minute = time_to_minute(sunset),
    dusk_minute = time_to_minute(dusk),
    date = ymd(str_sub(date, 1, 10))
  ) %>% 
  select(date, sunset, dusk, ends_with("minute"))

sunset_times %>% 
  filter(dusk == min(dusk) | dusk == max(dusk))

vod_stops_la <- 
  stops_la %>% 
  left_join(
    sunset_times,
    by = "date"
  ) %>% 
  mutate(
    minute = time_to_minute(time),
    minutes_after_dark = minute - dusk_minute,
    is_dark = minute > dusk_minute,
    min_dusk_minute = min(dusk_minute),
    max_dusk_minute = max(dusk_minute),
    is_black = subject_race == "black"
  ) %>% 
  filter(
    # Filter to get only the intertwilight period
    minute >= min_dusk_minute,
    minute <= max_dusk_minute,
    # Remove ambigous period between sunset and dusk
    !(minute > sunset_minute & minute < dusk_minute),
    # Compare only white and black drivers
    subject_race %in% c("black", "white")
  )

vod_stops_la %>% nrow()

vod_stops_la %>% 
  filter(time > hm("18:30"), time < hm("18:45")) %>% 
  group_by(is_dark) %>% 
  summarize(prop_black = mean(is_black))

# For Oakland
# Oakland: 37.8044° N, 122.2712° W

center_lat = 37.8044
center_lng = -122.2712
tz <- lutz::tz_lookup_coords(center_lat, center_lng, warn = F)

# Helper function
time_to_minute <- function(time) {
  hour(hms(time)) * 60 + minute(hms(time))
}

stops_o <- stops %>% filter(city == "Oakland")
# Compute sunset time for each date in our dataset
sunset_times <- 
  stops_o %>%
  mutate(
    lat = center_lat,
    lon = center_lng
  ) %>% 
  select(date, lat, lon) %>%
  distinct() %>%
  getSunlightTimes(
    data = ., 
    keep = c("sunset", "dusk"), 
    tz = tz
  ) %>% 
  mutate_at(vars("sunset", "dusk"), ~format(., "%H:%M:%S")) %>% 
  mutate(
    sunset_minute = time_to_minute(sunset),
    dusk_minute = time_to_minute(dusk),
    date = ymd(str_sub(date, 1, 10))
  ) %>% 
  select(date, sunset, dusk, ends_with("minute"))

sunset_times %>% 
  filter(dusk == min(dusk) | dusk == max(dusk))

vod_stops_o <- 
  stops_o %>% 
  left_join(
    sunset_times,
    by = "date"
  ) %>% 
  mutate(
    minute = time_to_minute(time),
    minutes_after_dark = minute - dusk_minute,
    is_dark = minute > dusk_minute,
    min_dusk_minute = min(dusk_minute),
    max_dusk_minute = max(dusk_minute),
    is_black = subject_race == "black"
  ) %>% 
  filter(
    # Filter to get only the intertwilight period
    minute >= min_dusk_minute,
    minute <= max_dusk_minute,
    # Remove ambigous period between sunset and dusk
    !(minute > sunset_minute & minute < dusk_minute),
    # Compare only white and black drivers
    subject_race %in% c("black", "white")
  )

vod_stops_o %>% nrow()

vod_stops_o %>% 
  filter(time > hm("18:30"), time < hm("18:45")) %>% 
  group_by(is_dark) %>% 
  summarize(prop_black = mean(is_black))

# For San Jose
# San Jose:  37.3382° N, 121.8863° W
center_lat = 37.3382
center_lng = -121.8863
tz <- lutz::tz_lookup_coords(center_lat, center_lng, warn = F)

# Helper function
time_to_minute <- function(time) {
  hour(hms(time)) * 60 + minute(hms(time))
}

stops_sj <- stops %>% filter(city == "San Jose")
# Compute sunset time for each date in our dataset
sunset_times <- 
  stops_sj %>%
  mutate(
    lat = center_lat,
    lon = center_lng
  ) %>% 
  select(date, lat, lon) %>%
  distinct() %>%
  getSunlightTimes(
    data = ., 
    keep = c("sunset", "dusk"), 
    tz = tz
  ) %>% 
  mutate_at(vars("sunset", "dusk"), ~format(., "%H:%M:%S")) %>% 
  mutate(
    sunset_minute = time_to_minute(sunset),
    dusk_minute = time_to_minute(dusk),
    date = ymd(str_sub(date, 1, 10))
  ) %>% 
  select(date, sunset, dusk, ends_with("minute"))

sunset_times %>% 
  filter(dusk == min(dusk) | dusk == max(dusk))

vod_stops_sj <- 
  stops_sj %>% 
  left_join(
    sunset_times,
    by = "date"
  ) %>% 
  mutate(
    minute = time_to_minute(time),
    minutes_after_dark = minute - dusk_minute,
    is_dark = minute > dusk_minute,
    min_dusk_minute = min(dusk_minute),
    max_dusk_minute = max(dusk_minute),
    is_black = subject_race == "black"
  ) %>% 
  filter(
    # Filter to get only the intertwilight period
    minute >= min_dusk_minute,
    minute <= max_dusk_minute,
    # Remove ambigous period between sunset and dusk
    !(minute > sunset_minute & minute < dusk_minute),
    # Compare only white and black drivers
    subject_race %in% c("black", "white")
  )

vod_stops_sj %>% nrow()

vod_stops_sj %>% 
  filter(time > hm("18:30"), time < hm("18:45")) %>% 
  group_by(is_dark) %>% 
  summarize(prop_black = mean(is_black))

# Some of them don't have very many stops for each city
# Combining all of the cities together
vod_stops <- rbind(vod_stops_b,vod_stops_la,vod_stops_o, vod_stops_sj)
vod_stops %>% nrow()
vod_stops %>% 
  filter(time > hm("18:30"), time < hm("18:45")) %>% 
  group_by(is_dark) %>% 
  summarize(prop_black = mean(is_black))

# The two models are directly from the tutorial, except instead of district I used city as the factor variable in model two
# This allowed us to account for which city the stop was made in
# In the paper, we only presented the results from model 2

mod1 <- glm(
  is_black ~ is_dark + splines::ns(minute, df = 6),
  family = binomial,
  data = vod_stops
)

summary(mod1)$coefficients["is_darkTRUE", c("Estimate", "Std. Error")]

summary(mod1) # To get the Z Coefficient

# Here I did city instead of district, to account for location, since we combinded the information from all of the cities. 
mod2 <- glm(
  is_black ~ is_dark + splines::ns(minute, df = 6) + as.factor(city),
  family = binomial,
  data = vod_stops
)

summary(mod2)$coefficients["is_darkTRUE", c("Estimate", "Std. Error")]

summary(mod2) # To get the Z Coefficient