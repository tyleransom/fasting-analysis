library(tidyverse)
library(lubridate)
library(jsonlite)
library(magrittr)
library(tsibble)

## import calorie data
#out <- system('ls ../data/raw/user-site-export/calories*.json', intern=T)
#calories <- list()
#for (f in seq(1,length(out))) {
#    temp <- fromJSON(out[f]) %>% as_tibble() %>% 
#            mutate(value = as.numeric(value)) %>%
#            separate(dateTime, c("date","time"), sep=" ") %>%
#            group_by(date) %>% summarize(calories = sum(value)) %>%
#            ungroup() %>%
#            mutate(date = mdy(date))
#    calories %<>% bind_rows(temp)
#    temp <- NULL
#}

## import distance data
#out <- system('ls ../data/raw/user-site-export/distance*.json', intern=T)
#distance <- list()
#for (f in seq(1,length(out))) {
#    temp <- fromJSON(out[f]) %>% as_tibble() %>% 
#            mutate(value = as.numeric(value)) %>%
#            separate(dateTime, c("date","time"), sep=" ") %>%
#            group_by(date) %>% summarize(distance = sum(value)) %>%
#            ungroup() %>%
#            mutate(date = mdy(date))
#    distance %<>% bind_rows(temp)
#    temp <- NULL
#}

# import steps data
out <- system('ls ../data/raw/user-site-export/steps*.json', intern=T)
steps <- list()
for (f in seq(1,length(out))) {
    temp <- fromJSON(out[f]) %>% as_tibble() %>% 
            mutate(value = as.numeric(value)) %>%
            separate(dateTime, c("date","time"), sep=" ") %>%
            group_by(date) %>% summarize(steps = sum(value)) %>%
            ungroup() %>%
            mutate(date = mdy(date))
    steps %<>% bind_rows(temp)
    temp <- NULL
}

# import sleep data
out <- system('ls ../data/raw/user-site-export/sleep*.json', intern=T)
sleep <- list()
for (f in seq(1,length(out))) {
    temp <- fromJSON(out[f]) %>% as_tibble() %>% 
            group_by(dateOfSleep) %>% 
            summarize(minutesAsleep = sum(minutesAsleep),
                      minutesAwake = sum(minutesAwake)) %>%
            ungroup() %>%
            rename(date = dateOfSleep) %>%
            mutate(date = as_date(date))
    sleep %<>% bind_rows(temp)
    temp <- NULL
}

## import very active mins data
#out <- system('ls ../data/raw/user-site-export/very_active*.json', intern=T)
#veryactivemin <- list()
#for (f in seq(1,length(out))) {
#    temp <- fromJSON(out[f]) %>% as_tibble() %>% 
#            mutate(value = as.numeric(value)) %>%
#            separate(dateTime, c("date","time"), sep=" ") %>%
#            group_by(date) %>% summarize(veryactivemin = sum(value)) %>%
#            ungroup() %>%
#            mutate(date = mdy(date))
#    veryactivemin %<>% bind_rows(temp)
#    temp <- NULL
#}

# import weight data
out <- system('ls ../data/raw/user-site-export/weight*.json', intern=T)
weight <- list()
for (f in seq(1,length(out))) {
    temp <- fromJSON(out[f]) %>% as_tibble() %>% 
            select(date,weight) %>%
            mutate(date = mdy(date))
    weight %<>% bind_rows(temp)
    temp <- NULL
}

# remove duplicates
weight %<>% distinct(date, .keep_all = TRUE)
sleep  %<>% distinct(date, .keep_all = TRUE)
steps  %<>% group_by(date) %>% 
            summarize(steps = sum(steps)) 

#steps_temp <- steps %>% 
#  group_by(date) %>% # group by id and name
#  mutate(dup = if_else(n() > 1, TRUE, FALSE)) # create "dup" column

all <- left_join(steps,sleep, by=c("date")) %>%
       #left_join(sleep, by=c("date")) %>%
       #left_join(calories, by=c("date")) %>%
       #left_join(veryactivemin, by=c("date")) %>%
       left_join(weight, by=c("date"))

# unit conversions and dates (day of week, month of year, weekend, etc.)
all %<>% mutate(sleephrs = minutesAsleep/60,
                day = wday(date),
                year = year(date),
                month = month(date))

# read in archive fasting log data and merge with rest of the data
df <- read_csv("../data/raw/fasting.csv")
df %<>% mutate(date_began = mdy(date_began)) %>%
        mutate(across(where(is.logical), ~ifelse(is.na(.), FALSE, .)))
df %<>% select(-location,-notes,-day_of_week)

# read in Zero app fasting log data (since Feb 2023)
# Read the JSON data from the file
json_data <- fromJSON("../data/raw/biodata.json")
# Extract the fast_data
fast_data <- json_data$fast_data
# Process the fast_data
daily_fasts <- fast_data %>%
  mutate(
    StartDTM = with_tz(as.POSIXct(StartDTM, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"), tzone = "America/Chicago"),
    EndDTM = with_tz(as.POSIXct(EndDTM, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"), tzone = "America/Chicago"),
    date_began = format(StartDTM, "%Y-%m-%d"),
    start_time = format(StartDTM, "%H:%M:%S"),
    end_time = format(EndDTM, "%H:%M:%S"),
    duration_seconds = as.numeric(difftime(EndDTM, StartDTM, units = "secs")),
    cumul_duration = sprintf("%02d:%02d:%02d", 
                             duration_seconds %/% 3600, 
                             (duration_seconds %% 3600) %/% 60, 
                             duration_seconds %% 60),
    cumul_dur_dec = round(duration_seconds / 3600, 2)
  ) %>%
  # Filter to keep only rows from 2023-04-03 onwards
  filter(as.Date(date_began) >= as.Date("2023-04-03")) %>%
  group_by(date_began) %>%
  summarise(
    start_time = first(start_time),
    end_time = last(end_time),
    cumul_duration = {
      total_seconds <- sum(duration_seconds)
      sprintf("%02d:%02d:%02d", 
              total_seconds %/% 3600, 
              (total_seconds %% 3600) %/% 60, 
              total_seconds %% 60)
    },
    cumul_dur_dec = round(sum(duration_seconds) / 3600, 2)
  ) %>%
  ungroup() %>%
  mutate(
    date_began = as.Date(date_began),
    start_time = as.POSIXct(start_time, format = "%H:%M:%S"),
    end_time = as.POSIXct(end_time, format = "%H:%M:%S")
  )
# ensure matching column types in archived fasting data
df <- df %>%
  mutate(
    date_began = as.Date(date_began),
    start_time = as.POSIXct(start_time, format = "%H:%M:%S"),
    end_time = as.POSIXct(end_time, format = "%H:%M:%S"),
    cumul_duration = as.character(cumul_duration),
    cumul_dur_dec = as.numeric(cumul_dur_dec)
  )

df %<>% bind_rows(daily_fasts)

all %<>% left_join(df, by=c("date" = "date_began"))

# restrict to sample period with fasting data
all %<>% filter(date > as_date('2019-11-15')) %>% as_tsibble(index = date)

# linearly interpolate missing sleep and step observations
all %<>% mutate(sleephrs = ifelse(is.na(sleephrs), approx(x = date[!is.na(sleephrs)], y = sleephrs[!is.na(sleephrs)], xout = date)$y, sleephrs),
                steps    = ifelse(is.na(steps),    approx(x = date[!is.na(steps)],    y = steps[!is.na(steps)], xout = date)$y,       steps   ))

# create separate dataframe that has all the fasting duration observations
fasting <- all

# linearly interpolate missing weight observations
all %<>% filter(date > as_date('2020-01-21')) %>% 
         mutate(weight = ifelse(is.na(weight), approx(x = date[!is.na(weight)], y = weight[!is.na(weight)], xout = date)$y, weight))

save(all,fasting,file='../data/cleaned/daily_fitbit.rda')

#### Aggregate to monthly level
# Helper function to calculate mode
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

all_monthly <- all %>%
  index_by(yearmonth = ~ yearmonth(.)) %>%
  summarise(
    # Numeric columns - take mean
    across(c(steps, minutesAsleep, minutesAwake, weight, sleephrs, day, year, month, 
             cumul_dur_dec),
           ~ mean(., na.rm = TRUE)),
    
    # Logical columns - take sum (to count occurrences)
    across(c(ate_restaurant, ate_pizza, homemade_pizza, ate_ice_cream, holiday_period, 
             dry_fast, traveling, hosting_fam),
           ~ sum(., na.rm = TRUE)),
    
    # For time columns, we'll take the most frequent (mode) value
    start_time = mode(start_time),
    end_time = mode(end_time),
    cumul_duration = mode(cumul_duration),
    
    .groups = "drop"
  )

save(all_monthly,fasting,file='../data/cleaned/monthly_fitbit.rda')