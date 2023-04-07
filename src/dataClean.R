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

# read in fasting log data and merge with rest of the data
df <- read_csv("../data/raw/fasting.csv")
df %<>% mutate(date_began = mdy(date_began)) %>%
        mutate(across(where(is.logical), ~ifelse(is.na(.), FALSE, .)))
df %<>% select(-location,-notes,-day_of_week)

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

