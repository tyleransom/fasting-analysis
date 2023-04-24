library(tidyverse)
library(magrittr)
library(lubridate)
library(tsibble)
library(tseries)
library(fable)
library(feasts)
library(dynlm)
library(modelsummary)

#-------------------------------------------------------------------------------
# set working directory
#-------------------------------------------------------------------------------
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

load('../data/cleaned/daily_fitbit.rda')

#-------------------------------------------------------------------------------
# data cleaning
#-------------------------------------------------------------------------------

# drop last observation (data corrupt)
all %<>% slice(-nrow(.))

# create "post two meals a day" variable
all %<>% mutate(post_tmad = case_when(date > as_date('2022-07-24') ~ "Post", TRUE ~ "Pre"))
post.tmad <- all %>% filter(date >= as_date('2022-07-24'))

#-------------------------------------------------------------------------------
# simple descriptive questions
#-------------------------------------------------------------------------------
# how many days since july 24 2022 have I gotten less than 7k steps?
ndays <- post.tmad %>% nrow
nmiss <- post.tmad %>% filter(steps<7000) %>% nrow
# note: this answer is wrong due to some way of aggregating steps over days in the FitBit API data
# the correct answer is 7 out of 251

# how many times per week did I eat out before/after TMAD?
agg <- all %>% as_tibble %>% group_by(post_tmad) %>% summarize(n_days = n(), n_pizza = sum(ate_pizza), n_restaurant = sum(ate_restaurant), n_ice_cream = sum(ate_ice_cream))
agg %<>% mutate(
                n_pizza_week = n_pizza/n_days*7,
                n_restaurant_week = n_restaurant/n_days*7,
                n_ice_cream_week = n_ice_cream/n_days*7,
               )
agg %>% select(post_tmad,ends_with("week")) %>% print


#-------------------------------------------------------------------------------
# transform time series to be detrended and seasonally adjusted
#-------------------------------------------------------------------------------
fasting_adj <- all %>% model(STL(cumul_dur_dec ~ season(window = Inf))) %>%
               components() %>%
               select(remainder)
all %<>% mutate(fasting_adj = fasting_adj$remainder)

weight_adj <- all %>% model(STL(weight ~ season(window = Inf))) %>%
              components() %>%
              select(remainder)
all %<>% mutate(weight_adj = weight_adj$remainder)

steps_adj <- all %>% model(STL(steps ~ season(window = Inf))) %>% 
             components() %>% 
             select(remainder)
all %<>% mutate(steps_adj = steps_adj$remainder/1000)

sleep_adj <- all %>% model(STL(sleephrs ~ season(window = Inf))) %>% 
             components() %>% 
             select(remainder)
all %<>% mutate(sleep_adj = sleep_adj$remainder)


#-------------------------------------------------------------------------------
# unit root tests
#-------------------------------------------------------------------------------
adf.test(all$fasting_adj, k=1) %>% print
adf.test(all$weight_adj,  k=1) %>% print
adf.test(all$steps_adj,   k=1) %>% print
adf.test(all$sleep_adj,   k=1) %>% print


#-------------------------------------------------------------------------------
# create some variables for later analysis
#-------------------------------------------------------------------------------
all %<>% mutate(long_fast     = cumul_dur_dec>=36)
all %<>% mutate(long_fast_adj = fasting_adj>=10)


#-------------------------------------------------------------------------------
# ARMA models
#-------------------------------------------------------------------------------
fit <- lm(weight_adj ~ lag(weight_adj)    + lag(weight_adj,2)    + lag(weight_adj,3)    + lag(weight_adj,4)    + lag(weight_adj,5)    + lag(weight_adj,6)    + lag(weight_adj,7)    + 
                       lag(steps_adj)     + lag(steps_adj,2)     + lag(steps_adj,3)     + lag(steps_adj,4)     + lag(steps_adj,5)     + lag(steps_adj,6)     + lag(steps_adj,7)     + 
                       lag(sleep_adj)     + lag(sleep_adj,2)     + lag(sleep_adj,3)     + lag(sleep_adj,4)     + lag(sleep_adj,5)     + lag(sleep_adj,6)     + lag(sleep_adj,7)     + 
                       lag(fasting_adj)   + lag(fasting_adj,2)   + lag(fasting_adj,3)   + lag(fasting_adj,4)   + lag(fasting_adj,5)   + lag(fasting_adj,6)   + lag(fasting_adj,7)   +
                       lag(long_fast_adj) + lag(long_fast_adj,2) + lag(long_fast_adj,3) + lag(long_fast_adj,4) + lag(long_fast_adj,5) + lag(long_fast_adj,6) + lag(long_fast_adj,7) +
                       lag(traveling)     + lag(traveling,2)     + lag(traveling,3)     + lag(traveling,4)     + lag(traveling,5)     + lag(traveling,6)     + lag(traveling,7)     +
                       lag(ate_pizza)     + lag(ate_pizza,2)     + lag(ate_pizza,3)     + lag(ate_pizza,4)     + lag(ate_pizza,5)     + lag(ate_pizza,6)     + lag(ate_pizza,7)     , data=all )
summary(fit)

fit <- lm(weight_adj ~ lag(weight_adj)    + lag(weight_adj,2)    + lag(weight_adj,3)    + lag(weight_adj,4)    + lag(weight_adj,5)    + lag(weight_adj,6)    + lag(weight_adj,7)    + 
                       lag(steps_adj)     + lag(steps_adj,2)     + lag(steps_adj,3)     + lag(steps_adj,4)     + lag(steps_adj,5)     + lag(steps_adj,6)     + lag(steps_adj,7)     + 
                       lag(sleep_adj)     + lag(sleep_adj,2)     + lag(sleep_adj,3)     + lag(sleep_adj,4)     + lag(sleep_adj,5)     + lag(sleep_adj,6)     + lag(sleep_adj,7)     +            
                       lag(fasting_adj)   + lag(fasting_adj,2)   + lag(fasting_adj,3)   + lag(fasting_adj,4)   + lag(fasting_adj,5)   + lag(fasting_adj,6)   + lag(fasting_adj,7)   +
                       lag(long_fast_adj) + lag(long_fast_adj,2) + lag(long_fast_adj,3) + lag(long_fast_adj,4) + lag(long_fast_adj,5) + lag(long_fast_adj,6) + lag(long_fast_adj,7) +
                       lag(traveling)     + lag(traveling,2)     + lag(traveling,3)     + lag(traveling,4)     + lag(traveling,5)     + lag(traveling,6)     + lag(traveling,7)     +
                       lag(ate_pizza)     + lag(ate_pizza,2)     + lag(ate_pizza,3)     + lag(ate_pizza,4)     + lag(ate_pizza,5)     + lag(ate_pizza,6)     + lag(ate_pizza,7)     , data=all %>% filter(date >= as_date('2022-07-24')))
summary(fit)

fit <- all %>% 
       model(
             ARIMA(
                   weight_adj ~ lag(weight_adj)    + lag(weight_adj,2)    + lag(weight_adj,3)    + lag(weight_adj,4)    + lag(weight_adj,5)    + lag(weight_adj,6)    + lag(weight_adj,7)    + 
                                lag(steps_adj)     + lag(steps_adj,2)     + lag(steps_adj,3)     + lag(steps_adj,4)     + lag(steps_adj,5)     + lag(steps_adj,6)     + lag(steps_adj,7)     + 
                                lag(sleep_adj)     + lag(sleep_adj,2)     + lag(sleep_adj,3)     + lag(sleep_adj,4)     + lag(sleep_adj,5)     + lag(sleep_adj,6)     + lag(sleep_adj,7)     +            
                                lag(fasting_adj)   + lag(fasting_adj,2)   + lag(fasting_adj,3)   + lag(fasting_adj,4)   + lag(fasting_adj,5)   + lag(fasting_adj,6)   + lag(fasting_adj,7)   +
                                lag(long_fast_adj) + lag(long_fast_adj,2) + lag(long_fast_adj,3) + lag(long_fast_adj,4) + lag(long_fast_adj,5) + lag(long_fast_adj,6) + lag(long_fast_adj,7) +
                                lag(traveling)     + lag(traveling,2)     + lag(traveling,3)     + lag(traveling,4)     + lag(traveling,5)     + lag(traveling,6)     + lag(traveling,7)     +
                                lag(ate_pizza)     + lag(ate_pizza,2)     + lag(ate_pizza,3)     + lag(ate_pizza,4)     + lag(ate_pizza,5)     + lag(ate_pizza,6)     + lag(ate_pizza,7)     
                  )
            )
report(fit)

#-------------------------------------------------------------------------------
# forecasting
#-------------------------------------------------------------------------------
my_dcmp_spec <- decomposition_model(
  STL(weight ~ season(window = Inf)),
  ETS(season_adjust ~ season("N")), SNAIVE(season_year)
)

all %>%
  filter(date < as_date('2022-07-24')) %>% 
  model(my_dcmp_spec) %>% 
  forecast(h="8 months") %>% 
  autoplot(all) + 
  theme_minimal() + 
  xlab("Calendar date") +
  ylab("Weight")
ggsave('../exhibits/figures/weight_forecast.png', width = 7, height = 7)
