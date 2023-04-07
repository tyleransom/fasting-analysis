library(tidyverse)
library(magrittr)
library(lubridate)
library(tsibble)
library(tseries)
library(car)
library(estimatr)
library(modelsummary)

load('../data/cleaned/daily_fitbit.rda')

#lm(weight ~ sleephrs + I(steps/1e3) + cumul_dur_dec + ate_restaurant + ate_pizza, data = all) %>% summary()

Count <- function(x) sum(!is.na(x))
datasummary(steps + sleephrs + cumul_dur_dec + day + month ~ Mean + Count, all, output="markdown") %>% print
all %<>% filter(date > as_date('2019-11-16')) %>% mutate(steps = na_if(steps,0),
                                                         daynum = day,
                                                         monthnum = month,
                                                         dayname = case_when(daynum==1 ~ "Sun", daynum==2 ~ "Mon", daynum==3 ~ "Tue", daynum==4 ~ "Wed", daynum==5 ~ "Thu", daynum==6 ~ "Fri", daynum==7 ~ "Sat"),
                                                         day = as.factor(dayname),
                                                         monthname = case_when(month==1 ~ "Jan", month==2 ~ "Feb", month==3 ~ "Mar", month==4 ~ "Apr", month==5 ~ "May", month==6 ~ "Jun", month==7 ~ "Jul", month==8 ~ "Aug", month==9 ~ "Sep", month==10 ~ "Oct", month==11 ~ "Nov", month==12 ~ "Dec"),
                                                         month = as.factor(monthname))
all %<>% drop_na(steps,sleephrs,cumul_dur_dec,day,month)
datasummary(steps + sleephrs + day + month ~ Mean + Count, all, output="markdown") %>% print

# is there a unit root?
adf.test(log(all$steps), k=1) %>% print
adf.test(log(all$sleephrs), k=1) %>% print
adf.test(log(all$cumul_dur_dec), k=1) %>% print
#adf.test(all$weight, k=1) %>% print

asgeag

# time series regression
est1 <- lm_robust(log(sleephrs) ~ lag(log(sleephrs)) + log(steps) + lag(log(steps)), data=all)
est1a<-        lm(log(sleephrs) ~ lag(log(sleephrs)) + log(steps) + lag(log(steps)), data=all)
glance(est1a) %>% print

est2 <- lm_robust(log(sleephrs) ~ lag(log(sleephrs)) + log(steps) + lag(log(steps)), data=all)
est2a<-        lm(log(sleephrs) ~ lag(log(sleephrs)) + log(steps) + lag(log(steps)), data=all)
glance(est2a) %>% print

linearHypothesis(est2, c("daySun=0", "dayMon=0", "dayTue=0", "dayWed=0", "dayThu=0", "daySat=0",
                         "monthAug=0", "monthDec=0", "monthFeb=0", "monthJan=0", "monthJul=0", "monthJun=0", "monthMar=0", "monthMay=0", "monthNov=0", "monthOct=0", "monthSep=0")) %>% print
linearHypothesis(est2a, c("daySun=0", "dayMon=0", "dayTue=0", "dayWed=0", "dayThu=0", "daySat=0",
                          "monthAug=0", "monthDec=0", "monthFeb=0", "monthJan=0", "monthJul=0", "monthJun=0", "monthMar=0", "monthMay=0", "monthNov=0", "monthOct=0", "monthSep=0")) %>% print
((deviance(est1a)-deviance(est2a))/17)/(deviance(est2a)/est2a$df)

modelsummary(list(est1,est2),stars=T,output="markdown") %>% print

