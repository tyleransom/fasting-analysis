library(tidyverse)
library(magrittr)
library(lubridate)
library(tsibble)
library(fable)

load('../data/cleaned/daily_fitbit.rda')

# make some figures of weight and fasting duration and steps
# graph of fasting duration
fasting %>% autoplot(cumul_dur_dec) + 
            theme_minimal()+
            xlab("Calendar date") +
            ylab("Fasting duration (hours)") +
            ylim(0, 100) +
            scale_y_continuous(breaks = seq(12, 96, by = 12))
ggsave('../exhibits/figures/fasting_duration.png')

# graph of weight
all %>% autoplot(weight) + 
        theme_minimal()+
        xlab("Calendar date") +
        ylab("Weight (lbs)") +
        ylim(140, 170)
ggsave('../exhibits/figures/weight.png')

# graph of steps
fasting %>% autoplot(steps) + 
            theme_minimal()+
            xlab("Calendar date") +
            ylab("Steps") +
            ylim(0, 20000) +
            scale_y_continuous(breaks = seq(2000, 20000, by = 2000))
ggsave('../exhibits/figures/steps.png')

# see https://fabletools.tidyverts.org/reference/decomposition_model.html
# ETS try

