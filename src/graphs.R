library(tidyverse)
library(magrittr)
library(lubridate)
library(tsibble)
library(fable)
library(feasts)

load('../data/cleaned/daily_fitbit.rda')

#-------------------------------------------------------------------------------
# make some figures of weight and fasting duration and steps
#-------------------------------------------------------------------------------
# graph of fasting duration
all %>% autoplot(cumul_dur_dec) + 
            theme_minimal()+
            xlab("Calendar date") +
            ylab("Fasting duration (hours)") +
            ylim(0, 100) +
            scale_y_continuous(breaks = seq(12, 96, by = 12))
ggsave('../exhibits/figures/fasting_duration.png', width = 7, height = 7)

# graph of weight
all %>% autoplot(weight) + 
        theme_minimal()+
        xlab("Calendar date") +
        ylab("Weight (lbs)") +
        ylim(140, 170)
ggsave('../exhibits/figures/weight.png', width = 7, height = 7)

# graph of steps
all %>% autoplot(steps) + 
            theme_minimal()+
            xlab("Calendar date") +
            ylab("Steps") +
            ylim(0, 20000) +
            scale_y_continuous(breaks = seq(2000, 20000, by = 2000))
ggsave('../exhibits/figures/steps.png', width = 7, height = 7)

# graph of hours slept
all %>% autoplot(sleephrs) + 
  theme_minimal()+
  xlab("Calendar date") +
  ylab("Hours slept") +
  ylim(3, 9) +
  scale_y_continuous(breaks = seq(3, 9, by = 0.5))
ggsave('../exhibits/figures/sleep.png', width = 7, height = 7)


#-------------------------------------------------------------------------------
# STL decompositions
#-------------------------------------------------------------------------------
all %>% 
  model(STL(cumul_dur_dec ~ season(window = Inf))) %>% 
  components() %>% 
  autoplot() +
  theme_minimal()
ggsave('../exhibits/figures/fasting_STL.png', width = 12, height = 8, units = "in", dpi = 1200)

all %>% 
  model(STL(weight ~ season(window = Inf))) %>% 
  components() %>% 
  autoplot() +
  theme_minimal()
ggsave('../exhibits/figures/weight_STL.png', width = 12, height = 8, units = "in", dpi = 1200)

all %>% 
  model(STL(steps ~ season(window = Inf))) %>% 
  components() %>% 
  autoplot() +
  theme_minimal()
ggsave('../exhibits/figures/steps_STL.png', width = 12, height = 8, units = "in", dpi = 1200)

all %>% 
  model(STL(sleephrs ~ season(window = Inf))) %>% 
  components() %>% 
  autoplot() +
  theme_minimal()
ggsave('../exhibits/figures/sleep_STL.png', width = 12, height = 8, units = "in", dpi = 1200)


#-------------------------------------------------------------------------------
# plot just the trend component from each STL decomp, all on the same graph
#-------------------------------------------------------------------------------
test1 <- all %>% 
  model(STL(cumul_dur_dec ~ season(window = Inf))) %>% 
  components() %>% 
  select(Date=date, Trend=trend) %>%
  mutate(var = "Fasting Duration (hrs)")

test2 <- all %>% 
  model(STL(weight ~ season(window = Inf))) %>% 
  components() %>% 
  select(Date=date, Trend=trend) %>%
  mutate(var = "Weight (lbs)")

test3 <- all %>% 
  model(STL(steps ~ season(window = Inf))) %>% 
  components() %>% 
  select(Date=date, Trend=trend) %>%
  mutate(var = "Steps")

test4 <- all %>% 
  model(STL(sleephrs ~ season(window = Inf))) %>% 
  components() %>% 
  select(Date=date, Trend=trend) %>%
  mutate(var = "Sleep (hrs)") 

test.long <- bind_rows(as_tibble(test1),
                       as_tibble(test2),
                       as_tibble(test3),
                       as_tibble(test4))

# create the ggplot with facets
test.long %>%
  ggplot(aes(x = Date, y = Trend)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~ var, ncol = 1, scales = "free_y")
ggsave('../exhibits/figures/all_trends.png', width = 9, height = 6, units = "in", dpi = 1200)
