"
Data loading for oneshot hypothetical 

All feature normalizization happens in loading phase 
"


library(assertr)
library(comprehenr)
library(epidatr)
library(epiprocess)
library(broom)
library(lubridate)
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(vroom)
library(simplecolors)
library(ggpubr)
library(eply)
library(colorspace)






uspop = covidcast::state_census %>%
  select(ABBR, POPESTIMATE2019) %>%
  rename(geo_value = ABBR) %>%
  rename(pop = POPESTIMATE2019) %>%
  mutate(geo_value = tolower(geo_value)) %>%
  slice(., 2:(n() - 4))


in_raw = vroom("../../versioned_feature/rebuild_inpatient_raw_avg.csv") %>%
  filter(time_value >= as.Date("2020-11-01") & time_value <= as.Date("2023-08-01")) %>%
  filter(!geo_value %in%  c("as", "gu", "mp", "pr", "vi", "fm", "mh")) %>%
  mutate(time_value = as.Date(time_value), issue_date = as.Date(issue_date))  %>%
  mutate(weekly_per = 100 * weekly_in_ratio) %>%
  rename(backcast_lag = lag) %>%
  arrange(geo_value, time_value, issue_date)

out_raw = vroom("../../versioned_feature/rebuild_outpatient_raw_avg.csv") %>%
  filter(time_value >= as.Date("2020-11-01") & time_value <= as.Date("2023-08-01")) %>%
  filter(!geo_value %in%  c("as", "gu", "mp", "pr", "vi")) %>%
  mutate(time_value = as.Date(time_value), issue_date = as.Date(issue_date)) %>%
  rename(backcast_lag = lag) %>%
  group_by(geo_value, time_value, issue_date) %>%
  filter(n() == 1)

all_hosp = vroom("../../versioned_feature/ground_truth.csv") %>%
  filter(time_value >= as.Date("2020-11-01")) %>%
  filter(geo_value != "vi") %>%
  select(-issue_date) %>%
  inner_join(uspop, by = "geo_value") %>%
  mutate(GT = GT / pop * 10^5)

train_hosp = vroom("../../versioned_feature/oneshot_hosp.csv") %>%
  mutate(time_value = as.Date(time_value)) %>%
  filter(geo_value != "vi") %>%
  inner_join(uspop, by = "geo_value") %>%
  mutate(GT = GT / pop * 10^5)


# The ones we can see in training and validating 
train_dat = in_raw %>%
  inner_join(select(out_raw, geo_value, time_value, issue_date, weekly_out_ratio),
             by = c("geo_value", "time_value", "issue_date")) %>%
  inner_join(select(train_hosp, geo_value, time_value, GT),
             by = c("geo_value", "time_value")) %>%
  inner_join(uspop, by = "geo_value") 


# The ones we use in predicting 
dat = in_raw %>%
  inner_join(select(out_raw, geo_value, time_value, issue_date, weekly_out_ratio),
             by = c("geo_value", "time_value", "issue_date")) %>%
  inner_join(select(all_hosp, geo_value, time_value, GT),
             by = c("geo_value", "time_value")) %>%
  inner_join(uspop, by = "geo_value") 

