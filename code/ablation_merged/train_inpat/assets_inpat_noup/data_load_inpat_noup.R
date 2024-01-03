"
Scripts for loading data, ablation study: only uses inpatient features
"


library(assertr)
library(comprehenr)
library(epidatr)
library(epiprocess)
library(broom)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(vroom)
library(simplecolors)
library(ggpubr)
library(eply)
library(colorspace)


"
All feature normalizization happens here 

No update: 2021-12-02 ~ 2023-08-01

"


uspop = covidcast::state_census %>%
  select(ABBR, POPESTIMATE2019) %>%
  rename(geo_value = ABBR) %>%
  rename(pop = POPESTIMATE2019) %>%
  mutate(geo_value = tolower(geo_value)) %>%
  slice(., 2:(n() - 4))


in_raw = vroom("../../../versioned_feature/rebuild_inpatient_raw_avg.csv") %>%
  filter(time_value >= as.Date("2020-11-01") & time_value <= as.Date("2023-08-01")) %>%
  filter(!geo_value %in%  c("as", "gu", "mp", "pr", "vi", "fm", "mh")) %>%
  filter(!geo_value %in%  c("as", "gu", "mp", "pr", "vi", "fm", "mh")) %>%
  mutate(time_value = as.Date(time_value), issue_date = as.Date(issue_date))  %>%
  mutate(weekly_per = 100 * weekly_in_ratio) %>%
  rename(backcast_lag = lag) %>%
  arrange(geo_value, time_value, issue_date)



labels_hosp = vroom("../../../versioned_feature/ground_truth.csv") %>%
  filter(time_value >= as.Date("2020-11-01") & time_value <= as.Date("2023-08-01")) %>%
  mutate(time_value = as.Date(time_value)) %>%
  select(-issue_date) %>%
  filter(geo_value != "vi") %>%
  inner_join(uspop, by = "geo_value") %>%
  mutate(GT = GT / pop * 10^5)



# Load one day hosp counts, potentially of different `issue_date`
# Then take rolling averages of different issue dates
# Recall that if averages are taken within a given issue date
# Discontinuities 
versioned_hosp = vroom("../../../versioned_feature/versioned_hhs_7dav.csv") %>%
  filter(time_value >= as.Date("2020-11-01") & time_value <= as.Date("2023-08-01")) %>%
  mutate(time_value = as.Date(time_value)) %>%
  filter(geo_value != "vi") %>%
  inner_join(uspop, by = "geo_value") %>%
  mutate(one_day = one_day / pop * 10^5)


dat = in_raw %>%
  inner_join(select(labels_hosp, geo_value, time_value, GT),
             by = c("geo_value", "time_value")) %>%
  inner_join(uspop, by = "geo_value") 

