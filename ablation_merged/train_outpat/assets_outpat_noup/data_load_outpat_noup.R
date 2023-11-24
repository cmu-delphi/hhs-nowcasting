"
Scripts for loading data, ablation study: only uses outpatient features
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



uspop = covidcast::state_census %>%
  select(ABBR, POPESTIMATE2019) %>%
  rename(geo_value = ABBR) %>%
  rename(pop = POPESTIMATE2019) %>%
  mutate(geo_value = tolower(geo_value)) %>%
  slice(., 2:(n() - 4))


out_raw = vroom("../../versioned_feature/rebuild_outpatient_raw_avg.csv") %>%
  filter(time_value >= as.Date("2020-11-01") & time_value <= as.Date("2023-07-31")) %>%
  filter(!geo_value %in%  c("as", "gu", "mp", "pr", "vi")) %>%
  mutate(time_value = as.Date(time_value), issue_date = as.Date(issue_date)) %>%
  rename(backcast_lag = lag) 

labels_hosp = vroom("../../versioned_feature/ground_truth.csv") %>%
  filter(time_value >= as.Date("2020-11-01") & time_value <= as.Date("2023-07-31")) %>%
  select(-issue_date) %>%
  mutate(time_value = as.Date(time_value)) %>%
  filter(geo_value != "vi") %>%
  inner_join(uspop, by = "geo_value") %>%
  mutate(GT = GT / pop * 10^5)

# Load one day hosp counts, potentially of different `issue_date`
# Then take rolling averages of different issue dates
# Recall that if averages are taken within a given issue date
# Discontinuities 
versioned_hosp = vroom("../../versioned_feature/versioned_hhs_7dav.csv") %>%
  filter(time_value >= as.Date("2020-11-01") & time_value <= as.Date("2023-07-31")) %>%
  mutate(time_value = as.Date(time_value)) %>%
  filter(geo_value != "vi") %>%
  inner_join(uspop, by = "geo_value") %>%
  mutate(one_day = one_day / pop * 10^5)


dat = out_raw %>%
  select(geo_value, time_value, issue_date, weekly_out_ratio) %>%
  inner_join(select(labels_hosp, geo_value, time_value, GT),
             by = c("geo_value", "time_value")) %>%
  inner_join(uspop, by = "geo_value") 
