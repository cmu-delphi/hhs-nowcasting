"
State level quantile tracking on top of all nowcasts
"
library(lubridate)
library(tidyverse)
library(vroom)

# Read nowcasts and intialized residuals and learning rate
state_pred = vroom("../../predictions/bl_versioned_hhs_mixed.csv")
#TODO: read initalized scores and learning rate 
state_lr = 
state_score_frame = 

miscover_lvl = 0.4



dump_dates = c(as.Date("2021-04-01"), as.Date("2021-05-01"), as.Date("2021-06-01"), 
               as.Date("2021-07-01"), as.Date("2021-08-01"), as.Date("2021-09-01"),
               as.Date("2021-10-01"), as.Date("2021-11-01"), as.Date("2021-12-01")) 


state_QT = c()

for (window_date in dump_dates) {

  max_date = as.numeric(as.Date("2022-01-31") - window_date) - 1

  for (i in seq(1, min(50, max_date))) {

      version = window_date + i
      version = as.Date(version, "1970-01-01")


      tmp = state_pred %>%
        # Stay between current dump and next dump
        filter(issue_date == version) %>%
        filter(time_value < ceiling_date(window_date, "month"))

      interval_tmp = tmp %>%
        inner_join(state_score_frame, by = "geo_value") %>%
        # Padding parameter, TODO: Change to additive padding (how?)
        mutate(d_t = pmax(state_fit, 1)) %>%
        mutate(
          lower = pmax(state_fit - score * d_t, 0),
          upper = pmax(state_fit + score * d_t, 0)
        )

      state_QT = rbind(state_QT, interval_tmp)

  }
  
  # Between world, compute coverage and update scores 
  miscover_freq = state_QT %>%
    filter(time_value >= as.Date(window_date)) %>%
    filter(time_value == issue_date) %>%
    group_by(geo_value) %>%
    summarise(update = sum((GT < lower | GT > upper) - miscover_lvl))

  state_lr = state_QT %>%
    group_by(geo_value) %>%
    filter(time_value >= as.Date(window_date)) %>%
    filter(time_value == issue_date) %>%
    mutate(scores = abs(state_fit - GT) / d_t) %>%
    summarise(lr = 0.1 * mean(scores)) 
    
  state_score_frame = state_score_frame %>%
    inner_join(state_lr, by = "geo_value") %>%
    inner_join(miscover_freq, by = "geo_value") %>%
    summarise(score = score + lr * update)

}