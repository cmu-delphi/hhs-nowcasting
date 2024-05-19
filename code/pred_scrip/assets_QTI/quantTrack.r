source("assets_update/data_load.R")
source("assets_update/unconstrained_state_level_big_lag.R")
source("assets_update/unconstrained_national_level_big_lag.R")

library(Hmisc)
library(lubridate)
library(tidyverse)
library(vroom)

state_pred = vroom("../../predictions/bl_versioned_hhs_mixed.csv")


"
Implement quantile tracking and baselines, over a range of alpha (nominal miscoverage) lvl
"
quantileTrack_Baseline = function(alpha_vector) {

  quant_frame = c(); weighted_frame = c(); unweighted_frame = c()

  for (alpha in alpha_vector) {

    state_QT = c(); unwei_Quant = c(); wei_Quant = c()

    miscover_lvl = alpha; quant_lvl = 1 - miscover_lvl/2

    window_date = as.Date("2021-04-01")
    version = as.Date(window_date + 1)
    vl = 2; cadence = 30
    gammas = signif(seq(0, 0.0625, length.out = 25), 3)

    # Intialize scores to be 1 - alpha/2 quantiles 
    train_end = window_date - vl * cadence 
    # Prevent intersection with previous test
    test_start = window_date + 1 
    test_start = as.Date(test_start, "1970-01-01")

    # state level val frame 
    state_val_frame = state_produce_fv(gammas, train_end, version)
    state_val_gamma = state_val_frame %>%
      group_by(geo_value, gamma) %>%
      summarise(MAE = mean(abs(.resid))) %>%
      mutate(Min = min(MAE)) %>%
      filter(MAE == Min) %>%
      select(geo_value, gamma)

    state_lr_frame = state_val_frame %>%
      select(geo_value, time_value, .resid, .fitted) %>%
      group_by(geo_value) %>%
      rename(resid = .resid) %>%
      filter(resid == max(resid)) %>%
      mutate(score = resid / max(.fitted, 1)) %>%
      mutate(lr = 0.1 * score) %>%
      select(geo_value, lr)

    tmp_state_gamma = state_val_gamma %>%
        rename(opt_g = gamma)

    state_score_frame = state_val_frame %>%
      inner_join(tmp_state_gamma, by = "geo_value") %>%
      filter(gamma == opt_g) %>%
      select(-opt_g) %>%
      rename(resid = .resid) %>%
      # Dampening: Cap the minimum of dampening to be 1
      mutate(d_t = pmax(abs(.fitted), 1)) %>%
      mutate(lower_e_t = -resid/d_t) %>%
      mutate(upper_e_t = resid / d_t) %>%
      summarise(lower_score = pmax(0, quantile(lower_e_t, probs = quant_lvl)),
                upper_score = pmax(0, quantile(upper_e_t, probs = quant_lvl)))

    # Produce both weighted and unweighted quantiles 
    unwei_quantiles = state_score_frame %>%
      rename(lower_q = lower_score,
            upper_q = upper_score)

    # Do we want to do initalization more carefully? 
    weighted_quantiles = state_val_frame %>%
      inner_join(tmp_state_gamma, by = "geo_value") %>%
      filter(gamma == opt_g) %>%
      mutate(backcast_lag = as.numeric(max(time_value) - time_value)) %>%
      rename(resid = .resid) %>%
      mutate(weights = exp(-gamma * backcast_lag)) %>%
      mutate(d_t = pmax(abs(.fitted), 1)) %>%
      mutate(lower_e_t = -resid/d_t) %>%
      mutate(upper_e_t = resid / d_t) %>%
      summarise(
        lower_q = pmax(0, wtd.quantile(lower_e_t, 
          weights = weights, probs = quant_lvl, normwt = TRUE)),
        upper_q = pmax(0, wtd.quantile(upper_e_t, 
          weights = weights, probs = quant_lvl, normwt = TRUE)))


    dump_dates = c(as.Date("2021-04-01"), as.Date("2021-05-01"), as.Date("2021-06-01"), 
                  as.Date("2021-07-01"), as.Date("2021-08-01"), as.Date("2021-09-01"),
                  as.Date("2021-10-01"), as.Date("2021-11-01"), as.Date("2021-12-01")) - 1

    for (window_date in dump_dates) {

      max_date = as.numeric(as.Date("2022-01-31") - window_date) - 1

      for (i in seq(1, min(50, max_date))) {
          
          window_date = as.Date(window_date, "1970-01-01")
          version = window_date + i
          version = as.Date(version, "1970-01-01")


          tmp = state_pred %>%
            # Stay between current dump and next dump
            filter(issue_date == version) %>%
            filter(time_value > window_date & 
                    time_value < ceiling_date(window_date + 1, "month"))
          if (nrow(tmp) == 0) {
            next
          }
          

          # Intervals via quantile tracking
          track_tmp = tmp %>%
            select(geo_value, time_value, issue_date, state_fit, GT, 
                  resid) %>%
            inner_join(state_score_frame, by = "geo_value") %>%
            # Padding parameter, TODO: Change to additive padding (how?)
            mutate(d_t = pmax(state_fit, 1)) %>%
            # Construct asymmetric intervals
            # Upper score is used to construct the LOWER end point
            mutate(
              lower = pmax(state_fit - lower_score * d_t, 0),
              upper = pmax(state_fit + upper_score * d_t, 0)
            ) %>%
            mutate(alpha = miscover_lvl)

          # Intervals with unweighted sample quantiles 
          unwei_tmp = tmp %>%
            select(geo_value, time_value, issue_date, state_fit, GT) %>%
            inner_join(unwei_quantiles, by = "geo_value") %>%
            mutate(d_t = pmax(state_fit, 1)) %>%
            mutate(
              lower = pmax(state_fit - lower_q * d_t, 0),
              upper = pmax(state_fit + upper_q * d_t, 0)
            ) %>%
            mutate(alpha = miscover_lvl)

          # Intervals with weighted sample quantiles 
          weighted_tmp = tmp %>%
            select(geo_value, time_value, issue_date, state_fit, GT) %>%
            inner_join(weighted_quantiles, by = "geo_value") %>%
            mutate(d_t = pmax(state_fit, 1)) %>%
            mutate(
              lower = pmax(state_fit - lower_q * d_t, 0),
              upper = pmax(state_fit + upper_q * d_t, 0)
            ) %>%
            mutate(alpha = miscover_lvl)


          state_QT = rbind(state_QT, track_tmp) 
          unwei_Quant = rbind(unwei_Quant, unwei_tmp)
          wei_Quant = rbind(wei_Quant, weighted_tmp) 
      }
      
      # Between world, compute coverage and update scores 
      miscover_freq = state_QT %>%
        filter(time_value >= as.Date(window_date)) %>%
        filter(time_value == issue_date) %>%
        group_by(geo_value) %>%
        # Track lower and upper seperately 
        summarise(update_lower = sum((GT < lower) - miscover_lvl/2),
                  update_upper = sum((GT > upper) - miscover_lvl/2))

      state_lr = state_QT %>%
        group_by(geo_value) %>%
        filter(time_value >= as.Date(window_date)) %>%
        filter(time_value == issue_date) %>%
        mutate(upper_scores = (state_fit - GT) / d_t) %>%
        mutate(lower_scores = -(state_fit - GT) / d_t) %>%
        summarise(upper_lr = 0.03 * max(upper_scores),
                  lower_lr = 0.03 * max(lower_score)) 
        
      state_score_frame = state_score_frame %>%
        inner_join(state_lr, by = "geo_value") %>%
        inner_join(miscover_freq, by = "geo_value") %>%
        group_by(geo_value) %>%
        summarise(lower_score = lower_score + upper_lr * update_lower,
                  upper_score = upper_score + lower_lr * update_upper) %>%
        mutate(lower_score = pmax(0, lower_score),
              upper_score = pmax(0, upper_score))
      
      # Consider both weighted and unweighted empirical quantiles
      state_val_frame = state_produce_fv(gammas, train_end, version)
      state_val_gamma = state_val_frame %>%
        group_by(geo_value, gamma) %>%
        summarise(MAE = mean(abs(.resid))) %>%
        mutate(Min = min(MAE)) %>%
        filter(MAE == Min) %>%
        select(geo_value, gamma)

      weighted_quantiles = wei_Quant %>%
        filter(time_value == issue_date) %>%
        inner_join(state_val_gamma, by = "geo_value") %>%
        group_by(geo_value) %>%
        mutate(weights = exp(-gamma * as.numeric(max(time_value) - time_value))) %>%
        mutate(d_t = max(state_fit, 1)) %>%
        mutate(lower_score = (state_fit - GT) / d_t,
              upper_score = (GT - state_fit) / d_t) %>%
        summarise(lower_q = pmax(0, wtd.quantile(lower_score, 
                    weights = weights, probs = quant_lvl, normwt = TRUE)),
                  upper_q = pmax(0, wtd.quantile(upper_score, 
                    weights = weights, probs = quant_lvl, normwt = TRUE)))

      unwei_quantiles = unwei_Quant %>%
        filter(time_value == issue_date) %>%
        group_by(geo_value) %>%
        mutate(d_t = max(state_fit, 1)) %>%
        mutate(lower_score = (state_fit - GT) / d_t,
              upper_score = (GT - state_fit) / d_t) %>%
        summarise(lower_q = pmax(0, quantile(lower_score, probs = quant_lvl)),
                  upper_q = pmax(0, quantile(upper_score, probs = quant_lvl)))


    }
    quant_frame = rbind(quant_frame, state_QT) 
    unweighted_frame = rbind(unweighted_frame, unwei_Quant)
    weighted_frame = rbind(weighted_frame, wei_Quant)
  }
  return(list(as_tibble(quant_frame), as_tibble(unweighted_frame), as_tibble(weighted_frame)))
}