"
Scenario 1: Recieving hospitalzations every 30 days 
"

source("assets_update/data_load.R")
source("assets_update/unconstrained_state_level_big_lag.R")
source("assets_update/unconstrained_national_level_big_lag.R")



gammas = signif(seq(0, 0.0625, length.out = 25), 3)
alphas = signif(seq(0, 1, length.out = 51))

cadence = 30
offset = 120
vl = 2



back_2 = c()
val_frame = c()
val_gamma = c()
max_lag = 19

state_model_coef = c()
national_model_coef = c()

# Level of miscover c. 1 - c gives the desired coverage level. e.g., if you 
# want 90% coverage, set miscover_lvl=0.1
miscover_lvl = 0.4 

# State level learning rates. To be initalized after first round of validation
state_lr_frame = c()
# Data frame for state level intervals
state_interval_frame = c()

dump_dates = c(as.Date("2021-04-01"), as.Date("2021-05-01"), as.Date("2021-06-01"), 
               as.Date("2021-07-01"), as.Date("2021-08-01"), as.Date("2021-09-01"),
               as.Date("2021-10-01"), as.Date("2021-11-01"), as.Date("2021-12-01")) 

dump_dates = dump_dates - 1

# Iterate through update dates 
# Iterate through update dates 
for (window_date in dump_dates) {
  
  max_date = as.numeric(as.Date("2022-07-31") - window_date) - 1
  print(max_date)
  
  if (max_date == 0) {
    break
  }


  for (i in seq(1, min(50, max_date))) {
  
    # Every day we recieve updated features 
    train_end = window_date - vl * cadence 
    # Prevent intersection with previous test
    test_start = window_date + 1 
    test_start = as.Date(test_start, "1970-01-01")
    
    # Don't see new labels, but see updated features
    version = window_date + i
    version = as.Date(version, "1970-01-01")
    
    # state level val frame 
    state_val_frame = state_produce_fv(gammas, train_end, version)
    
    # National level val frame 
    national_val_frame = national_produce_fv(gammas, train_end, version)
    
    if (nrow(state_val_frame) == 0) {
      
      break
      
    }
    
    # Select FV gamma and retrain
    # Two levels: state and national level
    # No more subsetting: `produce_fv` only produces 2 months of FV data
    state_val_gamma = state_val_frame %>%
      group_by(geo_value, gamma) %>%
      summarise(MAE = mean(abs(.resid))) %>%
      mutate(Min = min(MAE)) %>%
      filter(MAE == Min) %>%
      select(geo_value, gamma)

    # Set learning rates of quantile tracker 
    # Intialize state_lr_frame to be 0.1 * max absolute residuals 
    if (window_date == dump_dates[1]) {
      if (i == 1){
        state_lr_frame = state_val_frame %>%
          select(geo_value, time_value, .resid) %>%
          group_by(geo_value) %>%
          mutate(resid = abs(.resid)) %>%
          filter(resid == max(resid)) %>%
          mutate(lr = 0.1 * resid) %>%
          # Herustic of lr outlined in middle of page 6
          select(geo_value, lr)

        # Initalize state level scores to be 1 - alpha quantile of the residual
        # of the selected model over the burn-in set
        state_score_frame = state_val_frame %>%
          mutate(resid = abs(.resid)) %>%
          summarise(scores = quantile(resid, probs = 1 - miscover_lvl))
      }
    } 

    national_gamma = national_val_frame %>%
      group_by(gamma) %>%
      summarise(MAE = mean(abs(.resid))) %>%
      mutate(Min = min(MAE)) %>%
      filter(MAE == Min) %>%
      select(gamma)
    
    # Retrain state-level and geo-pooled model 
    ## Using all available data
    state_train = state_get_train_ar(window_date, version) %>%
      group_by(geo_value) %>%
      merge(state_val_gamma, by = "geo_value")
    
    national_train = national_get_train_ar(window_date, version) %>%
      ungroup() %>%
      mutate(ng = national_gamma$gamma)
    
    ## Retrain models 
    state_selected_models = state_train %>%
      group_by(geo_value) %>%
      do(model = lm(GT ~ in_6 + in_13 + in_20 + out_6 + out_13 + out_20, 
                    weights = exp(-gamma * backcast_lag) / max(exp(-gamma * backcast_lag)),
                    data = .))
    
    national_selected_models = lm(GT ~ in_6 + in_13 + in_20 + out_6 + out_13 + out_20, 
                    weights = exp(-ng * backcast_lag) / max(exp(-ng * backcast_lag)),
                    data = national_train)
    
    
    # After selection and retrain, store the models with `geo_value` and `issue_date`
    # It is important to store the RETRAINED coefficents
    state_selected_tmp = state_selected_models %>%
      group_by(geo_value) %>%
      group_modify(~ bind_rows(coef(.$model[[1]]))) %>%
      mutate(issue_date = as.Date(version, "1970-01-01"))
    
    state_model_coef = rbind(state_model_coef, state_selected_tmp)

    national_model_coef = rbind(national_model_coef, c(national_selected_models$coefficients, 
      as.Date(version, "1970-01-01")))
  
    # Find mixing weights 
    mixed_val_frame = alpha_fv(alphas, state_val_frame, national_val_frame, 
      state_val_gamma, national_gamma)
    opt_alpha = mixed_val_frame %>%
      group_by(geo_value, alpha) %>%
      summarise(MAE = mean(.resid)) %>%
      mutate(Min = min(MAE)) %>%
      filter(MAE == Min) %>%
      rename(optimal = alpha) %>%
      select(geo_value, optimal)
    
    
    
    state_test = state_get_test_backnow_raw(test_start, version)
    national_test = national_get_test_backnow_raw(test_start, version)
    
    # If not test points at all, next date in test time
    if (nrow(state_test) == 0) {
      next
    }
    
    # Another round of making sure out of sample
    stopifnot(max(state_val_frame$time_value) <= min(state_test$time_value))
    stopifnot(max(state_train$time_value) <= min(state_test$time_value))
    
    # Use inner_join to avoid problems in null data
    # So that you don't actually predict training samples...
    state_test = state_test %>%
      group_by(geo_value) %>%
      nest() %>%
      inner_join(state_selected_models) %>%
      verify(nrow(.) != 0)
    
    national_test = national_test %>%
      mutate(national_fit = predict(national_selected_models, newdata = .)) %>%
      verify(nrow(.) != 0)
    

    state_Tested = state_test %>%
      do(augment(.$model[[1]], newdata = .$data[[1]])) %>%
      rename(state_fit = .fitted) %>%
      select(geo_value, time_value, issue_date, state_fit, GT) %>%
      mutate(resid = abs(state_fit - GT))
    
    national_Tested = national_test %>%
      select(geo_value, time_value, issue_date, national_fit)
    
    # Book-keeping gamma for tracking effective sample size 
    state_gamma  = state_val_gamma %>%
      rename(state_optimal_gamma = gamma)
    
    print(range(state_Tested$time_value))

    Tested = state_Tested %>%
      inner_join(national_Tested, by = c("geo_value", "time_value", "issue_date")) %>%
      inner_join(opt_alpha, by = "geo_value") %>%
      # Book-keep gamma for effective sample size calculaiton
      inner_join(state_gamma, by = "geo_value") %>%
      mutate(national_gamma = national_gamma$gamma) %>%
      group_by(geo_value) %>%
      mutate(mixed_pred = optimal * state_fit + (1 - optimal) * national_fit) %>%
      mutate(staleness = as.numeric(time_value - window_date))
    
    # Construct intervals: f(X_t) - qt <= y <= f(X_t) + q_t
    state_intervals = state_Tested %>%
      inner_join(state_score_frame, by = "geo_value") %>%
      inner_join(state_lr_frame, by = "geo_value") %>%
      group_by(geo_value) %>%
      mutate(lower = pmax(state_fit - scores, 0),
            upper = pmax(state_fit  + scores, 0))

    state_interval_frame = rbind(state_interval_frame, state_intervals)
    back_2 = rbind(back_2, Tested)
    
  }
  
  # New data has been seen, update scores here 
  # Compute miscoverage during last month
  # Compute coverage only over nowcasts
  # Adapt the update step to be mimicking doing 30 update steps at once 
  last_month = as.Date(window_date) - 1
  last_month = floor_date(last_month, "month")

  miscover_freq = state_intervals %>%
    filter(time_value >= last_month) %>%
    filter(time_value == issue_date) %>%
    group_by(geo_value) %>%
    summarise(update = sum((GT < lower | GT > upper) - miscover_lvl)) 
  
  # Update learning rates
  state_lr_frame = state_interval_frame %>%
    group_by(geo_value) %>%
    # set lr to be max of a rolling past, see coomment after prop1
    filter(time_value >= window_date - vl) %>%
    filter(resid == max(resid)) %>%
    # Herustic of lr outlined in middle of page 6
    mutate(lr = 0.1 * resid) %>%
    select(geo_value, lr) 
  
  # Update scores
  state_score_frame = state_score_frame %>%
    inner_join(state_lr_frame, by = "geo_value") %>%
    inner_join(miscover_freq, by = "geo_value") %>%
    group_by(geo_value) %>%
    mutate(scores = scores + lr * update) %>%
    select(geo_value, scores)


}


write.csv(state_interval_frame, "../../predictions/scenario1_state_quantileTracker.csv",
  row.names = FALSE)
write.csv(back_2, "../../predictions/bl_versioned_hhs_mixed.csv", row.names = FALSE)


