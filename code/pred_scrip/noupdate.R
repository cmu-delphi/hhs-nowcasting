"
Scenario 2: No further hospitalizations recieved after Dec 1, 2021 
"

source("assets_noupdate/data_load.R")
source("assets_noupdate/unconstrained_state_level_big_lag.R")
source("assets_noupdate/unconstrained_national_level_bl.R")



omi_start = as.Date("2021-11-30")
end_date = as.Date("2023-08-01")
gammas = signif(seq(0, 0.0625, length.out = 25), 3)
alphas = signif(seq(0, 1, length.out = 51))

cadence = 30
offset = 120
vl = 2

state_model_coef = c()
national_model_coef = c()

back_2 = c()
val_frame = c()
val_gamma = c()


# Still need to iterate through all dates, with the exception of no retraining after
# 30 days of beginning of start of omicron
# NOTE: Still need to produce backcasts

for (d in seq(omi_start + 1, end_date, by = 1)) {
  
  # If less than 30 days after start, still retrain

  if (d <= omi_start + 30) {
    
    # Train end is always the same in scenario 2
    train_end = omi_start - vl * cadence
    test_start = omi_start + 1
    
    version = as.Date(d, "1970-01-01")
    
    # state level val frame 
    state_val_frame = state_produce_fv(gammas, train_end, version)
    
    # National level val frame 
    national_val_frame = national_produce_fv(gammas, train_end, version)
    
    if (nrow(state_val_frame) == 0) {
      
      break
      
    }
    
    # Make sure test predictions are out of sample
    stopifnot(state_val_frame$time_value <= as.Date(test_start, "1970-01-01"))
    
    
    # Select FV gamma and retrain, both state-level and geo-pooled
    state_val_gamma = state_val_frame %>%
      group_by(geo_value, gamma) %>%
      summarise(MAE = mean(abs(.resid))) %>%
      mutate(Min = min(MAE)) %>%
      filter(MAE == Min) %>%
      select(geo_value, gamma)
    
    
    national_gamma = national_val_frame %>%
      group_by(gamma) %>%
      summarise(MAE = mean(abs(.resid))) %>%
      mutate(Min = min(MAE)) %>%
      filter(MAE == Min) %>%
      select(gamma)
    
    # Refit, this time using all the data, with `omi_start` as cutoff
    # Becuase no future labels are made available to us
    ## State level data
    state_train = state_get_train_ar(omi_start, version) %>%
      group_by(geo_value) %>%
      merge(state_val_gamma, by = "geo_value")
    
    ## National level data
    national_train = national_get_train_ar(omi_start, version) %>%
      ungroup() %>%
      mutate(ng = national_gamma$gamma)
    
    ## Refit, state-level and geo-pooled
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
    
    
    
    # After selection and retrain, store the models with `geo_value` and `issue_date`
    # It is important to store the RETRAINED coefficents
    state_selected_tmp = state_selected_models %>%
      group_by(geo_value) %>%
      group_modify(~ bind_rows(coef(.$model[[1]]))) %>%
      mutate(issue_date = as.Date(version, "1970-01-01"))
    
    
    
    # Find alphas now
    mixed_val_frame = alpha_fv(alphas, state_val_frame, national_val_frame, state_val_gamma, national_gamma)
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
      select(geo_value, time_value, issue_date, state_fit, GT)
    
    national_Tested = national_test %>%
      select(geo_value, time_value, issue_date, national_fit)
    
    Tested = state_Tested %>%
      inner_join(national_Tested, by = c("geo_value", "time_value", "issue_date")) %>%
      inner_join(opt_alpha, by = "geo_value") %>%
      group_by(geo_value) %>%
      mutate(mixed_pred = optimal * state_fit + (1 - optimal) * national_fit) %>%
      mutate(staleness = as.numeric(time_value - omi_start))
    
    
    back_2 = rbind(back_2, Tested)
    
  }
  
  else {
    
    # After 30 days, all signales considerd finalized. 
    # Freeze models and just predict. 
    
    version = as.Date(d, "1970-01-01")
    
    print(as.Date(d, "1970-01-01"))
    

    state_test = state_get_test_oneshot_impute(version)
    national_test = national_get_test_oneshot_impute(version)

    # Exception handling: no data on nowcast date
    if (is.null(state_test)) {
      
      next
      
    }
          
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
      select(geo_value, time_value, issue_date, state_fit, GT)
    
    national_Tested = national_test %>%
      select(geo_value, time_value, issue_date, national_fit)
    
    Tested = state_Tested %>%
      inner_join(national_Tested, by = c("geo_value", "time_value", "issue_date")) %>%
      inner_join(opt_alpha, by = "geo_value") %>%
      group_by(geo_value) %>%
      mutate(mixed_pred = optimal * state_fit + (1 - optimal) * national_fit) %>%
      mutate(staleness = as.numeric(time_value - omi_start))
    
    
    back_2 = rbind(back_2, Tested)
    
  }
  
}


write.csv(back_2, "../../predictions/oneshot_omicron.csv", row.names = FALSE)


