## --------------------------------------------------------------------------------------------------------------
source("assets_all_past/data_load.R")
source("assets_all_past/uncon_state_big_lag.R")
source("assets_all_past/uncon_national_big.R")


## --------------------------------------------------------------------------------------------------------------
"
Ablation: Unweighted training on all past
Hack: just set gamma = 0, rest of pipeline stays the same. 
"

gammas = 0
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


dump_dates = c(as.Date("2021-04-01"), as.Date("2021-05-01"), as.Date("2021-06-01"), 
               as.Date("2021-07-01"), as.Date("2021-08-01"), as.Date("2021-09-01"),
               as.Date("2021-10-01"), as.Date("2021-11-01"), as.Date("2021-12-01"),
               as.Date("2022-01-01")) 

dump_dates = dump_dates - 1

for (window_date in dump_dates) {
  
  
  # The part where we change things
  # Here we will iterate through time points in prediction set + 30 days
  # This means for all `time_value` in the test set, we would have `backcast_lag` 
  # up to 30
  # On day T before end of month, we will produce nowcast \hat{Y}_{T}^{(T)} and a 
  # bunch of trailing nowcasts \hat{Y}_{< T}^{(T)}
  # If at end of month, get feature for whole month with newer version 
  # (`issue_date`) after end of month
  # `window_date`: last observation before test set 
  # that can be used to fit the model
  # So `window_date` contains 60 days of validation data
  
  max_date = as.numeric(as.Date("2022-07-31") - window_date) - 1
  print(max_date)
  
  
  if (max_date == 0) {
    
    break
    
  }
  
  # Iterating through test set 
  # Adhering to hypothetical that we are not looking at
  # predictions with backcast_lag = 30 for the entirety 
  # of test set
  for (i in seq(1, min(50, max_date))) {
    
    
    # In each day in test, we first calibrate
    # Then produce
    
    # Preparing for calibrating FV here.
    # You are not observing new labels of hosp
    # So you are training with updated versions of features
    
    # Subsetting so that we dont see val set 
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
    
    # Make sure test predictions are out of sample
    stopifnot(state_val_frame$time_value <= as.Date(test_start, "1970-01-01"))
    
    
    # Select FV gamma and retrain
    # Two levels: state and national level
    # No more subsetting: `produce_fv` only produces 2 months of FV data
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
    
    # Refit, this time using all the data, with `window_date` as cutoff
    # State level
    state_train = state_get_train_ar(window_date, version) %>%
      group_by(geo_value) %>%
      merge(state_val_gamma, by = "geo_value")
    
    national_train = national_get_train_ar(window_date, version) %>%
      ungroup() %>%
      mutate(ng = national_gamma$gamma)
    
    
    # We have changed our model to be autoregressive
    state_selected_models = state_train %>%
      group_by(geo_value) %>%
      do(model = lm(GT ~ in_5 + in_12 + in_19 + out_5 + out_12 + out_19, 
                    weights = exp(-gamma * backcast_lag) / max(exp(-gamma * backcast_lag)),
                    data = .))
    
    national_selected_models = lm(GT ~ in_5 + in_12 + in_19 + out_5 + out_12 + out_19, 
                    weights = exp(-ng * backcast_lag) / max(exp(-ng * backcast_lag)),
                    data = national_train)
    
    
    # After selection and retrain, store the models with `geo_value` and `issue_date`
    # It is important to store the RETRAINED coefficents
    state_selected_tmp = state_selected_models %>%
      group_by(geo_value) %>%
      group_modify(~ bind_rows(coef(.$model[[1]]))) %>%
      mutate(issue_date = as.Date(version, "1970-01-01"))
    
    state_model_coef = rbind(state_model_coef, state_selected_tmp)
    
    # national_selected_tmp = national_selected_models %>%
    #   summarise(~ bind_rows(coef(national_selected_models))) %>%
    #   mutate(issue_date = as.Date(version, "1970-01-01"))
    
    national_model_coef = rbind(national_model_coef, c(national_selected_models$coefficients, 
      as.Date(version, "1970-01-01")))
    
    
    
    
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
      mutate(staleness = as.numeric(time_value - window_date))
    
    
    back_2 = rbind(back_2, Tested)
    
  }
  
  
  
}



write.csv(back_2, "../../../predictions/ablation_allpast.csv", row.names = FALSE)


