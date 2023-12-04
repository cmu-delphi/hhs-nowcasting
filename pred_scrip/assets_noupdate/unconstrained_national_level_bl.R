


national_get_train_ar = function(train_end, version) {
  
  train = train_dat %>%
    filter(issue_date == as.Date(version, "1970-01-01")) %>%
    filter(time_value <= as.Date(train_end, "1970-01-01")) %>%
    group_by(geo_value, time_value) %>%
    select(geo_value, time_value, issue_date, weekly_in_ratio,
           weekly_out_ratio, GT) %>%
    # Create auto regressive feat
    ungroup() %>%
    mutate(in_6 = lag(weekly_in_ratio, n = 6)) %>%
    mutate(in_13 = lag(weekly_in_ratio, n = 13)) %>%
    mutate(in_20 = lag(weekly_in_ratio, n = 20)) %>%
    mutate(out_6 = lag(weekly_out_ratio, n = 6)) %>%
    mutate(out_13 = lag(weekly_out_ratio, n = 13)) %>%
    mutate(out_20 = lag(weekly_out_ratio, n = 20)) %>%
    # Get rid of original feature 
    select(-weekly_in_ratio, -weekly_out_ratio) %>%
    # Get rid of entries with na 
    na.omit() %>%
    ungroup() %>%
    mutate(backcast_lag = as.numeric(issue_date - time_value))
  

  
  
  return(train)
  
}  



national_get_fv_val_ar = function(train_end, version, max_lag = 20, vl = 60) {
  
  train_end = as.Date(train_end, "1970-01-01") - max_lag 
  
  stopifnot(version >= train_end + vl)
  
  
  # We keep the lag column for ease of manipulation in 
  # two stage FV later in `produce_fv`.
  # We need to look earlier than `train_end` since we are lagging at most by 7
  test = train_dat %>% 
          filter(issue_date == as.Date(version, "1970-01-01")) %>%
          filter(time_value > as.Date(train_end, "1970-01-01")  &
                   time_value <= as.Date(train_end, "1970-01-01") + max_lag + vl) %>%
          select(geo_value, time_value, issue_date, weekly_in_ratio,
                   weekly_out_ratio, GT) %>%
            # Create auto regressive feat
            ungroup() %>%
            group_by(geo_value) %>%
            mutate(in_6 = lag(weekly_in_ratio, n = 6)) %>%
            mutate(in_13 = lag(weekly_in_ratio, n = 13)) %>%
            mutate(in_20 = lag(weekly_in_ratio, n = 20)) %>%
            mutate(out_6 = lag(weekly_out_ratio, n = 6)) %>%
            mutate(out_13 = lag(weekly_out_ratio, n = 13)) %>%
            mutate(out_20 = lag(weekly_out_ratio, n = 20)) %>%
            # discard original feature 
            select(-weekly_in_ratio, -weekly_out_ratio) %>%
            # Get rid of entries with na 
            na.omit() %>%
            ungroup()
  
  return(test)  
  
}



national_produce_fv = function(gammas, train_end, version, max_lag = 20) {
  
  val_frame = c()
  
  train = national_get_train_ar(train_end, version)
  
  # `get_fv_val` gets 60 fv obs all at once
  # `get_fv_val` looks 60 days ahead from `train_end`
  # Handle at end of data
  toVal = national_get_fv_val_ar(train_end, version, max_lag)
  
  stopifnot(max(train$time_value) <= min(toVal$time_value))
  
  if (nrow(toVal) == 0) {
    
    return(data.frame())
    
  }
  
  # Subset `toVal` into two seperate months
  # after first month of FV, merge first part into training 
  toVal_first = toVal %>% 
    filter(time_value > as.Date(train_end, "1970-01-01") &
             time_value <= as.Date(train_end, "1970-01-01") + 30) 
  
  toVal_second = toVal %>%
    filter(time_value > as.Date(train_end, "1970-01-01") + 30)
  
  # First part, validate on first month of validation observation `toVal_first`
  
  for (g in gammas) {
    
    
    train = train %>%
      ungroup()
    
    
    fitted_models = 
     lm(GT ~ in_6 + in_13 + in_20 + out_6 + out_13 + out_20, 
                    weights = exp(-g * backcast_lag) / max(exp(-g * backcast_lag)),
                    data = train)



    Valed = toVal_first %>%
      mutate(.fitted = predict(fitted_models, newdata = .)) %>%    
      mutate(.resid = abs(GT - .fitted)) %>%
      select(geo_value, time_value, issue_date, .fitted, GT, .resid) %>%
      mutate(gamma = g) %>%
      # Last obs for 1st part of is train_end
      # Compute staleness for FV observation in the 1st part
      # Staleness defined as test point time_value - time_value of last training obs
      mutate(staleness = as.numeric(time_value - max(train$time_value)))
    
    val_frame = rbind(val_frame, Valed)
    
    
    # Brute book-keepingrange*()
    train = national_get_train_ar(train_end, version) 
    toVal = national_get_fv_val_ar(train_end, version, max_lag)
    
    toVal_first = toVal %>% 
      ungroup() %>%
      filter(time_value > as.Date(train_end, "1970-01-01") &
               time_value <= as.Date(train_end, "1970-01-01") + 30)
    
    
  }
  
  
  # Second part 
  
  # New training set: Merge first part of FV into new training
  # Need to re-compute `backcast_lag`
  # `backcast_lag` = the difference between `time_value` of last training observation
  # and `time_value` of other observations
  train = train %>% select(-backcast_lag)
  train = rbind(train, toVal_first) %>%
    mutate(backcast_lag = as.numeric(max(time_value) - time_value))
  
  
  for (g in gammas) {
    
    train = train %>%
      ungroup()
    
    
    fitted_models = 
     lm(GT ~ in_6 + in_13 + in_20 + out_6 + out_13 + out_20, 
                    weights = exp(-g * backcast_lag) / max(exp(-g * backcast_lag)),
                    data = train)


    Valed = toVal_second %>%
      mutate(.fitted = predict(fitted_models, newdata = .)) %>%
      mutate(.resid = abs(GT - .fitted)) %>%
      select(geo_value, time_value, issue_date, .fitted, GT, .resid) %>%
      mutate(gamma = g) %>%
      # Last obs for 1st part of is train_end
      # Compute staleness for FV observation in the 1st part
      # Staleness defined as test point time_value - time_value of last training obs
      mutate(staleness = as.numeric(time_value - max(train$time_value)))


    val_frame = rbind(val_frame, Valed)
    
    
    # Brute book-keeping
    train = national_get_train_ar(train_end, version) %>%
      select(-backcast_lag)
    
    toVal = national_get_fv_val_ar(train_end, version, max_lag)
    
    toVal_first = toVal %>% 
      filter(time_value > as.Date(train_end, "1970-01-01") &
               time_value <= as.Date(train_end, "1970-01-01") + 30)
    toVal_second = toVal %>%
      filter(time_value > as.Date(train_end, "1970-01-01") + 30)
    
    # Need to update train here!!!!!
    train = rbind(train, toVal_first) %>%
      mutate(backcast_lag = as.numeric(max(time_value) - time_value)) 
    
    
  }  
  
  # Make sure staleness is never more than 30 
  stopifnot(max(val_frame$staleness) <= 30)
  
  return(val_frame)
  
}




"
Function for getting autoregressive test feature. No imputation. 
"

national_get_test_backnow_raw = function(test_start, date, max_lag = 20) {
  
  # Roll back in time to include the first test point
  test_start = test_start - max_lag
  
  toRe = c()
  
  # End of Month, adjusting for lag
  eom = ceiling_date(test_start + max_lag, "month") - 1
  eom = as.Date(eom, "1970-01-01")
  
  # Prepare for producing backcast with backcast lag from 1 to 30 
  # If version is larger than 30 +7 after test start, then we are 
  # Over end of month, so end of test must always be end of month 
  #FIXME: Data dump modified to be at beginning of everymonth.
  # As a result, the time needed to wrap back is not 29 days.
  if (date < eom) {
      
    version = date
    
  } else {
    
    version = date
    date = eom
    
  }
  
  
  
  # Iterate through each date in the test set
  # Note again there is only one `issue_date` for the test set we return 
  i = 0
  for (d in seq(test_start + max_lag, date, by = 1)) {
    
    i = i + 1
    

    # d = min(d, as.Date("2021-12-01"))
    
    in_tibble = dat %>%
                  filter(issue_date == version) %>%
                  filter(time_value == d - 20 | time_value == d - 13 | 
                           time_value == d - 6) %>%
                  select(geo_value, time_value, issue_date, weekly_in_ratio) %>%
                  pivot_wider(
                    names_from = time_value, values_from = weekly_in_ratio) %>%
                  mutate(time_value = as.Date(d, "1970-01-01")) %>%
                  mutate(issue_date = as.Date(version, "1970-01-01")) %>% 
                  rename_at(vars(3:5), ~c("in_20", "in_13", "in_6")) %>%
                  select(geo_value, time_value, issue_date, in_20, in_13, in_6)

    
        
    
    out_tibble = dat %>%
                  filter(issue_date == version) %>%
                  filter(time_value == d - 20 | time_value == d - 13 |
                           time_value == d- 6) %>%
                  select(geo_value, time_value, issue_date, weekly_out_ratio) %>%
                  pivot_wider(
                    names_from = time_value, values_from = weekly_out_ratio) %>%
                  mutate(time_value = as.Date(d, "1970-01-01")) %>%
                  mutate(issue_date = as.Date(version, "1970-01-01")) %>% 
                  rename_at(vars(3:5), ~c("out_20", "out_13", "out_6")) %>%
                  select(geo_value, time_value, issue_date, out_20, out_13, out_6)
    
    f_tibble = inner_join(in_tibble, out_tibble, by = c("geo_value", "time_value", "issue_date")) %>%
                  inner_join(all_hosp, by = c("geo_value", "time_value"))

    toRe = rbind(toRe, f_tibble)
    
  
    # At end of all the data 
    # Will get duplicates even as we progress
    # if (d == as.Date("2021-12-01")) {
    #   
    #   break
    #   
    # }
    
    
  }
  
  toRe = toRe %>% ungroup()

  return(toRe)
  
  
}



"
Function for getting autoregressive test feature with oneshot scenario
After all updates have ceased
The function takes in a reference date we are standing on
Then produces test feature for every day since the beginning of month 
Until the given reference date


Variables: 
  'date': The reference date we are standing on
"

national_get_test_oneshot = function(date, max_lag = 20) {
  
  # Floor to first day of the test month
  # Since we are predicting from start of month to `date`
  first_day = floor_date(date, "month") - 30
  # Further back to produce test time feat
  test_start = first_day - max_lag
  
  toRe = c()
  
  # Version specifies the end date of test sets
  version = date
  
  # Iterate through each date in the test set
  # Note again there is only one `issue_date` for the test set we return 
  i = 0
  for (d in seq(test_start + max_lag, date, by = 1)) {
    
    i = i + 1
    
    
    # d = min(d, as.Date("2021-12-01"))
    
    in_tibble = dat %>%
      filter(issue_date == version) %>%
      filter(time_value == d - 20 | time_value == d - 13 | 
               time_value == d - 6) %>%
      select(geo_value, time_value, issue_date, weekly_in_ratio) %>%
      pivot_wider(
        names_from = time_value, values_from = weekly_in_ratio) %>%
      mutate(time_value = as.Date(d, "1970-01-01")) %>%
      mutate(issue_date = as.Date(version, "1970-01-01")) %>% 
      rename_at(vars(3:5), ~c("in_20", "in_13", "in_6")) %>%
      select(geo_value, time_value, issue_date, in_20, in_13, in_6)
    
    
    
    
    out_tibble = dat %>%
      filter(issue_date == version) %>%
      filter(time_value == d - 20 | time_value == d - 13 |
               time_value == d- 6) %>%
      select(geo_value, time_value, issue_date, weekly_out_ratio) %>%
      pivot_wider(
        names_from = time_value, values_from = weekly_out_ratio) %>%
      mutate(time_value = as.Date(d, "1970-01-01")) %>%
      mutate(issue_date = as.Date(version, "1970-01-01")) %>% 
      rename_at(vars(3:5), ~c("out_20", "out_13", "out_6")) %>%
      select(geo_value, time_value, issue_date, out_20, out_13, out_6)
    
    f_tibble = inner_join(in_tibble, out_tibble, by = c("geo_value", "time_value", "issue_date")) %>%
      inner_join(all_hosp, by = c("geo_value", "time_value"))
    
    toRe = rbind(toRe, f_tibble)
    
    
    
    
  }
  
  toRe = toRe %>% ungroup()
  
  return(toRe)
  
  
}



"
Test time imputation by backsearch over slack number of days. 
"
national_get_test_oneshot_impute = function(date, max_lag = 20, slack = 3) {
  
  
  
  # Floor to first day of the test month
  # Since we are predicting from start of month to `date`
  first_day = floor_date(date, "month") - 30
  # Further back to produce test time feat
  test_start = first_day - max_lag
  
  toRe = c()
  
  # Version specifies the end date of test sets
  version = date
  
  # Iterate through each date in the test set
  # Note again there is only one `issue_date` for the test set we return 
  i = 0
  for (d in seq(test_start + max_lag, date, by = 1)) {
    
    i = i + 1
    

    # d = min(d, as.Date("2021-12-01"))
    
    # Need to handle situation where an entire `time_value` is missing 
    
    in_6 = dat %>%
      filter(issue_date == version) %>%
      # search no more than slack days back
      filter(time_value >= d - 6 - slack & time_value <= d - 6) %>%
      group_by(geo_value, issue_date) %>%
      filter(time_value == max(time_value)) %>%
      mutate(time_value = as.Date(d - 6, "1970-01-01"))
    
  

    in_13 = dat %>%
      filter(issue_date == version) %>%
      # search no more than slack days back
      filter(time_value >= d - 13 - slack & time_value <= d - 13) %>%
      group_by(geo_value, issue_date) %>%
      filter(time_value == max(time_value)) %>%
      mutate(time_value = as.Date(d - 13, "1970-01-01"))
    
    in_20 = dat %>%
      filter(issue_date == version) %>%
      # search no more than slack days back
      filter(time_value >= d - 20 - slack & time_value <= d - 20) %>%
      group_by(geo_value, issue_date) %>%
      filter(time_value == max(time_value)) %>%
      mutate(time_value = as.Date(d - 20, "1970-01-01"))
    
    if (nrow(in_6) == 0 | nrow(in_13) == 0 | nrow(in_20) == 0) {

      break

    }
    

    in_tibble = rbind(in_6, in_13, in_20) %>%
      select(geo_value, time_value, issue_date, weekly_in_ratio) %>%
      group_by(geo_value, issue_date) %>%
      arrange(time_value, by_group = TRUE)
    
    
    
    in_tibble = in_tibble %>%
      pivot_wider(
        names_from = time_value, values_from = weekly_in_ratio) %>%
      mutate(time_value = as.Date(d, "1970-01-01")) %>%
      mutate(issue_date = as.Date(version, "1970-01-01")) %>% 
      rename_at(vars(3:5), ~c("in_20", "in_13", "in_6")) %>%
      select(geo_value, time_value, issue_date, in_20, in_13, in_6)
    
    
    out_tibble = rbind(in_6, in_13, in_20) %>%
      select(geo_value, time_value, issue_date, weekly_out_ratio) %>%
      group_by(geo_value, issue_date) %>%
      arrange(time_value, by_group = TRUE)
    
    
    
    out_tibble = out_tibble %>%
      select(geo_value, time_value, issue_date, weekly_out_ratio) %>%
      pivot_wider(
        names_from = time_value, values_from = weekly_out_ratio) %>%
      mutate(time_value = as.Date(d, "1970-01-01")) %>%
      mutate(issue_date = as.Date(version, "1970-01-01")) %>% 
      rename_at(vars(3:5), ~c("out_20", "out_13", "out_6")) %>%
      select(geo_value, time_value, issue_date, out_20, out_13, out_6)
    
    f_tibble = inner_join(in_tibble, out_tibble, by = c("geo_value", "time_value", "issue_date")) %>%
      inner_join(all_hosp, by = c("geo_value", "time_value"))
    
    toRe = rbind(toRe, f_tibble)
    
    
    
    
  }
  
  if (is.null(toRe)) {

    return(NULL)

  } else{

  toRe = toRe %>% ungroup()
  return(toRe)
  

  }



}




"
Function for choosing mixing weight alpha
"

alpha_fv = function(alphas, state_frame, national_frame, state_gamma, national_gamma) {

  # Extract national gamma value from df
  national_opt_g = national_gamma$gamma

  state_gamma = state_gamma %>%
    group_by(geo_value) %>%
    rename(state_opt_g = gamma)

  state_optimal = state_frame %>%
    inner_join(state_gamma, by = "geo_value") %>%
    filter(gamma == state_opt_g) %>%
    rename(state_fit = .fitted) %>%
    select(geo_value, time_value, issue_date, state_opt_g, state_fit, GT)

  national_optimal = national_frame %>%
    filter(gamma == national_opt_g) %>%
    rename(national_fit = .fitted) %>%
    rename(national_opt_g = gamma) %>%
    select(geo_value, time_value, issue_date, national_opt_g, national_fit)


  val_frame = c()


  for (a in alphas) {

    merged_tmp = state_optimal %>%
      left_join(national_optimal,
        by = c("geo_value", "time_value", "issue_date")) %>%
        mutate(alpha = a) %>%
        mutate(mixed = a * state_fit + (1 - a) * national_fit) %>%
        mutate(.resid = abs(GT - mixed))

    val_frame = rbind(val_frame, merged_tmp)

  }

  return(val_frame)

}