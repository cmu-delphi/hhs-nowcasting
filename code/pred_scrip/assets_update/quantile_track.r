"
Script containing code for implementing quantile tracking, 
outlined in section 2.1 of the paper 'Conformal PID Control for Time Series Prediction',
from Angelopolous et al., 2023
"


"
Quantile tracking: Invert the absolute residual to construct a confidence interval.
C_t = {y: |f(x_t) - y| <= q_t} iff f(x_t) - q_t <= y <= f(x_t) + q_t

Inputs:
  predictions: data frame: predictions of a single geo_value. 
  scores: A number q_t: the estimated score at t-th iteration of a single geo_value.
"
quantile_tracking = function(predictions, scores) {

  lower_b = pmax(predictions - scores, 0)
  upper_b = pmax(predictions + scores, 0)
  return(tibble(lower=lower_b, upper=upper_b))

}



"
Update the score of quantile tracking, based on the following update:
q_t = q_{t - 1} + eta (err_t - lvl)
    = q_{t - 1} + eta (1 - lvl) if y_t is not in the interval
      q_{t - 1} - alpha otherwise 

Inputs:
  last_interval: data frame of predictions and interval since the last 
    data dump, of a given geo_value. 
  historical_res: data frame of residuals for all time points. 
  lvl: 1 - lvl gives the desired level of coverage.   
" 
score_update = function(last_interval, historical_res, lvl, 
  window_size=90) {

  miscovered = mean(frame_interval$lower > frame_interval$y | 
    frame_interval$upper < frame_interval$y)
  score = unique(last_interval$score)

  stopifnot(length(score) == 1)

  historical_res = historical_res %>% tail(window_size)
  lr = 0.1 * max(historical_res)
  updated_score = score - lr * (miscovered - lvl)

  return(updated_score)

}
