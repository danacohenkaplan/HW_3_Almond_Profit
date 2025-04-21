#' computes profit from almond yield
#' @param baseline_yield average yield (tons/acre) 
#' @param price ($/ton)
#' @param yield (tons/acre) - corresponds to a year
#' @param profit_margin (percent of sales as profit)
#' @return baseline_profit ($/acre) data frame with estimate of profit

profit_from_yield <- function(baseline_yield = 0.9, price = 1000, profit_margin = .5, yield_anomaly) {
  #calculate power
  baseline_profit <-  (baseline_yield + yield_anomaly) * price * profit_margin 
  
  
  return(baseline_profit)
  
}
  
