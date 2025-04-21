
#' Almond Yield and Profit wrapper function
#'
#' @param year # year of the data (1988-2010)
#' @param precip # precipitation in mm
#' @param min_temp # minimum temperature in degrees Celsius
#'
#' @return # data frame with year, yield anomaly, yield, and profit
#' @export
#'
almond_yield_anomaly_and_profit <- function(year, precip, min_temp){
  
  #coefficients kept the same
  coefficients <- c(-0.015, -0.0046, -0.07, 0.0043, 0.28)
  
  #yield anomaly function
  yield_anomaly <- coefficients[1] * min_temp + coefficients[2] * min_temp^2 + coefficients[3] * precip + coefficients[4] * precip^2 + coefficients[5]
  
  baseline_yield <- 0.9 #0.9 tons per acre average (From Lobell et al 2006)
  price <- 1000 # $1000 per ton assumption
  profit_margin <- .5 #50% profit margin assumption
  yield <- (baseline_yield + yield_anomaly) #total yield
  
  #profit as a function of total yield, price per ton, profit margin
  profit <- (baseline_yield + yield_anomaly) * price * profit_margin 
  return(data.frame(year, yield_anomaly, yield, profit))
}
