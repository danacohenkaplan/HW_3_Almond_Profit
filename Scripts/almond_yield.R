#Almond Yield Function - Assignment #2 - Dana Cohen-Kaplan
 
#From Lobell et al 2006: Y = "0.015T{n,2} + 0.0046T^2{n,2} + -0.07P{1} + 0.0043P^2{1} + 0.28
 
##Y = yield in tons/acre, subscript indicates variable and month of climate variable, negative values indicate months before harvest 


#' Models almond yield based on climate variables

#' @param year The year of the yield being modeled 
#' @param climate_df The dataframe containing the climate data broken out into "day" "month" "year" "wy" "tmax_c" "tmin_c" "precip"
#' @param coefficients A vector of coefficients to be used in the model from Lobell et al 2006
#' @param yield Yield anomaly measured in tons/acre 

#Write function, specifying coefficients and monthly temp and precip subsets
almond_yield <- function(year, climate_df) {
  # Define the coefficients for the model
  coefficients <- c(-0.015, -0.0046, -0.07, 0.0043, 0.28)
  
  
  # Prep climate data
  # feb data for Tn2 (min temp for Feb)
  feb_climate <- climate_df[climate_df$month == 2, ]
  Tn2 <- mean(feb_climate$tmin_c, na.rm = TRUE)
  
  
  # jan data for P1 (precip for Jan)
  jan_climate <- climate_df[climate_df$month == 1, ]
  P1 <- sum(jan_climate$precip, na.rm = TRUE)
  
  # Calculate the yield using the model
  yield <- coefficients[1] * Tn2 + coefficients[2] * Tn2^2 + coefficients[3] * P1 + coefficients[4] * P1^2 + coefficients[5]
  
  return(yield)
}
  
