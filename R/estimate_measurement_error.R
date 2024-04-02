#' Add temperature from a reference to a hourly_temp object
#' @param x hourly_temp object
#' @param ref hourly_temp object with the closest reference temperature
#' @return hourly_temp object with the reference temperature in columns temp_r
add_temp_reference <- function(x, ref) {

}

#' Estimate measurement errors for a hourly_temp object
#'
estimate_temp_error <- function(x, ref) {
  x <- add_temp_reference(x, ref)

}
