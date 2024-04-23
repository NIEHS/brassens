#' Calibration of citizen weather station with regard to a reference network
#' @param x a data.frame with the observations to calibrate
#' @param ref a data.frame with the reference observations
#' @param max_dist the maximum distance in meters to consider
#' a station as a reference (default = 10000)
#' @return a list with the calibrated observations, the average bias,
#' the number of reference stations used to compute the bias, and the
#' start and end time of the observations. Improvement idea: time constraint,
#' bias calculation per month / season / on a sliding temporal window
calib_cws <- function(x, ref, max_dist = 10000) {
  # Estimate measurement error and select observations within
  # x meters from reference stations
  x_err <- calc_temp_error(x, ref)
  x_err <- x_err[which(x_err$dist_to_ref <= max_dist), ]
  n <- length(unique(x_err$site_id))
  ts <- min(x_err$time, na.rm = TRUE)
  te <- max(x_err$time, na.rm = TRUE)

  # Compute the average bias (param option: per hour of the day)
  x_err$hour <- lubridate::hour(x_err$time)
  x_err <- x_err |>
    dplyr::group_by(site_id, geometry, network, hour, dist_to_ref) |>
    dplyr::summarise(
      temp_err = median(temp_err, na.rm = TRUE),
      temp = median(temp, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    as.data.frame()

  median_err <- x_err |>
    dplyr::group_by(hour) |>
    dplyr::summarise(bias = median(temp_err, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    as.data.frame()

  # 4 - Remove this bias to all the observations
  y <- x
  y$hour <- lubridate::hour(y$time)
  y <- merge(y, median_err, by = "hour")
  y$temp_cal <- y$temp - y$bias

  # 5 - Return obs corrected and the average bias, the number of cws used to
  # compute the bias, ts and te
  return(list(obs = y, bias = median_err, n = n, ts = ts, te = te))
}
