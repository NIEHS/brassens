#' Calibration of citizen weather station with regard to a reference network
#' @param x sf (or inherited) object with citizen weather stations to calibrate
#' with columns site_id and geometry
#' @param ref sf (or inherited) object with reference weather stations
#' with columns site_id and geometry
#' @param max_dist the maximum distance in meters to consider
#' a station as a reference (default = 10000)
#' @return a list with the calibrated observations, the average bias,
#' the number of reference stations used to compute the bias, and the
#' start and end time of the observations.
#' @importFrom dplyr filter group_by summarise ungroup
#' @importFrom stats median
calib_cws <- function(x, ref, max_dist = 10000) {
  temp_err <- site_id <- geometry <- network <- temp <- dist_to_ref <- NULL
  hour <- NULL
  stopifnot(
    "max_dist must be between 0 and 20000" =
      max_dist > 0 & max_dist <= 20000
  )
  # Estimate measurement error and select observations within
  # x meters from reference stations
  n_ref <- length(unique(ref$site_id))
  n_days <- round(
    difftime(max(x$time, na.rm = TRUE),
      min(x$time, na.rm = TRUE),
      units = "days"
    )
  )
  if (n_days < 14) {
    warning(paste0("calibration on a short period (", n_days, " days)"))
  }
  if (n_ref < 3) {
    warning(paste0("calibration with only ", n_ref, " ref stations"))
  }
  x_err <- est_temp_error(x, ref)
  x_err <- x_err[which(x_err$dist_to_ref <= max_dist), ]
  n <- length(unique(x_err$site_id))
  ts <- min(x$time, na.rm = TRUE)
  te <- max(x$time, na.rm = TRUE)
  # Compute the average bias (per hour of the day)
  x_err$hour <- lubridate::hour(x_err$time)
  x_err <- x_err |>
    dplyr::filter(!is.na(temp_err)) |>
    dplyr::group_by(site_id, geometry, network, hour, dist_to_ref) |>
    dplyr::summarise(
      temp_err = median(temp_err, na.rm = TRUE),
      temp = median(temp, na.rm = TRUE),
      n_days = dplyr::n(),
    ) |>
    dplyr::ungroup() |>
    as.data.frame()
  median_err <- x_err |>
    dplyr::filter(!is.na(temp_err)) |>
    dplyr::group_by(hour) |>
    dplyr::summarise(
      bias = median(temp_err, na.rm = TRUE),
      med_dist_to_ref = median(dist_to_ref),
      n_pairs_x_days = sum(n_days, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    as.data.frame()

  # 4 - Remove this bias to all the observations
  y <- x
  y$hour <- lubridate::hour(y$time)
  y <- merge(y, median_err, by = "hour")
  y$temp_cal <- y$temp - y$bias

  # 5 - Return obs corrected and the average bias, the number of cws used to
  # compute the bias, ts and te
  cat("calib_cws() done\n")
  return(list(
    obs = y,
    bias = median_err,
    n_ref_cws_pairs = n,
    ts = ts,
    te = te
  ))
}
