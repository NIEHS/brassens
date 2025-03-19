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
calib_cws_old <- function(x, ref, max_dist = 10000) {
  temp_err <- ref_id <- site_id <- NULL
  geometry <- network <- temp <- dist_to_ref <- NULL
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
    dplyr::group_by(site_id, geometry, network, hour, ref_id, dist_to_ref) |>
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
      n_pairs_x_days = sum(n_days, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    as.data.frame()
  # Statistics on bias per ref station
  ref_stats <- x_err |>
    dplyr::group_by(ref_id, hour) |>
    dplyr::summarise(
      bias_avg = mean(temp_err, na.rm = TRUE),
      dist_to_ref_avg = mean(dist_to_ref, na.rm = TRUE),
      n_days = sum(n_days, na.rm = TRUE),
      n_cws = n(),
    ) |>
    dplyr::ungroup() |>
    as.data.frame()
  # Remove this bias to all the observations
  y <- x
  y$hour <- lubridate::hour(y$time)
  y <- merge(y, median_err, by = "hour")
  y$temp <- y$temp - y$bias

  # Return obs corrected and the average bias, the number of cws used to
  # compute the bias, ts and te
  cat("calib_cws() done\n")
  return(list(
    obs = y,
    ref_stats = ref_stats,
    bias = median_err,
    n_ref_cws_pairs = n,
    ts = ts,
    te = te
  ))
}

calib_cws <- function(ref, cws, max_dist = 5000) {
  stopifnot(
    "cws is not a sf" = inherits(cws, "sf"),
    "ref is not a sf" = inherits(ref, "sf")
  )
  stopifnot(
    "timezone for cws$time should be UTC" = lubridate::tz(cws$time) == "UTC",
    "timezone for ref$time should be UTC" = lubridate::tz(ref$time) == "UTC"
  )
  ts <- min(cws$time, na.rm = TRUE)
  te <- max(cws$time, na.rm = TRUE)
  ref <- terra::vect(ref) |>
    dplyr::rename("ref_id" = site_id) |>
    dplyr::rename("ref_temp" = temp)
  ref_loc <- dplyr::distinct(ref[, c("ref_id")]) 
  cws <- terra::vect(cws)
  cws_loc <- dplyr::distinct(cws[, c("site_id")])
  stopifnot("ref and cws must have same crs" = terra::same.crs(cws, ref))
  buf_ref <- terra::buffer(ref_loc, width = max_dist)
  # Select cws points within each buffer
  cws_in_buf <- list()
  for (i in seq_len(nrow(ref_loc))) {
    cws_in_buf[[i]] <- terra::intersect(cws, buf_ref[i, ])
  }
  cws_in_buf <- do.call(rbind, cws_in_buf)
  # merge with ref measurement
  cws_in_buf <- merge(
    cws_in_buf,
    ref[, c("time", "ref_id", "ref_temp")],
    by = c("time", "ref_id")
  )
  cws_in_buf$meas_err <- cws_in_buf$temp - cws_in_buf$ref_temp
  cws_in_buf$utc <- lubridate::hour(cws_in_buf$time)
  ref_stats <- cws_in_buf |>
    dplyr::group_by(ref_id, utc) |>
    dplyr::summarise(
      bias_avg = median(meas_err, na.rm = TRUE),
      n_cws = dplyr::n_distinct(site_id),
    ) |>
    dplyr::ungroup() |>
    as.data.frame()
  overall_meas_err <- cws_in_buf |>
    dplyr::group_by(utc) |>
    dplyr::summarise(
      bias_med = median(meas_err, na.rm = TRUE),
      bias_mean = mean(meas_err, na.rm = TRUE),
      sd = sd(meas_err, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    as.data.frame()
  # correct cws
  cws$utc <- lubridate::hour(cws$time)
  cws <- merge(cws, overall_meas_err, by = "utc")
  cws$temp_bef_cal <- cws$temp
  cws$temp <- cws$temp_bef_cal - cws$bias_med
  return(
    list(
      obs = cws,
      bias = overall_meas_err,
      ref_stats = ref_stats,
      cws_in_buf = cws_in_buf
    )
  )
}
