calib_cws <- function(ref, cws, max_dist = 5000) {
  utc <- meas_err <- site_id <- ref_id <- temp <- NULL
  stopifnot(
    "cws is not a sf" = inherits(cws, "sf"),
    "ref is not a sf" = inherits(ref, "sf")
  )
  stopifnot(
    "timezone for cws$time should be UTC" = lubridate::tz(cws$time) == "UTC",
    "timezone for ref$time should be UTC" = lubridate::tz(ref$time) == "UTC"
  )
  ref <- terra::vect(ref) |>
    terra::project("epsg:4326") |>
    dplyr::rename("ref_id" = site_id) |>
    dplyr::rename("ref_temp" = temp)
  cws <- terra::vect(cws) |>
    terra::project("epsg:4326")
  stopifnot("ref and cws must have same crs" = terra::same.crs(cws, ref))
  ref_loc <- dplyr::distinct(ref[, c("ref_id")])
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
  list(
    obs = cws,
    bias = overall_meas_err,
    ref_stats = ref_stats,
    cws_in_buf = cws_in_buf
  )
}
