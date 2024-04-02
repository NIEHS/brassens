manage_na <- function(data, na_thresh = 0.1) {
  # remove lines where temp is na
  output <- data[which(!(is.na(data$temp))), ]
  # timeserie length
  n_tot <- as.numeric(difftime(max(output$time),
    min(output$time),
    unit = "hour"
  )) + 1
  n_thresh <- n_tot * (1 - na_thresh)
  # remove stations with more than na_tresh % of na
  n <- output |>
    group_by("site_id") |>
    dplyr::summarize(n = dplyr::n()) |>
    data.frame()
  keep_id <- unique(n[which(n$n >= n_thresh), c("site_id")])
  output <- output[which(output$site_id %in% keep_id), ]
  return(output)
}
