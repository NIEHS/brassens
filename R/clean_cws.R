#' Remove NA and stations with too many NA
#' @param data formatted sftime (with columns: site_id, temp, time)
#' @param na_thresh threshold of NA to remove a station (0 <= na_tresh <= 1)
#' @return cleaned data.frame
#' @importFrom dplyr group_by n summarize
manage_na <- function(data, na_thresh = 0.1) {
  stopifnot(
    "threshold must be between 0 and 1" =
      na_thresh >= 0 & na_thresh <= 1,
    "data must be a sftime" = inherits(data, "sftime"),
    "site_id, temp, lat, lon, time missing or mispelled" =
      all(c("site_id", "temp", "time") %in% colnames(data))
  )
  # remove lines where temp is na
  output <- data[which(!(is.na(data$temp))), ]
  # timeserie length
  n_tot <- as.numeric(
    difftime(
      max(output$time),
      min(output$time),
      units = "hour"
    )
  ) + 1
  n_thresh <- n_tot * (1 - na_thresh)
  # remove stations with more than na_tresh % of na
  n <- output |>
    dplyr::group_by("site_id") |>
    dplyr::summarize(n = dplyr::n()) |>
    data.frame()
  keep_id <- unique(n[which(n$n >= n_thresh), c("site_id")])
  output <- output[which(output$site_id %in% keep_id), ]
  return(output)
}

#' Clean Weather Underground data with CrowdQC+. Keep only observations
#' passing all tests (o3 level)
#' @param x sftime with columns: site_id, temp, lat, lon, time
#' @return cleaned data.frame
#' @import sftime
clean_cws <- function(x) {
  stopifnot(
    "site_id, temp, lat, lon, time missing or mispelled" =
      all(c("site_id", "temp", "lat", "lon", "time") %in% colnames(x)),
    "x must be a sftime" = inherits(x, "sftime")
  )
  x_qcp <- x |>
    dplyr::rename("p_id" = "site_id") |>
    dplyr::rename("ta" = "temp") |>
    as.data.table()
  x_qcp$p_id <- as.character(x_qcp$p_id)
  x_qcp <- CrowdQCplus::cqcp_padding(x_qcp)
  x_qcp <- CrowdQCplus::cqcp_add_dem_height(x_qcp)
  data <- CrowdQCplus::cqcp_padding(x_qcp)
  ok <- CrowdQCplus::cqcp_check_input(x_qcp)
  if (ok) {
    data_qc <- CrowdQCplus::cqcp_qcCWS(
      data,
      m5_radius = 10000,
      m5_n_buddies = 5,
    )
  }
  col_rm <- c(
    "m1",
    "m2",
    "m3",
    "m4",
    "m5",
    "z",
    "isolated",
    "ta_int",
    "o1",
    "o2",
    "o3"
  )
  data_qc <- data_qc |>
    as.data.frame()
  x_qc <- data_qc[which(data_qc$o3), !(colnames(data_qc) %in% col_rm)] |>
    dplyr::rename("site_id" = "p_id") |>
    dplyr::rename("temp" = "ta") |>
    sftime::st_as_sftime(
      coords = c("lon", "lat"),
      time_column_name = "time",
      crs = 4326,
      remove = FALSE
    )
  cat("clean_cws() done\n")
  return(x_qc)
}

#' Format an area to be used in clean_cws_large (cut in squares of res*res m2)
#' @param area polygon
#' @param epsg_m crs in meters (default: epsg:32119)
#' @param res resolution of the squares in meters (default: 100km)
#' @return sf of polygons
#' @importFrom terra vect project mask as.polygons
cut_area <- function(area, epsg_m = "epsg:32119", res = 100000) {
  # project area with a crs in meters
  area_m <- area |>
    format_area() |>
    terra::vect() |>
    terra::project(y = epsg_m) # linear unit of nc_shp in meters
  # cut area in squares of res*res m2
  r <- terra::rast(area_m, res = res)
  terra::values(r) <- 1:terra::ncell(r)
  v <- terra::mask(r, area_m) |>
    terra::as.polygons() |>
    terra::project("epsg:4326") |>
    sf::st_as_sf()
  return(v)
}


#' Clean Weather Underground data with CrowdQC+ on an area >100km*100km
#' @param x sftime of formatted weather station data
#' (with columns: site_id, temp, lat, lon, time)
#' @param area polygon
#' @param epsg_m crs in meters (default: epsg:32119)
#' @param res resolution of the squares in meters (default: 100km)
#' @return cleaned data.frame
clean_cws_large <- function(x, area, epsg_m = "epsg:32119", res = 100000) {
  # cut area in squares of res*res m2
  v <- cut_area(area, epsg_m, res)
  # apply crowcQC+ on each square
  x_clean <- list()
  for (sq in seq_len(nrow(v))) {
    # select stations in square
    x_in_sq <- sf::st_filter(x, v[sq, ])
    cat("Square ", sq, " has ", length(unique(x_in_sq$site_id)), " stations\n")
    if (nrow(x_in_sq) == 0) {
      next
    }
    x_clean[[sq]] <- x_in_sq |>
      clean_cws()
  }
  x_cleaned <- do.call("rbind", x_clean)
  return(x_cleaned)
}
