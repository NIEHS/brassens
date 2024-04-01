#' Format observations directly downloaded on PurpleAir API.
#' @param raw a data.frame with the raw observations
#' @param raw_tz the initial timezone, see PurpleAir API documentation
#' @param raw_temp_unit the initial temperature unit
#' @param raw_crs the initial coordinate reference system
#' @return sftime from hourly_temp class
#' @author Eva Marques
format_pa <- function(raw,
                      raw_tz = "EST",
                      raw_temp_unit = "F",
                      raw_crs = 4326) {
  x <- raw
  x$time_stamp <- as.POSIXct(x$time_stamp, tz = raw_tz)
  x$time_utc <- lubridate::with_tz(x$time_stamp, tzone = "UTC")
  x$latitude <- as.numeric(x$latitude)
  x$longitude <- as.numeric(x$longitude)
  if (raw_temp_unit != "C") {
    x$temperature <- convert_temp(as.numeric(x$temperature), raw_temp_unit, "C")
  }
  x <- hourly_temp(x,
                   temp = "temperature",
                   lat = "latitude",
                   lon = "longitude",
                   time = "time_utc")
  x <- x |>
    sftime::st_as_sftime(coords = c("lon", "lat"),
                       time_column_name = "time",
                       crs = raw_crs,
                       remove = FALSE) 
  return(x)
}

format_wu <- function(raw) {
  
}



