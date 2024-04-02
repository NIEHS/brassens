#' Format observations directly downloaded on PurpleAir API.
#' @param raw a data.frame with the raw observations
#' @param raw_tz the initial timezone, see PurpleAir API documentation
#' @param raw_temp_unit the initial temperature unit
#' @param raw_crs the initial coordinate reference system
#' @return sftime from hourly_temp class
#' @author Eva Marques
format_pa <- function(raw,
                      raw_tz = "UTC",
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
                   time = "time_utc",
                   network = "PA") |>
    sftime::st_as_sftime(coords = c("lon", "lat"),
                         time_column_name = "time",
                         crs = raw_crs,
                         remove = FALSE)
  return(x)
}

#' Format observations sent by IBM.
#' @param raw a data.frame with the raw observations
#' @param raw_tz the initial timezone, see PurpleAir API documentation
#' @param raw_temp_unit the initial temperature unit
#' @param raw_crs the initial coordinate reference system
#' @return sftime from hourly_temp class
#' @author Eva Marques
format_wu <- function(raw,
                      raw_tz = "UTC",
                      raw_temp_unit = "F",
                      raw_crs = 4326) {
  x <- raw
  if (raw_temp_unit != "C") {
    x$tempAvg_c <- convert_temp(x$tempAvg, from = raw_temp_unit, to = "C")
    x$tempLow_c <- convert_temp(x$tempLow, from = raw_temp_unit, to = "C")
  }
  x$obsTimeUtc <- as.POSIXct(x$obsTimeUtc, tz = "UTC") |>
    lubridate::floor_date(unit = "hours")
  x <- hourly_temp(x,
                   temp = "tempAvg_c",
                   lat = "lat",
                   lon = "lon",
                   time = "obsTimeUtc",
                   network = "WU") |>
    sftime::st_as_sftime(coords = c("lon", "lat"),
                         time_column_name = "time",
                         crs = raw_crs,
                         remove = FALSE)
  return(x)
}

#' Summarize hourly temperature at each site.
#' @param x a data.frame, data.table, sf or sftime
#' @param time the column name for the time
#' @param temp the column name for the temperature
#' @param lat the column name for the latitude
#' @param lon the column name for the longitude
#' @return a data.frame of hourly average of temperature at each site
#' @author Eva Marques
summarize_hourly_temp <- function(x, time, temp, lat, lon) {
  hourly_avg <- x |>
    dplyr::rename("lat" = lat) |>
    dplyr::rename("lon" = lon) |>
    dplyr::rename("time" = time) |>
    dplyr::rename("temp" = temp)
  hourly_avg$time <- lubridate::floor_date(hourly_avg$time, "hour")
  hourly_avg <- hourly_avg |>
    dplyr::group_by(lat, lon, time) |>
    dplyr::summarise(temp = mean(temp, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    as.data.frame()
  return(hourly_avg)
}
