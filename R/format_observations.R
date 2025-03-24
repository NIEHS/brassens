#' Summarize hourly temperature at each site.
#' This function might be used when stations record more than
#' one observation per hour.
#' @param x a data.frame, data.table, sf
#' @param time the column name for the time
#' @param temp the column name for the temperature
#' @param lat the column name for the latitude
#' @param lon the column name for the longitude
#' @return a data.frame of hourly average of temperature at each site
#' @author Eva Marques
#' @importFrom lubridate floor_date
#' @importFrom stats median
#' @importFrom dplyr group_by summarise ungroup rename
#' @importFrom data.table as.data.table
#' @export
summarize_hourly_temp <- function(x, time, temp, lat, lon) {
  stopifnot(
    "x is not a data.frame, data.table, sf" =
      inherits(x, c("sf", "data.table", "data.frame")),
    "time, temp, lat, lon are not all characters" =
      is.character(time) &
      is.character(temp) &
      is.character(lat) &
      is.character(lon),
    "time, temp, lat, lon columns missing or mispelled" =
      all(c(time, temp, lat, lon) %in% colnames(x))
  )
  hourly_avg <- x |>
    data.table::as.data.table() |>
    dplyr::rename("lat" = lat) |>
    dplyr::rename("lon" = lon) |>
    dplyr::rename("time" = time) |>
    dplyr::rename("temp" = temp)
  # timestamp corresponds to measurements in previous 60min
  hourly_avg$time <- lubridate::floor_date(hourly_avg$time, "hour")
  hourly_avg <- hourly_avg |>
    dplyr::group_by(lat, lon, time) |>
    dplyr::summarise(temp = median(temp, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    as.data.frame()
  # remove duplicates
  hourly_avg <- unique(hourly_avg)
  hourly_avg
}

remove_daily_cws <- function(x) {
  site_id <- NULL
  ts <- min(x$time, na.rm = TRUE)
  te <- max(x$time, na.rm = TRUE)
  n_days <- ceiling(as.numeric(difftime(te, ts, units = "days")))
  stats <- x |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(count = dplyr::n())
  cws_to_remove <- stats[which(stats$count <= n_days), ]$site_id
  x[which(!(x$site_id %in% cws_to_remove)), ]
}

#' Format observations directly downloaded on PurpleAir API.
#' @param raw a data.frame, data.table, sf with the raw observations
#' and columns "time_stamp", "latitude", "longitude", "temperature"
#' @param raw_tz the initial timezone, see PurpleAir API documentation
#' @param raw_temp_unit the initial temperature unit
#' @param raw_crs the initial coordinate reference system
#' @return sf from hourly_temp class
#' @importFrom lubridate with_tz floor_date
#' @author Eva Marques
#' @export
format_pa <- function(
    raw,
    raw_tz = "UTC",
    raw_temp_unit = "F",
    raw_crs = 4326) {
  pa_cols <- c("time_stamp", "latitude", "longitude", "temperature")
  stopifnot(
    "raw is not a data.frame, data.table, sf" =
      inherits(raw, c("sf", "data.table", "data.frame")),
    "Some columns missing or mispelled" =
      all(pa_cols %in% colnames(raw))
  )
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
    network = "PA"
  ) |>
    sf::st_as_sf(
      coords = c("lon", "lat"),
      crs = raw_crs,
      remove = FALSE
    ) |>
    remove_daily_cws()
  cat("format_pa() done\n")
  x
}

#' Format WeatherUnderground hourly data.
#' @param raw a data.frame, data.table, sf with the raw observations
#' and columns "obsTimeUtc", "lat", "lon", "tempAvg"
#' @param raw_tz the initial timezone, see PurpleAir API documentation
#' @param raw_temp_unit the initial temperature unit
#' @param raw_crs the initial coordinate reference system
#' @return sf from hourly_temp class
#' @importFrom lubridate floor_date
#' @author Eva Marques
#' @export
format_wu <- function(
    raw,
    raw_tz = "UTC",
    raw_temp_unit = "F",
    raw_crs = 4326) {
  wu_cols <- c("obsTimeUtc", "lat", "lon", "tempAvg")
  stopifnot(
    "raw is not a data.frame, data.table, sf" =
      inherits(raw, c("sf", "data.table", "data.frame")),
    "Some columns missing or mispelled" =
      all(wu_cols %in% colnames(raw))
  )
  x <- raw
  if (raw_temp_unit != "C") {
    x$tempAvg_c <- x$tempAvg |>
      as.numeric() |>
      convert_temp(from = raw_temp_unit, to = "C")
  }
  x$obsTimeUtc <- as.POSIXct(x$obsTimeUtc, tz = "UTC") |>
    lubridate::floor_date(unit = "hours") + lubridate::hours(1)
  x <- summarize_hourly_temp(
    x,
    "obsTimeUtc",
    "tempAvg_c",
    "lat",
    "lon"
  ) |>
    hourly_temp(
      temp = "temp",
      lat = "lat",
      lon = "lon",
      time = "time",
      network = "WU"
    ) |>
    sf::st_as_sf(
      coords = c("lon", "lat"),
      crs = raw_crs,
      remove = FALSE
    ) |>
    remove_daily_cws()
  cat("format_wu() done\n")
  x
}

#' Format observations from raw data downloaded on NOAA website
#' See function load_ghcnh_station.
#' @param raw a data.frame, data.table, sf with the raw observations
#' and columns "Year", "Month", "Day", "Hour", "temperature", "Latitude",
#' "Longitude", "temperature_Source_Code"
#' @return sf from hourly_temp class
#' @importFrom dplyr rename
#' @author Eva Marques
#' @export
format_ghcnh <- function(raw) {
  ghcnh_cols <- c(
    "DATE",
    "temperature",
    "Latitude",
    "Longitude",
    "temperature_Source_Code"
  )
  stopifnot(
    "raw is not a data.frame, data.table, sf" =
      inherits(raw, c("sf", "data.table", "data.frame")),
    "Some columns missing or mispelled" =
      all(ghcnh_cols %in% colnames(raw))
  )
  x <- raw
  x$time <- as.POSIXct(x$DATE, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  x <- x |>
    summarize_hourly_temp(
      "time",
      "temperature",
      "Latitude",
      "Longitude"
    ) |>
    hourly_temp(
      temp = "temp",
      lat = "lat",
      lon = "lon",
      time = "time",
      network = "GHCNh"
    )
  network <- raw[, c("Longitude", "Latitude", "temperature_Source_Code")] |>
    dplyr::rename("lon" = "Longitude") |>
    dplyr::rename("lat" = "Latitude") |>
    dplyr::rename("network" = "temperature_Source_Code") |>
    unique()
  x$network <- NULL
  x <- merge(x, network, by = c("lon", "lat"))
  # only keep observations from ASOS/AWOS and US CRN networks
  x <- x[which(x$network %in% c(343, 345)), ]
  x$network <- factor(x$network,
    levels = c(343, 345),
    labels = c("NCEI/ASOS/AWOS", "NCEI/US CRN")
  )
  x <- x |>
    sf::st_as_sf(
      coords = c("lon", "lat"),
      crs = 4326,
      remove = FALSE
    ) |>
    remove_daily_cws()
  cat("format_ghcnh() done\n")
  x
}
