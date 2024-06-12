#' Summarize hourly temperature at each site.
#' This function might be used when stations record more than
#' one observation per hour.
#' @param x a data.frame, data.table, sf or sftime
#' @param time the column name for the time
#' @param temp the column name for the temperature
#' @param lat the column name for the latitude
#' @param lon the column name for the longitude
#' @return a data.frame of hourly average of temperature at each site
#' @author Eva Marques
#' @importFrom lubridate floor_date
#' @importFrom stats median
#' @importFrom dplyr group_by summarise ungroup
#' @importFrom data.table as.data.table
#' @export
summarize_hourly_temp <- function(x, time, temp, lat, lon) {
  stopifnot(
    "x is not a data.frame, data.table, sf or sftime" =
      inherits(x, c("sf", "sftime", "data.table", "data.frame")),
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
  hourly_avg$time <- lubridate::floor_date(hourly_avg$time, "hour")
  hourly_avg <- hourly_avg |>
    dplyr::group_by(lat, lon, time) |>
    dplyr::summarise(temp = median(temp, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    as.data.frame()
  # remove duplicates
  hourly_avg <- unique(hourly_avg)
  return(hourly_avg)
}

#' Format observations directly downloaded on PurpleAir API.
#' @param raw a data.frame, data.table, sf or sftime with the raw observations
#' and columns "time_stamp", "latitude", "longitude", "temperature"
#' @param raw_tz the initial timezone, see PurpleAir API documentation
#' @param raw_temp_unit the initial temperature unit
#' @param raw_crs the initial coordinate reference system
#' @return sftime from hourly_temp class
#' @importFrom lubridate with_tz floor_date
#' @author Eva Marques
#' @export
format_pa <- function(raw,
                      raw_tz = "UTC",
                      raw_temp_unit = "F",
                      raw_crs = 4326) {
  pa_cols <- c("time_stamp", "latitude", "longitude", "temperature")
  stopifnot(
    "raw is not a data.frame, data.table, sf or sftime" =
      inherits(raw, c("sf", "sftime", "data.table", "data.frame")),
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
    sftime::st_as_sftime(
      coords = c("lon", "lat"),
      time_column_name = "time",
      crs = raw_crs,
      remove = FALSE
    )
  cat("format_pa() done\n")
  return(x)
}

#' Format observations sent by IBM.
#' @param raw a data.frame, data.table, sf or sftime with the raw observations
#' and columns "obsTimeUtc", "lat", "lon", "tempAvg"
#' @param raw_tz the initial timezone, see PurpleAir API documentation
#' @param raw_temp_unit the initial temperature unit
#' @param raw_crs the initial coordinate reference system
#' @return sftime from hourly_temp class
#' @importFrom lubridate floor_date
#' @author Eva Marques
#' @export
format_wu <- function(raw,
                      raw_tz = "UTC",
                      raw_temp_unit = "F",
                      raw_crs = 4326) {
  wu_cols <- c("obsTimeUtc", "lat", "lon", "tempAvg")
  stopifnot(
    "raw is not a data.frame, data.table, sf or sftime" =
      inherits(raw, c("sf", "sftime", "data.table", "data.frame")),
    "Some columns missing or mispelled" =
      all(wu_cols %in% colnames(raw))
  )
  x <- raw
  if (raw_temp_unit != "C") {
    x$tempAvg_c <- convert_temp(x$tempAvg, from = raw_temp_unit, to = "C")
  }
  x$obsTimeUtc <- as.POSIXct(x$obsTimeUtc, tz = "UTC") |>
    lubridate::floor_date(unit = "hours")
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
    sftime::st_as_sftime(
      coords = c("lon", "lat"),
      time_column_name = "time",
      crs = raw_crs,
      remove = FALSE
    )
  cat("format_wu() done\n")
  return(x)
}

#' Format observations from raw data downloaded on NOAA website
#' See function load_ghcnh_station.
#' @param raw a data.frame, data.table, sf or sftime with the raw observations
#' and columns "Year", "Month", "Day", "Hour", "temperature", "Latitude",
#' "Longitude", "temperature_Source_Code"
#' @return sftime from hourly_temp class
#' @importFrom dplyr rename
#' @author Eva Marques
#' @export
format_ghcnh <- function(raw) {
  ghcnh_cols <- c(
    "Year",
    "Month",
    "Day",
    "Hour",
    "temperature",
    "Latitude",
    "Longitude",
    "temperature_Source_Code"
  )
  stopifnot(
    "raw is not a data.frame, data.table, sf or sftime" =
      inherits(raw, c("sf", "sftime", "data.table", "data.frame")),
    "Some columns missing or mispelled" =
      all(ghcnh_cols %in% colnames(raw))
  )
  x <- raw
  # note: as.POSIXct return a double when called through apply.
  # this is why it is called after the apply
  x$time <- apply(
    x[, c("Year", "Month", "Day", "Hour")],
    1,
    function(x) {
      paste0(
        x[1],
        "-",
        x[2],
        "-",
        x[3],
        " ",
        x[4],
        ":00:00"
      )
    }
  ) |>
    as.POSIXct(format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
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
    sftime::st_as_sftime(
      coords = c("lon", "lat"),
      time_column_name = "time",
      crs = 4326,
      remove = FALSE
    )
  cat("format_ghcnh() done\n")
  return(x)
}
