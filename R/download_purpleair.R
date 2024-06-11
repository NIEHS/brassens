# ` `@title` `Load PurpleAir data`


#' Find a list of sensors in a specific area
#'
#' @param nwlat North-west latitude
#' @param selat South-east latitude
#' @param nwlng North-west longitude
#' @param selng South-east longitude
#' @param api_key API key for PurpleAir
#' @param location_type Location type (0 = outside, 1 = inside)
#' @return A data frame with sensors information
#' @export
#' @import httr
#' @importFrom jsonlite fromJSON
find_sensors <- function(nwlat,
                         selat,
                         nwlng,
                         selng,
                         api_key,
                         location_type = 0) {
  fields <- "sensor_index,latitude, longitude, temperature, humidity"
  query_list <- list(
    nwlng = nwlng,
    nwlat = nwlat,
    selng = selng,
    selat = selat,
    max_age = 0,
    location_type = 0,
    fields = fields
  )
  url_base <- paste0("https://api.purpleair.com/v1/sensors/")
  # GET PurpleAir sensor history data
  r_temp <- httr::GET(
    url = url_base,
    query = query_list,
    config = add_headers("X-API-Key" = api_key)
  )
  # Structurized data in form of R vectors and lists
  r_parsed <- fromJSON(content(r_temp, as = "text"))
  # Data frame from JSON data
  sensors <- as.data.frame(r_parsed$data)
  colnames(sensors) <- r_parsed$fields
  return(sensors)
}

#' Load PurpleAir sensor history data
#'
#' @param start_ts Start timestamp
#' @param end_ts End timestamp
#' @param sensor_index Sensor index
#' @param api_key API key for PurpleAir
#' @param average Average time in minutes
#' @param fields Fields to be included in the data
#' @return A data frame with sensor history data
#' @export
#' @import httr
#' @importFrom jsonlite fromJSON
request_sensor_history <- function(start_ts,
                                   end_ts,
                                   sensor_index,
                                   api_key,
                                   average = "60",
                                   fields = "temperature, humidity") {
  query_list <- list(
    start_timestamp = as.character(as.integer(start_ts)),
    end_timestamp = as.character(as.integer(end_ts)),
    average = average,
    fields = fields
  )
  url_base <- paste0(
    "https://api.purpleair.com/v1/sensors/",
    sensor_index,
    "/history"
  )
  # GET PurpleAir sensor history data
  r_temp <- httr::GET(
    url = url_base,
    query = query_list,
    config = add_headers("X-API-Key" = api_key)
  )
  # Structurized data in form of R vectors and lists
  r_parsed <- fromJSON(content(r_temp, as = "text"))
  # Data frame from JSON data
  s_history <- as.data.frame(r_parsed$data)
  if (nrow(s_history) == 0) {
    return(NULL)
  } else {
    colnames(s_history) <- r_parsed$fields
    s_history$time_stamp <- as.POSIXct(s_history$time_stamp,
      origin = "1970-01-01",
      tz = "UTC"
    )
    return(s_history)
  }
}


request_sensors_history <- function(nwlat,
                                    selat,
                                    nwlng,
                                    selng,
                                    location_type = 0,
                                    start_ts,
                                    end_ts,
                                    api_key,
                                    average = "60",
                                    fields = "temperature, humidity") {
  sensors <- find_sensors(
    nwlat,
    selat,
    nwlng,
    selng,
    api_key,
    location_type
  )
  for (s in sensors$sensor_index) {
    s_history <- request_sensor_history(
      start_ts,
      end_ts,
      s,
      api_key,
      average,
      fields
    )
    if (!is.null(s_history)) {
      s_history$latitude <- as.numeric(sensors[
        sensors$sensor_index == s,
        "latitude"
      ])
      s_history$longitude <- as.numeric(sensors[
        sensors$sensor_index == s,
        "longitude"
      ])
      s_history$sensor_index <- s
      if (exists("sensors_history")) {
        sensors_history <- rbind(sensors_history, s_history)
      } else {
        sensors_history <- s_history
      }
    }
  }
  if (exists("sensors_history")) {
    return(sensors_history)
  } else {
    return(NULL)
  }
}

# to be tested
#' Download all PurpleAir stations data in area between two dates
#' @param ts start date
#' @param te end date
#' @param area a sf, sfc, SpatRaster or SpatVector object
#' @param api_key API key for PurpleAir
download_pa <- function(ts, te, area, api_key) {
  bounds <- area |>
    format_area() |>
    sf::st_bbox()
  pa <- request_sensors_history(
    bounds[2],
    bounds[4],
    bounds[1],
    bounds[3],
    0,
    ts,
    te,
    api_key
  )
  return(pa)
}

#' If a file is provided, open data from file. If not, call download_pa().
#' @param ts start date
#' @param te end date
#' @param area a sf, sfc, SpatRaster or SpatVector object
#' @param storage_file file path where PurpleAir data is stored
#' @param api_key API key for PurpleAir
#' @import utils
load_pa <- function(ts, te, area, storage_file = NULL, api_key = NULL) {
  if (is.null(storage_file)) {
    pa <- download_pa(ts, te, area, api_key)
  } else {
    extension <- substr(
      storage_file,
      nchar(storage_file) - 2,
      nchar(storage_file)
    )
    if (extension == "rds") {
      pa <- readRDS(storage_file)
    } else if (extension == "csv") {
      pa <- read.csv(storage_file)
    }
    pa$time_stamp <- as.POSIXct(pa$time_stamp,
      tz = "UTC",
      format = "%Y-%m-%d %H:%M:%S"
    )
    pa$latitude <- as.numeric(pa$latitude)
    pa$longitude <- as.numeric(pa$longitude)
    pa <- pa |>
      sf::st_as_sf(
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
      )
    area <- area |>
      format_area() |>
      sf::st_transform(crs = 4326)
    pa <- sf::st_filter(pa, area)
    pa <- pa[which(dplyr::between(pa$time_stamp, ts, te)), ]
  }
  # back to dataframe class
  pa$geometry <- NULL
  if (nrow(pa) == 0) {
    message("No PurpleAir found at those dates and area.")
  }
  return(data.table::data.table(pa))
}
