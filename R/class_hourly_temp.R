#' S4 constructor for weather station hourly observations
setClass("hourly_temp",
  contains = c("data.frame")
)

setValidity("hourly_temp", function(object) {
  stopifnot(
    "object is not a data.frame" =
      is(object, "data.frame"),
    "temp must be numeric" =
      is.numeric(object$temp),
    "site_id must be a factor" =
      is.factor(object$site_id),
    "lat must be numeric" =
      is.numeric(object$lat),
    "lon must be numeric" =
      is.numeric(object$lon),
    "time must be a POSIXct" =
      lubridate::is.POSIXct(object$time),
    "time must be in UTC" =
      attr(object$time, "tzone") == "UTC",
    "network must be a character" =
      is.character(object$network)
  )
  obs_id <- object[, c("lon", "lat", "time")]
  obs_id$time <- lubridate::floor_date(obs_id$time, "hour")
  stopifnot(
    "duplicates found (more than 1 observation per hour?)" =
      !any(duplicated(obs_id))
  )
  return(TRUE)
})


#' Build a hourly_temp from a data.frame, data.table, sf or sftime
#' @param x a data.frame, data.table, sf or sftime
#' @param temp the column name for the temperature in degree celcius
#' @param lat the column name for the latitude
#' @param lon the column name for the longitude
#' @param time the column name for the time in UTC
#' @param network the name of the network
#' @return a hourly_temp object
#' @importFrom methods new
#' @author Eva Marques
hourly_temp <- function(x,
                        temp = "temp",
                        lat = "lat",
                        lon = "lon",
                        time = "time",
                        network) {
  stopifnot(
    "x is not a data.frame, data.table, sf or sftime." =
      class(x)[1] %in% c("data.frame", "data.table", "sf", "sftime"),
    "time, temp, lat, lon are not all characters." =
      is.character(time) &
      is.character(temp) &
      is.character(lat) &
      is.character(lon),
    "temp, lat, lon, time columns missing or mispelled." =
      c(temp, lat, lon, time) %in% colnames(x)
  )
  x <- as.data.frame(x)
  y <- x |>
    dplyr::rename("temp" = temp) |>
    dplyr::rename("lat" = lat) |>
    dplyr::rename("lon" = lon) |>
    dplyr::rename("time" = time)
  y$network <- network
  y <- generate_site_id(y)
  y <- y[, c("site_id", "temp", "lat", "lon", "time", "network")] |>
    methods::new(Class = "hourly_temp")
  return(y)
}
