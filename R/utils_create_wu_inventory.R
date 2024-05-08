#' @title Create Weather Underground inventory
#' @description Create an inventory of Weather Underground data files with
#' content metadata (station ID, latitude, longitude, start and end timestamps)
#' @param fpath the path to the Weather Underground data files
#' @param invpath the path to the output inventory file
#' @return a data.frame with the inventory
create_wu_inventory <- function(fpath, invpath) {
  flist <- list.files(fpath, full.names = TRUE, recursive = TRUE)
  inventory <- NULL
  for (i in 1:length(flist)) {
    f <- flist[i]
    df <- read.csv(file = f, nrows = 1)
    dates <- data.table::fread(file = f,
                               colClasses = c(rep("NULL", 2),
                                              NA,
                                              rep("NULL", 19)))
    dates <- dates$obsTimeUtc |>
      as.POSIXct(tz = "UTC") |>
      lubridate::floor_date(unit = "hours")
    inventory[[i]] <- data.frame(fname = f,
                                 stationID = df$stationID,
                                 lat = df$lat,
                                 lon = df$lon,
                                 ts_utc = min(dates, na.rm = TRUE),
                                 te_utc = max(dates, na.rm = TRUE))
  }
  inventory <- do.call("rbind", inventory)
  write.csv(inventory, invpath, row.names = FALSE)
  inventory <- inventory |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  return(inventory)
}

