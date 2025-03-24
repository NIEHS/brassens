#' @title Create Weather Underground inventory
#' @description Create an inventory of Weather Underground data files with
#' content metadata (station ID, latitude, longitude, start and end timestamps)
#' @param fpath the path to the Weather Underground data files
#' @param invpath the path to the output inventory file
#' @return a data.frame with the inventory
#' @import utils
create_wu_inventory <- function(fpath, invpath) {
  # nolint start
  obsTimeUtc <- NULL
  # nolint end
  flist <- list.files(fpath, full.names = TRUE, recursive = TRUE)
  inventory <- NULL
  for (i in seq_along(flist)) {
    f <- flist[i]
    ncol <- ncol(read.csv(file = f, nrows = 1))
    # info is a datatable with key elements (stationID, obsTimeUtc, lon, lat)
    info <- data.table::fread(
      file = f,
      colClasses = c(
        NA,
        rep("NULL", 1),
        NA,
        rep("NULL", 2),
        NA,
        NA,
        rep("NULL", ncol - 7)
      )
    )
    # remove unavailable rows
    info <- info[as.character(obsTimeUtc) != "unavailable", ]
    info <- info[as.character(obsTimeUtc) != "api_failure", ]
    if (nrow(info != 0)) {
      # if only unavailable rows, do not keep the file
      info$obsTimeUtc <- info$obsTimeUtc |>
        as.POSIXct(tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ") |>
        lubridate::floor_date(unit = "hours")
      # inventory keep info and starting + ending timestamps
      inventory[[i]] <- data.frame(
        fname = f,
        stationID = info$stationID[1],
        lat = info$lat[1],
        lon = info$lon[1],
        ts_utc = min(info$obsTimeUtc, na.rm = TRUE),
        te_utc = max(info$obsTimeUtc, na.rm = TRUE)
      )
    }
  }
  cat(class(inventory))
  inventory <- do.call("rbind", inventory)
  inventory$lat <- as.numeric(inventory$lat)
  inventory$lon <- as.numeric(inventory$lon)
  write.csv(inventory, invpath, row.names = FALSE)
  inventory <- inventory |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  return(inventory)
}

# use this version for first batch of data in the Triangle and whole NC
create_wu_inventory_oldversion <- function(fpath, invpath) {
  flist <- list.files(fpath, full.names = TRUE, recursive = TRUE)
  inventory <- NULL
  for (i in seq_along(flist)) {
    f <- flist[i]
    df <- read.csv(file = f, nrows = 1)
    dates <- data.table::fread(
      file = f,
      colClasses = c(
        rep("NULL", 2),
        NA,
        rep("NULL", 19)
      )
    )
    dates <- dates$obsTimeUtc |>
      as.POSIXct(tz = "UTC") |>
      lubridate::floor_date(unit = "hours")
    inventory[[i]] <- data.frame(
      fname = f,
      stationID = df$stationID,
      lat = df$lat,
      lon = df$lon,
      ts_utc = min(dates, na.rm = TRUE),
      te_utc = max(dates, na.rm = TRUE)
    )
  }
  inventory <- do.call("rbind", inventory)
  write.csv(inventory, invpath, row.names = FALSE)
  inventory <- inventory |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  return(inventory)
}
