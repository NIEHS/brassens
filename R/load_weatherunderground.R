#' Load Weather Underground stations for a given period and area.
#' @param ts Start time
#' @param te End time
#' @param area Area of interest
#' @param inventory an sf object with inventory of
#' @return a data.table with the raw WU data
#' @importFrom data.table fread
#' @author Eva Marques
#' @export
load_wu <- function(ts, te, area, inventory) {
  obsTimeUtc <- NULL # nolint
  wu <- NULL
  stopifnot(
    "ts is not a POSIXct" = inherits(ts, "POSIXct"),
    "te is not a POSIXct" = inherits(te, "POSIXct"),
    "ts is after te" = ts <= te
  )
  area <- area |>
    format_area() |>
    sf::st_transform(crs = sf::st_crs(inventory))
  # extract stations in inventory that are within the area
  if (!sf::st_is_valid(area)) {
    area <- terra::vect(area)
    inventory <- terra::vect(inventory)
    inv_selection <- terra::crop(inventory, area) |>
      sf::st_as_sf()
  } else {
    inv_selection <- sf::st_filter(inventory, area)
  }
  # check type of ts_utc and te_utc
  if (
    !inherits(inv_selection$ts_utc, "POSIXct") ||
      !inherits(inv_selection$te_utc, "POSIXct")
  ) {
    stop("inventory times must be POSIXt objects")
  }
  inv_selection <- inv_selection[
    which(
      inv_selection$ts_utc <= te &
        inv_selection$te_utc >= ts
    ),
  ]
  for (f in inv_selection$fname) {
    aws <- data.table::fread(f)
    aws <- aws[as.character(obsTimeUtc) != "unavailable"]
    aws <- aws[as.character(obsTimeUtc) != "api failure"]
    aws$obsTimeUtc <- as.POSIXct(
      aws$obsTimeUtc,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )
    aws <- aws[obsTimeUtc >= ts & obsTimeUtc <= te, ]
    # datetime type problem (character if "unavailable" or double)
    # and not useful, so set to NULL:
    aws$datetime <- NULL
    if (exists("wu")) {
      wu <- rbind(wu, aws)
    } else {
      wu <- aws
    }
  }
  if (!exists("wu")) {
    wu <- NULL
    message("No data found for the given period")
  }
  wu <- unique(wu)
  wu$tempAvg <- as.numeric(wu$tempAvg)
  wu$lon <- as.numeric(wu$lon)
  wu$lat <- as.numeric(wu$lat)
  cat("load_wu() done\n")
  wu
}
