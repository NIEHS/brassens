#' Find the closest reference site for each site in a cws object
#' @param cws sf (or inherited) object with citizen weather stations
#' with columns site_id and geometry
#' @param ref sf (or inherited) object with reference weather stations
#' with columns site_id and geometry
#' @return sf object with the closest reference site for each cws
#' with additional columns ref_id and dist_to_ref
#' @export
find_closest_ref <- function(cws, ref) {
  stopifnot("cws does not inherit from sf" = inherits(cws, "sf"),
            "ref does not inherit from sf" = inherits(ref, "sf"))
  # check column names
  cols <- c("site_id", "geometry")
  stopifnot("some columns missing in cws" = all(cols %in% colnames(cws)),
            "some columns missing in ref" = all(cols %in% colnames(ref)))
  cws_loc <- unique(cws[, cols])
  ref_loc <- unique(ref[, cols])
  nearest <- sf::st_nearest_feature(cws_loc, ref_loc)
  ref_nearest <- ref_loc[nearest, ]
  cws_loc$ref_id <- ref_nearest$site_id
  cws_loc$ref_geometry <- ref_nearest$geometry
  cws_loc$dist_to_ref <- sf::st_distance(cws_loc$geometry,
                                         cws_loc$ref_geometry,
                                         by_element = TRUE) |>
    as.numeric()
  cws_loc$ref_geometry <- NULL
  r <- merge(cws,
        as.data.frame(cws_loc[, c("site_id", "ref_id", "dist_to_ref")],
        by = "site_id"))
  return(r)
}

#' Add temperature from a reference to citizen weather stations observations
#' @param cws sf (or inherited) object with citizen weather stations
#' with columns site_id and geometry
#' @param ref sf (or inherited) object with reference weather stations
#' with columns site_id and geometry
#' @return sf (or inherited) object with additional columns temp_ref, temp_err,
#' ref_id, dist_to_ref
est_temp_error <- function(cws, ref) {
  # check column names
  cols <- c("site_id", "temp", "geometry", "time")
  stopifnot("some columns missing in cws" = all(cols %in% colnames(cws)),
            "some columns missing in ref" = all(cols %in% colnames(ref))
            )
  # check time class
  stopifnot("time should inherit from POSIXct in cws" =
              inherits(cws$time, "POSIXct"),
            "time should inherit from POSIXct in ref" =
              inherits(ref$time, "POSIXct")
            )
  # check crs
  stopifnot("cws and ref have different crs" =
              sf::st_crs(cws) == sf::st_crs(ref)
            )
  # check time is rounded to the hour (minutes and seconds to 0)
  stopifnot("time should be rounded to the hour in cws" =
              all(lubridate::minute(cws$time) == 0) &
              all(lubridate::second(cws$time) == 0),
            "time should be rounded to the hour in ref" =
              all(lubridate::minute(ref$time) == 0),
              all(lubridate::second(ref$time) == 0)
            )

  cws_r <- find_closest_ref(cws, ref)
  ref_reformat <- ref[, c("site_id",
                          "temp",
                          "time")] |>
    dplyr::rename(temp_ref = "temp") |>
    dplyr::rename(ref_id = "site_id") |>
    as.data.frame()
  ref_reformat$geometry <- NULL
  r <- merge(cws_r, ref_reformat, by = c("time", "ref_id"), all.x = T)
  r$temp_err <- r$temp - r$temp_ref
  return(r)
}

