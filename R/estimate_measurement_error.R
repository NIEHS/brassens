#' Find the closest reference site for each site in a cws object
#' @param cws sf (or inherited) object with citizen weather stations
#' @param ref sf (or inherited) object with reference weather stations
#' @return sf object with the closest reference site for each cws
#' with additional columns ref_id, ref_geometry and dist_to_ref
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
#' @param cws sf object with citizen weather stations
#' @param ref sf object with reference weather stations
#' @return hourly_temp object with the reference temperature in columns temp_r
calc_temp_error <- function(cws, ref) {
  # check column names
  cols <- c("site_id", "temp", "geometry", "time")
  stopifnot("some columns missing in cws" = all(cols %in% colnames(cws)),
            "some columns missing in ref" = all(cols %in% colnames(ref)))
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

