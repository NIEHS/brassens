find_closest_ref <- function(x, ref) {
  stopifnot("x is not a sf" = is(x, "sf"),
            "ref is not a sf" = is(ref, "sf"))
  nearest <- sf::st_nearest_feature(x, ref)
  return(ref[nearest, ])
}

#' Add temperature from a reference to a hourly_temp object
#' @param x hourly_temp object
#' @param ref hourly_temp object with the closest reference temperature
#' @return hourly_temp object with the reference temperature in columns temp_r
add_temp_reference <- function(x, ref) {
  if (is(x, "sftime")) {
    x <- x |> sftime::st_as_sftime(coords = c("lon", "lat"),
                                   time_column_name = "time",
                                   crs = 4326,
                                   remove = FALSE)
  }
  if (class(ref) != "sftime") {
    ref <- ref |> sftime::st_as_sftime(coords = c("lon", "lat"),
                                      time_column_name = "time",
                                      crs = 4326,
                                      remove = FALSE)
  }

}


#' Estimate measurement errors for a hourly_temp object
#'
estimate_temp_error <- function(x, ref) {
  x <- add_temp_reference(x, ref)

}
