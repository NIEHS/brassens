#' Tranform spatial objects to a POLYGON surrounding an area
#' @param x a sf, sfc, SpatRaster or SpatVector object
#' @return a POLYGON object or a bbox
format_area <- function(x) {
  stopifnot("x is not a sf, sfc, SpatRaster or SpatVector" =
              inherits(x, c("sf", "sfc", "SpatRaster", "SpatVector")))
  if (inherits(x, c("sf"))) {
    sf <- x[, attributes(x)$sf_column]
    geom <- sf::st_geometry_type(sf, by_geometry = FALSE)
    if (geom %in% c("POLYGON", "MULTIPOLYGON")) {
      y <- sf::st_transform(x, crs = 4326)
    } else {
      y <- x |>
        sf::st_transform(crs = 4326) |>
        sf::st_bbox() |>
        sf::st_as_sfc()
    }
  } else if (inherits(x, "sfc")) {
    geom <- sf::st_geometry_type(x, by_geometry = FALSE)
    if (geom %in% c("POLYGON", "MULTIPOLYGON")) {
      y <- sf::st_transform(x, crs = 4326)
    } else {
      y <- x |>
        sf::st_transform(crs = 4326) |>
        sf::st_bbox() |>
        sf::st_as_sfc()
    }
  } else if (inherits(x, "SpatVector")){
      if (terra::geomtype(x) == "polygons") {
        y <- x |>
          terra::project("EPSG:4326") |>
          sf::st_as_sf()
      } else {
        y <- x |>
          terra::project("EPSG:4326") |>
          sf::st_bbox() |>
          sf::st_as_sfc()
      }
  } else if(inherits(x, "SpatRaster")) {
      y <- x |>
        terra::project("EPSG:4326") |>
        sf::st_bbox() |>
        sf::st_as_sfc()
  }
  return(y)
}
