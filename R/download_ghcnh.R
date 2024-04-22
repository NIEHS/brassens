#' Find all the GHCN-H stations within a polygon
#' @param polygon a sf object with a POLYGON geometry
#' @return a sf object with the GHCN-H stations within the polygon
#' @author Eva Marques
#' @export
find_ghcnh_polygon <- function(polygon) {
  stopifnot("polygon is not a sf or sfc" = inherits(polygon, c("sf", "sfc")),
            "polygon is not a POLYGON" =
              sf::st_geometry_type(polygon, by_geometry = FALSE) == "POLYGON")
  url <- paste0("https://www.ncei.noaa.gov/oa/",
                "global-historical-climatology-network/hourly/doc/",
                "ghcnh-station-list.csv")
  inv <- read.csv(url, sep = ",", header = FALSE)
  colnames(inv) <- c("site_id",
                     "lat",
                     "lon",
                     "elevation",
                     "state",
                     "name",
                     "gsnflag",
                     "hcnflag",
                     "wmoid")
  inv <- inv |> sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  inv_in_poly <- sf::st_filter(inv, polygon)
  return(inv_in_poly)
}

#' Find the nearest GHCN-H station to a point
#' @param lat latitude of the point (in WGS84)
#' @param lon longitude of the point (in WGS84)
#' @return a sf object with the nearest GHCN-H station
#' @author Eva Marques
#' @export
find_nearest_ghcnh <- function(lat, lon) {
  my_point <- sf::st_point(c(lon, lat)) |>
    sf::st_sfc(crs = 4326) |>
    sf::st_sf()
  url <- paste0("https://www.ncei.noaa.gov/oa/",
                "global-historical-climatology-network/hourly/doc/",
                "ghcnh-station-list.csv")
  inv <- read.csv(url, sep = ",", header = FALSE)
  colnames(inv) <- c("site_id",
                     "lat",
                     "lon",
                     "elevation",
                     "state",
                     "name",
                     "gsnflag",
                     "hcnflag",
                     "wmoid")
  inv <- inv |> sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  nearest <- sf::st_nearest_feature(my_point, inv)
  return(inv[nearest,])
}

#' Download GHCN-H station data of a given year
#' @param site_id the GHCN-H station ID
#' @param year the year of the data
#' @return a data.frame with the GHCN-H station raw data
#' @author Eva Marques
#' @export
download_ghcnh_station <- function(site_id, year) {
  url <- paste0("https://www.ncei.noaa.gov/oa/",
                "global-historical-climatology-network/hourly/access/by-year/",
                as.character(year),
                "/psv/GHCNh_",
                site_id,
                "_",
                as.character(year),
                ".psv")
  if (RCurl::url.exists(url)){
    x <- read.table(url, sep = "|", header = TRUE, stringsAsFactors = FALSE) |>
      tidyr::drop_na(temperature)
    return(x)
  } else {
    message("The URL does not exist for station ", site_id, ".")
    return(NULL)
  }
}


#' Download all GHCN-H stations data in area between two dates
#' @param ts start date
#' @param te end date
#' @param area a sf, sfc, SpatRaster or SpatVector object
#' @return a data.frame with the GHCN-H stations observations in the area
download_ghcnh <- function(ts, te, area) {
  year_ts <- lubridate::year(ts)
  year_te <- lubridate::year(te)
  bounds <- area |>
    format_area()
  area_inv <- find_ghcnh_polygon(bounds)
  for (i in 1:nrow(area_inv)) {
    site_id <- area_inv[i, ]$site_id
    for (y in year_ts:year_te) {
      if (exists("ghcnh")) {
        ghcnh <- rbind(ghcnh,
                       download_ghcnh_station(site_id, year = y))
      } else {
        ghcnh <- download_ghcnh_station(site_id, year = y)
      }
    }
  }
  ghcnh <- format_ghcnh(ghcnh)
  ghcnh <- ghcnh[which(between(ghcnh$time, ts, te)), ]
  return(ghcnh)
}

