
find_ghcnh_polygon <- function(polygon) {
  stopifnot("polygon is not a sf" = is(polygon, "sf"),
            "polygon is not a POLYGON" =
              all(sf::st_geometry_type(polygon) == "POLYGON"))
  polygon <- sf::st_transform(polygon, crs = 4326)
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

load_ghcnh_station <- function(site_id, year) {
  url <- paste0("https://www.ncei.noaa.gov/oa/",
                "global-historical-climatology-network/hourly/access/by-year/",
                as.character(year),
                "/psv/GHCNh_",
                site_id,
                "_",
                as.character(year),
                ".psv")
  if (RCurl::url.exists(url)){
    x <- read.table(url, sep = "|", header = TRUE, stringsAsFactors = FALSE)
    return(x)
  } else {
    warning("The URL does not exist for station ", site_id, ".")
    return(NULL)
  }
}


load_ghcnh_period <- function(rpath, ts, te, wpath) {
  files <- list.files(rpath, full.names = TRUE)
  for (f in files) {
    data <- read.table(f, sep = "|", header = TRUE, stringsAsFactors = FALSE)
    data$time <- apply(
      data[, c("Year", "Month", "Day", "Hour")],
      1,
      function(x) {
        as.POSIXct(
          paste0(
            x[1],
            "-",
            x[2],
            "-",
            x[3],
            " ",
            x[4],
            ":00:00"
          ),
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        )
      }
    )
    data <- data[which(data$time >= ts & data$time <= te), ]
    if (exists("output")) {
      output <- rbind(output, data)
    } else {
      output <- data
    }
  }
  fname <- paste0(
    wpath,
    "ghcnh_",
    format(ts, "%Y%m%d"),
    "_",
    format(te, "%Y%m%d"),
    ".csv"
  )
  data.table::fwrite(output, fname)
  return(output)
}



format_ghcnh <- function(x) {
  cat("todoooo")
}
