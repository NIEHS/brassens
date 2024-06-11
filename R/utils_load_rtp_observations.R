open_hw_transects <- function(fpath) {
  f <- terra::vect(fpath) |>
    sf::st_as_sf()
  f$temp <- convert_temp(f$temp_f, from = "F", to = "C")
  f$datetime <- as.POSIXct(f$datetime, tz = "EST")
  f$time <- lubridate::with_tz(f$datetime, tz = "UTC")
  f$lon <- sf::st_coordinates(f)[, 1]
  f$lat <- sf::st_coordinates(f)[, 2]
  f$network <- "HEATWATCH"
  f <- generate_site_id(f)
  f <- f[, c("site_id", "temp", "lat", "lon", "time", "network")] |>
    sftime::st_as_sftime(time_column_name = "time")
  return(f)
}


load_rtp_observations <- function(ts, te) {
  # load RTP polygons
  nc_shp <- paste0(
    "NC_county_boundary/",
    "North_Carolina_State_and_County_Boundary_Polygons.shp"
  )
  nc_borders <- terra::vect(paste0("../input/", nc_shp))
  counties <- c("Wake", "Orange", "Durham", "Chatham", "Granville")
  rtp_poly <- nc_borders[which(nc_borders$County %in% counties), ] |>
    terra::project("EPSG:4326") |>
    sf::st_as_sf()

  # load and format WeatherUnderground
  wu <- data.table::fread("../input/rtp/wu_20210701_20210731.csv") |>
    format_wu()

  # load and format PurpleAir
  pa <- data.table::fread("../input/rtp/pa_20210720_20210727.csv") |>
    format_pa()

  # load and format GHCNh
  ghcnh <- download_ghcnh(ts, te, rtp_poly)

  # load and format HeatWatch
  hw_path <- "../input/traverses_chw_raleigh-durham_110821/"
  hw_pt_am <- open_hw_transects(fpath = paste0(hw_path, "am_trav.shp"))
  hw_pt_af <- open_hw_transects(fpath = paste0(hw_path, "af_trav.shp"))
  hw_pt_pm <- open_hw_transects(fpath = paste0(hw_path, "pm_trav.shp"))

  # load and format ECONET
  eco1 <- read.csv("../input/econet/econet_20210601_20210831_NK5FS1N1_1.csv")
  eco2 <- read.csv("../input/econet/econet_20210601_20210831_NK5FS1N1_2.csv")
  eco3 <- read.csv("../input/econet/econet_20210601_20210831_NK5FS1N1_3.csv")
  eco4 <- read.csv("../input/econet/econet_20210601_20210831_NK5FS1N1_4.csv")
  eco <- rbind(eco1, eco2, eco3, eco4)
  eco$time_est <- as.POSIXct(eco$time_est,
                             tz = "EST",
                             format = "%m/%d/%y %H:%M")
  eco$time <- lubridate::with_tz(eco$time_est, tzone = "UTC")
  eco$temp <- as.numeric(eco$temp)
  eco$lat <- as.numeric(eco$lat)
  eco$lon <- as.numeric(eco$lon)
  eco <- eco |>
    na.omit() |>
    hourly_temp(
      temp = "temp",
      lat = "lat",
      lon = "lon",
      time = "time",
      network = "ECONET"
    ) |>
    sftime::st_as_sftime(
      coords = c("lon", "lat"),
      time_column_name = "time",
      crs = 4326,
      remove = FALSE
    )

  # merge all observations
  var_list <- c("site_id", "time", "temp", "lat", "lon", "geometry", "network")
  cws <- rbind(
    wu[, var_list],
    pa[, var_list]
  )
  # remove observations with high latitude
  cws <- cws[which(cws$lat < 38), ]
  ref <- rbind(
    eco[, var_list],
    ghcnh[, var_list],
    hw_pt_am[, var_list],
    hw_pt_af[, var_list],
    hw_pt_pm[, var_list]
  )
  # select only the period of interest
  cws <- cws[which(between(cws$time, ts, te)), ]
  ref <- ref[which(between(ref$time, ts, te)), ]
  # return
  r <- list(
    "poly" = rtp_poly,
    "cws" = cws,
    "ref" = ref
  )
  return(r)
}
