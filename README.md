[![R-CMD-check](https://github.com/NIEHS/brassens/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/NIEHS/brassens/actions/workflows/check-standard.yaml)
[![cov](https://NIEHS.github.io/brassens/badges/coverage.svg)](https://github.com/NIEHS/brassens/actions)
[![lint](https://github.com/NIEHS/brassens/actions/workflows/lint.yaml/badge.svg)](https://github.com/NIEHS/brassens/actions/workflows/lint.yaml)
[![pkgdown](https://github.com/NIEHS/brassens/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/NIEHS/brassens/actions/workflows/pkgdown.yaml)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

# brassens

The development of this library is in progress.

## A package to deal with Citizen Weather Stations temperature data

- calls to PurpleAir API from R session to recover timeseries of temperature on a given spatial area
- formatting data
- cleaning non-conventional measurements of temperature with statistical tools (CrowdQC+ library)
- calibration with reference stations (GHCNh dataset used by default)
 
## Pipeline tutorial
pa_file parameter can be NULL:  in this case, you need to provide an API key to download data from PurpleAir API to load_pa(). 

#### Prepare all parameters
```
> config <- list(
  ts = as.POSIXct("2021-07-22 00:00:00", tz = "UTC"),
  te = as.POSIXct("2021-07-23 23:59:59", tz = "UTC"),
  area = your_polygon,
  wu_inv = your_wu_inventory,
  pa_file = path_to_your_pa_file
)
```

#### Load GHCNh data 
You can also use another reference network for calibration. 
```
ghcnh <- download_ghcnh(config$ts, config$te, config$area)
```

#### Open and process citizen weather stations from WeatherUnderground and PurpleAir.
You can tune maximum distance. 
```
wu_list <- load_wu(config$ts, config$te, config$area, config$wu_inv) |>
  format_wu() |>
  clean_cws() |>
  calib_cws(ref = ghcnh, max_dist = 20000)

pa_list <- load_pa(
  ts = config$ts,
  te = config$te,
  area = config$area,
  storage_file = config$pa_file
) |>
  format_pa() |>
  clean_cws() |>
  calib_cws(ref = ghcnh, max_dist = 20000)
```


## References

**CrowdQC+ library**  
Fenner, D., Bechtel, B., Demuzere, M., Kittner, J. and Meier, F. (2021): CrowdQC+ – A quality-control for crowdsourced air-temperature observations enabling world-wide urban climate applications. Frontiers in Environmental Science 9: 720747. DOI: 10.3389/fenvs.2021.720747.

**Global Historical Climatology Network - hourly**   
Menne, Matthew J.; Noone, Simon; Casey, Nancy W.; Dunn, Robert H.; McNeill, Shelley; Kantor, Diana; Thorne, Peter W.; Orcutt, Karen; Cunningham, Sam; Risavi, Nicholas. 2023. Global Historical Climatology Network-Hourly (GHCNh). NOAA National Centers for Environmental Information. [accessed on 2024/06/12]

**PurpleAir data download functions adapted from**   
Callahan J, Martin H, Wilson K, Brasel T, Miller H (2023). AirSensor: Process and Display Data from Air Quality Sensors. R package version 1.1.1, https://CRAN.R-project.org/package=AirSensor.

**PurpleAir:** https://www2.purpleair.com  
**WeatherUnderground:** https://www.wunderground.com
