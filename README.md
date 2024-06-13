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
 
## Directory structure
- `./R`: R functions  
- `./vignettes`: illustrative examples of milestone analyses  
- `./tests/testthat`: testing routines for R code

## References

**CrowdQC+ library**  
Fenner, D., Bechtel, B., Demuzere, M., Kittner, J. and Meier, F. (2021): CrowdQC+ – A quality-control for crowdsourced air-temperature observations enabling world-wide urban climate applications. Frontiers in Environmental Science 9: 720747. DOI: 10.3389/fenvs.2021.720747.

**Global Historical Climatology Network - hourly**   
Menne, Matthew J.; Noone, Simon; Casey, Nancy W.; Dunn, Robert H.; McNeill, Shelley; Kantor, Diana; Thorne, Peter W.; Orcutt, Karen; Cunningham, Sam; Risavi, Nicholas. 2023. Global Historical Climatology Network-Hourly (GHCNh). NOAA National Centers for Environmental Information. [accessed on 2024/06/12]

**PurpleAir data download functions adapted from**   
Callahan J, Martin H, Wilson K, Brasel T, Miller H (2023). AirSensor: Process and Display Data from Air Quality Sensors. R package version 1.1.1, https://CRAN.R-project.org/package=AirSensor.

**PurpleAir:** https://www2.purpleair.com  
**WeatherUnderground:** https://www.wunderground.com
