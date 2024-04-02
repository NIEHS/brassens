#` @title Generate unique sensor ID
#` @description This function generates a unique sensor "id" column based on
#` the latitude and longitude of the sensor.
#` @param x A data frame with columns "lat" and "lon".
#` @return A data frame with an additional column "id".
#` @author Eva Marques
generate_site_id <- function(x) {
  stopifnot("lat and lon columns are missing" =
              all(c("lat", "lon") %in% colnames(x)))
  x$site_id <- interaction(sprintf("%.4f", x$lon),
                           sprintf("%.4f", x$lat),
                           sep = "_")
  return(x)
}
