#' Load Weather Underground stations for a given period.
#' @param rpath Path to the directory containing the Weather Underground
#' station files of a region
#' @param ts Start time
#' @param te End time
#' @return A data.table containing the weather data for the given period
#' @import data.table
#' @export
#' @author Eva Marques
load_wu_period <- function(rpath, ts, te, wpath) {
  files <- list.files(rpath, full.names = TRUE)
  for (f in files) {
    aws <- data.table::fread(f)
    aws$datetime <- as.POSIXct(aws$obsTimeUtc, tz = "UTC")
    aws <- aws[datetime >= ts & datetime <= te, ]
    if (exists("wu")) {
      wu <- rbind(wu, aws)
    } else {
      wu <- aws
    }
  }
  fname <- paste0(wpath,
                  "wu_",
                  format(ts, "%Y%m%d"),
                  "_",
                  format(te, "%Y%m%d"),
                  ".csv")
  if (!exists("wu")) {
    stop("No data found for the given period")
  }
  data.table::fwrite(wu, fname)
  return(wu)
}

