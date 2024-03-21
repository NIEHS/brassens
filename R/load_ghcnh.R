load_ghcnh_period <- function(rpath, ts, te, wpath) {
  files <- list.files(rpath, full.names = TRUE)
  for (f in files) {
    data <- read.table(f, sep = "|", header = TRUE, stringsAsFactors = FALSE)
    data$time <- apply(data[, c("Year", "Month", "Day", "Hour")],
                       1,
                       function(x) {
                         as.POSIXct(paste0(x[1],
                                           "-",
                                           x[2],
                                           "-",
                                           x[3],
                                           " ",
                                           x[4],
                                           ":00:00"),
                                    format = "%Y-%m-%d %H:%M:%S",
                                    tz = "UTC")
    })
    data <- data[which(data$time >= ts & data$time <= te), ]
    if (exists("output")) {
      output <- rbind(output, data)
    } else {
      output <- data
    }
  }
  fname <- paste0(wpath,
                  "ghcnh_",
                  format(ts, "%Y%m%d"),
                  "_",
                  format(te, "%Y%m%d"),
                  ".csv")
  data.table::fwrite(output, fname)
  return(output)
}
