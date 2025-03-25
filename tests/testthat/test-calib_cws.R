testthat::test_that("calib_cws works well", {
  x <- "../testdata/wu_raw_simulated_testdata.rds" |>
    testthat::test_path() |>
    readRDS() |>
    format_wu()
  ref <- "../testdata/ghcnh_formatted_testdata.csv" |>
    testthat::test_path() |>
    read.csv()
  ref <- ref |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  ref$time <- as.POSIXct(
    ref$time,
    tz = "UTC",
    format = "%Y-%m-%d %H:%M:%S"
  )
  # not same crs
  expect_no_error(out <- calib_cws(ref, x))
  expect_true(class(out) == "list")
  out_names <- c("obs", "bias", "ref_stats", "cws_in_buf")
  expect_true(all(out_names %in% names(out)))
  expect_true(inherits(out$obs, "data.frame"))
  expect_true(nrow(out$obs) == nrow(x))
  expect_true(nrow(out$bias) == 24)
  expect_true(inherits(out$bias, "data.frame"))
  bias_names <- c("utc", "bias_med", "bias_mean", "sd")
  expect_true(all(bias_names %in% names(out$bias)))
  expect_error(calib_cws(ref, x, max_dist = 0))
  expect_true(abs(median(out$obs$bias_mean)) < 7)
})
