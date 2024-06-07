testthat::test_that("summarize_hourly_temp works well", {
  expect_error(summarize_hourly_temp(x = "Vive les croissants et les baguettes",
                                     time = "obsTimeUtc",
                                     temp = "tempAvg",
                                     lat = "lat",
                                     lon = "lon"))
  x <- readRDS(testthat::test_path("../testdata/wu_raw_simulated_testdata.rds"))
  expect_error(summarize_hourly_temp(x = x,
                                     time = "obsTimeUtc",
                                     temp = "tempAvgggg",
                                     lat = "lat",
                                     lon = "lon"))
  expect_error(summarize_hourly_temp(x = x,
                                     time = "obsTimeUtc",
                                     temp = 24,
                                     lat = "lat",
                                     lon = "lon"))
  expect_no_error(summarize_hourly_temp(x = x,
                                        time = "obsTimeUtc",
                                        temp = "tempAvg",
                                        lat = "lat",
                                        lon = "lon"))
  expect_no_error(summarize_hourly_temp(x = as.data.frame(x),
                                        time = "obsTimeUtc",
                                        temp = "tempAvg",
                                        lat = "lat",
                                        lon = "lon"))
  # convert x to sf
  x_sf <- x |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  expect_no_error(summarize_hourly_temp(x = x_sf,
                                        time = "obsTimeUtc",
                                        temp = "tempAvg",
                                        lat = "lat",
                                        lon = "lon"))
  # convert x to sftime
  x_sftime <- x |>
    sftime::st_as_sftime(coords = c("lon", "lat"),
                         time_column_name = "obsTimeUtc",
                         crs = 4326,
                         remove = FALSE)
  expect_no_error(summarize_hourly_temp(x = x_sftime,
                                        time = "obsTimeUtc",
                                        temp = "tempAvg",
                                        lat = "lat",
                                        lon = "lon"))
  # test when rows are duplicated
  nrow(x)
  x_dup <- rbind(x, x)
  expect_no_error(summarize_hourly_temp(x = x_dup,
                                        time = "obsTimeUtc",
                                        temp = "tempAvg",
                                        lat = "lat",
                                        lon = "lon"))
  summarized_dup <- summarize_hourly_temp(x = x_dup,
                        time = "obsTimeUtc",
                        temp = "tempAvg",
                        lat = "lat",
                        lon = "lon")
  expect_true(nrow(summarized_dup) <= nrow(x))
  expect_true(all(lubridate::second(summarized_dup$time) == 0))
  expect_true(all(lubridate::minute(summarized_dup$time) == 0))
  expect_true(nrow(unique(summarized_dup[, c("lon", "lat", "time")])) ==
                nrow(summarized_dup))
})

testthat::test_that("format_pa works well", {
  raw <- "../testdata/pa_raw_simulated_testdata.rds" |>
    testthat::test_path() |>
    readRDS()
  expect_no_error(format_pa(raw))
  raw_df <- raw |>
    as.data.frame()
  expect_no_error(format_pa(raw_df))
  raw_sf <- raw |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  expect_no_error(format_pa(raw_sf))
  raw_sftime <- raw |>
    sftime::st_as_sftime(coords = c("longitude", "latitude"),
                         time_column_name = "time_stamp",
                         crs = 4326,
                         remove = FALSE)
  expect_no_error(format_pa(raw_sftime))
  # missing columns
  expect_error(format_pa(raw[, c("time_stamp", "latitude", "longitude")]))
  # test output format
  expect_true(inherits(format_pa(raw), "sftime"))
  cols <- c("site_id", "time", "lon", "lat", "geometry", "temp", "network")
  expect_true(all(colnames(format_pa(raw)) %in% cols))
  expect_true(all(cols %in% colnames(format_pa(raw))))
})

testthat::test_that("format_wu works well", {

})

testthat::test_that("format_ghcnh works well", {

})

