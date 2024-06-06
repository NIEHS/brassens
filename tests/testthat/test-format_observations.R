testthat::test_that("summarize_hourly_temp works well", {
  x <- readRDS(testthat::test_path("../testdata/wu_raw_simulated_testdata.rds"))
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

})

testthat::test_that("format_wu works well", {

})

testthat::test_that("format_ghcnh works well", {

})

