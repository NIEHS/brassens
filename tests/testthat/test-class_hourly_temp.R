testthat::test_that("hourly_temp works well", {
  expect_error(hourly_temp("bonjour les amis", network = "cava?"))
  x <- readRDS(testthat::test_path("../testdata/wu_raw_simulated_testdata.rds"))
  expect_no_error(hourly_temp(x,
    time = "obsTimeUtc",
    temp = "tempAvg",
    lat = "lat",
    lon = "lon",
    network = "WU"
  ))
  my_new_x <- hourly_temp(x,
    time = "obsTimeUtc",
    temp = "tempAvg",
    lat = "lat",
    lon = "lon",
    network = "WU"
  )
  expect_true(inherits(my_new_x, "hourly_temp"))
  expect_true(inherits(my_new_x, "data.frame"))
  cols <- c("site_id", "temp", "lat", "lon", "time", "network")
  expect_true(all(colnames(my_new_x) %in% cols))
  expect_equal(nrow(x), nrow(my_new_x))
  expect_error(hourly_temp(x,
    time = "obsTimeUtc",
    temp = "tempAvgggg",
    lat = "lat",
    lon = "lon",
    network = "WU"
  ))
  expect_error(hourly_temp(x,
    network = "WU"
  ))
})
