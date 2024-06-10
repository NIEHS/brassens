testthat::test_that("manage_na works well", {
  data <- "../testdata/wu_raw_simulated_testdata.rds" |>
    testthat::test_path() |>
    readRDS() |>
    format_wu()
  expect_no_error(manage_na(data))
  no_na <- manage_na(data)
  expect_true(nrow(no_na[which(is.na(no_na$temp)), ]) == 0)
  date_range <- as.numeric(
    difftime(
      max(data$time),
      min(data$time),
      unit = "hour"
    )
  ) + 1
  full_ts <- manage_na(data, na_thresh = 0.0)
  nb_stations <- length(unique(full_ts$site_id))
  expect_true(nrow(full_ts) == nb_stations * date_range)
})

testthat::test_that("clean_cws works well", {

})


testthat::test_that("clean_cws_large works well", {

})
