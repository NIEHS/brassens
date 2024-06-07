testthat::test_that("load_pa works well", {
  ts = as.POSIXct("2021-07-21 00:00:00", tz = "UTC")
  te = as.POSIXct("2021-07-22 23:59:59", tz = "UTC")
  triangle <- rbind(
    c(-79.19, 35.6),
    c(-79.19, 36.11),
    c(-78.39, 36.11),
    c(-78.39, 35.6)
  ) |>
    terra::vect("polygons", crs = "epsg:4326")
  pa_path <- testthat::test_path("../testdata/pa_raw_simulated_testdata.rds")
  expect_no_error(
    load_pa(
      ts = ts,
      te = te,
      area = triangle,
      storage_file = pa_path
    )
  )
  ts = as.POSIXct("2000-07-21 00:00:00", tz = "UTC")
  te = as.POSIXct("2000-07-22 23:59:59", tz = "UTC")
  expect_message(
    load_pa(
      ts = ts,
      te = te,
      area = triangle,
      storage_file = pa_path
    )
  )
})
