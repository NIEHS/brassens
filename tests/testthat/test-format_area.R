testthat::test_that("format_area works well", {
  expect_error(format_area("a"))
  triangle <- rbind(
    c(-79.19, 35.6),
    c(-79.19, 36.11),
    c(-78.39, 36.11),
    c(-78.39, 35.6)
  ) |>
    terra::vect("polygons", crs = "epsg:4326")
  expect_no_error(format_area(triangle))
  expect_no_error(inherits(format_area(triangle), c("sf", "sfc")))
  expect_no_error(format_area(format_area(triangle)))
  x <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = -79.19,
    xmax = -78.39,
    ymin = 35.6,
    ymax = 36.11
  )
  terra::values(x) <- 1:100
  expect_no_error(format_area(x))
})
