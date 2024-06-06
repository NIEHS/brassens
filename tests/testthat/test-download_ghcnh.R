testthat::test_that("find_ghcnh_polygon works well", {
  area <- rbind(
    c(-78.5, 35.6),
    c(-78.5, 36.0),
    c(-78.2, 36.0),
    c(-78.2, 35.6)
  ) |>
    terra::vect("polygons", crs = "epsg:4326")
  expect_no_error(find_ghcnh_polygon(area))
  empty_samp <- find_ghcnh_polygon(area)
  expect_true(inherits(empty_samp, "sf"))
  expect_equal(nrow(empty_samp), 0)
  expect_error(find_ghcnh_polygon(1))
  triangle <- rbind(
    c(-79.19, 35.6),
    c(-79.19, 36.11),
    c(-78.39, 36.11),
    c(-78.39, 35.6)
  ) |>
    terra::vect("polygons", crs = "epsg:4326")
  expect_no_error(find_ghcnh_polygon(triangle))
  triangle_samp <- find_ghcnh_polygon(triangle)
  expect_equal(nrow(triangle_samp), 6)
})

testthat::test_that("find_nearest_ghcnh works well", {
  expect_error(find_nearest_ghcnh("-78", "35"))
  expect_no_error(find_nearest_ghcnh(lat = 35.7, lon = -78.69))
  my_ghcnh <- find_nearest_ghcnh(lat = 35.7, lon = -78.69)
  expect_equal(nrow(my_ghcnh), 1)
  expect_equal(my_ghcnh$name, " RALEIGH STATE UNIV            ")
})

testthat::test_that("download_ghcnh_station works well", {

})

testthat::test_that("download_ghcnh works well", {

})

