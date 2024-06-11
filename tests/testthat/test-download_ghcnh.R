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
  # no page found:
  expect_message(download_ghcnh_station("USW00013722", 2100))
  expect_true(is.null(download_ghcnh_station("USW00013722", 2100)))
  expect_true(is.null(download_ghcnh_station("a", 2100)))
  expect_true(is.null(download_ghcnh_station("a", 210000)))
  # page found but empty:
  expect_message(download_ghcnh_station("USC00310750", 2021))
  expect_true(is.null(download_ghcnh_station("USC00310750", 2021)))
  # test parameters:
  expect_error(download_ghcnh_station(13722, 2023))
  # no error:
  expect_no_error(download_ghcnh_station("USW00013722", 2023))
})


testthat::test_that("download_ghcnh works well", {
  ts <- as.POSIXct("2021-07-01 00:00:00", tz = "UTC")
  te <- as.POSIXct("2021-07-02 23:59:59", tz = "UTC")
  area <- rbind(
    c(-78.79, 35.8),
    c(-78.79, 36),
    c(-78.78, 36),
    c(-78.78, 35.8)
  ) |>
    terra::vect("polygons", crs = "epsg:4326")
  expect_no_error(download_ghcnh(ts, te, area))
  expect_error(download_ghcnh(
    as.Date("2021-07-22"),
    as.Date("2021-07-24"),
    area
  ))
})
