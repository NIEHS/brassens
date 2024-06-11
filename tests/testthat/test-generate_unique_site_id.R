testthat::test_that("generate_unique_site_id works well", {
  x <- readRDS(testthat::test_path("../testdata/wu_raw_simulated_testdata.rds"))
  expect_no_error(generate_site_id(x))
  # convert x to sf:
  x_sf <- x |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  expect_no_error(generate_site_id(x_sf))
  # convert to sftime:
  x_sftime <- x |>
    sftime::st_as_sftime(
      coords = c("lon", "lat"),
      time_column_name = "obsTimeUtc",
      crs = 4326,
      remove = FALSE
    )
  expect_no_error(generate_site_id(x_sftime))
  # lat and lon missing:
  expect_error(generate_site_id(x[, c("stationID", "obsTimeUtc")]))
  # some tests on site_id:
  x_new <- generate_site_id(x)
  expect_true(class(x_new$site_id) == "factor")
  expect_true(all(between(nchar(as.character(x_new$site_id)), 17, 21)))
})
