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
  expect_error(manage_na(data, na_thresh = 10))
  expect_error(manage_na(data, na_thresh = -1))
  expect_error(manage_na(data = "Je n'ai pas d'inspiration aujourd'hui"))
  expect_error(manage_na(data = data[ , -which(colnames(data) == "temp")]))
})

testthat::test_that("clean_cws works well", {
  x <- "../testdata/wu_raw_simulated_testdata.rds" |>
    testthat::test_path() |>
    readRDS() |>
    format_wu()
  expect_no_error(clean_cws(x))
  cleaned <- clean_cws(x)
  expect_error(clean_cws(x[ , -which(colnames(x) == "temp")]))
  expect_true(inherits(cleaned, "sftime"))
  cols <- c("site_id", "temp", "lat", "lon", "time")
  expect_true(all(cols %in% colnames(cleaned)))
  expect_error(clean_cws("Aboli bibelot d'inanite sonore"))
})


testthat::test_that("cut_area works well", {
  nc_url <- paste0("https://services1.arcgis.com/YBWrN5qiESVpqi92/arcgis/rest/",
                   "services/ncgs_state_county_boundary/FeatureServer/0/",
                   "query?outFields=*&where=1%3D1&f=geojson")
  nc <- terra::vect(nc_url)
  expect_no_error(cut_area(nc))
  nc_squared <- cut_area(nc)
  expect_equal(nrow(nc_squared), 20)
})
