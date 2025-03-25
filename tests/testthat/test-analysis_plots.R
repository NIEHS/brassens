test_that("timeseries function works correctly", {
  cws <- "../testdata/wu_raw_simulated_testdata.rds" |>
    testthat::test_path() |>
    readRDS() |>
    format_wu()
  ts <- as.POSIXct("2021-07-01 00:00:00")
  te <- as.POSIXct("2021-07-31 23:00:00")
  expect_no_error(plot <- timeseries(cws, ts, te, temp))
  expect_true(inherits(plot, "ggplot"))
  # check if the plot has the correct layers
  expect_true(
    any(
      sapply(
        plot$layers,
        function(layer) inherits(layer$geom, "GeomLine")
      )
    )
  )
  # check if the plot has the correct x and y aesthetics
  expect_true(plot$labels$x == "time (UTC)")
  expect_true(plot$labels$y == "temp")
  expect_true(plot$labels$group == "site_id")
  expect_true(plot$labels$colour == "network")
  expect_error(timeseries(cws$temp, ts, te, temp))
  expect_error(timeseries(cws, te, ts, temp))
  expect_error(timeseries(cws, ts, te, no_exist))
  cws$geometry <- NULL
  expect_no_error(timeseries(cws, ts, te, temp))
  cws <- as.data.table(cws)
  expect_no_error(timeseries(cws, ts, te, temp))
  cws$site_id <- NULL
  expect_error(timeseries(cws, ts, te, temp))
})


test_that("hourly_boxplot function works correctly", {
  cws <- "../testdata/wu_raw_simulated_testdata.rds" |>
    testthat::test_path() |>
    readRDS() |>
    format_wu()
  ts <- as.POSIXct("2021-07-21 00:00:00")
  te <- as.POSIXct("2021-07-21 03:00:00")
  expect_no_error(plot <- hourly_boxplot(cws, ts, te, temp))
  expect_true(inherits(plot, "ggplot"))
  expect_true(plot$labels$x == "network")
  expect_true(plot$labels$y == "temp")
  expect_true(plot$labels$colour == "network")
  expect_true(plot$labels$group == "network")
  cws$network <- NULL
  expect_error(hourly_boxplot(cws, ts, te, temp))
})

test_that("tile_ts function works correctly", {
  cws <- "../testdata/wu_raw_simulated_testdata.rds" |>
    testthat::test_path() |>
    readRDS() |>
    format_wu()
  ts <- as.POSIXct("2021-07-01 00:00:00")
  te <- as.POSIXct("2021-07-31 23:00:00")
  expect_no_error(plot <- tile_ts(cws, ts, te, temp))
  expect_true(inherits(plot, "ggplot"))
  expect_true(plot$labels$x == "time")
  expect_true(plot$labels$y == "weather station")
  expect_true(plot$labels$fill == "temp")
  cws$time <- NULL
  expect_error(tile_ts(cws, ts, te, temp))
})
