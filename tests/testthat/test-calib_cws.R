testthat::test_that("calib_cws works well", {
  x <- "../testdata/wu_raw_simulated_testdata.rds" |>
    testthat::test_path() |>
    readRDS() |>
    format_wu()
  ref <- "../testdata/ghcnh_formatted_testdata.rds" |>
    testthat::test_path() |>
    readRDS()
  expect_warning(out <- calib_cws(x, ref))
  expect_true(class(out) == "list")
  out_names <- c("obs", "bias", "n_ref_cws_pairs", "ts", "te")
  expect_true(all(out_names %in% names(out)))
  expect_true(inherits(out$obs, "sf"))
  expect_true(nrow(out$obs) == nrow(x))
  expect_true(nrow(out$bias) == 24)
  expect_true(inherits(out$ts, "POSIXct"))
  expect_true(inherits(out$te, "POSIXct"))
  expect_true(is.numeric(out$n_ref_cws_pairs))
  expect_true(out$n_ref_cws_pairs == 16)
  expect_true(inherits(out$bias, "data.frame"))
  bias_names <- c("hour", "bias", "med_dist_to_ref", "n_pairs_x_days")
  expect_true(all(bias_names %in% names(out$bias)))
  expect_error(calib_cws(x, ref, max_dist = 0))
  expect_error(calib_cws(x, ref, max_dist = 20001))
  expect_true(abs(median(out$obs$bias)) < 7)
})



