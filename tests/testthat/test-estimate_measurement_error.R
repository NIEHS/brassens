testthat::test_that("find_closest_ref works well", {
  cws <- "../testdata/wu_raw_simulated_testdata.rds" |>
    testthat::test_path() |>
    readRDS() |>
    format_wu()
  ref <- "../testdata/ghcnh_formatted_testdata.rds" |>
    testthat::test_path() |>
    readRDS()
  expect_no_error(find_closest_ref(cws, ref))
  expect_error(find_closest_ref(as.data.frame(cws), ref))
  expect_error(find_closest_ref(cws, as.data.frame(ref)))
  expect_error(find_closest_ref(cws, ref[, c("geometry")]))
  expect_error(find_closest_ref(cwsref[, c("site_id")], ref))
  r <- find_closest_ref(cws, ref)
  expect_true(all(c("ref_id", "dist_to_ref") %in% colnames(r)))
  expect_true(all(colnames(cws) %in% colnames(r)))
  expect_true(all(r$dist_to_ref >= 0))
  expect_true(all(r$dist_to_ref <= 100000))
  expect_equal(as.character(r[which(r$site_id == "-78.394156_35.631584"),
                              ]$ref_id[1]),
               "-78.781900_35.892200")
})


testthat::test_that("calc_temp_error works well", {
  cws <- "../testdata/wu_raw_simulated_testdata.rds" |>
    testthat::test_path() |>
    readRDS() |>
    format_wu()
  ref <- "../testdata/ghcnh_formatted_testdata.rds" |>
    testthat::test_path() |>
    readRDS()
  expect_no_error(calc_temp_error(cws, ref))
  r <- calc_temp_error(cws, ref)
  new_cols <- c("temp_err", "temp_ref", "ref_id", "dist_to_ref")
  expect_true(all(new_cols %in% colnames(r)))
  expect_true(all(colnames(cws) %in% colnames(r)))
  expect_true(inherits(r, "sf"))
  expect_equal(r$temp_ref + r$temp_err, r$temp, tolerance = 1e-6)
  t1 <- as.POSIXct("2021-07-21 06:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-07-21 15:00:00", tz = "UTC")
  eg1 <- r[which(r$site_id == "-78.394156_35.631584" & r$time == t1), ]
  eg2 <- r[which(r$site_id == "-78.394156_35.631584" & r$time == t2), ]
  expect_equal(eg1$ref_id, eg2$ref_id)
  expect_false(eg1$temp_ref == eg2$temp_ref)
  expect_equal(eg2$temp_ref, 28.9)
  expect_error(calc_temp_error(cws[, c("site_id", "temp", "geometry")], ref))
  expect_error(calc_temp_error(cws, ref[, c("site_id", "temp", "geometry")]))
})
