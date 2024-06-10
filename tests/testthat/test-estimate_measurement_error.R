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
