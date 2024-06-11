testthat::test_that("convert_temp works well", {
  expect_error(convert_temp(0, "C", "X"))
  expect_error(convert_temp(0, "X", "C"))
  expect_error(convert_temp("X", "C", "F"))
  expect_no_error(convert_temp(c(0, 32), "C", "F"))
  expect_equal(convert_temp(0, "C", "F"), 32)
  expect_equal(convert_temp(32, "F", "C"), 0)
  expect_equal(convert_temp(0, "C", "K"), 273.15)
  expect_equal(convert_temp(273.15, "K", "C"), 0)
  expect_equal(convert_temp(32, "F", "K"), 273.15)
  expect_equal(convert_temp(273.15, "K", "F"), 32)
})


testthat::test_that("my_pal works well", {
  are_colors <- function(x) {
    sapply(x, function(y) {
      tryCatch(is.matrix(col2rgb(y)),
        error = function(e) FALSE
      )
    })
  }
  expect_error(my_pal("fromage"))
  expect_no_error(pal1 <- my_pal("temp"))
  expect_no_error(pal2 <- my_pal("sw"))
  expect_no_error(pal3 <- my_pal("reds"))
  expect_no_error(pal4 <- my_pal("prior"))
  expect_no_error(pal5 <- my_pal("uhi"))
  expect_true(all(are_colors(pal1)))
  expect_true(all(are_colors(pal2)))
  expect_true(all(are_colors(pal3)))
  expect_true(all(are_colors(pal4)))
  expect_true(all(are_colors(pal5)))
})
