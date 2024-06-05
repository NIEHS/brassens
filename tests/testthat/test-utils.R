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

})
