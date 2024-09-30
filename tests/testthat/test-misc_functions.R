test_that("num2words works", {
  expect_equal(num2words(4), "four")
  expect_equal(num2words(400, capital = TRUE), "Four hundred")
})
