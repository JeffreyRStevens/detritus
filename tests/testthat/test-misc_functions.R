test_that("num2words() works", {
  expect_equal(num2words(4), "four")
  expect_equal(num2words(400, capital = TRUE), "Four hundred")
})

test_that("reverse_code() works", {
  expect_equal(reverse_code(1:3, 1, 3), 3:1)
})
