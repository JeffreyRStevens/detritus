
test_that("compare_models() throws no errors", {
  expect_no_error(models157 <- compare_models(model1, model5, model7))
})

models157 <- compare_models(model1, model5, model7)

test_that("model comparison tables are correct", {
  expect_equal(round(models157$BIC, 2), c(211.69, 205.73, 185.90))
  expect_equal(models157$Formula, c("mpg ~ 1", "mpg ~ 1 + (1 | gear)", "mpg ~ hp + (1 | gear) + (1 | carb)"))
})

# test_that("best model is found", {
  # utils::globalVariables("model7")
  # expect_no_error(besttestmodel <- find_best_model(models157))
  # expect_true(find_best_model(models157)$best_model_name == "model7")
# })

test_that("formulas are built correctly", {
  expect_equal(build_formula(model1), "mpg ~ 1")
  expect_equal(build_formula(model3), "mpg ~ disp + hp")
  expect_equal(build_formula(model5), "mpg ~ 1 + (1 | gear)")
  expect_equal(build_formula(model7), "mpg ~ hp + (1 | gear) + (1 | carb)")
})

test_that("fixed effects are extracted correctly", {
  expect_equal(build_formula(model1, all = TRUE)$fixed, "1")
  expect_equal(build_formula(model3, all = TRUE)$fixed, "disp + hp")
  expect_equal(build_formula(model5, all = TRUE)$fixed, "1")
  expect_equal(build_formula(model7, all = TRUE)$fixed, "hp")
})

test_that("random effects are extracted correctly", {
  expect_true(is.null(build_formula(model1, all = TRUE)$random))
  expect_equal(build_formula(model5, all = TRUE)$random, "(1 | gear)")
  expect_equal(build_formula(model7, all = TRUE)$random, "(1 | gear) + (1 | carb)")
})
