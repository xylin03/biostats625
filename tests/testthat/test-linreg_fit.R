# test-fit_linear.R

test_that("fit_linear() runs and returns expected structure", {
  data <- mtcars
  model <- fit_linear(mpg ~ wt + hp + disp, data = data)

  # Check output class and names
  expect_type(model, "list")
  expected_names <- c(
    "coefficients", "r.squared", "adj.r.squared",
    "f.statistic", "f.p.value", "n_removed", "n_used",
    "fitted.values", "residuals"
  )
  expect_true(all(expected_names %in% names(model)))

  # Check coefficient table structure
  coef_df <- model$coefficients
  expect_s3_class(coef_df, "data.frame")
  expect_true(all(c("Estimate", "Std.Error", "t.value", "p.value") %in% names(coef_df)))

  # R-squared should be between 0 and 1
  expect_true(model$r.squared >= 0 && model$r.squared <= 1)

  # Residuals should sum approximately to zero
  expect_lt(abs(sum(model$residuals)), 1e-8)
})

test_that("fit_linear() agrees with lm() output", {
  data <- mtcars
  model_custom <- fit_linear(mpg ~ wt + hp + disp, data = data)
  model_base <- lm(mpg ~ wt + hp + disp, data = data)
  coef_custom <- model_custom$coefficients$Estimate
  coef_base <- coef(model_base)

  # Coefficients numerically match
  expect_equal(as.numeric(coef_custom), as.numeric(coef_base), tolerance = 1e-6)

  # R-squared matches lm()
  expect_equal(model_custom$r.squared, summary(model_base)$r.squared, tolerance = 1e-6)
})

test_that("fit_linear() handles missing values correctly", {
  data <- mtcars
  data$hp[1:2] <- NA
  n_before <- nrow(data)
  model <- fit_linear(mpg ~ wt + hp + disp, data = data)

  # Check that some rows were removed
  expect_gt(model$n_removed, 0)
  expect_equal(model$n_used + model$n_removed, n_before)
})


