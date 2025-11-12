# test-fit_linear.R

test_that("fit_linear() runs and returns expected structure", {
  data <- mtcars
  model <- fit_linear(mpg ~ wt + hp + disp, data = data)

  # Check class and names
  expect_s3_class(model, "fit_linear")
  expected_names <- c(
    "formula", "terms", "coefficients", "r.squared", "adj.r.squared",
    "f.statistic", "f.p.value", "n_removed", "n_used",
    "fitted.values", "residuals"
  )
  expect_true(all(expected_names %in% names(model)))

  # Coefficient table structure
  coef_df <- model$coefficients
  expect_s3_class(coef_df, "data.frame")
  expect_true(all(c("Estimate", "Std.Error", "t.value", "p.value") %in% names(coef_df)))

  # R-squared range
  expect_true(model$r.squared >= 0 && model$r.squared <= 1)

  # Residuals sum to ~0
  expect_lt(abs(sum(model$residuals)), 1e-8)
})

test_that("fit_linear() agrees with lm() output", {
  data <- mtcars
  model_custom <- fit_linear(mpg ~ wt + hp + disp, data = data)
  model_base <- lm(mpg ~ wt + hp + disp, data = data)
  summary_base <- summary(model_base)

  # Coefficients
  coef_custom <- model_custom$coefficients$Estimate
  coef_base <- coef(model_base)
  expect_equal(as.numeric(coef_custom), as.numeric(coef_base), tolerance = 1e-6)

  # R-squared and adjusted R-squared
  expect_equal(model_custom$r.squared, summary_base$r.squared, tolerance = 1e-6)
  expect_equal(model_custom$adj.r.squared, summary_base$adj.r.squared, tolerance = 1e-6)

  # F-statistic
  f_custom <- model_custom$f.statistic
  f_base <- as.numeric(summary_base$fstatistic[1])
  expect_equal(as.numeric(f_custom), f_base, tolerance = 1e-6)

  # F-statistic p-value
  df1 <- summary_base$fstatistic[2]  # numerator df
  df2 <- summary_base$fstatistic[3]  # denominator df
  f_p_base <- pf(f_base, df1, df2, lower.tail = FALSE)
  expect_equal(as.numeric(model_custom$f.p.value), as.numeric(f_p_base), tolerance = 1e-6)
})

test_that("fit_linear() handles missing values correctly", {
  data <- mtcars
  data$hp[1:2] <- NA
  n_before <- nrow(data)
  model <- fit_linear(mpg ~ wt + hp + disp, data = data)

  # Missing value handling
  expect_gt(model$n_removed, 0)
  expect_equal(model$n_used + model$n_removed, n_before)
})
