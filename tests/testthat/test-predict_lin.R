# test-predict_lin.R

test_that("predict_linear() returns correct predictions for numeric predictors", {
  # Fit model using mtcars dataset
  fit <- fit_linear(mpg ~ wt + hp + disp, data = mtcars)

  # Predict on same data
  preds <- predict_linear(fit, newdata = mtcars)

  # Expected: numeric vector of correct length
  expect_type(preds, "double")
  expect_length(preds, nrow(mtcars))

  # Predictions should be close to fitted values
  expect_equal(preds, fit$fitted.values, tolerance = 1e-8)
})

test_that("predict_linear() returns fitted values when newdata = NULL", {
  fit <- fit_linear(mpg ~ wt + hp + disp, data = mtcars)

  preds <- predict_linear(fit)

  expect_equal(preds, fit$fitted.values)
})

test_that("predict_linear() errors if input is not from fit_linear()", {
  fake_fit <- list(model = "not_real")
  expect_error(predict_linear(fake_fit, mtcars),
               "fit must be a fitted model returned by fit_linear")
})

test_that("predict_linear() errors when newdata is not a data frame", {
  fit <- fit_linear(mpg ~ wt + hp + disp, data = mtcars)

  expect_error(predict_linear(fit, newdata = 5),
               "newdata must be a data frame")
})

test_that("predict_linear() detects missing variables in newdata", {
  fit <- fit_linear(mpg ~ wt + hp + disp, data = mtcars)

  # Drop 'hp' column from newdata
  bad_data <- mtcars[, c("wt", "disp")]

  expect_error(predict_linear(fit, bad_data),
               "Missing variables in newdata")
})

test_that("predict_linear() works with categorical predictors", {
  # Create dataset with categorical variable
  df <- mtcars
  df$cyl <- factor(df$cyl)

  fit <- fit_linear(mpg ~ wt + cyl, data = df)

  # Predict using same data
  preds <- predict_linear(fit, df)

  expect_type(preds, "double")
  expect_length(preds, nrow(df))

  # Predictions should be numeric and finite
  expect_true(all(is.finite(preds)))
})

test_that("predict_linear() aligns columns correctly even if newdata has extra columns", {
  fit <- fit_linear(mpg ~ wt + hp, data = mtcars)

  newdata <- mtcars
  newdata$extra <- rnorm(nrow(mtcars))

  preds <- predict_linear(fit, newdata)
  expect_length(preds, nrow(newdata))
})

test_that("predict_linear() works correctly with a newly imported dataset", {
  # Simulate a newly imported dataset
  df <- data.frame(
    age = c(23, 45, 31, 52, 41, 36, 29, 48),
    gender = factor(c("Male", "Female", "Female", "Male", "Male", "Female", "Female", "Male")),
    bmi = c(21.3, 25.4, 30.2, 28.5, 27.1, 23.9, 31.4, 26.2),
    cholesterol = c(180, 210, 190, 230, 205, 195, 220, 200),
    bp = c(115, 132, 140, 128, 125, 118, 135, 130)
  )

  # Add one missing value to test handling
  df$bmi[3] <- NA

  # Fit model with interaction term
  fit <- fit_linear(bp ~ age + gender + bmi + cholesterol, data = df)

  # Create new dataset without missing data for prediction
  new_df <- df[!is.na(df$bmi), ]

  # Generate predictions
  preds <- predict_linear(fit, new_df)

  # Compare with lm() predictions
  base_model <- lm(bp ~ age + gender + bmi + cholesterol, data = df)
  base_preds <- predict(base_model, newdata = new_df)

  # Expectations
  expect_type(preds, "double")
  expect_equal(length(preds), nrow(new_df))
  expect_equal(unname(preds), unname(base_preds), tolerance = 1e-6)

  # Check model structure and missing value handling
  expect_s3_class(fit, "fit_linear")
  expect_gt(fit$n_removed, 0)
  expect_equal(fit$n_used + fit$n_removed, nrow(df))
})

