test_that("linreg_fit works correctly", {
  data(mtcars)
  model <- linreg_fit(mpg ~ wt + hp + disp, data = mtcars)

  expect_s3_class(model, "linreg_fit")

  preds <- linreg_predict(model, mtcars)
  expect_length(preds, nrow(mtcars))
})

