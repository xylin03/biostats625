#' @title Predict from a fitted linear regression model
#' @description
#' The `predict_linear()` function generates predictions from a fitted linear
#' regression model created by `fit_linear()`. It uses the model coefficients
#' and the provided new data to compute predicted Y values.
#'
#' @param fit A fitted model object returned by `fit_linear()`.
#' @param newdata A data frame containing the same predictor variables as used in
#'   the fitted model. If `newdata` is `NULL`, predictions for the training data
#'   (used in fitting) will be returned.
#'
#' @return A numeric vector of predicted values.
#'
#' @details
#' This function constructs a vector that contain all the predictions made by by-hand calculation
#' based on the fitted linear regression model created by `fit_linear()`. It allows you to make several
#' predictions at the same time, which is more efficient than the original R function `predict()`.
#'
#' @importFrom stats model.matrix terms
#' @examples
#' model <- fit_linear(mpg ~ wt + hp + disp, data = mtcars)
#' predict_linear(model, newdata = mtcars[1:5, ])
#' @export
predict_linear <- function(fit, newdata = NULL) {
  if (!is.list(fit) || is.null(fit$coefficients)) {
    stop("fit must be a fitted model returned by fit_linear().")
  }

  # Extract coefficient names and values
  beta <- fit$coefficients$Estimate
  names(beta) <- rownames(fit$coefficients)

  # If no new data provided, predict on training data (stored in fit$fitted.values)
  if (is.null(newdata)) {
    message("No new data provided; returning fitted values from training data.")
    return(fit$fitted.values)
  }

  if (!is.data.frame(newdata)) {
    stop("newdata must be a data frame.")
  }

  # Build model matrix for new data
  X_new <- model.matrix(~ ., data = newdata)

  # Ensure columns match coefficients
  missing_vars <- setdiff(names(beta), colnames(X_new))
  extra_vars <- setdiff(colnames(X_new), names(beta))

  if (length(missing_vars) > 0) {
    stop("Missing variables in newdata: ", paste(missing_vars, collapse = ", "))
  }

  # Align columns for multiplication
  X_new <- X_new[, names(beta), drop = FALSE]

  # Compute predictions
  preds <- as.vector(X_new %*% beta)
  return(preds)
}
