#' @title Fit a complete linear regression model
#' @description
#' The `fit_linear()` function removes all rows with missing values from the dataset
#' and fits a linear regression model using the specified formula.
#' It returns a list containing the fitted model's coefficients and key summary
#' statistics such as R-squared, adjusted R-squared, F-statistic, p-value,
#' and the coefficient table. The function also reports the number of rows removed
#' due to missing values.
#'
#' @param formula A model formula (e.g., `Y ~ X1 + X2`).
#' @param data A data frame containing the variables in the model.
#'
#' @return A list containing:
#' \item{coefficients}{A data frame with estimates, standard errors, t-values, and p-values.}
#' \item{r_squared}{Model R-squared.}
#' \item{adj_r_squared}{Adjusted R-squared.}
#' \item{f_statistic}{Overall F-statistic for the model.}
#' \item{f_p.value}{p-value associated with the F-statistic.}
#' \item{n_removed}{Number of rows removed due to missing data.}
#' \item{n_used}{Number of rows used for fitting.}
#'
#' @details
#' This function performs a complete-case analysis by removing all observations
#' containing missing values (`NA`). It then fits the model using direct matrix algebra
#' for clarity and efficiency. Results are equivalent to base R's `lm()`.
#'
#' @importFrom stats model.frame model.matrix model.response na.omit pf pt
#' @examples
#' model <- fit_linear(mpg ~ wt + hp + disp, data = mtcars)
#' model$coefficients
#' @export
fit_linear <- function(formula, data) {
  # Remove missing rows
  n_before <- nrow(data)
  data <- na.omit(data)
  n_after <- nrow(data)
  n_removed <- n_before - n_after
  if (n_removed > 0) message(n_removed, " rows removed due to missing values.")

  # Create model matrix and response
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  X <- model.matrix(formula, data)

  n <- nrow(X)
  p <- ncol(X)

  # Compute coefficients: (X'X)^-1 X'y
  XtX_inv <- solve(t(X) %*% X)
  beta <- XtX_inv %*% t(X) %*% y
  fitted <- X %*% beta
  residuals <- y - fitted

  # Model statistics
  df_resid <- n - p
  sse <- sum(residuals^2)
  sst <- sum((y - mean(y))^2)
  sigma2 <- sse / df_resid

  # Standard errors
  se <- sqrt(diag(sigma2 * XtX_inv))
  t_val <- beta / se
  p_val <- 2 * pt(-abs(t_val), df_resid)

  # R^2 and adjusted R^2
  r2 <- 1 - sse / sst
  adj_r2 <- 1 - (1 - r2) * ((n - 1) / df_resid)

  # F-statistic
  msr <- (sst - sse) / (p - 1)
  mse <- sse / df_resid
  f_stat <- msr / mse
  f_p <- 1 - pf(f_stat, p - 1, df_resid)

  coef_df <- data.frame(
    Estimate = as.vector(beta),
    Std.Error = se,
    t.value = as.vector(t_val),
    p.value = as.vector(p_val),
    row.names = colnames(X)
  )

  list(
    coefficients = coef_df,
    r.squared = r2,
    adj.r.squared = adj_r2,
    f.statistic = f_stat,
    f.p.value = f_p,
    n_removed = n_removed,
    n_used = n_after,
    fitted.values = as.vector(fitted),
    residuals = as.vector(residuals)
  )
}

