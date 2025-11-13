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
#' @importFrom stats model.frame model.matrix model.response na.omit pf pt terms
#' @examples
#' model <- fit_linear(mpg ~ wt + hp + disp, data = mtcars)
#' model$coefficients
#' @export
fit_linear <- function(formula, data) {
  # Remove rows with missing values
  data_complete <- na.omit(data)
  n_used <- nrow(data_complete)
  n_removed <- nrow(data) - n_used

  # Create model matrix
  X <- model.matrix(formula, data_complete)
  y <- model.response(model.frame(formula, data_complete))

  # Fit model using normal equations
  beta_hat <- solve(t(X) %*% X, t(X) %*% y)

  # Predicted values and residuals
  fitted <- X %*% beta_hat
  residuals <- y - fitted

  # Degrees of freedom
  n <- length(y)
  p <- ncol(X)
  df_resid <- n - p

  # Estimate sigma^2
  sigma2 <- sum(residuals^2) / df_resid

  # Variance-covariance matrix and SEs
  var_beta <- sigma2 * solve(t(X) %*% X)
  se_beta <- sqrt(diag(var_beta))

  # t-values and p-values
  t_values <- beta_hat / se_beta
  p_values <- 2 * pt(-abs(t_values), df = df_resid)

  # R-squared and adjusted R-squared
  ss_total <- sum((y - mean(y))^2)
  ss_res <- sum(residuals^2)
  r2 <- 1 - ss_res / ss_total
  adj_r2 <- 1 - (1 - r2) * ((n - 1) / df_resid)

  # F-statistic and p-value
  msr <- (ss_total - ss_res) / (p - 1)
  mse <- ss_res / df_resid
  f_stat <- msr / mse
  f_p <- pf(f_stat, p - 1, df_resid, lower.tail = FALSE)

  # Coefficient table
  coef_df <- data.frame(
    Estimate = as.numeric(beta_hat),
    Std.Error = se_beta,
    t.value = as.numeric(t_values),
    p.value = p_values,
    row.names = colnames(X)
  )

  # Create terms object
  terms_obj <- terms(formula, data = data_complete)

  # Return full structured list
  model <- list(
    formula = formula,
    terms = terms_obj,
    coefficients = coef_df,
    r.squared = r2,
    adj.r.squared = adj_r2,
    f.statistic = f_stat,
    f.p.value = f_p,
    n_removed = n_removed,
    n_used = n_used,
    fitted.values = as.vector(fitted),
    residuals = as.vector(residuals)
  )

  class(model) <- "fit_linear"
  return(model)
}


