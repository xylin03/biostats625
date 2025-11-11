#' @importFrom stats coef confint formula lm median na.omit pf predict sd
#' @title Fit a linear regression model after cleaning missing data
#' @description
#' The `linreg_fit()` function removes all rows with missing values from the dataset
#' and fits a linear regression model using the specified formula. It returns a list
#' containing the fitted model and key model statistics such as R-squared, adjusted
#' R-squared, F-statistic, p-value, and the coefficient table. The function also
#' reports the number of rows removed due to missing values.
#'
#' @param formula An object of class "formula" describing the model (e.g., Y ~ X1 + X2).
#' @param data A data frame containing the variables in the model.
#'
#' @return A list containing:
#' \item{model}{The fitted lm object.}
#' \item{summary}{A summary of the model including p-values, R-squared, and so on.}
#'
#' @details
#' This function first removes all observations containing missing values (\code{NA})
#' to ensure a complete-case analysis. It then fits a linear regression model using
#' the base R \code{lm()} function. The function prints a message indicating how many
#' rows were removed due to missing data. Key summary statistics are extracted from
#' the model summary to provide a concise description of model performance.
#'
#' @examples
#' model <- linreg_fit(mpg ~ wt + hp + disp, data = mtcars)
#' model$summary
#' @export
linreg_fit <- function(formula, data) {
  if (!inherits(formula, "formula")) stop("formula must be a valid formula object")
  if (!is.data.frame(data)) stop("data must be a data frame")

  # Remove rows with missing values
  n_before <- nrow(data)
  data_clean <- na.omit(data)
  n_after <- nrow(data_clean)
  n_removed <- n_before - n_after

  # Inform user about data cleaning
  if (n_removed > 0) {
    message(paste0("Removed ", n_removed, " rows containing missing values."))
  } else {
    message("No missing values detected. Using full dataset.")
  }

  # Fit the complete model
  model <- lm(formula, data = data_clean)
  s <- summary(model)

  output <- list(
    model = model,
    summary = list(
      coefficients = coef(s),
      r.squared = s$r.squared,
      adj.r.squared = s$adj.r.squared,
      f.statistic = s$fstatistic[1],
      p.value = pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE),
      residual.se = s$sigma
    )
  )

  class(output) <- "linreg_fit"
  return(output)
}

#' Interpret model significance
#'
#' @description
#' Provides a text-based interpretation of model significance based on R^2 and overall p-value.
#'
#' @param fit A linreg_fit object from linreg_fit().
#'
#' @return A character string interpreting model strength and significance.
#' @examples
#' model <- linreg_fit(mpg ~ wt + hp + disp, data = mtcars)
#' linreg_interpret(model)
#' @export
linreg_interpret <- function(fit) {
  if (!inherits(fit, "linreg_fit")) stop("fit must be a linreg_fit object")

  # Fetch R-squared and p-value
  r2 <- fit$summary$r.squared
  pval <- fit$summary$p.value

  # Check R_squared
  interpret_r2 <- if (r2 > 0.8) {
    "very strong"
  } else if (r2 > 0.5) {
    "moderate"
  } else if (r2 > 0.2) {
    "weak"
  } else {
    "very weak"
  }

  # check p-value with 0.05 confidence level
  sig_text <- if (pval < 0.05) "statistically significant" else "not statistically significant"

  message <- paste0(
    "The model explains ", round(r2 * 100, 1), "% of the variance in the response, ",
    "indicating a ", interpret_r2, " relationship. ",
    "The overall model is ", sig_text, " (p = ", signif(pval, 3), ")."
  )

  return(message)
}

#' Compute variable statistics
#'
#' @description
#' Calculates basic summary statistics for a numeric variable.
#'
#' @param x A numeric vector.
#'
#' @return A list with mean, median, standard deviation, min, and max.
#' @examples
#' data_info(mtcars$mpg)
#' @export
data_info <- function(x) {
  if (!is.numeric(x)) stop("x must be numeric")

  # find the descriptive data of interested variables
  result <- list(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )

  return(result)
}

#' Predict from fitted linear regression model
#'
#' @description
#' Uses the fitted model from linreg_fit() to predict Y values given new data.
#' This function can be used, for example, to predict a new patient's blood
#' pressure (BLP) using the fitted regression model.
#'
#' @param fit A linreg_fit object.
#' @param newdata A data frame containing predictor values for prediction.
#'   Must include all predictor variables used in the fitted model.
#'
#' @return A numeric vector of predicted values.
#' @examples
#' model <- linreg_fit(mpg ~ wt + hp + disp, data = mtcars)
#' new_data <- data.frame(wt = 2.5, hp = 110, disp = 109)
#' linreg_predict(model, new_data)

#' @export
linreg_predict <- function(fit, newdata) {
  # Check that the model object is from linreg_fit()
  if (!inherits(fit, "linreg_fit")) {
    stop("fit must be a linreg_fit object")
  }

  # Check that newdata is a data frame
  if (!is.data.frame(newdata)) {
    stop("newdata must be a data frame")
  }

  # Get variable names used in the model
  model_vars <- all.vars(formula(fit$model))[-1]  # exclude response
  missing_vars <- setdiff(model_vars, names(newdata))
  if (length(missing_vars) > 0) {
    stop("Missing predictors in newdata: ", paste(missing_vars, collapse = ", "))
  }

  # Predict values
  preds <- predict(fit$model, newdata)

  return(preds)
}

#' Confidence Interval Calculator
#'
#' @description
#' Calculates 95% confidence intervals for model coefficients.
#'
#' @param fit A linreg_fit object.
#'
#' @return A data frame containing coefficient estimates, lower, and upper 95% CI.
#' @examples
#' model <- linreg_fit(mpg ~ wt + hp + disp, data = mtcars)
#' CI_calculator(model)
#' @export
CI_calculator <- function(fit) {
  if (!inherits(fit, "linreg_fit")) stop("fit must be a linreg_fit object")

  ci <- confint(fit$model, level = 0.95)
  return(as.data.frame(ci))
}
