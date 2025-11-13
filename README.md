biostats625
================

<!-- badges: start -->

[![R-CMD-check](https://github.com/xylin03/biostats625/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/xylin03/biostats625/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/xylin03/biostats625/graph/badge.svg)](https://app.codecov.io/gh/xylin03/biostats625)
<!-- badges: end -->

## Overview

biostats625 provides a set of tools for performing ordinary least
squares (OLS) linear regression in a simple and transparent way.
Currently, it includes two main functions:

- `fit_linear()` Fits a linear regression model after removing missing
  values. Returns coefficients, R-squared, adjusted R-squared,
  F-statistic, p-values, residuals, and fitted values.
- `linreg_predict()` Predicts responses for new data using a model
  fitted by fit_linear().

These functions are designed to reproduce the behavior of base R’s
`lm()` and `predict.lm()` while being simple enough for educational use.
You can learn more about them in `vignette("linreg-vignette")`.

## Installation

``` r
install.packages("devtools") # if you don't have devtools
devtools::install_github("xylin03/biostats625")
```

## Usage

``` r
# First, load the package
library(biostats625)

# Fit a linear regression model
# Example with mtcars dataset
model <- fit_linear(mpg ~ wt + hp + disp, data = mtcars)

# View coefficients
model$coefficients
##                  Estimate  Std.Error     t.value      p.value
## (Intercept) 37.1055052690 2.11081525 17.57875558 1.161936e-16
## wt          -3.8008905826 1.06619064 -3.56492586 1.330991e-03
## hp          -0.0311565508 0.01143579 -2.72447633 1.097103e-02
## disp        -0.0009370091 0.01034974 -0.09053451 9.285070e-01

# Check R-squared
model$r.squared
## [1] 0.8268361

# Predict on new dataWriting
new_data <- data.frame(wt = c(2.5, 3.0), hp = c(110, 150), disp = c(109, 160))
preds <- predict_linear(model, new_data)
preds
## [1] 24.07392 20.87943
```

## License

This package is released under the MIT License.
