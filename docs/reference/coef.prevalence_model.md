# Results of the estimation of a model of prevalences

Methods to retrieve and display the results of a model estimated with
[`fitPrevalenceModel`](https://patrickaubert.github.io/healthexpectancies/reference/fitPrevalenceModel.md).

## Usage

``` r
# S3 method for class 'prevalence_model'
coef(object, ...)

# S3 method for class 'prevalence_model'
fitted(object, ...)

# S3 method for class 'prevalence_model'
residuals(object, ...)

# S3 method for class 'prevalence_model'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  what = c("coefficients", "fitted", "observed", "fit_stats", "lag_search", "shifts"),
  ...
)

# S3 method for class 'prevalence_model'
print(x, ...)

# S3 method for class 'prevalence_model'
summary(object, ...)

# S3 method for class 'summary.prevalence_model'
print(x, ...)
```

## Arguments

- object, x:

  a model estimated with
  [`fitPrevalenceModel`](https://patrickaubert.github.io/healthexpectancies/reference/fitPrevalenceModel.md)

- ...:

  not used

- row.names, optional:

  not used

- what:

  table to be returned by
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) (see
  above)

## Value

a data frame, a vector, or (for summary) a list of class
"summary.prevalence_model"

## Details

- [`coef()`](https://rdrr.io/r/stats/coef.html) returns the coefficients
  in a data frame (long format), one row per series, age and term:
  "intercept" (value of the transformed prevalence at the last year of
  the estimation) and "slope" (annual change) for method "trend";
  "alpha" (age effects) and "beta" (slope on the mortality rates, with
  age = NA if it is common to all ages) for method "mortality". For
  method "shift", "shift" (speed of the horizontal shift of the curve of
  prevalences, in years of age per calendar year, with age = NA). For
  method "target", "reference" (value of the indicator at the reference
  year) for rules "constant" and "horizon", and "intercept" and "slope"
  (value and annual change of the indicator) for rule "trend"; with
  scope "min_age", only the minimum age (or 'anchor_age') is concerned.
  Columns: variables defining series, 'age', 'term', 'estimate',
  'std.error', 'n'.

- [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) and
  [`residuals()`](https://rdrr.io/r/stats/residuals.html) return fitted
  prevalences and residuals (on the scale of the estimation) as vectors,
  in the order of the rows of `as.data.frame(x, what = "fitted")`.

- [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
  one of the tables of the model, selected by `what`: "coefficients",
  "fitted" (observed and fitted prevalences and residuals by year and
  age), "observed" (data used for the estimation), "fit_stats" (number
  of observations, residual standard deviation and R-squared for each
  series) or "lag_search" (sum of squared residuals for each value of
  'age_lag' tried, for method "mortality" when 'age_lag' is a vector) or
  "shifts" (for method "shift": shift of the curve of prevalences
  estimated for each year, 'shift_obs', and shift predicted by the
  model, 'shift_fit'). These tables can be used directly to draw
  graphics.

- [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`print()`](https://rdrr.io/r/base/print.html) display a short
  description of the model.

## See also

[`fitPrevalenceModel`](https://patrickaubert.github.io/healthexpectancies/reference/fitPrevalenceModel.md),
[`predict.prevalence_model`](https://patrickaubert.github.io/healthexpectancies/reference/predict.prevalence_model.md)

## Examples

``` r
ages <- 60:100; years <- 2010:2019
obs <- expand.grid(age = ages, year = years)
obs$pix <- plogis(-6 + 0.12 * (obs$age - 60) - (0.01 + 0.0005 * (obs$age - 60)) * (obs$year - 2019))
m <- fitPrevalenceModel(obs, method = "trend")
summary(m)
#> Model of prevalences by age, method "trend" (scale: logit)
#> Years used for the estimation: 2010 to 2019  
#> 
#> Fit statistics by series (sigma: residual standard deviation on the scale of the estimation):
#>  n_obs n_ages n_years     sigma r2
#>    410     41      10 3.291e-16  1
head(coef(m))
#>   age      term estimate    std.error  n
#> 1  60 intercept    -6.00 2.610151e-16 10
#> 2  61 intercept    -5.88 0.000000e+00 10
#> 3  62 intercept    -5.76 0.000000e+00 10
#> 4  63 intercept    -5.64 0.000000e+00 10
#> 5  64 intercept    -5.52 0.000000e+00 10
#> 6  65 intercept    -5.40 5.536967e-16 10
head(as.data.frame(m, what = "fitted"))
#>   year age         pix     pix_fit         resid
#> 1 2010  60 0.002704851 0.002704851  0.000000e+00
#> 2 2010  61 0.003062367 0.003062367  0.000000e+00
#> 3 2010  62 0.003466974 0.003466974  0.000000e+00
#> 4 2010  63 0.003924829 0.003924829  0.000000e+00
#> 5 2010  64 0.004442879 0.004442879  0.000000e+00
#> 6 2010  65 0.005028962 0.005028962 -8.881784e-16
```
