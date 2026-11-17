# Projects prevalences of disability with an estimated model

Calculates prevalences by age and year from a model estimated with
[`fitPrevalenceModel`](https://patrickaubert.github.io/healthexpectancies/reference/fitPrevalenceModel.md).

## Usage

``` r
# S3 method for class 'prevalence_model'
predict(
  object,
  newdata,
  interval = FALSE,
  level = 0.95,
  on_unfeasible = c("warn", "clip", "error"),
  ...
)
```

## Arguments

- object:

  a model estimated with
  [`fitPrevalenceModel`](https://patrickaubert.github.io/healthexpectancies/reference/fitPrevalenceModel.md)

- newdata:

  a data frame with variables 'year', 'age' (+ 'qx' for method
  "mortality") and, optionally, the variables defining series

- interval:

  if TRUE, adds the lower and upper bounds of a prediction interval
  ('pix_lwr' and 'pix_upr')

- level:

  confidence level of the prediction interval

- on_unfeasible:

  method "target" only: what to do if the hypothesis cannot be satisfied
  with the projected mortality, i.e. if it requires prevalences outside
  the interval 0 to 1 (all ages) or if no uniform evolution can reach
  the target (minimum age). "warn" (default) returns the values as they
  are with a warning (prevalences outside the interval 0 to 1, or NA for
  a uniform evolution without solution); "clip" bounds prevalences to
  the interval 0 to 1 (or to the closest feasible solution), with a
  warning; "error" stops.

- ...:

  not used

## Value

the rows of `newdata` with a new variable 'pix' (projected prevalence),
and 'pix_lwr' and 'pix_upr' if requested. If the scale of the model is
"log" or "level", prevalences are bounded to the interval 0 to 1 (with a
warning).

## Details

With methods "trend" and "shift", `newdata` only needs the years and
ages. With method "mortality", `newdata` must also contain the mortality
rates 'qx' for all the ages of the model and for each year (they are
used at ages \\a+k\\, see
[`fitPrevalenceModel`](https://patrickaubert.github.io/healthexpectancies/reference/fitPrevalenceModel.md)).
Variables defining series (sex, categories) can be omitted from
`newdata`: it is then used for all the values of these variables in the
model (for example, mortality rates by sex apply to all categories).
Rows of `newdata` with ages that are not in the model are dropped.

Prediction intervals (with `interval = TRUE`) are calculated on the
scale of the estimation, from the residual variance of the regression,
and then transformed back to prevalences. They do not take into account
the uncertainty on the projected mortality rates, nor the uncertainty on
the validity of the model outside the period of estimation. They are not
available for estimator = "endpoints", nor for methods "target" and
"shift".

## See also

[`fitPrevalenceModel`](https://patrickaubert.github.io/healthexpectancies/reference/fitPrevalenceModel.md),
[`projectPrevalence`](https://patrickaubert.github.io/healthexpectancies/reference/projectPrevalence.md)

## Examples

``` r
ages <- 60:100; years <- 2010:2019
obs <- expand.grid(age = ages, year = years)
obs$pix <- plogis(-6 + 0.12 * (obs$age - 60) - (0.01 + 0.0005 * (obs$age - 60)) * (obs$year - 2019))
m <- fitPrevalenceModel(obs, method = "trend")
proj <- predict(m, newdata = expand.grid(age = ages, year = c(2025, 2030)), interval = TRUE)
head(proj)
#>   age year         pix     pix_lwr     pix_upr
#> 1  60 2025 0.002328964 0.002328964 0.002328964
#> 2  61 2025 0.002617277 0.002617277 0.002617277
#> 3  62 2025 0.002941177 0.002941177 0.002941177
#> 4  63 2025 0.003305027 0.003305027 0.003305027
#> 5  64 2025 0.003713722 0.003713722 0.003713722
#> 6  65 2025 0.004172744 0.004172744 0.004172744
```
