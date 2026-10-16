# healthexpectancies (development version)

## New functions

* `fitPrevalenceModel()` estimates a model of the evolution of prevalences of disability by age on past observations,
  and `predict()` projects prevalences from it. Two methods: extrapolation of the trend of prevalences at each age
  (`method = "trend"`), link with the evolution of mortality rates, with an age shift between disability and death
  that can be selected on a grid (`method = "mortality"`), and hypotheses on life expectancies (`method = "target"`):
  constant, linear trend or change of x percent at a given horizon of life expectancy with disability, disability-free
  life expectancy, their share in total life expectancy or their ratio, at each age (inversion of the Sullivan method)
  or uniformly at all ages from the value at the minimum age. At the youngest ages, where the inversion is unstable,
  `from_age` applies the hypothesis from a given age only.
* `fitPrevalenceModel(method = "shift")`: horizontal shift of the curve of prevalences by age at a constant speed
  (in years of age per calendar year). The years used for the estimation are set with `fit_years` and
  `exclude_years`. The results of the estimation are available with `coef()`, `fitted()`, `residuals()`, `summary()` and
  `as.data.frame(what = ...)`.
* `projectPrevalence()` estimates, projects and calculates life expectancies in one step.
* New dataset `FRAPAprevalence`: prevalence of the APA by age bracket, 2010-2024 (source: DREES, via package `asdep`).

## Changes

* `prevalenceApprox()` and `ApproxPrevalenceTable()` now smooth prevalences by age on the logit scale, which guarantees
  prevalences between 0 and 1 and removes a strong underestimation of prevalences at the first ages (more than 50 percent at the
  minimum age with the former method). Use `method = "legacy"` (`option = "legacy"` in `ApproxPrevalenceTable()`) to get earlier results.
  Prevalences must now be proportions between 0 and 1 (not percentages) with the default method.
* `ApproxPrevalenceTable()` identifies age brackets from the variable 'age' (numbers or labels such as "[60,65)"), sorts rows
  by age, checks them against `agecuts` and the weights against the range of ages, and returns explicit error messages that name the series concerned.
* `prevalenceForecast()` checks its arguments and has a new argument `on_unfeasible` that sets what to do when a hypothesis
  would require prevalences outside the interval 0 to 1 ("warn", "clip" or "error").
* `CompleteDFLEtable()`: missing values are handled group by group, with a single warning (columns entirely `NA` are treated as absent);
  the share of life expectancy lived at age 0 only uses the 0.2 coefficient of the Sullivan manual at age 0;
  variables defining categories are restored in all cases.
* Packages `matlib`, `shiny`, `shinydashboard` and `plotly` are no longer required to install the package;
  `tidyr` and `stringr`, used by the code, are now declared.
* Tests (`testthat`) have been added.

## Bug fixes

* Functions failed in a session where the tidyverse was not attached: imports are now declared.
* `prevalenceForecast()` with hypothesis `cstPctDLE` did not work.
* `CompleteDFLEtable()`: mean, median and modal ages of disability returned `Inf` (with a warning) when prevalences were negative; they are now `NA`.
