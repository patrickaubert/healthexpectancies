# Projects prevalences of disability and calculates life expectancies

Estimates a model of the evolution of prevalences with
[`fitPrevalenceModel`](https://patrickaubert.github.io/healthexpectancies/reference/fitPrevalenceModel.md),
projects prevalences with
[`predict.prevalence_model`](https://patrickaubert.github.io/healthexpectancies/reference/predict.prevalence_model.md)
and calculates life expectancies with the Sullivan method
([`CompleteDFLEtable`](https://patrickaubert.github.io/healthexpectancies/reference/CompleteDFLEtable.md)),
in a single step. The estimated model is kept as an attribute of the
result.

## Usage

``` r
projectPrevalence(
  tab,
  newdata,
  method = c("trend", "mortality", "target", "shift"),
  on_unfeasible = c("warn", "clip", "error"),
  ...
)
```

## Arguments

- tab:

  observed prevalences by age and year (see
  [`fitPrevalenceModel`](https://patrickaubert.github.io/healthexpectancies/reference/fitPrevalenceModel.md))

- newdata:

  projection years: data frame with variables 'year', 'age' and 'qx'
  (mortality rate). Mortality rates must cover all the ages up to the
  last age of the life table (where qx = 1), as required by
  [`CompleteDFLEtable`](https://patrickaubert.github.io/healthexpectancies/reference/CompleteDFLEtable.md).
  The model must also be estimated on all these ages (otherwise, a
  warning is issued and the life table stops at the last age of the
  model).

- method:

  "trend", "mortality", "target" or "shift" (see
  [`fitPrevalenceModel`](https://patrickaubert.github.io/healthexpectancies/reference/fitPrevalenceModel.md))

- on_unfeasible:

  method "target" only: what to do if the hypothesis cannot be satisfied
  (see
  [`predict.prevalence_model`](https://patrickaubert.github.io/healthexpectancies/reference/predict.prevalence_model.md))

- ...:

  other arguments of
  [`fitPrevalenceModel`](https://patrickaubert.github.io/healthexpectancies/reference/fitPrevalenceModel.md)
  (fit_years, exclude_years, categories, scale, etc.)

## Value

a data frame with prevalences ('pix'), life expectancies and other
indicators calculated by
[`CompleteDFLEtable`](https://patrickaubert.github.io/healthexpectancies/reference/CompleteDFLEtable.md).
The model is available with `attr(result, "model")`.

## See also

[`fitPrevalenceModel`](https://patrickaubert.github.io/healthexpectancies/reference/fitPrevalenceModel.md),
[`predict.prevalence_model`](https://patrickaubert.github.io/healthexpectancies/reference/predict.prevalence_model.md),
[`prevalenceForecast`](https://patrickaubert.github.io/healthexpectancies/reference/prevalenceForecast.md)

## Examples

``` r
# prevalences of the APA by single year of age, 2010-2024 (smoothing of the 6 age brackets, weighted by the population)
apa <- FRAPAprevalence[FRAPAprevalence$decomp == 6, c("year", "age", "prevalence")]
pop <- aggregate(popx ~ year + age0101, FRInseePopulation[FRInseePopulation$geo == "france" & FRInseePopulation$age0101 %in% 60:99, ], sum)
names(pop) <- c("year", "age", "popx")
pix <- ApproxPrevalenceTable(apa, agecuts = seq(60, 85, 5), agemin = 60, agemax = 99, weights.tab = pop, categories = "year")
#> 'prevalence' is used as prevalence variable.
pix <- data.frame(year = pix$year, age = pix$age, pix = pix$prevalence.approx)
# extension up to age 104 (prevalence constant after age 99)
pix <- rbind(pix, do.call(rbind, lapply(100:104, function(a) transform(pix[pix$age == 99, ], age = a))))

# mortality rates (Insee projections), the last age (104) having a mortality rate of 1
mort <- FRInseeMortalityForecast2026
mort <- mort[mort$sex == "all" & mort$geo == "France" & mort$def.age == "current age (approx)" &
             mort$year %in% c(2025, 2030) & mort$age %in% 60:104, c("year", "age", "qx")]
mort$qx[mort$age == 104] <- 1

# trend of prevalences at each age, estimated on 2010-2019, then projection and life expectancies
res <- projectPrevalence(pix, newdata = mort, method = "trend", fit_years = 2010:2019)
res[res$age == 60, c("year", "age", "pix", "ex", "DLEx", "pctDLEx")]
#>    year age         pix       ex     DLEx  pctDLEx
#> 1  2025  60 0.002573981 25.95770 2.397822 9.237422
#> 46 2030  60 0.002428811 26.31772 2.350002 8.929353
attr(res, "model")                  # the estimated model
#> Model of prevalences by age, method "trend" (scale: logit)
#>   Years used for the estimation: 2010 to 2019 (10 years)
#>   Series: 1
#>   Ages: 60 to 104
#>   Estimator: ols
#> Use coef(), as.data.frame(, what = ) and predict() to retrieve results and project prevalences.

# hypotheses on life expectancies need mortality rates in the estimation table too
mort_obs <- FRInseeMortalityrates
mort_obs <- mort_obs[mort_obs$sex == "all" & mort_obs$geo == "france" & !is.na(mort_obs$age) &
                     mort_obs$age %in% 60:104 & mort_obs$year %in% 2010:2019, c("year", "age", "qx")]
mort_obs$qx[mort_obs$age == 104] <- 1
tab <- merge(pix, mort_obs, by = c("year", "age"))

# uniform evolution of the prevalences at all ages, such that life expectancy with the APA at age 60
# follows its trend of 2010-2019
res2 <- projectPrevalence(tab, newdata = mort, method = "target", indicator = "dle", rule = "trend",
                          scope = "min_age", fit_years = 2010:2019)
res2[res2$age == 60, c("year", "age", "pix", "ex", "DLEx", "pctDLEx")]
#>    year age         pix       ex     DLEx  pctDLEx
#> 1  2025  60 0.002368359 25.95770 2.349853 9.052626
#> 46 2030  60 0.002158772 26.31772 2.296411 8.725720

# share of life expectancy lived with the APA, at each age, decreasing by 10 percent between 2019 and 2040.
# (at the youngest ages, where prevalences are close to 0, the hypothesis can require slightly negative
# prevalences: they are bounded to 0)
res3 <- suppressWarnings(projectPrevalence(tab, newdata = mort, method = "target", indicator = "pct_dle",
                         rule = "horizon", change = -10, horizon = 2040, on_unfeasible = "clip"))
res3[res3$age %in% c(60, 85), c("year", "age", "pix", "DLEx", "pctDLEx")]
#>    year age          pix     DLEx   pctDLEx
#> 1  2025  60 0.0005170823 2.370714  9.132991
#> 26 2025  85 0.1737367725 2.730443 36.204274
#> 46 2030  60 0.0000000000 2.345566  8.912495
#> 71 2030  85 0.1645435766 2.728838 35.316915
```
