# Estimates a model of the evolution of prevalences of disability by age

Estimates, on past observations of prevalences of disability by age, a
model that can then be used to project prevalences (see
[`predict.prevalence_model`](https://patrickaubert.github.io/healthexpectancies/reference/predict.prevalence_model.md)).
Four methods are available.

## Usage

``` r
fitPrevalenceModel(
  tab,
  method = c("trend", "mortality", "target", "shift"),
  fit_years = NULL,
  exclude_years = NULL,
  categories = NULL,
  scale = c("logit", "log", "level"),
  estimator = c("ols", "endpoints"),
  slope = c("common", "by_age"),
  age_lag = 0,
  eps = 1e-06,
  indicator = c("dle", "dfle", "pct_dle", "pct_dfle", "ratio"),
  rule = c("constant", "trend", "horizon"),
  scope = c("all_ages", "min_age"),
  ref_year = NULL,
  change = NULL,
  horizon = NULL,
  after = c("linear", "constant"),
  anchor_age = NULL,
  uniform_scale = c("odds", "level"),
  from_age = NULL,
  n_extrap = 5
)
```

## Arguments

- tab:

  a data frame with variables 'year', 'age' (single years of age) and
  'pix' (prevalence), plus 'qx' (mortality rate) if method is
  "mortality" or "target". Optional variables 'sex' and 'categ', and the
  variables listed in 'categories', define separate series: one model is
  estimated for each series, and each series must contain the same ages
  for all years. Prevalences can be obtained for example with
  [`ApproxPrevalenceTable`](https://patrickaubert.github.io/healthexpectancies/reference/ApproxPrevalenceTable.md).

- method:

  "trend", "mortality", "target" or "shift" (see Details)

- fit_years:

  vector of the years used for the estimation (default: all years of
  'tab')

- exclude_years:

  vector of years to be left out of the estimation (for example the
  years of a crisis)

- categories:

  names of variables of 'tab' defining separate series (in addition to
  'sex' and 'categ' when they exist)

- scale:

  scale on which prevalences are modelled: "logit" (default, keeps
  projected prevalences between 0 and 1), "log" or "level" (prevalences
  themselves)

- estimator:

  method "trend", or method "target" with rule "trend": "ols" (default)
  or "endpoints" (slope between the first and the last years)

- slope:

  method "mortality" only: "common" (default) for a single slope
  \\\beta\\, or "by_age"

- age_lag:

  method "mortality" only: age shift \\k\\ (in years of age) between the
  age of prevalences and the age of the mortality rates (default 0). A
  vector selects the best value (see Details).

- eps:

  with scale "logit" or "log", prevalences below 'eps' (or, with
  "logit", above 1 - 'eps') are replaced by this bound, with a warning

- indicator:

  method "target" only: "dle", "dfle", "pct_dle", "pct_dfle" or "ratio"
  (see Details)

- rule:

  method "target" only: "constant", "trend" or "horizon" (see Details)

- scope:

  method "target" only: "all_ages" (a target at each age) or "min_age"
  (uniform evolution of prevalences, see Details)

- ref_year:

  methods "target" (rules "constant" and "horizon") and "shift":
  reference year (default: last year of the estimation)

- change:

  method "target", rule "horizon": relative change of the indicator, in
  percent, between the reference year and 'horizon'

- horizon:

  method "target", rule "horizon": year at which the change is reached

- after:

  method "target", rule "horizon": "linear" (default, the change goes on
  at the same pace after 'horizon') or "constant"

- anchor_age:

  method "target", scope "min_age": age at which the hypothesis is
  applied (default: minimum age)

- uniform_scale:

  method "target", scope "min_age": "odds" (default) or "level" (see
  Details)

- from_age:

  method "target", scope "all_ages": age from which the hypothesis is
  applied (default NULL: all ages, see Details)

- n_extrap:

  method "shift": number of ages used to extrapolate the curve beyond
  the first and the last ages (default 5)

## Value

an object of class "prevalence_model", a list containing in particular
the data frames 'coefficients' (one row per series, age and term; see
[`coef.prevalence_model`](https://patrickaubert.github.io/healthexpectancies/reference/coef.prevalence_model.md)),
'fitted', 'fit_stats' and 'observed'. Use
[`predict.prevalence_model`](https://patrickaubert.github.io/healthexpectancies/reference/predict.prevalence_model.md)
to project prevalences, and the other methods listed in
[`coef.prevalence_model`](https://patrickaubert.github.io/healthexpectancies/reference/coef.prevalence_model.md)
to retrieve the results of the estimation.

## Details

**Method "trend"** extrapolates prevalences at each age (separately).
Denoting \\p\_{a,t}\\ the prevalence at age \\a\\ and year \\t\\ and
\\g\\ the transformation defined by 'scale' (logit, log or identity):
\$\$g(p\_{a,t}) = \alpha_a + \beta_a (t - t_0) + \varepsilon\_{a,t}\$\$
where \\t_0\\ is the last year used for the estimation. With estimator =
"ols", coefficients are estimated by ordinary least squares over all the
years used for the estimation; with estimator = "endpoints", the slope
is calculated from the first and the last years only.

**Method "mortality"** links the evolution of prevalences to the
evolution of mortality rates \\q\_{a,t}\\. If disability occurs on
average \\k\\ years before death, a decrease of mortality at age \\a+k\\
should translate into a decrease of the prevalence of disability at age
\\a\\: \$\$g(p\_{a,t}) = \alpha_a + \beta \ln q\_{a+k,t} +
\varepsilon\_{a,t}\$\$ The intercepts \\\alpha_a\\ are specific to each
age. The slope \\\beta\\ is common to all ages if slope = "common" (the
default), or specific to each age (\\\beta_a\\) if slope = "by_age"
(more flexible, but noisier). The age shift \\k\\ is given by 'age_lag'
(in years of age, not necessarily an integer: \\\ln q\\ is interpolated
linearly between ages). If 'age_lag' is a vector, the value that
minimises the sum of squared residuals is retained (separately for each
series, see 'categories'). This is a descriptive criterion, not a
statistical test. For ages \\a\\ such that \\a+k\\ is higher than the
last age of the table, the mortality rate of the last age is used.

**Method "shift"** assumes that the curve of prevalences by age of a
reference year \\t_0\\ moves towards older ages at a constant speed
\\s\\ (in years of age per calendar year): \$\$\mathrm{logit}\\p\_{a,t}
= \mathrm{logit}\\p\_{a - s\\(t - t_0),\\t_0}\$\$ For each year \\t\\ of
the estimation, the shift \\d_t\\ that brings the curve of the reference
year closest to the curve of year \\t\\ is searched (least squares on
the logit of prevalences); \\s\\ is the slope of the regression of
\\d_t\\ on \\t - t_0\\, without intercept (since \\d\_{t_0} = 0\\). The
\\d_t\\ are available with `as.data.frame(model, what = "shifts")`.
Values between ages are interpolated linearly (on the logit scale), and
the curve is extrapolated linearly beyond the first and the last ages,
using the slope estimated on the 'n_extrap' first (or last) ages. This
is a simple hypothesis: a single speed is assumed for all ages, whereas
observed shifts are usually faster at some ages than at others, so that
the method can be a poor representation of observed evolutions.

**Method "target"** projects prevalences from a hypothesis on life
expectancies, with the Sullivan method (see also
[`prevalenceForecast`](https://patrickaubert.github.io/healthexpectancies/reference/prevalenceForecast.md)).
Denote EVI the life expectancy with disability at age \\a\\ and year
\\t\\, EVSI the disability-free life expectancy and \\e = EVI + EVSI\\
the total life expectancy. The hypothesis is made of three choices:

- the **indicator** : "dle" (EVI), "dfle" (EVSI), "pct_dle" (share of
  EVI in total life expectancy, in percent, \\100\\EVI/e\\), "pct_dfle"
  (share of EVSI) or "ratio" (EVSI/EVI). A linear evolution of "pct_dle"
  and of "pct_dfle" are equivalent hypotheses; this is not the case for
  a relative change (rule "horizon"), nor for "dle" and "dfle" (since
  \\e\\ does not evolve linearly).

- the **rule** giving the value of the indicator \\v\_{a,t}\\ in the
  projection years: "constant" (\\v\_{a,t} = v\_{a,t_0}\\, with \\t_0\\
  the reference year 'ref_year'); "trend" (linear extrapolation of the
  indicator at each age, estimated over the years of the estimation,
  with 'estimator'); "horizon" (the indicator changes by 'change'
  percent between the reference year and the year 'horizon', linearly in
  between: \\v\_{a,t} = v\_{a,t_0}\\\[1 + (change/100)\\(t -
  t_0)/(horizon - t_0)\]\\; after 'horizon' the change goes on linearly
  (after = "linear") or stops (after = "constant")).

- the **scope**: with scope = "all_ages", the rule is applied at each
  age, which gives a targeted EVI at each age; prevalences are the ones
  that give this EVI, given the projected mortality (inversion of the
  Sullivan method, as in
  [`prevalenceForecast`](https://patrickaubert.github.io/healthexpectancies/reference/prevalenceForecast.md)).
  At the youngest ages, where prevalences are very low, the inversion is
  unstable and can require negative prevalences. If 'from_age' is given,
  the hypothesis is only applied to the ages \\a \ge\\ 'from_age', and
  prevalences of younger ages follow the evolution of the odds at age
  'from_age': \\\mathrm{logit}\\p\_{a,t} = \mathrm{logit}\\p\_{a,t_0} +
  \[\mathrm{logit}\\p\_{a_0,t} - \mathrm{logit}\\p\_{a_0,t_0}\]\\ (the
  hypothesis is then not satisfied at ages below 'from_age'). With scope
  = "min_age", the rule is only applied at the minimum age (or at
  'anchor_age'), and prevalences of all ages evolve uniformly, from
  those of the reference year, so that the targeted EVI at this age is
  reached: the odds of prevalences are multiplied by the same factor at
  all ages (uniform_scale = "odds", prevalences always remain between 0
  and 1) or prevalences are multiplied by the same factor, with a
  maximum of 1 (uniform_scale = "level"). This is a theoretical
  hypothesis, as prevalences do not usually evolve in the same
  proportion at all ages.

With method "target", 'tab' must also contain mortality rates for all
the ages of the life table, up to its last age (consecutive years of
age), and the estimation does not transform prevalences ('scale' is not
used). Prevalences "fitted" by the model are the ones implied by the
hypothesis with the observed mortality of each year (for rule
"constant", they are the observed prevalences at the reference year);
residuals are differences of logits. A hypothesis may be impossible to
satisfy with the projected mortality: see argument 'on_unfeasible' of
[`predict.prevalence_model`](https://patrickaubert.github.io/healthexpectancies/reference/predict.prevalence_model.md).

## See also

[`predict.prevalence_model`](https://patrickaubert.github.io/healthexpectancies/reference/predict.prevalence_model.md),
[`projectPrevalence`](https://patrickaubert.github.io/healthexpectancies/reference/projectPrevalence.md),
[`prevalenceForecast`](https://patrickaubert.github.io/healthexpectancies/reference/prevalenceForecast.md)

## Examples

``` r
# prevalences by age (simulated) : the odds of disability decrease by 2 percent a year at age 80, 1 percent at age 60
ages <- 60:100; years <- 2010:2019
obs <- expand.grid(age = ages, year = years)
obs$pix <- plogis(-6 + 0.12 * (obs$age - 60) - (0.01 + 0.0005 * (obs$age - 60)) * (obs$year - 2019))

m <- fitPrevalenceModel(obs, method = "trend", fit_years = 2010:2019)
m
#> Model of prevalences by age, method "trend" (scale: logit)
#>   Years used for the estimation: 2010 to 2019 (10 years)
#>   Series: 1
#>   Ages: 60 to 100
#>   Estimator: ols
#> Use coef(), as.data.frame(, what = ) and predict() to retrieve results and project prevalences.
head(coef(m))                       # coefficients by age
#>   age      term estimate    std.error  n
#> 1  60 intercept    -6.00 2.610151e-16 10
#> 2  61 intercept    -5.88 0.000000e+00 10
#> 3  62 intercept    -5.76 0.000000e+00 10
#> 4  63 intercept    -5.64 0.000000e+00 10
#> 5  64 intercept    -5.52 0.000000e+00 10
#> 6  65 intercept    -5.40 5.536967e-16 10

# prevalences of the APA by single year of age, 2010-2024 (smoothing of the 6 age brackets, weighted by the population)
apa <- FRAPAprevalence[FRAPAprevalence$decomp == 6, c("year", "age", "prevalence")]
pop <- aggregate(popx ~ year + age0101, FRInseePopulation[FRInseePopulation$geo == "france" & FRInseePopulation$age0101 %in% 60:99, ], sum)
names(pop) <- c("year", "age", "popx")
pix <- ApproxPrevalenceTable(apa, agecuts = seq(60, 85, 5), agemin = 60, agemax = 99, weights.tab = pop, categories = "year")
#> 'prevalence' is used as prevalence variable.
pix <- data.frame(year = pix$year, age = pix$age, pix = pix$prevalence.approx)
# extension up to age 104 (prevalence constant after age 99)
pix <- rbind(pix, do.call(rbind, lapply(100:104, function(a) transform(pix[pix$age == 99, ], age = a))))

# link with mortality: the age shift between disability and death is searched between 0 and 6 years
mort <- FRInseeMortalityrates
mort <- mort[mort$sex == "all" & mort$geo == "france" & !is.na(mort$age) & mort$age %in% 60:104 &
             mort$year %in% 2010:2019, c("year", "age", "qx")]
mort$qx[mort$age == 104] <- 1
m2 <- fitPrevalenceModel(merge(pix, mort, by = c("year", "age")), method = "mortality",
                         fit_years = 2010:2019, age_lag = seq(0, 6, by = 0.5))
summary(m2)
#> Model of prevalences by age, method "mortality" (scale: logit)
#> Years used for the estimation: 2010 to 2019  
#> Age shift k: 0.5 
#> 
#> Slope on ln(q):
#>  estimate std.error
#>       1.6   0.07121
#> 
#> Fit statistics by series (sigma: residual standard deviation on the scale of the estimation):
#>  n_obs n_ages n_years   sigma     r2
#>    450     45      10 0.04026 0.5554
as.data.frame(m2, what = "lag_search")
#>    age_lag       rss selected
#> 1      0.0 0.6803797    FALSE
#> 2      0.5 0.6547633     TRUE
#> 3      1.0 0.6932980    FALSE
#> 4      1.5 0.6744808    FALSE
#> 5      2.0 0.7179977    FALSE
#> 6      2.5 0.7018281    FALSE
#> 7      3.0 0.7443353    FALSE
#> 8      3.5 0.7310730    FALSE
#> 9      4.0 0.7738706    FALSE
#> 10     4.5 0.7641225    FALSE
#> 11     5.0 0.8076077    FALSE
#> 12     5.5 0.8025072    FALSE
#> 13     6.0 0.8471726    FALSE

# horizontal shift of the curve of prevalences by age
m3 <- fitPrevalenceModel(pix, method = "shift", fit_years = 2010:2019)
coef(m3)                                  # years of age per calendar year
#>   age  term   estimate   std.error n
#> 1  NA shift 0.07008925 0.004306869 9
as.data.frame(m3, what = "shifts")
#>    year   shift_obs   shift_fit
#> 1  2010 -0.64939426 -0.63080329
#> 2  2011 -0.61264976 -0.56071404
#> 3  2012 -0.51574784 -0.49062478
#> 4  2013 -0.45553188 -0.42053553
#> 5  2014 -0.30314299 -0.35044627
#> 6  2015 -0.26962993 -0.28035702
#> 7  2016 -0.04293830 -0.21026776
#> 8  2017 -0.06282677 -0.14017851
#> 9  2018 -0.03756193 -0.07008925
#> 10 2019  0.00000000  0.00000000
```
