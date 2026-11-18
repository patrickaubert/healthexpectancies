# Changelog

## healthexpectancies 0.1.1

Correction on errors in datasets (FRDreesAPA) or documentation of
datasets (FRInseePopulationForecast2021 and
FRInseePopulationForecast2026)

## healthexpectancies 0.1.0

First version with a NEWS file. It lists the changes since version
0.0.0.9003 (November 2021).

### New functions

- [`fitPrevalenceModel()`](https://patrickaubert.github.io/healthexpectancies/reference/fitPrevalenceModel.md)
  estimates a model of the evolution of prevalences of disability by age
  on past observations, and
  [`predict()`](https://rdrr.io/r/stats/predict.html) projects
  prevalences from it. Three methods: extrapolation of the trend of
  prevalences at each age (`method = "trend"`), link with the evolution
  of mortality rates, with an age shift between disability and death
  that can be selected on a grid (`method = "mortality"`), and
  hypotheses on life expectancies (`method = "target"`): constant,
  linear trend or change of x percent at a given horizon of life
  expectancy with disability, disability-free life expectancy, their
  share in total life expectancy or their ratio, at each age (inversion
  of the Sullivan method) or uniformly at all ages from the value at one
  age (the minimum age, or `anchor_age`), with a common shift of the
  logit of prevalences or a common factor on prevalences
  (`uniform_scale`). At the youngest ages, where the inversion is
  unstable, `from_age` applies the hypothesis from a given age only.
- `fitPrevalenceModel(method = "shift")`: horizontal shift of the curve
  of prevalences by age at a constant speed (in years of age per
  calendar year). The years used for the estimation are set with
  `fit_years` and `exclude_years`. The results of the estimation are
  available with [`coef()`](https://rdrr.io/r/stats/coef.html),
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html),
  [`residuals()`](https://rdrr.io/r/stats/residuals.html),
  [`summary()`](https://rdrr.io/r/base/summary.html) and
  `as.data.frame(what = ...)`.
- [`projectPrevalence()`](https://patrickaubert.github.io/healthexpectancies/reference/projectPrevalence.md)
  estimates, projects and calculates life expectancies in one step.

### Datasets

- New dataset `FRAPAprevalence`: prevalence of the APA by age bracket,
  2010-2024 (source: DREES, via package `asdep`).
- New datasets of Insee population projections:
  `FRInseePopulationForecast2021` and `FRInseePopulationForecast2026`
  (population by sex and age, central scenario, with observed values for
  past years).
- New datasets of Insee mortality projections:
  `FRInseeMortalityForecast2021` (observed values up to 2020,
  projections up to 2120) and `FRInseeMortalityForecast2026` (observed
  values up to 2022, projections up to 2125).
- New dataset `FRInseeMortalityrates_t69`: observed mortality rates by
  sex and age from Insee’s table T69, as an alternative to
  `FRInseeMortalityrates`.
- New dataset `FRDreesVQSsurvey2021`: prevalences of disabilities by sex
  and age bracket from the 2021 *Vie quotidienne et santé* survey.
- New dataset `FRGaliEUSilc`: prevalence of general activity limitations
  (GALI) by sex and age bracket, from the SRCV survey (French part of
  EU-SILC), updated up to 2024.
- New dataset `FRDreesEHPA`: share of people living in institutions for
  older people by sex and age, from the EHPA surveys (2007 to 2023).
- Updated datasets: `FRDreesAPA` (years 2016 to 2024, by sex and age),
  `FRInseePopulation` and `FRInseeMortalityrates` (versions of January
  2026). Error corrected in `FRInseeMortalityForecast2026`.

### Changes

- [`prevalenceApprox()`](https://patrickaubert.github.io/healthexpectancies/reference/prevalenceApprox.md)
  and
  [`ApproxPrevalenceTable()`](https://patrickaubert.github.io/healthexpectancies/reference/ApproxPrevalenceTable.md)
  now smooth prevalences by age on the logit scale, which guarantees
  prevalences between 0 and 1 and removes a strong underestimation of
  prevalences at the first ages (more than 50 percent at the minimum age
  with the former method). Use `method = "legacy"` (`option = "legacy"`
  in
  [`ApproxPrevalenceTable()`](https://patrickaubert.github.io/healthexpectancies/reference/ApproxPrevalenceTable.md))
  to get earlier results. Prevalences must now be proportions between 0
  and 1 (not percentages) with the default method.
- [`ApproxPrevalenceTable()`](https://patrickaubert.github.io/healthexpectancies/reference/ApproxPrevalenceTable.md)
  identifies age brackets from the variable ‘age’ (numbers or labels
  such as “\[60,65)”), sorts rows by age, checks them against `agecuts`
  and the weights against the range of ages, and returns explicit error
  messages that name the series concerned.
- [`prevalenceForecast()`](https://patrickaubert.github.io/healthexpectancies/reference/prevalenceForecast.md)
  checks its arguments and has a new argument `on_unfeasible` that sets
  what to do when a hypothesis would require prevalences outside the
  interval 0 to 1 (“warn”, “clip” or “error”).
- [`CompleteDFLEtable()`](https://patrickaubert.github.io/healthexpectancies/reference/CompleteDFLEtable.md):
  missing values are handled group by group, with a single warning
  (columns entirely `NA` are treated as absent); the share of life
  expectancy lived at age 0 only uses the 0.2 coefficient of the
  Sullivan manual at age 0; variables defining categories are restored
  in all cases.
- Packages `matlib`, `shiny`, `shinydashboard` and `plotly` are no
  longer required to install the package; `tidyr` and `stringr`, used by
  the code, are now declared.
- Tests (`testthat`) have been added.
- [`projectPrevalence()`](https://patrickaubert.github.io/healthexpectancies/reference/projectPrevalence.md)
  warns when the model does not cover all the ages of the life table
  given in `newdata` (life expectancies would then be calculated with a
  life table that stops at the last age of the model).
- Shiny app
  ([`runExample()`](https://patrickaubert.github.io/healthexpectancies/reference/runExample.md)):
  prevalences by single year of age are now smoothed with
  [`prevalenceApprox()`](https://patrickaubert.github.io/healthexpectancies/reference/prevalenceApprox.md)
  (logit of prevalences) instead of a polynomial approximation, which
  could give negative prevalences; hypotheses that would require
  prevalences outside the interval 0 to 1 are bounded
  (`on_unfeasible = "clip"`).
- The package passes `R CMD check` without errors or warnings (non-ASCII
  characters removed from the code, variables of dplyr pipelines
  declared, files ignored when building the package).

### Documentation

- New vignette “Methods for projecting prevalences” describing the
  methods of
  [`fitPrevalenceModel()`](https://patrickaubert.github.io/healthexpectancies/reference/fitPrevalenceModel.md),
  with an application to the APA.
- The vignette “how-to-use-healthexpectancies” is replaced by “Getting
  started with healthexpectancies”, a tutorial with executed examples of
  the main functions and an overview of the datasets.
- README and pkgdown website updated.
- Citation: `citation("healthexpectancies")` (file `inst/CITATION`) and
  `CITATION.cff` file for GitHub and Zenodo.

### Bug fixes

- Functions failed in a session where the tidyverse was not attached:
  imports are now declared.
- [`prevalenceForecast()`](https://patrickaubert.github.io/healthexpectancies/reference/prevalenceForecast.md)
  with hypothesis `cstPctDLE` did not work.
- Shiny app
  ([`runExample()`](https://patrickaubert.github.io/healthexpectancies/reference/runExample.md)):
  the default hypothesis (constant prevalences) stopped with an error,
  since it used an obsolete name of the hypothesis.
- Examples of
  [`prevalenceApprox()`](https://patrickaubert.github.io/healthexpectancies/reference/prevalenceApprox.md)
  failed when dplyr was not attached; they now use base R only.
- [`prevalence_to_polynomial()`](https://patrickaubert.github.io/healthexpectancies/reference/prevalence_to_polynomial.md):
  typo (`False` instead of `FALSE`) in the expansion of prevalences by
  age bracket.
- Documentation of the dataset `sullivan`: all variables are now
  described.
- [`CompleteDFLEtable()`](https://patrickaubert.github.io/healthexpectancies/reference/CompleteDFLEtable.md):
  mean, median and modal ages of disability returned `Inf` (with a
  warning) when prevalences were negative; they are now `NA`.
