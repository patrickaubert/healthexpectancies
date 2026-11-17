# Package index

## Health expectancies (Sullivan method)

Calculation of life expectancies with and without disability from
mortality rates and prevalences of disability by age.

- [`CompleteDFLEtable()`](https://patrickaubert.github.io/healthexpectancies/reference/CompleteDFLEtable.md)
  : Complete a dataframe with useful variables for the calculation of
  disability-free life expectancies (DFLE)

## Prevalences by single year of age

Smoothing of prevalences observed by age bracket into prevalences by
single year of age.

- [`ApproxPrevalenceTable()`](https://patrickaubert.github.io/healthexpectancies/reference/ApproxPrevalenceTable.md)
  : Transforms a table with prevalences by age brackets into a table
  with (smoothed) prevalences by age
- [`prevalenceApprox()`](https://patrickaubert.github.io/healthexpectancies/reference/prevalenceApprox.md)
  : Estimates prevalences by age from values by age brackets
- [`prevalence_to_polynomial()`](https://patrickaubert.github.io/healthexpectancies/reference/prevalence_to_polynomial.md)
  : Estimates a polynomial approximation for prevalences by age

## Projection of prevalences

Projection of prevalences and health expectancies, under hypotheses on
life expectancies
([`prevalenceForecast()`](https://patrickaubert.github.io/healthexpectancies/reference/prevalenceForecast.md))
or with models estimated on past observations
([`fitPrevalenceModel()`](https://patrickaubert.github.io/healthexpectancies/reference/fitPrevalenceModel.md)).

- [`prevalenceForecast()`](https://patrickaubert.github.io/healthexpectancies/reference/prevalenceForecast.md)
  : Forecasts prevalences of disability and DFLEs
- [`fitPrevalenceModel()`](https://patrickaubert.github.io/healthexpectancies/reference/fitPrevalenceModel.md)
  : Estimates a model of the evolution of prevalences of disability by
  age
- [`predict(`*`<prevalence_model>`*`)`](https://patrickaubert.github.io/healthexpectancies/reference/predict.prevalence_model.md)
  : Projects prevalences of disability with an estimated model
- [`coef(`*`<prevalence_model>`*`)`](https://patrickaubert.github.io/healthexpectancies/reference/coef.prevalence_model.md)
  [`fitted(`*`<prevalence_model>`*`)`](https://patrickaubert.github.io/healthexpectancies/reference/coef.prevalence_model.md)
  [`residuals(`*`<prevalence_model>`*`)`](https://patrickaubert.github.io/healthexpectancies/reference/coef.prevalence_model.md)
  [`as.data.frame(`*`<prevalence_model>`*`)`](https://patrickaubert.github.io/healthexpectancies/reference/coef.prevalence_model.md)
  [`print(`*`<prevalence_model>`*`)`](https://patrickaubert.github.io/healthexpectancies/reference/coef.prevalence_model.md)
  [`summary(`*`<prevalence_model>`*`)`](https://patrickaubert.github.io/healthexpectancies/reference/coef.prevalence_model.md)
  [`print(`*`<summary.prevalence_model>`*`)`](https://patrickaubert.github.io/healthexpectancies/reference/coef.prevalence_model.md)
  : Results of the estimation of a model of prevalences
- [`projectPrevalence()`](https://patrickaubert.github.io/healthexpectancies/reference/projectPrevalence.md)
  : Projects prevalences of disability and calculates life expectancies

## Interactive example

- [`runExample()`](https://patrickaubert.github.io/healthexpectancies/reference/runExample.md)
  : Runs examples of uses of the package

## Datasets

Datasets provide examples for calculations. Apart from the example of
the Sullivan manual, they concern France: most are data provided by
Insee (the French national statistical institute) or DREES (the
statistical unit of the French Ministry for Health and Social affairs).

### Sullivan manual

- [`sullivan`](https://patrickaubert.github.io/healthexpectancies/reference/sullivan.md)
  : An example of calculation using Sullivan's method.
- [`description_sullivan`](https://patrickaubert.github.io/healthexpectancies/reference/description_sullivan.md)
  : A description of the variables in the dataset from Sullivan manual's
  calculation example.

### Prevalences

- [`FRAPAprevalence`](https://patrickaubert.github.io/healthexpectancies/reference/FRAPAprevalence.md)
  : Prevalence of the APA by age bracket, France, 2010-2024
- [`FRDreesAPA`](https://patrickaubert.github.io/healthexpectancies/reference/FRDreesAPA.md)
  : A dataset with share of APA beneficiaries in France
- [`FRDreesAPA2017`](https://patrickaubert.github.io/healthexpectancies/reference/FRDreesAPA2017.md)
  : A dataset with share of APA beneficiaries in France (December 2017)
- [`FRDreesVQSsurvey2014`](https://patrickaubert.github.io/healthexpectancies/reference/FRDreesVQSsurvey2014.md)
  : A dataset with disability prevalences in France (2014)
- [`FRDreesVQSsurvey2021`](https://patrickaubert.github.io/healthexpectancies/reference/FRDreesVQSsurvey2021.md)
  : A dataset with disability prevalences in France (2021)
- [`FRGaliEUSilc`](https://patrickaubert.github.io/healthexpectancies/reference/FRGaliEUSilc.md)
  : A dataset with prevalence of general activity limitations (GALI) in
  France
- [`FRDreesEHPA`](https://patrickaubert.github.io/healthexpectancies/reference/FRDreesEHPA.md)
  : A dataset with proportions on people living in institutions in
  France according to Drees's EHPA survey

### Population

- [`FRInseePopulation`](https://patrickaubert.github.io/healthexpectancies/reference/FRInseePopulation.md)
  : A dataset with population by sex and age, from Insee's 'bilan
  démographique' (annual population estimates)
- [`FRInseePopulationForecast2016`](https://patrickaubert.github.io/healthexpectancies/reference/FRInseePopulationForecast2016.md)
  : A dataset with population by sex and age, from Insee's 2016
  population forecast (central scenario)
- [`FRInseePopulationForecast2021`](https://patrickaubert.github.io/healthexpectancies/reference/FRInseePopulationForecast2021.md)
  : A dataset with population by sex and age, from Insee's 2021
  population forecast (central scenario)
- [`FRInseePopulationForecast2026`](https://patrickaubert.github.io/healthexpectancies/reference/FRInseePopulationForecast2026.md)
  : A dataset with population by sex and age, from Insee's 2026
  population forecast (central scenario)

### Mortality

- [`FRInseeMortalityrates`](https://patrickaubert.github.io/healthexpectancies/reference/FRInseeMortalityrates.md)
  : A dataset with observed mortality rates by sex and age, from Insee
- [`FRInseeMortalityrates_t69`](https://patrickaubert.github.io/healthexpectancies/reference/FRInseeMortalityrates_t69.md)
  : An (alternative) dataset with observed mortality rates by sex and
  age, from Insee
- [`FRInseeMortalityForecast2016`](https://patrickaubert.github.io/healthexpectancies/reference/FRInseeMortalityForecast2016.md)
  : A dataset with mortality rates from Insee's 2016 population forecast
  (central scenario)
- [`FRInseeMortalityForecast2021`](https://patrickaubert.github.io/healthexpectancies/reference/FRInseeMortalityForecast2021.md)
  : A dataset with mortality rates from Insee's 2021 population forecast
  (central scenario)
- [`FRInseeMortalityForecast2026`](https://patrickaubert.github.io/healthexpectancies/reference/FRInseeMortalityForecast2026.md)
  : A dataset with mortality rates from Insee's 2026 population forecast
  (central scenario)
