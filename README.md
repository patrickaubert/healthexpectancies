# healthexpectancies

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24313924.svg)](https://doi.org/10.5281/zenodo.24313924)

An R package to calculate health expectancies, and in particular disability-free life expectancies (DFLE), with the Sullivan method, from mortality rates and prevalences of disability by age. The package also smooths prevalences observed by age bracket into prevalences by single year of age, and projects prevalences and health expectancies, either under hypotheses on life expectancies or with models estimated on past observations.

The package includes several datasets for France (prevalences of disability and of the APA, population and mortality rates, observed and projected by Insee), and a Shiny app (also available online at: https://patrick-aubert.shinyapps.io/projprevalence/).

## Documentation

Website of the package, with the help pages of all functions and datasets: https://patrickaubert.github.io/healthexpectancies/

Two vignettes are available:

* [Getting started with healthexpectancies](https://patrickaubert.github.io/healthexpectancies/articles/how-to-use-healthexpectancies.html): a tutorial on the main functions, and an overview of the datasets;
* [Methods for projecting prevalences](https://patrickaubert.github.io/healthexpectancies/articles/projection-methods.html): the methods available to project prevalences, their assumptions, and a comparison on the example of the APA.

## Installation

```r
# install.packages("remotes")
remotes::install_github("patrickaubert/healthexpectancies", ref = "main")
```

## Main functions

* `CompleteDFLEtable()` calculates life expectancies, disability-free life expectancies (DFLE) and life expectancies with disability (DLE) at all ages, with the Sullivan method. Its input is a data frame with at least the variables `age`, `qx` (or `mx`) and `pix` (prevalence of disability); the output is the same data frame completed with the variables of the life table and the health expectancies. The dataset `description_sullivan` describes all the variables of the output.
* `ApproxPrevalenceTable()` and `prevalenceApprox()` convert prevalences observed by age bracket into smoothed prevalences by single year of age, consistent with the observed prevalences of the brackets. `prevalence_to_polynomial()` approximates them with a polynomial function.
* `prevalenceForecast()` projects prevalences and health expectancies from a reference year, given projected mortality rates, under simple hypotheses: constant prevalences, constant DFLE, constant DLE, or constant share of DFLE (or DLE) in total life expectancy.
* `fitPrevalenceModel()` estimates a model of the evolution of prevalences by age on past years (trend of prevalences, link with mortality, hypotheses on life expectancies, or shift of the curve of prevalences), and `predict()` projects prevalences with it. `projectPrevalence()` does both steps and calculates life expectancies.

## Interactive example (Shiny app)

The app requires packages that are not needed to install `healthexpectancies` itself:

```r
# install.packages(c("shiny", "shinydashboard", "plotly", "tidyverse"))
library(healthexpectancies)
runExample()
```

## References

Calculations are derived from the Sullivan manual, available on the INED website:
https://reves.site.ined.fr/en/resources/computation_online/sullivan/

If you use the package, please cite it (`citation("healthexpectancies")` in R gives the reference in text and BibTeX formats):

* Patrick Aubert, 2026, *healthexpectancies: Functions to Calculate Health and Disability-Free Life Expectancies (DFLE) Using Sullivan Method*, R package version 0.1.0, https://doi.org/10.5281/zenodo.24313924

The first application of the package is described in:

* Patrick Aubert, 2021, "[Allocation personnalisée d’autonomie : la part de l’espérance de vie passée en tant que bénéficiaire diminue depuis 2010](https://drees.solidarites-sante.gouv.fr/publications/etudes-et-resultats/allocation-personnalisee-dautonomie-la-part-de-lesperance-de-vie)", *Études et résultats* n°1212, DREES.

## License

This package is released under the EUPL license.
