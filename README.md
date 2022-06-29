# healthexpectancies

A R package containing functions that calculate disability-free life expectancy (DFLE) from mortality rates and prevalences of disability by age.

The package also includes several data bases as examples (an example from the sullivan manual, forecasted population and mortality rates for France, prevalences of disability among elder people from the French VQS survey) and a Shiny app (also available online at: https://patrick-aubert.shinyapps.io/projprevalence/).

## Documentation:

https://patrickaubert.github.io/healthexpectancies/index.html

## References

Calculations are derived from the Sullivan manual, available on the INED website:
https://reves.site.ined.fr/en/resources/computation_online/sullivan/

This package is released under EUPL license. This version is a preliminary version of an on-going work: please be aware of it!

If you use the package, please include the following article in your list of references:

Patrick Aubert, 2021, "[Allocation personnalisée d’autonomie : la part de l’espérance de vie passée en tant que bénéficiaire diminue depuis 2010](https://drees.solidarites-sante.gouv.fr/publications/etudes-et-resultats/allocation-personnalisee-dautonomie-la-part-de-lesperance-de-vie)", *Études et résultats* n°1212, DREES.

## To install the package:

remotes::install_github("patrickaubert/healthexpectancies",ref='main')

## To run the example (Shiny app):

library(tidyverse)

library(shiny)

library(shinydashboard)

library(plotly)

library(healthexpectancies)

healthexpectancies::runExample()

## Functions and data in the package:

The main function in the package is *CompleteDFLEtable*, which calculates several indicators, among which disability-free and in-disability life expectancies (DFLE and DLE) at all ages. Its input is a dataset containing some information on age, mortality rates and prevalences of disability (at least 'age', 'qx' or 'mx', and 'pix' variables must be within the input dataset). The output is the same dataset enriched with values of life expectancies and disability-free life expectancies (and several other indicators). If some indispensable variables are missing (eg 'age'), the output is the same as the input dataset. Dataset *description_sullivan* provides full description of the variables in the output dataset of the *CompleteDFLEtable* function.

Another useful function is *prevalenceForecast*, which forecasts prevalences of disabilies, DFLEs and DLEs, given prevalences at a reference year and forecasted mortality rates. Several options are available for the forecast, according to the hypothesis on the evolution of DFLE: constant DFLE over time, constant DLE, constant share of DFLE in total life expectancy, or constant prevalences of disability.

Since prevalences of disabilities are usually observed by age brackets only (they are measured through survey data, and sample size issues impede observation at each age), two functions enable to smooth them and estimate prevalences by age. *prevalence_to_polynomial* smoothes prevalences by age brackets through polynomial approximation of degree 4, while *prevalenceApprox* minimises the sum of squares of second-differences of prevalences by age under the constraint that weighted average prevalences by age brackets are equal to the 'prevalence' input vector (*second-differences* rather than first-differences are used in the minimisation function, since prevalences according to age are usually parabolic).

The package also includes several datasets for France: *FRInseeMortalityForecast2016* and *FRInseeMortalityForecast2021* (forecasted mortality rates for men and women at all ages), *FRInseePopulationForecast2016* and *FRInseePopulationForecast2021* (forecasted population of men and women at all ages), *FRDreesVQSsurvey2014* (prevalences of disabilities after age 60, by 5-year age brackets, from the 2014 *Vie quotidienne et santé* [VQS] Survey), *FRDreesAPA2017* (shares of beneficiaries of *allocation personnalisée d'autonomie* [APA] in the total population in Decembre, 2017, from the *Aide sociale* survey), and *sullivan* (example 1 from the Sullivan manual).
