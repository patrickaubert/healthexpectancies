# healthexpectancies

A R package containing functions that calculate disability-free life expectancy (DFLE) from mortality rates and prevalences of disability by age.

The package also includes several data bases as examples (an example from the sullivan manual, forecasted population and mortality rates for France, prevalences of disability among elder people from the French VQS survey) and a Shiny app.

## References:

Calculations are derived from the Sullivan manual, available on the INED website:
https://reves.site.ined.fr/en/resources/computation_online/sullivan/

This package is released under EUPL license. This version is a very preliminary version of an on-going work: please be aware of it!

## To install package:

remotes::install_github("patrickaubert/healthexpectancies",ref='main')

## To run the example (Shiny app):

library(tidyverse)

library(shiny)

library(shinydashboard)

library(plotly)

library(healthexpectancies)

healthexpectancies::runExample()




