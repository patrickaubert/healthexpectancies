# A dataset with share of APA beneficiaries in France (December 2017)

APA is a benefit covering long term care expenditures both at home ('à
domicile') or in institutions ('en établissement'). Shares of
beneficiaries are estimated through the 2017 'Aide sociale' survey. They
are estimated by age brackets (eg. '\[60;65)' stands for ages 60 to 64)
A more complete dataset of APA prevalences, as well as prevalences for
other social allowances in France, can be found in the 'asdep' package.

## Usage

``` r
FRDreesAPA2017
```

## Format

A data frame with 48 observations and 5 variables:

- typepresta:

  type of benefits (eg. 'à domicile' or 'en établissement')

- prevalence:

  share of beneficiaires within the total population in the given age
  bracket (number between 0 and 1)

- sex:

  sex ('female' or 'male')

- age:

  minimal age within the age bracket

- agebracket:

  age bracket (eg '\[60;65)' = between 60 and 64 years old)

## Source

<https://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/publications/les-dossiers-de-la-drees/article/incapacites-et-perte-d-autonomie-des-personnes-agees-en-france-une-evolution>
