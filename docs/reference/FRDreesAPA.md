# A dataset with share of APA beneficiaries in France

APA is a benefit covering long term care expenditures both at home ('à
domicile') or in institutions ('en établissement'). Shares of
beneficiaries are estimated through the 2016 to 2024 'Aide sociale'
survey. They are estimated by age brackets (eg. '\[60;65)' stands for
ages 60 to 64) A more complete dataset of APA prevalences, as well as
prevalences for other social allowances in France, can be found in the
'asdep' package.

## Usage

``` r
FRDreesAPA
```

## Format

A data frame with 591 observations and 6 variables:

- typepresta:

  type of benefits (eg. 'APA à domicile' or 'APA en établissement')

- prevalence:

  share of beneficiaires within the total population in the given age
  bracket (number between 0 and 1)

- sex:

  sex ('female' or 'male')

- agebracket:

  age bracket (eg '\[60;65)' = between 60 and 64 years old)

- age:

  minimal age within the age bracket

- year:

  year of observation (value for the month of December)

## Source

<https://data.drees.solidarites-sante.gouv.fr/explore/dataset/les-caracteristiques-des-beneficiaires-de-l-aide-sociale-departementale-aux-pers/information/>
