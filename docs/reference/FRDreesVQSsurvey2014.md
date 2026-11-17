# A dataset with disability prevalences in France (2014)

Prevalences are estimated through the 2014 'Vie quotidienne et santé'
(VQS) survey. They are estimated by age brackets (eg. '\[60;65)' stands
for ages 60 to 64)

## Usage

``` r
FRDreesVQSsurvey2014
```

## Format

A data frame with 176 observations and 5 variables:

- limitationtype:

  type of limitation (eg. GALI, physical limitation, etc.)

- prevalence:

  prevalence of limitation in the given age bracket (number between 0
  and 1)

- sex:

  sex ('female' or 'male')

- age:

  minimal age within the age bracket

- agebracket:

  age bracket (eg '\[60;65)' = between 60 and 64 years old)

## Source

<https://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/publications/les-dossiers-de-la-drees/article/incapacites-et-perte-d-autonomie-des-personnes-agees-en-france-une-evolution>
