# A dataset with disability prevalences in France (2021)

Prevalences are estimated through the 2021 'Vie quotidienne et santé'
(VQS) survey. They are estimated by age brackets (eg. '\[60;65)' stands
for ages 60 to 64)

## Usage

``` r
FRDreesVQSsurvey2021
```

## Format

A data frame with 4626 observations and 6 variables:

- limitationtype:

  type of limitation (in French) (eg. 'Difficultés pour marcher', etc.)

- limitationintensity:

  strength of limitation (in French) (eg. 'Ne peut pas du tout', etc.)

- sex:

  sex ('female' or 'male' or 'all')

- agebracket:

  age bracket (eg '\[60;65)' = between 60 and 64 years old)

- prevalence:

  prevalence of limitation in the given age bracket (number between 0
  and 1)

- nb:

  number of persons with limitation in the given age bracket (number
  between 0 and 1)

## Source

<https://data.drees.solidarites-sante.gouv.fr/explore/dataset/enquete-vie-quotidienne-et-sante-2021-donnees-detaillees/information/>
