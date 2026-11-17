# A dataset with prevalence of general activity limitations (GALI) in France

Prevalences are measured in the SRCV survey ie. the French version of
EU-SILC. They are measured by sex and age bracket, for severe general
activity limitations and for limitations including moderate ones.

## Usage

``` r
FRGaliEUSilc
```

## Format

A data frame with 2 736 observations and 5 variables:

- incap:

  type of general activity limitations ('gali_incl_moderate' or
  'gali_severe')

- prevalence:

  share of people with general activity limitations in the given age
  bracket (number between 0 and 1)

- sex:

  sex ('female' or 'male' or 'all)

- agebracket:

  age bracket (eg '\[60;65)' = between 60 and 64 years old)

- year:

  year of observation (value for the month of December)

## Source

<https://drees.solidarites-sante.gouv.fr/publications-communique-de-presse/etudes-et-resultats/260122-ER-esperance-de-vie-sans-incapacite>
