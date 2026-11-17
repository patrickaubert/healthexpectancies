# A dataset with population by sex and age, from Insee's 'bilan démographique' (annual population estimates)

The current version of the data table was released in 2026 and contains
observations from 1991 (France) or 1901 (metropolitan France) to 2026.
Observations for the last three years (2023 and more) are provisional.

## Usage

``` r
FRInseePopulation
```

## Format

A data frame with 32 400 observations and 5 variables:

- age0101:

  age at January, 1st of the year

- year:

  year

- sex:

  sex ('male' or 'female')

- geo:

  geographical field ('france' or 'metropolitan france')

- popx:

  population at each year, age, sex

## Source

<https://www.insee.fr/fr/outil-interactif/5014911/pyramide.htm>
