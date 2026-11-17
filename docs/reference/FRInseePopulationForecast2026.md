# A dataset with population by sex and age, from Insee's 2026 population forecast (central scenario)

The dataset also includes observed data from 1990 to 2022. Population is
measured on the 1st of January of each year.

## Usage

``` r
FRInseePopulationForecast2026
```

## Format

A data frame with 34107 observations and 6 variables:

- age0101:

  age at January, 1st of the year

- year:

  year

- sex:

  sex ('male' or 'female' or 'all')

- geo:

  geographical field ('Metropolitan France','France excluding
  Mayotte','France')

- type.obs:

  'observed' or 'forecasted'

- popx0101:

  population on the 1st of January at each year, age, sex

## Source

<https://www.insee.fr/fr/statistiques/8990852?sommaire=8991068>
