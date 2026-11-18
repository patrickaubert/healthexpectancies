# A dataset with population by sex and age, from Insee's 2021 population forecast (central scenario)

The dataset also includes observed data from 1962 to 2021. Population is
measured on the 1st of January of each year.

## Usage

``` r
FRInseePopulationForecast2021
```

## Format

A data frame with 50325 observations and 6 variables:

- age0101:

  age at January, 1st of the year

- year:

  year

- sex:

  sex ('male' or 'female' or 'all')

- geo:

  geographical field ('france' only)

- type.obs:

  'observed' or 'forecasted'

- popx0101:

  population on the 1st of January at each year, age, sex

## Source

<https://www.insee.fr/fr/statistiques/5894093?sommaire=5760764>
