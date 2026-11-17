# A dataset with mortality rates from Insee's 2016 population forecast (central scenario)

Note: Insee's file provides number of death by age at the end of the
year, not age at last birthday. We assume deaths are evenly distributed
within the year, so we use simple average to convert from definition of
age to the other.

## Usage

``` r
FRInseeMortalityForecast2016
```

## Format

A data frame with 14036 observations and 4 variables:

- age:

  age

- year:

  year

- sex:

  sex ('male' or 'female')

- qx:

  mortality rates at each year, age, sex

## Source

<https://www.insee.fr/fr/statistiques/2496716?sommaire=2496793>
