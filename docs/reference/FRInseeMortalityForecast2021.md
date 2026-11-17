# A dataset with mortality rates from Insee's 2021 population forecast (central scenario)

The dataset includes observed values from 1962 to 2020, then forecasted
values from 2021 to 2120.

## Usage

``` r
FRInseeMortalityForecast2021
```

## Format

A data frame with 115434 observations and 6 variables:

- age:

  age

- year:

  year

- sex:

  sex ('male' or 'female' or 'all')

- type.obs:

  type of observation ('observed' or 'observed (prov.)' or 'forecasted')

- def.age:

  definition of age ('age at end of year' or 'current age (approx)')

- qx:

  mortality rates at each year, age, sex

## Source

<https://www.insee.fr/fr/statistiques/5894093?sommaire=5760764>

## Details

Note: Insee's file provides number of death by age at the end of the
year, not age at last birthday. We assume deaths are evenly distributed
within the year, so we use simple average to convert from definition of
age to the other.
