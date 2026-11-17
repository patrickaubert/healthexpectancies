# A dataset with mortality rates from Insee's 2026 population forecast (central scenario)

The dataset includes observed values from 1962 to 2022, then forecasted
values from 2023 to 2125.

## Usage

``` r
FRInseeMortalityForecast2026
```

## Format

A data frame with 112584 observations and 7 variables:

- age:

  age

- year:

  year

- geo:

  geographical field (France or metropolitan France according to year of
  observation)

- sex:

  sex ('male' or 'female' or 'all')

- type.obs:

  type of observation ('observed' or 'observed (prov.)' or 'forecasted')

- def.age:

  definition of age ('age at end of year' or 'current age (approx)')

- qx:

  mortality rates at each year, age, sex

## Source

<https://www.insee.fr/fr/statistiques/8990852?sommaire=8991068>

## Details

Note: Insee's file provides number of death by age at the end of the
year, not age at last birthday. We assume deaths are evenly distributed
within the year, so we use simple average to convert from definition of
age to the other.
