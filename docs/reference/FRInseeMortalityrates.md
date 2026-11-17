# A dataset with observed mortality rates by sex and age, from Insee

The current version of the data table was released in January, 2026 and
contains observations from 1978 (Metropolitan France) or 1998 (France
including oversea territories) to 2024 Observations correspond to an
average over 3 years: for instance, values for 2017 are actually an
average between 2016 and 2018. Data are from 'tableau 68' (TM68)
released by Insee.

## Usage

``` r
FRInseeMortalityrates
```

## Format

A data frame with 21962 observations and 5 variables:

- age:

  age

- year:

  year

- geo:

  field ('france' or 'metropolitan france')

- sex:

  sex ('male' or 'female' or 'all)

- qx:

  mortality rate at each year, age, sex

## Source

<https://www.insee.fr/fr/statistiques/7624538?sommaire=7624746>
