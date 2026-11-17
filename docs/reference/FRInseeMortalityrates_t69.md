# An (alternative) dataset with observed mortality rates by sex and age, from Insee

This dataset corresponds to data released by Insee as T69, as comparison
to the FRInseeMortalityrates, which corresponds to T68 (see Insee's
website for a presentation of the differences).

## Usage

``` r
FRInseeMortalityrates_t69
```

## Format

A data frame with 67872 observations and 7 variables:

- age:

  age

- def.age:

  definition of age: at the end of the year vs. current age

- year:

  year

- sex:

  sex ('male' or 'female' or 'all')

- geo:

  geographical field ('france' or 'metropolitan france')

- status:

  'p' for 'provisional', ” otherwise

- qx:

  mortality rate at each year, age, sex

## Source

<https://www.insee.fr/fr/statistiques/fichier/8721189/>

## Details

The current version of the data table was released in January, 2026 and
contains observations from 1946 (metropolitan france) or 1994 (france)
to 2025. The geographical field is France including oversea territories.
