# Prevalence of the APA by age bracket, France, 2010-2024

Number of beneficiaries and prevalence (share in the population) of the
APA (allocation personnalisee d'autonomie, the French benefit for older
people with long-term care needs), for all GIR (levels of loss of
autonomy) and all places of residence (at home and in institutions), by
age bracket. The data come from the annual survey 'Aide sociale' of the
DREES, compiled in the package 'asdep' (version of january 2026, data up
to 31 december 2024).

## Usage

``` r
FRAPAprevalence
```

## Format

A data frame with 162 observations and 7 variables:

- year:

  year (31 december)

- decomp:

  decomposition of ages: 6 or 8 age brackets

- agebracket:

  label of the age bracket (for example "\[70,75)" for 70 to 74 years
  old)

- age:

  minimum age of the age bracket

- nb:

  number of beneficiaries of the APA in the age bracket

- pop:

  total population in the age bracket

- prevalence:

  prevalence of the APA in the age bracket (between 0 and 1)

## Source

<http://www.data.drees.sante.gouv.fr/>

## Details

Two decompositions of ages above 60 are available, identified by
variable 'decomp': 6 brackets (5-year brackets up to 85, then 85 and
over), available for 2010 to 2024, and 8 brackets (5-year brackets up to
95, then 95 and over), available for 2016 to 2024. Total figures for all
GIR are those recalculated as the sum of the numbers by GIR. Prevalences
by age bracket can be converted to prevalences by single year of age
with
[`ApproxPrevalenceTable`](https://patrickaubert.github.io/healthexpectancies/reference/ApproxPrevalenceTable.md).
