# Transforms a table with prevalences by age brackets into a table with (smoothed) prevalences by age

This function transforms a table with prevalences by age brackets and by
(optional) sex, year and categories into a table with smoothed
prevalences at all ages (for all sex, year and categories).

## Usage

``` r
ApproxPrevalenceTable(
  tab,
  agecuts,
  agemin,
  agemax,
  weights.tab = NULL,
  categories = c("categ"),
  option = ""
)
```

## Arguments

- tab:

  a dataframe containing categorisation variables (year, sex, categ) and
  prevalences by age bracket

- agecuts:

  a vector with age defining the age brackets (minimum age in each age
  bracket)

- agemin:

  minimum age in the output table

- agemax:

  maximum age in the output table

- weights.tab:

  a dataframe with weights (ie population) by age and some of the
  categorisation variables of the 'tab' dataframe

- categories:

  a vector of names of variables of the 'tab' dataframe, representing
  categories

- option:

  smoothing method : "" (default, logit scale), "legacy", "polynomial"
  or "polynomialD" (see Details)

## Value

a table containing the same categorisation variables than the input
table, plus prevalences by age

## Details

Parameters must include the vector of age cuts, which defines the age
brackets. For instance, c(60,70,80) defines age brackets `[60,70)`,
`[70,80)` and `[80,Inf]`.

By default, the calculation minimises the sum of squares of
second-differences of the logit of prevalences by age, under the
constraint that average prevalences by age brackets (weighted according
to the 'weight' vector, usually the vector of population size at each
age) are equal to the 'prevalence' input vector (see
[`prevalenceApprox`](https://patrickaubert.github.io/healthexpectancies/reference/prevalenceApprox.md));
prevalences must then be proportions between 0 and 1. If 'option' is set
to 'legacy', the former method (second-differences of prevalences rather
than of their logit, which underestimates prevalences at the first ages)
is used. Alternatively, if 'option' is set to 'polynomial', polynomial
approximation is calculated. The degree of the polynomial function can
be constrained by setting option to 'polynomialD' (with D = degree,
between 1 and 4) ; default value is 4.

In the output table, 'prevalence.raw' corresponds to input values of
prevalences (constant by age bracket) while 'prevalence.approx'
corresponds to smoothed values (different value at every age).
