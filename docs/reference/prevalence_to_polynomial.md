# Estimates a polynomial approximation for prevalences by age

Given a vector of ages and a vector of prevalences (constant over each
age brackets), the function returns the vector of prevalences by age
smoothed by polynomial approximation of degree 4 (default).

## Usage

``` r
prevalence_to_polynomial(
  prevalence,
  agecuts = NULL,
  agemin,
  agemax,
  weight = rep(1, (agemax - agemin + 1)),
  degree = 4
)
```

## Arguments

- prevalence:

  a vector with observed prevalences by age or agebracket

- agecuts:

  an optional vector with age defining the age brackets (minimum age in
  each age bracket)

- agemin:

  minimum age in the output vector

- agemax:

  maximum age in the output vector

- weight:

  a vector of weights for the regression (optional)

- degree:

  the degree of the polynomial function used to smooth prevalences
  (default value is 4)

## Value

a vector with prevalences according to polynomial approximation

## Details

Prevalences are supposed to be constant for all ages superior to 100
