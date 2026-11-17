# Estimates prevalences by age from values by age brackets

Given a vector of prevalences by age brackets and the vector of age cuts
(which defines the age brackets), the function returns a vector of
prevalences at all ages, such that the average prevalence in each age
bracket (weighted according to the 'weight' vector, usually the vector
of population size at each age) is equal to the 'prevalence' input
vector.

## Usage

``` r
prevalenceApprox(
  prevalence,
  agecuts,
  agemin,
  agemax,
  weight = rep(1, (agemax - agemin + 1)),
  categories = c(""),
  method = c("logit", "legacy")
)
```

## Arguments

- prevalence:

  a vector with observed prevalences by age bracket

- agecuts:

  a vector with age defining the age brackets (minimum age in each age
  bracket)

- agemin:

  minimum age in the output vector

- agemax:

  maximum age in the output vector

- weight:

  a vector of weights for each age

- categories:

  a vector of names of variables of the 'prevalence' dataframe,
  representing categories (optional)

- method:

  "logit" (default) or "legacy" (see Details)

## Value

a vector with prevalences according to polynomial approximation

## Details

Two methods are available. With `method = "logit"` (default), the
prevalences minimise the sum of squares of second-differences of their
logit (the logit scale guarantees prevalences strictly between 0 and 1
and accommodates the exponential rise of prevalences with age).
Prevalences must then be expressed as proportions between 0 and 1. A
prevalence of exactly 0 or 1 in a bracket cannot be matched on the logit
scale: it is replaced by 1e-6 or 1 - 1e-6, with a warning.

With `method = "legacy"`, the prevalences minimise the sum of squares of
second-differences of the prevalences themselves, as in earlier versions
of the package. This method implicitly assumes that prevalences are null
just before the minimum age: it strongly underestimates prevalences at
the first ages (typically by more than 50 percent at the minimum age)
and is kept only to reproduce earlier results.

'prevalence' and 'weight' can also be given as dataframes containing
sex, year and/or categories variables. In this case, the output is a
dataframe and prevalences are smoothed for all sex, years and
categories.

If 'categories' is provided (a vector of variable names in 'prevalence'
and 'weights'), smoothing is performed for each separate values of
category variables.

## Examples

``` r
# Prevalences of the APA in 2019 by 5-year age bracket (proportions), from age 60 to 85 and over
apa <- FRAPAprevalence[FRAPAprevalence$decomp == 6 & FRAPAprevalence$year == 2019, ]
# Population by single year of age (both sexes) on 1 January 2020, used as weights
pop <- FRInseePopulation[FRInseePopulation$geo == "france" & FRInseePopulation$year == 2020 &
                           FRInseePopulation$age0101 %in% 60:99, ]
pop <- aggregate(popx ~ age0101, data = pop, FUN = sum)
prevalenceApprox(prevalence = apa$prevalence, agecuts = seq(60, 85, 5),
                 agemin = 60, agemax = 99, weight = pop$popx)
#>  [1] 0.002738337 0.003527740 0.004539565 0.005822761 0.007418243 0.009336929
#>  [7] 0.011522241 0.013875074 0.016281914 0.018668897 0.021067769 0.023694473
#> [13] 0.026803737 0.030665498 0.035534781 0.041591862 0.048861751 0.057345695
#> [19] 0.067043409 0.078001389 0.090385456 0.104611060 0.121167746 0.140592750
#> [25] 0.163421499 0.190102350 0.220874787 0.255843633 0.294930920 0.337839294
#> [31] 0.384033940 0.432752885 0.483046991 0.533849504 0.584063531 0.632653576
#> [37] 0.678724800 0.721578556 0.760739002 0.795951496
# same, with the smoothing of raw prevalences
prevalenceApprox(prevalence = apa$prevalence, agecuts = seq(60, 85, 5),
                 agemin = 60, agemax = 99, weight = pop$popx, method = "legacy")
#>  [1] 0.001064385 0.002762136 0.004757515 0.006807486 0.008761225 0.010558942
#>  [7] 0.012230742 0.013852207 0.015543319 0.017469037 0.019837860 0.022903160
#> [13] 0.026812688 0.031609264 0.037233779 0.043531420 0.050275741 0.057520698
#> [19] 0.065590526 0.075056835 0.086708259 0.101555541 0.120234534 0.143023632
#> [25] 0.169858908 0.200345160 0.233778843 0.269557762 0.307171259 0.346196188
#> [31] 0.386287228 0.427168636 0.468622256 0.510479131 0.552610847 0.594922626
#> [37] 0.637346379 0.679834482 0.722355000 0.764887472

# Data frames: prevalences of the APA in 2017 by sex and type of benefit, weighted by the population by sex
w <- FRInseePopulation[FRInseePopulation$year == 2018 & FRInseePopulation$geo == "france" &
                         FRInseePopulation$age0101 >= 60, ]
w$sex <- ifelse(w$sex == "F", "female", "male")
names(w)[names(w) == "popx"] <- "weight"
w <- w[order(w$sex, w$age0101), ]
res <- prevalenceApprox(prevalence = FRDreesAPA2017, agecuts = seq(60, 95, 5), agemin = 60, agemax = 99,
                        weight = w, categories = "typepresta")
head(res)
#>   age  sex  prevalence           typepresta
#> 1  60 male 0.001462059 APA en établissement
#> 2  61 male 0.001751143 APA en établissement
#> 3  62 male 0.002095760 APA en établissement
#> 4  63 male 0.002502308 APA en établissement
#> 5  64 male 0.002973064 APA en établissement
#> 6  65 male 0.003501918 APA en établissement
```
