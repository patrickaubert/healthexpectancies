# Forecasts prevalences of disability and DFLEs

Given a table for the reference year with mortality rates and
prevalences of disability, and a projection table with mortality rates,
the function calculates prevalences and disability-free life
expectancies.

## Usage

``` r
prevalenceForecast(
  tabref,
  tabproj,
  hypo,
  includevars = c(),
  on_unfeasible = c("warn", "clip", "error")
)
```

## Arguments

- tabref:

  a data frame for the reference year, containing variables: year, age,
  qx (mortality rate) and pix (prevalences) (+ optional: sex)

- tabproj:

  a data frame for projection years, containing variables: year, age,
  and qx (mortality rate) (+optional: sex)

- hypo:

  projection hypotheses
  ('cstDFLE','cstDLE','cstPreval','cstPctDFLE','cstPctDLE')

- includevars:

  a vector of names of variables that must be included in the output
  table (otherwise, only pix, ex, DFLEx, DLEx and pctDFLEx are kept)

- on_unfeasible:

  what to do if some prevalences are outside the interval 0 to 1: "warn"
  (default) returns the results as they are with a warning; "clip"
  bounds prevalences to the interval 0 to 1 and recalculates life
  expectancies accordingly (so that the table remains consistent), with
  a warning; "error" stops.

## Value

a data frame with prevalences, DFLE, DLE and share of DFLE in total LE,
by year and age (and optionnally sex)

## Details

Projection hypotheses can be one of the following:

- 'cstDFLE' : disability-free life expectancies constant in all year

- 'cstDLE' : in-disability life expectancies constant in all year

- 'cstPreval' : prevalences by age (and sex) constant in all year

- 'cstPctDFLE' : share of disability-free life expectancy in total
  life-expectancy constant

- 'cstPctDLE' : share of in-disability life expectancy in total
  life-expectancy constant

With the hypotheses on life expectancies, prevalences are obtained by
inverting the Sullivan method: given the projected mortality, the
prevalence at each age is the one that gives the targeted life
expectancy at this age and at the following age. A hypothesis may be
impossible to satisfy, for example if it would require a negative
prevalence at some ages. Argument 'on_unfeasible' sets what to do in
this case.
