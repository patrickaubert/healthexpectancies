% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ApproxPrevalenceTable.R
\name{ApproxPrevalenceTable}
\alias{ApproxPrevalenceTable}
\title{Transforms a table with prevalences by age brackets into a table with (smoothed) prevalences by age}
\usage{
ApproxPrevalenceTable(
  tab,
  agecuts,
  agemin,
  agemax,
  weights.tab = NULL,
  categories = c("categ"),
  option = ""
)
}
\arguments{
\item{tab}{a dataframe containing categorisation variables (year, sex, categ) and prevalences by age bracket}

\item{agecuts}{a vector with age defining the age brackets (minimum age in each age bracket)}

\item{agemin}{minimum age in the output table}

\item{agemax}{maximum age in the output table}

\item{weights.tab}{a dataframe with weights (ie population) by age and some of the categorisation variables of the 'tab' dataframe}

\item{categories}{a vector of names of variables of the 'tab' dataframe, representing categories}

\item{option}{maximum age in the output table}
}
\value{
a table containing the same categorisation variables than the input table, plus prevalences by age
}
\description{
This function transforms a table with prevalences by age brackets and by (optional) sex, year and categories
into a table with smoothed prevalences at all ages (for all sex, year and categories).
}
\details{
Parameters must include the vector of age cuts, which defines the age brackets. For instance, c(60,70,80)
defines age brackets \verb{[60,70)}, \verb{[70,80)} and \verb{[80,Inf]}.

The calculation minimises the sum of squares of second-differences of prevalences
by age, under the constraint that average prevalences by age brackets
(weighted according to the 'weight' vector, usually the vector of population size at each age)
are equal to the 'prevalence' input vector. Alternatively, if 'option' is set to 'polynomial',
polynomial approximation is calculated. The degree of the polynomial function can be constrained
by setting option to 'polynomialD' (with D = degree, between 1 and 4) ; default value is 4.

In the output table, 'prevalence.raw' corresponds to input values of prevalences (constant by age bracket)
while 'prevalence.approx' corresponds to smoothed values (different value at every age).
}
