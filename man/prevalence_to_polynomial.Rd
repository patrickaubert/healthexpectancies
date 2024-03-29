% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prevalence_to_polynomial.R
\name{prevalence_to_polynomial}
\alias{prevalence_to_polynomial}
\title{Estimates a polynomial approximation for prevalences by age}
\usage{
prevalence_to_polynomial(
  prevalence,
  agecuts = NULL,
  agemin,
  agemax,
  weight = rep(1, (agemax - agemin + 1)),
  degree = 4
)
}
\arguments{
\item{prevalence}{a vector with observed prevalences by age or agebracket}

\item{agecuts}{an optional vector with age defining the age brackets (minimum age in each age bracket)}

\item{agemin}{minimum age in the output vector}

\item{agemax}{maximum age in the output vector}

\item{weight}{a vector of weights for the regression (optional)}

\item{degree}{the degree of the polynomial function used to smooth prevalences (default value is 4)}
}
\value{
a vector with prevalences according to polynomial approximation
}
\description{
Given a vector of ages and a vector of prevalences (constant over each age brackets),
the function returns the vector of prevalences by age smoothed by polynomial approximation of degree 4 (default).
}
\details{
Prevalences are supposed to be constant for all ages superior to 100
}
