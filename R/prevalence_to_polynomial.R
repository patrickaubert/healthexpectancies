#' Estimates a polynomial approximation for prevalences by age
#'
#' Given a vector of ages and a vector of prevalences (constant over each age brackets),
#' the function returns the vector of prevalences by age smoothed by polynomial approximation of degree 4.
#'
#' Prevalences are supposed to be constant for all ages superior to 100
#'
#' @param age a vector with observed ages (eg. each ages between 60 and 100)
#' @param prevalence a vector with observed prevalences (constant over each age brackets if prevalences are only observed as averages by age bracket)
#' @param weight a vector of weights for the regression (optional)
#'
#' @return a vector with prevalences according to polynomial approximation
#'
#' @export
prevalence_to_polynomial <- function (age, prevalence, weight = rep(1,NROW(prevalence)) ) {

  if (NROW(age) != NROW(prevalence)) { stop("Error: Different size of input vectors") }

  tab <- data.frame(prevalence) %>%
    mutate(a = pmin(100,as.numeric(age)), a2 = a^2, a3 = a^3, a4 = a^4)

  fitted_models <- tab %>%
    do(model = lm(prevalence ~ a + a2 + a3 + a4 , data = ., weights = weight))

  return( fitted_models$model[[1]]$fitted.values )
}
