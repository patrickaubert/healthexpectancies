#' Estimates a polynomial approximation for prevalences by age
#'
#' Given a vector of ages and a vector of prevalences, the function returns the coefficients
#' of a polynomial approximation of degree 4
#'
#' Prevalences are supposed to be constant for all ages superior to 100
#'
#' @param age a vector with observed ages
#' @param prevalence a vector with observed prevalences
#' @param weight a vector of weights for the regression (optional)
#'
#' @return a vector with prevalences according to polynomial approximation
#'
#' @export
#'
#' @examples
prevalence_to_polynomial <- function (age, prevalence, weight = rep(1,NROW(prevalence)) ) {

  if (NROW(age) != NROW(prevalence)) { stop("Error: Different size of input vectors") }

  tab <- data.frame(prevalence) %>%
    mutate(a = pmin(100,as.numeric(age)), a2 = a^2, a3 = a^3, a4 = a^4)

  fitted_models <- tab %>%
    do(model = lm(prevalence ~ a + a2 + a3 + a4 , data = ., weights = weight))

  fitted_models$model[[1]]$fitted.values
}
