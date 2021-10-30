#' Estimates a polynomial approximation for prevalences by age
#'
#' Given a vector of ages and a vector of prevalences (constant over each age brackets),
#' the function returns the vector of prevalences by age smoothed by polynomial approximation of degree 4.
#'
#' Prevalences are supposed to be constant for all ages superior to 100
#'
#' @param prevalence a vector with observed prevalences by age or agebracket
#' @param agecuts an optional vector with age defining the age brackets (minimum age in each age bracket)
#' @param agemin minimum age in the output vector
#' @param agemax maximum age in the output vector
#' @param weight a vector of weights for the regression (optional)
#'
#' @return a vector with prevalences according to polynomial approximation
#'
#' @export
prevalence_to_polynomial <- function (prevalence,
                                      agecuts = NULL, agemin, agemax,
                                      weight = rep(1,(agemax-agemin+1)) ) {

  if (NROW(weight) != (agemax-agemin+1)) { stop("Error: Size of weights vector not compatible with agemin and agemax") }

  # if prevalence is only provided by agebrackets, the vector is expanded in a vector with values for every age between agemin and agemax
  if (!is.null(agecuts)) {

    if (NROW(agecuts) != NROW(prevalence)) { stop("Error: Different size of input vectors agecuts and prevalence") }

    agebracket <- cut(c(agemin:agemax), breaks = c(agecuts,Inf), include.lowest = TRUE, right = FALSE)

    prevalence <- (data.frame(
      age = c(agemin:agemax),
      agebracket = agebracket,
      stringsAsFactors = FALSE
    ) %>% left_join(
      data.frame(prevalence = prevalence, agebracket = unique(agebracket), stringsAsFactors = False),
      by = "agebracket"
    ))$prevalence

  }

  age <- c(agemin:agemax)

  tab <- data.frame(prevalence) %>%
    mutate(a = pmin(100,as.numeric(age)), a2 = a^2, a3 = a^3, a4 = a^4)

  fitted_models <- tab %>%
    do(model = lm(prevalence ~ a + a2 + a3 + a4 , data = ., weights = weight))

  return( fitted_models$model[[1]]$fitted.values )
}
