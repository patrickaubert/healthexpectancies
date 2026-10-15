#' Results of the estimation of a model of prevalences
#'
#' Methods to retrieve and display the results of a model estimated with \code{\link{fitPrevalenceModel}}.
#'
#' * \code{coef()} returns the coefficients in a data frame (long format), one row per series, age and term: "intercept"
#' (value of the transformed prevalence at the last year of the estimation) and "slope" (annual change) for method "trend";
#' "alpha" (age effects) and "beta" (slope on the mortality rates, with age = NA if it is common to all ages) for method "mortality".
#' For method "target", "reference" (value of the indicator at the reference year) for rules "constant" and "horizon", and "intercept" and "slope"
#' (value and annual change of the indicator) for rule "trend"; with scope "min_age", only the minimum age (or 'anchor_age') is concerned.
#' Columns: variables defining series, 'age', 'term', 'estimate', 'std.error', 'n'.
#' * \code{fitted()} and \code{residuals()} return fitted prevalences and residuals (on the scale of the estimation) as vectors,
#' in the order of the rows of \code{as.data.frame(x, what = "fitted")}.
#' * \code{as.data.frame()} returns one of the tables of the model, selected by \code{what}: "coefficients", "fitted"
#' (observed and fitted prevalences and residuals by year and age), "observed" (data used for the estimation), "fit_stats"
#' (number of observations, residual standard deviation and R-squared for each series) or "lag_search" (sum of squared residuals for each value
#' of 'age_lag' tried, for method "mortality" when 'age_lag' is a vector). These tables can be used directly to draw graphics.
#' * \code{summary()} and \code{print()} display a short description of the model.
#'
#' @param object,x a model estimated with \code{\link{fitPrevalenceModel}}
#' @param what table to be returned by \code{as.data.frame()} (see above)
#' @param row.names,optional not used
#' @param ... not used
#' @return a data frame, a vector, or (for summary) a list of class "summary.prevalence_model"
#' @seealso \code{\link{fitPrevalenceModel}}, \code{\link{predict.prevalence_model}}
#' @examples
#' ages <- 60:100; years <- 2010:2019
#' obs <- expand.grid(age = ages, year = years)
#' obs$pix <- plogis(-6 + 0.12 * (obs$age - 60) - (0.01 + 0.0005 * (obs$age - 60)) * (obs$year - 2019))
#' m <- fitPrevalenceModel(obs, method = "trend")
#' summary(m)
#' head(coef(m))
#' head(as.data.frame(m, what = "fitted"))
#' @rdname coef.prevalence_model
#' @export
coef.prevalence_model <- function(object, ...) { object$coefficients }

#' @rdname coef.prevalence_model
#' @export
fitted.prevalence_model <- function(object, ...) { object$fitted$pix_fit }

#' @rdname coef.prevalence_model
#' @export
residuals.prevalence_model <- function(object, ...) { object$fitted$resid }

#' @rdname coef.prevalence_model
#' @export
as.data.frame.prevalence_model <- function(x, row.names = NULL, optional = FALSE,
                                           what = c("coefficients","fitted","observed","fit_stats","lag_search"), ...) {
  what <- match.arg(what)
  res <- x[[what]]
  if (is.null(res)) { stop("No 'lag_search' table: it only exists for method \"mortality\" when 'age_lag' is a vector.", call. = FALSE) }
  as.data.frame(res)
}

#' @rdname coef.prevalence_model
#' @export
print.prevalence_model <- function(x, ...) {
  st <- x$settings
  cat("Model of prevalences by age, method \"",x$method,"\"", if (x$method != "target") paste0(" (scale: ",st$scale,")"), "\n", sep = "")
  cat("  Years used for the estimation: ", min(st$fit_years), " to ", max(st$fit_years), " (", NROW(st$fit_years), " years)\n", sep = "")
  cat("  Series: ", NROW(x$series), if (NROW(st$groupvars) > 0) paste0(" (defined by ", paste(st$groupvars, collapse = ", "), ")") else "", "\n", sep = "")
  cat("  Ages: ", min(x$fitted$age), " to ", max(x$fitted$age), "\n", sep = "")
  if (x$method == "trend") { cat("  Estimator: ", st$estimator, "\n", sep = "") }
  if (x$method == "mortality") {
    cat("  Slope: ", st$slope, " ; age shift: ", paste(unique(round(st$age_lag$age_lag, 3)), collapse = ", "), "\n", sep = "") }
  if (x$method == "target") {
    cat("  Indicator: ", st$indicator, " ; rule: ", st$rule, " ; scope: ", st$scope,
        if (st$scope == "min_age") paste0(" (age ", st$anchor_age, ", ", st$uniform_scale, " scale)") else "", "\n", sep = "")
    if (st$rule != "trend") { cat("  Reference year: ", st$ref_year, "\n", sep = "") }
    if (st$rule == "horizon") { cat("  Change of ", st$change, " percent at ", st$horizon, " (then ", st$after, ")\n", sep = "") }
  }
  cat("Use coef(), as.data.frame(, what = ) and predict() to retrieve results and project prevalences.\n")
  invisible(x)
}

#' @rdname coef.prevalence_model
#' @export
summary.prevalence_model <- function(object, ...) {
  key <- NULL
  if (object$method == "mortality" && object$settings$slope == "common") {
    key <- object$coefficients[object$coefficients$term == "beta", ]
  }
  structure(list(method = object$method, settings = object$settings, fit_stats = object$fit_stats, beta = key), class = "summary.prevalence_model")
}

#' @rdname coef.prevalence_model
#' @export
print.summary.prevalence_model <- function(x, ...) {
  cat("Model of prevalences by age, method \"",x$method,"\"", if (x$method != "target") paste0(" (scale: ",x$settings$scale,")"), "\n", sep = "")
  cat("Years used for the estimation:", paste(range(x$settings$fit_years), collapse = " to "),
      if (NROW(x$settings$exclude_years) > 0) paste0("(excluding ",paste(x$settings$exclude_years, collapse = ", "),")") else "", "\n")
  if (x$method == "mortality") {
    cat("Age shift k:", paste(unique(round(x$settings$age_lag$age_lag, 3)), collapse = ", "), "\n")
    if (!is.null(x$beta)) { cat("\nSlope on ln(q):\n"); print(x$beta[ , setdiff(names(x$beta), c("age","term","n")), drop = FALSE], row.names = FALSE, digits = 4) }
  }
  cat("\nFit statistics by series (sigma: residual standard deviation on the scale of the estimation",
      if (x$method == "target") ", here the logit of prevalences", "):\n", sep = "")
  print(x$fit_stats, row.names = FALSE, digits = 4)
  invisible(x)
}
