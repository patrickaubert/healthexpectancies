#' Projects prevalences of disability and calculates life expectancies
#'
#' Estimates a model of the evolution of prevalences with \code{\link{fitPrevalenceModel}}, projects prevalences
#' with \code{\link{predict.prevalence_model}} and calculates life expectancies with the Sullivan method
#' (\code{\link{CompleteDFLEtable}}), in a single step. The estimated model is kept as an attribute of the result.
#'
#' @param tab observed prevalences by age and year (see \code{\link{fitPrevalenceModel}})
#' @param newdata projection years: data frame with variables 'year', 'age' and 'qx' (mortality rate). Mortality rates must cover
#' all the ages up to the last age of the life table (where qx = 1), as required by \code{\link{CompleteDFLEtable}}.
#' @param method "trend", "mortality", "target" or "shift" (see \code{\link{fitPrevalenceModel}})
#' @param on_unfeasible method "target" only: what to do if the hypothesis cannot be satisfied (see \code{\link{predict.prevalence_model}})
#' @param ... other arguments of \code{\link{fitPrevalenceModel}} (fit_years, exclude_years, categories, scale, etc.)
#' @return a data frame with prevalences ('pix'), life expectancies and other indicators calculated by \code{\link{CompleteDFLEtable}}.
#' The model is available with \code{attr(result, "model")}.
#' @seealso \code{\link{fitPrevalenceModel}}, \code{\link{predict.prevalence_model}}, \code{\link{prevalenceForecast}}
#' @examples
#' # prevalences of the APA by single year of age, 2010-2024 (smoothing of the 6 age brackets, weighted by the population)
#' apa <- FRAPAprevalence[FRAPAprevalence$decomp == 6, c("year", "age", "prevalence")]
#' pop <- aggregate(popx ~ year + age0101, FRInseePopulation[FRInseePopulation$geo == "france" & FRInseePopulation$age0101 %in% 60:99, ], sum)
#' names(pop) <- c("year", "age", "popx")
#' pix <- ApproxPrevalenceTable(apa, agecuts = seq(60, 85, 5), agemin = 60, agemax = 99, weights.tab = pop, categories = "year")
#' pix <- data.frame(year = pix$year, age = pix$age, pix = pix$prevalence.approx)
#' # extension up to age 104 (prevalence constant after age 99)
#' pix <- rbind(pix, do.call(rbind, lapply(100:104, function(a) transform(pix[pix$age == 99, ], age = a))))
#'
#' # mortality rates (Insee projections), the last age (104) having a mortality rate of 1
#' mort <- FRInseeMortalityForecast2026
#' mort <- mort[mort$sex == "all" & mort$geo == "France" & mort$def.age == "current age (approx)" &
#'              mort$year %in% c(2025, 2030) & mort$age %in% 60:104, c("year", "age", "qx")]
#' mort$qx[mort$age == 104] <- 1
#'
#' # trend of prevalences at each age, estimated on 2010-2019, then projection and life expectancies
#' res <- projectPrevalence(pix, newdata = mort, method = "trend", fit_years = 2010:2019)
#' res[res$age == 60, c("year", "age", "pix", "ex", "DLEx", "pctDLEx")]
#' attr(res, "model")                  # the estimated model
#'
#' # hypotheses on life expectancies need mortality rates in the estimation table too
#' mort_obs <- FRInseeMortalityrates
#' mort_obs <- mort_obs[mort_obs$sex == "all" & mort_obs$geo == "france" & !is.na(mort_obs$age) &
#'                      mort_obs$age %in% 60:104 & mort_obs$year %in% 2010:2019, c("year", "age", "qx")]
#' mort_obs$qx[mort_obs$age == 104] <- 1
#' tab <- merge(pix, mort_obs, by = c("year", "age"))
#'
#' # uniform evolution of the prevalences at all ages, such that life expectancy with the APA at age 60
#' # follows its trend of 2010-2019
#' res2 <- projectPrevalence(tab, newdata = mort, method = "target", indicator = "dle", rule = "trend",
#'                           scope = "min_age", fit_years = 2010:2019)
#' res2[res2$age == 60, c("year", "age", "pix", "ex", "DLEx", "pctDLEx")]
#'
#' # share of life expectancy lived with the APA, at each age, decreasing by 10 percent between 2019 and 2040.
#' # (at the youngest ages, where prevalences are close to 0, the hypothesis can require slightly negative
#' # prevalences: they are bounded to 0)
#' res3 <- suppressWarnings(projectPrevalence(tab, newdata = mort, method = "target", indicator = "pct_dle",
#'                          rule = "horizon", change = -10, horizon = 2040, on_unfeasible = "clip"))
#' res3[res3$age %in% c(60, 85), c("year", "age", "pix", "DLEx", "pctDLEx")]
#' @export
projectPrevalence <- function(tab, newdata, method = c("trend","mortality","target","shift"), on_unfeasible = c("warn","clip","error"), ...) {
  method <- match.arg(method); on_unfeasible <- match.arg(on_unfeasible)
  model <- fitPrevalenceModel(tab, method = method, ...)
  if (!("qx" %in% names(newdata))) { stop("'newdata' must contain the mortality rates 'qx' to calculate life expectancies.", call. = FALSE) }
  pred <- predict(model, newdata, on_unfeasible = on_unfeasible)
  gv <- model$settings$groupvars
  res <- CompleteDFLEtable(pred[ , c(gv, "year", "age", "qx", "pix")], categories = setdiff(gv, c("sex","categ")))
  attr(res, "model") <- model
  res
}
