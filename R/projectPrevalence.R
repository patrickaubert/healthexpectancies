#' Projects prevalences of disability and calculates life expectancies
#'
#' Estimates a model of the evolution of prevalences with \code{\link{fitPrevalenceModel}}, projects prevalences
#' with \code{\link{predict.prevalence_model}} and calculates life expectancies with the Sullivan method
#' (\code{\link{CompleteDFLEtable}}), in a single step. The estimated model is kept as an attribute of the result.
#'
#' @param tab observed prevalences by age and year (see \code{\link{fitPrevalenceModel}})
#' @param newdata projection years: data frame with variables 'year', 'age' and 'qx' (mortality rate). Mortality rates must cover
#' all the ages up to the last age of the life table (where qx = 1), as required by \code{\link{CompleteDFLEtable}}.
#' @param method "trend" or "mortality" (see \code{\link{fitPrevalenceModel}})
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
#' @export
projectPrevalence <- function(tab, newdata, method = c("trend","mortality"), ...) {
  method <- match.arg(method)
  model <- fitPrevalenceModel(tab, method = method, ...)
  if (!("qx" %in% names(newdata))) { stop("'newdata' must contain the mortality rates 'qx' to calculate life expectancies.", call. = FALSE) }
  pred <- predict(model, newdata)
  gv <- model$settings$groupvars
  res <- CompleteDFLEtable(pred[ , c(gv, "year", "age", "qx", "pix")], categories = setdiff(gv, c("sex","categ")))
  attr(res, "model") <- model
  res
}
