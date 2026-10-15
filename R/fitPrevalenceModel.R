#' Estimates a model of the evolution of prevalences of disability by age
#'
#' Estimates, on past observations of prevalences of disability by age, a model that can then be used to
#' project prevalences (see \code{\link{predict.prevalence_model}}). Three methods are available.
#'
#' **Method "trend"** extrapolates prevalences at each age (separately). Denoting \eqn{p_{a,t}} the prevalence at age
#' \eqn{a} and year \eqn{t} and \eqn{g} the transformation defined by 'scale' (logit, log or identity):
#' \deqn{g(p_{a,t}) = \alpha_a + \beta_a (t - t_0) + \varepsilon_{a,t}}
#' where \eqn{t_0} is the last year used for the estimation. With estimator = "ols", coefficients are estimated by
#' ordinary least squares over all the years used for the estimation; with estimator = "endpoints", the slope is
#' calculated from the first and the last years only.
#'
#' **Method "mortality"** links the evolution of prevalences to the evolution of mortality rates \eqn{q_{a,t}}.
#' If disability occurs on average \eqn{k} years before death, a decrease of mortality at age \eqn{a+k} should translate
#' into a decrease of the prevalence of disability at age \eqn{a}:
#' \deqn{g(p_{a,t}) = \alpha_a + \beta \ln q_{a+k,t} + \varepsilon_{a,t}}
#' The intercepts \eqn{\alpha_a} are specific to each age. The slope \eqn{\beta} is common to all ages if
#' slope = "common" (the default), or specific to each age (\eqn{\beta_a}) if slope = "by_age" (more flexible, but noisier).
#' The age shift \eqn{k} is given by 'age_lag' (in years of age, not necessarily an integer: \eqn{\ln q} is
#' interpolated linearly between ages). If 'age_lag' is a vector, the value that minimises the sum of squared residuals is retained
#' (separately for each series, see 'categories'). This is a descriptive criterion, not a statistical test. For ages \eqn{a} such that
#' \eqn{a+k} is higher than the last age of the table, the mortality rate of the last age is used.
#'
#' **Method "target"** projects prevalences from a hypothesis on life expectancies, with the Sullivan method (see also
#' \code{\link{prevalenceForecast}}). Denote EVI the life expectancy with disability at age \eqn{a} and year \eqn{t}, EVSI the disability-free
#' life expectancy and \eqn{e = EVI + EVSI} the total life expectancy. The hypothesis is made of three choices:
#' * the **indicator** : "dle" (EVI), "dfle" (EVSI), "pct_dle" (share of EVI in total life expectancy, in percent, \eqn{100\,EVI/e}),
#' "pct_dfle" (share of EVSI) or "ratio" (EVSI/EVI). A linear evolution of "pct_dle" and of "pct_dfle" are equivalent hypotheses; this is not
#' the case for a relative change (rule "horizon"), nor for "dle" and "dfle" (since \eqn{e} does not evolve linearly).
#' * the **rule** giving the value of the indicator \eqn{v_{a,t}} in the projection years: "constant" (\eqn{v_{a,t} = v_{a,t_0}}, with
#' \eqn{t_0} the reference year 'ref_year'); "trend" (linear extrapolation of the indicator at each age, estimated over the years of the
#' estimation, with 'estimator'); "horizon" (the indicator changes by 'change' percent between the reference year and the year 'horizon',
#' linearly in between: \eqn{v_{a,t} = v_{a,t_0}\,[1 + (change/100)\,(t - t_0)/(horizon - t_0)]}; after 'horizon' the change goes on
#' linearly (after = "linear") or stops (after = "constant")).
#' * the **scope**: with scope = "all_ages", the rule is applied at each age, which gives a targeted EVI at each age; prevalences are the ones that give
#' this EVI, given the projected mortality (inversion of the Sullivan method, as in \code{\link{prevalenceForecast}}). With scope = "min_age",
#' the rule is only applied at the minimum age (or at 'anchor_age'), and prevalences of all ages evolve uniformly, from those of the reference
#' year, so that the targeted EVI at this age is reached: the odds of prevalences are multiplied by the same factor at all ages
#' (uniform_scale = "odds", prevalences always remain between 0 and 1) or prevalences are multiplied by the same factor, with a maximum of 1
#' (uniform_scale = "level"). This is a theoretical hypothesis, as prevalences do not usually evolve in the same proportion at all ages.
#'
#' With method "target", 'tab' must also contain mortality rates for all the ages of the life table, up to its last age (consecutive years of age),
#' and the estimation does not transform prevalences ('scale' is not used). Prevalences "fitted" by the model are the ones implied by the hypothesis
#' with the observed mortality of each year (for rule "constant", they are the observed prevalences at the reference year); residuals are differences
#' of logits. A hypothesis may be impossible to satisfy with the projected mortality: see argument 'on_unfeasible' of
#' \code{\link{predict.prevalence_model}}.
#'
#' @param tab a data frame with variables 'year', 'age' (single years of age) and 'pix' (prevalence), plus 'qx' (mortality rate)
#' if method is "mortality" or "target". Optional variables 'sex' and 'categ', and the variables listed in 'categories', define separate series:
#' one model is estimated for each series, and each series must contain the same ages for all years. Prevalences can be obtained
#' for example with \code{\link{ApproxPrevalenceTable}}.
#' @param method "trend", "mortality" or "target" (see Details)
#' @param fit_years vector of the years used for the estimation (default: all years of 'tab')
#' @param exclude_years vector of years to be left out of the estimation (for example the years of a crisis)
#' @param categories names of variables of 'tab' defining separate series (in addition to 'sex' and 'categ' when they exist)
#' @param scale scale on which prevalences are modelled: "logit" (default, keeps projected prevalences between 0 and 1),
#' "log" or "level" (prevalences themselves)
#' @param estimator method "trend", or method "target" with rule "trend": "ols" (default) or "endpoints" (slope between the first and the last years)
#' @param slope method "mortality" only: "common" (default) for a single slope \eqn{\beta}, or "by_age"
#' @param age_lag method "mortality" only: age shift \eqn{k} (in years of age) between the age of prevalences and the age of the
#' mortality rates (default 0). A vector selects the best value (see Details).
#' @param eps with scale "logit" or "log", prevalences below 'eps' (or, with "logit", above 1 - 'eps') are replaced by this bound, with a warning
#' @param indicator method "target" only: "dle", "dfle", "pct_dle", "pct_dfle" or "ratio" (see Details)
#' @param rule method "target" only: "constant", "trend" or "horizon" (see Details)
#' @param scope method "target" only: "all_ages" (a target at each age) or "min_age" (uniform evolution of prevalences, see Details)
#' @param ref_year method "target" only: reference year of the rules "constant" and "horizon" (default: last year of the estimation)
#' @param change method "target", rule "horizon": relative change of the indicator, in percent, between the reference year and 'horizon'
#' @param horizon method "target", rule "horizon": year at which the change is reached
#' @param after method "target", rule "horizon": "linear" (default, the change goes on at the same pace after 'horizon') or "constant"
#' @param anchor_age method "target", scope "min_age": age at which the hypothesis is applied (default: minimum age)
#' @param uniform_scale method "target", scope "min_age": "odds" (default) or "level" (see Details)
#'
#' @return an object of class "prevalence_model", a list containing in particular the data frames 'coefficients'
#' (one row per series, age and term; see \code{\link{coef.prevalence_model}}), 'fitted', 'fit_stats' and 'observed'.
#' Use \code{\link{predict.prevalence_model}} to project prevalences, and the other methods listed in
#' \code{\link{coef.prevalence_model}} to retrieve the results of the estimation.
#'
#' @seealso \code{\link{predict.prevalence_model}}, \code{\link{projectPrevalence}}, \code{\link{prevalenceForecast}}
#' @examples
#' # prevalences by age (simulated) : the odds of disability decrease by 2 percent a year at age 80, 1 percent at age 60
#' ages <- 60:100; years <- 2010:2019
#' obs <- expand.grid(age = ages, year = years)
#' obs$pix <- plogis(-6 + 0.12 * (obs$age - 60) - (0.01 + 0.0005 * (obs$age - 60)) * (obs$year - 2019))
#'
#' m <- fitPrevalenceModel(obs, method = "trend", fit_years = 2010:2019)
#' m
#' head(coef(m))                       # coefficients by age
#'
#' # prevalences of the APA by single year of age, 2010-2024 (smoothing of the 6 age brackets, weighted by the population)
#' apa <- FRAPAprevalence[FRAPAprevalence$decomp == 6, c("year", "age", "prevalence")]
#' pop <- aggregate(popx ~ year + age0101, FRInseePopulation[FRInseePopulation$geo == "france" & FRInseePopulation$age0101 %in% 60:99, ], sum)
#' names(pop) <- c("year", "age", "popx")
#' pix <- ApproxPrevalenceTable(apa, agecuts = seq(60, 85, 5), agemin = 60, agemax = 99, weights.tab = pop, categories = "year")
#' pix <- data.frame(year = pix$year, age = pix$age, pix = pix$prevalence.approx)
#' # extension up to age 104 (prevalence constant after age 99)
#' pix <- rbind(pix, do.call(rbind, lapply(100:104, function(a) transform(pix[pix$age == 99, ], age = a))))
#'
#' # link with mortality: the age shift between disability and death is searched between 0 and 6 years
#' mort <- FRInseeMortalityrates
#' mort <- mort[mort$sex == "all" & mort$geo == "france" & !is.na(mort$age) & mort$age %in% 60:104 &
#'              mort$year %in% 2010:2019, c("year", "age", "qx")]
#' mort$qx[mort$age == 104] <- 1
#' m2 <- fitPrevalenceModel(merge(pix, mort, by = c("year", "age")), method = "mortality",
#'                          fit_years = 2010:2019, age_lag = seq(0, 6, by = 0.5))
#' summary(m2)
#' as.data.frame(m2, what = "lag_search")
#' @export
fitPrevalenceModel <- function(tab, method = c("trend","mortality","target"), fit_years = NULL, exclude_years = NULL,
                               categories = NULL, scale = c("logit","log","level"),
                               estimator = c("ols","endpoints"), slope = c("common","by_age"),
                               age_lag = 0, eps = 1e-6,
                               indicator = c("dle","dfle","pct_dle","pct_dfle","ratio"), rule = c("constant","trend","horizon"),
                               scope = c("all_ages","min_age"), ref_year = NULL, change = NULL, horizon = NULL,
                               after = c("linear","constant"), anchor_age = NULL, uniform_scale = c("odds","level")) {

  supplied <- names(match.call())[-1]
  method <- match.arg(method); scale <- match.arg(scale)
  estimator <- match.arg(estimator); slope <- match.arg(slope)
  indicator <- match.arg(indicator); rule <- match.arg(rule); scope <- match.arg(scope)
  after <- match.arg(after); uniform_scale <- match.arg(uniform_scale)
  target_args <- c("indicator","rule","scope","ref_year","change","horizon","after","anchor_age","uniform_scale")
  if (method != "target" & any(target_args %in% supplied)) {
    warning(paste0("Arguments ",paste0("'",intersect(target_args, supplied),"'", collapse = ", ")," are ignored unless method = \"target\"."), call. = FALSE)
  }
  if (method == "trend" & any(c("slope","age_lag") %in% supplied)) {
    warning("Arguments 'slope' and 'age_lag' are ignored with method = \"trend\".", call. = FALSE)
  }
  if (method == "mortality" & ("estimator" %in% supplied)) {
    warning("Argument 'estimator' is ignored with method = \"mortality\".", call. = FALSE)
  }
  if (method == "target") {
    if (any(c("scale","slope","age_lag") %in% supplied)) { warning("Arguments 'scale', 'slope' and 'age_lag' are ignored with method = \"target\".", call. = FALSE) }
    if (rule != "trend" & ("estimator" %in% supplied)) { warning("Argument 'estimator' is only used with rule = \"trend\".", call. = FALSE) }
    if (rule == "horizon") {
      if (!is.numeric(change) || length(change) != 1 || is.na(change)) { stop("With rule = \"horizon\", 'change' (relative change in percent) must be a number.", call. = FALSE) }
      if (!is.numeric(horizon) || length(horizon) != 1 || is.na(horizon)) { stop("With rule = \"horizon\", 'horizon' (year) must be a number.", call. = FALSE) }
    }
  }

  # == controls on the input table

  tab <- as.data.frame(tab)
  need <- c("year","age","pix"); if (method %in% c("mortality","target")) { need <- c(need, "qx") }
  miss <- setdiff(need, names(tab))
  if (NROW(miss) > 0) { stop(paste0("Missing variable(s) in 'tab': ", paste0("'",miss,"'", collapse = ", "), "."), call. = FALSE) }
  if (anyNA(tab[ , need])) { stop(paste0("Missing values (NA) are not allowed in ", paste0("'",need,"'", collapse = ", "), "."), call. = FALSE) }
  tab$year <- pm_as_numeric(tab$year, "year"); tab$age <- pm_as_numeric(tab$age, "age")
  notfound <- setdiff(categories, names(tab))
  if (NROW(notfound) > 0) { stop(paste0("Variable(s) of 'categories' not found in 'tab': ", paste0("'",notfound,"'", collapse = ", "), "."), call. = FALSE) }
  groupvars <- intersect(unique(c(categories, "sex", "categ")), names(tab))
  if (method %in% c("mortality","target") && any(tab$qx <= 0)) { stop("Mortality rates 'qx' must be strictly positive.", call. = FALSE) }
  if (!is.numeric(age_lag) || anyNA(age_lag) || any(age_lag < 0)) { stop("'age_lag' must be a non-negative number (or a vector of such numbers).", call. = FALSE) }

  # == years used for the estimation

  years_avail <- sort(unique(tab$year))
  if (is.null(fit_years)) { fit_years <- years_avail }
  notyears <- setdiff(fit_years, years_avail)
  if (NROW(notyears) > 0) { stop(paste0("Year(s) of 'fit_years' not found in 'tab': ", paste(notyears, collapse = ", "), "."), call. = FALSE) }
  years_fit <- sort(setdiff(fit_years, exclude_years))
  uses_trend <- (method == "trend") | (method == "target" & rule == "trend")
  nmin <- if (!uses_trend) { ifelse(method == "target", 1, 3) } else { ifelse(estimator == "endpoints", 2, 3) }
  if (NROW(years_fit) < nmin) {
    stop(paste0("At least ",nmin," years are needed for the estimation (",NROW(years_fit)," available)."), call. = FALSE)
  }
  if (NROW(years_fit) < 5 & nmin > 2) {
    warning(paste0("Only ",NROW(years_fit)," years are used for the estimation: results may be unreliable."), call. = FALSE)
  }

  # == method "target" : hypotheses on life expectancies
  if (method == "target") {
    if (is.null(ref_year)) { ref_year <- max(years_fit) }
    if (!(ref_year %in% years_avail)) { stop(paste0("'ref_year' (",ref_year,") is not a year of 'tab'."), call. = FALSE) }
    if (rule == "horizon" && horizon <= ref_year) { stop("'horizon' must be later than the reference year.", call. = FALSE) }
    res <- pm_fit_target(tab, years_fit, ref_year, groupvars, indicator, rule, scope, estimator, change, horizon, after,
                         anchor_age, uniform_scale, eps)
    fits <- res$fits; seriestab <- res$seriestab
    groupvals <- lapply(seriestab, function(d) d[1, groupvars, drop = FALSE])
    addkey <- function(df, k) { if (NROW(groupvars) > 0) { cbind(groupvals[[k]], df, row.names = NULL) } else { df } }
    coefficients <- do.call(rbind, lapply(names(fits), function(k) addkey(fits[[k]]$coefficients, k)))
    fit_stats    <- do.call(rbind, lapply(names(fits), function(k) addkey(fits[[k]]$stats, k)))
    fitted <- do.call(rbind, lapply(names(fits), function(k) {
      f <- fits[[k]]; df <- expand.grid(age = f$details$ages, year = years_fit)
      df$pix <- as.vector(f$obs); df$pix_fit <- as.vector(f$pfit); df$resid <- as.vector(f$resid)
      addkey(df[ , c("year","age","pix","pix_fit","resid")], k) }))
    rownames(coefficients) <- rownames(fit_stats) <- rownames(fitted) <- NULL
    return(structure(list(method = "target",
                          settings = list(scale = NA_character_, indicator = indicator, rule = rule, scope = scope, estimator = estimator,
                                          ref_year = ref_year, change = change, horizon = horizon, after = after,
                                          anchor_age = fits[[1]]$details$anchor_age, uniform_scale = uniform_scale,
                                          fit_years = years_fit, exclude_years = exclude_years, categories = categories,
                                          groupvars = groupvars, eps = eps, origin = max(years_fit)),
                          coefficients = coefficients, fitted = fitted, fit_stats = fit_stats, lag_search = NULL,
                          observed = tab[tab$year %in% res$years_all, , drop = FALSE],
                          series = lapply(names(fits), function(k) list(key = k, group = groupvals[[k]], details = fits[[k]]$details))),
                     class = "prevalence_model"))
  }

  tab <- tab[tab$year %in% years_fit, , drop = FALSE]

  # == transformation of prevalences

  if (any(tab$pix < 0) | (scale == "logit" & any(tab$pix > 1))) {
    stop(ifelse(scale == "logit", "With scale = \"logit\", prevalences must be between 0 and 1.", "Prevalences must not be negative."), call. = FALSE)
  }
  lowbound <- ifelse(scale == "level", -Inf, eps); upbound <- ifelse(scale == "logit", 1 - eps, Inf)
  if (any(tab$pix < lowbound | tab$pix > upbound)) {
    warning(paste0("Prevalences too close to ",ifelse(scale == "logit", "0 or 1", "0")," have been replaced by bounds ('eps' = ",eps,")."), call. = FALSE)
    tab$pix <- pmin(pmax(tab$pix, lowbound), upbound)
  }
  gfun <- pm_transform(scale)

  # == estimation, series by series

  keys <- if (NROW(groupvars) > 0) do.call(paste, c(tab[groupvars], sep = " / ")) else rep("all data", nrow(tab))
  seriestab <- split(tab, factor(keys, levels = unique(keys)))
  fits <- lapply(names(seriestab), function(k) {
    d <- seriestab[[k]]
    ages <- sort(unique(d$age))
    if (anyDuplicated(d[ , c("age","year")]) > 0 | nrow(d) != NROW(ages) * NROW(years_fit)) {
      stop(paste0("Series '",k,"': the prevalences must be given exactly once for each age and each year used for the estimation."), call. = FALSE)
    }
    Y <- matrix(NA_real_, NROW(ages), NROW(years_fit), dimnames = list(ages, years_fit))
    Y[cbind(match(d$age, ages), match(d$year, years_fit))] <- gfun$fwd(d$pix)
    pm_fit_series(Y, d, ages, years_fit, method, estimator, slope, age_lag, key = k)
  })
  names(fits) <- names(seriestab)

  # == assembling results

  groupvals <- lapply(seriestab, function(d) d[1, groupvars, drop = FALSE])
  addkey <- function(df, k) { if (NROW(groupvars) > 0) { cbind(groupvals[[k]], df, row.names = NULL) } else { df } }
  coefficients <- do.call(rbind, lapply(names(fits), function(k) addkey(fits[[k]]$coefficients, k)))
  fit_stats    <- do.call(rbind, lapply(names(fits), function(k) addkey(fits[[k]]$stats, k)))
  age_lags     <- do.call(rbind, lapply(names(fits), function(k) addkey(data.frame(age_lag = fits[[k]]$details$age_lag), k)))
  lag_search   <- if (method == "mortality" & NROW(age_lag) > 1) {
    do.call(rbind, lapply(names(fits), function(k) addkey(fits[[k]]$lag_search, k))) } else { NULL }
  fitted <- do.call(rbind, lapply(names(fits), function(k) {
    f <- fits[[k]]; df <- expand.grid(age = f$details$ages, year = years_fit)
    df$pix <- as.vector(gfun$inv(f$Y)); df$pix_fit <- as.vector(gfun$inv(f$Yfit)); df$resid <- as.vector(f$Y - f$Yfit)
    addkey(df[ , c("year","age","pix","pix_fit","resid")], k) }))
  rownames(coefficients) <- rownames(fit_stats) <- rownames(fitted) <- NULL

  structure(list(method = method,
                 settings = list(scale = scale, estimator = estimator, slope = slope, age_lag = age_lags, fit_years = years_fit,
                                 exclude_years = exclude_years, categories = categories, groupvars = groupvars, eps = eps,
                                 origin = max(years_fit)),
                 coefficients = coefficients, fitted = fitted, fit_stats = fit_stats, lag_search = lag_search,
                 observed = tab,
                 series = lapply(names(fits), function(k) list(key = k, group = groupvals[[k]], details = fits[[k]]$details)) ),
            class = "prevalence_model")
}


# ---- internal functions -------------------------------------------------------------------------------------------

# transformation of prevalences (forward and inverse)
pm_transform <- function(scale) {
  switch(scale,
         logit = list(fwd = stats::qlogis, inv = stats::plogis),
         log   = list(fwd = log, inv = exp),
         level = list(fwd = identity, inv = identity))
}

# log of mortality rates at ages a + k (linear interpolation between ages; the last age is used beyond the end of the table)
pm_lnq_shifted <- function(lnq, ages, k) {
  if (k == 0) { return(lnq) }
  stats::approx(ages, lnq, xout = ages + k, rule = 2)$y
}

# estimation for one series. Y : matrix (ages x years) of transformed prevalences
pm_fit_series <- function(Y, d, ages, years, method, estimator, slope, age_lag, key) {

  nA <- NROW(ages); nT <- NROW(years); N <- nA * nT
  tss <- sum((Y - rowMeans(Y))^2)

  if (method == "trend") {

    t0 <- max(years); tt <- years - t0
    if (estimator == "ols") {
      tbar <- mean(tt); Sxx <- sum((tt - tbar)^2); ybar <- rowMeans(Y)
      b <- as.vector((Y - ybar) %*% (tt - tbar)) / Sxx; a <- ybar - b * tbar
      Yfit <- a + outer(b, tt); rss_a <- rowSums((Y - Yfit)^2); s2_a <- rss_a / (nT - 2)
      se_b <- sqrt(s2_a / Sxx); se_a <- sqrt(s2_a * (1 / nT + tbar^2 / Sxx)); df <- nT - 2
      details <- list(ages = ages, years = years, origin = t0, intercept = a, slope = b, tbar = tbar, Sxx = Sxx, sigma2 = s2_a, df = df, estimator = "ols")
    } else {
      b <- (Y[ , nT] - Y[ , 1]) / (years[nT] - years[1]); a <- Y[ , nT]
      Yfit <- a + outer(b, tt); se_a <- se_b <- rep(NA_real_, nA); df <- NA_real_
      details <- list(ages = ages, years = years, origin = t0, intercept = a, slope = b, estimator = "endpoints")
    }
    coefficients <- rbind(data.frame(age = ages, term = "intercept", estimate = a, std.error = se_a, n = nT),
                          data.frame(age = ages, term = "slope",     estimate = b, std.error = se_b, n = nT))
    rss <- sum((Y - Yfit)^2); dfres <- N - 2 * nA
    lag_search <- NULL

  } else {

    lnq <- matrix(NA_real_, nA, nT, dimnames = dimnames(Y))
    lnq[cbind(match(d$age, ages), match(d$year, years))] <- log(d$qx)
    estimate_k <- function(k) {
      X <- apply(lnq, 2, function(v) pm_lnq_shifted(v, ages, k)); xbar <- rowMeans(X); ybar <- rowMeans(Y)
      Xc <- X - xbar; Yc <- Y - ybar
      if (slope == "common") {
        Sxx <- sum(Xc^2)
        if (!(Sxx > 1e-12)) { stop(paste0("Series '",key,"': no variation of mortality rates over the years used for the estimation."), call. = FALSE) }
        beta <- rep(sum(Xc * Yc) / Sxx, nA); Sxx_a <- rep(NA_real_, nA)
      } else {
        Sxx_a <- rowSums(Xc^2); beta <- ifelse(Sxx_a > 1e-12, rowSums(Xc * Yc) / pmax(Sxx_a, 1e-12), 0); Sxx <- NA_real_
      }
      alpha <- ybar - beta * xbar; Yfit <- alpha + beta * X
      list(k = k, X = X, xbar = xbar, Sxx = Sxx, Sxx_a = Sxx_a, alpha = alpha, beta = beta, Yfit = Yfit, rss = sum((Y - Yfit)^2))
    }
    cand <- lapply(sort(unique(age_lag)), estimate_k)
    best <- cand[[which.min(sapply(cand, function(z) z$rss))]]
    lag_search <- if (NROW(cand) > 1) data.frame(age_lag = sapply(cand, function(z) z$k), rss = sapply(cand, function(z) z$rss), selected = sapply(cand, function(z) z$k == best$k)) else NULL
    Yfit <- best$Yfit; rss <- best$rss
    if (slope == "common") {
      dfres <- N - nA - 1; s2 <- rss / dfres
      se_beta <- sqrt(s2 / best$Sxx); se_alpha <- sqrt(s2 / nT + best$xbar^2 * s2 / best$Sxx)
      coefficients <- rbind(data.frame(age = ages, term = "alpha", estimate = best$alpha, std.error = se_alpha, n = nT),
                            data.frame(age = NA_real_, term = "beta", estimate = best$beta[1], std.error = se_beta, n = N))
      details <- list(ages = ages, years = years, age_lag = best$k, alpha = best$alpha, beta = best$beta, xbar = best$xbar, Sxx = best$Sxx,
                      sigma2 = s2, df = dfres, slope = "common")
    } else {
      dfres <- N - 2 * nA; rss_a <- rowSums((Y - Yfit)^2); s2_a <- rss_a / (nT - 2)
      se_beta <- ifelse(best$Sxx_a > 1e-12, sqrt(s2_a / pmax(best$Sxx_a, 1e-12)), NA_real_)
      se_alpha <- sqrt(s2_a * (1 / nT + best$xbar^2 / pmax(best$Sxx_a, 1e-12)))
      coefficients <- rbind(data.frame(age = ages, term = "alpha", estimate = best$alpha, std.error = se_alpha, n = nT),
                            data.frame(age = ages, term = "beta",  estimate = best$beta,  std.error = se_beta,  n = nT))
      details <- list(ages = ages, years = years, age_lag = best$k, alpha = best$alpha, beta = best$beta, xbar = best$xbar, Sxx_a = best$Sxx_a,
                      sigma2 = s2_a, df = nT - 2, slope = "by_age")
    }
    details$estimator <- NULL
  }

  stats <- data.frame(n_obs = N, n_ages = nA, n_years = nT, sigma = ifelse(dfres > 0, sqrt(rss / dfres), NA_real_),
                      r2 = ifelse(tss > 0, 1 - rss / tss, NA_real_))
  if (is.null(details$age_lag)) { details$age_lag <- NA_real_ }
  list(coefficients = coefficients, stats = stats, details = details, Y = Y, Yfit = Yfit, lag_search = lag_search)
}

# numeric version of a variable that may be stored as text or factor (for example 'year' after ApproxPrevalenceTable)
pm_as_numeric <- function(x, name) {
  if (is.numeric(x)) { return(x) }
  res <- suppressWarnings(as.numeric(as.character(x)))
  if (anyNA(res)) { stop(paste0("Variable '",name,"' must be numeric."), call. = FALSE) }
  res
}
