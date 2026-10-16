# Internal functions for the method "target" of fitPrevalenceModel() :
# hypotheses on life expectancies (EVI = life expectancy with disability, EVSI = disability-free life expectancy)
# given as a target for each age (inversion of the Sullivan method) or for the minimum age only (uniform evolution).

# Sullivan calculation for one year : life table functions from a vector of mortality rates over contiguous ages.
# Returns lx, Lx, ex and, if prevalences are given, DLEx and DFLEx.
pm_sullivan <- function(qx, ages, pix = NULL) {
  tab <- data.frame(year = 1, age = ages, qx = qx)
  if (!is.null(pix)) { tab$pix <- pix }
  res <- suppressWarnings(CompleteDFLEtable(tab))
  res <- res[order(res$age), ]
  out <- list(lx = res$lx, Lx = res$Lx, ex = res$ex)
  if (!is.null(pix)) { out$DLEx <- res$DLEx; out$DFLEx <- res$DFLEx }
  out
}

# value of the indicator (observed) from life expectancies
pm_indicator_value <- function(indicator, DLEx, DFLEx, ex) {
  switch(indicator,
         dle      = DLEx,
         dfle     = DFLEx,
         pct_dle  = 100 * DLEx / ex,
         pct_dfle = 100 * DFLEx / ex,
         ratio    = ifelse(DLEx > 1e-12, DFLEx / DLEx, NA_real_))
}

# targeted life expectancy with disability (EVI) from the value of the indicator and total life expectancy ex
pm_target_evi <- function(indicator, v, ex) {
  switch(indicator,
         dle      = v,
         dfle     = ex - v,
         pct_dle  = v / 100 * ex,
         pct_dfle = (1 - v / 100) * ex,
         ratio    = ex / (1 + v))
}

# value of the indicator given by the rule, for all ages (rows) and the given years (columns)
pm_rule_values <- function(det, years) {
  na <- NROW(det$ages); ny <- NROW(years)
  switch(det$rule,
         constant = matrix(det$Iref, na, ny),
         trend    = det$intercept + outer(det$slope, years - det$origin),
         horizon  = {
           w <- pmax((years - det$ref_year) / (det$horizon - det$ref_year), 0)
           if (det$after == "constant") { w <- pmin(w, 1) }
           det$Iref * (1 + det$change / 100 * matrix(w, na, ny, byrow = TRUE))
         })
}

# prevalences that give the targeted EVI at each age (inversion of the Sullivan method)
pm_invert_evi <- function(evi, lx, Lx) {
  DLT <- evi * lx
  (DLT - c(DLT[-1], 0)) / Lx
}

# uniform evolution : finds the shift of the odds (or the factor) applied to the prevalences of the reference year at all ages
# so that EVI at the anchor age is equal to the target. Returns the prevalences (NA if no solution)
pm_uniform_prevalences <- function(target_evi, p_ref, lx, Lx, anchor_idx, uniform_scale, eps = 1e-6) {
  idx <- anchor_idx:NROW(p_ref)
  shifted <- if (uniform_scale == "odds") {
    base <- stats::qlogis(pmin(pmax(p_ref, eps), 1 - eps))
    function(z) stats::plogis(base + z)
  } else {
    function(z) pmin(p_ref * z, 1)
  }
  evi <- function(z) sum(shifted(z)[idx] * Lx[idx]) / lx[anchor_idx]
  range_z <- if (uniform_scale == "odds") c(-40, 40) else c(0, 1 / max(min(p_ref[p_ref > 0]), 1e-12))
  lo <- evi(range_z[1]) ; hi <- evi(range_z[2])
  if (!is.finite(target_evi) || target_evi < lo - 1e-12 || target_evi > hi + 1e-12) {
    return(list(pix = rep(NA_real_, NROW(p_ref)), status = ifelse(target_evi < lo, "below", "above"),
                clipped = shifted(ifelse(is.finite(target_evi) && target_evi < lo, range_z[1], range_z[2]))))
  }
  z <- stats::uniroot(function(z) evi(z) - target_evi, range_z, tol = 1e-12)$root
  list(pix = shifted(z), status = "ok", clipped = shifted(z))
}

# projection of one series: prevalences (ages x years) for the years of the columns of Qmat (mortality rates, ages x years)
pm_project_target <- function(det, Qmat, years) {
  na <- NROW(det$ages); ny <- NROW(years)
  values <- pm_rule_values(det, years)
  pix <- matrix(NA_real_, na, ny, dimnames = list(det$ages, years)); clipped <- pix; status <- rep("ok", ny)
  anchor_idx <- match(det$anchor_age, det$ages)
  for (j in seq_len(ny)) {
    s <- pm_sullivan(Qmat[ , j], det$ages)
    if (det$scope == "all_ages") {
      evi <- pm_target_evi(det$indicator, values[ , j], s$ex)
      p <- pm_invert_evi(evi, s$lx, s$Lx)
      # hypothesis applied from age 'from_age' : younger ages follow the evolution of the odds at this age
      if (!is.null(det$from_age) && det$from_age > min(det$ages)) {
        i0 <- match(det$from_age, det$ages); lo <- seq_len(i0 - 1)
        lg <- function(x) stats::qlogis(pmin(pmax(x, det$eps), 1 - det$eps))
        p[lo] <- stats::plogis(lg(det$p_ref[lo]) + (lg(p[i0]) - lg(det$p_ref[i0])))
      }
      pix[ , j] <- clipped[ , j] <- p
    } else {
      evi <- pm_target_evi(det$indicator, values[anchor_idx, j], s$ex[anchor_idx])
      u <- pm_uniform_prevalences(evi, det$p_ref, s$lx, s$Lx, anchor_idx, det$uniform_scale)
      pix[ , j] <- u$pix; clipped[ , j] <- u$clipped; status[j] <- u$status
    }
  }
  list(pix = pix, clipped = clipped, status = status)
}

# estimation of the method "target" for all series. Returns the elements of the model object.
pm_fit_target <- function(tab, years_fit, ref_year, groupvars, indicator, rule, scope, estimator, change, horizon, after,
                          anchor_age, uniform_scale, eps, from_age = NULL) {

  years_all <- sort(union(years_fit, ref_year))
  tab <- tab[tab$year %in% years_all, , drop = FALSE]
  keys <- if (NROW(groupvars) > 0) do.call(paste, c(tab[groupvars], sep = " / ")) else rep("all data", nrow(tab))
  seriestab <- split(tab, factor(keys, levels = unique(keys)))

  fits <- lapply(names(seriestab), function(k) {
    d <- seriestab[[k]]; ages <- sort(unique(d$age))
    if (anyDuplicated(d[ , c("age","year")]) > 0 | nrow(d) != NROW(ages) * NROW(years_all)) {
      stop(paste0("Series '",k,"': the prevalences and mortality rates must be given exactly once for each age and each year used."), call. = FALSE)
    }
    if (NROW(ages) < 3 || any(diff(ages) != 1)) {
      stop(paste0("Series '",k,"': ages must be consecutive years of age (at least 3), up to the last age of the life table."), call. = FALSE)
    }
    a0 <- ifelse(is.null(anchor_age), min(ages), anchor_age)
    if (!(a0 %in% ages)) { stop(paste0("Series '",k,"': 'anchor_age' (",a0,") is not an age of the series."), call. = FALSE) }
    P <- Q <- matrix(NA_real_, NROW(ages), NROW(years_all), dimnames = list(ages, years_all))
    pos <- cbind(match(d$age, ages), match(d$year, years_all)); P[pos] <- d$pix; Q[pos] <- d$qx
    V <- sapply(seq_along(years_all), function(j) { s <- pm_sullivan(Q[ , j], ages, P[ , j])
      pm_indicator_value(indicator, s$DLEx, s$DFLEx, s$ex) })
    dimnames(V) <- dimnames(P)
    iref <- match(ref_year, years_all)
    if (rule != "trend" && indicator == "ratio" && anyNA(V[ , iref])) {
      stop(paste0("Series '",k,"': with indicator = \"ratio\", the life expectancy with disability must be strictly positive at all ages of the reference year."), call. = FALSE)
    }
    if (!is.null(from_age) && scope == "all_ages" && !(from_age %in% ages)) {
      stop(paste0("Series '",k,"': 'from_age' (",from_age,") is not an age of the series."), call. = FALSE)
    }
    det <- list(ages = ages, years = years_fit, ref_year = ref_year, origin = max(years_fit), indicator = indicator, rule = rule, scope = scope,
                anchor_age = a0, change = change, horizon = horizon, after = after, estimator = estimator, uniform_scale = uniform_scale,
                Iref = V[ , iref], p_ref = P[ , iref], from_age = if (scope == "all_ages") from_age else NULL, eps = eps)
    ny <- NROW(years_fit); se_a <- se_b <- rep(NA_real_, NROW(ages))
    if (rule == "trend") {
      Vf <- V[ , as.character(years_fit), drop = FALSE]; tt <- years_fit - det$origin
      if (any(is.na(Vf))) { stop(paste0("Series '",k,"': the indicator cannot be calculated for all ages and years (zero life expectancy with disability?)."), call. = FALSE) }
      if (estimator == "ols") {
        tbar <- mean(tt); Sxx <- sum((tt - tbar)^2); vbar <- rowMeans(Vf)
        b <- as.vector((Vf - vbar) %*% (tt - tbar)) / Sxx; a <- vbar - b * tbar
        s2 <- rowSums((Vf - (a + outer(b, tt)))^2) / (ny - 2)
        se_b <- sqrt(s2 / Sxx); se_a <- sqrt(s2 * (1 / ny + tbar^2 / Sxx))
      } else {
        b <- (Vf[ , ny] - Vf[ , 1]) / (years_fit[ny] - years_fit[1]); a <- Vf[ , ny]
      }
      det$intercept <- a; det$slope <- b
    }
    # coefficients (for the scope "min_age", only the anchor age is used)
    cf <- switch(rule,
      trend = rbind(data.frame(age = ages, term = "intercept", estimate = det$intercept, std.error = se_a, n = ny),
                    data.frame(age = ages, term = "slope", estimate = det$slope, std.error = se_b, n = ny)),
      data.frame(age = ages, term = "reference", estimate = det$Iref, std.error = NA_real_, n = 1))
    if (scope == "min_age") { cf <- cf[cf$age == a0, ] }
    # fitted prevalences : implied by the hypothesis, with the observed mortality of each year
    pr <- pm_project_target(det, Q[ , as.character(years_fit), drop = FALSE], years_fit)
    pfit <- pr$clipped; pfit <- pmin(pmax(pfit, 0), 1)
    obs <- P[ , as.character(years_fit), drop = FALSE]
    lg <- function(p) stats::qlogis(pmin(pmax(p, eps), 1 - eps))
    resid <- lg(obs) - lg(pfit)
    tss <- sum((lg(obs) - rowMeans(lg(obs)))^2); rss <- sum(resid^2)
    stats <- data.frame(n_obs = length(obs), n_ages = NROW(ages), n_years = ny, sigma = sqrt(rss / length(obs)),
                        r2 = ifelse(tss > 0, 1 - rss / tss, NA_real_))
    list(details = det, coefficients = cf, stats = stats, obs = obs, pfit = pfit, resid = resid)
  })
  names(fits) <- names(seriestab)
  list(fits = fits, seriestab = seriestab, years_all = years_all)
}
