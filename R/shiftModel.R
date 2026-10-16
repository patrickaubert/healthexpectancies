# Internal functions for the method "shift" of fitPrevalenceModel() :
# the curve of prevalences by age of a reference year moves to the right (towards older ages) at a constant speed.

# curve of the logit of prevalences of the reference year, at any (non necessarily integer) age:
# linear interpolation between ages, linear extrapolation beyond the first and the last ages
# (slope estimated by least squares on the 'n_extrap' first or last ages)
pm_shift_curve <- function(det, x) {
  ages <- det$ages; y <- det$Yref; n <- NROW(ages); m <- det$n_extrap
  slope_lo <- stats::coef(stats::lm(y[1:m] ~ ages[1:m]))[[2]]
  slope_hi <- stats::coef(stats::lm(y[(n - m + 1):n] ~ ages[(n - m + 1):n]))[[2]]
  res <- stats::approx(ages, y, xout = x, rule = 1)$y
  below <- !is.na(x) & x < ages[1]; above <- !is.na(x) & x > ages[n]
  res[below] <- y[1] + slope_lo * (x[below] - ages[1])
  res[above] <- y[n] + slope_hi * (x[above] - ages[n])
  res
}

# estimation of the method "shift" for all series
pm_fit_shift <- function(tab, years_fit, ref_year, groupvars, n_extrap, eps, dmax = 20) {

  years_all <- sort(union(years_fit, ref_year))
  tab <- tab[tab$year %in% years_all, , drop = FALSE]
  keys <- if (NROW(groupvars) > 0) do.call(paste, c(tab[groupvars], sep = " / ")) else rep("all data", nrow(tab))
  seriestab <- split(tab, factor(keys, levels = unique(keys)))
  lg <- function(p) stats::qlogis(pmin(pmax(p, eps), 1 - eps))

  fits <- lapply(names(seriestab), function(k) {
    d <- seriestab[[k]]; ages <- sort(unique(d$age))
    if (anyDuplicated(d[ , c("age","year")]) > 0 | nrow(d) != NROW(ages) * NROW(years_all)) {
      stop(paste0("Series '",k,"': the prevalences must be given exactly once for each age and each year used."), call. = FALSE)
    }
    if (NROW(ages) < 3 || n_extrap > NROW(ages) || n_extrap < 2) {
      stop(paste0("Series '",k,"': at least 3 ages are needed, and 'n_extrap' must be between 2 and the number of ages."), call. = FALSE)
    }
    Y <- matrix(NA_real_, NROW(ages), NROW(years_all), dimnames = list(ages, years_all))
    Y[cbind(match(d$age, ages), match(d$year, years_all))] <- lg(d$pix)
    det <- list(ages = ages, years = years_fit, ref_year = ref_year, origin = ref_year, Yref = Y[ , as.character(ref_year)], n_extrap = n_extrap, shift = NA_real_)
    others <- setdiff(years_fit, ref_year)
    if (NROW(others) < 2) {
      stop("At least 2 years other than the reference year are needed to estimate the shift.", call. = FALSE)
    }
    dt <- sapply(others, function(t) {
      stats::optimize(function(dd) sum((Y[ , as.character(t)] - pm_shift_curve(det, ages - dd))^2), c(-dmax, dmax))$minimum
    })
    tau <- others - ref_year
    s <- sum(dt * tau) / sum(tau^2)
    s2 <- sum((dt - s * tau)^2) / (NROW(others) - 1); se <- sqrt(s2 / sum(tau^2))
    det$shift <- s
    # fitted values for the years of the estimation
    Yfit <- sapply(years_fit, function(t) pm_shift_curve(det, ages - s * (t - ref_year)))
    Yobs <- Y[ , as.character(years_fit), drop = FALSE]; dimnames(Yfit) <- dimnames(Yobs)
    resid <- Yobs - Yfit
    tss <- sum((Yobs - rowMeans(Yobs))^2); rss <- sum(resid^2)
    shifts <- data.frame(year = years_fit, shift_obs = ifelse(years_fit == ref_year, 0, NA_real_), shift_fit = s * (years_fit - ref_year))
    shifts$shift_obs[match(others, years_fit)] <- dt
    list(details = det,
         coefficients = data.frame(age = NA_real_, term = "shift", estimate = s, std.error = se, n = NROW(others)),
         stats = data.frame(n_obs = length(Yobs), n_ages = NROW(ages), n_years = NROW(years_fit),
                            sigma = sqrt(rss / length(Yobs)), r2 = ifelse(tss > 0, 1 - rss / tss, NA_real_)),
         shifts = shifts, obs = stats::plogis(Yobs), pfit = stats::plogis(Yfit), resid = resid)
  })
  names(fits) <- names(seriestab)
  list(fits = fits, seriestab = seriestab, years_all = years_all)
}
