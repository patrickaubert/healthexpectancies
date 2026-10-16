# courbe de référence linéaire en logit : un décalage horizontal équivaut à un décalage vertical, retrouvé exactement
shifted_curves <- function(s0, years = 2010:2019, ages = 60:100, t0 = 2019) {
  d <- expand.grid(age = ages, year = years)
  d$pix <- plogis(-5 + 0.12 * (d$age - s0 * (d$year - t0) - 60))
  d
}
truth <- function(s0, years, ages = 60:100, t0 = 2019) {
  d <- expand.grid(age = ages, year = years)
  plogis(-5 + 0.12 * (d$age - s0 * (d$year - t0) - 60))
}

test_that("shift : la vitesse de décalage d'une courbe exactement décalée est retrouvée (vers les âges élevés ou faibles)", {
  for (s0 in c(0.1, -0.15)) {
    m <- fitPrevalenceModel(shifted_curves(s0), method = "shift")
    expect_equal(coef(m)$estimate, s0, tolerance = 1e-6)
    expect_equal(coef(m)$term, "shift"); expect_true(is.na(coef(m)$age))
    expect_equal(max(abs(residuals(m))), 0, tolerance = 1e-5)
    sh <- as.data.frame(m, what = "shifts")
    expect_equal(sh$shift_obs, s0 * (sh$year - 2019), tolerance = 1e-5)
    expect_equal(sh$shift_fit, s0 * (sh$year - 2019), tolerance = 1e-8)
  }
})

test_that("shift : la projection prolonge le décalage, y compris hors des âges de la table", {
  m <- fitPrevalenceModel(shifted_curves(0.1), method = "shift")
  nd <- expand.grid(age = 60:100, year = c(2025, 2030))
  pr <- predict(m, nd)
  expect_equal(pr$pix, truth(0.1, c(2025, 2030)), tolerance = 1e-6)
  expect_equal(nrow(pr), nrow(nd))
})

test_that("shift : une courbe de référence non linéaire est retrouvée de façon approchée", {
  d <- expand.grid(age = 60:100, year = 2010:2019)
  f <- function(x) -5 + 0.12 * (x - 60) + 0.002 * (x - 60)^2
  d$pix <- plogis(f(d$age - 0.1 * (d$year - 2019)))
  m <- fitPrevalenceModel(d, method = "shift")
  expect_lt(abs(coef(m)$estimate - 0.1), 0.01)
})

test_that("shift : fit_years, exclude_years et ref_year", {
  d <- shifted_curves(0.1, years = 2005:2019); bad <- d; bad$pix[bad$year == 2012] <- plogis(-3 + 0.05 * (bad$age[bad$year == 2012] - 60))
  m_incl <- fitPrevalenceModel(bad, method = "shift", fit_years = 2010:2019)
  m_excl <- fitPrevalenceModel(bad, method = "shift", fit_years = 2010:2019, exclude_years = 2012)
  expect_equal(coef(m_excl)$estimate, 0.1, tolerance = 1e-6)
  expect_gt(abs(coef(m_incl)$estimate - 0.1), 1e-4)
  expect_equal(m_excl$settings$fit_years, setdiff(2010:2019, 2012))
  m_ref <- fitPrevalenceModel(d, method = "shift", ref_year = 2015)
  expect_equal(coef(m_ref)$estimate, 0.1, tolerance = 1e-6)
  expect_equal(predict(m_ref, expand.grid(age = 60:100, year = 2025))$pix, truth(0.1, 2025), tolerance = 1e-6)
})

test_that("shift : une vitesse par série (sexe, catégories)", {
  a <- shifted_curves(0.1); a$sex <- "female"; b <- shifted_curves(0.2); b$sex <- "male"
  m <- fitPrevalenceModel(rbind(a, b), method = "shift")
  expect_equal(coef(m)$estimate[order(coef(m)$sex)], c(0.1, 0.2), tolerance = 1e-6)
  pr <- predict(m, expand.grid(age = 60:100, year = 2025))
  expect_equal(sort(unique(pr$sex)), c("female", "male"))
  expect_equal(pr$pix[pr$sex == "male"], truth(0.2, 2025), tolerance = 1e-6)
})

test_that("shift : contrôles, avertissements et méthodes S3", {
  d <- shifted_curves(0.1)
  expect_error(fitPrevalenceModel(d, method = "shift", ref_year = 1999), "not a year")
  expect_error(fitPrevalenceModel(d, method = "shift", fit_years = c(2018, 2019)), "At least 3 years")
  expect_warning(fitPrevalenceModel(d, method = "shift", fit_years = 2017:2019), "Only 3 years")          # le minimum : 3 années, dont l'année de référence
  expect_error(fitPrevalenceModel(d, method = "shift", n_extrap = 1), "n_extrap")
  expect_error(fitPrevalenceModel(d, method = "shift", n_extrap = 50), "n_extrap")
  expect_warning(fitPrevalenceModel(d, method = "shift", scale = "log"), "ignored")
  expect_warning(fitPrevalenceModel(d, method = "trend", n_extrap = 3), "ignored unless")
  expect_warning(fitPrevalenceModel(d, method = "trend", ref_year = 2019), "ignored unless")
  m <- fitPrevalenceModel(d, method = "shift")
  expect_warning(predict(m, expand.grid(age = 60:100, year = 2025), interval = TRUE), "not available")
  expect_warning(predict(m, expand.grid(age = 60:100, year = 2060)), "Extrapolation")
  expect_error(as.data.frame(fitPrevalenceModel(d, method = "trend"), what = "shifts"), "shifts")
  expect_output(print(m), "shift of the curve")
  expect_output(print(summary(m)), "Shift of the curve")
  expect_equal(names(as.data.frame(m, what = "fitted")), c("year", "age", "pix", "pix_fit", "resid"))
  bad <- d; bad$pix[1] <- 1.2
  expect_error(fitPrevalenceModel(bad, method = "shift"), "between 0 and 1")
})

test_that("shift : projectPrevalence calcule les espérances de vie", {
  q <- simulate_qx(ages = 60:100, years = 2010:2040)
  res <- projectPrevalence(shifted_curves(0.1), newdata = q[q$year %in% c(2025, 2030), ], method = "shift")
  expect_true(all(c("pix", "ex", "DFLEx", "DLEx", "pctDLEx") %in% names(res)))
  expect_equal(sort(unique(res$year)), c(2025, 2030))
})

test_that("shift : la vitesse est la pente, sans constante, de la régression des décalages annuels sur l'écart à l'année de référence", {
  years <- 2010:2019; tau <- years - 2019
  dt <- c(-0.9, -1.2, -0.5, -0.8, -0.3, -0.6, -0.1, -0.2, -0.15, 0)         # décalages annuels irréguliers
  d <- expand.grid(age = 60:100, year = years)
  d$pix <- plogis(-5 + 0.12 * (d$age - dt[match(d$year, years)] - 60))
  m <- fitPrevalenceModel(d, method = "shift")
  expect_equal(coef(m)$estimate, sum(dt * tau) / sum(tau^2), tolerance = 1e-6)
  expect_equal(as.data.frame(m, what = "shifts")$shift_obs, dt, tolerance = 1e-5)
  expect_gt(coef(m)$std.error, 0)
})
