# ---------------------------------------------------------------------------------------------------
# Méthode "trend"
# ---------------------------------------------------------------------------------------------------

test_that("trend : les coefficients d'une tendance logit exacte sont retrouvés", {
  s <- simulate_trend()
  m <- fitPrevalenceModel(s$data, method = "trend")
  cf <- coef(m)
  expect_s3_class(m, "prevalence_model")
  expect_equal(cf$estimate[cf$term == "intercept"], s$alpha, tolerance = 1e-8)   # niveau en 2019 (dernière année)
  expect_equal(cf$estimate[cf$term == "slope"], s$beta, tolerance = 1e-8)
  expect_equal(unique(cf$n), 10)
})

test_that("trend : la projection prolonge la tendance", {
  s <- simulate_trend()
  m <- fitPrevalenceModel(s$data, method = "trend")
  nd <- expand.grid(age = s$ages, year = c(2025, 2035))
  expect_warning(pr <- predict(m, nd), NA)
  vrai <- plogis(s$alpha[match(nd$age, s$ages)] + s$beta[match(nd$age, s$ages)] * (nd$year - 2019))
  expect_equal(pr$pix, vrai, tolerance = 1e-8)
  expect_equal(nrow(pr), nrow(nd))
})

test_that("trend : l'estimateur 'endpoints' retrouve aussi une tendance exacte, avec 2 années seulement", {
  s <- simulate_trend(years = 2010:2019)
  m <- fitPrevalenceModel(s$data, method = "trend", estimator = "endpoints")
  expect_equal(coef(m)$estimate[coef(m)$term == "slope"], s$beta, tolerance = 1e-8)
  m2 <- fitPrevalenceModel(s$data, method = "trend", estimator = "endpoints", fit_years = c(2015, 2019))
  expect_equal(coef(m2)$estimate[coef(m2)$term == "slope"], s$beta, tolerance = 1e-8)
})

test_that("trend : fit_years et exclude_years choisissent les années d'estimation", {
  s <- simulate_trend(years = 2005:2019)
  d <- s$data; d$pix[d$year == 2012] <- 0.5                          # une année aberrante
  m_all  <- fitPrevalenceModel(d, method = "trend", fit_years = 2010:2019)
  m_excl <- fitPrevalenceModel(d, method = "trend", fit_years = 2010:2019, exclude_years = 2012)
  expect_equal(m_all$settings$fit_years, 2010:2019)
  expect_equal(m_excl$settings$fit_years, setdiff(2010:2019, 2012))
  expect_equal(coef(m_excl)$estimate[coef(m_excl)$term == "slope"], s$beta, tolerance = 1e-8)
  expect_gt(max(abs(coef(m_all)$estimate[coef(m_all)$term == "slope"] - s$beta)), 1e-3)
})

test_that("trend : l'échelle 'level' et 'log' fonctionnent", {
  s <- simulate_trend()
  expect_no_error(m1 <- fitPrevalenceModel(s$data, method = "trend", scale = "level"))
  expect_no_error(m2 <- fitPrevalenceModel(s$data, method = "trend", scale = "log"))
  expect_equal(fitted(m2), s$data$pix[order(s$data$year, s$data$age)] , tolerance = 1e-1)
})

test_that("trend : les intervalles de prédiction encadrent la prévision et s'élargissent avec le niveau", {
  set.seed(1)
  s <- simulate_trend(); d <- s$data; d$pix <- plogis(qlogis(d$pix) + rnorm(nrow(d), sd = 0.05))
  m <- fitPrevalenceModel(d, method = "trend")
  nd <- expand.grid(age = s$ages, year = 2030)
  p95 <- predict(m, nd, interval = TRUE, level = 0.95); p80 <- predict(m, nd, interval = TRUE, level = 0.80)
  expect_true(all(p95$pix_lwr < p95$pix & p95$pix < p95$pix_upr))
  expect_true(all(p95$pix_upr - p95$pix_lwr > p80$pix_upr - p80$pix_lwr))
  expect_warning(predict(fitPrevalenceModel(s$data, method = "trend", estimator = "endpoints"), nd, interval = TRUE), "not available")
})

test_that("trend : une extrapolation très lointaine est signalée", {
  s <- simulate_trend()
  m <- fitPrevalenceModel(s$data, method = "trend")                    # période d'estimation : 9 ans
  expect_warning(predict(m, expand.grid(age = s$ages, year = 2050)), "Extrapolation")
})

# ---------------------------------------------------------------------------------------------------
# Méthode "mortality"
# ---------------------------------------------------------------------------------------------------

test_that("mortality : la pente et les effets d'âge d'un lien exact sont retrouvés", {
  s <- simulate_mortality(k = 3)
  m <- fitPrevalenceModel(s$data, method = "mortality", age_lag = 3)
  cf <- coef(m)
  expect_equal(cf$estimate[cf$term == "beta"], 1.5, tolerance = 1e-8)
  expect_true(is.na(cf$age[cf$term == "beta"]))                         # pente commune : age = NA
  # intercepts : alpha_a estimés = alpha_a vrais (ils absorbent la moyenne de ln q)
  expect_equal(cf$estimate[cf$term == "alpha"], s$alpha, tolerance = 1e-8)
})

test_that("mortality : la projection avec la mortalité projetée redonne les prévalences exactes", {
  s <- simulate_mortality(k = 3)
  m <- fitPrevalenceModel(s$data, method = "mortality", age_lag = 3)
  nd <- s$qx_all[s$qx_all$year %in% c(2030, 2040), ]
  pr <- predict(m, nd)
  truth <- simulate_mortality(k = 3, years = c(2030, 2040))$data
  expect_equal(pr$pix[order(pr$year, pr$age)], truth$pix[order(truth$year, truth$age)], tolerance = 1e-8)
})

test_that("mortality : age_lag fournit un vecteur, le meilleur décalage est retenu (entier ou non)", {
  for (k in c(0, 2, 3, 2.5)) {
    s <- simulate_mortality(k = k)
    m <- fitPrevalenceModel(s$data, method = "mortality", age_lag = seq(0, 5, by = 0.5))
    expect_equal(m$settings$age_lag$age_lag, k)
    ls <- as.data.frame(m, what = "lag_search")
    expect_equal(ls$age_lag[ls$selected], k)
    expect_equal(nrow(ls), 11)
  }
})

test_that("mortality : la pente par âge retrouve la pente commune sur des données exactes", {
  s <- simulate_mortality(k = 2)
  m <- fitPrevalenceModel(s$data, method = "mortality", slope = "by_age", age_lag = 2)
  cf <- coef(m); b <- cf$estimate[cf$term == "beta" & cf$age < 98]                    # aux âges a + k > 100, ln q est constant : pas de pente
  expect_equal(b, rep(1.5, length(b)), tolerance = 1e-6)
})

test_that("mortality : qx doit couvrir tous les âges du modèle dans newdata", {
  s <- simulate_mortality(k = 3)
  m <- fitPrevalenceModel(s$data, method = "mortality", age_lag = 3)
  nd <- s$qx_all[s$qx_all$year == 2030 & s$qx_all$age < 90, ]
  expect_error(predict(m, nd), "all ages of the model")
  expect_error(predict(m, nd[ , c("year", "age")]), "'qx'")
})

test_that("mortality : intervalle de prédiction pour la pente commune", {
  set.seed(2)
  s <- simulate_mortality(k = 3); d <- s$data; d$pix <- plogis(qlogis(d$pix) + rnorm(nrow(d), sd = 0.05))
  m <- fitPrevalenceModel(d, method = "mortality", age_lag = 3)
  pr <- predict(m, s$qx_all[s$qx_all$year == 2030, ], interval = TRUE)
  expect_true(all(pr$pix_lwr < pr$pix & pr$pix < pr$pix_upr))
})

# ---------------------------------------------------------------------------------------------------
# Séries, contrôles et méthodes S3
# ---------------------------------------------------------------------------------------------------

test_that("categories et sex définissent des séries : estimer ensemble ou séparément donne les mêmes résultats", {
  s <- simulate_trend()$data
  a <- s; a$lieu <- "dom"; b <- s; b$lieu <- "etab"; b$pix <- plogis(qlogis(b$pix) + 1 + 0.01 * (b$year - 2019))
  both <- rbind(a, b)
  m <- fitPrevalenceModel(both, method = "trend", categories = "lieu")
  mb <- fitPrevalenceModel(b[ , c("age", "year", "pix")], method = "trend")
  expect_equal(NROW(m$series), 2)
  expect_equal(coef(m)$estimate[coef(m)$lieu == "etab"], coef(mb)$estimate)
  # projection : une variable de série absente de newdata est étendue à toutes ses valeurs
  pr <- predict(m, expand.grid(age = 60:100, year = 2025))
  expect_equal(sort(unique(pr$lieu)), c("dom", "etab"))
  expect_equal(nrow(pr), 2 * 41)
})

test_that("les contrôles d'entrée signalent les erreurs", {
  s <- simulate_trend()$data
  expect_error(fitPrevalenceModel(s[ , c("age", "year")]), "'pix'")
  expect_error(fitPrevalenceModel(s, method = "mortality"), "'qx'")
  expect_error(fitPrevalenceModel(s, fit_years = 2000:2005), "not found")
  expect_error(fitPrevalenceModel(s, fit_years = 2018:2019), "At least 3 years")
  expect_error(fitPrevalenceModel(s, categories = "zzz"), "not found in 'tab'")
  bad <- s; bad$pix[1] <- NA
  expect_error(fitPrevalenceModel(bad), "Missing values")
  bad <- s[-1, ]
  expect_error(fitPrevalenceModel(bad), "exactly once")
  pct <- s; pct$pix <- pct$pix * 100
  expect_error(fitPrevalenceModel(pct), "between 0 and 1")
})

test_that("des arguments propres à une autre méthode sont signalés", {
  s <- simulate_trend()$data
  expect_warning(fitPrevalenceModel(s, method = "trend", age_lag = 2), "ignored")
})

test_that("des prévalences nulles sont bornées avec un avertissement", {
  s <- simulate_trend()$data; s$pix[s$age == 60] <- 0
  expect_warning(m <- fitPrevalenceModel(s), "replaced by bounds")
  expect_true(all(is.finite(coef(m)$estimate)))
})

test_that("les méthodes S3 renvoient les résultats de l'estimation", {
  s <- simulate_trend()$data
  m <- fitPrevalenceModel(s, method = "trend")
  expect_equal(nrow(coef(m)), 2 * 41)
  expect_equal(length(fitted(m)), nrow(s)); expect_equal(length(residuals(m)), nrow(s))
  expect_equal(names(as.data.frame(m, what = "fitted")), c("year", "age", "pix", "pix_fit", "resid"))
  expect_equal(nrow(as.data.frame(m, what = "observed")), nrow(s))
  expect_equal(nrow(as.data.frame(m, what = "fit_stats")), 1)
  expect_error(as.data.frame(m, what = "lag_search"), "lag_search")
  expect_output(print(m), "trend")
  expect_output(print(summary(m)), "Fit statistics")
  expect_equal(max(abs(residuals(m))), 0, tolerance = 1e-8)
})

test_that("projectPrevalence enchaîne estimation, projection et calcul des espérances de vie", {
  s <- simulate_mortality(k = 3)
  res <- projectPrevalence(s$data, newdata = s$qx_all[s$qx_all$year %in% c(2030, 2040), ], method = "mortality", age_lag = 3)
  expect_true(all(c("pix", "ex", "DFLEx", "DLEx", "pctDFLEx") %in% names(res)))
  expect_s3_class(attr(res, "model"), "prevalence_model")
  expect_equal(sort(unique(res$year)), c(2030, 2040))
  expect_error(projectPrevalence(s$data, newdata = s$qx_all[ , c("year", "age")], method = "mortality"), "qx")
})
