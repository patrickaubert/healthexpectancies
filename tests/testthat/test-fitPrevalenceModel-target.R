tgt  <- simulate_target()                                       # 2010-2040 ; l'EVI est linéaire dans le temps
obs  <- tgt[tgt$year <= 2019, c("year", "age", "qx", "pix")]
projq <- tgt[tgt$year %in% c(2025, 2030, 2040), c("year", "age", "qx")]
ref19 <- CompleteDFLEtable(obs[obs$year == 2019, ]); ref19 <- ref19[order(ref19$age), ]

# indicateurs recalculés à partir de prévalences projetées
indicators <- function(pr) { r <- CompleteDFLEtable(pr[ , c("year", "age", "qx", "pix")]); r[order(r$year, r$age), ] }

# ---------------------------------------------------------------------------------------------------
# portée "all_ages" : cibles à chaque âge
# ---------------------------------------------------------------------------------------------------

test_that("règle constante : mêmes résultats que prevalenceForecast() pour les quatre indicateurs", {
  hyp <- c(dle = "cstDLE", dfle = "cstDFLE", pct_dle = "cstPctDLE", pct_dfle = "cstPctDFLE")
  for (ind in names(hyp)) {
    m  <- fitPrevalenceModel(obs, method = "target", indicator = ind, rule = "constant")
    pr <- predict(m, projq)
    pf <- prevalenceForecast(obs[obs$year == 2019, ], projq, hyp[[ind]])
    pf <- pf[pf$year %in% unique(projq$year), ]
    expect_equal(pr$pix[order(pr$year, pr$age)], pf$pix[order(pf$year, pf$age)], tolerance = 1e-9, info = ind)
  }
})

test_that("règle constante : les prévalences ajustées redonnent les prévalences observées à l'année de référence", {
  m <- fitPrevalenceModel(obs, method = "target", indicator = "dfle", rule = "constant")
  f <- as.data.frame(m, what = "fitted"); f <- f[f$year == 2019, ]
  expect_equal(f$pix_fit, f$pix, tolerance = 1e-9)
  expect_equal(m$settings$ref_year, 2019)
  expect_equal(coef(m)$term, rep("reference", 41))
  m2 <- fitPrevalenceModel(obs, method = "target", indicator = "dle", rule = "constant", ref_year = 2015)
  f2 <- as.data.frame(m2, what = "fitted"); f2 <- f2[f2$year == 2015, ]
  expect_equal(f2$pix_fit, f2$pix, tolerance = 1e-9)
})

test_that("règle de tendance : pentes et projections d'une évolution linéaire de l'EVI sont retrouvées", {
  m <- fitPrevalenceModel(obs, method = "target", indicator = "dle", rule = "trend", fit_years = 2010:2019)
  cf <- coef(m)
  expect_equal(cf$estimate[cf$term == "slope"], -0.004 * (1 + 0.02 * (60:100 - 60)), tolerance = 1e-8)
  expect_equal(cf$estimate[cf$term == "intercept"], tgt$DLEx[tgt$year == 2019], tolerance = 1e-8)
  pr <- predict(m, projq)
  vrai <- tgt[tgt$year %in% c(2025, 2030, 2040), ]
  expect_equal(pr$pix[order(pr$year, pr$age)], vrai$pix[order(vrai$year, vrai$age)], tolerance = 1e-7)
  # l'estimateur à deux points retrouve aussi une tendance exacte
  m2 <- fitPrevalenceModel(obs, method = "target", indicator = "dle", rule = "trend", estimator = "endpoints")
  expect_equal(predict(m2, projq)$pix, pr$pix, tolerance = 1e-7)
  # les prévalences ajustées sont celles observées (données exactes)
  expect_equal(max(abs(residuals(m))), 0, tolerance = 1e-6)
})

test_that("règle de tendance : la part de l'EVI et la part de l'EVSI donnent la même hypothèse", {
  a <- predict(fitPrevalenceModel(obs, method = "target", indicator = "pct_dle",  rule = "trend"), projq)
  b <- predict(fitPrevalenceModel(obs, method = "target", indicator = "pct_dfle", rule = "trend"), projq)
  expect_equal(a$pix, b$pix, tolerance = 1e-9)
})

test_that("règle d'évolution à un horizon : le ratio EVSI/EVI évolue de x % à l'horizon, linéairement avant et après", {
  rho_ref <- ref19$DFLEx / ref19$DLEx
  m <- fitPrevalenceModel(obs, method = "target", indicator = "ratio", rule = "horizon", change = 3, horizon = 2030)
  expect_no_warning(pr <- predict(m, projq))
  ind <- indicators(pr)
  rho <- function(y) { x <- ind[ind$year == y, ]; x$DFLEx / x$DLEx }
  expect_equal(rho(2030), rho_ref * 1.03, tolerance = 1e-8)
  expect_equal(rho(2025), rho_ref * (1 + 0.03 * (2025 - 2019) / (2030 - 2019)), tolerance = 1e-8)
  expect_equal(rho(2040), rho_ref * (1 + 0.03 * (2040 - 2019) / (2030 - 2019)), tolerance = 1e-8)         # after = "linear"
  m2 <- fitPrevalenceModel(obs, method = "target", indicator = "ratio", rule = "horizon", change = 3, horizon = 2030, after = "constant")
  expect_no_warning(pr2 <- predict(m2, projq))
  ind2 <- indicators(pr2)
  expect_equal((ind2$DFLEx / ind2$DLEx)[ind2$year == 2040], rho_ref * 1.03, tolerance = 1e-8)
})

test_that("règle d'évolution à un horizon : la part de l'EVI évolue de x % à l'horizon", {
  m <- fitPrevalenceModel(obs, method = "target", indicator = "pct_dle", rule = "horizon", change = -20, horizon = 2030)
  ind <- indicators(predict(m, projq))
  expect_equal(ind$pctDLEx[ind$year == 2030], ref19$pctDLEx * 0.8, tolerance = 1e-8)
})

# ---------------------------------------------------------------------------------------------------
# portée "min_age" : évolution uniforme déterminée à l'âge minimal
# ---------------------------------------------------------------------------------------------------

test_that("évolution uniforme (odds) : l'EVI à l'âge minimal est constant et les odds de tous les âges évoluent du même facteur", {
  m <- fitPrevalenceModel(obs, method = "target", indicator = "dle", rule = "constant", scope = "min_age")
  expect_equal(nrow(coef(m)), 1)                                   # seul l'âge minimal est utilisé
  pr <- predict(m, projq); ind <- indicators(pr)
  expect_equal(ind$DLEx[ind$age == 60], rep(ref19$DLEx[1], 3), tolerance = 1e-7)
  for (y in c(2025, 2030, 2040)) {
    x <- pr[pr$year == y, ]; p0 <- obs$pix[obs$year == 2019][match(x$age, 60:100)]
    shift <- qlogis(x$pix) - qlogis(p0)
    expect_equal(shift, rep(shift[1], length(shift)), tolerance = 1e-7)
  }
})

test_that("évolution uniforme (niveau) : les prévalences sont multipliées par un même facteur", {
  m <- fitPrevalenceModel(obs, method = "target", indicator = "dle", rule = "constant", scope = "min_age", uniform_scale = "level")
  pr <- predict(m, projq); ind <- indicators(pr)
  expect_equal(ind$DLEx[ind$age == 60], rep(ref19$DLEx[1], 3), tolerance = 1e-7)
  x <- pr[pr$year == 2030, ]; ratio <- x$pix / obs$pix[obs$year == 2019][match(x$age, 60:100)]
  expect_equal(ratio, rep(ratio[1], length(ratio)), tolerance = 1e-7)
})

test_that("évolution uniforme : la cible à l'âge minimal peut suivre une tendance, et l'âge d'ancrage est paramétrable", {
  m <- fitPrevalenceModel(obs, method = "target", indicator = "dle", rule = "trend", scope = "min_age")
  expect_equal(nrow(coef(m)), 2)
  ind <- indicators(predict(m, projq))
  expect_equal(ind$DLEx[ind$age == 60], tgt$DLEx[tgt$year %in% c(2025, 2030, 2040) & tgt$age == 60], tolerance = 1e-7)
  m70 <- fitPrevalenceModel(obs, method = "target", indicator = "dle", rule = "constant", scope = "min_age", anchor_age = 70)
  ind70 <- indicators(predict(m70, projq))
  expect_equal(ind70$DLEx[ind70$age == 70], rep(ref19$DLEx[ref19$age == 70], 3), tolerance = 1e-7)
})

# ---------------------------------------------------------------------------------------------------
# cibles infaisables
# ---------------------------------------------------------------------------------------------------

test_that("une cible qui impose des prévalences hors de [0,1] est signalée, bornée ou provoque une erreur", {
  m <- fitPrevalenceModel(obs, method = "target", indicator = "pct_dle", rule = "horizon", change = 300, horizon = 2030)
  expect_warning(pr <- predict(m, projq), "outside the interval")
  expect_true(any(pr$pix > 1 | pr$pix < 0))
  expect_warning(prc <- predict(m, projq, on_unfeasible = "clip"), "bounded")
  expect_true(all(prc$pix >= 0 & prc$pix <= 1))
  expect_error(predict(m, projq, on_unfeasible = "error"), "outside the interval")
})

test_that("évolution uniforme sans solution : NA avec un avertissement, borne avec 'clip', erreur avec 'error'", {
  m <- fitPrevalenceModel(obs, method = "target", indicator = "pct_dle", rule = "horizon", change = 2000, horizon = 2030, scope = "min_age")
  expect_warning(pr <- predict(m, projq), "no uniform evolution")
  expect_true(any(is.na(pr$pix[pr$year >= 2030])))
  expect_true(all(is.na(pr$pix[pr$year == 2030])))
  expect_warning(prc <- predict(m, projq, on_unfeasible = "clip"), "bounded")
  expect_false(anyNA(prc$pix)); expect_true(all(prc$pix >= 0 & prc$pix <= 1))
  expect_error(predict(m, projq, on_unfeasible = "error"), "no uniform evolution")
})

# ---------------------------------------------------------------------------------------------------
# contrôles, séries et méthodes S3
# ---------------------------------------------------------------------------------------------------

test_that("les paramètres de la règle d'horizon et les arguments incohérents sont contrôlés", {
  expect_error(fitPrevalenceModel(obs, method = "target", rule = "horizon"), "'change'")
  expect_error(fitPrevalenceModel(obs, method = "target", rule = "horizon", change = 10), "'horizon'")
  expect_error(fitPrevalenceModel(obs, method = "target", rule = "horizon", change = 10, horizon = 2019), "later than")
  expect_error(fitPrevalenceModel(obs, method = "target", ref_year = 1999), "not a year")
  expect_error(fitPrevalenceModel(obs, method = "target", scope = "min_age", anchor_age = 30), "anchor_age")
  expect_error(fitPrevalenceModel(obs[ , c("year", "age", "pix")], method = "target"), "'qx'")
  expect_warning(fitPrevalenceModel(obs, method = "trend", rule = "trend"), "ignored unless")
  expect_warning(fitPrevalenceModel(obs, method = "target", scale = "log"), "ignored")
  gap <- obs[obs$age != 80, ]
  expect_error(fitPrevalenceModel(gap, method = "target"), "exactly once|consecutive")
})

test_that("predict exige des taux de mortalité pour tous les âges du modèle", {
  m <- fitPrevalenceModel(obs, method = "target", indicator = "dle", rule = "constant")
  expect_error(predict(m, projq[projq$age < 100, ]), "all ages of the model")
  expect_error(predict(m, projq[ , c("year", "age")]), "'qx'")
  expect_warning(predict(m, projq, interval = TRUE), "not available")
})

test_that("plusieurs séries : chacune a sa référence ; les méthodes S3 et projectPrevalence fonctionnent", {
  a <- obs; a$sex <- "female"; b <- obs; b$sex <- "male"; b$pix <- pmin(1, b$pix * 1.5)
  both <- rbind(a, b)
  m <- fitPrevalenceModel(both, method = "target", indicator = "dfle", rule = "constant")
  expect_equal(NROW(m$series), 2)
  q2 <- rbind(transform(projq, sex = "female"), transform(projq, sex = "male"))
  pr <- predict(m, q2)
  expect_equal(nrow(pr), 2 * 3 * 41)
  solo <- predict(fitPrevalenceModel(b[ , c("year", "age", "qx", "pix")], method = "target", indicator = "dfle", rule = "constant"), projq)
  expect_equal(pr$pix[pr$sex == "male"][order(pr$year[pr$sex == "male"], pr$age[pr$sex == "male"])],
               solo$pix[order(solo$year, solo$age)], tolerance = 1e-9)
  expect_output(print(m), "Indicator: dfle")
  expect_output(print(summary(m)), "Fit statistics")
  expect_equal(names(as.data.frame(m, what = "fitted")), c("sex", "year", "age", "pix", "pix_fit", "resid"))
  res <- projectPrevalence(both, newdata = q2, method = "target", indicator = "dle", rule = "trend")
  expect_true(all(c("pix", "ex", "DFLEx", "DLEx") %in% names(res)))
  expect_s3_class(attr(res, "model"), "prevalence_model")
})

# ---------------------------------------------------------------------------------------------------
# from_age : hypothèse appliquée à partir d'un âge, les âges plus jeunes suivent les odds de cet âge
# ---------------------------------------------------------------------------------------------------

test_that("from_age : mêmes résultats que l'inversion complète aux âges supérieurs, et mêmes décalages d'odds en dessous", {
  args <- list(tab = obs, method = "target", indicator = "ratio", rule = "horizon", change = 3, horizon = 2030)
  full <- predict(do.call(fitPrevalenceModel, args), projq)
  part <- predict(do.call(fitPrevalenceModel, c(args, from_age = 70)), projq)
  expect_equal(part$pix[part$age >= 70], full$pix[full$age >= 70], tolerance = 1e-9)
  for (y in c(2025, 2030, 2040)) {
    x <- part[part$year == y, ]; p0 <- obs$pix[obs$year == 2019][match(x$age, 60:100)]
    shift <- qlogis(x$pix) - qlogis(p0)
    expect_equal(shift[x$age < 70], rep(shift[x$age == 70], sum(x$age < 70)), tolerance = 1e-9)
  }
})

test_that("from_age : évite les prévalences négatives aux jeunes âges, l'hypothèse restant vérifiée à partir de cet âge", {
  args <- list(tab = obs, method = "target", indicator = "ratio", rule = "horizon", change = 10, horizon = 2030)
  expect_warning(predict(do.call(fitPrevalenceModel, args), projq), "outside the interval")
  m <- do.call(fitPrevalenceModel, c(args, from_age = 65))
  expect_no_warning(pr <- predict(m, projq))
  expect_true(all(pr$pix > 0 & pr$pix < 1))
  ind <- indicators(pr); rho_ref <- ref19$DFLEx / ref19$DLEx
  x <- ind[ind$year == 2030, ]
  expect_equal((x$DFLEx / x$DLEx)[x$age >= 65], (rho_ref * 1.10)[ref19$age >= 65], tolerance = 1e-8)
  expect_output(print(m), "Applied from age 65")
})

test_that("from_age : contrôles", {
  expect_error(fitPrevalenceModel(obs, method = "target", from_age = 30), "from_age")
  expect_warning(fitPrevalenceModel(obs, method = "target", scope = "min_age", from_age = 70), "ignored")
  expect_warning(fitPrevalenceModel(obs, method = "trend", from_age = 70), "ignored unless")
})
