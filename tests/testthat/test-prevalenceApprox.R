# Données de test : prévalence logistique 'vraie' par âge, agrégée en tranches de 5 ans -------------

ages_test  <- 60:99
weight_test <- exp(-0.08 * (ages_test - 60)) * 1e5          # population décroissante avec l'âge
cuts_test  <- seq(60, 95, 5)
bracket_means <- function(p, w = weight_test, cuts = cuts_test) {
  br <- findInterval(ages_test, cuts)
  sapply(seq_along(cuts), function(b) sum(p[br == b] * w[br == b]) / sum(w[br == b]))
}
truth_logistic <- plogis(-6.3 + 0.17 * (ages_test - 60))    # 0,2 % à 60 ans, environ 45 % à 99 ans
prev_logistic  <- bracket_means(truth_logistic)

approx_test <- function(prev, ...) prevalenceApprox(prev, cuts_test, 60, 99, weight_test, ...)

# ---------------------------------------------------------------------------

test_that("les deux méthodes respectent les prévalences observées par tranche d'âge", {
  for (m in c("logit", "legacy")) {
    est <- approx_test(prev_logistic, method = m)
    expect_length(est, 40)
    expect_equal(bracket_means(est), prev_logistic, tolerance = 1e-8)
  }
})

test_that("la méthode logit renvoie des prévalences strictement comprises entre 0 et 1", {
  est <- approx_test(prev_logistic)
  expect_true(all(est > 0 & est < 1))
})

test_that("la méthode logit retrouve une courbe logistique, y compris aux premiers âges", {
  est <- approx_test(prev_logistic)
  expect_lt(abs(est[1] / truth_logistic[1] - 1), 0.01)         # erreur relative à 60 ans < 1 %
  expect_lt(max(abs(est - truth_logistic)), 1e-3)
})

test_that("la méthode 'legacy' sous-estime fortement la prévalence au premier âge (biais documenté)", {
  est <- approx_test(prev_logistic, method = "legacy")
  expect_lt(est[1] / truth_logistic[1], 0.6)
})

test_that("un plateau est restitué à l'identique", {
  est <- approx_test(rep(0.3, 8))
  expect_equal(est, rep(0.3, 40), tolerance = 1e-6)
})

test_that("la méthode par défaut est 'logit'", {
  expect_equal(approx_test(prev_logistic), approx_test(prev_logistic, method = "logit"))
})

test_that("des prévalences exprimées en pourcentage provoquent une erreur claire", {
  expect_error(approx_test(prev_logistic * 100), "between 0 and 1")
  expect_no_error(approx_test(prev_logistic * 100, method = "legacy"))
})

test_that("les arguments incohérents sont signalés", {
  expect_error(prevalenceApprox(prev_logistic, cuts_test, 55, 99, rep(1, 45)), "agemin")
  expect_error(prevalenceApprox(prev_logistic, rev(cuts_test), 60, 99, weight_test), "increasing")
  expect_error(prevalenceApprox(prev_logistic, cuts_test, 60, 80, weight_test[1:21]), "No age")
  expect_error(approx_test(c(NA, prev_logistic[-1])), "Missing")
})

test_that("une prévalence nulle dans une tranche est remplacée par une borne, avec un avertissement", {
  prev0 <- prev_logistic; prev0[1] <- 0
  expect_warning(est <- approx_test(prev0), "replaced")
  expect_true(all(is.finite(est)) && all(est > 0))
})

test_that("le calcul par sexe, année et catégories renvoie un tableau cohérent", {
  prev_df <- data.frame(sex = rep(c("female","male"), each = 8), prevalence = c(prev_logistic, prev_logistic / 2))
  w_df <- data.frame(sex = rep(c("female","male"), each = 40), weight = rep(weight_test, 2))
  res <- prevalenceApprox(prev_df, cuts_test, 60, 99, w_df)
  expect_equal(nrow(res), 80)
  expect_equal(res$prevalence[res$sex == "female"], approx_test(prev_logistic))
  expect_equal(res$prevalence[res$sex == "male"],   approx_test(prev_logistic / 2))
  res_l <- prevalenceApprox(prev_df, cuts_test, 60, 99, w_df, method = "legacy")
  expect_equal(res_l$prevalence[res_l$sex == "female"], approx_test(prev_logistic, method = "legacy"))
})

test_that("ApproxPrevalenceTable utilise logit par défaut et accepte option = 'legacy'", {
  tab <- data.frame(sex = "female", age = cuts_test, prevalence = prev_logistic)
  wtab <- data.frame(sex = "female", age = ages_test, popx = weight_test)
  new <- suppressMessages(ApproxPrevalenceTable(tab, cuts_test, 60, 99, weights.tab = wtab))
  old <- suppressMessages(ApproxPrevalenceTable(tab, cuts_test, 60, 99, weights.tab = wtab, option = "legacy"))
  expect_equal(new$prevalence.approx, approx_test(prev_logistic))
  expect_equal(old$prevalence.approx, approx_test(prev_logistic, method = "legacy"))
  expect_equal(new$prevalence.raw[1], prev_logistic[1])
})
