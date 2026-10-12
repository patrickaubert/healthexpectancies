# Données de test ------------------------------------------------------------------------------------

ages_t  <- 60:99
cuts_t  <- seq(60, 95, 5)
w_t     <- exp(-0.08 * (ages_t - 60)) * 1e5
prev_t  <- local({
  vrai <- plogis(-6.3 + 0.17 * (ages_t - 60)); br <- findInterval(ages_t, cuts_t)
  sapply(seq_along(cuts_t), function(b) sum(vrai[br == b] * w_t[br == b]) / sum(w_t[br == b]))
})

# table de prévalences par tranche d'âge, pour 2 catégories de lieu
tab_t  <- data.frame(lieu = rep(c("dom", "etab"), each = 8), age = rep(cuts_t, 2),
                     prevalence = c(prev_t, prev_t / 2))
# poids par âge, pour les mêmes catégories
wtab_t <- data.frame(lieu = rep(c("dom", "etab"), each = 40), age = rep(ages_t, 2), popx = rep(w_t, 2))

approx_tab <- function(tab = tab_t, wtab = wtab_t, ...) {
  suppressMessages(ApproxPrevalenceTable(tab, cuts_t, 60, 99, weights.tab = wtab, categories = "lieu", ...))
}

# ------------------------------------------------------------------------------------------------------

test_that("entrées valides : résultat inchangé et conforme à prevalenceApprox", {
  res <- approx_tab()
  expect_equal(nrow(res), 80)
  expect_equal(res$prevalence.approx[res$lieu == "dom"],  prevalenceApprox(prev_t,     cuts_t, 60, 99, w_t))
  expect_equal(res$prevalence.approx[res$lieu == "etab"], prevalenceApprox(prev_t / 2, cuts_t, 60, 99, w_t))
})

test_that("l'ordre des lignes de 'tab' n'a pas d'importance", {
  res <- approx_tab()
  shuffled <- tab_t[c(8:1, 16:9), ]                                  # ordre inversé dans chaque catégorie
  res2 <- approx_tab(tab = shuffled)
  key <- function(d) d[order(d$lieu, d$age), c("lieu", "age", "prevalence.approx")]
  expect_equal(key(res2), key(res), ignore_attr = TRUE)
})

test_that("l'ordre des lignes de 'weights.tab' n'a pas d'importance", {
  res <- approx_tab()
  res2 <- approx_tab(wtab = wtab_t[rev(seq_len(nrow(wtab_t))), ])
  key <- function(d) d[order(d$lieu, d$age), c("lieu", "age", "prevalence.approx")]
  expect_equal(key(res2), key(res), ignore_attr = TRUE)
})

test_that("'weights.tab' peut contenir des âges en dehors de [agemin, agemax]", {
  extra <- rbind(wtab_t,
                 data.frame(lieu = rep(c("dom", "etab"), each = 20), age = rep(c(0:19), 2), popx = 5),
                 data.frame(lieu = rep(c("dom", "etab"), each = 10), age = rep(100:109, 2), popx = 1))
  expect_equal(approx_tab(wtab = extra)$prevalence.approx, approx_tab()$prevalence.approx)
})

test_that("les tranches d'âge peuvent être données sous forme d'étiquettes", {
  lab <- tab_t
  lab$age <- rep(c("[60,65)", "[65,70)", "[70,75)", "[75,80)", "[80,85)", "[85,90)", "[90,95)", "[95,Inf]"), 2)
  expect_equal(approx_tab(tab = lab)$prevalence.approx, approx_tab()$prevalence.approx)
})

test_that("une tranche d'âge manquante ou en double est signalée avec le nom de la catégorie", {
  missing <- tab_t[-3, ]                                              # tranche 70-74 absente pour 'dom'
  expect_error(approx_tab(tab = missing), "dom")
  dup <- rbind(tab_t, tab_t[1, ])
  expect_error(approx_tab(tab = dup), "do not match 'agecuts'")
})

test_that("des poids manquants ou incomplets sont signalés", {
  expect_error(approx_tab(wtab = wtab_t[wtab_t$lieu == "dom", ]), "No weights found.*etab")
  expect_error(approx_tab(wtab = wtab_t[!(wtab_t$lieu == "etab" & wtab_t$age == 75), ]), "etab.*exactly once")
})

test_that("des prévalences manquantes sont signalées", {
  na_tab <- tab_t; na_tab$prevalence[2] <- NA
  expect_error(approx_tab(tab = na_tab), "Missing values.*dom")
})

test_that("les arguments incohérents sont signalés", {
  expect_error(suppressMessages(ApproxPrevalenceTable(tab_t, rev(cuts_t), 60, 99, wtab_t, "lieu")), "increasing")
  expect_error(suppressMessages(ApproxPrevalenceTable(tab_t, cuts_t, 55, 99, wtab_t, "lieu")), "agemin")
})

test_that("une variable de 'categories' absente de 'tab' est signalée par un avertissement", {
  expect_warning(suppressMessages(ApproxPrevalenceTable(tab_t, cuts_t, 60, 99, wtab_t, categories = c("lieu", "gir"))),
                 "gir")
})
