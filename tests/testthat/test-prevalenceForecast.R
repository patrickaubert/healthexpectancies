# Les hypothèses 'cst*' conservent, entre l'année de référence et l'année projetée,
# la quantité visée : on le vérifie âge par âge.

forecast_case <- function(hypo) {
  tabref  <- mortality_table(sexes = "female", years = 2020)
  tabproj <- mortality_table(sexes = "female", years = 2030)
  tabproj$pix <- NULL
  quiet(prevalenceForecast(tabref, tabproj, hypo))
}

compare_years <- function(res, var) {
  ref  <- res[res$year == 2020, ]; proj <- res[res$year == 2030, ]
  expect_equal(proj$age, ref$age)
  list(ref = ref[[var]], proj = proj[[var]])
}

test_that("cstPreval : les prévalences par âge sont inchangées", {
  x <- compare_years(forecast_case("cstPreval"), "pix")
  expect_equal(x$proj, x$ref)
})

test_that("cstDFLE : l'espérance de vie sans incapacité à chaque âge est inchangée", {
  x <- compare_years(forecast_case("cstDFLE"), "DFLEx")
  expect_equal(x$proj, x$ref, tolerance = 1e-6)
})

test_that("cstDLE : l'espérance de vie avec incapacité à chaque âge est inchangée", {
  x <- compare_years(forecast_case("cstDLE"), "DLEx")
  expect_equal(x$proj, x$ref, tolerance = 1e-6)
})

test_that("cstPctDFLE : la part de l'EVSI dans l'espérance de vie est inchangée", {
  x <- compare_years(forecast_case("cstPctDFLE"), "pctDFLEx")
  expect_equal(x$proj, x$ref, tolerance = 1e-6)
})

test_that("la table renvoyée contient l'année de référence puis l'année projetée", {
  res <- forecast_case("cstDFLE")
  expect_equal(sort(unique(res$year)), c(2020, 2030))
})

test_that("cstPctDLE et cstPctDFLE sont équivalentes (part de l'EVI = 1 - part de l'EVSI)", {
  a <- forecast_case("cstPctDLE"); b <- forecast_case("cstPctDFLE")
  expect_equal(a$pix, b$pix, tolerance = 1e-9)
  x <- compare_years(forecast_case("cstPctDLE"), "pctDLEx")
  expect_equal(x$proj, x$ref, tolerance = 1e-6)
})

test_that("une hypothèse inconnue, des variables manquantes ou plusieurs années de référence provoquent une erreur", {
  tabref  <- mortality_table(sexes = "female", years = 2020)
  tabproj <- mortality_table(sexes = "female", years = 2030)
  expect_error(prevalenceForecast(tabref, tabproj, "cstXYZ"), "must be one of")
  expect_error(prevalenceForecast(tabref[ , names(tabref) != "pix"], tabproj, "cstPreval"), "'pix'")
  expect_error(prevalenceForecast(tabref, tabproj[ , c("year", "age")], "cstPreval"), "'qx'")
  expect_error(prevalenceForecast(mortality_table(sexes = "female", years = c(2020, 2030)), tabproj, "cstPreval"), "single reference year")
})

test_that("une colonne 'pix' présente dans tabproj est ignorée", {
  tabref  <- mortality_table(sexes = "female", years = 2020)
  tabproj <- mortality_table(sexes = "female", years = 2030)
  expect_equal(quiet(prevalenceForecast(tabref, tabproj, "cstDFLE")),
               quiet(prevalenceForecast(tabref, tabproj[ , names(tabproj) != "pix"], "cstDFLE")))
})

# --- cas où l'hypothèse ne peut pas être satisfaite (prévalences négatives aux grands âges) ----------------------

infeasible_case <- function() {
  f <- as.data.frame(healthexpectancies::FRInseeMortalityForecast2016)
  f <- f[f$sex == "female" & f$age >= 60 & f$year %in% c(2013, 2020), ]
  ref <- f[f$year == 2013, ]; ref$pix <- 0.1
  list(ref = ref, proj = f[f$year == 2020, c("year", "age", "qx")])
}

test_that("une cible infaisable est signalée par défaut, sans changer les résultats", {
  x <- infeasible_case()
  expect_warning(res <- prevalenceForecast(x$ref, x$proj, "cstDFLE"), "outside the interval")
  expect_true(any(res$pix < 0))
})

test_that("on_unfeasible = 'clip' borne les prévalences et garde un tableau cohérent", {
  x <- infeasible_case()
  expect_warning(res <- prevalenceForecast(x$ref, x$proj, "cstDFLE", on_unfeasible = "clip"), "bounded")
  expect_true(all(res$pix >= 0 & res$pix <= 1))
  p <- res[res$year == 2020, ]
  recalc <- CompleteDFLEtable(data.frame(year = 2020, age = p$age, qx = x$proj$qx[match(p$age, x$proj$age)], pix = p$pix))
  expect_equal(p$DLEx, recalc$DLEx)
})

test_that("on_unfeasible = 'error' s'arrête", {
  x <- infeasible_case()
  expect_error(prevalenceForecast(x$ref, x$proj, "cstDFLE", on_unfeasible = "error"), "outside the interval")
})

test_that("un cas faisable ne produit aucun avertissement", {
  tabref  <- mortality_table(sexes = "female", years = 2020)
  tabproj <- mortality_table(sexes = "female", years = 2030)
  expect_no_warning(prevalenceForecast(tabref, tabproj, "cstDFLE"))
})
