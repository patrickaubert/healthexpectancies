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
