# ---------------------------------------------------------------------------
# 1. Calculs : comparaison avec l'exemple du manuel Sullivan
# ---------------------------------------------------------------------------

test_that("les résultats retrouvent ceux de l'exemple du manuel Sullivan", {
  ref <- as.data.frame(healthexpectancies::sullivan)
  res <- CompleteDFLEtable(sullivan_input())
  # les colonnes du jeu 'sullivan' contiennent les résultats publiés (arrondis) : on les compare à ceux recalculés
  expect_equal(res$lx,       ref$lx,       tolerance = 1e-3)
  expect_equal(res$Lx,       ref$Lx,       tolerance = 1e-3)
  expect_equal(res$ex,       ref$ex,       tolerance = 1e-3)
  expect_equal(res$DFLEx,    ref$DFLEx,    tolerance = 1e-3)
  expect_equal(res$pctDFLEx, ref$pctDFLEx, tolerance = 1e-3)
})

test_that("les identités de base sont respectées", {
  res <- CompleteDFLEtable(mortality_table(sexes = "female", years = 2020))
  expect_equal(res$lx[1], 1e5)
  expect_equal(res$ex, res$Tx / res$lx)
  expect_equal(res$DFLEx + res$DLEx, res$ex)
  expect_equal(res$Lx, res$DFLx + res$DLx)
})

# ---------------------------------------------------------------------------
# 2. Coefficient des années vécues : 0,2 à l'âge 0 seulement
# ---------------------------------------------------------------------------

test_that("Lx utilise 0,2 à l'âge 0 et 0,5 aux autres âges de départ", {
  # table commençant à l'âge 0 (exemple Sullivan)
  res0 <- CompleteDFLEtable(sullivan_input())
  expect_equal(res0$Lx[1], 0.2 * res0$lx[1] + 0.8 * res0$lx[2])
  expect_equal(res0$Lx[2], 0.5 * res0$lx[2] + 0.5 * res0$lx[3])
  # table commençant à 60 ans
  res60 <- CompleteDFLEtable(mortality_table(sexes = "female", years = 2020))
  expect_equal(res60$Lx[1], 0.5 * res60$lx[1] + 0.5 * res60$lx[2])
})

# ---------------------------------------------------------------------------
# 3. Valeurs manquantes
# ---------------------------------------------------------------------------

test_that("une colonne entièrement NA est traitée comme une variable absente", {
  tab <- mortality_table(sexes = "female")
  tab_na <- tab
  tab_na$mx <- NA_real_
  expect_no_warning(res <- CompleteDFLEtable(tab_na))
  expect_equal(res$ex, CompleteDFLEtable(tab)$ex)
  expect_equal(res$mx, tab$qx / (1 - 0.5 * tab$qx)[order(tab$year, tab$age)], ignore_attr = TRUE)
})

test_that("un NA partiel sur qx est signalé une fois et n'affecte que le groupe concerné", {
  tab <- mortality_table(sexes = "female", years = c(2020, 2030))
  tab_na <- tab
  tab_na$qx[tab_na$year == 2030 & tab_na$age == 75] <- NA

  expect_warning(res <- CompleteDFLEtable(tab_na), regexp = "year=2030")
  w <- NULL
  withCallingHandlers(CompleteDFLEtable(tab_na),
                      warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  expect_length(w, 1)

  expect_true(all(is.na(res$ex[res$year == 2030])))
  expect_false(any(is.na(res$ex[res$year == 2020])))
  expect_equal(res$ex[res$year == 2020], CompleteDFLEtable(tab[tab$year == 2020, ])$ex)
})

test_that("un NA partiel sur pix ne touche que les EVSI, pas l'espérance de vie", {
  tab <- mortality_table(sexes = "female", years = 2030)
  tab$pix[tab$age == 75] <- NA
  res <- quiet(CompleteDFLEtable(tab))
  expect_true("DFLEx" %in% names(res))   # la colonne n'est pas supprimée
  expect_false(any(is.na(res$ex)))
  expect_true(all(is.na(res$DFLEx[res$age <= 75])))
  expect_false(any(is.na(res$DFLEx[res$age > 75])))
})

test_that("les NA dans age, year ou une variable de 'categories' provoquent une erreur", {
  tab <- mortality_table(sexes = "female")
  t1 <- tab; t1$age[3] <- NA
  expect_error(CompleteDFLEtable(t1), "'age'")
  t2 <- tab; t2$year[3] <- NA
  expect_error(CompleteDFLEtable(t2), "'year'")
  t3 <- tab; t3$lieu <- "dom"; t3$lieu[2] <- NA
  expect_error(CompleteDFLEtable(t3, categories = "lieu"), "'lieu'")
})

# ---------------------------------------------------------------------------
# 4. Catégories
# ---------------------------------------------------------------------------

test_that("les variables de 'categories' sont restituées dans tous les cas", {
  base <- mortality_table(sexes = "male", years = c(2020, 2030))

  # 1 seule catégorie, plusieurs années, 1 seul sexe
  t1 <- base; t1$lieu <- "dom"
  r1 <- CompleteDFLEtable(t1, categories = "lieu")
  expect_false("categ" %in% names(r1)); expect_true("lieu" %in% names(r1))
  expect_true(all(r1$lieu == "dom"))

  # 1 seule catégorie, 1 seule année, 1 seul sexe
  t2 <- t1[t1$year == 2020, ]
  r2 <- CompleteDFLEtable(t2, categories = "lieu")
  expect_false("categ" %in% names(r2)); expect_true("lieu" %in% names(r2))

  # sans sexe, avec deux variables de catégories
  t3 <- base; t3$sex <- NULL; t3$lieu <- "dom"; t3$gir <- "G1"
  r3 <- CompleteDFLEtable(t3, categories = c("lieu","gir"))
  expect_false("categ" %in% names(r3)); expect_true(all(c("lieu","gir") %in% names(r3)))
})

test_that("calculer par catégories donne le même résultat que calculer chaque catégorie séparément", {
  base <- mortality_table(sexes = "male", years = c(2020, 2030))
  t_dom  <- base; t_dom$lieu  <- "dom"
  t_etab <- base; t_etab$lieu <- "etab"; t_etab$pix <- pmin(0.9, t_etab$pix * 3)
  both <- rbind(t_dom, t_etab)

  res  <- CompleteDFLEtable(both, categories = "lieu")
  sep  <- rbind(transform(CompleteDFLEtable(t_dom[ , names(base)]),  lieu = "dom"),
                transform(CompleteDFLEtable(t_etab[ , names(base)]), lieu = "etab"))
  key <- function(d) d[order(d$lieu, d$year, d$age), c("lieu","year","age","ex","DFLEx")]
  expect_equal(key(res), key(sep), ignore_attr = TRUE)
})

# ---------------------------------------------------------------------------
# 5. Noms français des variables
# ---------------------------------------------------------------------------

test_that("les noms 'sexe' et 'annee' sont acceptés et conservés dans le résultat", {
  tab <- mortality_table(sexes = c("female","male"), years = c(2020, 2030))
  fr <- tab; names(fr)[names(fr) == "sex"] <- "sexe"; names(fr)[names(fr) == "year"] <- "annee"
  res <- CompleteDFLEtable(fr)
  expect_true(all(c("sexe","annee") %in% names(res)))
  expect_false(any(c("sex","year") %in% names(res)))
  expect_equal(sort(res$ex), sort(CompleteDFLEtable(tab)$ex))
})
