test_that("le jeu de données FRAPAprevalence est cohérent", {
  d <- healthexpectancies::FRAPAprevalence
  expect_equal(names(d), c("year", "decomp", "agebracket", "age", "nb", "pop", "prevalence"))
  expect_true(all(d$prevalence > 0 & d$prevalence < 1))
  expect_equal(d$prevalence, d$nb / d$pop, tolerance = 1e-6)
  d6 <- d[d$decomp == 6, ]; d8 <- d[d$decomp == 8, ]
  expect_equal(sort(unique(d6$year)), 2010:2024); expect_equal(sort(unique(d8$year)), 2016:2024)
  expect_true(all(table(d6$year) == 6)); expect_true(all(table(d8$year) == 8))
  expect_equal(sort(unique(d6$age)), seq(60, 85, 5)); expect_equal(sort(unique(d8$age)), seq(60, 95, 5))
})

test_that("FRAPAprevalence peut être lissée par âge avec ApproxPrevalenceTable", {
  apa <- healthexpectancies::FRAPAprevalence
  apa <- apa[apa$decomp == 6 & apa$year %in% 2018:2019, c("year", "age", "prevalence")]
  w <- data.frame(year = rep(2018:2019, each = 40), age = rep(60:99, 2), popx = rep(exp(-0.08 * (0:39)) * 1e5, 2))
  res <- suppressMessages(ApproxPrevalenceTable(apa, agecuts = seq(60, 85, 5), agemin = 60, agemax = 99, weights.tab = w, categories = "year"))
  expect_equal(nrow(res), 80)
  expect_true(all(res$prevalence.approx > 0 & res$prevalence.approx < 1))
})
