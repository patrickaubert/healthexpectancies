# Jeux de données utilisés dans les tests -------------------------------------

# table de mortalité (Insee, projections 2016) à partir de 60 ans, avec une prévalence d'incapacité fictive croissante avec l'âge
mortality_table <- function(sexes = c("female","male"), years = c(2020,2030)) {
  tab <- healthexpectancies::FRInseeMortalityForecast2016
  tab <- tab[tab$sex %in% sexes & tab$year %in% years & tab$age >= 60, ]
  tab$pix <- pmin(0.9, 0.02 * exp(0.08 * (tab$age - 60)))
  as.data.frame(tab)
}

# exemple du manuel Sullivan, réduit aux variables d'entrée (le reste est recalculé)
sullivan_input <- function() {
  as.data.frame(healthexpectancies::sullivan[ , c("year","age","mx","qx","pix")])
}

# récupère les résultats d'un calcul en ignorant les avertissements
quiet <- function(expr) suppressWarnings(expr)

# ---- données simulées pour fitPrevalenceModel() -------------------------------------------------------------------

# tendance : logit(p) = alpha_a + beta_a * (t - 2019), sans bruit
simulate_trend <- function(ages = 60:100, years = 2010:2019) {
  alpha <- -6 + 0.12 * (ages - 60)
  beta  <- -(0.01 + 0.0005 * (ages - 60))
  d <- expand.grid(age = ages, year = years)
  d$pix <- plogis(alpha[match(d$age, ages)] + beta[match(d$age, ages)] * (d$year - 2019))
  list(data = d, alpha = alpha, beta = beta, ages = ages)
}

# table de mortalité simulée : qx baisse plus vite aux âges élevés ; qx = 1 au dernier âge
simulate_qx <- function(ages = 60:100, years = 2010:2040) {
  d <- expand.grid(age = ages, year = years)
  d$qx <- exp(-5.5 + 0.08 * (d$age - 60) - (0.01 + 0.0003 * (d$age - 60)) * (d$year - 2019))
  d$qx[d$age == max(ages)] <- 1
  d
}

# lien avec la mortalité : logit(p) = alpha_a + beta * ln q(a + k, t), k entier ou demi-entier, sans bruit
simulate_mortality <- function(k = 3, beta = 1.5, ages = 60:100, years = 2010:2019) {
  q <- simulate_qx(ages, sort(unique(c(2010:2040, years))))              # mortalité simulée de 2010 à 2040 (au moins)
  lnq <- function(a, y) log(q$qx[q$age == min(a, max(ages)) & q$year == y])        # âge plafonné au dernier âge
  x <- function(a, y) { f <- floor(a + k); w <- a + k - f; (1 - w) * lnq(f, y) + ifelse(w > 0, w * lnq(f + 1, y), 0) }
  alpha <- 3 + 0.01 * (ages - 60)
  d <- q[q$year %in% years, ]
  d$pix <- plogis(alpha[match(d$age, ages)] + beta * mapply(x, d$age, d$year))
  list(data = d, alpha = alpha, beta = beta, k = k, qx_all = q)
}

# ---- données simulées pour method = "target" -----------------------------------------------------------------------

# prévalences telles que l'espérance de vie avec incapacité (EVI) à chaque âge évolue linéairement dans le temps
# (obtenues par inversion de la méthode de Sullivan, avec la mortalité simulée de 2010 à 2040)
simulate_target <- function(ages = 60:100, slope0 = -0.004) {
  q <- simulate_qx(ages, 2010:2040)
  base <- CompleteDFLEtable(data.frame(q[q$year == 2019, c("year", "age", "qx")], pix = plogis(-5 + 0.12 * (ages - 60))))
  base <- base[order(base$age), ]
  d <- q
  d$DLEx <- base$DLEx[match(d$age, base$age)] + (slope0 * (1 + 0.02 * (d$age - 60))) * (d$year - 2019)
  r <- CompleteDFLEtable(d[ , c("year", "age", "qx", "DLEx")])
  r <- r[order(r$year, r$age), c("year", "age", "qx", "pix", "DLEx", "ex")]
  rownames(r) <- NULL
  r
}
