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
