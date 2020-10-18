#' An example of calculation using Sullivan's method.
#'
#' A dataset taken from the 'Ex 1' example in the Sullivan manual (june 2007 version)
#' available at https://reves.site.ined.fr/en/resources/computation_online/sullivan/
#'
#' @format A data frame with  rows and  variables:
#' \describe{
#'   \item{year}{year}
#' }
#' @source \url{https://reves.site.ined.fr/fichier/s_rubrique/20182/sullivan_manual_jun2007.en.xls}
"sullivan"

#' A description of the variables in the dataset from Sullivan manual's calculation example.
#'
#' @format A data frame with 2 variables:
#' \describe{
#'   \item{heading}{name of the variable in the sullivan dataset}
#'   \item{description}{description of the variable from the sullivan dataset}
#' }
#' @source \url{https://reves.site.ined.fr/fichier/s_rubrique/20182/sullivan_manual_jun2007.en.xls}
"description_sullivan"

#' A dataset with mortality rates from Insee's 2016 population forecast (central scenario)
#'
#' Note: Insee's file provides number of death by age at the end of the year, not age at last birthday.
#' We assume deaths are evenly distributed within the year, so we use simple average to convert from
#' definition of age to the other.
#'
#' @format A data frame with 14036 observations and 4 variables:
#' \describe{
#'   \item{age}{age}
#'   \item{year}{year}
#'   \item{sex}{sex ('male' or 'female')}
#'   \item{qx}{mortality rates at each year, age, sex}
#' }
#' @source \url{https://www.insee.fr/fr/statistiques/2496716?sommaire=2496793}
"FRmortalityForecast2016"
