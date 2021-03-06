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
"FRInseeMortalityForecast2016"

#' A dataset with population by sex and age, from Insee's 2016 population forecast (central scenario)
#'
#'
#' @format A data frame with 14036 observations and 4 variables:
#' \describe{
#'   \item{age0101}{age at January, 1st of the year}
#'   \item{year}{year}
#'   \item{sex}{sex ('male' or 'female')}
#'   \item{popx}{population at each year, age, sex}
#' }
#' @source \url{https://www.insee.fr/fr/statistiques/2496716?sommaire=2496793}
"FRInseePopulationForecast2016"

#' A dataset with population by sex and age, from Insee's 'bilan démographique' (annual population estimates)
#'
#' The current version of the data table was released in January, 2020 and contains observations
#' from 1991 to 2020. Observations for the last three years (2018 and more) are provisional.
#'
#' @format A data frame with 6060 observations and 4 variables:
#' \describe{
#'   \item{age0101}{age at January, 1st of the year}
#'   \item{year}{year}
#'   \item{sex}{sex ('male' or 'female')}
#'   \item{popx}{population at each year, age, sex}
#' }
#' @source \url{https://www.insee.fr/fr/statistiques/3312958}
"FRInseePopulation"

#' A dataset with observed mortality rates by sex and age, from Insee
#'
#' The current version of the data table was released in June, 2020 and contains observations
#' from 1976 (Metropolitan France) or 1998 (France including oversea territories) to 2017.
#' Observations correspond to an average over 3 years: for instance, values for 2017
#' are actually an average between 2016 and 2018.
#' Data are from 'tableau 68' (TM68) released by Insee.
#'
#' @format A data frame with 17552 observations and 5 variables:
#' \describe{
#'   \item{age}{age}
#'   \item{year}{year}
#'   \item{geo}{field ('france' or 'metropolitan france')}
#'   \item{sex}{sex ('male' or 'female' or 'all)}
#'   \item{qx}{mortality rate at each year, age, sex}
#' }
#' @source \url{https://www.insee.fr/fr/statistiques/4503155?sommaire=4503178}
"FRInseeMortalityrates"

#' A dataset with disability prevalences in France (2014)
#'
#' Prevalences are estimated through the 2014 'Vie quotidienne et santé' (VQS) survey.
#' They are estimated by age brackets (eg. '[60;65)' stands for ages 60 to 64)
#'
#' @format A data frame with 176 observations and 5 variables:
#' \describe{
#'   \item{limitationtype}{type of limitation (eg. GALI, physical limitation, etc.)}
#'   \item{prevalence}{prevalence of limitation in the given age bracket (number between 0 and 1)}
#'   \item{sex}{sex ('female' or 'male')}
#'   \item{age}{minimal age within the age bracket}
#'   \item{agebracket}{age bracket (eg '[60;65)' = between 60 and 64 years old)}
#' }
#' @source \url{https://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/publications/les-dossiers-de-la-drees/article/incapacites-et-perte-d-autonomie-des-personnes-agees-en-france-une-evolution}
"FRDreesVQSsurvey2014"

#' A dataset with share of APA beneficiaries in France (December 2017)
#'
#' APA is a benefit covering long term care expenditures both at home ('à domicile') or in institutions ('en établissement').
#' Shares of beneficiaries are estimated through the 2017 'Aide sociale' survey.
#' They are estimated by age brackets (eg. '[60;65)' stands for ages 60 to 64)
#'
#' @format A data frame with 48 observations and 5 variables:
#' \describe{
#'   \item{typepresta}{type of benefits (eg. 'à domicile' or 'en établissement')}
#'   \item{prevalence}{share of beneficiaires within the total population in the given age bracket (number between 0 and 1)}
#'   \item{sex}{sex ('female' or 'male')}
#'   \item{age}{minimal age within the age bracket}
#'   \item{agebracket}{age bracket (eg '[60;65)' = between 60 and 64 years old)}
#' }
#' @source \url{https://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/publications/les-dossiers-de-la-drees/article/incapacites-et-perte-d-autonomie-des-personnes-agees-en-france-une-evolution}
"FRDreesAPA2017"

#' A dataset with share of APA beneficiaries in France
#'
#' APA is a benefit covering long term care expenditures both at home ('à domicile') or in institutions ('en établissement').
#' Shares of beneficiaries are estimated through the 2016, 2017 and 2018 'Aide sociale' survey.
#' They are estimated by age brackets (eg. '[60;65)' stands for ages 60 to 64)
#'
#' @format A data frame with 138 observations and 6 variables:
#' \describe{
#'   \item{typepresta}{type of benefits (eg. 'à domicile' or 'en établissement')}
#'   \item{prevalence}{share of beneficiaires within the total population in the given age bracket (number between 0 and 1)}
#'   \item{sex}{sex ('female' or 'male')}
#'   \item{age}{minimal age within the age bracket}
#'   \item{agebracket}{age bracket (eg '[60;65)' = between 60 and 64 years old)}
#'   \item{annee}{year of observation (value for the monthe of December)}
#' }
#' @source \url{https://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/publications/les-dossiers-de-la-drees/article/incapacites-et-perte-d-autonomie-des-personnes-agees-en-france-une-evolution}
"FRDreesAPA"
