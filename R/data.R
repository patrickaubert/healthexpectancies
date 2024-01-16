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

#' A dataset with mortality rates from Insee's 2021 population forecast (central scenario)
#'
#' The dataset includes observed values from 1962 to 2020, then forecasted values from 2021 to 2070.
#'
#' Note: Insee's file provides number of death by age at the end of the year, not age at last birthday.
#' We assume deaths are evenly distributed within the year, so we use simple average to convert from
#' definition of age to the other.
#'
#' @format A data frame with 79134 observations and 6 variables:
#' \describe{
#'   \item{age}{age}
#'   \item{year}{year}
#'   \item{sex}{sex ('male' or 'female' or 'all')}
#'   \item{type.obs}{type of observation ('observed' or 'observed (prov.)' or 'forecasted')}
#'   \item{def.age}{definition of age ('age at end of year' or 'current age (approx)')}
#'   \item{qx}{mortality rates at each year, age, sex}
#' }
#' @source \url{https://www.insee.fr/fr/statistiques/5894083?sommaire=5760764}
"FRInseeMortalityForecast2021"

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

#' A dataset with population by sex and age, from Insee's 2021 population forecast (central scenario)
#'
#' The dataset also includes observed data from 1990 to 2022. Population is measured on the 1st of January of each year.
#'
#' @format A data frame with 25334 observations and 6 variables:
#' \describe{
#'   \item{age0101}{age at January, 1st of the year}
#'   \item{year}{year}
#'   \item{sex}{sex ('male' or 'female' or 'all')}
#'   \item{geo}{geographical field ('france' only)}
#'   \item{type.obs}{'observed' or 'forecasted'}
#'   \item{popx0101}{population on the 1st of January at each year, age, sex}
#' }
#' @source \url{https://www.insee.fr/fr/outil-interactif/5014911/pyramide.htm#!l=en}
"FRInseePopulationForecast2021"

#' A dataset with population by sex and age, from Insee's 'bilan démographique' (annual population estimates)
#'
#' The current version of the data table was released in 2023 and contains observations
#' from 1991 (France) or 1901 (metropolitan France) to 2023.
#' Observations for the last three years (2021 and more) are provisional.
#'
#' @format A data frame with 31 200 observations and 5 variables:
#' \describe{
#'   \item{age0101}{age at January, 1st of the year}
#'   \item{year}{year}
#'   \item{sex}{sex ('male' or 'female')}
#'   \item{geo}{geographical field ('france' or 'metropolitan france')}
#'   \item{popx}{population at each year, age, sex}
#' }
#' @source \url{https://www.insee.fr/fr/outil-interactif/5014911/pyramide.htm}
"FRInseePopulation"

#' A dataset with observed mortality rates by sex and age, from Insee
#'
#' The current version of the data table was released in June, 2021 and contains observations
#' from 1976 (Metropolitan France) or 1998 (France including oversea territories) to 2018
#' Observations correspond to an average over 3 years: for instance, values for 2017
#' are actually an average between 2016 and 2018.
#' Data are from 'tableau 68' (TM68) released by Insee.
#'
#' @format A data frame with 18170 observations and 5 variables:
#' \describe{
#'   \item{age}{age}
#'   \item{year}{year}
#'   \item{geo}{field ('france' or 'metropolitan france')}
#'   \item{sex}{sex ('male' or 'female' or 'all)}
#'   \item{qx}{mortality rate at each year, age, sex}
#' }
#' @source \url{https://www.insee.fr/fr/statistiques/5390366?sommaire=5390468}
"FRInseeMortalityrates"

#' An (alternative) dataset with observed mortality rates by sex and age, from Insee
#'
#' This dataset corresponds to data released by Insee as T69, as comparison to the FRInseeMortalityrates, which
#' corresponds to T68 (see Insee's website for a presentation of the differences).
#'
#' The current version of the data table was released in January, 2022 and contains observations
#' from 1994 to 2022. The geographical field is France including oversea territories.
#'
#' @format A data frame with 17574 observations and 5 variables:
#' \describe{
#'   \item{age}{age}
#'   \item{def.age}{definition of age: at the end of the year vs. current age}
#'   \item{year}{year}
#'   \item{sex}{sex ('male' or 'female' or 'all)}
#'   \item{qx}{mortality rate at each year, age, sex}
#' }
#' @source \url{https://www.insee.fr/fr/statistiques/6686511?sommaire=6686521}
"FRInseeMortalityrates_t69"


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

#' A dataset with disability prevalences in France (2021)
#'
#' Prevalences are estimated through the 2021 'Vie quotidienne et santé' (VQS) survey.
#' They are estimated by age brackets (eg. '[60;65)' stands for ages 60 to 64)
#'
#' @format A data frame with 4626 observations and 6 variables:
#' \describe{
#'   \item{limitationtype}{type of limitation [in French] (eg. 'Difficultés pour marcher', etc.)}
#'   \item{limitationintensity}{strength of limitation [in French] (eg. 'Ne peut pas du tout', etc.)}
#'   \item{sex}{sex ('female' or 'male' or 'all')}
#'   \item{agebracket}{age bracket (eg '[60;65)' = between 60 and 64 years old)}
#'   \item{prevalence}{prevalence of limitation in the given age bracket (number between 0 and 1)}
#'   \item{nb}{number of persons with limitation in the given age bracket (number between 0 and 1)}
#' }
#' @source \url{https://data.drees.solidarites-sante.gouv.fr/explore/dataset/enquete-vie-quotidienne-et-sante-2021-donnees-detaillees/information/}
"FRDreesVQSsurvey2021"

#' A dataset with share of APA beneficiaries in France (December 2017)
#'
#' APA is a benefit covering long term care expenditures both at home ('à domicile') or in institutions ('en établissement').
#' Shares of beneficiaries are estimated through the 2017 'Aide sociale' survey.
#' They are estimated by age brackets (eg. '[60;65)' stands for ages 60 to 64)
#' A more complete dataset of APA prevalences, as well as prevalences for other social allowances in France, can be found in the 'asdep' package.
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
#' A more complete dataset of APA prevalences, as well as prevalences for other social allowances in France, can be found in the 'asdep' package.
#'
#' @format A data frame with 138 observations and 6 variables:
#' \describe{
#'   \item{typepresta}{type of benefits (eg. 'à domicile' or 'en établissement')}
#'   \item{prevalence}{share of beneficiaires within the total population in the given age bracket (number between 0 and 1)}
#'   \item{sex}{sex ('female' or 'male')}
#'   \item{agebracket}{age bracket (eg '[60;65)' = between 60 and 64 years old)}
#'   \item{age}{minimal age within the age bracket}
#'   \item{year}{year of observation (value for the monthe of December)}
#' }
#' @source \url{https://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/publications/les-dossiers-de-la-drees/article/incapacites-et-perte-d-autonomie-des-personnes-agees-en-france-une-evolution}
"FRDreesAPA"


#' A dataset with prevalence of general activity limitations (GALI) in France
#'
#' Prevalences are measured in the SRCV survey ie. the French version of EU-SILC.
#' They are measured by sex and age bracket, for severe general activity limitations and for limitations including moderate ones.
#'
#' @format A data frame with 2 448 observations and 5 variables:
#' \describe{
#'   \item{incap}{type of general activity limitations ('gali_incl_moderate' or 'gali_severe')}
#'   \item{prevalence}{share of people with general activity limitations in the given age bracket (number between 0 and 1)}
#'   \item{sex}{sex ('female' or 'male' or 'all)}
#'   \item{agebracket}{age bracket (eg '[60;65)' = between 60 and 64 years old)}
#'   \item{year}{year of observation (value for the month of December)}
#' }
#' @source \url{https://drees.solidarites-sante.gouv.fr/publications-communique-de-presse/etudes-et-resultats/lesperance-de-vie-sans-incapacite-65-ans-1}
"FRGaliEUSilc"

#' A dataset with proportions on people living in institutions in France according to Drees's EHPA survey
#'
#' The EHPA survey is collected every four years. Data are available for 2007, 2011, 2015 and 2019. Numbers of people are measured on the Decembre 31st of each year.
#'
#' @format A data frame with 756 observations and 6 variables:
#' \describe{
#'   \item{year}{year of observation}
#'   \item{sex}{sex ('female' or 'male' or 'all)}
#'   \item{age}{age}
#'   \item{nb}{number of elder people living in institutions}
#'   \item{poptot}{total population by age and sexe}
#'   \item{prevalence}{share of elder people living in institutions}
#' }
#' @source \url{https://drees.solidarites-sante.gouv.fr/sources-outils-et-enquetes/07-lenquete-aupres-des-etablissements-dhebergement-pour-personnes-agees}
"FRDreesEHPA"
