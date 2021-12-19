# data-raw/healthexpectancies_dataset.R
# Data import and processing pipeline

library(devtools)
library(readxl)
library(tidyverse)

# ===================================================================================
# Examples from the sullivan manual (2007 version)
# ===================================================================================

# raw data are downloaded from: https://reves.site.ined.fr/en/resources/computation_online/sullivan/

sullivan <- read_excel("data-raw/sullivan_manual_jun2007.en.xls",
                       sheet = "Ex 1",
                       range = "A5:O91")
names(sullivan)[1] <- "year"
names(sullivan)[2] <- "age"
names(sullivan)[12] <- "DFLx"
names(sullivan)[13] <- "DFTx"
names(sullivan)[15] <- "pctDFLEx"
# names(sullivan)
sullivan$age[nrow(sullivan)] <- 85

sullivan <- sullivan %>%
  mutate(year = as.numeric(year),
         #sex = as.factor(sex),
         age = as.numeric(age))

names_sullivan <- read_excel("data-raw/sullivan_manual_jun2007.en.xls",
                             sheet = "Ex 1",
                             range = "A4:O4")
description_sullivan <- data.frame(
  heading = c(
    names(sullivan),
    "DLx",
    "DLEx",
    "pctDLEx",
    "MeanDAx",
    "MedianDAx",
    "ModalDAx"
    ),
  description = c(
    names(names_sullivan),
    "person years lived with disability",
    "with disability life expectancy",
    "proportion of life spent with disability",
    "average conjonctural age of person years lived with disability",
    "median conjonctural age of person years lived with disability",
    "modal conjonctural age of person years lived with disability"
  )
)

# tab <- sullivan %>% select(year,age,Px,Dx,pix)
# tab <- rbind(tab %>% mutate(sex="male"), tab %>% mutate(sex="female"))


# ===================================================================================
# Forecasted mortality rates for men and women, from Insee's 2016 population forecast
# ===================================================================================

# raw data are downloaded from: https://www.insee.fr/fr/statistiques/2496793


mortalityMale <- read_excel("data-raw/irsocprojpop1370_FECcentESPcentMIGcent.xls",
                            sheet = "hyp_mortaliteH",
                            range = "A5:BG126")
names(mortalityMale) <- c("age3112", tail(names(mortalityMale),-1) )
mortalityMale <- mortalityMale %>%
  pivot_longer(-c("age3112"), names_to = "year", values_to = "qx") %>%
  mutate(qx = qx/10000 )

mortalityFemale <- read_excel("data-raw/irsocprojpop1370_FECcentESPcentMIGcent.xls",
                            sheet = "hyp_mortaliteF",
                            range = "A5:BG126")
names(mortalityFemale) <- c("age3112", tail(names(mortalityFemale),-1) )
mortalityFemale <- mortalityFemale %>%
  pivot_longer(-c("age3112"), names_to = "year", values_to = "qx") %>%
  mutate(qx = qx/10000 )

FRmortalityForecast2016 <- rbind(
  mortalityFemale %>% mutate(sex = "female"),
  mortalityMale %>% mutate(sex = "male")
  ) %>%
  mutate(year = as.numeric(year),
         age3112 = as.numeric(age3112),
         sex = as.factor(sex))


# transform dataset from age at the end of the year to age at last birthday

FRInseeMortalityForecast2016 <- rbind(
  FRmortalityForecast2016 %>% mutate(qx = qx/2, age = age3112),
  FRmortalityForecast2016 %>% mutate(qx = qx/2, age = pmax(0,age3112-1) )
  ) %>%
  select(-age3112) %>%
  group_by(year,sex,age) %>%
  summarise_all(sum) %>%
  ungroup()

# essaiInsee <- FRInseeMortalityForecast2016 %>% mutate(pix = (age/150)^2, mx = qx) %>% filter(year %in% c(2013,2015,2020,2030))

# ===================================================================================
# Forecasted populations, from Insee's 2016 population forecast
# ===================================================================================

# raw data are downloaded from: https://www.insee.fr/fr/statistiques/2496793


popMale <- read_excel("data-raw/irsocprojpop1370_FECcentESPcentMIGcent.xls",
                            sheet = "populationH",
                            range = "A5:BG114")
names(popMale) <- c("age0101", tail(names(popMale),-1) )
popMale <- popMale %>%
  pivot_longer(-c("age0101"), names_to = "year", values_to = "popx")

popFemale <- read_excel("data-raw/irsocprojpop1370_FECcentESPcentMIGcent.xls",
                              sheet = "populationF",
                              range = "A5:BG114")
names(popFemale) <- c("age0101", tail(names(popFemale),-1) )
popFemale <- popFemale %>%
  pivot_longer(-c("age0101"), names_to = "year", values_to = "popx")

FRInseePopulationForecast2016 <- rbind(
  popFemale %>% mutate(sex = "female"),
  popMale %>% mutate(sex = "male")
  ) %>%
  mutate(age0101 = recode(age0101, "108 et +" = "108"),
         year = as.numeric(year),
         age0101 = as.numeric(age0101),
         sex = as.factor(sex))

# ===================================================================================
# Observed mortality rates in France, Insee
# ===================================================================================

# Table : Table de mortalité des années 2016 - 2019, données provisoires arrêtées à fin décembre 2019 - Séries depuis 1977
# source : https://www.insee.fr/fr/statistiques/5390366?sommaire=5390468
# released : June, 2d 2021

mr_fr <- function(path) {
  onglets <- excel_sheets(path)
  lionglets <- list()

  for (an in 1:NROW(onglets)) {
    year <- onglets[an]
    cases <- case_when(year>=2012 ~ "A4:J109",
                       year>=2011 ~ "A12:J119",
                       year>=1990 ~ "A12:J114",
                       year>=1977 ~ "A12:G114")
    colkeep <- c(1,3,6,9)
    colnames <- c("age","male","female","all")
    tab <- read_excel(
      path,
      sheet = year,
      range = cases
    )
    colkeep <- colkeep[colkeep<=ncol(tab)]
    tab <- tab[,colkeep]
    names(tab) <- colnames[1:ncol(tab)]
    tab <- tab %>%
      filter(!is.na(age)) %>%
      pivot_longer(cols=-c("age"),names_to="sex",values_to="qx") %>%
      mutate(qx = qx/100000,
             sex = as.factor(sex),
             age = as.numeric(age),
             year = as.numeric(year)-1)
    lionglets[[an]] <- tab
  }
  return( do.call("rbind", lionglets) )
}

FRInseeMortalityrates <- rbind(
  mr_fr("data-raw/fe_t68.xlsx") %>% mutate(geo = "france"),
  mr_fr("data-raw/fm_t68.xlsx") %>% mutate(geo = "metropolitan france")
)

# == correction of errors :
# 2021/07/17: add value for 2018 (2017-2019 average, published in June 2021)
# 2021/06/21: data for france were erroneously those from metropolitan france

# ===================================================================================
# Population of France, Insee
# ===================================================================================

# Table : Pyramide des âges interactive - France et France métropolitaine
# source : https://www.insee.fr/fr/outil-interactif/5014911/pyramide.htm
# released : ?

FRInseePopulation <- bind_rows(
  read_csv2("data-raw/donnees_pyramide_act.csv") %>% mutate(geo="france"),
  read_csv2("data-raw/donnees_pyramide_act_fm.csv") %>% mutate(geo="metropolitan france")  )  %>%
  rename(year = ANNEE,
         sex = SEXE,
         popx = POP,
         age0101 = AGE) %>%
  mutate(year = as.numeric(year),
         age0101 = as.numeric(age0101),
         sex = as.factor(sex),
         geo = as.factor(geo))

# == An earlier version used the another file released by Insee

#popobsMale <- read_excel("data-raw/pyramides-des-ages_bilan-demo_2019.xlsx",
#                        sheet = "France",
#                        range = "B10:AF111")
#names(popobsMale) <- c("age0101",c(1991:2020) )
#popobsMale <- popobsMale %>%
#  pivot_longer(-c("age0101"), names_to = "year", values_to = "popx")
#
#popobsFemale <- read_excel("data-raw/pyramides-des-ages_bilan-demo_2019.xlsx",
#                           sheet = "France",
#                           range = "B114:AF215")
#names(popobsFemale) <- c("age0101",c(1991:2020) )
#popobsFemale <- popobsFemale %>%
#  pivot_longer(-c("age0101"), names_to = "year", values_to = "popx")
#
#FRInseePopulation <- rbind(
#  popobsFemale %>% mutate(sex = "female"),
#  popobsMale %>% mutate(sex = "male")
#) %>%
#  mutate(age0101 = recode(age0101, "100 ou +" = "100"),
#         year = as.numeric(year),
#         age0101 = as.numeric(age0101),
#         sex = as.factor(sex))

# == revision :
# 2021/07/17: add value for 2021

# ===================================================================================
# Disability prevalences after age 60, from DREES' 2014 VQS survey
# ===================================================================================

# data from "graphique 2" can be downloaded on the DREES website: https://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/publications/les-dossiers-de-la-drees/article/incapacites-et-perte-d-autonomie-des-personnes-agees-en-france-une-evolution

prevMale <- read_excel("data-raw/dd_vqs_tableau_etude_20180228.xlsx",
                      sheet = "Graphique 2",
                      range = "A4:I15")
names(prevMale)[1] <- "limitationtype"
prevMale <- prevMale %>%
  pivot_longer(-c("limitationtype"), names_to = "tempage", values_to = "prevalence") %>%
  mutate(sex = "male")

prevFemale <- read_excel("data-raw/dd_vqs_tableau_etude_20180228.xlsx",
                       sheet = "Graphique 2",
                       range = "A18:I29")
names(prevFemale)[1] <- "limitationtype"
prevFemale <- prevFemale %>%
  pivot_longer(-c("limitationtype"), names_to = "tempage", values_to = "prevalence") %>%
  mutate(sex = "female")

FRDreesVQSsurvey2014 <- rbind( prevFemale, prevMale) %>%
  mutate(prevalence = prevalence / 100,
         sex = as.factor(sex),
         age = as.numeric( substr(tempage,0,2) ),
         agebracket = cut(age, breaks = c(seq(60,95,5),Inf), include.lowest = TRUE, right = FALSE) ) %>%
  select(-tempage)

# essaiVQS <- FRInseeMortalityForecast2016 %>% filter(year==2014, age>=60) %>% mutate(agebracket = cut(age, breaks = c(seq(60,95,5),Inf), include.lowest = TRUE, right = FALSE))
# prevVQS <- FRDreesVQSsurvey2014 %>% filter(limitationtype == "GALI") %>% mutate(pix = prevalence) %>% select(agebracket,sex,pix)
# essaiVQS <- essaiVQS %>% left_join(prevVQS, by = c("sex","agebracket"))

# ===================================================================================
# Beneficiaries of APA in December 2017 (Source: DREES, Aide sociale survey)
# ===================================================================================

# data from "Fiche 15" in "L'aide et l'action sociales en France - édition 2019" can be downloaded on the DREES website:
# https://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/publications/panoramas-de-la-drees/article/l-aide-et-l-action-sociales-en-france-perte-d-autonomie-handicap-protection-de

FRDreesAPA2017 <- read_excel(
  "data-raw/aas19_15_les_be_ne_ficiaires_et_les_de_penses_d_apa_srok.xlsx",
  sheet = "G01",
  range = "D4:G20") %>%
  select(-...1) %>%
  mutate(sex = c( rep("male",8), rep("female",8) ) ,
         age = c( seq(60,95,5), seq(60,95,5) ) ,
         agebracket = cut( age , breaks = c(seq(60,95,5),Inf), include.lowest = TRUE, right = FALSE)) %>%
  pivot_longer(cols = -c(sex,age,agebracket), names_to="typepresta",values_to="prevalence") %>%
  mutate(typepresta = recode(typepresta, "TOTAL" = "APA domicile+établissement"))

FRDreesAPA2018 <- read_excel(
  "data-raw/dataDrees_APA par sexe et age.xlsx",
  sheet = "sexeage2018",
  range = "C4:F18") %>%
  select(-...1) %>%
  mutate(sex = c( rep("male",7), rep("female",7) ) ,
         age = c( seq(60,90,5), seq(60,90,5) ) ,
         agebracket = cut( age , breaks = c(seq(60,90,5),Inf), include.lowest = TRUE, right = FALSE)) %>%
  pivot_longer(cols = -c(sex,age,agebracket), names_to="typepresta",values_to="prevalence") %>%
  mutate(typepresta = recode(typepresta, "TOTAL" = "APA domicile+établissement"))

FRDreesAPA2016 <- read_excel(
  "data-raw/dataDrees_APA par sexe et age.xlsx",
  sheet = "sexeage2016",
  range = "B3:E19") %>%
  select(-...1) %>%
  mutate(sex = c( rep("male",8), rep("female",8) ) ,
         age = c( seq(60,95,5), seq(60,95,5) ) ,
         agebracket = cut( age , breaks = c(seq(60,95,5),Inf), include.lowest = TRUE, right = FALSE)) %>%
  pivot_longer(cols = -c(sex,age,agebracket), names_to="typepresta",values_to="prevalence") %>%
  mutate(typepresta = recode(typepresta, "TOTAL" = "APA domicile+établissement"))

FRDreesAPA <- rbind(
  FRDreesAPA2016 %>% mutate(year = 2016),
  FRDreesAPA2017 %>% mutate(year = 2017),
  FRDreesAPA2018 %>% mutate(year = 2018)
)

#library(ggplot2)
#ggplot(FRDreesAPA,aes(y=prevalence,x=age,colour=as.factor(Annee))) +
#  geom_line() +
#  facet_wrap(sex ~ typepresta)
#ggplot(FRDreesAPA,aes(y=prevalence,x=Annee,colour=agebracket)) +
#  geom_line() +
#  facet_wrap(sex ~ typepresta)


# ===================================================================================
# Prevalence of GALI from Insee's SRCV survey (French version of EU-SILC)
# ===================================================================================

# Data are taken from DREES's yearly publication about Disability-free life expectancies
# Additional tables with prevalences of GALI by age brackets are disseminated with the publication


# == prévalences des incapacités (au sens du GALI)

library(httr)

httr::GET("https://drees.solidarites-sante.gouv.fr/sites/default/files/2021-10/ER1213.xls",
          write_disk(fileloc <- tempfile(fileext = ".xls")))
txincap <- bind_rows(
  read_excel(path = fileloc, sheet="DC-8", range = "B4:O20") %>%
    mutate(sex="male",incap="gali_incl_moderate"),
  read_excel(path = fileloc, sheet="DC-9", range = "B4:O20") %>%
    mutate(sex="male",incap="gali_severe"),
  read_excel(path = fileloc, sheet="DC-10", range = "B4:O20") %>%
    mutate(sex="female",incap="gali_incl_moderate"),
  read_excel(path = fileloc, sheet="DC-11", range = "B4:O20") %>%
    mutate(sex="female",incap="gali_severe")
)
unlink(fileloc)

txincap <- txincap %>%
  rename(age=Âge) %>%
  pivot_longer(cols=-c("age","sex","incap"),names_to="year",values_to="txincap") %>%
  arrange(sex,incap,year,age) %>%
  mutate(year=as.numeric(year),
         agelow = str_extract(age,"^[[:digit:]]{1,2}(?<=[![:digit:]])"),
         agehigh = str_extract(age,"(?<=[[:digit:]]{1,2}\\-)[[:digit:]]{2}"),
         age =ifelse(!is.na(agehigh),
                     paste0("[",agelow,",",(as.numeric(agehigh)+1),")"),
                     paste0("[",agelow,",Inf]"))) %>%
  select(-agelow,-agehigh)

poploc <- FRInseePopulation %>%
  rename(age = age0101) %>%
  mutate(year = year-1,
         sex = recode(sex, "M" = "male", "F" = "female")) %>%
  filter(year>=2008,geo=="metropolitan france")

txincap_all <- poploc %>%
  mutate(age = cut(age, c(0,seq(15,85,5),Inf), include.lowest = TRUE, right=FALSE)) %>%
  filter(year %in% unique(txincap$year)) %>%
  select(age,sex,year,popx) %>%
  left_join(txincap, by=c("year","age","sex")) %>%
  mutate(nbincap=popx*txincap/100) %>%
  select(-sex,-txincap) %>%
  group_by(year,age,incap) %>% summarise_all(sum) %>% ungroup() %>%
  mutate(txincap=100*nbincap/popx,
         sex="all") %>%
  select(-popx,-nbincap)

FRGaliEUSilc <- bind_rows(txincap, txincap_all) %>%
  arrange(sex,year,incap) %>%
  rename(agebracket = age, prevalence=txincap) %>%
  mutate(prevalence=prevalence/100)

# ===================================================================================
usethis::use_data(FRInseeMortalityForecast2016,
                  FRInseePopulationForecast2016,
                  FRInseeMortalityrates,
                  FRInseePopulation,
                  FRDreesVQSsurvey2014,
                  FRDreesAPA2017,
                  FRDreesAPA,
                  FRGaliEUSilc,
                  sullivan,
                  description_sullivan,
                  overwrite = T)
