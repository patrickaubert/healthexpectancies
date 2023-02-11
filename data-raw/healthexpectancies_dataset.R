# data-raw/healthexpectancies_dataset.R
# Data import and processing pipeline

library(devtools)
library(readxl)
library(httr)
library(openxlsx)
library(tidyverse)
library(janitor)

load_all()

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
# mortality rate are averages over 3 years (for instance, 2018 is the average of 2017, 2018, and 2019)
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
# == Alternative: mortality data from table 'T69' released by Insee

# quotient de mortalité d'après les tableaux T69 de l'Insee (table de mortalité abrégée)
# données téléchargées le 13/04/2022 à l'adresse https://www.insee.fr/fr/statistiques/6036439?sommaire=6036447
# (Bilan démographique 2021 - Chiffres détaillés - Paru le : 18/01/2022 )
# nouvelle extraction des données le 04/07/2022
# (Bilan démographique 2022 - Chiffres détaillés - Paru le : 17/01/2023 )
# extraction des données le 02/02/2023

#url_t69 <- "https://www.insee.fr/fr/statistiques/fichier/6036439/fe_dod_quotients_mortalite.xlsx"
url_t69_fe <- "https://www.insee.fr/fr/statistiques/fichier/6686511/fe_dod_quotients_mortalite.xlsx"
url_t69_fm <- "https://www.insee.fr/fr/statistiques/fichier/6686511/fm_dod_quotients_mortalite.xlsx"

qmort_t69 <- bind_rows(
  read.xlsx(
    #xlsxFile = "data-raw/fe_dod_quotients_mortalite.xlsx",
    xlsxFile = url_t69_fe,
    sheet = "Qmort-F", #"qmortf",
    startRow = 5,
    colNames = TRUE, skipEmptyRows = TRUE, skipEmptyCols = TRUE) %>%
    filter(grepl("^[[:digit:]]{4}",Année)) %>%
    pivot_longer(cols=-"Année",names_to="age",values_to="qx") %>%
    mutate(sex = "female"),
  read.xlsx(
    #xlsxFile = "data-raw/fe_dod_quotients_mortalite.xlsx",
    xlsxFile = url_t69_fe,
    sheet = "Qmort-H", #"qmorth",
    startRow = 5,
    colNames = TRUE, skipEmptyRows = TRUE, skipEmptyCols = TRUE) %>%
    filter(grepl("^[[:digit:]]{4}",Année)) %>%
    pivot_longer(cols=-"Année",names_to="age",values_to="qx") %>%
    mutate(sex = "male")
) %>%
  rename(year=Année) %>%
  mutate(age = str_extract(age,"^[[:digit:]]+") %>% as.numeric(),
         year= as.numeric(year),
         qx = qx/100000)

# correction pour tenir compte du fait qu'il s'agit de l'âge atteint en fin d'année => on considère que les décès à l'âge A au 31/12 se répartissent entre les âges A-1 et A en année révolue
# on calcule la part des décès en A-1 et 1 à partir de la comparaison entre les tables de quotients de mortalité T68 et T69 de l'Insee, en moyenne pour les trois dernières années disponibles
# (cf. on cherche à estimer la portion p(x) page 13 du document méthodologique de l'Insee https://www.insee.fr/fr/metadonnees/source/fichier/Indicateurs_d%C3%A9mo_mai2018.pdf)

corr <- qmort_t69 %>%
  filter(year>=max(FRInseeMortalityrates$year)-1,year<=max(FRInseeMortalityrates$year)+1) %>%
  select(-year) %>%
  group_by(sex,age) %>% summarise_all(mean) %>% ungroup()

corr_ref <- FRInseeMortalityrates %>%
  filter(geo=="france",sex!="all",year==max(FRInseeMortalityrates$year)) %>%
  select(sex,age,qx)

corr <- corr_ref %>%
  left_join(corr %>% rename(qx_69=qx), by=c("sex","age")) %>%
  left_join(corr %>% rename(qx_69p=qx) %>% mutate(age=age-1), by=c("sex","age")) %>%
  arrange(sex,age) %>%
  group_by(sex) %>%
  mutate(ecart=cumsum(qx)-cumsum(qx_69)) %>%
  ungroup() %>%
  mutate(px=ecart/qx_69p,
         age=age+1) %>%
  filter(!is.na(px)) %>%
  select(sex,age,px)

# on applique le correctif

# ancienne méthode (hypothèse p(x)=0.5 à tous âges)
#qmort_t69_bis <- bind_rows(
#  qmort_t69 %>% mutate(qx=qx/2,age=pmax(0,age-1)),
#  qmort_t69 %>% mutate(qx=qx/2) ) %>%
#  group_by(year,sex,age) %>% summarise_all(sum) %>% ungroup()

# nouvelle méthode (p(x) estimé par comparaison entre les tables T68 et T69)
qmort_t69_bis <- qmort_t69 %>%
  left_join(corr ,by=c("sex","age")) %>%
  left_join(qmort_t69 %>% rename(qxf=qx) %>% mutate(age=age-1) %>% filter(age>=0), by=c("year","sex","age")) %>%
  left_join(corr %>% rename(pxf=px)  %>% mutate(age=age-1),by=c("sex","age")) %>%
  mutate(px = case_when(
    !is.na(px) ~ px,
    is.na(px) & age==0 ~ 0,
    is.na(px) & age>0 ~0.5
  ),
  qxf = ifelse(is.na(qxf),qx,qxf),
  pxf = ifelse(is.na(pxf),px,pxf)) %>%
  mutate(qx = (1-px)*qx + pxf*qxf) %>%
  select(year,age,sex,qx)

qmort_t69 <- bind_rows(
  qmort_t69 %>% mutate(def.age = "age at end of year"),
  qmort_t69_bis %>% mutate(def.age = "current age (approx)")
) %>%
  mutate(def.age = as.factor(def.age))

# ajout des quotients de mortalité pour l'ensemble des sexes
# (RQ méthode utilisée jusqu'au 31/07/2022 : en faisant l'hypothèse d'un partage 50/50 d'hommes et de femmes à la naissance)

partnaiss <- FRInseePopulation %>% filter(age0101==0,geo=="france") %>% select(year,sex,popx) %>%
  mutate(sex = recode(as.character(sex), "M"="male","F"="female"),
         year=year-1) %>%
  rename(partnaiss=popx) %>%
  group_by(year) %>% mutate(partnaiss=partnaiss/sum(partnaiss)) %>% ungroup()

# transitoire : on réplique en 2022 la part de naissance de 2021
partnaiss <- bind_rows(partnaiss, partnaiss %>% filter(year==2021) %>% mutate(year=2022))

qmort_t69_all <- qmort_t69 %>%
  mutate(qx=1-qx) %>%
  arrange(year,sex,def.age,age) %>% group_by(year,sex,def.age) %>% mutate(qx=cumprod(qx)) %>% ungroup() %>%
  left_join(partnaiss, by=c("year","sex") ) %>%
  mutate(qx = qx * partnaiss) %>%
  select(-sex,-partnaiss) %>% group_by(year,def.age,age) %>% summarise_all(sum) %>% ungroup()

qmort_t69_all <- qmort_t69_all %>%
  left_join(qmort_t69_all %>% mutate(age=age+1) %>% rename(lqx=qx), by=c("year","age","def.age")) %>%
  mutate(qx = ifelse(age==0,1-qx,1-qx/lqx),
         sex = "all") %>%
  select(-lqx)

qmort_t69 <- bind_rows( qmort_t69 , qmort_t69_all)

# verif <- CompleteDFLEtable(qmort_t69 %>% filter(def.age=="current age (approx)")) %>% select(sex,year,age,ex) %>% filter(age %in% c(0,60,65))

FRInseeMortalityrates_t69 <- qmort_t69

# == correction of errors :
# 2023/02/02 : add year 2022
# 2022/10/09 : add 2021 for all sexes current age
# 2022/07/31 : take into account the share of male/female at birth in calculating average life expectancy and mortality ratios
# 2022/07/04 : a more accurate estimate of the share of deaths before people's birthday is used (the share was supposed to be egal to 0.5 at every age in previous versions)

# ===================================================================================
# Forecasted populations, from Insee's 2021 population forecast
# ===================================================================================

# Table : Pyramide des âges interactive
# source 1 : https://www.insee.fr/fr/outil-interactif/5014911/pyramide.htm#!l=en (for ages up to 99)
# source 2 : https://www.insee.fr/fr/statistiques/5894083?sommaire=5760764
# released : ?
# extraction source 1 : 2022/06/24
# extraction source 2 : 2022/06/30
# note: also includes observed data for 1990-2022

# data from interactive pyramids (source 1)

FRInseePopulationForecast2021 <- bind_rows(
  read_csv2("data-raw/donnees_pyramide_act_2022.csv") %>% mutate(geo="france",type.obs="observed"),
  read_csv2("data-raw/donnees_pyramide_proj_2022.csv") %>% mutate(geo="france",type.obs="forecasted")  )  %>%
  rename(year = ANNEE,
         sex = SEXE,
         popx0101 = POP,
         age0101 = AGE) %>%
  mutate(year = as.numeric(year),
         age0101 = as.numeric(age0101),
         sex = as.factor(sex) %>% recode("M" = "male", "F" = "female"),
         geo = as.factor(geo) )

# data from 2021 population forecast (source 2)
# NB: data from the interactive pyramids aggregates all ages above 99

urlproj2021 <- "https://www.insee.fr/fr/statistiques/fichier/5894083/00_central.xlsx"

projMale <- openxlsx::read.xlsx(urlproj2021, sheet = "populationH", rows=c(2,102:108))
names(projMale)[1] <- "age0101"
projMale <- projMale %>% pivot_longer(-c("age0101"), names_to = "year", values_to = "popx0101")

projFemale <- openxlsx::read.xlsx(urlproj2021,sheet = "populationF", rows=c(2,102:108))
names(projFemale)[1] <- "age0101"
projFemale <- projFemale %>% pivot_longer(-c("age0101"), names_to = "year", values_to = "popx0101")

FRInseePopulationForecast2021_2 <- rbind(
  projFemale %>% mutate(sex = "female"),
  projMale %>% mutate(sex = "male")
) %>%
  filter(year %in% unique(FRInseePopulationForecast2021$year), popx0101>0) %>%
  mutate(type.obs = case_when(year<= 2021 ~ "observed",year >= 2022 ~ "forecasted") %>% as.factor(),
         age0101 = as.numeric(str_extract(age0101,"^[[:digit:]]+")),
         sex = as.factor(sex),
         year=as.numeric(year),
         geo="france")

verif <- FRInseePopulationForecast2021 %>% filter(age0101>=99) %>% select(year,sex,age0101,popx0101) %>%
  left_join(FRInseePopulationForecast2021_2 %>%
              select(year,sex,popx0101) %>%
              rename(pop2=popx0101) %>%
              group_by(year,sex) %>% summarise_all(sum) %>% ungroup(),
            by = c("year","sex")) %>%
  mutate(ecart=popx0101-pop2, correc=popx0101/pop2)

FRInseePopulationForecast2021_2 <- FRInseePopulationForecast2021_2 %>%
  left_join(verif %>% select(year,sex,correc), by=c("year","sex") ) %>%
  mutate(popx0101 = round(popx0101*correc)) %>%
  select(-correc)


# final table

FRInseePopulationForecast2021 <- bind_rows(
  FRInseePopulationForecast2021 %>%
    filter(age0101<99,!(year==2022 & type.obs=="forecasted")),
  FRInseePopulationForecast2021_2
) %>%
  arrange(year,sex,age0101)

FRInseePopulationForecast2021 <- bind_rows(
  FRInseePopulationForecast2021,
  FRInseePopulationForecast2021 %>%
    select(-sex) %>%
    group_by(year,geo,type.obs,age0101) %>% summarise_all(sum) %>% ungroup() %>%
    mutate(sex = "all")
)

# ===================================================================================
# Forecasted mortality rates for men and women, from Insee's 2021 population forecast
# ===================================================================================

# raw data are downloaded from: https://www.insee.fr/fr/statistiques/5894083?sommaire=5760764
# ('central' scenario)
# download: 2022/06/29

ulrmort2021 <- "https://www.insee.fr/fr/statistiques/fichier/5894083/00_central.xlsx"

mortalityMale <- openxlsx::read.xlsx(ulrmort2021,
                            sheet = "hyp_mortaliteH",
                            rows=c(2:123))
names(mortalityMale)[1] <- "age3112"
mortalityMale <- mortalityMale %>%
  pivot_longer(-c("age3112"), names_to = "year", values_to = "qx") %>%
  mutate(qx = qx/100000 )

mortalityFemale <- openxlsx::read.xlsx(ulrmort2021,
                                     sheet = "hyp_mortaliteF",
                                     rows=c(2:123))
names(mortalityFemale)[1] <- "age3112"
mortalityFemale <- mortalityFemale %>%
  pivot_longer(-c("age3112"), names_to = "year", values_to = "qx") %>%
  mutate(qx = qx/100000 )

FRmortalityForecast2021 <- rbind(
  mortalityFemale %>% mutate(sex = "female"),
  mortalityMale %>% mutate(sex = "male")
) %>%
  mutate(year = as.numeric(str_extract(year,"^[[:digit:]]{4}")),
         type.obs = case_when(year<= 2017 ~ "observed",
                          year>2017 & year <= 2020 ~ "observed (prov.)",
                          year >= 2021 ~ "forecasted") %>% as.factor(),
         age3112 = as.numeric(age3112),
         sex = as.factor(sex))


# transform dataset from age at the end of the year to age at last birthday

FRmortalityForecast2021_2 <- rbind(
  FRmortalityForecast2021 %>% mutate(qx = qx/2, age = age3112),
  FRmortalityForecast2021 %>% mutate(qx = qx/2, age = pmax(0,age3112-1) )
) %>%
  select(-age3112) %>%
  group_by(year,type.obs,sex) %>%  mutate(qx = ifelse(age==max(age),2*qx,qx)) %>%  ungroup() %>%
  group_by(year,type.obs,sex,age) %>%
  summarise_all(sum) %>%
  ungroup()

FRmortalityForecast2021 <- bind_rows(
  FRmortalityForecast2021  %>% mutate(def.age = "age at end of year") %>% rename(age=age3112),
  FRmortalityForecast2021_2 %>% mutate(def.age = "current age (approx)")
) %>%
  mutate(def.age = as.factor(def.age))

# adding mortality rates for both sex, supposing there are 50-50 men and women ar birth

FRmortalityForecast2021_all <- FRmortalityForecast2021 %>%
  mutate(qx=1-qx) %>%
  arrange(year,sex,type.obs,def.age,age) %>% group_by(year,sex,type.obs,def.age) %>% mutate(qx=cumprod(qx)) %>% ungroup() %>%
  select(-sex) %>% group_by(year,type.obs,def.age,age) %>% summarise_all(mean) %>% ungroup()

FRmortalityForecast2021_all <- FRmortalityForecast2021_all %>%
  left_join(FRmortalityForecast2021_all %>% mutate(age=age+1) %>% rename(lqx=qx), by=c("year","age","type.obs","def.age")) %>%
  mutate(qx = ifelse(age==0,1-qx,1-qx/lqx),
         sex = "all") %>%
  select(-lqx)

FRInseeMortalityForecast2021 <- bind_rows( FRmortalityForecast2021 , FRmortalityForecast2021_all)

# FRInseeMortalityForecast2021 %>% filter(year==1962) %>% ggplot(aes(y=qx,x=age,colour=sex,group=sex)) + geom_line() + facet_wrap(~def.age)
# FRInseeMortalityForecast2021 %>% filter(year==2021) %>% ggplot(aes(y=qx,x=age,colour=sex,group=sex)) + geom_line() + facet_wrap(~def.age)
# FRInseeMortalityForecast2021 %>% filter(year==2070) %>% ggplot(aes(y=qx,x=age,colour=sex,group=sex)) + geom_line() + facet_wrap(~def.age)


# ===================================================================================
# Population of France, Insee
# ===================================================================================

# Table : Pyramide des âges interactive - France et France métropolitaine
# source : https://www.insee.fr/fr/outil-interactif/5014911/pyramide.htm
# complementary source (2021 forecast) : https://www.insee.fr/fr/outil-interactif/5896897/pyramide.htm#!y=2026&c=0
# released : ?
# new extraction for france : 2022/10/09

FRInseePopulation <- bind_rows(
  #read_csv2("data-raw/donnees_pyramide_act.csv") %>% mutate(geo="france"),
  read_csv2("https://www.insee.fr/fr/outil-interactif/5014911/data/FR/donnees_pyramide_act.csv") %>% mutate(geo="france"),
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
# Disability prevalences, from DREES' 2021 VQS survey
# ===================================================================================

# source : https://data.drees.solidarites-sante.gouv.fr/explore/dataset/enquete-vie-quotidienne-et-sante-2021-donnees-detaillees/information/
# Excel file : 'Enquête Vie quotidienne et santé 2021 - Données nationales.xlsx'
# Data published and extracted 2023/02/01
# New extraction 2023/02/11, following new release by DREES on 2023/02/10

prev_vqs_2021 <- read.xlsx(xlsxFile="https://data.drees.solidarites-sante.gouv.fr/api/datasets/1.0/enquete-vie-quotidienne-et-sante-2021-donnees-detaillees/attachments/enquete_vie_quotidienne_et_sante_2021_donnees_nationales_xlsx/",
                           sheet="Données nationales_parts",
                           rows=c(4:129))

eff_vqs_2021 <- read.xlsx(xlsxFile="https://data.drees.solidarites-sante.gouv.fr/api/datasets/1.0/enquete-vie-quotidienne-et-sante-2021-donnees-detaillees/attachments/enquete_vie_quotidienne_et_sante_2021_donnees_nationales_xlsx/",
                           sheet="Données nationales_effectifs",
                           rows=c(4:129))

# names of columns
nb_age <- (ncol(prev_vqs_2021)-3)/3

names(prev_vqs_2021)[4:ncol(prev_vqs_2021)] <- paste0(
  c(rep("male",nb_age),rep("female",nb_age),rep("all",nb_age)),
  "_",names(prev_vqs_2021)[4:ncol(prev_vqs_2021)])

names(eff_vqs_2021)[4:ncol(eff_vqs_2021)]  <- names(prev_vqs_2021)[4:ncol(prev_vqs_2021)]

FRDreesVQSsurvey2021 <- prev_vqs_2021 %>%
  fill(all_of(c("X1","X2","X3")), .direction="down") %>%
  pivot_longer(cols = -c("X1","X2","X3"),names_to="sex_age",values_to="prevalence")  %>%
  filter(!is.na(prevalence)) %>%
  full_join(
    eff_vqs_2021 %>%
      fill(all_of(c("X1","X2","X3")), .direction="down") %>%
      pivot_longer(cols = -c("X1","X2","X3"),names_to="sex_age",values_to="nb")  %>%
      filter(!is.na(nb)),
    by=c("X1","X2","X3","sex_age")
  ) %>%
  select(-X1) %>%
  mutate(prevalence= prevalence/100,
         sex = sex_age %>% str_extract("^[^_]+(?=_)"),
         age = sex_age %>% str_replace("^[^_]+_","") %>%
           recode("74-79.ans"="75-79.ans")) %>%
  select(-sex_age) %>%
  #filter(!(X3 %in% c("Non","Aucune","Très bon","Bon","Assez bon"))) %>%
  mutate(X3 = X3 %>% str_replace("^N.+ pas du tout","Ne peut pas du tout")) %>%
  mutate(agebracket = case_when(
    age == "Total" ~ "[5,Inf)",
    age == "85.ans.et.plus" ~ "[85,Inf)",
    TRUE ~ paste0("[",str_extract(age,"^[[:digit:]]+(?=\\-)"),",",(as.numeric(str_extract(age,"(?<=\\-)[[:digit:]]+(?=\\.)"))+1),")") ),
    agebracket = factor(agebracket)) %>%
  select(-age) %>%
  rename(limitationtype=X2,
         limitationintensity=X3)

usethis::use_data(FRDreesVQSsurvey2021, overwrite = T)

# dfle2021 <- FRInseeMortalityrates_t69 %>% filter(year==2021 & def.age=="current age (approx)" & age>=5) %>% select(-def.age) %>% mutate(agebracket=cut(age,breaks=c(seq(5,85,5),Inf),include_lowest=TRUE,right=FALSE))
# dfle2021 <- dfle2021 %>% left_join(FRDreesVQSsurvey2021, by=c("sex","agebracket")) %>% arrange(sex,limitationtype,limitationintensity,age) %>% select(-nb,-agebracket) %>% rename(pix=prevalence)
# dfle2021 <- CompleteDFLEtable(dfle2021, categories=c("limitationtype","limitationintensity"))
# dfle2021 %>% filter(age==60 & sex=="all" & grepl("^Difficultés",limitationtype) & limitationintensity!="Aucune") %>% ggplot(aes(y=DLEx,x=limitationtype,fill=limitationintensity)) + geom_bar(stat="identity",position="stack") + coord_flip() + labs(title="Espérance de vie avec ou sans incapacité à 60 ans")

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
# Prevalence of elder people living in institutions, from DREES's EHPA survey
# ===================================================================================

urlehpa2019 <- "https://drees.solidarites-sante.gouv.fr/sites/default/files/2022-07/ER1237.xlsx"
#urlehpa2015 <- "https://drees.solidarites-sante.gouv.fr/sites/default/files/2020-08/er1015.xlsx"
urlehpa2011 <- "https://drees.solidarites-sante.gouv.fr/sites/default/files/2020-08/er899.xls"

# year 2015 and 2019

eff1519 <- read.xlsx(
  xlsxFile = urlehpa2019,
  sheet="Graph1- Pyramide âges",
  rows = c(5:70)) %>%
  janitor::clean_names()

eff1519 <- eff1519 %>%
  rename(age=age_en_annees) %>%
  pivot_longer(cols = -"age",names_to="categ",values_to="nb") %>%
  mutate(sexe=categ %>% str_extract("^[^_]+(?=_)") %>% tolower(),
         annee=categ %>% str_extract("(?<=_)[^_]+$"),
         age=as.numeric(age),
         annee = as.numeric(annee),
         nb=abs(nb)) %>%
  select(-categ)

# years 2007 and 2011

httr::GET(urlehpa2011, write_disk(fileloc <- tempfile(fileext = ".xls")))
eff0711 <- read_excel(path = fileloc, sheet="graphe 1", range = "B3:F64")
unlink(fileloc)

eff0711 <- eff0711 %>%
  janitor::clean_names() %>%
  rename(age=age_revolu) %>%
  pivot_longer(cols = -"age",names_to="categ",values_to="nb") %>%
  mutate(sexe=categ %>% str_extract("^[^_]+(?=_)"),
         sexe=ifelse(is.na(sexe),"hommes",sexe),
         annee=categ %>% str_extract("[[:digit:]]+"),
         age=as.numeric(age),
         annee = as.numeric(annee),
         nb=abs(nb)) %>%
  select(-categ)

eff <- bind_rows(eff0711, eff1519) %>%
  arrange(annee,sexe,age) %>%
  left_join(
    FRInseePopulationForecast2021 %>%
      filter(geo=="france",type.obs=="observed",year<=2020) %>%
      mutate(year=year-1,
             sex = as.character(sex) %>% recode("male"="hommes","female"="femmes")) %>%
      rename(sexe=sex,annee=year,poptot=popx0101,age=age0101) %>%
      select(annee,sexe,age,poptot),
    by=c("annee","sexe","age")   )

FRDreesEHPA <- bind_rows(
  eff,
  eff %>%
    select(-sexe) %>%
    group_by(annee,age) %>% summarise_all(sum, na.rm=TRUE) %>% ungroup() %>%
    mutate(sexe="ensemble")
  ) %>%
  mutate(prevalence=nb/poptot) %>%
  rename(sex=sexe,year=annee) %>%
  mutate(sex = sex %>% recode("ensemble"="all","femmes"="female","hommes"="male"))

# logit <- function(x){log(x/(1-x))}
# eff %>% mutate(annee=as.factor(annee)) %>% ggplot(aes(y=prevalence,x=age,colour=annee,group=annee)) + geom_line() + facet_grid(~sexe) + coord_trans(y = "logit")


# ====================================================================================
usethis::use_data(FRInseeMortalityForecast2016,
                  FRInseeMortalityForecast2021,
                  FRInseeMortalityrates,
                  FRInseeMortalityrates_t69,
                  FRInseePopulation,
                  FRInseePopulationForecast2016,
                  FRInseePopulationForecast2021,
                  FRDreesVQSsurvey2014,
                  FRDreesVQSsurvey2021,
                  FRDreesAPA2017,
                  FRDreesAPA,
                  FRDreesEHPA,
                  FRGaliEUSilc,
                  sullivan,
                  description_sullivan,
                  overwrite = T)
