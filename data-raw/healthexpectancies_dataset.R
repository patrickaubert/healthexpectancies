# data-raw/healthexpectancies_dataset.R
# Data import and processing pipeline

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
         age = as.numeric(age),
         sex = as.factor(sex))

names_sullivan <- read_excel("data-raw/sullivan_manual_jun2007.en.xls",
                             sheet = "Ex 1",
                             range = "A4:O4")
description_sullivan <- data.frame(
  heading = names(sullivan),
  description = names(names_sullivan)
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
         age = as.numeric(age),
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
usethis::use_data(FRInseeMortalityForecast2016,
                  FRInseePopulationForecast2016,
                  FRDreesVQSsurvey2014,
                  sullivan,
                  description_sullivan,
                  overwrite = T)
