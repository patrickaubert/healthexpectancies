# Creates the dataset 'FRAPAprevalence' : prevalences of the APA (allocation personnalisee d'autonomie) by age bracket
# Source : package 'asdep' (DREES, enquete annuelle 'Aide sociale'), table 'ASDEPprevalaidessoc'
# Total of the APA, all GIR and all places of residence (at home and in institutions)

library(dplyr)

asdep_table <- as.data.frame(asdep::ASDEPprevalaidessoc)     # (installed package 'asdep')

apa <- asdep_table %>% filter(prestation == "APA", gir == "Ensemble", lieu == "ensemble", recale_gir)

# the same year can appear in two decompositions of ages : 6 brackets (60-64, ..., 80-84, 85+) and 8 brackets (..., 90-94, 95+)
FRAPAprevalence <- bind_rows(
  apa %>% filter(decomp6age) %>% mutate(decomp = 6L),
  apa %>% filter(decomp8age) %>% mutate(decomp = 8L)
) %>%
  mutate(year = as.integer(annee),
         agebracket = as.character(age),
         age = as.integer(sub("^.([0-9]+),.*$", "\\1", agebracket))) %>%
  select(year, decomp, agebracket, age, nb, pop, prevalence) %>%
  arrange(decomp, year, age) %>%
  as.data.frame()

save(FRAPAprevalence, file = "data/FRAPAprevalence.rda", compress = "bzip2", version = 2)
