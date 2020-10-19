server <- function(input, output) {

  # ========================================================
  # mise en forme des graphiques dans l'appli

  ggplotlocal <- function(...) {
    ggplot(...)
    # to be done: ajout mis en forme
  }


  # ========================================================
  # table des prévalences en 2014 (source : DREES, VQS 2014)

  prevage <- reactive({

    # --- récupération des données sources : VQS 2014 pour les prévalences, projections de l'Insee (2016) pour les populations

    donnees <- FRInseePopulationForecast2016 %>%
      rename(age = age0101) %>%
      mutate(year = as.numeric(year),
             age = as.numeric(age),
             sex = as.factor(sex)) %>%
      mutate(year = year-1,
             agebracket = cut(age, breaks = c(seq(60,95,5),Inf), include.lowest = TRUE, right = FALSE)) %>%
      filter(year==2014, age>=60) %>%
      left_join( FRDreesVQSsurvey2014  %>% filter(limitationtype == input$limtype ) %>% select(-age),
                 by = c("sex","agebracket") ) %>%
      mutate(a = age+0.5-60, a2 = a^2, a3 = a^3, a4 = a^4)

    # --- regressions

    fitted_models <- donnees %>% group_by(sex) %>%
      do(model = lm(prevalence ~ a + a2 + a3 + a4 , data = ., weights=popx))

    resultats <- fitted_models %>% augment(model) %>%
      ungroup() %>%
      mutate(age=a-0.5+60) %>%
      select(sex, age, prevalence, .fitted) %>%
      filter(age <= 100) %>%
      rename(prevalence.2014 = prevalence,
             prev.approx.2014 = .fitted) %>%
      pivot_longer(cols=-c(sex,age),
                   names_to = "typeprev",
                   values_to = "prevalence")

    resultats
  })

  # ========================================================
  # table des prévalences projetées

  prevproj <- reactive({

  })

  # ========================================================
  # calcul des EVSI

  evsi <- reactive({

    prev <- prevage() %>%
      filter(typeprev == "prev.approx.2014") %>%
      rename(pix = prevalence) %>%
      select(sex,age,pix)

    qmort <- FRInseeMortalityForecast2016 %>%
      select(year,sex,age,qx) %>%
      filter(age >= 60, year %in% c(2014)) %>%
      mutate(mx = qx) %>%
      left_join(prev, by = c("sex","age")) %>%
      filter(!is.na(pix), !is.na(qx))

    CompleteDFLEtable(qmort) %>%
      select(sex,age,year,ex,DFLEx,pctDFLEx) %>%
      pivot_longer(cols=-c(sex,age,year),
                   names_to = "indicateur",
                   values_to = "ev")
  })

  # ========================================================
  # graphiques en output

  #to be done:
  #       axe vertical = de 0 à 100%
  #       belle mise en forme (arrondi, etc)
  #       ajouter plotly

  # --- prévalences
  output$prevproj <- renderPlot({
    tab <- prevage()
    ggplot(tab , aes(x=age,y=prevalence,colour=typeprev) ) +
      geom_line() +
      facet_wrap( ~ sex)
  })

  # --- EV et EVSI
  output$evsi <- renderPlot({
    tab <- evsi() %>%
      filter(indicateur %in% c("ex","DFLEx")) %>%
      mutate(indicateur = recode(indicateur,
                                 "ex" = "EV",
                                 "DFLEx" = "EVSI"))
    ggplot(tab , aes(x=age,y=ev,colour=indicateur) ) +
      geom_line() +
      facet_wrap( ~ sex)
  })

  # ---- Nombre de personnes en incapacité
  # to be done ...

  # ---- EV et EVSI à 65 ans, pour diverses années (2025, 2030, 2035, 2040, 2050)
  # to be done ...

  # ---- texte explicatif
  # to be done ...

}
