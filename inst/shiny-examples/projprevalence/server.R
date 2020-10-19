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
  # calcul des EVSI

  evsi <- reactive({

    # années retenues
    vyear <- unique( c(2014, input$anneeProj, 2020, 2025, 2030, 2035, 2040, 2050) )
    vyear <- vyear[order(vyear)]

    # 1) on récupère les prévalences par âge quinquennal et on calcul des prévalences approchées par âge fin

    # --- récupération des données sources : VQS 2014 pour les prévalences, projections de l'Insee (2016) pour les populations
    donnees <- FRInseePopulationForecast2016 %>%
      rename(age = age0101) %>%
      mutate(year = as.numeric(year),
             age = as.numeric(age),
             sex = as.factor(sex)) %>%
      mutate(year = year-1)

    donneesregr <- donnees %>%
      mutate(agebracket = cut(age, breaks = c(seq(60,95,5),Inf), include.lowest = TRUE, right = FALSE)) %>%
      filter(year==2014, age>=60) %>%
      left_join( FRDreesVQSsurvey2014  %>% filter(limitationtype == input$limtype ) %>% select(-age),
                 by = c("sex","agebracket") ) %>%
      mutate(a = age+0.5-60, a2 = a^2, a3 = a^3, a4 = a^4)

    # --- regressions
    fitted_models <- donneesregr %>% group_by(sex) %>%
      do(model = lm(prevalence ~ a + a2 + a3 + a4 , data = ., weights=popx))
    prevalences2014 <- fitted_models %>% augment(model) %>%
      ungroup() %>%
      mutate(age=a-0.5+60) %>%
      select(sex, age, prevalence, .fitted) %>%
      filter(age <= 100) %>%
      rename(prevalence.2014 = prevalence,
             prev.approx.2014 = .fitted) %>%
      pivot_longer(cols=-c(sex,age),
                   names_to = "indicateur",
                   values_to = "prevalence")

    # prévalences en 2014
    prev2014 <- prevalences2014 %>%
      filter(indicateur == "prev.approx.2014") %>%
      rename(pix = prevalence) %>%
      select(sex,age,pix)

    # 2) à partir des prévalences et des quotients de mortalité, on calcule les EV et EVSI en 2014

    # calcul des EVSI et EVI à l'année de référence (2014)
    qmort2014 <- FRInseeMortalityForecast2016 %>%
      select(year,sex,age,qx) %>%
      filter(age >= 60, year %in% c(2014) ) %>%
      mutate(mx = qx) %>%
      left_join(prev2014, by = c("sex","age")) %>%
      filter(!is.na(pix), !is.na(qx))
    EV2014 <- CompleteDFLEtable(qmort2014)

    # 3) selon l'hypothèse, on calcule en projection : soit les EVSI & EVI en fonction des prévalences, soit les prévalences en fonctions des EVI et EVSI

    # calcul des prévalences en projection
    qmortproj <- FRInseeMortalityForecast2016 %>%
      select(year,sex,age,qx) %>%
      filter(age >= 60, year %in% c( vyear[vyear != 2014] ) ) %>%
      mutate(mx = qx) %>%
      left_join(EV2014 %>% select(sex,age,DFLEx,DLEx,pctDFLEx,pix), by = c("sex","age")) %>%
      filter(!is.na(DFLEx), !is.na(qx))
    if (input$optionProj == "evsicst") { qmortproj <- qmortproj %>% select(-c(DLEx,pctDFLEx,pix))
    } else if (input$optionProj == "pctevsicst") { qmortproj <- qmortproj %>% select(-c(DFLEx,DLEx,pix))
    } else if (input$optionProj == "evicst") { qmortproj <- qmortproj %>% select(-c(pctDFLEx,DFLEx,pix))
    } else if (input$optionProj == "prevcst") { qmortproj <- qmortproj %>% select(-c(DFLEx,DLEx,pctDFLEx))
    }
    EVproj <- CompleteDFLEtable(qmortproj)

    # tables avec les valeurs en projections
    projections <- rbind( EV2014, EVproj) %>%
      select(sex,age,year,ex,DFLEx,DLEx,pctDFLEx,pix) %>%
      pivot_longer(cols=-c(sex,age,year),
                   names_to = "indicateur",
                   values_to = "ev")

    # 4) on rassemble tous les indicateurs dans la table en sortie

    # en sortie : tables avec les prévalences et les EV, par sexe et par année
    rbind(
      prevalences2014 %>%
        rename(ev = prevalence) %>%
        select(sex,age,indicateur,ev) %>%
        mutate(year = 2014),
      projections %>%
        filter((indicateur != "pix") | (year != 2014))
    ) %>%
      mutate(indicateur = recode(indicateur,
                                 "pix" = "prev.proj",
                                 "ex" = "EV",
                                 "DFLEx" = "EVSI",
                                 "DLEx" = "EVI",
                                 "pctDFLEx" = "% EVSI/EV"),
             sex = recode(sex, "male" = "Hommes", "female" = "Femmes"),
             ev = case_when(indicateur %in% c("prev.proj","prevalence.2014","prev.approx.2014") ~ round( 100 * ev, 1),
                            indicateur %in% c("EV","EVSI","EVI","% EVSI/EV") ~ round( ev, 1) )
      )
  })

  # ========================================================
  # graphiques en output

  # --- prévalences
  output$prevproj <- renderPlotly({
    #tab <- prevage() %>%
    tab <- evsi() %>%
      filter(year %in% c( 2014, input$anneeProj),
             indicateur %in% c("prev.proj","prevalence.2014","prev.approx.2014") ) %>%
      rename(prevalence = ev)
    g <- ggplot(tab , aes(x=age,y=prevalence,colour=indicateur) ) +
      geom_line() +
      scale_y_continuous(limits = c(0,100)) +
      facet_wrap( ~ sex)
    ggplotly(g)
  })

  # --- EV et EVSI en 2014
  output$evsi <- renderPlotly({
    tab <- evsi() %>%
      filter(year == 2014,
             indicateur %in% c("EV","EVSI","EVI"))
    g <- ggplot(tab , aes(x=age,y=ev,colour=indicateur) ) +
      geom_line() +
      facet_wrap( ~ sex)
    ggplotly(g)
  })

  # --- EV et EVSI projeté
  output$evsiproj <- renderPlotly({
    tab <- evsi() %>%
      filter(year == input$anneeProj,
             indicateur %in% c("EV","EVSI","EVI"))
    g <- ggplot(tab , aes(x=age,y=ev,colour=indicateur) ) +
      geom_line() +
      facet_wrap( ~ sex)
    ggplotly(g)
  })

  # --- proportion EV passée sans incapacité
  output$pctevsiproj <- renderPlotly({
    tab <- evsi() %>%
      filter(indicateur %in% c("% EVSI/EV"))
    g <- ggplot(tab , aes(x=age,y=ev,colour=year) ) +
      geom_line() +
      facet_wrap( ~ sex)
    ggplotly(g)
  })

  # --- EV et EVSI projetées, à un âge donné
  output$evsiprojage <- renderPlotly({
    tab <- evsi() %>%
      filter(indicateur %in% c("EV","EVSI","EVI"),
             age == input$ageEVSI)
    g <- ggplot(tab , aes(x=year,y=ev,colour=indicateur) ) +
      geom_line() +
      facet_wrap( ~ sex)
    ggplotly(g)
  })

  # --- proportion EV passée sans incapacité, à un âge donné
  output$pctevsiprojage <- renderPlotly({
    tab <- evsi() %>%
      filter(indicateur %in% c("% EVSI/EV"),
             age == input$ageEVSI)
    g <- ggplot(tab , aes(x=year,y=ev,colour=indicateur) ) +
      geom_line() +
      scale_y_continuous(limits = c(0,100)) +
      facet_wrap( ~ sex)
    ggplotly(g)
  })


  # ---- Nombre de personnes en incapacité
  # to be done ...

  # ---- EV et EVSI à 65 ans, pour diverses années (2025, 2030, 2035, 2040, 2050)
  # to be done ...

  # ---- texte explicatif
  # to be done ...

}
