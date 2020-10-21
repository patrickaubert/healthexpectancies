server <- function(input, output) {

  # ========================================================
  # mise en forme des graphiques dans l'appli

  ggplotlocal <- function(...) {
    ggplot(...)
    # to be done: ajout mis en forme
  }

  # ========================================================
  # calcul des EVSI

  evsi <- reactive({

    # années retenues
    vyear <- unique( c(2014, input$anneeProj, 2020, 2025, 2030, 2035, 2040, 2050) )
    vyear <- vyear[order(vyear)]

    # 1) on récupère les prévalences par âge quinquennal et on calcul des prévalences approchées par âge fin

    # --- récupération des données sources :  projections de l'Insee (2016) pour les populations
    donnees <- FRInseePopulationForecast2016 %>%
      rename(age = age0101) %>%
      mutate(year = as.numeric(year),
             age = as.numeric( age ),
             sex = as.factor(sex)) %>%
      mutate(year = year-1) %>%
      filter(age >= 60, year %in% c( vyear ) )

    # --- récupération des données sources : VQS 2014 pour les prévalences, qu'on apprie aux projections de l'Insee (2016) pour les populations
    donneesregr <- donnees %>%
      mutate(agebracket = cut(age, breaks = c(seq(60,95,5),Inf), include.lowest = TRUE, right = FALSE)) %>%
      filter(year==2014, age>=60) %>%
      left_join( FRDreesVQSsurvey2014  %>% filter(limitationtype == input$limtype ) %>% select(-age),
                 by = c("sex","agebracket") ) %>%
      rename(prevalence.ref = prevalence)

    # --- lissage des prévalences
    prevalencesref <- donneesregr %>% group_by(sex) %>%
      mutate(prev.approx = case_when(
        input$approxPrev ~ prevalence_to_polynomial(age, prevalence.ref, weight=popx),
        !input$approxPrev ~ prevalence.ref) ) %>%
      ungroup() %>%
      select(sex, age, prev.approx, prevalence.ref) %>%
      pivot_longer(cols=-c(sex,age),
                   names_to = "indicateur",
                   values_to = "prevalence")

    # --- prévalences de référence, retenues pour les projections
    prevref <- prevalencesref %>%
      filter(indicateur == "prev.approx") %>%
      rename(pix = prevalence) %>%
      select(sex,age,pix)

    # 2) à partir des prévalences et des quotients de mortalité, on calcule les EV et EVSI à l'année de référence
    #    puis, selon l'hypothèse, on calcule en projection : soit les EVSI & EVI en fonction des prévalences, soit les prévalences en fonctions des EVI et EVSI

    # --- calcul des coefficients de mortalité et des prévalences pour l'année de référence (2014)
    qmortref <- FRInseeMortalityForecast2016 %>%
      # on filtre selon l'âge et l'année
      select(year,sex,age,qx) %>%
      filter(age >= 60, age <= 105, year %in% c(2014) ) %>%
      # on ajoute les prévalences en projection
      left_join(prevref, by = c("sex","age")) %>%
      filter(!is.na(pix), !is.na(qx))

    # --- récupération des coefficients de mortalité en projection
    qmortproj <- FRInseeMortalityForecast2016 %>%
      select(year,sex,age,qx) %>%
      filter(age >= 60, age <= 105, year %in% c( vyear[vyear != 2014] ) )

    # --- tables avec les valeurs en projections
    projections <- prevalenceForecast( qmortref, qmortproj , input$optionProj) %>%
      select(sex,age,year,ex,DFLEx,DLEx,pctDFLEx,pix)

    # 3) on ajoute les effectifs pour avoir des nombres de personnes âgées en incapacité

    projections <- projections %>%
      left_join(donnees, by = c("age","year","sex")) %>%
      mutate(nbIncap = popx * pix)   %>%
      pivot_longer(cols=-c(sex,age,year),
                   names_to = "indicateur",
                   values_to = "ev")

    # 4) on rassemble tous les indicateurs dans la table en sortie

    # en sortie : tables avec les prévalences et les EV, par sexe et par année
    rbind(
      prevalencesref %>%
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
             ev = case_when(indicateur %in% c("prev.proj","prevalence.ref","prev.approx") ~ round( 100 * ev, 1),
                            indicateur %in% c("EV","EVSI","EVI","% EVSI/EV") ~ round( ev, 1),
                            indicateur %in% c("popx","nbIncap") ~ round( ev, 1))
      )
  })

  # ========================================================
  # graphiques en output

  # --- prévalences observées, lissées et projetées
  output$prevproj <- renderPlotly({
    #tab <- prevage() %>%
    tab <- evsi() %>%
      filter(year %in% c( 2014, input$anneeProj),
             indicateur %in% c("prev.proj","prevalence.ref","prev.approx") ) %>%
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
      #filter(year == 2014,
      filter(year %in% c(2014, input$anneeProj) ,
             indicateur %in% c("EV","EVSI","EVI"))
    g <- ggplot(tab , aes(x=age,y=ev,colour=indicateur) ) +
      geom_line() +
      facet_wrap(year ~ sex)
    ggplotly(g)
  })

  # --- EV et EVSI projetées
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
    tab <- rbind(tab,
                 tab %>% mutate(ev = 100 - ev,
                                indicateur = "% EVI/EV"))
    g <- ggplot(tab , aes(x=year,y=ev,colour=indicateur) ) +
      geom_line() +
      scale_y_continuous(limits = c(0,100)) +
      facet_wrap( ~ sex)
    ggplotly(g)
  })

  # ---- répartition par âge des personnes en incapacité, à l'année de projection
  output$nbIncapAge <- renderPlotly({

    tab <- evsi() %>%
      filter(indicateur %in% c("nbIncap"), year == input$anneeProj) %>%
      select(sex,age,ev) %>%
      mutate(agebracket = cut(age, breaks = c(seq(60,95,5),Inf), include.lowest = TRUE, right = FALSE) ) %>%
      group_by(sex,agebracket) %>% summarise_all(sum) %>% ungroup() %>%
      rename(nb = ev) %>%
      mutate(nb.milliers = round( nb/1000, 0))
    g <- ggplot(tab , aes(x=agebracket,y=nb.milliers) ) +
      geom_bar(stat="identity", position="identity") +
      facet_wrap( ~ sex)
    ggplotly(g)
  })

  # ---- Nombre de personnes en incapacité, selon l'année
  output$nbIncapProj <- renderPlotly({
    tab <- evsi() %>%
      filter(indicateur %in% c("nbIncap")) %>%
      select(year,sex,ev) %>%
      group_by(year,sex) %>% summarise_all(sum) %>% ungroup() %>%
      rename(nb = ev) %>%
      mutate(nb.milliers = round( nb/1000, 0))
    g <- ggplot(tab , aes(x=year,y=nb.milliers,colour=sex) ) +
      geom_line()
    ggplotly(g)
  })



  # ---- texte explicatif
  # to be done ...

}
