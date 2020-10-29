server <- function(input, output) {

  # ========================================================
  # mise en forme des graphiques dans l'appli

  ggplotlocal <- function(...) {
    ggplot(...)
    # to be done: ajout mis en forme
  }

  # ========================================================
  # calcul des EVSI
  refyear <- reactive({
    nameapa <- unique(FRDreesAPA2017$typepresta)
    if (input$limtype %in% nameapa)  { 2017 # Source: DREES, enquête Aide sociale 2017 (prévalences de l'APA)
    } else { 2014 } # Source : DREES, enquête VQS 2014 (prévalences des limitations et restrictions d'activité)
  })

  evsi <- reactive({

    # années retenues
    refyear <- refyear()
    vyear <- unique( c(refyear, input$anneeProj, 2020, 2025, 2030, 2035, 2040, 2050) )
    vyear <- vyear[order(vyear)]

    # 1) on récupère les prévalences par âge quinquennal et on calcul des prévalences approchées par âge fin

    # --- récupération des données sources :  projections de l'Insee (2016) pour les populations
    donneespop <- FRInseePopulationForecast2016 %>%
      rename(age = age0101) %>%
      mutate(year = as.numeric(year),
             age = as.numeric( age ),
             sex = as.factor(sex)) %>%
      mutate(year = year-1) %>%
      filter(age >= 60, year %in% c( vyear ) )

    # --- récupération des données sources : VQS 2014 ou Aide sociale 2017 pour les prévalences
    donneesprev <- rbind(
      FRDreesVQSsurvey2014,
      FRDreesAPA2017 %>% rename(limitationtype = typepresta)
      ) %>%
      filter(limitationtype == input$limtype ) %>%
      select(-age)

    # --- récupération des données sources : appariement prévalences aux projections de l'Insee (2016) pour les populations
    donneesregr <- donneespop %>%
      mutate(agebracket = cut(age, breaks = c(seq(60,95,5),Inf), include.lowest = TRUE, right = FALSE)) %>%
      filter(year == refyear, age>=60) %>%
      left_join( donneesprev,   by = c("sex","agebracket") ) %>%
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
      filter(age >= 60, age <= input$ageFinCalcul, year %in% c(refyear) ) %>%
      # on ajoute les prévalences en projection
      left_join(prevref, by = c("sex","age")) %>%
      filter(!is.na(pix), !is.na(qx))

    # --- récupération des coefficients de mortalité en projection
    qmortproj <- FRInseeMortalityForecast2016 %>%
      select(year,sex,age,qx) %>%
      filter(age >= 60, age <= input$ageFinCalcul, year %in% c( vyear[vyear != refyear] ) )

    # --- tables avec les valeurs en projections
    projections <- prevalenceForecast( qmortref, qmortproj , input$optionProj) %>%
      select(sex,age,year,ex,DFLEx,DLEx,pctDFLEx,pix)

    # 3) on ajoute les effectifs pour avoir des nombres de personnes âgées en incapacité

    projections <- projections %>%
      left_join(donneespop, by = c("age","year","sex")) %>%
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
        mutate(year = refyear),
      projections %>%
        filter((indicateur != "pix") | (year != refyear))
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
    tab <- evsi() %>%
      filter(year %in% c( refyear(), input$anneeProj),
             indicateur %in% c("prev.proj","prevalence.ref","prev.approx") ) %>%
      rename(prevalence = ev)
    g <- ggplot(tab , aes(x=age,y=prevalence,colour=indicateur) ) +
      geom_line() +
      scale_y_continuous(limits = c(0,100)) +
      facet_wrap( ~ sex)
    ggplotly(g)
  })

  # --- ratio des prévalences projetées par rapport aux observées, par sexe et âge
  output$ratioprev <- renderPlotly({
    tab <- evsi() %>%
      filter(year %in% c( refyear(), input$anneeProj),
             indicateur %in% c("prev.proj","prev.approx") ) %>%
      rename(prevalence = ev)
    tab <- tab %>% filter(indicateur == "prev.approx") %>%
      left_join(tab %>% filter(indicateur == "prev.proj") %>% rename(ratio = prevalence),
                by = c("sex","age")) %>%
      mutate(ratio = round( 100 * ratio / prevalence , 1) )
    g <- ggplot(tab , aes(x=age,y=ratio,colour=sex) ) +
      geom_line()
    ggplotly(g)
  })

  # --- EV et EVSI en 2014 (ou 2017)
  output$evsi <- renderPlotly({
    tab <- evsi() %>%
      filter(year %in% c(refyear(), input$anneeProj) ,
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



  # ---- textes explicatifs
  output$textePreval <- renderUI({
    txt <- paste("Prévalences pour l'incapacité : ",input$limtype,
                 " (voir documentation) selon l'âge, pour l'année de référence et l'année de projection (",input$anneeProj,"). ",
                 sep="")
    if (input$approxPrev) {
      txt <- paste(txt,
                   "Les prévalences par âge sont approximées par une fonction polynomiale de degré 4. ",sep = " ")
    }
    txt <- paste(
      txt,
      "Projections réalisées selon l'hypothèse : ",
      recode(input$optionProj,
             "cstDFLE" = "EVSI constante",
             "cstDLE" = "EVI constante",
             "cstPctDFLE" = "part de l'EVSI dans l'EV totale constante",
             "cstPrev" = "prévalences des incapacités constantes"),
      ".",
      sep="")
    txt
  })
  output$texteAge <- renderUI({
    paste("Espérances de vie (avec et sans incapacité) à l'âge de ",input$ageEVSI," ans.",sep="")
  })
  output$texteAn <- renderUI({
    paste("Projections pour l'année ",input$anneeProj,".",sep="")
  })

  output$documentation <- renderUI({
    doc <- paste(
      "Cette application interactive permet de réaliser des <b>projections des espérances de vie, avec et sans incapacité</b>,",
      "sous certaines hypothèses paramétrables par l'utilisateur.",
      "<br><br>",
      "L'application s'appuie sur le <b>package R <i>healthexpectancies</i></b>, dont elle illustre les possibilités.",
      "Ce package peut être téléchargé à l'adresse suivante : ",
      "<a href='https://github.com/patrickaubert/healthexpectancies/'> https://github.com/patrickaubert/healthexpectancies</a>.",
      "Le package contient des fonctions permettant de calculer des espérances de vie (EV), des espérances de vie sans incapacité (EVSI),",
      "et des espérances de vie en incapacité (EVI) à partir de données sur la mortalité par âge et par année",
      "et sur les prévalences des incapacités pour une année de référence,",
      "ainsi que d'hypothèses sur l'évolution des incapacités ou des espérances de vie en incapacité à l'avenir.",
      "Les calculs mettent en oeuvre la méthode de Sullivan, dont une présentation est disponible sur le site internet de l'INED :",
      "<a href='https://reves.site.ined.fr/en/resources/computation_online/sullivan/'> https://reves.site.ined.fr/en/resources/computation_online/sullivan/</a>.",
      "<br><br>",
      "Les données de base sont les <b>projections démographiques publiées par l'Insee en 2016</b> (scénario central),",
      "disponibles à l'adresse suivante : ",
      "<a href='https://www.insee.fr/fr/statistiques/2496793'>https://www.insee.fr/fr/statistiques/2496793</a>",
      ", ainsi que les données de l'<b>enquête <i>Vie quotidienne et santé</i></b> (VQS), réalisée par la DREES en 2014,",
      "et dont les premiers résultats ont été publiés dans Carrère et Brunel (2018) :",
      "<a href='https://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/publications/les-dossiers-de-la-drees/article/incapacites-et-perte-d-autonomie-des-personnes-agees-en-france-une-evolution'>",
      "https://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/publications/les-dossiers-de-la-drees/article/incapacites-et-perte-d-autonomie-des-personnes-agees-en-france-une-evolution</a>",
      "(données du graphique 2 pour les prévalences par âge). ",
      "L'application permet aussi de visualiser les parts de bénéficiaires et les espérances de durée dans ",
      "l'allocation personnalisée d'autonomie (APA), à domicile et/ou en établissement, à partir des données de ",
      "l'enquête Aide sociale de la DREES pour décembre 2017. Ces données sont tirées du graphique 1 de la fiche 15 ",
      "du Panorama annuel <i>L'aide et l'action sociales en France - édition 2019</i>, téléchargeable à l'adresse suivante : ",
      "<a href='https://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/publications/panoramas-de-la-drees/article/l-aide-et-l-action-sociales-en-france-perte-d-autonomie-handicap-protection-de'>https://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/publications/panoramas-de-la-drees/article/l-aide-et-l-action-sociales-en-france-perte-d-autonomie-handicap-protection-de</a>",
      ".<br><br>",
      "La définition des incapacités, l'hypothèse sur leur évolution,",
      "l'année de projection et l'âge auquel sont calculées les EVSI et EVI",
      "peuvent être paramétrés par l'utilisateur.",
      "Par exemple, les paramètres actuels correspondent à une projection en",
      input$anneeProj,
      ", un calcul des EVSI et EVI à l'âge de",
      input$ageEVSI,
      "ans, pour des incapacités définies comme :",
      input$limtype,
      ", et supposées évoluer à l'avenir selon l'hypothèse : ",
      recode(input$optionProj,
             "cstDFLE" = "EVSI constante",
             "cstDLE" = "EVI constante",
             "cstPctDFLE" = "part de l'EVSI dans l'EV totale constante",
             "cstPrev" = "prévalences des incapacités constantes"),
      ".<br><br>",
      "Les différentes <b>options disponibles pour l'hypothèse d'évolutions des incapacités</b> sont les suivantes :",
      "<li><i>Prévalences constantes</i> : les prévalences des incapacités par sexe à chaque âge fin restent constantes par rapport à la valeur à l'année de référence",
      "<li><i>EVSI constante</i> : l'espérance de vie sans incapacité reste constante en projection, pour chaque sexe et à chaque âge fin ; toutes les années de vie gagnées sont donc passés en incapacité",
      "<li><i>EVI constante</i> l'espérance de vie en incapacité reste constante en projection, pour chaque sexe et à chaque âge fin ; toutes les années de vie gagnées sont donc passés sans incapacité",
      "<li><i>% EVSI/EV constant</i> : la part de l'espérance de vie sans incapacité dans l'espérance de vie totale reste constante au cours du temps, à chaque âge et pour chaque sexe ; les gains d'espérance de vie se partagent donc entre vie sans et avec incapacité, au prorata de la part observée pour l'année de référence.",
      ".<br><br>",
      "Les <b>définitions des incapacités</b> mesurées dans l'enquête VQS de 2014 sont :",
      "<li>Maladie chronique",
      "<li>Au moins une LF (limitation fonctionnelle)",
      "<li>LF physique (au moins une limitation fonctionnelle de nature physique)",
      "<li>LF sensorielle (au moins une limitation fonctionnelle de nature sensorielle)",
      "<li>LF cognitive (au moins une limitation fonctionnelle de nature cognitive)",
      "<li>Très mauvais état de santé (d'après l'état de santé déclaré par le répondant à l'enquête)",
      "<li>Aide humaine",
      "<li>AT ou AL (au moins une aide technique ou un aménagement du logement)",
      "<li>Laver (difficulté à se laver seul)",
      "<li>Score VQS ≥40 (score de dépendance, construit pour les besoins de l'enquête, au-dessus du seuil de 40)",
      "<li>GALI (indicateur de limitations d'activité générale : répondre 'oui, fortement limité' à la question : êtes-vous, depuis au moins 6 mois pour un problème de santé, limité dans les activités que les gens font habituellement ?",
      "<li>APA à domicile (bénéficiaire de l'allocation personnalisée d'autonomie à domicile)",
      "<li>APA en établissement (bénéficiaire de l'allocation personnalisée d'autonomie en établissement d'hébergement)",
      "<li>APA domicile+établissement (bénéficiaire de l'allocation personnalisée d'autonomie à domicile ou en établissement d'hébergement)",
      sep=" ")
    HTML(doc)
  })

  output$mentionslegales <- renderUI({
    mentions <- paste(
      "Cette application interactive, de même que le package R <i>healthexpectancies</i> sur lequel elle s'appuie",
      "ont été développés par Patrick Aubert en octobre 2020.",
      "<br><br>",
      "Le code source est diffusé gratuitement, sous licence EUPL. Il a été développé en dehors du cadre professionnel",
      "et peut contenir des erreurs. L'utilisateur est averti que la réutilisation des résultats de cette application",
      "n'engage pas l'auteur du code.",
      sep=" ")
    HTML(mentions)
  })


}
