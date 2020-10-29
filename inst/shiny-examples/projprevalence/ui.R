ui <- dashboardPage(

  dashboardHeader(title = "Projection d'EVSI"#,
                  #titleWidth = 250
                  ),

  # ====================================================================
  # options de projections

  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualiser les résultats", tabName = "result", icon = icon("chart-bar")),
      menuItem("Paramétrer la projection", tabName = "param", icon = icon("cog"),
               selectInput("limtype",
                           label = "Limitation",
                           choices = unique(FRDreesVQSsurvey2014$limitationtype),
                           selected = "GALI"),
               sliderInput("anneeProj", "Année de projection :", 2015, 2050, 1, value = 2030),
               selectInput("optionProj",
                           label = "Option de projection",
                           choices = list("Prévalences constantes" = "cstPrev",
                                          #"Décalage prévalences" = "prevagetrans",
                                          "EVSI constante" = "cstDFLE",
                                          "% EVSI/EV constant" = "cstPctDFLE",
                                          "EVI constante" = "cstDLE")),
               sliderInput("ageEVSI", "EVSI calculée à l'âge suivant :", 60, 90, 1, value = 65),
               sliderInput("ageFinCalcul", "Tronquer le calcul à l'âge :", 100, 120, 1, value = 110)),
      menuItem("Documentation", tabName = "doc", icon = icon("book")),
      menuItem("Mentions légales", tabName = "info", icon = icon("th"))
    ) # fin sidebarMenu
  ), # fin dashboardSidebar

  # ====================================================================
  # affichage des projections

  dashboardBody(
    tabItems(

      # === graphiques et indicateurs
      tabItem(
        tabName = "result",
        # --- Première ligne : Boxes prévalences par âge
        fluidRow(
          box(title = "Prévalences des incapacités selon l'âge, observées et projetées (en %)",
              solidHeader = TRUE,
              width = 12,
              htmlOutput("textePreval"),
              checkboxInput("approxPrev", label="Lisser les prévalences",  value = TRUE  ),
              plotlyOutput("prevproj", height = 250)
          )
        ),

        # --- Deuxième ligne : EV, EVSI et EVI (à gauche = à tous âges, pour l'année de référence et l'année projetée ; à droite à l'âge sélectionné, pour toutes les années)
        fluidRow(
          box(title = "Espérance de vie sans (EVSI) et avec incapacité (EVI)",
              solidHeader = TRUE,
              plotlyOutput("evsi", height = 500)
          ),
          box(title = "EV et EVSI en projection, à un âge donné",
              solidHeader = TRUE,
              htmlOutput("texteAge"),
              plotlyOutput("evsiprojage", height = 210)
          ),
          box(title = "Part de l'EV passée sans incapacité, à un âge donné",
              solidHeader = TRUE,
              plotlyOutput("pctevsiprojage", height = 190)
          )
          #box(title = "EVSI et EV en projection",
          #    solidHeader = TRUE,
          #    plotlyOutput("evsiproj", height = 250)
          #)#,
          #box(title = "Part de l'EV passée sans incapacité, à tous âges",
          #    solidHeader = TRUE,
          #    plotlyOutput("pctevsiproj", height = 250)
          #)
        ),

        # --- Troisième ligne : nombre de personnes en incapacité, par année et par tranches d'âges pour l'année de projection sélectionnée
        fluidRow(
          box(title = "Nombre de personnes avec limitations (en milliers)",
              solidHeader = TRUE,
              plotlyOutput("nbIncapProj", height = 270)
          ),
          box(title = "Répartition par âge des personnes avec limitations",
              solidHeader = TRUE,
              htmlOutput("texteAn"),
              plotlyOutput("nbIncapAge", height = 250)
          )
        ),

        # --- Quatrième ligne : ratio de prévalences observées (éventuellement lissées et projetées)
        fluidRow(
          box(title = "Ratio des prévalences projetées / observées (en %)",
              solidHeader = TRUE,
              plotlyOutput("ratioprev", height = 250)
          )
        )
      ),

      # === documentation
      tabItem(
        tabName = "doc",
        htmlOutput("documentation")
      ),

      # === mentions légales
      tabItem(
        tabName = "info",
        htmlOutput("mentionslegales")
      )
    ) # fin tabItems
  ) # fin dashboardBody
)
