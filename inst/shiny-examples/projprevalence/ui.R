ui <- dashboardPage(

  dashboardHeader(title = "Projection d'EVSI et de prévalences"),

  # ====================================================================
  # options de projections

  dashboardSidebar(
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
    sliderInput("ageEVSI", "EVSI calculée à l'âge suivant :", 60, 90, 1, value = 65)
    # ajout autres options
    # ajout conditional panel = ampleur du décalage
  ),

  # ====================================================================
  # affichage des projections

  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(title = "Prévalences, observées et projetées (en %)",
          solidHeader = TRUE,
          width = 12,
          checkboxInput("approxPrev", label="Lisser les prévalences",  value = TRUE  ),
          plotlyOutput("prevproj", height = 250)
      )
    ),
    fluidRow(
      box(title = "Esp. de vie sans (EVSI) et avec incapacité (EVI)",
          solidHeader = TRUE,
          plotlyOutput("evsi", height = 500)
      ),
      box(title = "EV et EVSI en projection, à un âge donné",
          solidHeader = TRUE,
          plotlyOutput("evsiprojage", height = 210)
      ),
      box(title = "Part de l'EV passée sans incapacité, à un âge donné",
          solidHeader = TRUE,
          plotlyOutput("pctevsiprojage", height = 210)
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
    fluidRow(
      box(title = "Nombre de personnes avec limitations",
          solidHeader = TRUE
      )
    )
  )
)
