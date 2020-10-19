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
                choices = list("Prévalences constantes" = "prevcst",
                               #"Décalage prévalences" = "prevagetrans",
                               "EVSI constante" = "evsicst",
                               "% EVSI/EV constant" = "pctevsicst",
                               "EVI constante" = "evicst")),
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
          plotlyOutput("prevproj", height = 250)
      )
    ),
    fluidRow(
      box(title = "Esp. de vie sans (EVSI) et avec incapacité (EVI), en 2014",
          solidHeader = TRUE,
          plotlyOutput("evsi", height = 250)
      ),
      box(title = "EVSI et EV en projection",
          solidHeader = TRUE,
          plotlyOutput("evsiproj", height = 250)
      )#,
      #box(title = "Part de l'EV passée sans incapacité, à tous âges",
      #    solidHeader = TRUE,
      #    plotlyOutput("pctevsiproj", height = 250)
      #)
    ),
    fluidRow(
      box(title = "EV et EVSI en projection, à un âge donné",
          solidHeader = TRUE,
          plotlyOutput("evsiprojage", height = 250)
          ),
      box(title = "Part de l'EV passée sans incapacité, à un âge donné",
          solidHeader = TRUE,
          plotlyOutput("pctevsiprojage", height = 250)
          )
    ),
    fluidRow(
      box(title = "Nombre de personnes avec limitations",
          solidHeader = TRUE
      )
    )
  )
)
