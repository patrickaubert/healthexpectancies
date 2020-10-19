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
                               "Décalage prévalences" = "prevagetrans"))
    # ajout autres options
    # ajout conditional panel = ampleur du décalage
  ),

  # ====================================================================
  # affichage des projections

  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(title = "Prévalences, observées et projetées",
          solidHeader = TRUE,
          plotOutput("prevproj", height = 250)
      )
    ),
    fluidRow(
      box(title = "EVSI à chaque âge",
          solidHeader = TRUE,
          plotOutput("evsi", height = 250)
      )
    ),
    fluidRow(
      box(title = "Nombre de personnes avec limitations",
          solidHeader = TRUE,
      )
    )
  )
)
