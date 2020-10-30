# =======================================================
# launch example app for package 'healthexpectancies'
# (the app calculates DFLEs and DLEs from French data on population and disabilities after age 60)

shinyApp(ui = ui, server = server)

# rsconnect::deployApp(appDir = "C:/Users/PA/Documents/R/Projets/healthexpectancies/inst/shiny-examples/projprevalence", server = "shinyapps.io")
