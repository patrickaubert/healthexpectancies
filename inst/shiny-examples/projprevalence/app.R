# =======================================================
# launch example app for package 'healthexpectancies'
# (the app calculates DFLEs and DLEs from French data on population and disabilities after age 60)

library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(healthexpectancies)

shinyApp(ui = ui, server = server)
