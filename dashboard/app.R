# app.R
library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(plotly)
library(glue)

# Source the UI and server components
source("/cloud/project/dashboard/ui.R")
source("/cloud/project/dashboard/server.R")

# Run the application
shinyApp(ui = ui, server = server)

