# app.R
library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(plotly)
library(glue)

# Source the UI and server components
source("dashboard/ui.R")
source("dashboard/server.R")

# Run the application
shinyApp(ui = ui, server = server)

