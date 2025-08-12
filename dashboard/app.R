# app.R
library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(plotly)
library(glue)
library(tibble)
library(dplyr)
library(markdown)

# Source the UI and server components
source("dashboard/ui.R")
source("dashboard/server.R")
source('nn_functions.R')

# Run the application
shinyApp(ui = ui, server = server)

