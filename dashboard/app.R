# app.R
library(shiny)
library(bslib)

# Source the UI and server components
source("~/dashboard/ui.R")
source("~/dashboard/server.R")

# Run the application
shinyApp(ui = ui, server = server)