# server.R
library(shiny)

server <- function(input, output, session) {
  
  # Reactive expressions
  data <- reactive({
    rnorm(input$n)
  })
  
  output$test <- renderText(expr = print(getwd()))
  
  # Outputs
  output$plot <- renderPlot({
    hist(data(), main = "Histogram of Random Data")
  })
  observeEvent(input$nav_click, {
    updateNavbarPage(session, "main_nav", selected = input$nav_click)
  })
}