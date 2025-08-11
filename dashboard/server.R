# server.R
library(shiny)

source('/cloud/project/dashboard/priors/priors_equal_mu.R')

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
  
  observe({
    mu <- input$mu
    
    s <- mu*(1-mu)
    updateNumericInput(session, "s", value = s)
  })
  
  output$N_parameter_plot <- renderPlotly({
    req(input$N, input$s, input$mu, input$sigma)
    step_size <- input$N / 10
    Ns <- seq(step_size, input$N, step_size)
    test_sizes <- rep(NA, length(Ns))
    for (i in 1:length(Ns)) 
      test_sizes[i] <- test_size_nn(N=Ns[i], s=input$s, mu=input$mu, sigma=input$sigma)[1]
    
    plot_ly() %>%
      add_trace(
        x = Ns, 
        y = test_sizes, 
        type = 'scatter', 
        mode = 'lines',
        line = list(color = 'orange', width = 2),
        name = 'Test Size'
      ) %>%
      layout(
        title = "Bigger N → Bigger Test",
        xaxis = list(title = "Available Population (N)"),
        yaxis = list(title = "Profit-Maximizing Sample Size"),
        hovermode = "x unified"
      )
  })

  priors_equal_server('priors_equal', example_data)
}