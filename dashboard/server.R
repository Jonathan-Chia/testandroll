# server.R
library(shiny)

source('/cloud/project/dashboard/priors/priors_equal_mu.R')

generate_sequence <- function(max_num) {
  sequence <- 1:19  # Always include 1-19
  
  multiplier <- 10   # Start with 10x
  
  while (multiplier <= max_num) {
    # Generate 2:19 × multiplier, but don't exceed max_num
    upper_limit <- min(19, floor(max_num / multiplier))
    if (upper_limit >= 2) {
      sequence <- c(sequence, (2:upper_limit) * multiplier)
    }
    multiplier <- multiplier * 10  # Move to next order of magnitude
  }
  
  # Ensure uniqueness and sorting
  sequence <- sort(unique(sequence))
  sequence <- sequence[sequence <= max_num]
  
  return(sequence)
}

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
  
  output$mu_parameter_plot <- renderPlotly({
    req(input$N, input$s, input$mu, input$sigma)
    step_size <- input$mu / 10
    mus <- seq(step_size, input$mu, step_size)
    test_sizes <- rep(NA, length(mus))
    for (i in 1:length(mus)) 
      test_sizes[i] <- test_size_nn(N=input$N, s=input$s, mu=mus[i], sigma=input$sigma)[1]
    
    plot_ly() %>%
      add_trace(
        x = mus, 
        y = test_sizes, 
        type = 'scatter', 
        mode = 'lines',
        line = list(color = 'orange', width = 2),
        name = 'Test Size'
      ) %>%
      layout(
        title = "μ Doesn't Affect the Test Size (When Same for both A and B)",
        xaxis = list(title = "Expected Average Profit per Customer (μ)"),
        yaxis = list(title = "Profit-Maximizing Sample Size"),
        hovermode = "x unified"
      )
  })
  
  output$s_parameter_plot <- renderPlotly({
    req(input$N, input$s, input$mu, input$sigma)
    step_size <- input$s / 10
    ss <- seq(step_size, input$s, step_size)
    test_sizes <- rep(NA, length(ss))
    for (i in 1:length(ss)) 
      test_sizes[i] <- test_size_nn(N=input$N, s=ss[i], mu=input$mu, sigma=input$sigma)[1]
    
    plot_ly() %>%
      add_trace(
        x = ss, 
        y = test_sizes, 
        type = 'scatter', 
        mode = 'lines',
        line = list(color = 'orange', width = 2),
        name = 'Test Size'
      ) %>%
      layout(
        title = "Bigger s → More Noise → Bigger Test",
        xaxis = list(title = "Noise in Profit per Customer (s)"),
        yaxis = list(title = "Profit-Maximizing Sample Size"),
        hovermode = "x unified"
      )
  })
  
  output$sigma_parameter_plot <- renderPlotly({
    req(input$N, input$s, input$mu, input$sigma)
    step_size <- input$sigma / 10
    sigmas <- seq(step_size, input$sigma, step_size)
    test_sizes <- rep(NA, length(sigmas))
    for (i in 1:length(sigmas)) 
      test_sizes[i] <- test_size_nn(N=input$N, s=input$s, mu=input$mu, sigma=sigmas[i])[1]
    
    plot_ly() %>%
      add_trace(
        x = sigmas, 
        y = test_sizes, 
        type = 'scatter', 
        mode = 'lines',
        line = list(color = 'orange', width = 2),
        name = 'Test Size'
      ) %>%
      layout(
        title = "Bigger σ → Bigger Difference between A and B → Smaller Test",
        xaxis = list(title = "Prior SD of Treatment Mean Profit (sigma)"),
        yaxis = list(title = "Profit-Maximizing Sample Size"),
        hovermode = "x unified"
      )
  })
  
  profit_rdf <- eventReactive(input$calculate, {
    n <- generate_sequence(input$N/2)
    out <- NULL
    n_star <- test_size_nn(N=input$N, s=input$s, mu=input$mu, sigma=input$sigma)
    for (i in 1:length(n)) {
      out <- rbind(out, test_eval_nn(n=c(n[i], n[i]), N=input$N, s=input$s, mu=input$mu, sigma=input$sigma))
    }
    out
  })
  
  best_profit_rdf <- eventReactive(input$calculate, {
    n_star <- test_size_nn(N=input$N, s=input$s, mu=input$mu, sigma=input$sigma)
    best_out <- test_eval_nn(n=n_star, N=input$N, s=input$s, mu=input$mu, sigma=input$sigma)
  })
  
  output$optimal_sample_size_plot <- renderPlotly({
    req(profit_rdf(), best_profit_rdf())
    # Create the base plot
    plot_ly(data = profit_rdf()) %>%
      # Main line with dynamic tooltips
      add_trace(
        x = ~n1, 
        y = ~profit, 
        type = 'scatter', 
        mode = 'lines',
        line = list(color = 'black'), 
        name = 'Expected Profit',
        hoverinfo = "text",
        text = ~paste0("Test Size: ", format(n1, nsmall=0, big.mark = ","),
                      "<br>Expected Profit: $", format(round(profit, 2), nsmall = 2, big.mark = ","))
      ) %>%
      # Horizontal lines with static tooltips
      add_segments(
        x = min(profit_rdf()$n1), 
        xend = max(profit_rdf()$n1),
        y = profit_rdf()$profit_rand[1], 
        yend = profit_rdf()$profit_rand[1],
        line = list(color = 'gray', dash = 'dash'), 
        name = 'Baseline Profit (Random Choice)',
        hoverinfo = "text",
        text = paste0(
          "Baseline Profit: $", 
          format(round(profit_rdf()$profit_rand[1], 2), nsmall = 2, big.mark = ","))
      ) %>%
      add_segments(
        x = min(profit_rdf()$n1), 
        xend = max(profit_rdf()$n1),
        y = profit_rdf()$profit_perfect[1], 
        yend = profit_rdf()$profit_perfect[1],
        line = list(color = 'blue', dash = 'dash'), 
        name = 'Perfect Profit (Theoretical Best)',
        hoverinfo = "text",
        text = paste0(
          "Perfect Profit: $", 
          format(round(profit_rdf()$profit_perfect[1], 2), nsmall = 2, big.mark = ","))
      ) %>%

      # Add vertical line for optimal n_star
      add_segments(x = n_star, xend = n_star,
                   y = min(profit_rdf()$profit), yend = max(profit_rdf()$profit),
                   line = list(color = 'orange', dash = 'dot'), name = 'Optimal n*',
                   hoverinfo = 'none') %>%
      
      # Add annotation for n_star
      add_annotations(x = n_star, y = best_profit_rdf()$profit + (profit_rdf()$profit_perfect[1]-profit_rdf()$profit_rand[1])/100,
                      text = paste0("n*=", format(round(n_star), big.mark=",")),
                      xanchor = 'left', showarrow = FALSE) %>%
      
      # Layout adjustments
      layout(
        title = "Expected Profit vs. Test Size",
        xaxis = list(title = "Test Size (n<sub>1</sub>=n<sub>2</sub>)"),
        yaxis = list(title = "Expected Profit",
                     tickprefix = "$",
                     tickformat = ",",
                     range = c(profit_rdf()$profit_rand[1], profit_rdf()$profit_perfect[1])),
        hovermode = "x unified"
      )
  })
  

  priors_equal_server('priors_equal', example_data)
}