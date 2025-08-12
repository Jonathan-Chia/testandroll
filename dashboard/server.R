# server.R
library(shiny)
library(scales)
library(shinyWidgets)
library(tibble)
library(dplyr)
library(markdown)

source('priors/priors_equal_mu.R')
source('nn_functions.R')

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
  ##########
  ## SURVEY
  
  observeEvent(input$survey_button, {
    
    issues <- data.frame(
      Topic = character(),
      Your_Answer = character(),
      Message = character(),
      stringsAsFactors = FALSE
    )
    
    # Check each condition and append to issues if failed
    if (!"Q1" %in% input$surveyboxes1) {
      issues <- rbind(
        issues,
        data.frame(
          Topic = "1. Objective",
          Your_Answer = "Your main objective is to get statistical significance.",
          Message = "Stick with hypothesis tests if statistical significance is really important."
        )
      )
    }
    
    if (!"Q2" %in% input$surveyboxes1) {
      issues <- rbind(
        issues,
        data.frame(
          Topic = "1. Objective",
          Your_Answer = "You are not willing to choose the best-performing variant.",
          Message = "Test & Roll is built to maximize profit despite not having statistically significant p-values! You should always choose the best variant after testing, unless expected loss is high + the test is high stakes."
        )
      )
    }
    
    if (!"Q1" %in% input$surveyboxes2) {
      issues <- rbind(
        issues,
        data.frame(
          Topic = "2. Test Design",
          Your_Answer = "You don't know your available population.",
          Message = "Can you estimate it? If not, then you might want to consider a multi-armed bandit A/B test."
        )
      )
    }
    
    if (!"Q1" %in% input$surveyboxes3) {
      issues <- rbind(
        issues,
        data.frame(
          Topic = "3. Data",
          Your_Answer = "Your key metric is neither continuous or a percentage/rate.",
          Message = "Test & Roll requires a continuous profit or percentage/rate that is normally distributed."
        )
      )
    }
    
    if (!"Q2" %in% input$surveyboxes3) {
      issues <- rbind(
        issues,
        data.frame(
          Topic = "3. Data",
          Your_Answer = "You don't have historical data or domain knowledge to form a prior belief about variant performance.",
          Message = "Some options to gain domain knowledge: run a regular hypothesis test, consult with experts to get domain knowledge, or run a multi-armed bandit A/B test."
        )
      )
    }
    
    if (!"Q3" %in% input$surveyboxes3) {
      issues <- rbind(
        issues,
        data.frame(
          Topic = "3. Data",
          Your_Answer = "Equal Prior Profit",
          Message = "Sorry! Test & Roll works when you have different expected profit for A and B (e.g., a direct mail campaign has a baseline expected sales from control group, and then a higher expected sales for test group); however, this is a future dashboard feature. Stay tuned!"
        )
      )
    }
    
    if (!"Q4" %in% input$surveyboxes3) {
      issues <- rbind(
        issues,
        data.frame(
          Topic = "3. Data",
          Your_Answer = "You can't measure outcomes quickly after the experiment.",
          Message = "Use a long-term retention study design."
        )
      )
    }
    
    if (!"Q1" %in% input$surveyboxes4) {
      issues <- rbind(
        issues,
        data.frame(
          Topic = "4. Constraints",
          Your_Answer = "There are legal/regulatory barriers that force you to choose the variant that is statistically significant.",
          Message = "Use a hypothesis test."
        )
      )
    }
    
    # Decide what to show
    if (nrow(issues) == 0) {
      showModal(
        modalDialog(
          title = "Test & Roll Fits Your Use Case!",
          "Sending you to the Crash Course Tab.",
          easyClose = TRUE
        )
      )
      nav_select(id="main_nav", selected = "crash_course_tab")
    } else {
      showModal(
        modalDialog(
          title = "Test & Roll Does Not Fit Your Use Case.",
          renderTable(issues, striped = TRUE, hover = TRUE),
          easyClose = TRUE,
          size = "l"
        )
      )
    }
  })
  
  observeEvent(input$nav_click, {
    updateNavbarPage(session, "main_nav", selected = input$nav_click)
  })
  
  ##########
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
        xaxis = list(title = "Prior Standard Deviation of Treatment Mean Profit (σ)"),
        yaxis = list(title = "Profit-Maximizing Sample Size"),
        hovermode = "x unified"
      )
  })
  
  n_star <- eventReactive(input$calculate, {
    test_size_nn(N=input$N, s=input$s, mu=input$mu, sigma=input$sigma)
  })
  
  profit_rdf <- eventReactive(input$calculate, {
    n <- generate_sequence(input$N/2)
    out <- NULL
    for (i in 1:length(n)) {
      out <- rbind(out, test_eval_nn(n=c(n[i], n[i]), N=input$N, s=input$s, mu=input$mu, sigma=input$sigma))
    }
    out
  })
  
  best_profit_rdf <- eventReactive(input$calculate, {
    best_out <- test_eval_nn(n=n_star(), N=input$N, s=input$s, mu=input$mu, sigma=input$sigma)
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
      add_segments(x = n_star(), xend = n_star(),
                   y = min(profit_rdf()$profit), yend = max(profit_rdf()$profit),
                   line = list(color = 'orange', dash = 'dot'), name = 'Optimal n*',
                   hoverinfo = 'none') %>%
      
      # Add annotation for n_star
      add_annotations(x = n_star(), y = best_profit_rdf()$profit + (profit_rdf()$profit_perfect[1]-profit_rdf()$profit_rand[1])/100,
                      text = paste0("n*=", format(round(n_star()), big.mark=",")),
                      xanchor = 'left', showarrow = FALSE) %>%
      
      # Layout adjustments
      layout(
        title = "Expected Profit vs. Test Size",
        xaxis = list(title = "Test Size (n<sub>1</sub>=n<sub>2</sub>)"),
        yaxis = list(title = "Expected Total Profit",
                     tickprefix = "$",
                     tickformat = ",",
                     range = c(profit_rdf()$profit_rand[1], profit_rdf()$profit_perfect[1])),
        hovermode = "x unified"
      )
  })
  
  ###################
  ## RESULTS TAB
  ###################
  example_test_results_rdf <- reactive({
    set.seed(19348)
    n_star <- round(test_size_nn(N=input$N, s=input$s, mu=input$mu, sigma=input$sigma)[1])
    group <- c(rep("A", n_star), rep("B", n_star)) 
    profit <- c(rnorm(n_star, mean=input$mu + (input$sigma / 50), sd=input$sigma), 
                rnorm(n_star, mean=input$mu, sd=input$sigma*1.2)) # performed worst but has more variation
    data.frame(group, profit) %>% arrange(profit)
  })
  
  output$example_results <- renderDT({
    datatable(example_test_results_rdf(),options = list(
      scrollY = '200px',
      pageLength = 5,
      dom = 'lt'  # show "Show X entries" dropdown + the table only
    ))
  })
  
  user_test_results_rdf <- eventReactive(
    list(input$test_results_file, input$use_example),
    {
      if (input$use_example > 0) {
        example_test_results_rdf()
      } else {
        req(input$test_results_file)
        read.csv(input$test_results_file$datapath)
      }
    }
  )
  
  
  observeEvent(input$test_results_file, {
    # Only run if file is uploaded
    req(input$test_results_file)
    # Check column names - adjust expected columns as needed
    expected_cols <- colnames(example_test_results_rdf())
    uploaded_cols <- colnames(user_test_results_rdf())
    
    if (!identical(sort(uploaded_cols), sort(expected_cols))) {
      showNotification("Uploaded Test Results do not have the same columns as Example Data!", type = "error", duration = 5)
      return()
    }
    
    showNotification("Files are valid and ready!", type = "message", duration = 3)
  })
  
  # Render uploaded table
  output$user_results <- renderDT({
    req(user_test_results_rdf())
    datatable(user_test_results_rdf(), options = list(
      scrollY = '200px',
      pageLength = 5,
      dom = 'lt'  # show "Show X entries" dropdown + the table only
    ))
  })
  
  posterior_results_rdf <- eventReactive(input$examine_results, {
    req(user_test_results_rdf())
    n_A <- sum(user_test_results_rdf()$group == "A")
    n_B <- sum(user_test_results_rdf()$group == "B")
    s <- sd(user_test_results_rdf()$profit)
    post_mean_A <- mean(user_test_results_rdf()[user_test_results_rdf()$group == "A", "profit"])
    post_mean_B <- mean(user_test_results_rdf()[user_test_results_rdf()$group == "B", "profit"])
    post_sd_A <- (1/100^2 + n_A/s^2)^-(1/2)
    post_sd_B <- (1/100^2 + n_B/s^2)^-(1/2)
    
    # Calculate dynamic x-range (mean ± 4 SDs)
    x_min <- min(post_mean_A - 5*post_sd_A, post_mean_B - 5*post_sd_B)  
    x_max <- max(post_mean_A + 5*post_sd_A, post_mean_B + 5*post_sd_B)
    x_vals <- seq(x_min, x_max, length.out = 500)
    
    # Create data frame for plotting
    data.frame(
      x = x_vals,
      posterior_A = dnorm(x_vals, mean = post_mean_A, sd = post_sd_A),
      posterior_B = dnorm(x_vals, mean = post_mean_B, sd = post_sd_B)
    )
  })
  
  posterior_diff_results_list <- eventReactive(input$examine_results,{
    req(user_test_results_rdf())
    n_A <- sum(user_test_results_rdf()$group == "A")
    n_B <- sum(user_test_results_rdf()$group == "B")
    s <- sd(user_test_results_rdf()$profit)
    post_mean_A <- mean(user_test_results_rdf()[user_test_results_rdf()$group == "A", "profit"])
    post_mean_B <- mean(user_test_results_rdf()[user_test_results_rdf()$group == "B", "profit"])
    post_sd_A <- (1/100^2 + n_A/s^2)^-(1/2)
    post_sd_B <- (1/100^2 + n_B/s^2)^-(1/2)
    
    diff_mean <- post_mean_A - post_mean_B
    diff_sd <- sqrt(post_sd_A^2 + post_sd_B^2)
    diff_x <- seq(diff_mean - 5*diff_sd, diff_mean + 5*diff_sd, length.out = 500)
    prob_A_greater <- 1 - pnorm(0, mean = diff_mean, sd = diff_sd)
    diff_data = data.frame(
      x = diff_x,
      density = dnorm(diff_x, mean = diff_mean, sd = diff_sd)
    )
    # Create the highlighted region data (right of zero)
    highlight_x <- diff_data$x[diff_data$x >= 0]
    highlight_y <- diff_data$density[diff_data$x >= 0]
    highlight_data <- data.frame(x = c(0, highlight_x, max(highlight_x)),
                                 y = c(0, highlight_y, 0))
    set.seed(123)
    n_samples <- 10000
    y_A <- rnorm(n_samples, mean = post_mean_A, sd = post_sd_A)
    y_B <- rnorm(n_samples, mean = post_mean_B, sd = post_sd_B)
    # 0. Absolute Difference in Means ----
    abs_diff <- y_A - y_B
    expected_diff <- mean(abs_diff)
    diff_hdi <- quantile(abs_diff, probs = c(0.025, 0.975))  # 95% HDI
    
    # 1. Expected Uplift (A relative to B) ----
    uplift <- (y_A - y_B) / y_B  # Relative uplift when A > B
    expected_uplift <- mean(uplift)
    uplift_hdi <- quantile(uplift, probs = c(0.025, 0.975))  # 95% HDI (can do this because it's normal)
    
    # 2. Expected Loss (risk of choosing A when B might be better) ----
    loss <- ifelse(y_B > y_A, (y_B - y_A) / y_A, 0)  # Relative loss when B > A
    expected_loss <- mean(loss)
    loss_hdi <- quantile(loss[loss > 0], probs = c(0.025, 0.975))  # 95% HDI (only positive losses)
    
    # Format results as a tidy data frame ----
    results <- tibble(
      Metric = c("Absolute Difference (A - B)",
                 "Expected Uplift (A vs B)", 
                 "Expected Loss (choosing A)", 
                 "Probability A > B"),
      Value = c(expected_diff, expected_uplift, expected_loss, prob_A_greater),
      HDI_lower = c(diff_hdi[1], uplift_hdi[1], loss_hdi[1], NA),
      HDI_upper = c(diff_hdi[2], uplift_hdi[2], loss_hdi[2], NA)
    ) %>%  mutate(
      Value = case_when(
        Metric == "Probability A > B" ~ scales::percent(Value, accuracy = 0.1),
        Metric == "Absolute Difference (A - B)" ~ scales::comma(Value, accuracy = 0.0001),
        TRUE ~ scales::percent(Value, accuracy = 0.1)
      ),
      HDI = ifelse(
        is.na(HDI_lower), 
        NA_character_,
        ifelse(
          Metric == "Absolute Difference (A - B)",
          paste0("[", scales::comma(HDI_lower, accuracy = 0.0001), 
                 ", ", scales::comma(HDI_upper, accuracy = 0.0001), "]"),
          paste0("[", scales::percent(HDI_lower, accuracy = 0.0001), 
                 ", ", scales::percent(HDI_upper, accuracy = 0.0001), "]")
        )
      )
    ) %>%
      select(Metric, Value, HDI)
    list(diff_data, prob_A_greater, highlight_data, results)
  })
  
  output$results_posteriors <- renderPlotly({
    req(posterior_results_rdf())
    plot_ly(posterior_results_rdf(), x = ~x) %>%
      add_lines(y = ~posterior_A, name = "A Posterior", line = list(color = "blue")) %>%
      add_lines(y = ~posterior_B, name = "B Posterior", line = list(color = "orange")) %>%
      layout(
        title = "Posterior Distributions",
        xaxis = list(title = "Mean profit"),
        yaxis = list(title = "Posterior density"),
        legend = list(orientation = "h", y = 1.1),
        hovermode = "x unified"
      )
  })
  
  output$results_posteriors_diffs <- renderPlotly({  
    req(posterior_diff_results_list())
    posterior_diff_results <- posterior_diff_results_list()[[1]]
    prob_A_greater <- posterior_diff_results_list()[[2]]
    posterior_highlight_results <- posterior_diff_results_list()[[3]]
    plot_ly() %>%
      add_lines(data = posterior_diff_results, x = ~x, y = ~density, 
                name = "A - B", line = list(color = "purple")) %>%
      add_polygons(data = posterior_highlight_results, x = ~x, y = ~y,
                   fillcolor = "rgba(128, 0, 128, 0.3)",
                   line = list(color = "rgba(128, 0, 128, 0)"),
                   name = paste0("P(A > B) = ", round(prob_A_greater, 3))) %>%
      add_segments(x = 0, xend = 0, y = 0, yend = max(posterior_diff_results$density),
                   line = list(color = "black", dash = "dot"),
                   name = "Zero") %>%
      layout(
        title = list(text = paste0("Difference Between Groups (A - B)<br>",
                                   "<sup>P(A > B) = ", round(prob_A_greater*100, 2), "%</sup>")),
        xaxis = list(title = "Difference in mean profit"),
        yaxis = list(title = "Density"),
        showlegend = TRUE
      )
  })
  
  output$results_table <- renderDT({
    req(posterior_diff_results_list())
    results_df <- posterior_diff_results_list()[[4]]
    datatable(results_df, options=list(dom='t'))
  })

  priors_equal_server('priors_equal', example_data)
}