#m1=m2
library(shiny)
library(bslib)
library(DT)
library(plotly)
library(shinyWidgets)
library(glue)

source('nn_functions.R')

generate_syn_expt <- function(nexpt, nobs, mu, sigma, omega) {
  nobs <- matrix(nobs, nrow=nexpt, ncol=2) # equal test sizes
  y <- matrix(NA, nrow=nexpt, ncol=2)
  for (e in 1:nexpt) {
    t <- rnorm(1, mean=mu, sd=omega)
    m <- rnorm(2, mean=t, sd=sigma)
    while (min(t,m) < 0 | max(t,m) > 1) { # reject values outside [0,1] 
      t <- rnorm(1, mean=mu, sd=omega)
      m <- rnorm(2, mean=t, sd=sigma)
    }
    y[e, 1] <- mean(rnorm(nobs[e,1], mean=m[1], sd=sqrt(m[1]*(1-m[1]))))
    y[e, 2] <- mean(rnorm(nobs[e,2], mean=m[2], sd=sqrt(m[2]*(1-m[2]))))
  }
  colnames(y) <- c("A_profit", "B_profit")
  colnames(nobs) <- c("A_observations", "B_observations")
  list(nexpt=nexpt, y=y, nobs=nobs)
}
set.seed(19980103)
example_data <- generate_syn_expt(nexpt=100, nobs=100000, 
                           mu=0.676, sigma=0.030, omega=0.199)
#write.csv(example_data$y, '/cloud/project/dashboard/priors/user_y.csv', row.names=FALSE)
#write.csv(example_data$nobs, '/cloud/project/dashboard/priors/user_nobs.csv', row.names=FALSE)

priors_equal_ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    card(
      card_header(
        div(style = "display: flex; justify-content: space-between; align-items: center;",
            span("Upload Your Data with Same Format as Example", style = "font-weight: bold; font-size: 1.25rem;"),
            fileInput(ns("prior_experiments_file"), NULL, accept = ".csv", buttonLabel = "Upload Experiments CSV"),
            fileInput(ns("prior_observations_file"), NULL, accept = ".csv", buttonLabel = "Upload Observations CSV")
        )
      ),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          
          # Left: Example data
          card(
            card_header("Example Data"),
            uiOutput(ns('example_nexpt')),
            DTOutput(ns("example_y")),
            DTOutput(ns("example_nobs")),
            actionButton(ns('use_example'), label='Use Example Data') |>
              popover(
                "Warning:",
                "This example data is derived from analyzing 2,101 website tests from Berman et al. (2018) which
were conducted across a wide variety of websites. Thus, this data can only provide a generalizable prior for website click-through analyses.",
                placement = "top",
                options = list(trigger = 'focus')
              )
          ),
          
          # Right: Placeholder for uploaded data
          card(
            card_header("Your Data"),
            uiOutput(ns('user_nexpt')),
            DTOutput(ns("user_y")),
            DTOutput(ns("user_nobs")),
            downloadButton(ns('output_data'), label='Prep User Data for Stan')
          )
        )
    )),
    markdown("## 2. Choose Priors"),
    withMathJax(helpText("Before we use the data to calculate \\(s\\) and \\(\\sigma\\), we need to set some priors for likely ranges for these parameters.")),    
    card(
        card_header(
              withMathJax(helpText("\\(\\sigma\\) Prior")),
              popover(
                bsicons::bs_icon("question-circle", class='ms-auto'),
                title = "Example",
                withMathJax(helpText("\\(\\sigma\\) is the variation between test and control within each experiment. We assign a normal distribution as the prior for σ:")),
                withMathJax("$$\\sigma \\sim \\mathcal{N}(0, 0.1)$$"),
                withMathJax(helpText("and we truncate to positive values since \\(\\sigma > 0\\).")),
                options = list(trigger = 'focus')
              ), class = "d-flex align-items-center gap-1"),
        numericInput(ns('sigma_prior_mean_input'), label = withMathJax(helpText("\\(\\sigma\\) mean")), value = 0) |> 
          popover("This should generally always be 0.", placement='right',options = list(trigger = 'focus')),
        numericInput(ns('sigma_prior_sd_input'), label = withMathJax(helpText("\\(\\sigma\\) sd")), value = 0.1) |> 
          popover("You should keep this smaller to be more conservative.", placement='right',options = list(trigger = 'focus')),
        plotlyOutput(ns('sigma_prior'))
      ),
    card(
      card_header(
        withMathJax(helpText("\\(\\mu\\) Prior (\\(\\mu\\) is used to calculate \\(s\\))")),
        popover(
          bsicons::bs_icon("question-circle", class='ms-auto'),
          title = "Example",
          withMathJax(helpText("\\(\\mu\\) is the average profit across previous experiments (profit just has to be a continuous, normally distributed outcome). For the example data, we are using average click rate from historical tests. We assign a normal distribution as the prior for μ:")),
          withMathJax("$$\\mu \\sim \\mathcal{N}(0.5, 0.1)$$"),
          withMathJax(helpText("This is a moderately informative prior that assumes most websites have clickthrough rates roughly between 30-70% (within 2 SDs).")),
          options = list(trigger = 'focus')
        ), class = "d-flex align-items-center gap-1"),
      numericInput(ns('mu_prior_mean_input'), label = withMathJax(helpText("\\(\\sigma\\) mean")), value = 0.5) |> 
        popover("This should never be too close to 0 or 1.", placement='right',options = list(trigger = 'focus')),
      numericInput(ns('mu_prior_sd_input'), label = withMathJax(helpText("\\(\\sigma\\) sd")), value = 0.1) |> 
        popover("Make sure your SD doesn't cause your mean to potentially reach higher than 1 or lower than 0.", placement='right',options = list(trigger = 'focus')),
      plotlyOutput(ns('mu_prior'))
    ),
      card(
        card_header(
          switchInput(ns('omega_switch'), label = HTML("<strong>&omega; Prior</strong>"), value=TRUE, onLabel = 'YES', offLabel = 'NO'),
          popover(
            bsicons::bs_icon("question-circle", class='ms-auto'),
            title = "Example",
            withMathJax(helpText("\\(\\omega\\) captures the variation in mean profit across experiments. Use this only if you believe each of your experiments have a different mean profit. The example data is based off different experiments from different websites, so we need ω.")),  
            markdown("We assign a normal distribution as the prior for ω:"),
            withMathJax("$$\\omega \\sim \\mathcal{N}(0, 0.1)$$"),
            options = list(trigger = 'focus')
          ), class = "d-flex align-items-center gap-1"),
        uiOutput(ns("omega_content")),  # Conditionally rendered
        plotlyOutput(ns('omega_prior'))
        ),
    card(
      card_header(
        downloadButton(ns('output_model'), label='Output Stan Meta Analysis Model'),
        popover(
          bsicons::bs_icon("question-circle", class='ms-auto'),
          title = "Info",
          markdown('Press the download button to take all the priors you set, and output the right stan model architecture.'),
          markdown('From here, you will take the prepped stan data and run it through the model on your local machine.'),
          withMathJax(helpText('Note: the stan model architecture will adjust based on whether you include \\(\\omega\\) or not.')),
          options = list(trigger = 'focus')
        ), class = "d-flex align-items-center gap-1")
    )
    )
}


priors_equal_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Render example table
    output$example_y <- renderDT({
      datatable(data$y,options = list(
        scrollY = '200px',
        pageLength = 5,
        dom = 'lt'  # show "Show X entries" dropdown + the table only
      ))
    })
    
    output$example_nobs <- renderDT({
      datatable(data$nobs,
                options = list(
                  scrollY = '200px',
                  pageLength = 5,
                  dom = 'lt'  # show "Show X entries" dropdown + the table only
                ))
    })
    user_y_rdf <- eventReactive(
      list(input$prior_experiments_file, input$use_example),
      {
        if (input$use_example > 0) {
          data$y
        } else {
          req(input$prior_experiments_file)
          read.csv(input$prior_experiments_file$datapath)
        }
      }
    )
    
    user_nobs_rdf <- eventReactive(
      list(input$prior_observations_file, input$use_example),
      {
        if (input$use_example > 0) {
          data$nobs
        } else {
          req(input$prior_observations_file)
          read.csv(input$prior_observations_file$datapath)
        }
      }
    )
    
    
    observeEvent(list(input$prior_experiments_file, input$prior_observations_file), {
      # Only run if both files are uploaded
      req(input$prior_experiments_file, input$prior_observations_file)
      # Check lengths
      if (nrow(user_y_rdf()) != nrow(user_nobs_rdf())) {
        showNotification("Files must have the same number of rows!", type = "error", duration = 5)
        return()
      }
      
      # Check column names - adjust expected columns as needed
      expected_cols <- colnames(data$y)
      uploaded_cols <- colnames(user_y_rdf())
      
      if (!identical(sort(uploaded_cols), sort(expected_cols))) {
        showNotification("File 1 does not have the required columns!", type = "error", duration = 5)
        return()
      }
      
      expected_cols <- colnames(data$nobs)
      uploaded_cols <- colnames(user_nobs_rdf())
      
      if (!identical(sort(uploaded_cols), sort(expected_cols))) {
        showNotification("File 2 must have exactly these columns: ", paste(expected_cols, collapse = ", "),
                         type = "error", duration = 5)
        return()
      }
      
      
      showNotification("Files are valid and ready!", type = "message", duration = 3)
      output$user_nexpt <- renderUI({
        value_box(value=nrow(user_y_rdf()), title='# of Experiments')
      })
    })
    
    output$example_nexpt <- renderUI({
      value_box(value=data$nexpt, title='# of Experiments')
    })

    
    observeEvent(input$use_example, {
      output$user_nexpt <- renderUI({
        value_box(value=nrow(user_y_rdf()), title='# of Experiments')
      })
    })
    
    # Render uploaded table
    output$user_y <- renderDT({
      req(user_y_rdf())
      datatable(user_y_rdf(), options = list(
        scrollY = '200px',
        pageLength = 5,
        dom = 'lt'  # show "Show X entries" dropdown + the table only
      ))
    })
    
    output$user_nobs <- renderDT({
      req(user_nobs_rdf())
      datatable(user_nobs_rdf(), options = list(
        scrollY = '200px',
        pageLength = 5,
        dom = 'lt'  # show "Show X entries" dropdown + the table only
      ))
    })
    
    output$sigma_prior <- renderPlotly({  # Changed from renderPlot to renderPlotly
      # Define the Normal distribution for sigma
      x <- seq(-0.5, 0.5, length.out = 1000)
      y <- dnorm(x, mean = input$sigma_prior_mean_input, sd = input$sigma_prior_sd_input)
      
      # Create a data frame
      df <- data.frame(x = x, y = y) %>% 
        mutate(truncated = ifelse(x >= 0, "Valid (σ > 0)", "Invalid (σ ≤ 0)"))
      
      # Create the plot
      plot_ly() %>%
        # Add the full normal curve
        add_trace(
          data = df,
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "lines",
          line = list(color = "gray", dash = "dot"),
          name = paste0("Normal(", input$sigma_prior_mean_input, ", ", input$sigma_prior_sd_input, ")"),
          hoverinfo = "x+y"
        ) %>%
        # Add the truncated portion (σ > 0)
        add_trace(
          data = filter(df, x >= 0),
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "lines",
          fill = "tozeroy",
          fillcolor = "rgba(78, 121, 167, 0.3)",
          line = list(color = "rgb(78, 121, 167)"),
          name = "Truncated Normal (σ > 0)",
          hoverinfo = "x+y"
        ) %>%
        # Add a vertical line at x=0
        add_segments(
          x = 0, xend = 0,
          y = 0, yend = max(y)*1.05,
          line = list(color = "red", dash = "dash"),
          name = "Truncation Boundary"
        ) %>%
        # Layout customization
        layout(
          title = paste0("<b>Normal(", input$sigma_prior_mean_input, ", ", input$sigma_prior_sd_input, 
                         ") with σ > 0 Truncation</b>"),
          xaxis = list(title = "σ (Standard Deviation μ between Test & Control)"),
          yaxis = list(title = "Density"),
          hovermode = "x unified",
          annotations = list(
            x = -0.15, y = max(y)*0.9,
            text = "Excluded by <lower=0>",
            showarrow = FALSE,
            font = list(color = "red")
          )
        )
    })
    
    output$mu_prior <- renderPlotly({  # Changed from renderPlot to renderPlotly
      # Define the Normal distribution for sigma
      x <- seq(0, 1, length.out = 1000)
      y <- dnorm(x, mean = input$mu_prior_mean_input, sd = input$mu_prior_sd_input)
      
      # Create a data frame
      df <- data.frame(x = x, y = y)
      
      # Create the plot
      plot_ly() %>%
        # Add the full normal curve
        add_trace(
          data = df,
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "lines",
          fill = "tozeroy",
          fillcolor = "rgba(78, 121, 167, 0.3)",
          line = list(color = "rgb(78, 121, 167)"),
          name = "Normal",
          hoverinfo = "x+y"
        ) %>%
        # Layout customization
        layout(
          title = paste0("<b>Normal(", input$mu_prior_mean_input, ", ", input$mu_prior_sd_input, 
                         ")"),
          xaxis = list(title = "μ (Mean Profit per Customer)"),
          yaxis = list(title = "Density"),
          hovermode = "x unified"
        )
    })
    
    output$omega_content <- renderUI({
      if (input$omega_switch) {
        tagList(
          numericInput(ns('omega_prior_mean_input'), label = withMathJax(helpText("\\(\\omega\\) mean")), value = 0) |> 
            popover("This should never be too close to 0 or 1, but add truncating if you do need it to be.", placement='right',options = list(trigger = 'focus')),
          numericInput(ns('omega_prior_sd_input'), label = withMathJax(helpText("\\(\\omega\\) sd")), value = 0.1) |> 
            popover("Generally you will want your SD to be around 0.1.", placement='right',options = list(trigger = 'focus'))
        )
      }
    })
    
    output$omega_prior <- renderPlotly({
      req(input$omega_switch, input$omega_prior_mean_input, input$omega_prior_sd_input)
      x <- seq(-0.5, 0.5, length.out = 1000)
      y <- dnorm(x, mean = input$omega_prior_mean_input, sd = input$omega_prior_sd_input)
      
      # Create a data frame
      df <- data.frame(x = x, y = y) %>% 
        mutate(truncated = ifelse(x >= 0, "Valid (σ > 0)", "Invalid (σ ≤ 0)"))
      
      # Create the plot
      plot_ly() %>%
        # Add the full normal curve
        add_trace(
          data = df,
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "lines",
          line = list(color = "gray", dash = "dot"),
          name = paste0("Normal(", input$omega_prior_mean_input, ", ", input$omega_prior_sd_input, ")"),
          hoverinfo = "x+y"
        ) %>%
        # Add the truncated portion (σ > 0)
        add_trace(
          data = filter(df, x >= 0),
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "lines",
          fill = "tozeroy",
          fillcolor = "rgba(78, 121, 167, 0.3)",
          line = list(color = "rgb(78, 121, 167)"),
          name = "Truncated Normal (σ > 0)",
          hoverinfo = "x+y"
        ) %>%
        # Add a vertical line at x=0
        add_segments(
          x = 0, xend = 0,
          y = 0, yend = max(y)*1.05,
          line = list(color = "red", dash = "dash"),
          name = "Truncation Boundary"
        ) %>%
        # Layout customization
        layout(
          title = paste0("<b>Normal(", input$omega_prior_mean_input, ", ", input$omega_prior_sd_input, 
                         ") with σ > 0 Truncation</b>"),
          xaxis = list(title = "ω (Standard Deviation of μ Between Experiments)"),
          yaxis = list(title = "Density"),
          hovermode = "x unified",
          annotations = list(
            x = -0.15, y = max(y)*0.9,
            text = "Excluded by <lower=0>",
            showarrow = FALSE,
            font = list(color = "red")
          )
        )

    })
    
    output$output_data <- downloadHandler(
      filename = function() {
        paste0("data_prepped_for_stan", Sys.Date(), ".rds")
      },
      content = function(file) {
        # Replace 'my_data' with your actual reactive or dataframe
        data_to_save <- list(nexpt=nrow(user_y_rdf()), y=user_y_rdf(), nobs=user_nobs_rdf())
        
        saveRDS(data_to_save, file)
      }
    )
    
    output$output_model <- downloadHandler(
      filename = function() {
        paste0("meta_analysis_model_", format(Sys.time(), "%Y%m%d_%H%M"), ".stan")
      },
      content = function(file) {
        # Validate inputs exist
        if (input$omega_switch) {
          req(
            input$omega_prior_mean_input,
            input$omega_prior_sd_input,
            input$sigma_prior_mean_input,
            input$sigma_prior_sd_input,
            input$mu_prior_mean_input,
            input$mu_prior_sd_input
          )
          
          # Create the Stan model content
          stan_code <- sprintf(
"// Bayesian Meta-Analysis Model
// Generated %s

data {
  int<lower=1> nexpt;          // number of experiments
  real<lower=0,upper=1> y[nexpt,2];  // observed mean response for each arm
  int nobs[nexpt,2];           // sample size for each arm (1 and 2)
}
parameters {
  real<lower=0, upper=1> m[nexpt,2]; 
  real<lower=0, upper=1> t[nexpt];
  real<lower=0, upper=1> mu;
  real<lower=0> sigma; 
  real<lower=0> omega; 
}
model {
  // priors
  mu ~ normal(%.3f, %.3f);
  omega ~ normal(%.3f, %.3f);   
  sigma ~ normal(%.3f, %.3f); 
  
  // likelihood
  for (i in 1:nexpt) {
    t[i] ~ normal(mu, omega);
    m[i,1] ~ normal(t[i], sigma);
    m[i,2] ~ normal(t[i], sigma);
    y[i,1] ~ normal(m[i,1], sqrt(m[i,1]*(1-m[i,1])/nobs[i,1])); 
    y[i,2] ~ normal(m[i,2], sqrt(m[i,2]*(1-m[i,2])/nobs[i,2]));
  }
}",
                  format(Sys.time(), '%Y-%m-%d %H:%M'),  # Timestamp
                  input$mu_prior_mean_input,             # mu mean
                  input$mu_prior_sd_input,               # mu sd
                  input$omega_prior_mean_input,          # omega mean
                  input$omega_prior_sd_input,            # omega sd
                  input$sigma_prior_mean_input,          # sigma mean
                  input$sigma_prior_sd_input             # sigma sd
          )
          
          # Write to file
          writeLines(stan_code, file, useBytes=TRUE)
        } else {
          req(
            input$sigma_prior_mean_input,
            input$sigma_prior_sd_input,
            input$mu_prior_mean_input,
            input$mu_prior_sd_input
          )
          
          # Create the Stan model content
          stan_code <- sprintf(
"// Bayesian Meta-Analysis Model
// Generated %s

data {
  int<lower=1> nexpt;          // number of experiments (now all from same website)
  real<lower=0,upper=1> y[nexpt,2];  // observed mean response for each arm
  int nobs[nexpt,2];           // sample size for each arm (1 and 2)
}
parameters {
  real<lower=0, upper=1> m[nexpt,2]; // arm-specific means
  real<lower=0, upper=1> mu;         // overall website mean (replaces t[i])
  real<lower=0> sigma;               // within-experiment variation
}
model {
  // priors
  mu ~ normal(%.3f, %.3f);
  sigma ~ normal(%.3f, %.3f); 

  // likelihood
  for (i in 1:nexpt) {
    m[i,1] ~ normal(mu, sigma); // both arms now share the same parent mu
    m[i,2] ~ normal(mu, sigma);
    y[i,1] ~ normal(m[i,1], sqrt(m[i,1]*(1-m[i,1])/nobs[i,1])); 
    y[i,2] ~ normal(m[i,2], sqrt(m[i,2]*(1-m[i,2])/nobs[i,2]));
  }
}",
                  format(Sys.time(), '%Y-%m-%d %H:%M'),
                  input$mu_prior_mean_input,
                  input$mu_prior_sd_input,
                  input$sigma_prior_mean_input,
                  input$sigma_prior_sd_input
          )
          
          # Write to file
          writeLines(stan_code, file, useBytes = TRUE)
        }
      }, contentType = "text/plain"
    )
})
}


# # test it separately
# ui <- fluidPage(
#   priors_equal_ui("priors_equal")
# )
# 
# server <- function(input, output, session) {
#   priors_equal_server("priors_equal", example_data)
# }
# 
# shinyApp(ui, server)

