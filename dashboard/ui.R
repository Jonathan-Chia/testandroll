# ui.R
library(shiny)
library(bslib)
library(bsicons)
library(tibble)
library(dplyr)
library(markdown)

source('priors/priors_equal_mu.R')

business_questions <- c(
  "Primary Goal: Main objective is to maximize expected profit (or another business KPI like revenue, conversions) rather than just statistical significance." = "Q1",
  "Decision Rule: You are willing to choose the best-performing variant after testing, even if it doesn’t meet traditional p-value thresholds." = "Q2"
)
test_design_questions <- c(
  "Available Population: You know the total available population you could treat." = "Q1",
  "Variants: You are comparing two variants (A/B test)." = "Q2"
)
measurement_questions <- c(
  "Outcome Metric: Your key metric is either continuous (e.g., revenue per user) or a percentage/rate (e.g., conversion)." = 'Q1',
  "Bayesian Prior: You have historical data or domain knowledge to form a prior belief about variant performance." = "Q2",
  "Equal Prior Profit: Your prior expected profit for both tests is equal (e.g., testing two creatives or two features on a website)." = "Q3",
  "Immediate Feedback: You can measure outcomes quickly after exposure (e.g., not long-term retention studies)." = "Q4"
)
risk_questions <- c(
  "Regulatory Constraints: There are no legal/regulatory barriers to choosing a variant without 'statistical significance'." = 'Q1'
)

# Define the UI using bslib
ui <- page_navbar(
    tags$head(tags$style(HTML("
    /* Override helpText defaults */
    .help-block {
      font-size: 1rem !important;
      font-weight: normal !important;
      color: inherit !important;
      margin-bottom: 4px;
    }
    /* Make sure math inherits these styles */
    .help-block .MathJax {
      font-size: inherit !important;
    }
      /* Override for card headers (more specific) */
    .card-header .help-block {
      font-size: 1.25rem !important;
      font-weight: bold !important;
    }
  "))),
  title = "Test & Roll Calculator",
  id='main_nav',
  # Theme specification - choose one of these:
  theme = bs_theme(
    version = 5,
    # Choose ONE of these valid options:
    # bootswatch = "morph",    # Modern purple theme
    # bootswatch = "quartz",   # Clean light theme
    # bootswatch = "vapor",    # Cyberpunk style
    bootswatch = "cosmo",     # Classic Bootstrap
    # Or use a custom color scheme:
    # bg = "#FFFFFF", 
    # fg = "#000000", 
    # primary = "#0d6efd"
  ),
  
  # Navbar items
  nav_panel(
    title = "Start",
    value='start',
      includeMarkdown("www/README.md")
  ),
  nav_panel(
    title = "Survey",
    value='survey_tab',
    h3("1. Objective"),
    checkboxGroupInput('surveyboxes1', label = '', choices=business_questions, width = '100%'),
    h3("2. Test Design"),
    checkboxGroupInput('surveyboxes2', label = '', choices=test_design_questions, width = '100%'),
    h3("3. Data"),
    checkboxGroupInput('surveyboxes3', label = '', choices=measurement_questions, width = '100%'),
    h3("4. Constraints"),
    checkboxGroupInput('surveyboxes4', label = '', choices=risk_questions, width = '100%'),
    layout_columns(
    actionButton('survey_button', label='Submit Survey'),
    col_widths = 6
    )
  ),
  nav_panel(
    title = 'Crash Course',
    value = 'crash_course_tab',
    withMathJax(),  # add latex rendering
    includeMarkdown("www/crash_course.md")
    
  ),
  nav_panel(
    title='Meta Analysis',
    value='priors_tab',
    markdown("## Introduction"),
    withMathJax(helpText("This tab helps you to find \\(s\\) and \\(\\sigma\\) through Bayesian meta analysis.")),
    markdown("Inputs:
             * Previous experiment results
             * Priors for the previous experiment parameters
             
             Outputs:
             * Formatted data
             * Stan model file (you'll have to run this locally due to lack of compute power)
             "),
    markdown("## Steps"),
    markdown("### 1. Input Previous Experiment Results"),
    priors_equal_ui('priors_equal')
  ),
  nav_panel(
    title = 'Sample Size',
    value = 'sample_size_tab',
    tags$head(
      tags$style(HTML("
      .popover {
        max-width: 600px !important;  /* Adjust width as needed */
      }
    "))
    ),
    card(
      card_header('Input Parameters to Calculate Profit Maximizing Sample Size', 
        popover(
        bsicons::bs_icon("question-circle", class='ms-auto'),
        p("If you have a test where the profit earned for each customer is:"),
        withMathJax("$$y \\sim \\mathcal{N}(m_1, s) \\text{ for group 1}$$"),
        withMathJax("$$y \\sim \\mathcal{N}(m_2, s) \\text{ for group 2}$$"),
        p("and your priors are"),
        withMathJax("$$(m_1, m_2 \\sim N(\\mu, \\sigma))$$"),
        p("the profit-maximizing sample size is:"),
        withMathJax("$$n_1 = n_2 = \\sqrt{\\frac{N}{4}\\left( \\frac{s}{\\sigma} \\right)^2 + \\left( \\frac{3}{4} \\left( \\frac{s}{\\sigma} \\right)^2 \\right)^2 } - \\frac{3}{4} \\left(\\frac{s}{\\sigma} \\right)^2$$"),
        p("Input your N, μ, and σ parameters and then press the calculate button to get n1 and n2!"),
        title='Info',
        options = list(trigger = 'focus')
      ), class = "d-flex align-items-center gap-1"),
      div(
        layout_columns(
        numericInput("N",label='N', value = 100000) |> 
          popover(placement='bottom',
                  markdown('N is the total number of customers you have available, i.e. the size of your email mailing list or the the number of visits that might visit a webpage in the next month.'),
                  plotlyOutput('N_parameter_plot', height = "300px"),
                  options = list(trigger = 'focus')
                  ),
        numericInput("mu",label="μ (mu)", value = 0.68) |> 
          popover(placement='bottom',
                  markdown("μ is the average profit across previous experiments, i.e. the average click-through rate or average revenue."),
                  plotlyOutput('mu_parameter_plot', height = "300px"),
                  options = list(trigger = 'focus')
          ),
        numericInput("sigma",label="σ (sigma)", value = 0.03) |> 
          popover(placement='bottom',
                  markdown("σ is the difference we expect between average profit for the two treatments, or in other words, the standard deviation for the difference in average profit between treatments."),
                  plotlyOutput('sigma_parameter_plot', height = "300px"),
                  options = list(trigger = 'focus')
          ),
        numericInput('s', label="s", value=0) |> 
          popover(placement='bottom',
                  markdown("s is how variable the profit is from one customer to another (approximated using μ). Hypothesis testing requires much bigger sample sizes (proportional to s^2 instead of s)."),
                  plotlyOutput('s_parameter_plot', height = "300px"),
                  options = list(trigger = 'focus')
          ),
        col_widths = c(3, 3, 3, 3)
      ),
      actionButton('calculate', 'Calculate Sample Size')
      ),
      plotlyOutput('optimal_sample_size_plot')
    )
  ),
  nav_panel(
    title='Results',
    value='results_tab',
    page_fluid(
      card(
        card_header(
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              span("Upload Your Test Results with Same Format as Example", style = "font-weight: bold; font-size: 1.25rem;"),
              fileInput("test_results_file", NULL, accept = ".csv", buttonLabel = "Upload Test Results CSV")
          )
        ),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            
            # Left: Example data
            card(
              card_header("Example Results"),
              DTOutput("example_results"),
              actionButton('use_example', label='Use Example Results') |>
                popover(
                  "Warning:",
                  "This is simulated data that you can use to understand how the rest of the tab works.",
                  placement = "top",
                  options = list(trigger = 'focus')
                )
            ),
            
            # Right: Placeholder for uploaded data
            card(
              card_header("Your Data"),
              DTOutput("user_results"),
              actionButton('examine_results', label='Examine Results')
            )
          )
        )),
      card(
        card_header(
          span("Results", style = "font-weight: bold; font-size: 1.25rem;"),
          popover(
            bsicons::bs_icon("question-circle", class='ms-auto'),
            title = "Info",
            withMathJax(helpText(
              HTML("
  <h4><b>Bayesian Uplift & Loss Calculation</b></h4>
  
  <b>Expected Uplift</b> (when choosing A over B):
  $$\\text{Uplift} = \\frac{Y_A - Y_B}{Y_B}$$
  <ul>
    <li>Positive value means A performs better than B</li>
    <li>95% Credible Interval (HDI) shows uncertainty range</li>
  </ul>
  
  <b>Expected Loss</b> (risk of choosing A if B is actually better):
  $$\\text{Loss} = \\max\\left(\\frac{Y_B - Y_A}{Y_A}, 0\\right)$$
  <ul>
    <li>Quantifies potential downside of implementing A</li>
    <li>Only counts scenarios where B > A</li>
  </ul>
  
  <b>Decision Guide</b>:
  <ul>
    <li>When uplift > 0 and loss is small → Implement A</li>
    <li>When uplift HDI includes 0 → Results inconclusive</li>
    <li>When loss is large → Might want to stick with B if this is a very high stakes test</li>
  </ul>
  ")
            )),
            options = list(trigger = 'focus')
          ), class = "d-flex align-items-center gap-1"),
        DTOutput('results_table'),
        plotlyOutput('results_posteriors'),
        plotlyOutput('results_posteriors_diffs')
      )
    )
  ),
  nav_panel(
    title = "Bibliography",
    includeMarkdown("www/bibliography.md")
  )
)