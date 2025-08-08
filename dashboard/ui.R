# ui.R
library(shiny)
library(bslib)
library(bsicons)

business_questions <- c(
  "Primary Goal: Main objective is to maximize expected profit (or another business KPI like revenue, conversions) rather than just statistical significance.",
  "Decision Rule: You are willing to choose the best-performing variant after testing, even if it doesn’t meet traditional p-value thresholds."
)
test_design_questions <- c(
  "Available Population: You know the total available population you could treat.",
  "Variants: You are comparing two variants (A/B test)."
)
measurement_questions <- c(
  "Outcome Metric: Your key metric is either continuous (e.g., revenue per user) or a percentage/rate (e.g., conversion).",
  "Bayesian Prior: You have historical data or domain knowledge to form a prior belief about variant performance.",
  "Immediate Feedback: You can measure outcomes quickly after exposure (e.g., not long-term retention studies)."
)
risk_questions <- c(
  "Type I/II Error Tradeoff: You are comfortable relaxing strict false-positive controls (Type I error) to focus on profit maximization.",
  "Regulatory Constraints: There are no legal/regulatory barriers to choosing a variant without 'statistical significance'."
)
sequential_questions <- c(
  "Repeated Use: You will run multiple sequential tests - this is not required, but Test & Roll's Bayesian approach shines most here."
)

# Define the UI using bslib
ui <- page_navbar(
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
    h3("1. Business Objective"),
    checkboxGroupInput('surveyboxes1', label = '', choices=business_questions, width = '100%'),
    h3("2. Test Design"),
    checkboxGroupInput('surveyboxes2', label = '', choices=test_design_questions, width = '100%'),
    h3("3. Measurement and Data"),
    checkboxGroupInput('surveyboxes3', label = '', choices=measurement_questions, width = '100%'),
    h3("4. Risk Appetite"),
    checkboxGroupInput('surveyboxes4', label = '', choices=risk_questions, width = '100%'),
    h3("5. Sequential Testing"),
    checkboxGroupInput('surveyboxes4', label = '', choices=sequential_questions, width = '100%'),
    layout_columns(
    actionButton('survey_button', label='Submit Survey'),
    col_widths = 6
    )
  ),
  nav_panel(
    title = 'Crash Course',
    value = 'crash_course_tab',
    includeMarkdown("www/crash_course.md")
    
  ),
  nav_panel(
    title='Priors',
    value='priors_tab'
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
        title='Info'
      ), class = "d-flex align-items-center gap-1"),
      div(
        layout_columns(
        numericInput("N", label=tooltip(trigger = "N","Available Population"), value = 100000),
        numericInput("mu",label=tooltip(trigger = "μ (mu)","Average Conversion Rate across Prev Experiments"), value = 0.68),
        numericInput("sigma",tooltip(trigger = "σ (sigma)","Standard Deviation of Conversion Rates across Prev Experiments"), value = 0.03),
        numericInput('s',tooltip(trigger = "s","Standard Error of Difference in Means (approximated using μ)"), value=0),
        col_widths = c(3, 3, 3, 3)
      ),
      actionButton('calculate', 'Calculate Sample Size')
      )
    )
  ),
  nav_panel(
    title='Results',
    value='results_tab'
  ),
  nav_panel(
    title = "Bibliography",
    includeMarkdown("www/bibliography.md")
  ),
  nav_panel(
    title='Testing',
    textOutput('test')
  )
)