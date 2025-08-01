# ui.R
library(shiny)
library(bslib)

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
      textOutput('test'),
      includeMarkdown("www/README.md")
  ),
  
  nav_panel(
    title = "Survey",
    value='survey_tab',
    h4("If you can check yes for every box, then Test & Roll will work for your use-case!"),
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
    markdown("Note: consider using multi-armed bandits if you can collect results in real time. If not, Test & Roll is the next best option.")
  ),
  
  nav_panel(
    title = "Bibliography",
    includeMarkdown("www/bibliography.md")
  )
)