# app.R

# Load necessary libraries
# If you don't have these installed, run:
# install.packages(c("shiny", "shinythemes", "ggplot2", "dplyr", "DT", "markdown", "rgl"))
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(DT) # For interactive tables
library(markdown) # For rendering markdown content
library(rgl) # For 3D graphics (rglwidget() is part of this package)

# --- Data and Content for the App ---

# Cardiac Cycle Data (for a simple interactive diagram)
cardiac_phases <- data.frame(
  phase = c("Atrial Systole", "Isovolumic Contraction", "Ventricular Ejection",
            "Isovolumic Relaxation", "Ventricular Filling"),
  description = c(
    "Atria contract, pushing blood into ventricles. This corresponds to the P wave on ECG and a slight rise in atrial pressure.",
    "Ventricles contract, increasing pressure rapidly. All valves are closed. This phase occurs between the R wave and the T wave on ECG.",
    "Ventricular pressure exceeds aortic/pulmonary pressure, opening semilunar valves. Blood is ejected. This corresponds to the ST segment and T wave on ECG.",
    "Ventricles relax, pressure drops rapidly. All valves are closed. This phase occurs after the T wave and before the next P wave.",
    "Ventricular pressure drops below atrial pressure, opening AV valves. Ventricles fill passively and then actively during atrial systole. This corresponds to the period after the T wave and before the P wave."
  ),
  # Time ranges for Wiggers Diagram highlighting (normalized 0-1)
  start_time = c(0.0, 0.15, 0.3, 0.5, 0.65),
  end_time = c(0.15, 0.3, 0.5, 0.65, 1.0) # Adjusted for a full cycle
)

# Wiggers Diagram Data (simplified for illustration)
# Normalized time 0 to 1 for one cardiac cycle
wiggers_data <- data.frame(
  time = seq(0, 1, by = 0.005), # Length: 201
  ecg_voltage = c(
    dnorm(seq(0.05, 0.1, length.out = 11), mean = 0.075, sd = 0.01) * 0.5, # P wave (11)
    rep(0, 20), # (20)
    c(-0.3, -0.7, 1.5, -0.5, -0.2), # QRS complex (5)
    rep(0, 30), # (30)
    dnorm(seq(0.6, 0.7, length.out = 21), mean = 0.65, sd = 0.03) * 0.8, # T wave (21)
    rep(0, 114) # Adjusted from 113 to 114 to make total length 201 (11+20+5+30+21+114 = 201)
  ),
  aortic_pressure = c(
    rep(80, 50), # Diastolic
    seq(80, 120, length.out = 20), # Systolic rise
    seq(120, 90, length.out = 30), # Systolic fall
    rep(80, 101) # Diastolic
  ),
  ventricular_pressure = c(
    seq(5, 10, length.out = 10), # Filling
    seq(10, 120, length.out = 20), # Isovolumic contraction & ejection
    seq(120, 5, length.out = 30), # Relaxation
    rep(5, 141) # Filling
  ),
  atrial_pressure = c(
    seq(5, 15, length.out = 10), # Atrial contraction
    rep(5, 40), # Isovolumic contraction
    seq(5, 10, length.out = 10), # Filling
    rep(5, 141) # Filling
  ),
  ventricular_volume = c(
    seq(120, 100, length.out = 10), # Atrial systole
    rep(100, 20), # Isovolumic contraction
    seq(100, 50, length.out = 30), # Ejection
    rep(50, 20), # Isovolumic relaxation
    seq(50, 120, length.out = 121) # Filling
  )
)
# Scale ECG voltage for plotting
wiggers_data$ecg_voltage <- wiggers_data$ecg_voltage * 0.2

# Quiz Questions
quiz_questions <- list(
  list(
    question = "Which ECG wave represents atrial depolarization?",
    options = c("P wave", "QRS complex", "T wave", "U wave"),
    answer = "P wave"
  ),
  list(
    question = "During which phase of the cardiac cycle do the ventricles contract but no blood is ejected?",
    options = c("Atrial Systole", "Ventricular Ejection", "Isovolumic Contraction", "Isovolumic Relaxation"),
    answer = "Isovolumic Contraction"
  ),
  list(
    question = "What is the primary symptom of Myocardial Infarction?",
    options = c("Leg swelling", "Chest pain", "Blurred vision", "Nosebleeds"),
    answer = "Chest pain"
  ),
  list(
    question = "Which valve prevents backflow of blood from the left ventricle to the left atrium?",
    options = c("Tricuspid valve", "Pulmonary valve", "Mitral valve", "Aortic valve"),
    answer = "Mitral valve"
  ),
  list(
    question = "What is the normal range for systolic blood pressure in a healthy adult?",
    options = c("Below 90 mmHg", "90-120 mmHg", "120-140 mmHg", "Above 140 mmHg"),
    answer = "90-120 mmHg"
  )
)

# Clinical Cases Data
clinical_cases <- list(
  list(
    case_id = 1,
    scenario = "A 65-year-old male presents to the emergency department with sudden onset of severe, crushing chest pain radiating to his left arm. He is diaphoretic and nauseous. His ECG shows ST-segment elevation in leads V2-V4.",
    question = "What is the most likely diagnosis?",
    options = c("Angina Pectoris", "Myocardial Infarction", "Pericarditis", "Heart Failure"),
    answer = "Myocardial Infarction",
    explanation = "ST-segment elevation on ECG with crushing chest pain is highly indicative of an acute myocardial infarction (heart attack)."
  ),
  list(
    case_id = 2,
    scenario = "A 78-year-old female complains of increasing shortness of breath, especially when lying flat, and swelling in her ankles. She has a history of hypertension. On examination, you hear crackles in her lungs.",
    question = "Which condition is most consistent with these symptoms?",
    options = c("Atrial Fibrillation", "Hypertension", "Heart Failure", "Valvular Stenosis"),
    answer = "Heart Failure",
    explanation = "Shortness of breath (orthopnea/dyspnea on exertion), ankle swelling, and lung crackles are classic signs of fluid overload due to the heart's inability to pump effectively, characteristic of heart failure."
  ),
  list(
    case_id = 3,
    scenario = "A 55-year-old patient reports intermittent palpitations and feels lightheaded at times. On physical examination, her pulse is irregularly irregular.",
    question = "What is the most probable arrhythmia?",
    options = c("Sinus Tachycardia", "Ventricular Tachycardia", "Atrial Fibrillation", "Bradycardia"),
    answer = "Atrial Fibrillation",
    explanation = "An 'irregularly irregular' pulse is the hallmark clinical finding for Atrial Fibrillation, where the atria beat chaotically."
  )
)

# --- User Interface (UI) ---
ui <- fluidPage(
  # Apply a clean theme
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel(
    h1("Cardiology Essentials for Medical Students", align = "center",
       style = "color: #2c3e50; font-family: 'Inter', sans-serif; font-weight: 600;")
  ),
  
  # Sidebar layout with navigation
  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = "background-color: #ecf0f1; padding: 20px; border-radius: 8px;",
      h3("Navigation", style = "color: #2c3e50; margin-bottom: 20px;"),
      # Navigation buttons
      actionButton("home_btn", "Home", icon = icon("house-medical"),
                   class = "btn-primary btn-block mb-2"),
      actionButton("cardiac_cycle_btn", "Cardiac Cycle", icon = icon("arrows-rotate"),
                   class = "btn-success btn-block mb-2"),
      actionButton("quiz_btn", "Quiz", icon = icon("question-circle"),
                   class = "btn-danger btn-block mb-2"),
      actionButton("model3d_btn", "3D Heart Model", icon = icon("heart"), # New button for 3D model
                   class = "btn-info btn-block mb-2"),
      actionButton("cases_btn", "Clinical Cases", icon = icon("user-doctor"), # New button for Clinical Cases
                   class = "btn-primary btn-block mb-2"),
      hr(),
      p("Learn the fundamentals of cardiology interactively.",
        style = "font-size: 0.9em; color: #7f8c8d; text-align: center;")
    ),
    
    # Main panel for displaying content
    mainPanel(
      width = 9,
      style = "background-color: #ffffff; padding: 30px; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
      # Conditional panels to show/hide content based on button clicks
      uiOutput("page_content")
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # Reactive value to control which page is shown
  current_page <- reactiveVal("home")
  
  # Initialize current_page to "home" on startup
  observeEvent(TRUE, {
    current_page("home")
  }, once = TRUE)
  
  # Observe navigation button clicks and update current_page
  observeEvent(input$home_btn, { current_page("home") })
  observeEvent(input$cardiac_cycle_btn, { current_page("cardiac_cycle") })
  observeEvent(input$quiz_btn, { current_page("quiz") })
  observeEvent(input$model3d_btn, { current_page("model3d") }) # New observer for 3D model
  observeEvent(input$cases_btn, { current_page("cases") }) # New observer for Clinical Cases
  
  # Render the appropriate page content based on current_page
  output$page_content <- renderUI({
    if (current_page() == "home") {
      tagList(
        h2("Welcome to Cardiology Essentials!", style = "color: #34495e;"),
        p("This interactive application is designed to help medical students understand core concepts in cardiology. Use the navigation panel on the left to explore different topics.",
          style = "font-size: 1.1em; line-height: 1.6;"),
        img(src = "https://placehold.co/600x300/e74c3c/ffffff?text=Heart+Diagram",
            alt = "Heart Diagram Placeholder",
            style = "max-width: 100%; height = auto; display: block; margin: 20px auto; border-radius: 8px;"),
        h3("Topics Covered:", style = "color: #34495e;"),
        tags$ul(
          tags$li("Cardiac Cycle: Explore the phases of a heartbeat with a Wiggers Diagram."),
          tags$li("Quiz: Test your knowledge!"),
          tags$li("3D Heart Model: Explore a simplified interactive 3D heart with blood flow and valve states."),
          tags$li("Clinical Cases: Apply your knowledge to real-world scenarios.")
        ),
        p("Start by selecting a topic from the sidebar!", style = "font-style: italic; color: #7f8c8d;")
      )
    } else if (current_page() == "cardiac_cycle") {
      tagList(
        h2("The Cardiac Cycle & Wiggers Diagram", style = "color: #34495e;"),
        p("The cardiac cycle describes the sequence of events that occurs during one complete heartbeat, involving both systole (contraction) and diastole (relaxation). The Wiggers Diagram integrates various physiological events over time.",
          style = "font-size: 1.1em; line-height: 1.6;"),
        hr(),
        fluidRow(
          column(4,
                 selectInput("cardiac_phase_select", "Select a Phase:",
                             choices = cardiac_phases$phase,
                             selected = cardiac_phases$phase[1])
          ),
          column(8,
                 wellPanel(
                   style = "background-color: #ecf0f1; border-radius: 8px; padding: 15px;",
                   h4("Description:", style = "color: #34495e;"),
                   textOutput("cardiac_phase_description")
                 )
          )
        ),
        plotOutput("wiggers_diagram_plot", height = "500px"),
        p("This diagram illustrates the synchronized electrical, pressure, volume, and sound events during a single heartbeat. Select a phase to highlight it.", style = "font-style: italic; color = #7f8c8d;")
      )
    } else if (current_page() == "quiz") {
      tagList(
        h2("Test Your Knowledge!", style = "color: #34495e;"),
        p("Answer the following questions to check your understanding.",
          style = "font-size: 1.1em; line-height: 1.6;"),
        hr(),
        uiOutput("quiz_question_ui"),
        actionButton("submit_quiz", "Submit Answer", class = "btn-primary mt-3"),
        br(), br(),
        textOutput("quiz_feedback"),
        br(),
        textOutput("quiz_score"), # Display quiz score
        actionButton("reset_quiz", "Reset Quiz", class = "btn-secondary mt-3") # Reset button
      )
    } else if (current_page() == "model3d") { # Updated 3D Heart Model page
      tagList(
        h2("Left Atrial Dynamics", style = "color: #34495e;"),
        p("Explore the Left Atrium and its function. Adjust the Left Ventricular function to see its impact on LA size. Select a phase to visualize blood flow and valve states.",
          style = "font-size: 1.1em; line-height: 1.6;"),
        hr(),
        fluidRow(
          column(6,
                 h4("3D Left Atrium Model:", style = "color: #34495e;"),
                 rglwidgetOutput("heart_3d_model", width = "100%", height = "400px"),
                 br(),
                 sliderInput("lv_function_slider", "Left Ventricular Function:",
                             min = 0, max = 70, value = 70, step = 1, # Max changed to 70
                             post = "%",
                             helpText("Lower % simulates reduced LV function (e.g., heart failure).")),
                 selectInput("la_cycle_phase", "LA Cycle Phase:",
                             choices = c("Filling", "Ejection"),
                             selected = "Filling")
          ),
          column(6,
                 h4("Left Atrial Physiological Curves:", style = "color: #34495e;"),
                 selectInput("la_chart_type", "Select Chart Type:",
                             choices = c("LA Pressure-Volume Loop",
                                         "Diastolic Filling (E/A Wave)",
                                         "LA Pressure Curve (A, C, V Waves)",
                                         "LA Volume Curve"),
                             selected = "LA Pressure-Volume Loop"),
                 plotOutput("dynamic_la_chart", height = "400px"), # Single dynamic plot
                 p("These charts conceptually illustrate how LA pressure and volume change with LV function and LA cycle phase, and how diastolic filling patterns are affected.", style = "font-style: italic; color: #7f8c8d;")
          )
        ),
        br(),
        p("This module focuses on the Left Atrium to demonstrate specific chamber dynamics and their physiological correlations.", style = "font-style: italic; color: #7f8c8d;")
      )
    } else if (current_page() == "cases") { # New Clinical Cases page
      tagList(
        h2("Clinical Cases", style = "color: #34495e;"),
        p("Apply your knowledge to the following clinical scenarios. Read the case carefully and select the most likely diagnosis.",
          style = "font-size: 1.1em; line-height: 1.6;"),
        hr(),
        uiOutput("case_scenario_ui"),
        actionButton("submit_case_answer", "Submit Answer", class = "btn-primary mt-3"),
        br(), br(),
        textOutput("case_feedback"),
        br(),
        actionButton("next_case", "Next Case", class = "btn-secondary mt-3")
      )
    }
  })
  
  # --- Wiggers Diagram Plot Output ---
  output$wiggers_diagram_plot <- renderPlot({
    req(input$cardiac_phase_select)
    
    # Get selected phase times for highlighting
    selected_phase_info <- cardiac_phases %>%
      filter(phase == input$cardiac_phase_select)
    
    highlight_start <- selected_phase_info$start_time
    highlight_end <- selected_phase_info$end_time
    
    ggplot(wiggers_data, aes(x = time)) +
      # Highlight selected phase
      geom_rect(aes(xmin = highlight_start, xmax = highlight_end, ymin = -Inf, ymax = Inf),
                fill = "lightblue", alpha = 0.3) +
      # ECG
      geom_line(aes(y = ecg_voltage * 50 + 100), color = "black", linetype = "solid", linewidth = 1) + # Scale for visibility
      annotate("text", x = 0.075, y = 110, label = "P", color = "black", size = 4) +
      annotate("text", x = 0.3, y = 80, label = "QRS", color = "black", size = 4) +
      annotate("text", x = 0.65, y = 110, label = "T", color = "black", size = 4) +
      # Pressures
      geom_line(aes(y = aortic_pressure), color = "#e74c3c", linewidth = 1, linetype = "solid") +
      geom_line(aes(y = ventricular_pressure), color = "#3498db", linewidth = 1, linetype = "solid") +
      geom_line(aes(y = atrial_pressure), color = "#2ecc71", linewidth = 1, linetype = "solid") +
      # Volume
      geom_line(aes(y = ventricular_volume * 0.5), color = "#9b59b6", linewidth = 1, linetype = "solid") + # Scale for visibility
      # Heart Sounds (simplified points) - Changed to annotate()
      annotate("point", x = 0.25, y = 10, color = "black", size = 4, shape = 8) + # S1
      annotate("text", x = 0.25, y = 5, label = "S1", color = "black", size = 4) +
      annotate("point", x = 0.55, y = 10, color = "black", size = 4, shape = 8) + # S2
      annotate("text", x = 0.55, y = 5, label = "S2", color = "black", size = 4) +
      
      labs(title = "Wiggers Diagram: Cardiac Cycle Events",
           x = "Time (normalized)", y = "Physiological Parameters") +
      scale_y_continuous(
        name = "Pressure (mmHg)",
        sec.axis = sec_axis(~./0.5, name = "Volume (mL)") # Secondary axis for volume
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#2c3e50"),
        axis.title = element_text(size = 14, color = "#34495e"),
        axis.text = element_text(size = 12, color = "#7f8c8d"),
        panel.grid.major = element_line(color = "#bdc3c7", linetype = "dotted"),
        panel.grid.minor = element_blank()
      ) +
      # Add custom legend
      guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid", "solid")))) +
      annotate("text", x = 0.9, y = 120, label = "Aortic Pressure", color = "#e74c3c", size = 3) +
      annotate("text", x = 0.9, y = 80, label = "Ventricular Pressure", color = "#3498db", size = 3) +
      annotate("text", x = 0.9, y = 40, label = "Atrial Pressure", color = "#2ecc71", size = 3) +
      annotate("text", x = 0.9, y = 60, label = "Ventricular Volume", color = "#9b59b6", size = 3) +
      annotate("text", x = 0.9, y = 100, label = "ECG", color = "black", size = 3)
  })
  
  
  # --- Quiz Logic ---
  current_question_index <- reactiveVal(1)
  quiz_feedback_text <- reactiveVal("")
  correct_answers_count <- reactiveVal(0)
  total_questions_answered <- reactiveVal(0)
  
  output$quiz_question_ui <- renderUI({
    idx <- current_question_index()
    if (idx <= length(quiz_questions)) {
      question_data <- quiz_questions[[idx]]
      tagList(
        h4(paste0("Question ", idx, " of ", length(quiz_questions), ": ", question_data$question), style = "color: #34495e;"),
        radioButtons(paste0("q", idx, "_answer"), "Select your answer:",
                     choices = question_data$options,
                     selected = character(0) # No default selection
        )
      )
    } else {
      tagList(
        h3("Quiz Completed!", style = "color: #28a745;"),
        p(paste0("You answered ", correct_answers_count(), " out of ", length(quiz_questions), " questions correctly."),
          style = "font-size: 1.1em;"),
        actionButton("restart_quiz", "Restart Quiz", class = "btn-secondary")
      )
    }
  })
  
  observeEvent(input$submit_quiz, {
    idx <- current_question_index()
    if (idx <= length(quiz_questions)) {
      question_data <- quiz_questions[[idx]]
      selected_answer_id <- paste0("q", idx, "_answer")
      user_answer <- input[[selected_answer_id]]
      
      if (is.null(user_answer)) {
        quiz_feedback_text("Please select an answer before submitting.")
      } else {
        total_questions_answered(total_questions_answered() + 1)
        if (user_answer == question_data$answer) {
          quiz_feedback_text(paste0("Correct! The answer was: ", question_data$answer))
          correct_answers_count(correct_answers_count() + 1)
        } else {
          quiz_feedback_text(paste0("Incorrect. The correct answer was: ", question_data$answer))
        }
        # Move to next question after a short delay for feedback
        invalidateLater(1500, session) # Wait 1.5 seconds
        current_question_index(idx + 1)
      }
    }
  })
  
  observeEvent(input$restart_quiz, {
    current_question_index(1)
    quiz_feedback_text("")
    correct_answers_count(0)
    total_questions_answered(0)
  })
  
  output$quiz_feedback <- renderText({
    quiz_feedback_text()
  })
  
  output$quiz_score <- renderText({
    if (current_question_index() > length(quiz_questions)) {
      paste0("Your score: ", correct_answers_count(), " / ", length(quiz_questions))
    } else {
      ""
    }
  })
  
  # --- Clinical Cases Logic ---
  current_case_index <- reactiveVal(1)
  case_feedback_text <- reactiveVal("")
  
  output$case_scenario_ui <- renderUI({
    idx <- current_case_index()
    if (idx <= length(clinical_cases)) {
      case_data <- clinical_cases[[idx]]
      tagList(
        h4(paste0("Case ", idx, " of ", length(clinical_cases), ":"), style = "color: #34495e;"),
        p(strong("Scenario:"), case_data$scenario, style = "font-size: 1.05em; line-height: 1.5;"),
        br(),
        h5(case_data$question, style = "color: #34495e;"),
        radioButtons(paste0("case", idx, "_answer"), "Select your answer:",
                     choices = case_data$options,
                     selected = character(0) # No default selection
        )
      )
    } else {
      tagList(
        h3("All Clinical Cases Completed!", style = "color: #28a745;"),
        p("You have reviewed all available clinical cases. Great job!", style = "font-size: 1.1em;"),
        actionButton("restart_cases", "Restart Cases", class = "btn-secondary")
      )
    }
  })
  
  observeEvent(input$submit_case_answer, {
    idx <- current_case_index()
    if (idx <= length(clinical_cases)) {
      case_data <- clinical_cases[[idx]]
      selected_answer_id <- paste0("case", idx, "_answer")
      user_answer <- input[[selected_answer_id]]
      
      if (is.null(user_answer)) {
        case_feedback_text("Please select an answer before submitting.")
      } else if (user_answer == case_data$answer) {
        case_feedback_text(paste0("Correct! ", case_data$explanation))
      } else {
        case_feedback_text(paste0("Incorrect. The correct answer was '", case_data$answer, "'. Explanation: ", case_data$explanation))
      }
    }
  })
  
  observeEvent(input$next_case, {
    # Only move to next if current case is answered or if it's the last case
    if (current_case_index() <= length(clinical_cases)) {
      case_feedback_text("") # Clear feedback for next case
      current_case_index(current_case_index() + 1)
    }
  })
  
  observeEvent(input$restart_cases, {
    current_case_index(1)
    case_feedback_text("")
  })
  
  output$case_feedback <- renderText({
    case_feedback_text()
  })
  
  # --- 3D Left Atrium Model Specific Reactives ---
  # Reactive expression to safely get the current LA cycle phase
  current_la_cycle_phase_reactive <- reactive({
    req(input$la_cycle_phase) # Ensure input is available
    input$la_cycle_phase
  })
  
  # --- 3D Left Atrium Model Output ---
  output$heart_3d_model <- renderRglwidget({
    # Clear any existing rgl device using the correct function
    if (rgl.cur() > 0) {
      clear3d()
      close3d()
    }
    
    # Open a new rgl device
    open3d(useNULL = TRUE)
    bg3d("lightgray") # Set background to light gray
    
    # Get current LA cycle phase and LV function
    current_la_phase <- current_la_cycle_phase_reactive() # Now correctly defined within server
    lv_function_percent <- input$lv_function_slider
    
    # Define colors
    la_color_filling <- "#f1c40f" # Yellow for filling
    la_color_ejection <- "#8e44ad" # Purple for ejection/contraction
    
    flow_color_oxy <- "#e74c3c" # Red for oxygenated blood flow
    no_flow_color <- "darkgray" # Color when no active flow
    
    valve_open_color <- "green"
    valve_closed_color <- "darkred"
    
    # Dynamic LA radius based on LV function
    base_la_radius_rgl_units <- 0.6 # Base radius in rgl units
    la_dilation_factor <- (70 - lv_function_percent) / 70
    la_dilation_factor <- max(0, min(1, la_dilation_factor)) # Clamp between 0 and 1
    la_radius_mod_rgl_units <- base_la_radius_rgl_units * (1 + la_dilation_factor * 0.5) # Max 50% increase at 0% LV function
    
    # Conceptual LA diameter in cm (for display)
    min_la_diameter_cm <- 3.8
    max_la_diameter_cm <- 6.0
    conceptual_la_diameter_cm <- round(min_la_diameter_cm + (max_la_diameter_cm - min_la_diameter_cm) * la_dilation_factor, 1)
    
    
    # Determine LA color, mitral valve state, and blood flow based on LA cycle phase
    mitral_valve_state <- valve_closed_color
    flow_pv_la <- no_flow_color # Pulmonary Veins to LA
    flow_la_lv <- no_flow_color # LA to LV
    
    if (current_la_phase == "Filling") {
      la_chamber_color <- la_color_filling
      mitral_valve_state <- valve_open_color
      flow_pv_la <- flow_color_oxy
    } else { # Ejection
      la_chamber_color <- la_color_ejection
      mitral_valve_state <- valve_closed_color # Mitral valve closes during LA contraction and LV systole
      flow_la_lv <- flow_color_oxy
    }
    
    
    # --- Left Atrium (main body - more detailed shape) ---
    # Central part
    spheres3d(0, 0, 0, radius = la_radius_mod_rgl_units, color = la_chamber_color, alpha = 0.8, lit = TRUE, smooth = TRUE, subdivisions = 100) # Increased subdivisions for smoother spheres
    # Appendage/lobe-like structures for more organic shape
    spheres3d(la_radius_mod_rgl_units * 0.7, la_radius_mod_rgl_units * 0.3, la_radius_mod_rgl_units * 0.2, radius = la_radius_mod_rgl_units * 0.4, color = la_chamber_color, alpha = 0.8, lit = TRUE, smooth = TRUE, subdivisions = 100)
    spheres3d(-la_radius_mod_rgl_units * 0.7, la_radius_mod_rgl_units * 0.3, la_radius_mod_rgl_units * 0.2, radius = la_radius_mod_rgl_units * 0.4, color = la_chamber_color, alpha = 0.8, lit = TRUE, smooth = TRUE, subdivisions = 100)
    spheres3d(0.2, la_radius_mod_rgl_units * 0.7, -la_radius_mod_rgl_units * 0.2, radius = la_radius_mod_rgl_units * 0.3, color = la_chamber_color, alpha = 0.8, lit = TRUE, smooth = TRUE, subdivisions = 100) # Another lobe
    
    # Main LA label - adjusted position and color
    rgl.texts(0, 0.5, la_radius_mod_rgl_units + 0.5, "Left Atrium", color = "black", cex = 1.2, adj = c(0.5, 0.5)) # Using rgl.texts
    # LA Size label - adjusted position and color
    rgl.texts(0, -la_radius_mod_rgl_units - 0.5, 0, paste0("LA Size: ~", conceptual_la_diameter_cm, " cm"), color = "black", cex = 0.9, adj = c(0.5, 0.5))
    
    
    # --- Pulmonary Veins (entering LA - more distinct) ---
    # Top left
    lines3d(x = c(-la_radius_mod_rgl_units - 0.3, -la_radius_mod_rgl_units * 0.8), y = c(0.4, 0.2), z = c(0.1, 0.1),
            color = flow_pv_la, lwd = 7, alpha = 0.9)
    rgl.texts(-la_radius_mod_rgl_units - 0.4, 0.4, 0.1, "Pulmonary Veins", color = "black", cex = 0.8, adj = c(0,0.5))
    # Bottom left
    lines3d(x = c(-la_radius_mod_rgl_units - 0.3, -la_radius_mod_rgl_units * 0.8), y = c(-0.4, -0.2), z = c(0.1, 0.1),
            color = flow_pv_la, lwd = 7, alpha = 0.9)
    # Top right
    lines3d(x = c(la_radius_mod_rgl_units + 0.3, la_radius_mod_rgl_units * 0.8), y = c(0.4, 0.2), z = c(0.1, 0.1),
            color = flow_pv_la, lwd = 7, alpha = 0.9)
    # Bottom right
    lines3d(x = c(la_radius_mod_rgl_units + 0.3, la_radius_mod_rgl_units * 0.8), y = c(-0.4, -0.2), z = c(0.1, 0.1),
            color = flow_pv_la, lwd = 7, alpha = 0.9)
    
    
    # --- Mitral Valve (connecting LA to conceptual LV outflow) ---
    # Positioned below the LA, representing the outflow tract
    spheres3d(0, -la_radius_mod_rgl_units * 0.8, 0, radius = 0.1, color = mitral_valve_state, alpha = 0.9, lit = TRUE, smooth = TRUE, subdivisions = 100)
    rgl.texts(0, -la_radius_mod_rgl_units * 0.8, 0.2, "Mitral Valve", color = "black", cex = 0.8, adj = c(0.5, 0.5))
    
    # --- Blood Flow LA to conceptual LV outflow ---
    lines3d(x = c(0, 0), y = c(-la_radius_mod_rgl_units * 0.8, -la_radius_mod_rgl_units * 1.5), z = c(0, 0),
            color = flow_la_lv, lwd = 5, alpha = 1)
    rgl.texts(0, -la_radius_mod_rgl_units * 1.7, 0, "To Left Ventricle", color = "black", cex = 0.8, adj = c(0.5, 0.5))
    
    
    # Add a subtle light source for better rendering
    light3d(theta = 45, phi = 45, diffuse = "white", specular = "white")
    
    # Set up the view
    view3d(theta = 30, phi = 30, zoom = 0.7)
    rglwidget() # Return the rglwidget object
  })
  
  # --- Dynamic LA Charts Output (Consolidated) ---
  output$dynamic_la_chart <- renderPlot({
    req(input$lv_function_slider, input$la_cycle_phase, input$la_chart_type)
    
    lv_function_percent <- input$lv_function_slider
    la_phase <- input$la_cycle_phase
    chart_type <- input$la_chart_type
    
    # Adjustments for reduced LV function (lower LV function means higher LA pressure/volume)
    lv_impairment_factor <- (70 - lv_function_percent) / 70
    lv_impairment_factor <- max(0, min(1, lv_impairment_factor)) # Clamp between 0 and 1
    
    # --- Common parameters for all plots ---
    time_points <- seq(0, 1, length.out = 100)
    
    # --- LA Pressure-Volume Loop Data ---
    base_pv_pressure <- 10
    base_pv_volume <- 60
    pv_pressure_increase_factor <- 1 + lv_impairment_factor * 0.5
    pv_volume_increase_factor <- 1 + lv_impairment_factor * 0.5
    
    if (la_phase == "Filling") {
      pv_la_pressure_start <- base_pv_pressure * pv_pressure_increase_factor * 0.8
      pv_la_pressure_end <- base_pv_pressure * pv_pressure_increase_factor * 1.2
      pv_la_volume_start <- base_pv_volume * pv_volume_increase_factor * 0.8
      pv_la_volume_end <- base_pv_volume * pv_volume_increase_factor * 1.2
      pv_highlight_color <- "#f1c40f" # Yellow
    } else { # Ejection
      pv_la_pressure_start <- base_pv_pressure * pv_pressure_increase_factor * 1.2
      pv_la_pressure_end <- base_pv_pressure * pv_pressure_increase_factor * 0.8
      pv_la_volume_start <- base_pv_volume * pv_volume_increase_factor * 1.2
      pv_la_volume_end <- base_pv_volume * pv_volume_increase_factor * 0.8
      pv_highlight_color <- "#8e44ad" # Purple
    }
    pv_plot_data <- data.frame(volume = c(pv_la_volume_start, pv_la_volume_end), pressure = c(pv_la_pressure_start, pv_la_pressure_end)) # Corrected: volume and pressure were swapped
    pv_full_loop_volume <- c(base_pv_volume * 0.8, base_pv_volume * 1.2, base_pv_volume * 1.2, base_pv_volume * 0.8, base_pv_volume * 0.8) * pv_volume_increase_factor
    pv_full_loop_pressure <- c(base_pv_pressure * 0.8, base_pv_pressure * 0.8, base_pv_pressure * 1.2, base_pv_pressure * 1.2, base_pv_pressure * 0.8) * pv_pressure_increase_factor
    pv_full_loop_data <- data.frame(volume = pv_full_loop_volume, pressure = pv_full_loop_pressure)
    
    
    # --- Diastolic Filling (E/A Wave) Data ---
    base_e_amplitude <- 100
    base_a_amplitude <- 50
    e_amplitude <- base_e_amplitude * (1 - lv_impairment_factor * 0.7)
    a_amplitude <- base_a_amplitude * (1 + lv_impairment_factor * 1.5)
    e_amplitude <- max(10, e_amplitude)
    a_amplitude <- max(10, a_amplitude)
    e_wave <- dnorm(time_points, mean = 0.2, sd = 0.05) * e_amplitude
    a_wave <- dnorm(time_points, mean = 0.7, sd = 0.05) * a_amplitude
    diastolic_plot_data <- data.frame(time = time_points, flow_velocity = e_wave + a_wave)
    diastolic_highlight_start_e <- 0.1
    diastolic_highlight_end_e <- 0.3
    diastolic_highlight_start_a <- 0.6
    diastolic_highlight_end_a <- 0.8
    diastolic_highlight_color_e <- "#f1c40f"
    diastolic_highlight_color_a <- "#8e44ad"
    
    
    # --- LA Pressure Curve (A, C, V Waves) Data ---
    base_la_pressure_min <- 5
    base_la_pressure_max <- 15
    la_pressure_a_wave_amp <- base_la_pressure_max * (1 + lv_impairment_factor * 0.8)
    la_pressure_c_wave_amp <- base_la_pressure_min * 1.1
    la_pressure_v_wave_amp <- base_la_pressure_max * (1 + lv_impairment_factor * 0.6)
    
    la_pressure_curve <- rep(base_la_pressure_min, length(time_points))
    la_pressure_curve <- la_pressure_curve + dnorm(time_points, mean = 0.1, sd = 0.03) * la_pressure_a_wave_amp
    la_pressure_curve <- la_pressure_curve + dnorm(time_points, mean = 0.3, sd = 0.02) * la_pressure_c_wave_amp
    la_pressure_curve <- la_pressure_curve + dnorm(time_points, mean = 0.7, sd = 0.05) * la_pressure_v_wave_amp
    la_pressure_plot_data <- data.frame(time = time_points, pressure = la_pressure_curve)
    
    la_pressure_highlight_start <- 0
    la_pressure_highlight_end <- 1
    la_pressure_highlight_color <- "lightgray"
    if (la_phase == "Filling") {
      la_pressure_highlight_start <- 0.5
      la_pressure_highlight_end <- 0.9
      la_pressure_highlight_color <- "#f1c40f"
    } else { # Ejection
      la_pressure_highlight_start <- 0.05
      la_pressure_highlight_end <- 0.15
      la_pressure_highlight_color <- "#8e44ad"
    }
    
    # --- LA Volume Curve Data ---
    la_volume_min_volume <- 30
    la_volume_max_volume <- 70
    la_volume_current_min_volume <- la_volume_min_volume * (1 + lv_impairment_factor * 0.5)
    la_volume_current_max_volume <- la_volume_max_volume * (1 + lv_impairment_factor * 0.5)
    
    la_volume_curve <- rep(la_volume_current_min_volume, length(time_points))
    la_volume_curve[time_points >= 0.4 & time_points <= 0.9] <- seq(la_volume_current_min_volume, la_volume_current_max_volume, length.out = sum(time_points >= 0.4 & time_points <= 0.9))
    la_volume_curve[time_points >= 0 & time_points <= 0.2] <- seq(la_volume_current_max_volume, la_volume_current_min_volume, length.out = sum(time_points >= 0 & time_points <= 0.2))
    la_volume_plot_data <- data.frame(time = time_points, volume = la_volume_curve)
    
    la_volume_highlight_start <- 0
    la_volume_highlight_end <- 1
    la_volume_highlight_color <- "lightgray"
    if (la_phase == "Filling") {
      la_volume_highlight_start <- 0.4
      la_volume_highlight_end <- 0.9
      la_volume_highlight_color <- "#f1c40f"
    } else { # Ejection
      la_volume_highlight_start <- 0
      la_volume_highlight_end <- 0.2
      la_volume_highlight_color <- "#8e44ad"
    }
    
    
    # --- Render selected chart ---
    if (chart_type == "LA Pressure-Volume Loop") {
      ggplot(pv_full_loop_data, aes(x = volume, y = pressure)) +
        geom_rect(aes(xmin = min(pv_plot_data$volume), xmax = max(pv_plot_data$volume),
                      ymin = min(pv_plot_data$pressure), ymax = max(pv_plot_data$pressure)),
                  fill = pv_highlight_color, alpha = 0.2) +
        geom_polygon(fill = "lightgray", alpha = 0.3) +
        geom_path(color = "darkgray", linewidth = 1) +
        geom_line(data = pv_plot_data, aes(x = volume, y = pressure), color = pv_highlight_color, linewidth = 2) +
        geom_point(data = pv_plot_data, aes(x = volume, y = pressure), color = pv_highlight_color, size = 4) +
        labs(title = paste0("Left Atrial Pressure-Volume Loop (LV Function: ", input$lv_function_slider, "%)"),
             x = "Left Atrial Volume (mL)", y = "Left Atrial Pressure (mmHg)") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
          axis.title = element_text(size = 12, color = "#34495e"),
          axis.text = element_text(size = 10, color = "#7f8c8d"),
          panel.grid.major = element_line(color = "#bdc3c7", linetype = "dotted"),
          panel.grid.minor = element_blank()
        ) +
        coord_cartesian(xlim = c(min(pv_full_loop_volume) * 0.9, max(pv_full_loop_volume) * 1.1),
                        ylim = c(min(pv_full_loop_pressure) * 0.9, max(pv_full_loop_pressure) * 1.1))
    } else if (chart_type == "Diastolic Filling (E/A Wave)") {
      ggplot(diastolic_plot_data, aes(x = time, y = flow_velocity)) +
        {
          if (la_phase == "Filling") {
            list(
              geom_rect(aes(xmin = diastolic_highlight_start_e, xmax = diastolic_highlight_end_e, ymin = -Inf, ymax = Inf),
                        fill = diastolic_highlight_color_e, alpha = 0.2)
            )
          } else if (la_phase == "Ejection") {
            list(
              geom_rect(aes(xmin = diastolic_highlight_start_a, xmax = diastolic_highlight_end_a, ymin = -Inf, ymax = Inf),
                        fill = diastolic_highlight_color_a, alpha = 0.2)
            )
          }
        } +
        geom_line(color = "#3498db", linewidth = 1.2) +
        geom_area(fill = "#3498db", alpha = 0.3) +
        geom_point(data = data.frame(time = time_points[which.max(e_wave)], flow_velocity = e_wave[which.max(e_wave)]), aes(x = time, y = flow_velocity), color = "darkblue", size = 3) +
        annotate("text", x = time_points[which.max(e_wave)],
                 y = e_wave[which.max(e_wave)] + 5,
                 label = paste0("E (", round(e_amplitude, 1), ")"), color = "darkblue", size = 4) +
        geom_point(data = data.frame(time = time_points[which.max(a_wave)], flow_velocity = a_wave[which.max(a_wave)]), aes(x = time, y = flow_velocity), color = "darkred", size = 3) +
        annotate("text", x = time_points[which.max(a_wave)],
                 y = a_wave[which.max(a_wave)] + 5,
                 label = paste0("A (", round(a_amplitude, 1), ")"), color = "darkred", size = 4) +
        labs(title = paste0("Diastolic Filling (E/A Wave) - E/A Ratio: ", round(e_amplitude / a_amplitude, 2)),
             x = "Time during Diastole", y = "Flow Velocity (conceptual)") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
          axis.title = element_text(size = 12, color = "#34495e"),
          axis.text = element_text(size = 10, color = "#7f8c8d"),
          panel.grid.major = element_line(color = "#bdc3c7", linetype = "dotted"),
          panel.grid.minor = element_blank()
        ) +
        coord_cartesian(ylim = c(0, max(c(e_amplitude, a_amplitude)) * 1.2))
    } else if (chart_type == "LA Pressure Curve (A, C, V Waves)") {
      ggplot(la_pressure_plot_data, aes(x = time, y = pressure)) +
        geom_rect(aes(xmin = la_pressure_highlight_start, xmax = la_pressure_highlight_end, ymin = -Inf, ymax = Inf),
                  fill = la_pressure_highlight_color, alpha = 0.2) +
        geom_line(color = "#2ecc71", linewidth = 1.2) +
        annotate("text", x = 0.1, y = max(la_pressure_curve) * 0.9, label = "A", color = "black", size = 4) +
        annotate("text", x = 0.3, y = max(la_pressure_curve) * 0.6, label = "C", color = "black", size = 4) +
        annotate("text", x = 0.7, y = max(la_pressure_curve) * 0.9, label = "V", color = "black", size = 4) +
        labs(title = paste0("Left Atrial Pressure Curve (LV Function: ", input$lv_function_slider, "%)"),
             x = "Time (conceptual)", y = "LA Pressure (mmHg)") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
          axis.title = element_text(size = 12, color = "#34495e"),
          axis.text = element_text(size = 10, color = "#7f8c8d"),
          panel.grid.major = element_line(color = "#bdc3c7", linetype = "dotted"),
          panel.grid.minor = element_blank()
        ) +
        coord_cartesian(ylim = c(0, max(la_pressure_curve) * 1.2))
    } else if (chart_type == "LA Volume Curve") {
      ggplot(la_volume_plot_data, aes(x = time, y = volume)) +
        geom_rect(aes(xmin = la_volume_highlight_start, xmax = la_volume_highlight_end, ymin = -Inf, ymax = Inf),
                  fill = la_volume_highlight_color, alpha = 0.2) +
        geom_line(color = "#9b59b6", linewidth = 1.2) +
        labs(title = paste0("Left Atrial Volume Curve (LV Function: ", input$lv_function_slider, "%)"),
             x = "Time (conceptual)", y = "LA Volume (mL)") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
          axis.title = element_text(size = 12, color = "#34495e"),
          axis.text = element_text(size = 10, color = "#7f8c8d"),
          panel.grid.major = element_line(color = "#bdc3c7", linetype = "dotted"),
          panel.grid.minor = element_blank()
        ) +
        coord_cartesian(ylim = c(min(la_volume_curve) * 0.9, max(la_volume_curve) * 1.1))
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
