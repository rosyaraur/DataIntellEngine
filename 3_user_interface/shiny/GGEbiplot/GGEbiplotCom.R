library(shiny)
library(metan)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT) # For a nice interactive data table

# ==========================================
# 1. BUILT-IN PRACTICE DATASET
# ==========================================
# Recreating the Table 4.4 wide dataset and reshaping it to LONG format
gge_data_wide <- data.frame(
  Genotype = c("Ann", "Ari", "Aug", "Cas", "Del", "Dia", "Ena", "Fun", "Ham",
               "Har", "Kar", "Kat", "Luc", "M12", "Reb", "Ron", "Rub", "Zav"),
  BH93 = c(4.460, 4.417, 4.669, 4.732, 4.390, 5.178, 3.375, 4.852, 5.038, 5.195, 4.293, 3.151, 4.104, 3.340, 4.375, 4.940, 3.786, 4.238),
  EA93 = c(4.150, 4.771, 4.578, 4.745, 4.603, 4.475, 4.175, 4.664, 4.741, 4.662, 4.530, 3.040, 3.878, 3.854, 4.701, 4.698, 4.969, 4.654),
  HW93 = c(2.849, 2.912, 3.098, 3.375, 3.511, 2.990, 2.741, 4.425, 3.508, 3.596, 2.760, 2.388, 2.302, 2.419, 3.655, 2.950, 3.379, 3.607),
  ID93 = c(3.084, 3.506, 3.460, 3.904, 3.848, 3.774, 3.157, 3.952, 3.437, 3.759, 3.422, 2.350, 3.718, 2.783, 3.592, 3.898, 3.353, 3.914),
  KE93 = c(5.940, 5.699, 6.070, 6.224, 5.773, 6.583, 5.342, 5.536, 5.960, 5.937, 6.142, 4.229, 4.555, 4.629, 6.189, 6.063, 4.774, 6.641),
  NN93 = c(4.450, 5.152, 5.025, 5.340, 5.421, 5.045, 4.267, 5.832, 4.859, 5.345, 5.250, 4.257, 5.149, 5.090, 5.141, 5.326, 5.304, 4.830),
  OA93 = c(4.351, 4.956, 4.730, 4.226, 5.147, 3.985, 4.162, 4.168, 4.977, 3.895, 4.856, 3.384, 2.596, 3.281, 3.933, 4.302, 4.322, 5.014),
  RN93 = c(4.039, 4.386, 3.900, 4.893, 4.098, 4.271, 4.063, 5.060, 4.514, 4.450, 4.137, 4.071, 4.956, 3.918, 4.208, 4.299, 4.858, 4.363),
  WP93 = c(2.672, 2.938, 2.621, 3.451, 2.832, 2.776, 2.032, 3.574, 2.859, 3.300, 3.149, 2.103, 2.886, 2.561, 2.925, 3.031, 3.382, 3.111)
)

practice_data_long <- gge_data_wide %>%
  pivot_longer(cols = -Genotype, names_to = "Environment", values_to = "Yield")

# ==========================================
# 2. USER INTERFACE (UI)
# ==========================================
ui <- fluidPage(
  
  titlePanel("Interactive Multi-Environment Trial (GGE) Analyzer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Data Source"),
      
      # Toggle between Practice Data and File Upload
      checkboxInput("use_practice", "Use built-in practice dataset (Wheat 1993)", value = TRUE),
      
      # Conditional Panel: Only show if user unchecks the practice dataset box
      conditionalPanel(
        condition = "input.use_practice == false",
        fileInput("file_upload", "Upload Long-Form CSV File",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        
        # Dynamic UI elements that will appear after a file is uploaded
        uiOutput("column_mapping_ui")
      ),
      
      hr(),
      h4("Global Plot Settings"),
      
      radioButtons("svp", "Scaling Method (SVP):",
                   choices = list("Genotype-focused (SVP=1)" = "genotype",
                                  "Environment-focused (SVP=2)" = "environment",
                                  "Symmetrical Scaling (SVP=3)" = "symmetrical"),
                   selected = "symmetrical"),
      
      helpText("Note: SVP primarily affects the basic biplot and the Which-Won-Where polygon.")
    ),
    
    # Main panel with tabs
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "pills",
        tabPanel("Data Preview", 
                 br(), 
                 h4("Current Dataset"),
                 DTOutput("data_table")),
        
        tabPanel("1. Basic Biplot", 
                 br(), 
                 plotOutput("plot_basic", height = "700px")),
        
        tabPanel("2. Performance & Stability", 
                 br(), 
                 plotOutput("plot_ranking", height = "700px")),
        
        tabPanel("3. Polygon (Which-Won-Where)", 
                 br(), 
                 plotOutput("plot_poly", height = "700px")),
        
        tabPanel("4. Rank Environments", 
                 br(), 
                 plotOutput("plot_env", height = "700px"))
      )
    )
  )
)

# ==========================================
# 3. SERVER LOGIC
# ==========================================
server <- function(input, output, session) {
  
  # Reactive variable to read the uploaded CSV
  uploaded_data <- reactive({
    req(input$file_upload)
    read.csv(input$file_upload$datapath)
  })
  
  # Render the dynamic dropdowns for column mapping if a custom file is uploaded
  output$column_mapping_ui <- renderUI({
    req(uploaded_data())
    cols <- names(uploaded_data())
    
    tagList(
      selectInput("col_gen", "Select Genotype Column:", choices = cols, selected = cols[1]),
      selectInput("col_env", "Select Environment Column:", choices = cols, selected = cols[2]),
      selectInput("col_resp", "Select Response/Yield Column:", choices = cols, selected = cols[3])
    )
  })
  
  # Reactive expression that decides which data to use based on the checkbox
  current_data <- reactive({
    if (input$use_practice) {
      return(practice_data_long)
    } else {
      req(uploaded_data())
      # If custom data is uploaded, ensure column names are standardized for the metan package
      req(input$col_gen, input$col_env, input$col_resp)
      
      df <- uploaded_data()
      # Rename the columns to standard names internally to avoid !!sym() complexity in Shiny
      df <- df %>% rename(
        Genotype = !!sym(input$col_gen),
        Environment = !!sym(input$col_env),
        Yield = !!sym(input$col_resp)
      )
      return(df)
    }
  })
  
  # Fit the GGE model reactively whenever the data changes
  fitted_model <- reactive({
    req(current_data())
    
    # Wrap in tryCatch to handle potential SVD errors if data is incomplete or has missing values
    tryCatch({
      gge(current_data(), env = Environment, gen = Genotype, resp = Yield)
    }, error = function(e) {
      validate(need(FALSE, paste("Error fitting model. Please ensure your data is in a long format with no missing values. System error:", e$message)))
    })
  })
  
  # --- Outputs ---
  
  # Render Data Table
  output$data_table <- renderDT({
    req(current_data())
    datatable(current_data(), options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # Helper function to apply styling to plots
  style_plot <- function(p, title) {
    p + labs(title = title, subtitle = paste("Scaling:", tools::toTitleCase(input$svp))) +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, color = "gray40"),
        text = element_text(size = 14)
      )
  }
  
  # Render Plot 1: Basic Biplot
  output$plot_basic <- renderPlot({
    req(fitted_model())
    p <- plot(fitted_model(), type = 1, SVP = input$svp)
    style_plot(p, "Basic GGE Biplot")
  })
  
  # Render Plot 2: Performance & Stability
  output$plot_ranking <- renderPlot({
    req(fitted_model())
    p <- plot(fitted_model(), type = 2) # AEC plot usually relies on default ranking math
    style_plot(p, "Mean Performance and Stability")
  })
  
  # Render Plot 3: Polygon (Which-Won-Where)
  output$plot_poly <- renderPlot({
    req(fitted_model())
    p <- plot(fitted_model(), type = 3, SVP = input$svp)
    style_plot(p, "Which-Won-Where Polygon")
  })
  
  # Render Plot 4: Rank Environments
  output$plot_env <- renderPlot({
    req(fitted_model())
    p <- plot(fitted_model(), type = 4)
    style_plot(p, "Rank Environments")
  })
}

# ==========================================
# 4. LAUNCH APP
# ==========================================
shinyApp(ui = ui, server = server)