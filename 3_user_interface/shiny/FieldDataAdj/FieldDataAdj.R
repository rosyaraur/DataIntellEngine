# ==============================================================================
# 0. INSTALL AND LOAD REQUIRED PACKAGES
# ==============================================================================
# install.packages(c("shiny", "emmeans", "dplyr", "tidyr", "ggplot2", "gstat", "sp", "plotly"))

library(shiny)
library(emmeans)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gstat)
library(sp)
library(plotly)

# ==============================================================================
# 1. THE UNIVERSAL ANCOVA FUNCTION
# ==============================================================================
analyze_spatial_ancova <- function(data, response, treatment, 
                                   numeric_covariate = NULL,
                                   block = NULL, row = NULL, col = NULL) {
  
  # A. Convert categorical variables to factors
  vars_to_factor <- c(treatment, block, row, col)
  vars_to_factor <- vars_to_factor[!sapply(vars_to_factor, is.null)]
  for (v in vars_to_factor) data[[v]] <- as.factor(data[[v]])
  
  # B. Build Formula & Fit Model
  model_terms <- c(block, row, col, numeric_covariate, treatment)
  model_terms <- model_terms[!sapply(model_terms, is.null)]
  formula_str <- paste(response, "~", paste(model_terms, collapse = " + "))
  
  model <- aov(as.formula(formula_str), data = data)
  
  # C. Calculate Adjusted Group Means
  adj_means <- emmeans(model, specs = treatment)
  emms_df <- as.data.frame(adj_means)
  
  # D. Calculate Individual Adjusted Values
  data$Fitted_Value <- fitted(model)
  data$Treatment_EMM <- emms_df$emmean[match(data[[treatment]], emms_df[[treatment]])]
  adj_col_name <- paste0(response, "_Adjusted")
  
  # Universal adjustment formula
  data[[adj_col_name]] <- data[[response]] - (data$Fitted_Value - data$Treatment_EMM)
  
  # E. Generate Diagnostic Plots
  plot_list <- list()
  
  # PLOT 1: Raw vs Adjusted Scatter Plot (NEW)
  plot_list$adj_vs_raw <- ggplot(data, aes_string(x = response, y = adj_col_name, color = treatment)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = 1) +
    geom_point(size = 4, alpha = 0.8) +
    theme_classic(base_size = 14) +
    labs(title = "Raw vs. Adjusted Observations",
         subtitle = "Dashed line is 1:1. Points above line were adjusted UP; points below were adjusted DOWN.",
         x = paste("Raw", response), 
         y = paste("Adjusted", response))
  
  # PLOT 2: 2D ANCOVA Regression
  if (!is.null(numeric_covariate)) {
    plot_list$ancova_plot <- ggplot(data, aes_string(x = numeric_covariate, y = response, color = treatment)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      theme_classic(base_size = 14) +
      labs(title = "ANCOVA: Response vs Covariate",
           subtitle = "Parallel slopes indicate the covariate effect being removed",
           x = numeric_covariate, y = paste("Raw", response))
  }
  
  # PLOT 3 & 4: Spatial Diagnostics
  if (!is.null(row) && !is.null(col)) {
    
    # Setup coordinates for mapping
    data$X_num <- as.numeric(as.character(data[[col]]))
    data$Y_num <- as.numeric(as.character(data[[row]]))
    
    # PLOT 3: Spatial Variogram
    sp_data <- data
    sp_data$Residuals <- resid(model)
    coordinates(sp_data) <- ~ X_num + Y_num
    
    v_model <- suppressWarnings(variogram(Residuals ~ 1, data = sp_data, cutoff = 6, width = 1))
    
    plot_list$variogram_plot <- ggplot(v_model, aes(x = dist, y = gamma)) +
      geom_point(size = 4, color = "#4C72B0") +
      geom_smooth(method = "loess", se = FALSE, color = "darkred", linetype = "dashed", formula = 'y ~ x') +
      theme_classic(base_size = 14) + expand_limits(y = 0) +
      labs(title = "Spatial Variogram of Model Residuals",
           subtitle = "A flat line indicates spatial gradients were modeled out successfully",
           x = "Distance between plots", y = "Semivariance")
    
    # PLOT 4: 3D Surface Plotly (Using tapply fix)
    mat_raw <- tapply(data[[response]], list(data[[row]], data[[col]]), FUN = mean, na.rm = TRUE)
    mat_adj <- tapply(data[[adj_col_name]], list(data[[row]], data[[col]]), FUN = mean, na.rm = TRUE)
    
    plot_list$surface_3d <- plot_ly() %>%
      add_surface(z = ~mat_raw, opacity = 0.5, colorscale = "Viridis", name = "Raw") %>%
      add_surface(z = ~mat_adj, type = "surface", hidesurface = TRUE, 
                  contours = list(x = list(show = TRUE), y = list(show = TRUE), z = list(show = FALSE)),
                  colorscale = "Reds", name = "Adjusted") %>%
      layout(title = "3D Spatial Surface: Raw (Solid) vs Adjusted (Wireframe)",
             scene = list(xaxis = list(title = col), yaxis = list(title = row), zaxis = list(title = response)))    
    
    # Cleanup spatial temp columns
    data <- data[, !(names(data) %in% c("X_num", "Y_num"))]
  }
  
  # Cleanup general temp columns
  data <- data[, !(names(data) %in% c("Fitted_Value", "Treatment_EMM"))]
  
  return(invisible(list(model = model, anova = summary(model), 
                        adjusted_means = adj_means, adjusted_data = data, plots = plot_list)))
}

# ==============================================================================
# 2. BUILT-IN DATASETS
# ==============================================================================
# Dataset 1: Lima Beans (Non-Spatial RCBD)
df_lima <- data.frame(
  Variety = rep(c("1", "2"), times = 5),
  Replication = rep(1:5, each = 2),
  X_DryMatter = c(34.0, 39.6, 33.4, 39.8, 34.7, 51.2, 38.9, 52.0, 36.1, 56.2),
  Y_Ascorbic  = c(93.0, 47.3, 94.8, 51.5, 91.7, 33.3, 80.8, 27.2, 80.2, 20.6)
)

# Dataset 2: 5x5 Spatial Field Trial
set.seed(42) 
df_lattice <- data.frame(
  Row = rep(1:5, times = 5),
  Column = rep(1:5, each = 5),
  Variety = c("A","B","C","D","E", "B","C","D","E","A", "C","D","E","A","B", "D","E","A","B","C", "E","A","B","C","D"),
  Nematodes = round(runif(25, 50, 150))
)
true_eff <- c(A=100, B=115, C=90, D=130, E=105)
row_eff <- c(-10, -5, 0, 5, 10)
col_eff <- c(15, 8, 0, -8, -15)
df_lattice$Yield <- round(true_eff[df_lattice$Variety] + row_eff[df_lattice$Row] + 
                            col_eff[df_lattice$Column] + (df_lattice$Nematodes * -0.4) + 
                            rnorm(25, 0, 4), 1)

# ==============================================================================
# 3. SHINY UI
# ==============================================================================
ui <- fluidPage(
  titlePanel("Advanced Spatial ANCOVA Analyzer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Data Source"),
      radioButtons("data_source", "Select Dataset:",
                   choices = c("Example 1: Lima Beans (RCBD)" = "ex1",
                               "Example 2: 5x5 Spatial Field" = "ex2",
                               "Upload Custom CSV" = "upload")),
      
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("file1", "Choose CSV File", accept = c(".csv"))
      ),
      
      h4("2. Model Variables"),
      uiOutput("var_selectors"),
      
      br(),
      actionButton("run", "Run Analysis", class = "btn-success btn-lg", width = "100%")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("1. ANOVA & Adjusted Means", 
                 br(),
                 downloadButton("dl_report", "Download Text Report", class = "btn-primary"),
                 hr(),
                 h4("Analysis of Variance (ANCOVA) Table"),
                 verbatimTextOutput("anovaOutput"),
                 h4("Adjusted Estimated Marginal Means (EMMs)"),
                 verbatimTextOutput("emmeansOutput")
        ),
        tabPanel("2. Raw vs Adjusted Plot", 
                 br(),
                 downloadButton("dl_raw_adj", "Download Plot", class = "btn-primary"),
                 hr(),
                 plotOutput("rawAdjPlot", height = "500px")
        ),
        tabPanel("3. ANCOVA Covariate Plot", 
                 br(),
                 downloadButton("dl_ancova", "Download Plot", class = "btn-primary"),
                 hr(),
                 plotOutput("ancovaPlot", height = "500px")
        ),
        tabPanel("4. Spatial Variogram", 
                 br(),
                 downloadButton("dl_variogram", "Download Plot", class = "btn-primary"),
                 hr(),
                 plotOutput("variogramPlot", height = "500px")
        ),
        tabPanel("5. 3D Spatial Surface", 
                 br(),
                 p(em("Use the camera icon in the top right corner of the plot to download as PNG.")),
                 hr(),
                 plotlyOutput("surfacePlot", height = "600px")
        ),
        tabPanel("6. Adjusted Data Preview", 
                 br(),
                 downloadButton("dl_data", "Download Adjusted CSV", class = "btn-primary"),
                 hr(),
                 dataTableOutput("dataPreview")
        )
      )
    )
  )
)

# ==============================================================================
# 4. SHINY SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  # Reactive data loader
  current_data <- reactive({
    if (input$data_source == "ex1") return(df_lima)
    if (input$data_source == "ex2") return(df_lattice)
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  
  # Dynamic UI Selectors
  output$var_selectors <- renderUI({
    req(current_data())
    cols <- names(current_data())
    
    def_resp <- if("Y_Ascorbic" %in% cols) "Y_Ascorbic" else if("Yield" %in% cols) "Yield" else cols[1]
    def_trt  <- if("Variety" %in% cols) "Variety" else cols[2]
    def_cov  <- if("X_DryMatter" %in% cols) "X_DryMatter" else if("Nematodes" %in% cols) "Nematodes" else "None"
    def_blk  <- if("Replication" %in% cols) "Replication" else "None"
    def_row  <- if("Row" %in% cols) "Row" else "None"
    def_col  <- if("Column" %in% cols) "Column" else "None"
    
    tagList(
      selectInput("response", "Response Variable (Y):", choices = cols, selected = def_resp),
      selectInput("treatment", "Treatment Factor:", choices = cols, selected = def_trt),
      selectInput("covariate", "Numeric Covariate (X):", choices = c("None", cols), selected = def_cov),
      selectInput("block", "Block Factor (Optional):", choices = c("None", cols), selected = def_blk),
      selectInput("row", "Row Factor (For Spatial):", choices = c("None", cols), selected = def_row),
      selectInput("col", "Column Factor (For Spatial):", choices = c("None", cols), selected = def_col)
    )
  })
  
  # Run the Analysis
  analysis_res <- eventReactive(input$run, {
    req(input$response, input$treatment)
    d <- current_data()
    
    cov_val <- if (input$covariate != "None") input$covariate else NULL
    blk_val <- if (input$block != "None") input$block else NULL
    row_val <- if (input$row != "None") input$row else NULL
    col_val <- if (input$col != "None") input$col else NULL
    
    res <- analyze_spatial_ancova(
      data = d, response = input$response, treatment = input$treatment,
      numeric_covariate = cov_val, block = blk_val, row = row_val, col = col_val
    )
    return(res)
  })
  
  # --- RENDER TEXT AND DATA ---
  output$anovaOutput <- renderPrint({ req(analysis_res()); print(analysis_res()$anova) })
  output$emmeansOutput <- renderPrint({ req(analysis_res()); print(analysis_res()$adjusted_means) })
  output$dataPreview <- renderDataTable({ req(analysis_res()); analysis_res()$adjusted_data }, options = list(pageLength = 10, scrollX = TRUE))
  
  # --- RENDER PLOTS ---
  output$rawAdjPlot <- renderPlot({
    req(analysis_res())
    analysis_res()$plots$adj_vs_raw
  })
  
  output$ancovaPlot <- renderPlot({
    req(analysis_res())
    if (is.null(analysis_res()$plots$ancova_plot)) {
      plot.new(); title(main = "No Numeric Covariate selected for regression plot.")
      return()
    }
    analysis_res()$plots$ancova_plot
  })
  
  output$variogramPlot <- renderPlot({
    req(analysis_res())
    if (is.null(analysis_res()$plots$variogram_plot)) {
      plot.new(); title(main = "Spatial Variogram requires both Row and Column variables.")
      return()
    }
    analysis_res()$plots$variogram_plot
  })
  
  output$surfacePlot <- renderPlotly({
    req(analysis_res())
    if (is.null(analysis_res()$plots$surface_3d)) {
      return(plot_ly() %>% layout(title = "3D Surface requires both Row and Column variables."))
    }
    analysis_res()$plots$surface_3d
  })
  
  # --- DOWNLOAD HANDLERS ---
  
  output$dl_report <- downloadHandler(
    filename = function() { paste0("ANCOVA_Report_", Sys.Date(), ".txt") },
    content = function(file) {
      res <- analysis_res()
      sink(file)
      cat("=== ANCOVA ANALYSIS REPORT ===\n\n")
      print(res$anova)
      cat("\n=== ADJUSTED ESTIMATED MARGINAL MEANS ===\n\n")
      print(res$adjusted_means)
      sink()
    }
  )
  
  output$dl_data <- downloadHandler(
    filename = function() { paste0("Adjusted_Data_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(analysis_res()$adjusted_data, file, row.names = FALSE) }
  )
  
  output$dl_raw_adj <- downloadHandler(
    filename = function() { "Raw_vs_Adjusted_Plot.png" },
    content = function(file) { ggsave(file, plot = analysis_res()$plots$adj_vs_raw, width = 8, height = 6) }
  )
  
  output$dl_ancova <- downloadHandler(
    filename = function() { "ANCOVA_Covariate_Plot.png" },
    content = function(file) {
      req(analysis_res()$plots$ancova_plot)
      ggsave(file, plot = analysis_res()$plots$ancova_plot, width = 8, height = 6) 
    }
  )
  
  output$dl_variogram <- downloadHandler(
    filename = function() { "Spatial_Variogram_Plot.png" },
    content = function(file) {
      req(analysis_res()$plots$variogram_plot)
      ggsave(file, plot = analysis_res()$plots$variogram_plot, width = 8, height = 6) 
    }
  )
}

# ==============================================================================
# 5. RUN APP
# ==============================================================================
shinyApp(ui = ui, server = server)