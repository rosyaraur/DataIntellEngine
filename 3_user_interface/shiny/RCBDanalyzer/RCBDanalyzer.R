# ==============================================================================
# INTERACTIVE RCBD DASHBOARD
# Includes: ANOVA, Post-Hoc, Regression, Visualizations, and Power Analysis
# ==============================================================================
# Install missing packages if necessary
# install.packages(c("shiny", "ggplot2", "agricolae", "multcomp", "lmPerm", "dplyr", "DT"))

# Install missing packages if necessary
# install.packages(c("shiny", "ggplot2", "agricolae", "multcomp", "lmPerm", "dplyr", "DT"))

library(shiny)
library(ggplot2)
library(agricolae)
library(multcomp)
library(lmPerm)
library(dplyr)
library(DT)

# ------------------------------------------------------------------------------
# 1. DEFAULT DATASET (Fallback if no file is uploaded)
# ------------------------------------------------------------------------------
rice_data <- data.frame(
  Treatment = rep(c(25, 50, 75, 100, 125, 150), each = 4),
  Rep = rep(c("I", "II", "III", "IV"), times = 6),
  Yield = c(
    3113, 3398, 3307, 3678,  
    5346, 5952, 4719, 4264,  
    5272, 5713, 5483, 4749,  
    5164, 4831, 4986, 4410,  
    4804, 4848, 4432, 4748,  
    6254, 6542, 6919, 6098   
  )
)

# ------------------------------------------------------------------------------
# 2. USER INTERFACE (UI)
# ------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("RCBD Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Data Input"),
      fileInput("file_upload", "Upload CSV File (Optional):", 
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      helpText("If no file is uploaded, the app uses the default Rice Yield dataset."),
      
      hr(),
      h4("2. Variable Mapping"),
      selectInput("treatment_col", "Treatment Column:", choices = NULL),
      selectInput("block_col", "Block (Rep) Column:", choices = NULL),
      selectInput("response_col", "Response Variable:", choices = NULL),
      
      hr(),
      h4("3. Analysis Settings"),
      selectInput("control_val", "Control Value (for Dunnett's):", choices = NULL),
      
      checkboxInput("is_quant", "Treatments are Quantitative?", value = TRUE),
      helpText("Check this box to enable Regression Analysis and scatter plots. The treatment variable will be treated as a continuous numeric value."),
      
      hr(),
      h4("4. Simulation Settings"),
      numericInput("n_sims", "Permutation Simulations:", value = 100, min = 10, max = 2000),
      actionButton("run_power", "Run Power Analysis", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("1. ANOVA & Post-Hoc", 
                 br(),
                 downloadButton("dl_anova", "Download ANOVA Table (.csv)", class = "btn-success"),
                 h3("ANOVA Table"), verbatimTextOutput("anova_out"),
                 
                 hr(),
                 downloadButton("dl_tukey", "Download Tukey Results (.csv)", class = "btn-success"),
                 h3("Tukey HSD"), verbatimTextOutput("tukey_out"),
                 
                 hr(),
                 h3("Dunnett's Test (vs Control)"), verbatimTextOutput("dunnett_out")
        ),
        tabPanel("2. Regression Analysis",
                 h3("Linear Regression Model"), 
                 helpText("Models the response as a function of blocks and the numeric treatment trend."),
                 verbatimTextOutput("reg_summary"),
                 h3("95% Confidence Intervals for Terms"),
                 verbatimTextOutput("reg_confint")
        ),
        tabPanel("3. Visualizations",
                 br(),
                 downloadButton("dl_barplot", "Save Bar Plot (.png)", class = "btn-info"),
                 h3("Treatment Means (Tukey Letters)"), plotOutput("bar_plot", height = "400px"),
                 
                 hr(),
                 downloadButton("dl_boxplot", "Save Box Plot (.png)", class = "btn-info"),
                 h3("Distribution by Treatment"), plotOutput("box_plot", height = "400px"),
                 
                 hr(),
                 downloadButton("dl_scatterplot", "Save Scatter Plot (.png)", class = "btn-info"),
                 h3("Regression Trendline"), plotOutput("scatter_plot", height = "400px"),
                 
                 hr(),
                 h3("Residual Diagnostics"), plotOutput("resid_plot", height = "400px")
        ),
        tabPanel("4. Power Analysis",
                 h3("Permutation ANOVA & Power"),
                 verbatimTextOutput("perm_power_out")
        ),
        tabPanel("Data Explorer", 
                 br(),
                 downloadButton("dl_data", "Download Full Dataset (.csv)", class = "btn-success"),
                 hr(),
                 DTOutput("data_table")
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 3. SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- REACTIVE DATA INGESTION ---
  raw_data <- reactive({
    inFile <- input$file_upload
    if (is.null(inFile)) return(rice_data)
    read.csv(inFile$datapath)
  })
  
  # --- DYNAMIC UI OBSERVERS ---
  observe({
    df <- raw_data()
    cols <- names(df)
    def_trt <- if("Treatment" %in% cols) "Treatment" else cols[1]
    def_blk <- if("Rep" %in% cols) "Rep" else cols[2]
    def_rsp <- if("Yield" %in% cols) "Yield" else cols[3]
    
    updateSelectInput(session, "treatment_col", choices = cols, selected = def_trt)
    updateSelectInput(session, "block_col", choices = cols, selected = def_blk)
    updateSelectInput(session, "response_col", choices = cols, selected = def_rsp)
  })
  
  observe({
    df <- raw_data()
    req(input$treatment_col %in% names(df))
    trt_levels <- unique(as.character(df[[input$treatment_col]]))
    def_ctrl <- if("25" %in% trt_levels) "25" else trt_levels[1]
    updateSelectInput(session, "control_val", choices = trt_levels, selected = def_ctrl)
  })
  
  prep_data <- reactive({
    df <- raw_data()
    req(input$treatment_col %in% names(df), input$block_col %in% names(df), input$response_col %in% names(df))
    df[[input$treatment_col]] <- as.factor(df[[input$treatment_col]])
    df[[input$block_col]] <- as.factor(df[[input$block_col]])
    return(df)
  })
  
  # --- MODELING ---
  anova_model <- reactive({
    df <- prep_data()
    form <- paste(input$response_col, "~", input$block_col, "+", input$treatment_col)
    aov(as.formula(form), data = df)
  })
  
  tukey_res <- reactive({
    HSD.test(anova_model(), input$treatment_col, group = TRUE)
  })
  
  reg_model <- reactive({
    req(input$is_quant)
    df <- raw_data() 
    req(input$treatment_col %in% names(df))
    df[[input$treatment_col]] <- as.numeric(as.character(df[[input$treatment_col]]))
    df[[input$block_col]] <- as.factor(df[[input$block_col]])
    form <- paste(input$response_col, "~", input$block_col, "+", input$treatment_col)
    lm(as.formula(form), data = df)
  })
  
  # --- TAB 1: TEXT OUTPUTS ---
  output$anova_out <- renderPrint({ summary(anova_model()) })
  output$tukey_out <- renderPrint({ tukey_res()$groups })
  output$dunnett_out <- renderPrint({
    df <- prep_data()
    req(input$control_val %in% levels(df[[input$treatment_col]]))
    df[[input$treatment_col]] <- relevel(df[[input$treatment_col]], ref = input$control_val)
    form <- paste(input$response_col, "~", input$block_col, "+", input$treatment_col)
    model_dunnett <- aov(as.formula(form), data = df)
    mcp_args <- setNames(list("Dunnett"), input$treatment_col)
    dunnett_test <- glht(model_dunnett, linfct = do.call(mcp, mcp_args))
    summary(dunnett_test)
  })
  
  # --- REACTIVE PLOT OBJECTS (Needed for downloading) ---
  bar_plot_obj <- reactive({
    df <- prep_data()
    t_res <- tukey_res()
    plot_data <- data.frame(Treatment = rownames(t_res$groups), Mean = t_res$groups[[input$response_col]], Groups = trimws(t_res$groups$groups))
    se_data <- df %>% group_by(!!sym(input$treatment_col)) %>% summarize(SE = sd(!!sym(input$response_col)) / sqrt(n()), .groups = 'drop')
    plot_data <- merge(plot_data, se_data, by.x = "Treatment", by.y = input$treatment_col)
    plot_data$Treatment <- factor(plot_data$Treatment, levels = unique(as.character(df[[input$treatment_col]])))
    
    ggplot(plot_data, aes(x = Treatment, y = Mean, fill = Treatment)) +
      geom_bar(stat = "identity", color = "black", width = 0.7) +
      geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
      geom_text(aes(label = Groups, y = Mean + SE + (max(Mean)*0.05)), size = 5, fontface = "bold") +
      labs(title = "Treatment Means with 1 Standard Error", subtitle = "Means sharing a letter are not significantly different") +
      theme_minimal() + theme(legend.position = "none", plot.title = element_text(face="bold"))
  })
  
  box_plot_obj <- reactive({
    ggplot(prep_data(), aes_string(x = input$treatment_col, y = input$response_col, fill = input$treatment_col)) +
      geom_boxplot(alpha = 0.7) + geom_jitter(width = 0.1, color = "black") +
      labs(title = "Boxplot of Treatment Distributions") +
      theme_minimal() + theme(legend.position = "none", plot.title = element_text(face="bold"))
  })
  
  scatter_plot_obj <- reactive({
    req(input$is_quant)
    df <- raw_data()
    df[[input$treatment_col]] <- as.numeric(as.character(df[[input$treatment_col]]))
    ggplot(df, aes_string(x = input$treatment_col, y = input$response_col)) +
      geom_point(size = 3, aes_string(color = input$block_col)) +
      geom_smooth(method = "lm", color = "black", linetype = "dashed") +
      labs(title = "Regression Trendline", subtitle = "Dashed line represents linear fit across treatments") +
      theme_minimal() + theme(plot.title = element_text(face="bold"))
  })
  
  # --- RENDER PLOTS ---
  output$bar_plot <- renderPlot({ bar_plot_obj() })
  output$box_plot <- renderPlot({ box_plot_obj() })
  output$scatter_plot <- renderPlot({ scatter_plot_obj() })
  output$resid_plot <- renderPlot({ par(mfrow = c(1, 2)); plot(anova_model(), which = 1:2) })
  
  # --- TAB 2 & 4 OUTPUTS ---
  output$reg_summary <- renderPrint({ if (!input$is_quant) return("Check 'Quantitative' in sidebar."); summary(reg_model()) })
  output$reg_confint <- renderPrint({ if (!input$is_quant) return("Check 'Quantitative' in sidebar."); confint(reg_model()) })
  
  power_results <- eventReactive(input$run_power, {
    withProgress(message = 'Running...', value = 0, {
      df <- prep_data()
      form <- paste(input$response_col, "~", input$block_col, "+", input$treatment_col)
      model <- anova_model()
      fitted_vals <- fitted(model)
      obs_residuals <- residuals(model)
      significant_count <- 0
      n_sims <- input$n_sims
      
      for (i in 1:n_sims) {
        incProgress(1/n_sims)
        sim_data <- df
        sim_data[[input$response_col]] <- fitted_vals + sample(obs_residuals, replace = TRUE)
        capture.output({
          sim_perm_model <- lmp(as.formula(form), data = sim_data, perm = "Prob", seqs = FALSE)
          coef_table <- summary(sim_perm_model)$coefficients
          p_val <- tryCatch(coef_table[grep(input$treatment_col, rownames(coef_table))[1], grep("Pr\\(Prob\\)", colnames(coef_table))[1]], error = function(e) NA)
        })
        if (is.numeric(p_val) && length(p_val) == 1 && !is.na(p_val) && p_val < 0.05) significant_count <- significant_count + 1
      }
      list(power = (significant_count / n_sims) * 100, sims = n_sims)
    })
  })
  
  output$perm_power_out <- renderPrint({
    res <- power_results()
    cat("====================================================\n")
    cat(sprintf("Estimated Permutation Power: %.2f%%\n", res$power))
    cat(sprintf("Based on %d simulations.\n", res$sims))
    cat("====================================================\n")
  })
  
  output$data_table <- renderDT({ datatable(raw_data()) })
  
  # ============================================================================
  # DOWNLOAD HANDLERS
  # ============================================================================
  
  output$dl_anova <- downloadHandler(
    filename = function() { paste0("ANOVA_Results_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(as.data.frame(summary(anova_model())[[1]]), file, row.names = TRUE) }
  )
  
  output$dl_tukey <- downloadHandler(
    filename = function() { paste0("Tukey_HSD_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(tukey_res()$groups, file, row.names = TRUE) }
  )
  
  output$dl_data <- downloadHandler(
    filename = function() { paste0("Raw_Data_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(raw_data(), file, row.names = FALSE) }
  )
  
  output$dl_barplot <- downloadHandler(
    filename = function() { paste0("BarPlot_Means_", Sys.Date(), ".png") },
    content = function(file) { ggsave(file, plot = bar_plot_obj(), width = 8, height = 6, dpi = 300, bg = "white") }
  )
  
  output$dl_boxplot <- downloadHandler(
    filename = function() { paste0("BoxPlot_", Sys.Date(), ".png") },
    content = function(file) { ggsave(file, plot = box_plot_obj(), width = 8, height = 6, dpi = 300, bg = "white") }
  )
  
  output$dl_scatterplot <- downloadHandler(
    filename = function() { paste0("ScatterPlot_Regression_", Sys.Date(), ".png") },
    content = function(file) { ggsave(file, plot = scatter_plot_obj(), width = 8, height = 6, dpi = 300, bg = "white") }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
