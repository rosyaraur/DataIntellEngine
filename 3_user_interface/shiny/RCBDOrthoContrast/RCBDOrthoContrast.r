# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(agricolae)
library(emmeans)
library(rhandsontable)

# ==========================================
# 1. THE ANALYSIS FUNCTION
# ==========================================
analyze_rcbd_contrast <- function(data, response, block, treatments, 
                                  contrast_matrix = NULL, 
                                  post_hoc = c("none", "LSD", "Tukey", "Duncan"),
                                  alpha = 0.05) {
  
  post_hoc <- match.arg(post_hoc)
  
  # Convert variables to factors for ANOVA
  data[[block]] <- as.factor(data[[block]])
  for (trt in treatments) {
    data[[trt]] <- as.factor(data[[trt]])
  }
  
  trt_term <- paste(treatments, collapse = " * ")
  formula_str <- paste(response, "~", block, "+", trt_term)
  
  model <- aov(as.formula(formula_str), data = data)
  results <- list(model = model, anova = summary(model))
  
  # Evaluate Orthogonal Contrasts safely
  if (!is.null(contrast_matrix) && length(contrast_matrix) > 0) {
    emm_term <- treatments[1]
    emm <- emmeans(model, specs = emm_term)
    contrast_res <- suppressMessages(contrast(emm, method = list(Custom = contrast_matrix)))
    results$contrasts <- contrast_res
  }
  
  # Run Post-Hoc Comparison
  if (post_hoc != "none") {
    trt_for_test <- if(length(treatments) > 1) treatments else treatments[1]
    if (post_hoc == "LSD") out_test <- LSD.test(model, trt_for_test, alpha = alpha, console = FALSE)
    if (post_hoc == "Tukey") out_test <- HSD.test(model, trt_for_test, alpha = alpha, console = FALSE)
    if (post_hoc == "Duncan") out_test <- duncan.test(model, trt_for_test, alpha = alpha, console = FALSE)
    results$post_hoc <- out_test
  }
  
  return(results)
}

# ==========================================
# 2. BUILT-IN DATASETS
# ==========================================
df_stand <- data.frame(
  Block = rep(1:6, each = 8),
  Treatment = rep(c("A", "B", "C", "D", "E", "F", "G", "H"), times = 6),
  Stand = c(8, 16, 14, 10, 8, 8, 7, 12,  8, 19, 16, 11, 7, 8, 6, 19,  
            9, 24, 14, 12, 1, 3, 6, 9,   7, 22, 13, 8,  1, 3, 6, 11,  
            7, 19, 14, 7,  3, 3, 4, 9,   5, 19, 13, 3,  2, 7, 4, 5)   
)

df_yield <- data.frame(
  Block = rep(1:6, each = 5),
  Spacing = rep(c(18, 24, 30, 36, 42), times = 6),
  Yield = c(33.6, 31.1, 33.0, 28.4, 31.4, 37.1, 34.5, 29.5, 29.9, 28.3,
            34.1, 30.5, 29.2, 31.6, 28.9, 34.6, 32.7, 30.7, 32.3, 28.6,
            35.4, 30.7, 30.7, 28.1, 29.6, 36.1, 30.3, 27.9, 26.9, 33.4)
)

# ==========================================
# 3. SHINY UI
# ==========================================
ui <- fluidPage(
  titlePanel("RCBD & Orthogonal Contrast Analyzer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Data Source"),
      radioButtons("data_source", "Select Dataset:",
                   choices = c("Example 1: Greenhouse (8 Levels)" = "ex1",
                               "Example 2: Soybeans (Polynomial, 5 Levels)" = "ex2",
                               "Upload Custom CSV" = "upload")),
      
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("file1", "Choose CSV File", accept = c(".csv"))
      ),
      
      h4("2. Model Variables"),
      uiOutput("var_selectors"),
      
      h4("3. Means Comparison"),
      selectInput("post_hoc", "Post-Hoc Test:", 
                  choices = c("none", "LSD", "Tukey", "Duncan"), selected = "LSD"),
      numericInput("alpha", "Alpha Level:", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
      
      # Dynamic UI for Polynomial Regression if Quantitative Treatment
      uiOutput("trend_ui"),
      
      br(),
      actionButton("run", "Run Analysis", class = "btn-success btn-lg", width = "100%")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("1. Contrast Editor", 
                 br(),
                 p("Define your contrasts below. The columns automatically match your selected Treatment variable. ", 
                   strong("Right-click any cell to add/remove rows.")),
                 rHandsontableOutput("contrast_table")
        ),
        tabPanel("2. ANOVA & Results", 
                 br(),
                 downloadButton("downloadReport", "Download Full Text Report", class = "btn-primary"),
                 hr(),
                 h4("Analysis of Variance"), verbatimTextOutput("anovaOutput"),
                 h4("Orthogonal Contrasts"), verbatimTextOutput("contrastOutput"),
                 h4("Post-Hoc Grouping"), verbatimTextOutput("posthocOutput")
        ),
        tabPanel("3. Treatment Plot", 
                 br(),
                 downloadButton("downloadTrtPlot", "Download Plot as PNG", class = "btn-primary"),
                 hr(),
                 plotOutput("barPlot", height = "500px"),
                 p("Letters above bars represent statistical grouping. Error bars represent ±1 Standard Error (SE).")
        ),
        tabPanel("4. Contrast Plot", 
                 br(),
                 downloadButton("downloadConPlot", "Download Plot as PNG", class = "btn-primary"),
                 hr(),
                 plotOutput("contrastPlot", height = "500px"),
                 p("Shows the estimated difference for each contrast. Error bars are ±1 Standard Error. Red dashed line is zero difference.")
        ),
        tabPanel("5. Trend/Regression Plot", 
                 br(),
                 downloadButton("downloadTrendPlot", "Download Plot as PNG", class = "btn-primary"),
                 hr(),
                 plotOutput("trendPlot", height = "500px"),
                 p(strong("Note:"), " This plot is only generated if your Treatment levels are purely numeric (quantitative). It displays individual observations (grey), treatment means (red), and the fitted polynomial regression trend line (blue).")
        ),
        tabPanel("6. Data Preview", dataTableOutput("dataPreview"))
      )
    )
  )
)

# ==========================================
# 4. SHINY SERVER
# ==========================================
server <- function(input, output, session) {
  
  # Data Loader
  current_data <- reactive({
    if (input$data_source == "ex1") return(df_stand)
    if (input$data_source == "ex2") return(df_yield)
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  
  # Dynamic Variables
  output$var_selectors <- renderUI({
    req(current_data())
    cols <- names(current_data())
    tagList(
      selectInput("response", "Response:", choices = cols, selected = if("Stand" %in% cols) "Stand" else if("Yield" %in% cols) "Yield" else cols[1]),
      selectInput("block", "Block:", choices = cols, selected = if("Block" %in% cols) "Block" else cols[2]),
      selectInput("treatment", "Treatment:", choices = cols, selected = if("Treatment" %in% cols) "Treatment" else if("Spacing" %in% cols) "Spacing" else cols[3])
    )
  })
  
  # --- CHECK IF TREATMENT IS QUANTITATIVE ---
  is_quantitative <- reactive({
    req(current_data(), input$treatment)
    trt_vals <- current_data()[[input$treatment]]
    # Check if we can convert all values to numeric without creating NAs
    num_vals <- suppressWarnings(as.numeric(as.character(trt_vals)))
    return(!any(is.na(num_vals)))
  })
  
  output$trend_ui <- renderUI({
    if(is_quantitative()) {
      tagList(
        h4("4. Polynomial Trend Plot"),
        numericInput("poly_deg", "Trend Regression Degree:", value = 1, min = 1, max = 4, step = 1),
        helpText("1 = Linear, 2 = Quadratic, 3 = Cubic")
      )
    }
  })
  
  # --- EDITABLE CONTRAST TABLE LOGIC ---
  contrast_template <- reactive({
    req(current_data(), input$treatment)
    d <- current_data()
    trt_var <- input$treatment
    if (!(trt_var %in% names(d))) return(NULL)
    
    lvls <- levels(as.factor(d[[trt_var]]))
    n_lvls <- length(lvls)
    
    if (input$data_source == "ex1" && n_lvls == 8) {
      df <- data.frame(
        Contrast_Name = c("A vs rest", "BC vs DEFGH", "B vs C", "DH vs EFG", "D vs H", "E vs FG", "F vs G"),
        A=c(-7,0,0,0,0,0,0), B=c(1,5,1,0,0,0,0), C=c(1,5,-1,0,0,0,0), D=c(1,-2,0,3,1,0,0),
        E=c(1,-2,0,-2,0,2,0), F=c(1,-2,0,-2,0,-1,1), G=c(1,-2,0,-2,0,-1,-1), H=c(1,-2,0,3,-1,0,0),
        stringsAsFactors = FALSE
      )
      colnames(df)[2:(n_lvls+1)] <- lvls
      return(df)
      
    } else if (input$data_source == "ex2" && n_lvls == 5) {
      df <- data.frame(
        Contrast_Name = c("Linear", "Quadratic", "Cubic", "Quartic"),
        L1=c(-2,2,-1,1), L2=c(-1,-1,2,-4), L3=c(0,-2,0,6), L4=c(1,-1,-2,-4), L5=c(2,2,1,1),
        stringsAsFactors = FALSE
      )
      colnames(df)[2:(n_lvls+1)] <- lvls
      return(df)
      
    } else {
      df <- data.frame(Contrast_Name = "Custom_1", stringsAsFactors = FALSE)
      for (l in lvls) df[[as.character(l)]] <- 0
      return(df)
    }
  })
  
  output$contrast_table <- renderRHandsontable({
    req(contrast_template())
    rhandsontable(contrast_template(), rowHeaders = TRUE) %>%
      hot_col("Contrast_Name", type = "text") %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })
  
  # --- RUN ANALYSIS ---
  analysis_res <- eventReactive(input$run, {
    req(input$response, input$block, input$treatment)
    d_filtered <- current_data()
    
    my_contrasts <- list()
    if (!is.null(input$contrast_table)) {
      df_contrasts <- hot_to_r(input$contrast_table)
      for (i in 1:nrow(df_contrasts)) {
        c_name <- df_contrasts$Contrast_Name[i]
        c_vals <- as.numeric(df_contrasts[i, -1])
        if (any(c_vals != 0)) my_contrasts[[c_name]] <- c_vals
      }
    }
    
    analyze_rcbd_contrast(d_filtered, input$response, input$block, c(input$treatment), 
                          my_contrasts, input$post_hoc, input$alpha)
  })
  
  # --- TEXT OUTPUTS ---
  output$anovaOutput <- renderPrint({ req(analysis_res()); print(analysis_res()$anova) })
  output$contrastOutput <- renderPrint({ req(analysis_res()); print(analysis_res()$contrasts) })
  output$posthocOutput <- renderPrint({ req(analysis_res()); print(analysis_res()$post_hoc$groups) })
  output$dataPreview <- renderDataTable({ current_data() }, options = list(pageLength = 10))
  
  # --- REACTIVE PLOTS ---
  
  # 1. Bar Plot
  trt_plot_obj <- reactive({
    req(analysis_res())
    if (input$post_hoc == "none") return(ggplot() + ggtitle("Select a Post-Hoc test to view plot") + theme_void())
    
    ph_obj <- analysis_res()$post_hoc
    grp_data <- ph_obj$groups
    mean_data <- ph_obj$means
    trt_names <- rownames(grp_data)
    
    plot_df <- data.frame(
      Treatment = factor(trt_names, levels = sort(trt_names)), 
      Mean = grp_data[, 1], Group = as.character(grp_data$groups)                    
    )
    plot_df$SE <- mean_data[trt_names, "std"] / sqrt(mean_data[trt_names, "r"])
    y_max <- max(plot_df$Mean + plot_df$SE)
    
    ggplot(plot_df, aes(x = Treatment, y = Mean)) +
      geom_bar(stat = "identity", fill = "#4C72B0", color = "black", alpha = 0.8, width = 0.6) +
      geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2, linewidth = 0.8) +
      geom_text(aes(label = Group, y = Mean + SE + (y_max * 0.05)), size = 6, fontface = "bold") +
      theme_classic(base_size = 14) +
      labs(title = paste("Treatment Means for", input$response), x = input$treatment, y = paste("Mean", input$response))
  })
  
  # 2. Contrast Plot
  con_plot_obj <- reactive({
    req(analysis_res())
    if (is.null(analysis_res()$contrasts)) return(ggplot() + ggtitle("No contrasts defined") + theme_void())
    
    c_df <- as.data.frame(analysis_res()$contrasts)
    if(nrow(c_df) == 0) return(ggplot() + ggtitle("Empty contrast matrix") + theme_void())
    
    ggplot(c_df, aes(x = reorder(contrast, estimate), y = estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
      geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), width = 0.2, linewidth = 1) +
      geom_point(size = 4, color = "#4C72B0") +
      coord_flip() + theme_classic(base_size = 14) +
      labs(title = "Estimated Contrast Means ± 1 SE", x = "Defined Contrast", y = "Estimate (Difference)")
  })
  
  # 3. Polynomial Trend Plot
  trend_plot_obj <- reactive({
    req(analysis_res(), is_quantitative())
    
    d_filtered <- current_data()
    resp_var <- input$response
    trt_var <- input$treatment
    deg <- if(!is.null(input$poly_deg)) input$poly_deg else 1
    
    # Convert Treatment to pure numbers for X axis
    d_filtered$x_num <- as.numeric(as.character(d_filtered[[trt_var]]))
    
    deg_name <- c("Linear", "Quadratic", "Cubic", "Quartic")[deg]
    
    ggplot(d_filtered, aes(x = x_num, y = !!sym(resp_var))) +
      # Individual points (jittered slightly to see overlaps)
      geom_point(position = position_jitter(width = 0.3), alpha = 0.4, size = 2) +
      # Treatment Means (Red dots)
      stat_summary(fun = mean, geom = "point", color = "darkred", size = 4) +
      # The fitted polynomial regression line
      geom_smooth(method = "lm", formula = y ~ poly(x, deg), color = "#4C72B0", fill = "lightblue", se = TRUE) +
      theme_classic(base_size = 14) +
      labs(title = paste(deg_name, "Regression Trend for", resp_var),
           subtitle = "Grey dots = Obs, Red dots = Means, Blue line = Fitted Polynomial Regression",
           x = trt_var, y = resp_var)
  })
  
  output$barPlot <- renderPlot({ trt_plot_obj() })
  output$contrastPlot <- renderPlot({ con_plot_obj() })
  output$trendPlot <- renderPlot({ 
    if(is_quantitative()) trend_plot_obj() else {
      plot.new()
      title(main = "Treatment is not quantitative. No trend plot generated.")
    }
  })
  
  # --- DOWNLOAD HANDLERS ---
  output$downloadReport <- downloadHandler(
    filename = function() { paste("RCBD_Analysis_", Sys.Date(), ".txt", sep = "") },
    content = function(file) {
      res <- analysis_res()
      sink(file)
      cat("=== RCBD ANALYSIS REPORT ===\n\n")
      print(res$anova)
      cat("\n--- ORTHOGONAL CONTRASTS ---\n")
      if(!is.null(res$contrasts)) print(res$contrasts) else cat("None\n")
      sink()
    }
  )
  
  output$downloadTrtPlot <- downloadHandler(
    filename = function() { paste("Treatment_Plot_", Sys.Date(), ".png", sep = "") },
    content = function(file) { ggsave(file, plot = trt_plot_obj(), width = 8, height = 6) }
  )
  
  output$downloadConPlot <- downloadHandler(
    filename = function() { paste("Contrast_Plot_", Sys.Date(), ".png", sep = "") },
    content = function(file) { ggsave(file, plot = con_plot_obj(), width = 8, height = 6) }
  )
  
  output$downloadTrendPlot <- downloadHandler(
    filename = function() { paste("Trend_Plot_", Sys.Date(), ".png", sep = "") },
    content = function(file) { ggsave(file, plot = trend_plot_obj(), width = 8, height = 6) }
  )
}

shinyApp(ui = ui, server = server)