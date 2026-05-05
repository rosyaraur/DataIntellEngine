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
analyze_contrast <- function(data, response, block = NULL, treatments, 
                                  contrast_matrix = NULL, 
                                  post_hoc = c("none", "LSD", "Tukey", "Duncan"),
                                  alpha = 0.05) {
  post_hoc <- match.arg(post_hoc)
  
  if (!is.null(block) && block != "None") data[[block]] <- as.factor(data[[block]])
  for (trt in treatments) data[[trt]] <- as.factor(data[[trt]])
  
  trt_term <- paste(treatments, collapse = " * ")
  formula_str <- if(!is.null(block) && block != "None") {
    paste(response, "~", block, "+", trt_term)
  } else {
    paste(response, "~", trt_term)
  }
  
  model <- aov(as.formula(formula_str), data = data)
  results <- list(model = model, anova = summary(model))
  
  # Orthogonal Contrasts via emmeans
  if (!is.null(contrast_matrix) && length(contrast_matrix) > 0) {
    emm <- emmeans(model, specs = treatments)
    contrast_res <- suppressMessages(contrast(emm, method = list(Custom = contrast_matrix)))
    results$contrasts <- contrast_res
  }
  
  # Post-Hoc Tests
  if (post_hoc != "none") {
    if (post_hoc == "LSD") out_test <- LSD.test(model, treatments, alpha = alpha, console = FALSE)
    if (post_hoc == "Tukey") out_test <- HSD.test(model, treatments, alpha = alpha, console = FALSE)
    if (post_hoc == "Duncan") out_test <- duncan.test(model, treatments, alpha = alpha, console = FALSE)
    results$post_hoc <- out_test
  }
  return(results)
}

# ==========================================
# 2. BUILT-IN DATASETS
# ==========================================
# Ex 1: 1-Way Categorical
df_stand <- data.frame(
  Block = rep(1:6, each = 8),
  Treatment = rep(c("A", "B", "C", "D", "E", "F", "G", "H"), times = 6),
  Stand = c(8, 16, 14, 10, 8, 8, 7, 12,  8, 19, 16, 11, 7, 8, 6, 19,  
            9, 24, 14, 12, 1, 3, 6, 9,   7, 22, 13, 8,  1, 3, 6, 11,  
            7, 19, 14, 7,  3, 3, 4, 9,   5, 19, 13, 3,  2, 7, 4, 5)   
)

# Ex 2: 1-Way Quantitative (Polynomial)
df_yield <- data.frame(
  Block = rep(1:6, each = 5),
  Spacing = rep(c(18, 24, 30, 36, 42), times = 6),
  Yield = c(33.6, 31.1, 33.0, 28.4, 31.4, 37.1, 34.5, 29.5, 29.9, 28.3,
            34.1, 30.5, 29.2, 31.6, 28.9, 34.6, 32.7, 30.7, 32.3, 28.6,
            35.4, 30.7, 30.7, 28.1, 29.6, 36.1, 30.3, 27.9, 26.9, 33.4)
)

# Ex 3: 2-Way CRD (Lambs)
df_lambs <- data.frame(
  Time = rep(c("AM", "PM"), each = 10),
  Estrogen = rep(rep(c("Control", "Treated"), each = 5), times = 2),
  Phospholipid = c(8.53, 20.53, 12.53, 14.00, 10.80,   17.53, 21.07, 20.80, 17.33, 20.07,
                   39.14, 26.20, 31.33, 45.80, 40.20,  32.00, 23.80, 28.87, 25.06, 29.33)
)

# Ex 4: 3-Way CRD (Seedlings - Simulating replicates from textbook totals)
set.seed(123)
tots <- c(266, 276, 286, 271, 66, 215, 252, 275, 289, 292, 167, 203, 152, 178, 197, 219, 52, 121)
reps <- as.vector(sapply(tots, function(t) { b <- floor(t/3); r <- t%%3; v <- c(b,b,b); if(r>0) v[1:r] <- v[1:r]+1; return(v) }))
df_seeds <- data.frame(
  Species = rep(c("Alfalfa", "Red_clover", "Sweet_clover"), each = 18),
  Soil = rep(rep(c("Silt_loam", "Sand", "Clay"), each = 6), times = 3),
  Fungicide = rep(rep(c("None", "Treated"), each = 3), times = 9),
  Emerged = reps
)

# ==========================================
# 3. SHINY UI
# ==========================================
ui <- fluidPage(
  titlePanel("Multi-Factor RCBD & Contrast Analyzer"),
  sidebarLayout(
    sidebarPanel(
      h4("1. Data Source"),
      radioButtons("data_source", "Select Dataset:",
                   choices = c("Ex 1: Greenhouse (1-Way, 8 Lvls)" = "ex1",
                               "Ex 2: Soybeans (1-Way, Quant)" = "ex2",
                               "Ex 3: Lambs (2-Way CRD)" = "ex3",
                               "Ex 4: Seedlings (3-Way CRD)" = "ex4",
                               "Upload Custom CSV" = "upload")),
      conditionalPanel("input.data_source == 'upload'", fileInput("file1", "Choose CSV", accept = c(".csv"))),
      
      h4("2. Model Variables"),
      uiOutput("var_selectors"),
      
      h4("3. Means Comparison"),
      selectInput("post_hoc", "Post-Hoc Test:", choices = c("none", "LSD", "Tukey", "Duncan"), selected = "Tukey"),
      numericInput("alpha", "Alpha Level:", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
      uiOutput("trend_ui"),
      br(), actionButton("run", "Run Analysis", class = "btn-success btn-lg", width = "100%")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("1. Contrast Editor", br(), rHandsontableOutput("contrast_table")),
        tabPanel("2. ANOVA & Results", br(), verbatimTextOutput("anovaOutput"), verbatimTextOutput("contrastOutput"), verbatimTextOutput("posthocOutput")),
        tabPanel("3. Treatment Plot", br(), plotOutput("barPlot", height = "500px")),
        tabPanel("4. Contrast Plot", br(), plotOutput("contrastPlot", height = "500px")),
        tabPanel("5. Trend Plot", br(), plotOutput("trendPlot", height = "500px")),
        tabPanel("6. Data Preview", dataTableOutput("dataPreview"))
      )
    )
  )
)

# ==========================================
# 4. SHINY SERVER
# ==========================================
server <- function(input, output, session) {
  current_data <- reactive({
    if (input$data_source == "ex1") return(df_stand)
    if (input$data_source == "ex2") return(df_yield)
    if (input$data_source == "ex3") return(df_lambs)
    if (input$data_source == "ex4") return(df_seeds)
    req(input$file1); read.csv(input$file1$datapath)
  })
  
  output$var_selectors <- renderUI({
    req(current_data()); cols <- names(current_data())
    
    # Auto-detect defaults for built-in examples
    def_resp <- if("Phospholipid" %in% cols) "Phospholipid" else if("Emerged" %in% cols) "Emerged" else if("Stand" %in% cols) "Stand" else cols[1]
    def_blk <- if("Block" %in% cols) "Block" else "None"
    def_trt <- if("Time" %in% cols) c("Time", "Estrogen") else if("Species" %in% cols) c("Species", "Soil", "Fungicide") else if("Spacing" %in% cols) "Spacing" else if("Treatment" %in% cols) "Treatment" else cols[3]
    
    tagList(
      selectInput("response", "Response:", choices = cols, selected = def_resp),
      selectInput("block", "Block (Optional):", choices = c("None", cols), selected = def_blk),
      # IMPORTANT: Multiple allows multi-factor selection!
      selectizeInput("treatment", "Treatment Factors (Select Multiple):", choices = cols, selected = def_trt, multiple = TRUE)
    )
  })
  
  # --- MULTI-FACTOR GRID & CONTRAST LOGIC ---
  contrast_template <- reactive({
    req(current_data(), input$treatment)
    d <- current_data(); trt_vars <- input$treatment
    if (!all(trt_vars %in% names(d))) return(NULL)
    
    # 1. Build the exact grid emmeans uses (First factor varies fastest)
    grid_list <- lapply(trt_vars, function(v) levels(as.factor(d[[v]])))
    names(grid_list) <- trt_vars
    eg <- expand.grid(grid_list)
    col_names <- apply(eg, 1, paste, collapse = " : ") # e.g., "AM : Control"
    n_cols <- length(col_names)
    
    # 2. Assign Dummies based on Dataset
    if (input$data_source == "ex1" && n_cols == 8) {
      df <- data.frame(Name = c("A vs rest", "BC vs DEFGH"), A=c(-7,0), B=c(1,5), C=c(1,5), D=c(1,-2), E=c(1,-2), F=c(1,-2), G=c(1,-2), H=c(1,-2))
      colnames(df)[2:9] <- col_names
      return(df)
    } else if (input$data_source == "ex2" && n_cols == 5) {
      df <- data.frame(Name = c("Linear", "Quadratic"), L1=c(-2,2), L2=c(-1,-1), L3=c(0,-2), L4=c(1,-1), L5=c(2,2))
      colnames(df)[2:6] <- col_names
      return(df)
    } else if (input$data_source == "ex3" && n_cols == 4) {
      # 2-WAY DUMMIES (Lambs: Time x Estrogen)
      # Grid: AM:Control, PM:Control, AM:Treated, PM:Treated
      df <- data.frame(
        Name = c("Main: Treated vs Control", "Main: AM vs PM", "Simple: Trt vs Ctrl (AM only)", "Interaction: Time x Estrogen"),
        C1 = c(-1, -1, -1, -1), # AM:Control
        C2 = c(-1,  1,  0,  1), # PM:Control
        C3 = c( 1, -1,  1,  1), # AM:Treated
        C4 = c( 1,  1,  0, -1), # PM:Treated
        stringsAsFactors = FALSE
      )
      colnames(df)[2:5] <- col_names
      return(df)
    } else if (input$data_source == "ex4" && n_cols == 18) {
      # 3-WAY DUMMIES (Seedlings: Species x Soil x Fungicide)
      # 18 Columns. Demo: Main effect of Fungicide (First 9 are None, Last 9 are Treated)
      df <- data.frame(Name = "Main Effect: Fungicide (Treated vs None)", stringsAsFactors = FALSE)
      df[1, 2:10] <- -1  # None group
      df[1, 11:19] <- 1  # Treated group
      colnames(df)[2:19] <- col_names
      return(df)
    } else {
      df <- data.frame(Name = "Custom_1", stringsAsFactors = FALSE)
      for (l in col_names) df[[l]] <- 0
      return(df)
    }
  })
  
  output$contrast_table <- renderRHandsontable({
    req(contrast_template())
    rhandsontable(contrast_template(), rowHeaders = TRUE) %>%
      hot_col("Name", type = "text") %>% hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })
  
  # --- RUN ANALYSIS ---
  analysis_res <- eventReactive(input$run, {
    req(input$response, input$treatment)
    d_filtered <- current_data()
    
    my_contrasts <- list()
    if (!is.null(input$contrast_table)) {
      df_c <- hot_to_r(input$contrast_table)
      for (i in 1:nrow(df_c)) {
        if (any(as.numeric(df_c[i, -1]) != 0)) my_contrasts[[df_c$Name[i]]] <- as.numeric(df_c[i, -1])
      }
    }
    
    analyze_contrast(d_filtered, input$response, input$block, input$treatment, my_contrasts, input$post_hoc, input$alpha)
  })
  
  # --- OUTPUTS ---
  output$anovaOutput <- renderPrint({ req(analysis_res()); print(analysis_res()$anova) })
  output$contrastOutput <- renderPrint({ req(analysis_res()); print(analysis_res()$contrasts) })
  output$posthocOutput <- renderPrint({ req(analysis_res()); print(analysis_res()$post_hoc$groups) })
  output$dataPreview <- renderDataTable({ current_data() }, options = list(pageLength = 10))
  
  output$barPlot <- renderPlot({
    req(analysis_res()); if (input$post_hoc == "none") return(ggplot() + theme_void())
    grp_data <- analysis_res()$post_hoc$groups; mean_data <- analysis_res()$post_hoc$means
    trt_names <- rownames(grp_data)
    plot_df <- data.frame(Treatment = factor(trt_names, levels = sort(trt_names)), Mean = grp_data[, 1], Group = as.character(grp_data$groups))
    plot_df$SE <- mean_data[trt_names, "std"] / sqrt(mean_data[trt_names, "r"])
    ggplot(plot_df, aes(x = Treatment, y = Mean)) + geom_bar(stat="identity", fill="#4C72B0") + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2) + geom_text(aes(label=Group, y=Mean+SE+max(SE)*2), fontface="bold") + theme_classic() + coord_flip()
  })
  
  output$contrastPlot <- renderPlot({
    req(analysis_res()); if(is.null(analysis_res()$contrasts)) return(ggplot() + theme_void())
    c_df <- as.data.frame(analysis_res()$contrasts)
    ggplot(c_df, aes(x = reorder(contrast, estimate), y = estimate)) + geom_hline(yintercept = 0, color="red", linetype="dashed") + geom_errorbar(aes(ymin=estimate-SE, ymax=estimate+SE), width=0.2) + geom_point(size=4) + coord_flip() + theme_classic()
  })
}

shinyApp(ui = ui, server = server)