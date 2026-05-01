#install.packages(c("shiny", "DT", "ggplot2", "lme4", "lmerTest", "emmeans", "dplyr", "tidyr"))
library(shiny)
library(DT)
library(ggplot2)
library(plotly) 
library(lme4)
library(lmerTest)
library(emmeans)
library(dplyr)
library(tidyr)

# -------------------------------------------------------------------------
# 1. CORE FUNCTION 
# -------------------------------------------------------------------------
calc_geno_values <- function(data, y_var, geno_var, loc_var = NULL, 
                             block_var = NULL, row_var = NULL, col_var = NULL, 
                             method = c("BLUP", "BLUE"), ci_level = 0.95) {
  method <- match.arg(method)
  
  # Ensure factors drop empty levels (Crucial for single-site splits)
  for (f in c(geno_var, loc_var, block_var, row_var, col_var)) {
    if (!is.null(f) && f != "None" && f %in% colnames(data)) {
      data[[f]] <- as.factor(as.character(data[[f]]))
    }
  }
  
  if(!is.null(loc_var) && loc_var == "None") loc_var <- NULL
  if(!is.null(block_var) && block_var == "None") block_var <- NULL
  if(!is.null(row_var) && row_var == "None") row_var <- NULL
  if(!is.null(col_var) && col_var == "None") col_var <- NULL
  
  formula_str <- paste(y_var, "~")
  if (method == "BLUE") { formula_str <- paste(formula_str, geno_var) 
  } else { formula_str <- paste(formula_str, "1 + (1 |", geno_var, ")") }
  
  random_terms <- c()
  if (!is.null(loc_var)) {
    random_terms <- c(random_terms, paste0("(1 | ", loc_var, ")"))
    if (!is.null(block_var)) random_terms <- c(random_terms, paste0("(1 | ", loc_var, ":", block_var, ")"))
    if (!is.null(row_var)) random_terms <- c(random_terms, paste0("(1 | ", loc_var, ":", row_var, ")"))
    if (!is.null(col_var)) random_terms <- c(random_terms, paste0("(1 | ", loc_var, ":", col_var, ")"))
  } else {
    if (!is.null(block_var)) random_terms <- c(random_terms, paste0("(1 | ", block_var, ")"))
    if (!is.null(row_var)) random_terms <- c(random_terms, paste0("(1 | ", row_var, ")"))
    if (!is.null(col_var)) random_terms <- c(random_terms, paste0("(1 | ", col_var, ")"))
  }
  if (length(random_terms) > 0) formula_str <- paste(formula_str, "+", paste(random_terms, collapse = " + "))
  
  model <- lmerTest::lmer(as.formula(formula_str), data = data)
  results <- list(model = model, method = method)
  
  if (method == "BLUP") {
    re <- ranef(model, condVar = TRUE); blups_raw <- re[[geno_var]]
    cond_var <- attr(blups_raw, "postVar"); blup_se <- sqrt(cond_var[1, 1, ])
    grand_mean <- fixef(model)["(Intercept)"]
    plot_df <- data.frame(Genotype = rownames(blups_raw), Estimate = blups_raw[, 1] + grand_mean, SE = blup_se)
    z_score <- qnorm(1 - (1 - ci_level) / 2)
    plot_df$Lower <- plot_df$Estimate - (z_score * plot_df$SE); plot_df$Upper <- plot_df$Estimate + (z_score * plot_df$SE)
    var_comp <- as.data.frame(VarCorr(model)); Vg <- var_comp$vcov[var_comp$grp == geno_var]
    mean_PEV <- mean(blup_se^2); results$heritability <- ifelse(Vg > 1e-8, 1 - (mean_PEV / Vg), 0)
    results$values <- plot_df
  } else {
    blue_em <- emmeans::emmeans(model, specs = geno_var)
    plot_df <- as.data.frame(confint(blue_em, level = ci_level))
    colnames(plot_df)[which(colnames(plot_df) == geno_var)] <- "Genotype"
    colnames(plot_df)[which(colnames(plot_df) == "emmean")] <- "Estimate"
    colnames(plot_df)[which(colnames(plot_df) == "lower.CL")] <- "Lower"
    colnames(plot_df)[which(colnames(plot_df) == "upper.CL")] <- "Upper"
    results$values <- plot_df
  }
  return(results)
}

# -------------------------------------------------------------------------
# 2. SHINY UI
# -------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Interactive Multi-Environment Trial Analysis (BLUE/BLUP)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV File", accept = c(".csv")),
      uiOutput("col_selectors"),
      hr(),
      radioButtons("method", "Calculation Method:", choices = c("BLUP", "BLUE"), selected = "BLUP"),
      sliderInput("ci_level", "Confidence/Prediction Interval:", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
      actionButton("run_btn", "Run Analysis", class = "btn-primary", width = "100%"),
      hr(),
      h4("Downloads"),
      downloadButton("downloadOverall", "Download Overall Avg", width = "100%", style="margin-bottom: 5px;"),
      downloadButton("downloadEnv", "Download By Environment", width = "100%", style="background-color:#5cb85c; color:white;") # NEW BUTTON
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", DTOutput("data_preview")),
        tabPanel("Overall Results", 
                 h4(textOutput("heritability_text")),
                 plotlyOutput("caterpillar_plot", height = "600px"), 
                 hr(),
                 DTOutput("results_table")),
        
        # NEW TAB FOR ENVIRONMENT-SPECIFIC RESULTS
        tabPanel("Results by Environment",
                 h4("Single-Site Analysis (GxE)"),
                 p("These values represent the calculated BLUEs/BLUPs for each genotype within each specific environment."),
                 DTOutput("env_results_table")),
        
        tabPanel("Site Comparisons & Stability", 
                 h4("Genotype Performance and Stability Analysis"),
                 uiOutput("geno_selector"),
                 radioButtons("plot_type", "Plot Type:", 
                              choices = c("Line Plot", "Heatmap", "Stability Plot"), 
                              inline = TRUE),
                 plotlyOutput("site_plot", height = "600px"))
      )
    )
  )
)

# -------------------------------------------------------------------------
# 3. SHINY SERVER
# -------------------------------------------------------------------------
server <- function(input, output, session) {
  raw_data <- reactive({ req(input$file1); read.csv(input$file1$datapath) })
  
  output$data_preview <- renderDT({ req(raw_data()); datatable(head(raw_data(), 50), options = list(scrollX = TRUE)) })
  
  output$col_selectors <- renderUI({
    req(raw_data()); cols <- colnames(raw_data())
    tagList(
      selectInput("y_var", "Response Variable (Y):", choices = cols),
      selectInput("geno_var", "Genotype Variable:", choices = cols),
      selectInput("loc_var", "Location Variable:", choices = c("None", cols), selected = "None"),
      selectInput("block_var", "Block/Rep Variable (Optional):", choices = c("None", cols), selected = "None"),
      selectInput("row_var", "Row Variable (Optional):", choices = c("None", cols), selected = "None"),
      selectInput("col_var", "Column Variable (Optional):", choices = c("None", cols), selected = "None")
    )
  })
  
  # OVERALL ANALYSIS
  analysis_results <- eventReactive(input$run_btn, {
    req(raw_data(), input$y_var, input$geno_var)
    withProgress(message = 'Fitting Overall Model...', value = 0.5, {
      calc_geno_values(raw_data(), input$y_var, input$geno_var, input$loc_var, 
                       input$block_var, input$row_var, input$col_var, input$method, input$ci_level)
    })
  })
  
  # NEW: ENVIRONMENT-SPECIFIC ANALYSIS
  env_results_data <- eventReactive(input$run_btn, {
    req(raw_data(), input$y_var, input$geno_var)
    
    # If no location is selected, return NULL for this tab
    if (input$loc_var == "None" || is.null(input$loc_var)) return(NULL)
    
    df <- raw_data()
    locs <- unique(as.character(df[[input$loc_var]]))
    
    withProgress(message = 'Fitting Single-Site Models...', value = 0, {
      res_list <- lapply(seq_along(locs), function(i) {
        loc <- locs[i]
        incProgress(1/length(locs), detail = paste("Site:", loc))
        
        # Subset data for the specific location
        sub_data <- df[df[[input$loc_var]] == loc, ]
        
        # Try-catch to prevent crash if one location is missing data or variance
        tryCatch({
          res <- calc_geno_values(
            data = sub_data, 
            y_var = input$y_var, 
            geno_var = input$geno_var, 
            loc_var = NULL, # Crucial: set to NULL because we are already within a site
            block_var = input$block_var, 
            row_var = input$row_var, 
            col_var = input$col_var, 
            method = input$method, 
            ci_level = input$ci_level
          )
          
          out_df <- res$values
          out_df$Location <- loc # Append the location name
          return(out_df)
        }, error = function(e) {
          return(NULL) # Skip if model fails for this specific location
        })
      })
    })
    
    # Combine all single-site results into one dataframe and reorder columns
    combined_df <- do.call(rbind, res_list)
    combined_df <- combined_df %>% select(Location, Genotype, Estimate, SE, Lower, Upper)
    return(combined_df)
  })
  
  # Render Texts & Tables
  output$heritability_text <- renderText({
    req(analysis_results()); res <- analysis_results()
    if (res$method == "BLUP") paste("Generalized Heritability (Reliability):", round(res$heritability, 3)) else "Method: BLUE"
  })
  
  output$results_table <- renderDT({
    req(analysis_results()); df <- analysis_results()$values %>% mutate(across(where(is.numeric), ~round(., 3)))
    datatable(df, rownames = FALSE)
  })
  
  # Render NEW Environment Table
  output$env_results_table <- renderDT({
    req(env_results_data())
    df <- env_results_data() %>% mutate(across(where(is.numeric), ~round(., 3)))
    datatable(df, rownames = FALSE, filter = "top") # Added filter so users can search by location
  })
  
  output$caterpillar_plot <- renderPlotly({
    req(analysis_results()); df <- analysis_results()$values
    df$Genotype <- reorder(df$Genotype, df$Estimate)
    
    p <- ggplot(df, aes(x = Genotype, y = Estimate, 
                        text = paste("Genotype:", Genotype, 
                                     "<br>Estimate:", round(Estimate, 2),
                                     "<br>Lower:", round(Lower, 2),
                                     "<br>Upper:", round(Upper, 2)))) +
      geom_point(color = "dodgerblue4", size = 2) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "dodgerblue4") +
      coord_flip() + theme_minimal() + labs(title = "Overall Genotype Estimates")
    
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(align = "left"))
  })
  
  # DOWNLOAD HANDLERS
  output$downloadOverall <- downloadHandler(
    filename = function() { paste("Overall_Results_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(analysis_results()$values, file, row.names = FALSE) }
  )
  
  output$downloadEnv <- downloadHandler(
    filename = function() { paste("By_Environment_Results_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(env_results_data(), file, row.names = FALSE) }
  )
  
  # Stability Plots...
  output$geno_selector <- renderUI({
    req(raw_data(), input$geno_var)
    genos <- unique(as.character(raw_data()[[input$geno_var]]))
    selectizeInput("selected_genos", "Select Genotypes to Compare:", choices = genos, selected = head(genos, 5), multiple = TRUE)
  })
  
  output$site_plot <- renderPlotly({
    req(raw_data(), input$selected_genos, input$y_var, input$loc_var)
    if (input$loc_var == "None") return(NULL)
    
    env_means <- raw_data() %>% group_by(.data[[input$loc_var]]) %>% summarise(Env_Mean = mean(.data[[input$y_var]], na.rm = TRUE))
    summary_df <- raw_data() %>% filter(.data[[input$geno_var]] %in% input$selected_genos) %>% group_by(.data[[input$geno_var]], .data[[input$loc_var]]) %>% summarise(Mean_Y = mean(.data[[input$y_var]], na.rm = TRUE), .groups = 'drop') %>% left_join(env_means, by = input$loc_var)
    colnames(summary_df)[1:2] <- c("Genotype", "Location")
    
    if (input$plot_type == "Stability Plot") {
      p <- ggplot(summary_df, aes(x = Env_Mean, y = Mean_Y, color = Genotype, group = Genotype, text = paste("Genotype:", Genotype, "<br>Location:", Location, "<br>Env Mean:", round(Env_Mean, 2), "<br>Geno Mean:", round(Mean_Y, 2)))) + geom_point(alpha = 0.8, size = 3) + geom_smooth(method = "lm", se = FALSE, size = 1, alpha = 0.6) + geom_abline(slope = 1, intercept = mean(summary_df$Mean_Y) - mean(summary_df$Env_Mean), linetype = "dashed", color = "red", size = 1.5) + theme_minimal() + labs(title = "Stability Analysis (Finlay-Wilkinson)", subtitle = "Red dashed line represents average stability (Slope = 1.0)", x = "Environment Mean (Site Quality)", y = "Genotype Performance at Site")
      return(ggplotly(p, tooltip = "text"))
    } else if (input$plot_type == "Line Plot") {
      p <- ggplot(summary_df, aes(x = Location, y = Mean_Y, group = Genotype, color = Genotype, text = paste("Genotype:", Genotype, "<br>Location:", Location, "<br>Mean:", round(Mean_Y, 2)))) + geom_line() + geom_point() + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      return(ggplotly(p, tooltip = "text"))
    } else {
      p <- ggplot(summary_df, aes(x = Location, y = Genotype, fill = Mean_Y, text = paste("Genotype:", Genotype, "<br>Location:", Location, "<br>Mean:", round(Mean_Y, 2)))) + geom_tile() + scale_fill_viridis_c() + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      return(ggplotly(p, tooltip = "text"))
    }
  })
}

shinyApp(ui, server)