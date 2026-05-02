library(shiny)
library(bslib)      
library(DT)         
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(dplyr)
library(ggplot2)
library(dplyr)

library(lme4)
library(lmerTest)
library(emmeans)
library(dplyr)
library(shiny)
library(bslib)      
library(DT)         
library(dplyr)
library(ggplot2)

library(shiny)
library(bslib)      
library(DT)         
library(dplyr)
library(ggplot2)


# Load the analysis and plotting functions we created earlier
#source("varietal_functions.R") 
analyze_varietal_trial <- function(data, 
                                   design = c("RCBD", "Lattice", "Prep"), 
                                   model_type = c("BLUE", "BLUP"), 
                                   handle_missing = c("drop", "predict"),
                                   y_var, 
                                   genotype_var, 
                                   rep_var = NULL, 
                                   block_var = NULL, 
                                   row_var = NULL, 
                                   col_var = NULL, 
                                   is_check_var = NULL, 
                                   plot_id_var = NULL,
                                   alpha = 0.05) {
  
  # 1. Match arguments and validate inputs
  design <- match.arg(design)
  model_type <- match.arg(model_type)
  handle_missing <- match.arg(handle_missing)
  
  if(!y_var %in% colnames(data)) stop(paste("Response variable", y_var, "not found in data."))
  if(!genotype_var %in% colnames(data)) stop(paste("Genotype variable", genotype_var, "not found in data."))
  
  # Convert variables to factors
  data[[genotype_var]] <- as.factor(data[[genotype_var]])
  if(!is.null(rep_var)) data[[rep_var]] <- as.factor(data[[rep_var]])
  if(!is.null(block_var)) data[[block_var]] <- as.factor(data[[block_var]])
  if(!is.null(row_var)) data[[row_var]] <- as.factor(data[[row_var]])
  if(!is.null(col_var)) data[[col_var]] <- as.factor(data[[col_var]])
  if(!is.null(is_check_var)) data[[is_check_var]] <- as.factor(data[[is_check_var]])
  
  # Handle missing data strategy
  if (handle_missing == "drop") {
    message("Dropping rows with missing values in the response variable...")
    data <- data[!is.na(data[[y_var]]), ]
  }
  
  # 2. Construct Formula dynamically
  fixed_eff <- c("1") # Intercept
  random_eff <- c()
  
  if (model_type == "BLUE") {
    fixed_eff <- c(fixed_eff, genotype_var)
  } else if (model_type == "BLUP") {
    random_eff <- c(random_eff, paste0("(1|", genotype_var, ")"))
  }
  
  if (!is.null(is_check_var) && model_type == "BLUP") {
    fixed_eff <- c(fixed_eff, is_check_var)
  }
  
  if (design == "RCBD") {
    if(is.null(rep_var)) stop("rep_var must be provided for RCBD.")
    random_eff <- c(random_eff, paste0("(1|", rep_var, ")"))
    
  } else if (design == "Lattice") {
    if(is.null(rep_var) || is.null(block_var)) stop("rep_var and block_var required for Lattice.")
    random_eff <- c(random_eff, paste0("(1|", rep_var, ")"), paste0("(1|", rep_var, ":", block_var, ")"))
    
  } else if (design == "Prep") {
    if(is.null(row_var) || is.null(col_var)) stop("row_var and col_var required for P-rep (Row-Col).")
    random_eff <- c(random_eff, paste0("(1|", row_var, ")"), paste0("(1|", col_var, ")"))
  }
  
  formula_str <- paste(y_var, "~", paste(fixed_eff, collapse = " + "))
  if (length(random_eff) > 0) {
    formula_str <- paste(formula_str, "+", paste(random_eff, collapse = " + "))
  }
  
  message("Fitting model: ", formula_str)
  
  # 3. Fit the model
  mod <- tryCatch({
    lmer(as.formula(formula_str), data = data, na.action = na.exclude)
  }, error = function(e) {
    stop("Error fitting model: ", e$message)
  })
  
  # 4. Extract Core Results
  results <- list()
  results$model <- mod
  var_comp <- as.data.frame(VarCorr(mod))
  results$variance_components <- var_comp
  
  # Predict Missing Values
  if (handle_missing == "predict") {
    imputed_data <- data
    predicted_vals <- predict(mod, newdata = data, allow.new.levels = TRUE)
    imputed_data$Model_Predicted_Value <- predicted_vals
    imputed_data$Is_Imputed <- is.na(imputed_data[[y_var]])
    imputed_data[[y_var]] <- ifelse(is.na(imputed_data[[y_var]]), predicted_vals, imputed_data[[y_var]])
    results$imputed_data <- imputed_data
  }
  
  # 5. Extract Estimates
  if (model_type == "BLUE") {
    em <- emmeans(mod, specs = genotype_var, lmer.df = "satterthwaite")
    results$estimates <- as.data.frame(em)
    results$comparisons <- as.data.frame(pairs(em))
    
  } else if (model_type == "BLUP") {
    ranef_obj <- ranef(mod, condVar = TRUE)
    geno_blups <- ranef_obj[[genotype_var]]
    se_blups <- sqrt(attr(geno_blups, "postVar")[1, 1, ])
    grand_mean_intercept <- fixef(mod)["(Intercept)"]
    
    results$estimates <- data.frame(
      Genotype = rownames(geno_blups),
      BLUP = geno_blups[[1]],
      Adjusted_Mean = geno_blups[[1]] + grand_mean_intercept,
      SE = se_blups
    )
    
    z_val <- qnorm(1 - alpha/2)
    results$estimates$Lower_CI <- results$estimates$Adjusted_Mean - (z_val * results$estimates$SE)
    results$estimates$Upper_CI <- results$estimates$Adjusted_Mean + (z_val * results$estimates$SE)
    colnames(results$estimates)[1] <- genotype_var
  }
  
  # ---------------------------------------------------------
  # NEW: 6. Trial Summary Statistics (Mean, CV, Heritability)
  # ---------------------------------------------------------
  
  # Overall Grand Mean of the observed trait
  grand_mean_val <- mean(data[[y_var]], na.rm = TRUE)
  
  # Residual Error Variance
  resid_var <- var_comp$vcov[var_comp$grp == "Residual"]
  
  # Coefficient of Variation (CV) %
  cv_percent <- (sqrt(resid_var) / grand_mean_val) * 100
  
  # Broad-Sense Heritability
  h2 <- NA 
  
  if (model_type == "BLUP") {
    geno_var <- var_comp$vcov[var_comp$grp == genotype_var]
    
    # Check if genotypic variance was successfully estimated > 0
    if (length(geno_var) > 0 && geno_var > 1e-6) {
      # Generalized Heritability based on Prediction Error Variance (PEV)
      # PEV is the square of the Standard Error of the BLUPs
      mean_pev <- mean(results$estimates$SE^2, na.rm = TRUE)
      
      # H^2 = 1 - (mean(PEV) / Sigma2_G)
      h2 <- 1 - (mean_pev / geno_var)
      h2 <- max(0, h2) # Safeguard against negative heritability due to rounding
      
    } else {
      # If genotypic variance is essentially 0, heritability is 0
      h2 <- 0 
    }
  }
  
  # Save metrics to the results list
  results$trial_stats <- data.frame(
    Trait = y_var,
    Grand_Mean = grand_mean_val,
    CV_Percent = cv_percent,
    Heritability = h2
  )
  
  return(results)
}

plot_varietal_means <- function(results, 
                                plot_type = c("point", "bar"),
                                sort_by_mean = TRUE, 
                                title = "Genotype Means with 95% CI",
                                x_label = "Trait Value",
                                y_label = "Genotype") {
  
  plot_type <- match.arg(plot_type)
  
  # 1. Extract the estimates dataframe
  if (is.null(results$estimates)) {
    stop("No estimates found in the results object.")
  }
  
  est_df <- results$estimates
  
  # 2. Standardize column names based on whether it was a BLUE or BLUP model
  if ("emmean" %in% colnames(est_df)) {
    # It's a BLUE model (LSMeans from emmeans)
    plot_df <- data.frame(
      Genotype = as.factor(est_df[[1]]), # First column is always the genotype variable
      Mean = est_df$emmean,
      Lower = est_df$lower.CL,
      Upper = est_df$upper.CL
    )
    model_subtitle <- "Estimates: BLUEs (LSMeans)"
    
  } else if ("Adjusted_Mean" %in% colnames(est_df)) {
    # It's a BLUP model (from our custom extraction)
    plot_df <- data.frame(
      Genotype = as.factor(est_df[[1]]),
      Mean = est_df$Adjusted_Mean,
      Lower = est_df$Lower_CI,
      Upper = est_df$Upper_CI
    )
    model_subtitle <- "Estimates: BLUPs (Adjusted Means)"
    
  } else {
    stop("Unrecognized estimates format. Ensure you are passing the output of analyze_varietal_trial().")
  }
  
  # 3. Sort Genotypes by Mean
  if (sort_by_mean) {
    plot_df$Genotype <- reorder(plot_df$Genotype, plot_df$Mean)
  }
  
  # 4. Generate the Plot
  p <- ggplot(plot_df, aes(x = Genotype, y = Mean))
  
  if (plot_type == "point") {
    p <- p + 
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "gray50", linewidth = 0.8) +
      geom_point(size = 3, color = "dodgerblue4")
  } else if (plot_type == "bar") {
    p <- p + 
      geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "gray30", linewidth = 0.8)
  }
  
  # Add formatting and flip coordinates for readability
  p <- p + 
    coord_flip() + 
    labs(title = title, subtitle = model_subtitle, x = y_label, y = x_label) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.y = element_text(size = 10), # Genotype names
      axis.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),  # Clean up horizontal lines
      panel.grid.minor.x = element_blank()
    )
  
  return(p)
}


# Load the analysis and plotting functions
#source("varietal_functions.R") 

# -------------------------------------------------------------------
# HELPER: GENERATE SIMULATED DATA
# -------------------------------------------------------------------
generate_example_data <- function() {
  set.seed(123)
  # Simulate 4 locations with distinct yield potentials
  env_params <- list(
    "Loc_1_Stress" = 2500,
    "Loc_2_Low" = 4000,
    "Loc_3_Optimal" = 6500,
    "Loc_4_High_Input" = 8000
  )
  
  dat_list <- lapply(names(env_params), function(env) {
    df <- expand.grid(
      Variety = paste0("Geno_", sprintf("%02d", 1:24)),
      Replicate = paste0("Rep_", 1:3)
    )
    df <- df[order(df$Replicate, df$Variety), ]
    df$Block <- rep(paste0("Blk_", 1:6), each = 4, times = 3)
    df$Environment <- env
    
    env_mean <- env_params[[env]]
    
    # Create Genotypic effects and specific GxE interaction (slopes)
    # Some genotypes will be highly responsive, others stable
    geno_base <- rep(rnorm(24, 0, 400), times = 3)
    gxe_slope <- rep(rnorm(24, 1, 0.3), times = 3) # Slopes around 1
    
    # Calculate yield based on environment mean * genotype responsiveness
    df$Yield_kg_ha <- (env_mean * gxe_slope) + geno_base + 
      rep(rnorm(3, 0, 100), each = 24) + # Rep effect
      rep(rnorm(18, 0, 150), each = 4) + # Block effect
      rnorm(72, 0, 300)                  # Error
    
    return(df)
  })
  return(do.call(rbind, dat_list))
}

# -------------------------------------------------------------------
# USER INTERFACE (UI)
# -------------------------------------------------------------------
ui <- page_sidebar(
  title = "Varietal Trial Analysis Hub",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    width = 350,
    
    radioButtons("data_source", "Data Source", 
                 choices = c("Upload .csv" = "upload", 
                             "Use Simulated Data (Example)" = "simulated")),
    
    conditionalPanel(
      condition = "input.data_source == 'upload'",
      fileInput("upload_data", "Upload Trial Data (.csv)", accept = ".csv")
    ),
    
    hr(),
    h5("1. Define Design"),
    selectInput("design", "Trial Design", choices = c("Lattice", "RCBD", "Prep")),
    selectInput("model_type", "Model Output", choices = c("BLUP (Adj. Means)" = "BLUP", "BLUE (LSMeans)" = "BLUE")),
    selectInput("missing_data", "Missing Data Strategy", choices = c("Predict values (Impute)" = "predict", "Drop NA rows" = "drop")),
    
    hr(),
    h5("2. Map Columns"),
    uiOutput("column_selectors"), 
    
    hr(),
    actionButton("run_btn", "Run Analysis", class = "btn-primary", width = "100%")
  ),
  
  # Main Content Area
  navset_card_underline(
    nav_panel("Raw Data Preview", DTOutput("raw_data_tbl")),
    nav_panel("Estimates & Results", 
              downloadButton("download_res", "Download Results"),
              br(), br(),
              DTOutput("results_tbl")),
    nav_panel("Trial Stats", DTOutput("stats_tbl")),
    nav_panel("Plot: Single Env", 
              selectInput("plot_env", "Select Environment:", choices = NULL),
              selectInput("plot_type", "Plot Type:", choices = c("point", "bar")),
              plotOutput("mean_plot", height = "600px")),
    # --- NEW: BOXPLOT TAB ---
    nav_panel("Plot: Distribution", 
              plotOutput("boxplot_env", height = "600px")),
    # --- NEW: STABILITY TAB ---
    nav_panel("Plot: Stability (GxE)", 
              uiOutput("stability_geno_selector"),
              plotOutput("stability_plot", height = "600px"))
  )
)

# -------------------------------------------------------------------
# SERVER LOGIC
# -------------------------------------------------------------------
server <- function(input, output, session) {
  
  # 1. Read Data
  trial_data <- reactive({
    if (input$data_source == "simulated") {
      return(generate_example_data())
    } else {
      req(input$upload_data)
      return(read.csv(input$upload_data$datapath, stringsAsFactors = FALSE))
    }
  })
  
  output$raw_data_tbl <- renderDT({
    req(trial_data())
    datatable(trial_data(), options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatRound(columns = sapply(trial_data(), is.numeric), digits = 2)
  })
  
  # 2. Map Columns
  output$column_selectors <- renderUI({
    req(trial_data())
    cols <- c("None", colnames(trial_data()))
    is_sim <- input$data_source == "simulated"
    
    tagList(
      selectInput("col_y", "Response Variable*", choices = cols[-1], selected = if(is_sim) "Yield_kg_ha" else NULL),
      selectInput("col_geno", "Genotype/Variety*", choices = cols[-1], selected = if(is_sim) "Variety" else NULL),
      selectInput("col_env", "Environment/Location", choices = cols, selected = if(is_sim) "Environment" else "None"),
      selectInput("col_rep", "Replicate", choices = cols, selected = if(is_sim) "Replicate" else "None"),
      selectInput("col_block", "Block", choices = cols, selected = if(is_sim) "Block" else "None"),
      selectInput("col_row", "Row", choices = cols),
      selectInput("col_col", "Column", choices = cols),
      selectInput("col_check", "Check Status", choices = cols)
    )
  })
  
  # 3. Run Analysis
  analysis_results <- eventReactive(input$run_btn, {
    req(trial_data(), input$col_y, input$col_geno)
    dat <- trial_data()
    
    get_col <- function(val) if(is.null(val) || val == "None") NULL else val
    env_var <- get_col(input$col_env)
    
    withProgress(message = 'Running Models...', value = 0, {
      if (is.null(env_var)) {
        incProgress(0.5)
        res <- analyze_varietal_trial(
          data = dat, design = input$design, model_type = input$model_type, handle_missing = input$missing_data,
          y_var = input$col_y, genotype_var = input$col_geno, rep_var = get_col(input$col_rep), 
          block_var = get_col(input$col_block), row_var = get_col(input$col_row), 
          col_var = get_col(input$col_col), is_check_var = get_col(input$col_check)
        )
        res$estimates$Environment <- "All Data"
        res$trial_stats$Environment <- "All Data"
        compiled_res <- list(All_Data = res)
      } else {
        envs <- unique(dat[[env_var]])
        compiled_res <- list()
        for (i in seq_along(envs)) {
          e <- envs[i]
          incProgress(i / length(envs), detail = e)
          sub_dat <- dat %>% filter(!!sym(env_var) == e)
          tryCatch({
            res <- analyze_varietal_trial(
              data = sub_dat, design = input$design, model_type = input$model_type, handle_missing = input$missing_data,
              y_var = input$col_y, genotype_var = input$col_geno, rep_var = get_col(input$col_rep), 
              block_var = get_col(input$col_block), row_var = get_col(input$col_row), 
              col_var = get_col(input$col_col), is_check_var = get_col(input$col_check)
            )
            res$estimates$Environment <- as.character(e)
            res$trial_stats$Environment <- as.character(e)
            compiled_res[[as.character(e)]] <- res
          }, error = function(err) {
            showNotification(paste("Skipped", e, "-", err$message), type = "warning", duration = 10)
          })
        }
      }
    })
    updateSelectInput(session, "plot_env", choices = names(compiled_res))
    return(compiled_res)
  })
  
  # 4 & 5. Compile Tables
  combined_estimates <- reactive({
    do.call(rbind, lapply(analysis_results(), function(x) x$estimates))
  })
  output$results_tbl <- renderDT({
    datatable(combined_estimates(), rownames = FALSE, filter = "top") %>%
      formatRound(columns = sapply(combined_estimates(), is.numeric), digits = 3)
  })
  
  combined_stats <- reactive({
    do.call(rbind, lapply(analysis_results(), function(x) x$trial_stats))
  })
  output$stats_tbl <- renderDT({
    datatable(combined_stats(), rownames = FALSE, options = list(dom = 't')) %>%
      formatRound(columns = sapply(combined_stats(), is.numeric), digits = 3)
  })
  
  # 6. Single Env Plot
  output$mean_plot <- renderPlot({
    req(analysis_results(), input$plot_env)
    env_res <- analysis_results()[[input$plot_env]]
    plot_varietal_means(env_res, plot_type = input$plot_type, 
                        title = paste("Performance -", input$plot_env), x_label = input$col_y)
  })
  
  # -------------------------------------------------------------------
  # NEW: 7. Distribution Boxplot
  # -------------------------------------------------------------------
  output$boxplot_env <- renderPlot({
    req(trial_data(), input$col_y)
    dat <- trial_data()
    env_col <- if(input$col_env == "None" || is.null(input$col_env)) NULL else input$col_env
    
    if (is.null(env_col)) {
      ggplot(dat, aes(x = "All Data", y = .data[[input$col_y]])) +
        geom_boxplot(fill = "steelblue", alpha = 0.7, width = 0.4) +
        theme_minimal(base_size = 14) +
        labs(title = "Trait Distribution", x = "", y = input$col_y)
    } else {
      # Order environments by median to show clear progression
      dat[[env_col]] <- reorder(dat[[env_col]], dat[[input$col_y]], FUN = median, na.rm = TRUE)
      
      ggplot(dat, aes(x = .data[[env_col]], y = .data[[input$col_y]], fill = .data[[env_col]])) +
        geom_boxplot(alpha = 0.7) +
        theme_minimal(base_size = 14) +
        labs(title = "Trait Distribution Across Environments",
             subtitle = "Ordered by environmental median",
             x = "Environment", y = input$col_y) +
        theme(legend.position = "none", axis.text.x = element_text(angle = 15, hjust = 1))
    }
  })
  
  # -------------------------------------------------------------------
  # NEW: 8. Stability Analysis (Finlay-Wilkinson)
  # -------------------------------------------------------------------
  stability_data <- reactive({
    req(combined_estimates())
    df <- combined_estimates()
    
    if (length(unique(df$Environment)) <= 1) return(NULL) # Requires multiple envs
    
    mean_col <- if ("emmean" %in% names(df)) "emmean" else "Adjusted_Mean"
    geno_col <- colnames(df)[1]
    
    # Calculate Environmental Index (Mean of all genotypes per environment)
    env_means <- df %>%
      group_by(Environment) %>%
      summarise(Env_Index = mean(.data[[mean_col]], na.rm = TRUE), .groups = "drop")
    
    df <- left_join(df, env_means, by = "Environment")
    df$Plot_Mean <- df[[mean_col]]
    df$Geno_ID <- df[[geno_col]]
    return(df)
  })
  
  output$stability_geno_selector <- renderUI({
    req(stability_data())
    df <- stability_data()
    
    # Pre-select Top 3 and Bottom 2 to show contrast in stability
    overall_means <- df %>%
      group_by(Geno_ID) %>%
      summarise(Mean = mean(Plot_Mean, na.rm = TRUE)) %>%
      arrange(desc(Mean))
    
    top_bot <- c(head(overall_means$Geno_ID, 3), tail(overall_means$Geno_ID, 2))
    
    selectizeInput("selected_genos_stab", "Select Genotypes to Analyze:",
                   choices = unique(df$Geno_ID), selected = top_bot, multiple = TRUE,
                   width = "100%", options = list(plugins = list('remove_button')))
  })
  
  output$stability_plot <- renderPlot({
    req(stability_data(), input$selected_genos_stab)
    df <- stability_data()
    
    df_sub <- df %>% filter(Geno_ID %in% input$selected_genos_stab)
    
    ggplot(df_sub, aes(x = Env_Index, y = Plot_Mean, color = Geno_ID, group = Geno_ID)) +
      geom_point(size = 4, alpha = 0.8) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) + # Baseline
      theme_minimal(base_size = 14) +
      labs(title = "Stability Analysis (Finlay-Wilkinson)",
           subtitle = "Dashed line represents environmental average (Slope = 1). \nSteep slope = Responsive. Flat slope = Stable.",
           x = "Environmental Index (Location Average)", 
           y = "Genotype Estimate (BLUE/BLUP)", 
           color = "Genotype") +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "right")
  })
  
  output$download_res <- downloadHandler(
    filename = function() { paste0("Trial_Results_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(combined_estimates(), file, row.names = FALSE) }
  )
}

shinyApp(ui = ui, server = server)

