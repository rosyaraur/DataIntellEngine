# app.R
library(shiny)
library(lme4)
library(dplyr)
library(tidyr)
library(DT)
library(bslib)
library(plotly)

# ==============================================================================
# 1. CORE FUNCTION (Comstock & Robinson GxE) 
# ==============================================================================
comstock_robinson_gxe <- function(data, trait, genotype, location, rep, year = NULL, method = c("mixed", "original")) {
  method <- match.arg(method)
  
  data[[genotype]] <- as.factor(data[[genotype]])
  data[[location]] <- as.factor(data[[location]])
  data[[rep]]      <- as.factor(data[[rep]])
  
  g <- nlevels(data[[genotype]])
  l <- nlevels(data[[location]])
  r <- nlevels(data[[rep]])
  is_multi_year <- !is.null(year)
  
  if (is_multi_year) {
    data[[year]] <- as.factor(data[[year]])
    y <- nlevels(data[[year]])
  } else {
    y <- 1 
  }
  
  # Helper function to standardize effect arrays/BLUPs into clean dataframes
  standardize_effect <- function(eff, term_names) {
    if(is.array(eff) || inherits(eff, "table") || (is.numeric(eff) && is.null(dim(eff)))) {
      df <- as.data.frame(as.table(eff))
      colnames(df)[ncol(df)] <- "Effect"
      if (ncol(df) - 1 == length(term_names)) {
        colnames(df)[1:(ncol(df)-1)] <- term_names
      }
      return(df)
    } else {
      # lme4 ranef logic fix: ensures Label is Column 1 and Effect is Column 2
      df <- data.frame(Level = rownames(eff), Effect = eff[[1]])
      if (length(term_names) > 1) {
        splits <- do.call(rbind, strsplit(as.character(df$Level), ":"))
        colnames(splits) <- term_names
        df <- cbind(as.data.frame(splits), Effect = df$Effect)
        df$Level <- NULL
      } else {
        # Rename the first column to the exact term name (e.g., "Genotype")
        colnames(df)[1] <- term_names[1]
        df$Level <- NULL
      }
      return(df)
    }
  }
  
  if (method == "original") {
    if (is_multi_year) {
      formula_aov <- as.formula(paste(trait, "~", rep, "+", genotype, "+", location, "+", year, "+", 
                                      paste0(genotype, ":", location), "+", paste0(genotype, ":", year), "+",
                                      paste0(location, ":", year), "+", paste0(genotype, ":", location, ":", year)))
    } else {
      formula_aov <- as.formula(paste(trait, "~", rep, "+", genotype, "+", location, "+", paste0(genotype, ":", location)))
    }
    
    model_aov <- aov(formula_aov, data = data)
    anova_table <- summary(model_aov)[[1]]
    rownames(anova_table) <- trimws(rownames(anova_table))
    
    ms_g  <- anova_table[genotype, "Mean Sq"]
    ms_l  <- anova_table[location, "Mean Sq"]
    ms_gl <- anova_table[paste0(genotype, ":", location), "Mean Sq"]
    ms_e  <- anova_table["Residuals", "Mean Sq"]
    
    effects_tables <- model.tables(model_aov, type = "effects")$tables
    
    effect_estimates <- list(
      G   = standardize_effect(effects_tables[[genotype]], genotype),
      L   = standardize_effect(effects_tables[[location]], location),
      GxL = standardize_effect(effects_tables[[paste0(genotype, ":", location)]], c(genotype, location))
    )
    
    if (is_multi_year) {
      ms_y   <- anova_table[year, "Mean Sq"]
      ms_gy  <- anova_table[paste0(genotype, ":", year), "Mean Sq"]
      ms_ly  <- anova_table[paste0(location, ":", year), "Mean Sq"]
      ms_gly <- anova_table[paste0(genotype, ":", location, ":", year), "Mean Sq"]
      
      var_e   <- ms_e; var_gly <- (ms_gly - ms_e) / r; var_ly  <- (ms_ly - ms_gly) / (r * g)
      var_gy  <- (ms_gy - ms_gly) / (r * l); var_gl  <- (ms_gl - ms_gly) / (r * y)
      var_y   <- (ms_y - ms_gy - ms_ly + ms_gly) / (r * g * l)
      var_l   <- (ms_l - ms_gl - ms_ly + ms_gly) / (r * g * y)
      var_g   <- (ms_g - ms_gl - ms_gy - ms_gly) / (r * l * y)
      
      effect_estimates$Y <- standardize_effect(effects_tables[[year]], year)
      effect_estimates$GxY <- standardize_effect(effects_tables[[paste0(genotype, ":", year)]], c(genotype, year))
      effect_estimates$GxLxY <- standardize_effect(effects_tables[[paste0(genotype, ":", location, ":", year)]], c(genotype, location, year))
    } else {
      var_e  <- ms_e; var_gl <- (ms_gl - ms_e) / r
      var_l  <- (ms_l - ms_gl) / (r * g); var_g  <- (ms_g - ms_gl) / (r * l)
    }
  } else if (method == "mixed") {
    if (is_multi_year) {
      formula_mixed <- as.formula(paste(trait, "~ 1 + (1|", rep, ") + (1|", location, ") + (1|", year, ") + (1|", location, ":", year, ") + (1|", genotype, ") + (1|", genotype, ":", location, ") + (1|", genotype, ":", year, ") + (1|", genotype, ":", location, ":", year, ")"))
    } else {
      formula_mixed <- as.formula(paste(trait, "~ 1 + (1|", rep, ") + (1|", location, ") + (1|", genotype, ") + (1|", genotype, ":", location, ")"))
    }
    
    model_mixed <- lmer(formula_mixed, data = data)
    var_comps <- as.data.frame(VarCorr(model_mixed))
    blups <- ranef(model_mixed)
    
    get_var <- function(group_name) {
      val <- var_comps$vcov[var_comps$grp == group_name]
      if(length(val) == 0) return(0) else return(val)
    }
    
    var_g  <- get_var(genotype); var_l  <- get_var(location)
    var_gl <- get_var(paste0(genotype, ":", location)); var_e  <- get_var("Residual")
    anova_table <- "ANOVA not generated for REML. See variance components."
    
    effect_estimates <- list(
      G   = standardize_effect(blups[[genotype]], genotype),
      L   = standardize_effect(blups[[location]], location),
      GxL = standardize_effect(blups[[paste0(genotype, ":", location)]], c(genotype, location))
    )
    
    if (is_multi_year) {
      var_y   <- get_var(year); var_gy  <- get_var(paste0(genotype, ":", year))
      var_ly  <- get_var(paste0(location, ":", year))
      var_gly <- get_var(paste0(genotype, ":", location, ":", year))
      
      effect_estimates$Y <- standardize_effect(blups[[year]], year)
      effect_estimates$GxY <- standardize_effect(blups[[paste0(genotype, ":", year)]], c(genotype, year))
      effect_estimates$GxLxY <- standardize_effect(blups[[paste0(genotype, ":", location, ":", year)]], c(genotype, location, year))
    }
  }
  
  if (is_multi_year) {
    var_p <- var_g + (var_gl / l) + (var_gy / y) + (var_gly / (l * y)) + (var_e / (l * y * r))
    var_table <- data.frame(
      Component = c("Genotypic (Vg)", "Location (Vl)", "Year (Vy)", "G x L (Vgl)", "G x Y (Vgy)", "L x Y (Vly)", "G x L x Y (Vgly)", "Error (Ve)", "Phenotypic (Vp)"),
      Estimate = round(c(var_g, var_l, var_y, var_gl, var_gy, var_ly, var_gly, var_e, var_p), 4)
    )
  } else {
    var_p <- var_g + (var_gl / l) + (var_e / (l * r))
    var_table <- data.frame(
      Component = c("Genotypic (Vg)", "Location (Vl)", "G x L (Vgl)", "Error (Ve)", "Phenotypic (Vp)"),
      Estimate = round(c(var_g, var_l, var_gl, var_e, var_p), 4)
    )
  }
  
  list(
    Method = toupper(method),
    Analysis_Type = ifelse(is_multi_year, "Multi-Year (GxLxY)", "Single-Year (GxL)"),
    ANOVA_Table = anova_table,
    Variance_Components = var_table,
    Heritability = var_g / var_p,
    Effect_Estimates = effect_estimates
  )
}

# ==============================================================================
# 2. DEFAULT EMBEDDED DATA
# ==============================================================================
wheat_yield_data <- data.frame(
  Genotype = rep(c("A", "B", "C", "D", "E", "F", "G"), each = 18),
  Location = rep(rep(c("L1", "L2", "L3"), each = 6), times = 7),
  Year     = rep(rep(c("Y1", "Y2"), each = 3), times = 21),
  Rep      = rep(c("R1", "R2", "R3"), times = 42),
  Yield    = c(60,65,60,80,65,75,70,75,70,72,82,90,48,45,50,50,40,40,
               80,90,83,70,60,60,85,90,90,70,85,80,40,40,40,38,40,50,
               25,28,30,40,35,35,35,30,30,40,35,35,35,25,20,35,30,30,
               50,65,50,40,40,40,48,50,52,45,45,50,50,50,45,40,48,40,
               52,50,55,55,54,50,40,40,60,48,38,45,38,30,40,35,40,35,
               22,25,25,30,28,32,28,25,30,26,28,28,45,50,45,50,50,50,
               30,30,25,28,34,35,40,45,35,30,32,35,45,35,38,44,45,40)
)

# ==============================================================================
# 3. UI
# ==============================================================================
ui <- page_sidebar(
  title = "GxE Stability Analysis Engine",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Configuration",
    radioButtons("data_source", "Data Source:", choices = c("Use Example Data", "Upload CSV")),
    conditionalPanel("input.data_source == 'Upload CSV'", fileInput("file_upload", "Upload CSV", accept = ".csv")),
    hr(),
    uiOutput("col_selectors"),
    hr(),
    uiOutput("filter_locs"),
    uiOutput("filter_years"),
    hr(),
    radioButtons("method", "Estimation Method:", choices = c("Mixed Model (REML)" = "mixed", "Original ANOVA" = "original")),
    actionButton("run_analysis", "Run Analysis", class = "btn-primary", width = "100%")
  ),
  
  navset_card_tab(
    nav_panel("Analysis Summary", 
              uiOutput("analysis_header"), br(),
              h4("Heritability"), verbatimTextOutput("heritability_out"), br(),
              h4("Variance Components"), DTOutput("var_comps_out")
    ),
    nav_panel("Effect Heatmaps",
              layout_columns(
                col_widths = c(4, 8),
                card(
                  selectInput("heatmap_effect", "Select Effect to Visualize:", choices = NULL),
                  checkboxInput("show_margins", "Show Main Effects in Margins", value = TRUE),
                  helpText("Red indicates positive effect (above average). Blue indicates negative effect (below average).")
                ),
                card(plotlyOutput("plotly_heatmap", height = "600px"))
              )
    ),
    nav_panel("Raw Effect Tables", 
              selectInput("view_effect", "Select Table:", choices = NULL),
              DTOutput("effect_table_out")
    ),
    nav_panel("ANOVA Table", verbatimTextOutput("anova_out")),
    nav_panel("Raw Data Preview", DTOutput("data_preview"))
  )
)

# ==============================================================================
# 4. SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  raw_data <- reactive({
    if (input$data_source == "Use Example Data") return(wheat_yield_data)
    req(input$file_upload)
    read.csv(input$file_upload$datapath)
  })
  
  output$col_selectors <- renderUI({
    req(raw_data())
    cols <- colnames(raw_data())
    tagList(
      selectInput("col_trait", "Trait:", choices=cols, selected=ifelse("Yield" %in% cols, "Yield", cols[1])),
      selectInput("col_geno", "Genotype:", choices=cols, selected=ifelse("Genotype" %in% cols, "Genotype", cols[2])),
      selectInput("col_loc", "Location:", choices=cols, selected=ifelse("Location" %in% cols, "Location", cols[3])),
      selectInput("col_year", "Year:", choices=cols, selected=ifelse("Year" %in% cols, "Year", cols[4])),
      selectInput("col_rep", "Replication:", choices=cols, selected=ifelse("Rep" %in% cols, "Rep", cols[5]))
    )
  })
  
  output$filter_locs <- renderUI({
    req(raw_data(), input$col_loc)
    locs <- unique(raw_data()[[input$col_loc]])
    checkboxGroupInput("selected_locs", "Locations:", choices=locs, selected=locs)
  })
  
  output$filter_years <- renderUI({
    req(raw_data(), input$col_year)
    years <- unique(raw_data()[[input$col_year]])
    checkboxGroupInput("selected_years", "Years (Select 1 for Single-Year):", choices=years, selected=years)
  })
  
  output$data_preview <- renderDT({ datatable(raw_data(), options = list(pageLength = 10, scrollX = TRUE)) })
  
  analysis_results <- eventReactive(input$run_analysis, {
    req(raw_data(), input$col_trait, input$selected_locs, input$selected_years)
    df <- raw_data() %>% filter(!!sym(input$col_loc) %in% input$selected_locs, !!sym(input$col_year) %in% input$selected_years)
    
    # Drop unused factor levels
    df[] <- lapply(df, function(x) if(is.factor(x) || is.character(x)) as.factor(as.character(x)) else x)
    
    year_arg <- if(length(input$selected_years) <= 1) NULL else input$col_year
    
    comstock_robinson_gxe(df, input$col_trait, input$col_geno, input$col_loc, input$col_rep, year_arg, input$method)
  })
  
  observeEvent(analysis_results(), {
    choices <- names(analysis_results()$Effect_Estimates)
    updateSelectInput(session, "heatmap_effect", choices = choices, selected = "GxL")
    updateSelectInput(session, "view_effect", choices = choices)
  })
  
  output$analysis_header <- renderUI({ h3(paste("Model Type:", analysis_results()$Analysis_Type)) })
  output$heritability_out <- renderText({ paste0(round(analysis_results()$Heritability * 100, 2), "%") })
  output$var_comps_out <- renderDT({ datatable(analysis_results()$Variance_Components, options=list(dom='t', paging=F), rownames=F) })
  output$anova_out <- renderPrint({ analysis_results()$ANOVA_Table })
  output$effect_table_out <- renderDT({ datatable(analysis_results()$Effect_Estimates[[input$view_effect]], options=list(pageLength=15)) })
  
  # ==============================================================================
  # HEATMAP ENGINE
  # ==============================================================================
  output$plotly_heatmap <- renderPlotly({
    req(analysis_results(), input$heatmap_effect)
    
    eff_name <- input$heatmap_effect
    eff_list <- analysis_results()$Effect_Estimates
    df <- eff_list[[eff_name]]
    
    g_col <- input$col_geno
    l_col <- input$col_loc
    y_col <- input$col_year
    
    if (eff_name == "G" || eff_name == "L" || eff_name == "Y") {
      # 1D Heatmap
      mat <- as.matrix(df$Effect)
      rownames(mat) <- as.character(df[[1]]) # Ensures text categories on axis
      colnames(mat) <- eff_name
      
    } else {
      # 2D/3D Heatmap
      if (eff_name == "GxL") {
        row_var <- g_col; col_var <- l_col
      } else if (eff_name == "GxY") {
        row_var <- g_col; col_var <- y_col
      } else if (eff_name == "GxLxY") {
        df$Env <- paste(df[[l_col]], df[[y_col]], sep = "_")
        row_var <- g_col; col_var <- "Env"
      }
      
      df_wide <- df %>% select(all_of(c(row_var, col_var, "Effect"))) %>% pivot_wider(names_from = col_var, values_from = Effect)
      mat <- as.matrix(df_wide[, -1])
      rownames(mat) <- df_wide[[1]]
      
      if (input$show_margins) {
        if (row_var == g_col && "G" %in% names(eff_list)) {
          g_margin <- eff_list[["G"]]
          matched_g <- g_margin$Effect[match(rownames(mat), g_margin[[g_col]])]
          mat <- cbind(mat, "Main(G)" = matched_g)
        }
        
        if (eff_name == "GxL" && "L" %in% names(eff_list)) {
          env_margin <- eff_list[["L"]]
          matched_env <- env_margin$Effect[match(colnames(mat)[1:(ncol(mat)-1)], env_margin[[l_col]])]
          mat <- rbind(mat, "Main(L)" = c(matched_env, NA))
        } else if (eff_name == "GxY" && "Y" %in% names(eff_list)) {
          env_margin <- eff_list[["Y"]]
          matched_env <- env_margin$Effect[match(colnames(mat)[1:(ncol(mat)-1)], env_margin[[y_col]])]
          mat <- rbind(mat, "Main(Y)" = c(matched_env, NA))
        }
      }
    }
    
    max_val <- max(abs(mat), na.rm = TRUE)
    
    plot_ly(
      x = colnames(mat), 
      y = rownames(mat), 
      z = mat, 
      type = "heatmap",
      colorscale = "RdBu",
      reversescale = TRUE,
      zmin = -max_val, zmax = max_val,
      xgap = 2, ygap = 2
    ) %>% 
      layout(
        title = paste("Effect Heatmap:", eff_name),
        xaxis = list(title = "", tickangle = 45),
        yaxis = list(title = "", autorange = "reversed"),
        margin = list(l = 50, r = 50, b = 100, t = 50)
      )
  })
}

shinyApp(ui = ui, server = server)