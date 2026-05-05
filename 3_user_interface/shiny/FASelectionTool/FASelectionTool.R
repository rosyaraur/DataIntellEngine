# ==============================================================================
# FAST: Factor Analytic Selection Tools Dashboard (EBLUPs & Advanced Export)
# ==============================================================================
library(shiny)
library(plotly)
library(lme4)

# --- 1. Enhanced Simulation Function (Variable GxE Levels) ---
simulate_breeding_data <- function(n_env = 20, n_geno = 150, seed = 42, gxe_level = "high") {
  set.seed(seed)
  envs <- paste0("E", 1:n_env)
  genos <- paste0("G", 1:n_geno)
  
  # Adjust mega-environments and variance based on requested GxE level
  if (gxe_level == "low") {
    me_count <- 1
    gxe_sd <- 2
  } else if (gxe_level == "medium") {
    me_count <- 2
    gxe_sd <- 5
  } else {
    me_count <- 3
    gxe_sd <- 10
  }
  
  me_assignments <- sample(1:me_count, n_env, replace = TRUE)
  
  geno_base <- rnorm(n_geno, mean = 60, sd = 5) 
  geno_me_effects <- matrix(rnorm(n_geno * me_count, mean = 0, sd = gxe_sd), nrow = n_geno, ncol = me_count)
  
  # Simulating some replications to allow the mixed model to partition variance
  df <- expand.grid(Genotype = genos, Env = envs, Rep = 1:2)
  
  df$Yield <- apply(df, 1, function(row) {
    g_idx <- match(row["Genotype"], genos)
    e_idx <- match(row["Env"], envs)
    me <- me_assignments[e_idx]
    
    base <- geno_base[g_idx]
    spec_effect <- geno_me_effects[g_idx, me]
    env_main_effect <- rnorm(1, 0, 3) 
    noise <- rnorm(1, 0, 2)
    
    return(base + env_main_effect + spec_effect + noise)
  })
  
  df <- as.data.frame(df)
  df$Yield <- as.numeric(df$Yield)
  return(df)
}

# --- 2. FAST Analysis Function (with EBLUPs) ---
fit_fast <- function(data, genotype_col, env_col, yield_col, k = 1) {
  
  data[[genotype_col]] <- as.factor(data[[genotype_col]])
  data[[env_col]] <- as.factor(data[[env_col]])
  
  counts <- table(data[[genotype_col]], data[[env_col]])
  is_replicated <- max(counts) > 1
  
  if (is_replicated) {
    message("Replicated data detected. Fitting (1|Genotype) + (1|Genotype:Env)...")
    form <- as.formula(paste(yield_col, "~", env_col, "+ (1|", genotype_col, ") + (1|", genotype_col, ":", env_col, ")"))
  } else {
    message("Unreplicated data detected. Fitting (1|Genotype) and using residuals for GxE...")
    form <- as.formula(paste(yield_col, "~", env_col, "+ (1|", genotype_col, ")"))
  }
  
  model <- lmer(form, data = data)
  
  random_effs <- predict(model) - predict(model, re.form = NA)
  if (!is_replicated) {
    random_effs <- random_effs + residuals(model)
  }
  
  data$Centered_EBLUP <- random_effs
  gxe_centered <- tapply(data$Centered_EBLUP, list(data[[genotype_col]], data[[env_col]]), mean)
  gxe_centered[is.na(gxe_centered)] <- 0
  
  svd_decomp <- svd(gxe_centered)
  
  var_raw <- svd_decomp$d^2
  var_percent <- (var_raw / sum(var_raw)) * 100
  var_table <- data.frame(
    Factor = paste0("Factor_", 1:length(var_raw)),
    Variance_Percent = round(var_percent, 2),
    Cumulative_Percent = round(cumsum(var_percent), 2)
  )
  
  max_k <- min(nrow(gxe_centered), ncol(gxe_centered), k)
  k <- max_k 
  
  D_sqrt <- diag(sqrt(svd_decomp$d[1:k]), nrow = k)
  geno_scores <- svd_decomp$u[, 1:k, drop = FALSE] %*% D_sqrt
  rownames(geno_scores) <- rownames(gxe_centered)
  colnames(geno_scores) <- paste0("Factor_", 1:k)
  
  env_loadings <- svd_decomp$v[, 1:k, drop = FALSE] %*% D_sqrt
  rownames(env_loadings) <- colnames(gxe_centered)
  colnames(env_loadings) <- paste0("Factor_", 1:k)
  
  op_scores <- geno_scores[, 1]
  reconstructed_gk <- geno_scores %*% t(env_loadings)
  residuals_k <- gxe_centered - reconstructed_gk
  specific_var_k <- rowSums(residuals_k^2)
  stability_k <- 1 / (specific_var_k + 0.01)
  
  fast_df <- data.frame(
    Genotype = rownames(geno_scores),
    OP = as.numeric(op_scores),
    Stability = as.numeric(stability_k)
  )
  
  return(list(
    FAST = fast_df[order(-fast_df$OP), ],
    Genotype_Scores = geno_scores,
    Env_Loadings = env_loadings,
    Variance_Explained = var_table[1:max_k, ],
    GxE_Centered = gxe_centered
  ))
}

# --- 3. Interactive Plotting Function (Includes EBLUPs) ---
plot_fast_interactive <- function(results, type = "selection", top_n = 10, highlight = NULL, digits = 2) {
  if (type == "selection") {
    df <- results$FAST
    df$Rank <- rank(-df$OP)
    df$Status <- "Standard"
    if (!is.null(highlight) && length(highlight) > 0 && highlight[1] != "") {
      df$Status[df$Genotype %in% highlight] <- "Highlighted"
    }
    df$Display_Label <- ifelse(df$Rank <= top_n | df$Status == "Highlighted", as.character(df$Genotype), "")
    
    p <- plot_ly(df, x = ~OP, y = ~Stability, type = 'scatter', mode = 'markers+text',
                 text = ~Display_Label, textposition = "top right", hoverinfo = 'text',
                 hovertext = ~paste("<b>Genotype:</b>", Genotype, "<br><b>OP:</b>", round(OP, digits), "<br><b>Stability:</b>", round(Stability, digits)),
                 color = ~Status, colors = c("Standard" = "#A9A9A980", "Highlighted" = "#FF8C00"),
                 marker = list(size = ~ifelse(Status == "Highlighted", 14, 10), line = list(color = "white", width = 1))) %>%
      layout(title = "Selection Plot", xaxis = list(title = "Overall Performance (F1)"), yaxis = list(title = "Stability Index"))
  } else if (type == "biplot") {
    g_df <- as.data.frame(results$Genotype_Scores)
    g_df$Genotype <- rownames(g_df)
    g_df$Status <- "Standard"
    if (!is.null(highlight) && length(highlight) > 0 && highlight[1] != "") {
      g_df$Status[g_df$Genotype %in% highlight] <- "Highlighted"
    }
    e_df <- as.data.frame(results$Env_Loadings)
    e_df$Env <- rownames(e_df)
    
    p <- plot_ly() %>%
      add_trace(data = g_df, x = ~Factor_1, y = ~Factor_2, type = 'scatter', mode = 'markers+text',
                text = ~ifelse(Status == "Highlighted", Genotype, ""), textposition = "top right",
                hoverinfo = 'text', hovertext = ~paste("<b>Genotype:</b>", Genotype),
                color = ~Status, colors = c("Standard" = "#A9A9A966", "Highlighted" = "#FF8C00"),
                marker = list(size = ~ifelse(Status == "Highlighted", 12, 8)), name = "Genotypes")
    
    for(i in 1:nrow(e_df)) {
      p <- p %>%
        add_segments(x = 0, xend = e_df$Factor_1[i], y = 0, yend = e_df$Factor_2[i],
                     line = list(color = 'steelblue', width = 2), hoverinfo = 'text', text = paste("<b>Env:</b>", e_df$Env[i]), showlegend = FALSE) %>%
        add_annotations(x = e_df$Factor_1[i], y = e_df$Factor_2[i], text = e_df$Env[i], showarrow = FALSE, font = list(color = 'steelblue', size = 12))
    }
    p <- p %>% layout(title = "GxE Biplot", xaxis = list(title = "Factor 1"), yaxis = list(title = "Factor 2"))
  } else if (type == "scree") {
    df <- results$Variance_Explained
    df$Factor <- factor(df$Factor, levels = df$Factor)
    p <- plot_ly(df, x = ~Factor) %>%
      add_bars(y = ~Variance_Percent, name = "Individual", marker = list(color = "steelblue"), hoverinfo = "text", hovertext = ~paste("Variance:", Variance_Percent, "%")) %>%
      add_lines(y = ~Cumulative_Percent, name = "Cumulative", line = list(color = "red", width = 2), hoverinfo = "text", hovertext = ~paste("Cumulative:", Cumulative_Percent, "%")) %>%
      add_markers(y = ~Cumulative_Percent, showlegend = FALSE, marker = list(color = "red", size = 8), hoverinfo = "none") %>%
      layout(title = "Variance Explained", yaxis = list(title = "Variance %"))
  } else if (type %in% c("loadings", "correlation")) {
    if (type == "loadings") {
      mat <- as.matrix(results$Env_Loadings)
      ttl <- "Environment Loadings"
    } else {
      Ge <- results$Env_Loadings %*% t(results$Env_Loadings)
      mat <- cov2cor(Ge)
      ttl <- "Genetic Correlation Matrix"
    }
    mat_r <- round(mat, digits)
    p <- plot_ly(x = colnames(mat), y = rownames(mat), z = mat, type = "heatmap", colorscale = "RdBu", reversescale = TRUE, zmin = -1, zmax = 1) %>%
      layout(title = ttl)
    
    text_labels <- list()
    for (i in 1:nrow(mat)) {
      for (j in 1:ncol(mat)) {
        text_labels[[length(text_labels) + 1]] <- list(
          x = colnames(mat)[j], y = rownames(mat)[i], text = mat_r[i, j],
          showarrow = FALSE, font = list(color = ifelse(abs(mat[i, j]) > 0.5, "white", "black"))
        )
      }
    }
    p <- p %>% layout(annotations = text_labels)
  } else if (type == "eblup") {
    mat <- as.matrix(results$GxE_Centered)
    max_val <- max(abs(mat), na.rm = TRUE)
    
    p <- plot_ly(x = colnames(mat), y = rownames(mat), z = mat, type = "heatmap",
                 colorscale = "RdBu", reversescale = TRUE, zmin = -max_val, zmax = max_val,
                 hovertemplate = paste("<b>Environment:</b> %{x}<br><b>Genotype:</b> %{y}<br><b>EBLUP:</b> %{z:.3f}<extra></extra>")) %>%
      layout(title = "Centered Genotype & GxE EBLUPs",
             xaxis = list(title = "Environment"),
             yaxis = list(title = "Genotype", showticklabels = FALSE))
  }
  return(p)
}

# ==============================================================================
# UI DEFINITION
# ==============================================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .well { background-color: #f8f9fa; border-radius: 8px; border: 1px solid #dee2e6; }
      h2 { color: #2c3e50; font-weight: bold; }
    "))
  ),
  
  titlePanel("FAST Dashboard: Elemental Selection Tools"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Data Source"),
      radioButtons("data_source", "Select Input Method:",
                   choices = c("Demo Mode: Low GxE" = "demo_low",
                               "Demo Mode: Medium GxE" = "demo_medium",
                               "Demo Mode: High GxE" = "demo_high",
                               "Upload User Data (CSV)" = "upload")),
      
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("file_pheno", "Upload Phenotypic Data (.csv)", accept = ".csv"),
        textInput("col_geno", "Column Name: Genotype", value = "Genotype"),
        textInput("col_env", "Column Name: Environment", value = "Env"),
        textInput("col_yield", "Column Name: Trait/Yield", value = "Yield")
      ),
      
      conditionalPanel(
        condition = "input.data_source.startsWith('demo')",
        helpText("Demo modes generate 150 genotypes across 20 environments (with 2 reps). Adjust the GxE level to see how the SVD variance and environmental clustering adapt.")
      ),
      
      hr(),
      h4("Model Parameters"),
      numericInput("k", "Latent Factors (k) to Extract:", value = 2, min = 1, max = 5),
      
      hr(),
      h4("Visualization Settings"),
      textInput("highlight", "Highlight Genotypes (comma separated):", value = "G10, G55, G120"),
      numericInput("top_n", "Label Top N Genotypes:", value = 5, min = 0, max = 50),
      br(),
      actionButton("run_btn", "Run Analysis", icon = icon("play"), class = "btn-primary btn-lg btn-block"),
      
      hr(),
      h4("Export Results"),
      selectInput("download_type", "Select Data to Export:",
                  choices = c("FAST Rankings (OP & Stability)" = "fast",
                              "Centered EBLUP Matrix (G + GxE)" = "eblup",
                              "Genotype Scores (All Factors)" = "scores",
                              "Environment Loadings (All Factors)" = "loadings",
                              "Genetic Correlation Matrix (Env x Env)" = "correlations",
                              "Variance Explained (Scree Data)" = "variance")),
      downloadButton("download_data", "Download Data (.csv)", class = "btn-success btn-block")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Selection", br(), plotlyOutput("plot_selection", height = "600px")),
        tabPanel("GxE Biplot", br(), plotlyOutput("plot_biplot", height = "600px")),
        tabPanel("EBLUP Heatmap", br(), plotlyOutput("plot_eblup", height = "600px")),
        tabPanel("Env Loadings", br(), plotlyOutput("plot_loadings", height = "600px")),
        tabPanel("Env Correlations", br(), plotlyOutput("plot_correlation", height = "600px")),
        tabPanel("Scree Plot", br(), plotlyOutput("plot_scree", height = "600px")),
        tabPanel("Data Preview", br(), dataTableOutput("data_preview")) 
      )
    )
  )
)

# ==============================================================================
# SERVER DEFINITION
# ==============================================================================
server <- function(input, output, session) {
  
  dataset <- reactive({
    if (input$data_source == "demo_low") {
      return(simulate_breeding_data(n_env = 20, n_geno = 150, seed = 42, gxe_level = "low"))
    } else if (input$data_source == "demo_medium") {
      return(simulate_breeding_data(n_env = 20, n_geno = 150, seed = 42, gxe_level = "medium"))
    } else if (input$data_source == "demo_high") {
      return(simulate_breeding_data(n_env = 20, n_geno = 150, seed = 42, gxe_level = "high"))
    } else {
      req(input$file_pheno)
      tryCatch({
        df <- read.csv(input$file_pheno$datapath)
        return(df)
      }, error = function(e) {
        showNotification("Error reading CSV file.", type = "error")
        return(NULL)
      })
    }
  })
  
  model_results <- eventReactive(input$run_btn, {
    df <- dataset()
    req(df) 
    
    # Optional dependency check
    if (!requireNamespace("lme4", quietly = TRUE)) {
      showNotification("Package 'lme4' is required but not installed.", type = "error")
      return(NULL)
    }
    
    if (startsWith(input$data_source, "demo")) {
      fit_fast(data = df, genotype_col = "Genotype", env_col = "Env", yield_col = "Yield", k = input$k)
    } else {
      req(input$col_geno, input$col_env, input$col_yield)
      if (!all(c(input$col_geno, input$col_env, input$col_yield) %in% colnames(df))) {
        showNotification("Column names do not match the uploaded dataset.", type = "error")
        return(NULL)
      }
      fit_fast(data = df, genotype_col = input$col_geno, env_col = input$col_env, yield_col = input$col_yield, k = input$k)
    }
  }, ignoreNULL = FALSE)
  
  clean_highlights <- reactive({
    req(input$highlight)
    trimws(unlist(strsplit(input$highlight, ",")))
  })
  
  output$data_preview <- renderDataTable({ head(dataset(), 50) })
  
  output$plot_selection <- renderPlotly({
    req(model_results())
    plot_fast_interactive(model_results(), type = "selection", top_n = input$top_n, highlight = clean_highlights())
  })
  output$plot_biplot <- renderPlotly({
    req(model_results())
    plot_fast_interactive(model_results(), type = "biplot", highlight = clean_highlights())
  })
  output$plot_eblup <- renderPlotly({
    req(model_results())
    plot_fast_interactive(model_results(), type = "eblup")
  })
  output$plot_loadings <- renderPlotly({
    req(model_results())
    plot_fast_interactive(model_results(), type = "loadings")
  })
  output$plot_correlation <- renderPlotly({
    req(model_results())
    plot_fast_interactive(model_results(), type = "correlation")
  })
  output$plot_scree <- renderPlotly({
    req(model_results())
    plot_fast_interactive(model_results(), type = "scree")
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      prefix <- switch(input$download_type,
                       "fast" = "FAST_Rankings_",
                       "eblup" = "Centered_EBLUPs_",
                       "scores" = "Genotype_Scores_",
                       "loadings" = "Env_Loadings_",
                       "correlations" = "Genetic_Correlations_",
                       "variance" = "Variance_Explained_")
      paste0(prefix, Sys.Date(), ".csv")
    },
    content = function(file) {
      req(model_results()) 
      res <- model_results()
      
      if (input$download_type == "fast") {
        out_df <- res$FAST
      } else if (input$download_type == "eblup") {
        out_df <- as.data.frame(res$GxE_Centered)
        out_df <- cbind(Genotype = rownames(out_df), out_df)
      } else if (input$download_type == "scores") {
        out_df <- as.data.frame(res$Genotype_Scores)
        out_df <- cbind(Genotype = rownames(out_df), out_df)
      } else if (input$download_type == "loadings") {
        out_df <- as.data.frame(res$Env_Loadings)
        out_df <- cbind(Environment = rownames(out_df), out_df)
      } else if (input$download_type == "correlations") {
        Ge <- res$Env_Loadings %*% t(res$Env_Loadings)
        cor_mat <- cov2cor(Ge)
        out_df <- as.data.frame(cor_mat)
        out_df <- cbind(Environment = rownames(out_df), out_df)
      } else if (input$download_type == "variance") {
        out_df <- res$Variance_Explained
      }
      
      write.csv(out_df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)