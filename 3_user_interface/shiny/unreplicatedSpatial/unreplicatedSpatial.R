library(shiny)
library(ggplot2)
library(gridExtra)
library(FNN)
library(lme4)
library(SpATS)
library(mgcv)
library(dplyr)
library(DT)
library(bslib)

# ==========================================
# 1. CORE FUNCTION 
# ==========================================
adjust_spatial_unreplicated <- function(field_data, method = "spats", knn_k = 4) {
  
  if(!all(c("Row", "Col", "yield", "is_check", "family_id") %in% names(field_data))) {
    stop("Data must contain columns: Row, Col, yield, is_check, family_id")
  }
  
  field_data$Row_f <- as.factor(field_data$Row)
  field_data$Col_f <- as.factor(field_data$Col)
  field_data$genotype <- as.factor(ifelse(field_data$is_check, "CHECK", as.character(field_data$family_id)))
  
  check_data <- field_data[field_data$is_check == TRUE, ]
  
  if(nrow(check_data) == 0) stop("No check plots found. Ensure 'is_check' column contains TRUE values.")
  
  # ==========================================
  # SPATIAL MODELING
  # ==========================================
  
  if (method == "loess") {
    model <- loess(yield ~ Row * Col, data = check_data, span = 0.3, control = loess.control(surface = "direct"))
    raw_pred <- as.numeric(predict(model, newdata = field_data))
    field_data$fitted_trend <- raw_pred - mean(raw_pred, na.rm = TRUE) 
    
  } else if (method == "knn") {
    knn_model <- knn.reg(train = check_data[, c("Row", "Col")], test = field_data[, c("Row", "Col")], y = check_data$yield, k = knn_k)
    raw_pred <- knn_model$pred
    field_data$fitted_trend <- raw_pred - mean(raw_pred, na.rm = TRUE) 
    
  } else if (method == "rowcol") {
    model <- lmer(yield ~ genotype + (1|Row_f) + (1|Col_f), data = field_data)
    field_data$fitted_trend <- predict(model, re.form = ~(1|Row_f) + (1|Col_f)) 
    
  } else if (method == "spats") {
    model <- SpATS(response = "yield", spatial = ~ PSANOVA(Col, Row, nseg = c(20, 20)), genotype = "genotype", genotype.as.random = FALSE, data = field_data)
    spat_pred <- predict(model, which = c("Col", "Row"))
    pred_col <- if("predicted.values" %in% names(spat_pred)) "predicted.values" else "predicted"
    
    spat_sub <- spat_pred[, c("Col", "Row", pred_col)]
    field_data <- dplyr::left_join(field_data, spat_sub, by = c("Col", "Row"))
    
    field_data$fitted_trend <- field_data[[pred_col]] - mean(field_data[[pred_col]], na.rm = TRUE)
    field_data[[pred_col]] <- NULL 
    
  } else if (method == "gam") {
    model <- gam(yield ~ genotype + s(Col, Row), data = field_data)
    terms_pred <- predict(model, type = "terms")
    spatial_col <- grep("s\\(", colnames(terms_pred))
    field_data$fitted_trend <- as.numeric(terms_pred[, spatial_col]) 
    
  } else {
    stop("Invalid method.")
  }
  
  field_data$adjusted_yield <- field_data$yield - field_data$fitted_trend
  
  # ==========================================
  # VISUALIZATIONS
  # ==========================================
  
  base_theme <- theme_minimal() + theme(panel.grid = element_blank(), legend.position = "bottom")
  
  plot_raw <- ggplot(field_data, aes(x = Col, y = Row, fill = yield)) +
    geom_tile() + scale_fill_viridis_c(option = "magma") +
    labs(title = "Raw Yield", x = "Col", y = "Row", fill = "Yield") + base_theme
  
  plot_adj <- ggplot(field_data, aes(x = Col, y = Row, fill = adjusted_yield)) +
    geom_tile() + scale_fill_viridis_c(option = "magma") +
    labs(title = paste("Adjusted Yield (", toupper(method), ")", sep=""), x = "Col", y = "Row", fill = "Adj Yield") + base_theme
  
  plot_res <- ggplot(field_data, aes(x = Col, y = Row, fill = fitted_trend)) +
    geom_tile() + scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue", midpoint = 0) +
    labs(title = "Modeled Spatial Trend", x = "Col", y = "Row", fill = "Trend Effect") + base_theme
  
  return(list(data = field_data, plot_raw = plot_raw, plot_adj = plot_adj, plot_res = plot_res))
}

# ==========================================
# 2. SHINY UI
# ==========================================
ui <- page_sidebar(
  title = "Field Trial Spatial Adjustment",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    # NEW: Updated radio buttons to include the 15-row spacing option
    radioButtons("data_source", "Data Source:", 
                 choices = c("Simulated (Checks every 30 rows)" = "sim_30", 
                             "Simulated (Checks every 15 rows)" = "sim_15",
                             "Upload CSV" = "upload")),
    conditionalPanel(
      condition = "input.data_source == 'upload'",
      fileInput("file1", "Choose CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      helpText("Required columns: Row, Col, yield, is_check (TRUE/FALSE), family_id")
    ),
    selectInput("method", "Spatial Model:", 
                choices = c("SpATS (2D Spline)" = "spats", 
                            "GAM (2D Smooth)" = "gam", 
                            "Row/Col Mixed Model" = "rowcol", 
                            "LOESS Surface" = "loess", 
                            "KNN Interpolation" = "knn")),
    conditionalPanel(
      condition = "input.method == 'knn'",
      numericInput("knn_k", "Number of Neighbors (K):", value = 6, min = 1, max = 20)
    ),
    actionButton("run", "Run Analysis", class = "btn-primary", width = "100%"),
    
    hr(),
    downloadButton("downloadData", "Download Adjusted Data", class = "btn-success", width = "100%")
  ),
  
  card(
    card_header("Spatial Heatmaps"),
    plotOutput("heatmaps", height = "500px")
  ),
  
  card(
    card_header("Breeder Decision Table (Family Rankings)"),
    helpText("This table highlights how the spatial adjustment changed the ranking of your experimental families. Look for families with large positive rank shifts—these were likely penalized by poor field placement in the raw data."),
    DTOutput("decision_table")
  )
)

# ==========================================
# 3. SHINY SERVER
# ==========================================
server <- function(input, output, session) {
  
  raw_data <- reactive({
    # NEW: Handle both simulation options dynamically
    if (input$data_source %in% c("sim_30", "sim_15")) {
      set.seed(42)
      
      n_families <- 90
      n_exp_rows <- 300
      total_cols <- 90
      check_rows_gap <- 2
      
      # Determine spacing based on user selection
      rows_per_block <- ifelse(input$data_source == "sim_30", 30, 15)
      
      # Calculate total rows accounting for the extra checks
      n_check_intervals <- n_exp_rows / rows_per_block 
      total_rows <- n_exp_rows + (n_check_intervals * check_rows_gap) 
      
      family_effects <- rnorm(n_families, mean = 50, sd = 5)
      check_effect <- 55 
      
      grid <- expand.grid(Row = 1:total_rows, Col = 1:total_cols)
      grid$fertility <- with(grid, 10 * sin(Row/20) * cos(Col/15) + 5 * sin(Row/5) + rnorm(nrow(grid), 0, 2))
      
      # Place checks dynamically based on the block spacing
      grid$is_check <- FALSE
      for(i in 1:n_check_intervals) {
        start_check <- i * rows_per_block + (i - 1) * check_rows_gap + 1
        grid$is_check[grid$Row %in% c(start_check, start_check + 1)] <- TRUE
      }
      
      grid$family_id <- NA
      non_check_indices <- which(!grid$is_check)
      grid$family_id[non_check_indices] <- rep(1:n_families, length.out = length(non_check_indices))
      
      grid$yield <- ifelse(grid$is_check, check_effect + grid$fertility, family_effects[grid$family_id] + grid$fertility)
      return(grid)
      
    } else {
      req(input$file1)
      df <- read.csv(input$file1$datapath)
      return(df)
    }
  })
  
  analysis_results <- eventReactive(input$run, {
    req(raw_data())
    withProgress(message = paste('Fitting', input$method, 'model...'), value = 0.5, {
      results <- adjust_spatial_unreplicated(raw_data(), method = input$method, knn_k = input$knn_k)
      return(results)
    })
  }, ignoreNULL = FALSE) 
  
  output$heatmaps <- renderPlot({
    req(analysis_results())
    res <- analysis_results()
    grid.arrange(res$plot_raw, res$plot_adj, res$plot_res, ncol = 3)
  })
  
  output$decision_table <- renderDT({
    req(analysis_results())
    adj_data <- analysis_results()$data
    
    family_summary <- adj_data %>%
      filter(!is_check) %>%
      group_by(family_id) %>%
      summarize(
        Raw_Mean_Yield = round(mean(yield, na.rm = TRUE), 2),
        Adj_Mean_Yield = round(mean(adjusted_yield, na.rm = TRUE), 2)
      ) %>%
      mutate(
        Raw_Rank = rank(-Raw_Mean_Yield, ties.method = "first"),
        Adj_Rank = rank(-Adj_Mean_Yield, ties.method = "first"),
        Rank_Shift = Raw_Rank - Adj_Rank 
      ) %>%
      arrange(Adj_Rank)
    
    datatable(family_summary, 
              rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE),
              colnames = c("Family ID", "Raw Yield Mean", "Adjusted Yield Mean", "Raw Rank", "Adjusted Rank", "Rank Shift")) %>%
      formatStyle(
        'Rank_Shift',
        color = styleInterval(c(-0.1, 0.1), c('red', 'black', 'green')),
        fontWeight = 'bold'
      )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("adjusted_field_data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(analysis_results())
      data_to_save <- analysis_results()$data
      write.csv(data_to_save, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)