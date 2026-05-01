#install.packages(c("shiny", "DT", "ggplot2", "dplyr", "bslib"))
#install.packages("ggdendro")
#install.packages("leaflet")
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(bslib)
library(ggdendro)
library(leaflet)

# =====================================================================
# 1. GEOGRAPHY & DATA SIMULATION
# =====================================================================
get_default_geo_coords <- function() {
  data.frame(
    Location = c("Ames", "Cedar Rapids", "Des Moines", "Waterloo", "Iowa City", "Sioux City", "Fort Dodge",
                 "Rochester", "Mankato", "St. Cloud", "Moorhead", "Marshall", "Owatonna", "Albert Lea",
                 "Lincoln", "Grand Island", "Kearney", "North Platte", "Hastings", "Norfolk"),
    Lat = c(42.0308, 41.9779, 41.5868, 42.4928, 41.6611, 42.5000, 42.5050,
            44.0121, 44.1636, 45.5579, 46.8739, 44.4469, 44.0841, 43.6480,
            40.8136, 40.9250, 40.6995, 41.1403, 40.5860, 42.0283),
    Lon = c(-93.6319, -91.6656, -93.6250, -92.3426, -91.5302, -96.4003, -94.1838,
            -92.4802, -93.9994, -94.1632, -96.7678, -95.7883, -93.2260, -93.3683,
            -96.7026, -98.3420, -99.0815, -100.7601, -98.3899, -97.4170),
    stringsAsFactors = FALSE
  )
}

simulate_met_data <- function(n_years = 3, n_locs = 20, n_reps = 3, n_checks = 8, n_lines_per_year = 92) {
  set.seed(Sys.time()) 
  geo_data <- get_default_geo_coords()
  n_locs <- min(n_locs, nrow(geo_data))
  locs <- geo_data$Location[1:n_locs]
  
 # mu = 65.0; sd_loc = 6.0; sd_year = 4.0; sd_loc_year = 2.0
 # sd_rep = 1.5; sd_g_check = 5.0; sd_g_line = 4.5; sd_gxe = 2.5; sd_error = 3.0
  # --- NEW PARAMETERS (High GxE & Environmental Variability) ---
  mu = 65.0; sd_loc = 25.0; sd_year = 8.0; sd_loc_year = 6.0
  sd_rep = 2.0; sd_g_check = 5.0; sd_g_line = 5.0; sd_gxe = 30.0; sd_error = 6.0
  
  check_names <- paste0("Check_", sprintf("%02d", 1:n_checks))
  check_eff <- setNames(rnorm(n_checks, 0, sd_g_check), check_names)
  
  df_checks <- expand.grid(Year = 2022:(2022 + n_years - 1), Genotype = check_names, stringsAsFactors = FALSE)
  df_checks$Type <- "Check"
  
  list_lines <- lapply(2022:(2022 + n_years - 1), function(y) {
    line_names <- paste0("Line_", y, "_", sprintf("%02d", 1:n_lines_per_year))
    data.frame(Year = y, Genotype = line_names, Type = "Experimental_Line", stringsAsFactors = FALSE)
  })
  df_lines <- do.call(rbind, list_lines)
  
  df_geno <- rbind(df_checks, df_lines)
  line_eff <- setNames(rnorm(length(unique(df_lines$Genotype)), 0, sd_g_line), unique(df_lines$Genotype))
  df_geno$G_Effect <- c(check_eff, line_eff)[df_geno$Genotype]
  
  df_env <- expand.grid(Year = 2022:(2022 + n_years - 1), Location = locs, Rep = 1:n_reps, stringsAsFactors = FALSE)
  df_env$Environment <- paste0(df_env$Location, "_Y", df_env$Year)
  
  eff_loc <- setNames(rnorm(n_locs, 0, sd_loc), locs)
  eff_year <- setNames(rnorm(n_years, 0, sd_year), 2022:(2022 + n_years - 1))
  eff_env <- setNames(rnorm(length(unique(df_env$Environment)), 0, sd_loc_year), unique(df_env$Environment))
  eff_rep <- setNames(rnorm(nrow(df_env), 0, sd_rep), paste0(df_env$Environment, "_R", df_env$Rep))
  
  df <- merge(df_env, df_geno, by = "Year", all.x = TRUE)
  df$Loc_Effect <- eff_loc[df$Location]
  df$Year_Effect <- eff_year[as.character(df$Year)]
  df$Env_Effect <- eff_env[df$Environment]
  df$Rep_Effect <- eff_rep[paste0(df$Environment, "_R", df$Rep)]
  
  df$GxE_ID <- paste0(df$Genotype, "_", df$Environment)
  df$GxE_Effect <- setNames(rnorm(length(unique(df$GxE_ID)), 0, sd_gxe), unique(df$GxE_ID))[df$GxE_ID]
  df$Error <- rnorm(nrow(df), 0, sd_error)
  
  df$Yield_bu_ac <- pmax(10.0, mu + df$Loc_Effect + df$Year_Effect + df$Env_Effect + 
                           df$Rep_Effect + df$G_Effect + df$GxE_Effect + df$Error)
  
  df <- df[, c("Year", "Location", "Environment", "Rep", "Genotype", "Type", "Yield_bu_ac")]
  df$Yield_bu_ac <- round(df$Yield_bu_ac, 2)
  return(df)
}

# =====================================================================
# 2. USER INTERFACE
# =====================================================================
ui <- page_sidebar(
  title = "Trial Netwrok Optimizer",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    width = 300,
    radioButtons("data_source", "Data Source:", choices = c("Simulate" = "sim", "Upload Target Data" = "upload")),
    
    conditionalPanel("input.data_source == 'upload'",
                     fileInput("file_yield", "1. Upload Yield Data (.csv)", accept = c(".csv")),
                     fileInput("file_geo", "2. Upload Geo-Coords (.csv)", accept = c(".csv")),
                     helpText("Note: The 'Location' column must match exactly between the two files.")),
    
    conditionalPanel("input.data_source == 'sim'",
                     sliderInput("n_locs", "Number of Locations:", 5, 20, 15),
                     actionButton("btn_simulate", "Generate Network Data", class = "btn-primary w-100"))
  ),
  
  navset_card_tab(
    id = "main_tabs", 
    nav_panel("1. Data Explorer", DTOutput("data_table")),
    
    nav_panel("2. Environment Clustering", 
              h4("Identify Redundant Testing Nodes"),
              p("Click directly on a location name below the dendrogram branch to instantly calculate the network prediction accuracy if that site were defunded."),
              plotOutput("cluster_plot", height = "500px", click = "dendro_click")),
    
    nav_panel("3. Spatial Network Map", 
              h4("Interactive Map Analysis"),
              p("Run the Full Network Optimization (Tab 4) to color-code this map by prediction accuracy impact. Click any marker to test dropping it."),
              leafletOutput("network_map", height = "600px")),
    
    nav_panel("4. Optimization & Drop Analysis", 
              h4("Targeted Drop Insight"),
              uiOutput("single_drop_result"),
              hr(),
              h4("Full Network LOLO Loop"),
              p("Calculates the network impact of dropping ANY single location. This populates the analytics map in Tab 3."),
              actionButton("run_lolo", "Run Full Optimization Loop", class = "btn-success"),
              br(), br(),
              plotOutput("lolo_plot", height = "500px"))
  )
)

# =====================================================================
# 3. SERVER LOGIC
# =====================================================================
server <- function(input, output, session) {
  
  # Reactive Variables
  dataset <- reactiveVal(NULL)
  geo_dataset <- reactiveVal(NULL)
  dendro_labels <- reactiveVal(NULL) 
  gold_standard_blups <- reactiveVal(NULL) 
  selected_loc <- reactiveVal(NULL)
  lolo_results <- reactiveVal(NULL)
  
  # --- Data Source: Simulate ---
  observeEvent(input$btn_simulate, {
    showNotification("Generating trial data...", type = "message")
    dataset(simulate_met_data(n_locs = input$n_locs))
    geo_dataset(get_default_geo_coords())
    
    # Reset downstream analyses
    gold_standard_blups(NULL)
    selected_loc(NULL)
    lolo_results(NULL)
  }, ignoreNULL = FALSE)
  
  # --- Data Source: Upload ---
  observeEvent(input$file_yield, {
    req(input$file_yield)
    dataset(read.csv(input$file_yield$datapath))
    gold_standard_blups(NULL)
    selected_loc(NULL)
    lolo_results(NULL)
  })
  
  observeEvent(input$file_geo, {
    req(input$file_geo)
    geo_dataset(read.csv(input$file_geo$datapath))
  })
  
  # --- Tab 1: Data Table ---
  output$data_table <- renderDT({
    req(dataset())
    datatable(dataset(), options = list(pageLength = 10, scrollX = TRUE), filter = "top")
  })
  
  # --- Tab 2: Clustering ---
  output$cluster_plot <- renderPlot({
    req(dataset())
    df <- dataset()
    
    check_data <- df %>%
      filter(Type == "Check" | Type == "check") %>% 
      group_by(Location, Genotype) %>%
      summarize(Mean_Yield = mean(Yield_bu_ac, na.rm = TRUE), .groups = "drop")
    
    if(nrow(check_data) == 0) return(plot(1, type="n", main="No checks found for clustering."))
    
    loc_matrix_df <- check_data %>% pivot_wider(names_from = Genotype, values_from = Mean_Yield) %>% drop_na()
    loc_matrix <- as.matrix(loc_matrix_df[, -1])
    rownames(loc_matrix) <- loc_matrix_df$Location
    
    hc <- hclust(dist(loc_matrix, method = "euclidean"), method = "ward.D2")
    dendr_data <- dendro_data(hc, type = "rectangle")
    dendro_labels(label(dendr_data))
    
    ggplot() +
      geom_segment(data = segment(dendr_data), aes(x = x, y = y, xend = xend, yend = yend), size = 0.8) +
      geom_text(data = label(dendr_data), aes(x = x, y = -0.5, label = label), hjust = 1, angle = 90, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0.4, 0.1))) +
      theme_dendro() + labs(title = "Hierarchical Clustering of Environments")
  })
  
  observeEvent(input$dendro_click, {
    req(dendro_labels())
    clicked_x <- round(input$dendro_click$x)
    labels_df <- dendro_labels()
    if(clicked_x >= 1 && clicked_x <= nrow(labels_df)) {
      selected_loc(labels_df$label[labels_df$x == clicked_x])
      updateTabsetPanel(session, "main_tabs", selected = "4. Optimization & Drop Analysis")
    }
  })
  
  # --- Tab 3: Advanced Spatial Map ---
  output$network_map <- renderLeaflet({
    req(dataset(), geo_dataset())
    active_locs <- unique(dataset()$Location)
    map_df <- geo_dataset() %>% filter(Location %in% active_locs)
    
    # If LOLO has been run, colorize the map based on accuracy impact
    if (!is.null(lolo_results())) {
      map_df <- map_df %>% left_join(lolo_results(), by = c("Location" = "Dropped_Location"))
      
      # Color Palette: Red (Critical) to Green (Redundant)
      pal <- colorNumeric(palette = "RdYlGn", domain = map_df$Prediction_Accuracy)
      
      leaflet(map_df) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(~Lon, ~Lat, layerId = ~Location, 
                         label = ~paste0(Location, " | Impact: ", round(Prediction_Accuracy, 4)),
                         color = ~pal(Prediction_Accuracy), fillOpacity = 0.9, radius = 10, stroke = TRUE, weight = 2) %>%
        addLegend("bottomright", pal = pal, values = ~Prediction_Accuracy, title = "Accuracy if Dropped")
    } else {
      # Default view if no analytics have been run yet
      leaflet(map_df) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(~Lon, ~Lat, layerId = ~Location, label = ~Location,
                         color = "#2c3e50", fillOpacity = 0.7, radius = 8)
    }
  })
  
  observeEvent(input$network_map_marker_click, {
    selected_loc(input$network_map_marker_click$id)
    updateTabsetPanel(session, "main_tabs", selected = "4. Optimization & Drop Analysis")
  })
  
  # --- Tab 4: Single Drop Analysis UI ---
  output$single_drop_result <- renderUI({
    req(selected_loc(), dataset())
    target_location <- selected_loc()
    df <- dataset()
    
    withProgress(message = paste('Testing Drop:', target_location), value = 0.5, {
      
      if(is.null(gold_standard_blups())) {
        model_full <- lmer(Yield_bu_ac ~ (1|Genotype) + (1|Environment), data = df)
        blups_full <- ranef(model_full)$Genotype
        blups_full$Genotype <- rownames(blups_full)
        colnames(blups_full)[1] <- "BLUP_Full"
        gold_standard_blups(blups_full)
      }
      
      reduced_data <- df %>% filter(Location != target_location)
      model_reduced <- lmer(Yield_bu_ac ~ (1|Genotype) + (1|Environment), data = reduced_data)
      blups_reduced <- ranef(model_reduced)$Genotype
      blups_reduced$Genotype <- rownames(blups_reduced)
      colnames(blups_reduced)[1] <- "BLUP_Reduced"
      
      comparison <- merge(gold_standard_blups(), blups_reduced, by = "Genotype")
      acc <- cor(comparison$BLUP_Full, comparison$BLUP_Reduced)
      
      color_class <- ifelse(acc > 0.995, "text-success", ifelse(acc > 0.985, "text-warning", "text-danger"))
      status_text <- ifelse(acc > 0.995, "Redundant (Safe to Drop)", ifelse(acc > 0.985, "Moderate Impact", "Critical Anchor (Keep)"))
      
      HTML(paste0("<div class='card p-3 shadow-sm border-start border-5' style='border-color: #3498db !important;'>",
                  "<h4>Results for: <b>", target_location, "</b></h4>",
                  "<h3>Accuracy: <span class='", color_class, "'>", round(acc, 4), "</span></h3>",
                  "<p class='text-muted mb-0'><strong>Status:</strong> ", status_text, "</p></div>"))
    })
  })
  
  # --- Tab 4: Full Network Model Loop ---
  observeEvent(input$run_lolo, {
    req(dataset())
    df <- dataset()
    locations <- unique(df$Location)
    n_locs <- length(locations)
    
    withProgress(message = 'Running Mixed Models...', value = 0, {
      if(is.null(gold_standard_blups())) {
        incProgress(0.1, detail = "Fitting Gold Standard...")
        model_full <- lmer(Yield_bu_ac ~ (1|Genotype) + (1|Environment), data = df)
        blups_full <- ranef(model_full)$Genotype
        blups_full$Genotype <- rownames(blups_full)
        colnames(blups_full)[1] <- "BLUP_Full"
        gold_standard_blups(blups_full)
      }
      
      results <- data.frame(Dropped_Location = character(), Prediction_Accuracy = numeric())
      
      for (i in seq_along(locations)) {
        loc <- locations[i]
        incProgress(0.9 / n_locs, detail = paste("Dropping", loc))
        
        reduced_data <- df %>% filter(Location != loc)
        model_reduced <- lmer(Yield_bu_ac ~ (1|Genotype) + (1|Environment), data = reduced_data)
        blups_reduced <- ranef(model_reduced)$Genotype
        blups_reduced$Genotype <- rownames(blups_reduced)
        colnames(blups_reduced)[1] <- "BLUP_Reduced"
        
        comparison <- merge(gold_standard_blups(), blups_reduced, by = "Genotype")
        results <- rbind(results, data.frame(Dropped_Location = loc, Prediction_Accuracy = cor(comparison$BLUP_Full, comparison$BLUP_Reduced)))
      }
      lolo_results(results)
    })
  })
  
  # --- Tab 4: Full Analysis Lollipop Plot ---
  output$lolo_plot <- renderPlot({
    req(lolo_results())
    res <- lolo_results()
    min_acc <- floor(min(res$Prediction_Accuracy, na.rm = TRUE) * 1000) / 1000 - 0.002
    
    ggplot(res, aes(x = Prediction_Accuracy, y = reorder(Dropped_Location, -Prediction_Accuracy), color = Prediction_Accuracy)) +
      geom_segment(aes(x = min_acc, xend = Prediction_Accuracy, y = reorder(Dropped_Location, -Prediction_Accuracy), yend = reorder(Dropped_Location, -Prediction_Accuracy)), size = 1.5) +
      geom_point(size = 5) +
      scale_color_viridis_c(direction = -1, option = "plasma") +
      coord_cartesian(xlim = c(min_acc, 1.0)) + 
      theme_minimal(base_size = 14) + labs(y = "Dropped Location", x = "Network Prediction Accuracy") + theme(legend.position = "none")
  })
}

shinyApp(ui, server)