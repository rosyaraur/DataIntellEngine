# Install required packages if missing:
# install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "corrplot", "viridis", "plotly"))

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(viridis)
library(plotly)

# ---------------------------------------------------------
# 1. Internal Function to Simulate Spatial Data
# ---------------------------------------------------------
generate_simulated_data <- function() {
  set.seed(42)
  locations <- paste0("Loc_", 1:5)
  genotypes <- paste0("G", sprintf("%02d", 1:40))
  traits <- c("Yield_kg_ha", "Plant_Height_cm")
  
  n_rows <- 8
  n_ranges <- 5
  
  df_list <- list()
  for(t in traits) {
    loc_means <- setNames(rnorm(5, mean = if(t=="Yield_kg_ha") 5000 else 120, sd = 500), locations)
    
    for(l in locations) {
      loc_df <- expand.grid(Row = 1:n_rows, Range = 1:n_ranges)
      loc_df$Genotype <- sample(genotypes) 
      loc_df$Loc <- l
      loc_df$Trait <- t
      
      true_g_dict <- setNames(rnorm(length(genotypes), mean = 0, sd = if(t=="Yield_kg_ha") 400 else 10), genotypes)
      gxe_noise_dict <- setNames(rnorm(length(genotypes), mean = 0, sd = if(t=="Yield_kg_ha") 200 else 5), genotypes)
      
      spatial_gradient <- (loc_df$Row * (if(t=="Yield_kg_ha") 30 else 0.5)) + 
        (loc_df$Range * (if(t=="Yield_kg_ha") 40 else 0.8))
      
      true_g <- true_g_dict[loc_df$Genotype]
      gxe_noise <- gxe_noise_dict[loc_df$Genotype]
      
      loc_df$Raw <- loc_means[l] + true_g + gxe_noise + spatial_gradient
      loc_df$BLUP <- true_g + (gxe_noise * 0.3) 
      loc_df$Residual <- loc_df$Raw - (loc_means[l] + loc_df$BLUP)
      
      df_list[[length(df_list) + 1]] <- loc_df
    }
  }
  
  # Reorder columns to make it look like a standard data entry sheet
  final_df <- bind_rows(df_list) %>%
    select(Trait, Loc, Row, Range, Genotype, Raw, BLUP, Residual)
  
  return(final_df)
}

# ---------------------------------------------------------
# 2. Define User Interface (UI)
# ---------------------------------------------------------
ui <- fluidPage(
  titlePanel("Multi-Environment Trial (MET) Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Data Source"),
      radioButtons("data_source", "Select Data Origin:",
                   choices = c("Use Simulated Example", "Upload Own Dataset"),
                   selected = "Use Simulated Example"),
      
      # --- NEW: Download Button ---
      downloadButton("download_sim_data", "Download Example Data Template"),
      br(), br(),
      
      conditionalPanel(
        condition = "input.data_source == 'Upload Own Dataset'",
        fileInput("file_upload", "Upload CSV File", accept = c(".csv")),
        helpText(strong("Expected CSV Columns:"), br(),
                 "Trait, Loc, Row, Range, Genotype, Raw, BLUP, Residual")
      ),
      
      hr(),
      h4("2. Display Settings"),
      selectInput("selected_trait", "Select Trait:", choices = NULL),
      selectInput("selected_metric", "Select Metric:", 
                  choices = c("Raw", "BLUP", "Residual")),
      
      hr(),
      h4("3. Spatial Field Settings"),
      selectInput("selected_loc", "Select Location:", choices = NULL),
      
      hr(),
      h4("4. Genotype Comparison Settings"),
      helpText("Select one or more genotypes (or checks) to compare against the environmental average."),
      selectizeInput("selected_genos", "Select Genotypes/Checks:", 
                     choices = NULL, multiple = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Spatial Field Map", 
                 br(),
                 plotlyOutput("heatmapPlot", height = "600px")),
        
        tabPanel("Location Correlation", 
                 br(),
                 plotOutput("corrPlot", height = "600px")),
        
        tabPanel("Genotype vs Env Average", 
                 br(),
                 plotlyOutput("envPlot", height = "600px"))
      )
    )
  )
)

# ---------------------------------------------------------
# 3. Define Server Logic
# ---------------------------------------------------------
server <- function(input, output, session) {
  
  # --- NEW: Download Handler ---
  output$download_sim_data <- downloadHandler(
    filename = function() {
      paste("MET_Example_Data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(generate_simulated_data(), file, row.names = FALSE)
    }
  )
  
  # Data loader
  app_data <- reactive({
    if (input$data_source == "Use Simulated Example") {
      return(generate_simulated_data())
    } else {
      req(input$file_upload)
      tryCatch({
        df <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
        return(df)
      }, error = function(e) {
        showNotification("Error reading CSV.", type = "error")
        return(NULL)
      })
    }
  })
  
  # Update Dropdowns dynamically
  observeEvent(app_data(), {
    req(app_data()) 
    updateSelectInput(session, "selected_trait", choices = unique(app_data()$Trait))
    updateSelectInput(session, "selected_loc", choices = unique(app_data()$Loc))
    
    genos <- unique(app_data()$Genotype)
    updateSelectizeInput(session, "selected_genos", choices = genos, selected = genos[1:3])
  })
  
  # --- Plot 1: Spatial Field Map ---
  map_data <- reactive({
    req(app_data(), input$selected_trait, input$selected_loc)
    app_data() %>% filter(Trait == input$selected_trait, Loc == input$selected_loc)
  })
  
  output$heatmapPlot <- renderPlotly({
    req(map_data(), input$selected_metric)
    df <- map_data()
    
    if(!all(c("Row", "Range") %in% colnames(df))) {
      return(plot_ly() %>% layout(title = "Columns 'Row' and/or 'Range' missing."))
    }
    
    p <- ggplot(df, aes(x = as.factor(Range), y = as.factor(Row), 
                        fill = .data[[input$selected_metric]],
                        text = paste("Genotype:", Genotype, 
                                     "<br>Row:", Row, 
                                     "<br>Range:", Range,
                                     "<br>Value:", round(.data[[input$selected_metric]], 2)))) +
      geom_tile(color = "white", size = 0.5) +
      geom_text(aes(label = Genotype), color = "black", size = 3) + 
      scale_fill_viridis(option = "magma", name = input$selected_metric) +
      scale_y_discrete(limits = rev) + 
      theme_minimal() +
      labs(title = paste("Spatial Field Layout for", input$selected_loc), x = "Range (Column)", y = "Row") +
      theme(panel.grid = element_blank())
    
    ggplotly(p, tooltip = "text")
  })
  
  # --- Plot 2: Correlation Plot ---
  output$corrPlot <- renderPlot({
    req(app_data(), input$selected_trait, input$selected_metric)
    df <- app_data() %>% filter(Trait == input$selected_trait)
    
    if(!input$selected_metric %in% colnames(df)) return()
    
    wide_data <- df %>%
      dplyr::select(Loc, Genotype, all_of(input$selected_metric)) %>%
      tidyr::pivot_wider(names_from = Loc, values_from = all_of(input$selected_metric)) %>%
      dplyr::select(-Genotype) 
    
    cor_matrix <- cor(wide_data, use = "pairwise.complete.obs")
    
    corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
             addCoef.col = "black", tl.col = "black", tl.srt = 45, diag = FALSE,
             title = paste("Location Correlation (", input$selected_metric, ")"), mar = c(0,0,3,0))
  })
  
  # --- Plot 3: Genotype vs Environmental Average ---
  output$envPlot <- renderPlotly({
    req(app_data(), input$selected_trait, input$selected_metric, input$selected_genos)
    
    df <- app_data() %>% filter(Trait == input$selected_trait)
    if(!input$selected_metric %in% colnames(df)) return()
    
    loc_means <- df %>%
      group_by(Loc) %>%
      summarize(Env_Mean = mean(.data[[input$selected_metric]], na.rm = TRUE), .groups = 'drop')
    
    plot_df <- df %>%
      filter(Genotype %in% input$selected_genos) %>%
      group_by(Loc, Genotype) %>%
      summarize(Value = mean(.data[[input$selected_metric]], na.rm = TRUE), .groups = 'drop') %>%
      left_join(loc_means, by = "Loc")
    
    p <- ggplot(plot_df, aes(x = Env_Mean, y = Value, color = Genotype, group = Genotype,
                             text = paste("Location:", Loc, 
                                          "<br>Genotype:", Genotype,
                                          "<br>Env Mean:", round(Env_Mean, 2), 
                                          "<br>Value:", round(Value, 2)))) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", size = 0.8) +
      geom_point(size = 3, alpha = 0.8) +
      geom_line(alpha = 0.6, size = 1) +
      theme_minimal() +
      labs(
        title = paste("Genotype Performance vs Environmental Mean"),
        subtitle = paste("Metric:", input$selected_metric),
        x = paste("Location Average", input$selected_metric, "(Environmental Index)"),
        y = paste("Genotype", input$selected_metric)
      ) +
      theme(
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")
      )
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)