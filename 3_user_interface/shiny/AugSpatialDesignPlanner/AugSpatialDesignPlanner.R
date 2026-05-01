library(shiny)
library(ggplot2)
library(DT)

# --- 1. CORE INVENTORY & SPATIAL BLOCKING FUNCTION ---
adesign_inventory <- function(locs_df, genotypes_csv_path, seed = 999, max_reps_per_loc_lines = 1) {
  
  # Read inputs
  locs <- locs_df
  genos_raw <- read.csv(genotypes_csv_path, header = TRUE, stringsAsFactors = FALSE)
  genos <- genos_raw[, 1:4] # Read strictly by position (First 4 columns)
  
  # Standardize internal column names
  colnames(locs) <- c("Environment", "Reps", "Row", "Range", "Block_Dir", "Block_Dim")
  colnames(genos) <- c("Name", "Type", "MaxRepsPerBlock", "SeedAvail")
  
  # Format data types and inventory
  genos$SeedAvail <- as.numeric(genos$SeedAvail)
  genos$MaxRepsPerBlock <- as.numeric(genos$MaxRepsPerBlock)
  genos$SeedAvail[tolower(genos$Type) == "check" | is.na(genos$SeedAvail)] <- Inf
  
  if (!is.null(seed) && !is.na(seed) && seed != 999) set.seed(seed)
  
  all_env_plans <- list()
  
  # Loop through Environments
  for (i in 1:nrow(locs)) {
    env_name <- locs$Environment[i]
    r <- as.numeric(locs$Reps[i])
    n_rows <- as.numeric(locs$Row[i])
    n_ranges <- as.numeric(locs$Range[i])
    b_dir <- trimws(as.character(locs$Block_Dir[i]))
    b_dim <- as.numeric(locs$Block_Dim[i])
    
    # Reset the tracking counter for lines planted in THIS specific location
    genos$CurrentLocReps <- 0
    
    # 1. Generate the physical spatial grid
    grid <- data.frame(
      Environment = env_name, 
      Row = rep(1:n_rows, each = n_ranges), 
      Range = rep(1:n_ranges, times = n_rows), 
      stringsAsFactors = FALSE
    )
    
    # 2. Cut the grid into spatial Blocks
    if (tolower(b_dir) %in% c("row", "rows")) {
      grid$Block <- ceiling(grid$Row / b_dim)
    } else if (tolower(b_dir) %in% c("range", "ranges", "col", "cols")) {
      grid$Block <- ceiling(grid$Range / b_dim)
    } else {
      stop(sprintf("Environment '%s': Block_Dir must be 'Row' or 'Range'. Found: '%s'", env_name, b_dir))
    }
    
    actual_reps <- max(grid$Block)
    env_data <- data.frame()
    
    # 3. Fill and randomize each spatial block based on inventory
    for (b in 1:actual_reps) {
      block_grid <- grid[grid$Block == b, ]
      block_size <- nrow(block_grid)
      block_trts <- c()
      
      # Assign Checks
      for (j in 1:nrow(genos)) {
        if (tolower(genos$Type[j]) == "check") {
          qty <- min(genos$MaxRepsPerBlock[j], genos$SeedAvail[j])
          if (qty > 0) block_trts <- c(block_trts, rep(genos$Name[j], qty))
        }
      }
      
      if (length(block_trts) > block_size) stop(sprintf("Environment '%s' Block %d: Check plots (%d) exceed physical block size (%d).", env_name, b, length(block_trts), block_size))
      
      # Assign Lines based on inventory availability AND Location Limits
      needed <- block_size - length(block_trts)
      for (j in 1:nrow(genos)) {
        if (needed == 0) break
        if (tolower(genos$Type[j]) != "check" && genos$SeedAvail[j] > 0) {
          
          # Calculate how many MORE times we are allowed to plant this line in THIS location
          allowed_in_loc <- max_reps_per_loc_lines - genos$CurrentLocReps[j]
          
          if (allowed_in_loc > 0) {
            qty <- min(needed, genos$MaxRepsPerBlock[j], genos$SeedAvail[j], allowed_in_loc)
            if (qty > 0) {
              block_trts <- c(block_trts, rep(genos$Name[j], qty))
              
              # Deduct from global seed inventory
              genos$SeedAvail[j] <- genos$SeedAvail[j] - qty
              # Add to current location tracking
              genos$CurrentLocReps[j] <- genos$CurrentLocReps[j] + qty
              
              needed <- needed - qty
            }
          }
        }
      }
      
      if (needed > 0) block_trts <- c(block_trts, rep("FILLER", needed))
      
      block_trts <- sample(block_trts) 
      block_grid$Treatment <- block_trts
      env_data <- rbind(env_data, block_grid)
    }
    
    # Format and Order Final Output
    env_data <- env_data[order(env_data$Row, env_data$Range), ] 
    env_data$PlotNumber <- 1:nrow(env_data)
    env_data <- env_data[, c("Environment", "PlotNumber", "Row", "Range", "Block", "Treatment")]
    
    all_env_plans[[i]] <- env_data
  }
  
  final_plan <- do.call(rbind, all_env_plans)
  
  # --- Generate Output Summaries ---
  
  # Remaining Inventory
  remaining_lines <- genos[tolower(genos$Type) != "check", c("Name", "SeedAvail")]
  colnames(remaining_lines) <- c("Line", "Remaining_Seed_Quantity")
  
  # Cross-tabulation Replication Summary (Lines x Environments)
  trt_env_counts <- as.data.frame.matrix(table(final_plan$Treatment, final_plan$Environment))
  trt_env_counts$Total_Plots <- rowSums(trt_env_counts)
  trt_env_counts$Treatment <- rownames(trt_env_counts)
  
  # Join with original Genotype Types to make it highly readable
  summary_df <- merge(genos_raw[, 1:2], trt_env_counts, by.x = 1, by.y = "Treatment", all.y = TRUE)
  colnames(summary_df)[1:2] <- c("Treatment", "Type")
  summary_df$Type[is.na(summary_df$Type)] <- "Filler"
  
  # Reorder columns: Treatment, Type, Total Plots, [Environments...]
  env_cols <- setdiff(names(summary_df), c("Treatment", "Type", "Total_Plots"))
  summary_df <- summary_df[, c("Treatment", "Type", "Total_Plots", env_cols)]
  
  # Sort table: Checks first, then Lines, then Fillers. Sorted descending by Total Plots.
  type_order <- ifelse(tolower(summary_df$Type) == "check", 1, ifelse(tolower(summary_df$Type) == "filler", 3, 2))
  summary_df <- summary_df[order(type_order, -summary_df$Total_Plots), ]
  rownames(summary_df) <- NULL
  
  return(list(
    TrialPlan = final_plan, 
    RemainingInventory = remaining_lines,
    Summary = summary_df
  ))
}


# --- 2. UI: USER INTERFACE ---
ui <- fluidPage(
  titlePanel("Interactive Augumeted Design Spatial Trial Designer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Seed Inventory"),
      p("Upload your Genotypes/Inventory CSV."),
      fileInput("file_genos", "Upload Genotypes CSV", accept = c(".csv")),
      
      hr(),
      h4("2. Global Parameters"),
      # NEW PARAMETER: Max Reps Per Location for Lines
      numericInput("max_reps_loc", "Max Reps Per Location (Experimental Lines):", value = 1, min = 1),
      p(tags$small(tags$em("Set to 1 to force lines to spread across locations rather than clumping in one environment."))),
      
      numericInput("seed", "Random Seed (for reproducibility):", value = 42),
      
      # Generate Button
      actionButton("generate", "Generate Trial Design", class = "btn-primary", style = "width: 100%; margin-top: 15px; font-weight: bold;"),
      
      hr(),
      h5("Genotype CSV Format:"),
      tags$ul(
        tags$li("Col 1: Name"),
        tags$li("Col 2: Type (Check/Line)"),
        tags$li("Col 3: Max Reps/Block"),
        tags$li("Col 4: Seed Available")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        # DYNAMIC UI TAB
        tabPanel("1. Location Parameters", 
                 br(),
                 h4("Environment Details"),
                 p("Add and configure your field locations below. Do not leave numeric fields blank."),
                 
                 fluidRow(
                   column(2, tags$b("Environment")),
                   column(1, tags$b("Reps")),
                   column(2, tags$b("Row")),
                   column(2, tags$b("Range")),
                   column(2, tags$b("Block_Dir")),
                   column(2, tags$b("Block_Dim")),
                   column(1, tags$b("Action"))
                 ),
                 hr(style = "margin-top: 5px; margin-bottom: 15px;"),
                 tags$div(id = "dynamic_locations"),
                 br(),
                 actionButton("add_env", "+ Add Environment", class = "btn-success", icon = icon("plus"))
        ),
        
        # OUTPUT TABS
        tabPanel("2. Field Map Layout", 
                 br(), plotOutput("fieldPlot", height = "700px")
        ),
        tabPanel("3. Trial Plan (Export)", 
                 br(), downloadButton("dl_plan", "Download Field Plan CSV"), br(), br(), DTOutput("planTable")
        ),
        tabPanel("4. Remaining Seed Inventory", 
                 br(), downloadButton("dl_inv", "Download Inventory CSV"), br(), br(), DTOutput("invTable")
        ),
        # NEW SUMMARY TAB
        tabPanel("5. Replication Summary", 
                 br(), 
                 p("This table tracks how many times each treatment was planted globally and broken down by environment."),
                 downloadButton("dl_summary", "Download Summary CSV"), 
                 br(), br(), 
                 DTOutput("summaryTable")
        )
      )
    )
  )
)


# --- 3. SERVER LOGIC ---
server <- function(input, output, session) {
  
  # --- DYNAMIC UI LOGIC ---
  rv <- reactiveValues(id_count = 0, active_ids = c())
  
  add_location_row <- function() {
    rv$id_count <- rv$id_count + 1
    id <- rv$id_count
    rv$active_ids <- c(rv$active_ids, id)
    
    insertUI(
      selector = "#dynamic_locations",
      where = "beforeEnd",
      ui = tags$div(
        id = paste0("loc_row_", id),
        fluidRow(
          style = "margin-bottom: 5px;",
          column(2, textInput(paste0("env_name_", id), label = NULL, value = paste("Env", id))),
          column(1, numericInput(paste0("env_reps_", id), label = NULL, value = 2, min = 1)),
          column(2, numericInput(paste0("env_row_", id), label = NULL, value = 4, min = 1)),
          column(2, numericInput(paste0("env_range_", id), label = NULL, value = 5, min = 1)),
          column(2, selectInput(paste0("env_bdir_", id), label = NULL, choices = c("Row", "Range"))),
          column(2, numericInput(paste0("env_bdim_", id), label = NULL, value = 2, min = 1)),
          column(1, actionButton(paste0("del_env_", id), label = NULL, icon = icon("trash"), class = "btn-danger"))
        )
      )
    )
    
    observeEvent(input[[paste0("del_env_", id)]], {
      removeUI(selector = paste0("#loc_row_", id))
      rv$active_ids <- rv$active_ids[rv$active_ids != id]
    }, ignoreInit = TRUE, once = TRUE)
  }
  
  observeEvent(session, { add_location_row() }, once = TRUE)
  observeEvent(input$add_env, { add_location_row() })
  
  
  # --- GENERATION LOGIC ---
  design_data <- eventReactive(input$generate, {
    req(input$file_genos) 
    
    if (length(rv$active_ids) == 0) {
      showNotification("Please add at least one Location in the 'Location Parameters' tab.", type = "error")
      return(NULL)
    }
    
    genos_check <- read.csv(input$file_genos$datapath, stringsAsFactors = FALSE)
    if (ncol(genos_check) < 4) {
      showNotification("Error: Genotypes CSV must have at least 4 columns.", type = "error", duration = 10)
      return(NULL)
    }
    if (is.numeric(genos_check[[2]])) {
      showNotification("Error: It looks like you uploaded your Locations CSV into the Genotypes slot! Column 2 of the Genotypes file should be text (Check/Line), not numbers.", type = "error", duration = 15)
      return(NULL)
    }
    
    for (id in rv$active_ids) {
      env_name <- input[[paste0("env_name_", id)]]
      reps <- input[[paste0("env_reps_", id)]]
      row_dim <- input[[paste0("env_row_", id)]]
      range_dim <- input[[paste0("env_range_", id)]]
      b_dim <- input[[paste0("env_bdim_", id)]]
      
      if (is.null(reps) || is.na(reps) || reps <= 0) return(showNotification(sprintf("Error in '%s': Reps cannot be blank or zero.", env_name), type = "error", duration = 8))
      if (is.null(row_dim) || is.na(row_dim) || row_dim <= 0) return(showNotification(sprintf("Error in '%s': Row cannot be blank or zero.", env_name), type = "error", duration = 8))
      if (is.null(range_dim) || is.na(range_dim) || range_dim <= 0) return(showNotification(sprintf("Error in '%s': Range cannot be blank or zero.", env_name), type = "error", duration = 8))
      if (is.null(b_dim) || is.na(b_dim) || b_dim <= 0) return(showNotification(sprintf("Error in '%s': Block_Dim cannot be blank or zero.", env_name), type = "error", duration = 8))
    }
    
    locs_list <- lapply(rv$active_ids, function(id) {
      data.frame(
        Environment = input[[paste0("env_name_", id)]],
        Reps = input[[paste0("env_reps_", id)]],
        Row = input[[paste0("env_row_", id)]],
        Range = input[[paste0("env_range_", id)]],
        Block_Dir = input[[paste0("env_bdir_", id)]],
        Block_Dim = input[[paste0("env_bdim_", id)]],
        stringsAsFactors = FALSE
      )
    })
    
    locs_df <- do.call(rbind, locs_list)
    
    tryCatch({
      adesign_inventory(
        locs_df = locs_df, 
        genotypes_csv_path = input$file_genos$datapath, 
        seed = input$seed,
        max_reps_per_loc_lines = input$max_reps_loc # Passing new variable to the function
      )
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 15)
      return(NULL)
    })
  })
  
  
  # --- RENDERING LOGIC ---
  check_names <- reactive({
    req(input$file_genos)
    genos <- read.csv(input$file_genos$datapath, stringsAsFactors = FALSE)[, 1:2]
    return(genos[tolower(genos[, 2]) == "check", 1])
  })
  
  output$fieldPlot <- renderPlot({
    req(design_data()) 
    plan <- design_data()$TrialPlan
    checks <- check_names()
    
    plan$is_check <- plan$Treatment %in% checks
    
    ggplot(plan, aes(x = Range, y = Row)) +
      geom_tile(aes(fill = as.factor(Block)), color = "white", linewidth = 0.5) +
      geom_tile(data = subset(plan, is_check == TRUE), aes(x = Range, y = Row), fill = "darkred", color = "darkred", width = 0.85, height = 0.85, alpha = 0.8) +
      geom_text(aes(label = PlotNumber), color = ifelse(plan$is_check, "darkgray", "gray60"), size = 3, hjust = 0, vjust = -1) +
      geom_text(aes(label = Treatment, fontface = ifelse(is_check, "bold", "plain"), color = ifelse(is_check, "white", "black")), size = 3.5) +
      scale_y_reverse(breaks = 1:max(plan$Row)) + 
      scale_x_continuous(breaks = 1:max(plan$Range), position = "top") +
      scale_color_identity() + 
      facet_wrap(~ Environment, scales = "free") + 
      labs(title = "Spatial-Driven Multi-Environment Layout", x = "Range (Field Column)", y = "Row", fill = "Spatial Block") +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        strip.text = element_text(face = "bold", size = 12, background = element_rect(fill = "gray90", color = NA)),
        plot.title = element_text(face = "bold", size = 14)
      )
  })
  
  output$planTable <- renderDT({
    req(design_data())
    datatable(design_data()$TrialPlan, options = list(pageLength = 20, scrollX = TRUE))
  })
  
  output$invTable <- renderDT({
    req(design_data())
    datatable(design_data()$RemainingInventory, options = list(pageLength = 20))
  })
  
  # Render the new Summary Table
  output$summaryTable <- renderDT({
    req(design_data())
    datatable(design_data()$Summary, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })
  
  # Download Handlers
  output$dl_plan <- downloadHandler(
    filename = function() { paste0("Trial_Plan_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(design_data()$TrialPlan, file, row.names = FALSE) }
  )
  
  output$dl_inv <- downloadHandler(
    filename = function() { paste0("Remaining_Inventory_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(design_data()$RemainingInventory, file, row.names = FALSE) }
  )
  
  output$dl_summary <- downloadHandler(
    filename = function() { paste0("Replication_Summary_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(design_data()$Summary, file, row.names = FALSE) }
  )
}

shinyApp(ui = ui, server = server)