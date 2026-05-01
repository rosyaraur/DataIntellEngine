library(shiny)
library(ggplot2)
library(DT)

# --- 1. CORE INVENTORY & P-REP SPATIAL BLOCKING FUNCTION ---
prep_design_inventory <- function(locs_df, genotypes_csv_path, check_proportion = 0.20, seed = 999) {
  
  if (check_proportion <= 0 || check_proportion >= 1) stop("Error: check_proportion must be strictly between 0 and 1.")
  
  # Read inputs
  locs <- locs_df
  genos_raw <- read.csv(genotypes_csv_path, header = TRUE, stringsAsFactors = FALSE)
  genos <- genos_raw[, 1:4] # Read strictly by position
  
  # Standardize internal column names
  colnames(locs) <- c("Environment", "Reps", "Row", "Range", "Block_Dir", "Block_Dim")
  colnames(genos) <- c("Name", "Type", "MaxRepsPerBlock", "SeedAvail")
  
  # Format data types and inventory
  genos$SeedAvail <- as.numeric(genos$SeedAvail)
  genos$SeedAvail[tolower(genos$Type) == "check" | is.na(genos$SeedAvail)] <- Inf
  
  if (!is.null(seed) && !is.na(seed) && seed != 999) set.seed(seed)
  
  all_env_plans <- list()
  
  # Loop through Environments
  for (i in 1:nrow(locs)) {
    env_name <- locs$Environment[i]
    n_rows <- as.numeric(locs$Row[i])
    n_ranges <- as.numeric(locs$Range[i])
    b_dir <- trimws(as.character(locs$Block_Dir[i]))
    b_dim <- as.numeric(locs$Block_Dim[i])
    
    total_plots <- n_rows * n_ranges
    target_check_plots <- round(total_plots * check_proportion)
    
    # Generate the physical spatial grid
    grid <- data.frame(
      Environment = env_name, 
      Row = rep(1:n_rows, each = n_ranges), 
      Range = rep(1:n_ranges, times = n_rows), 
      stringsAsFactors = FALSE
    )
    
    # Cut the grid into spatial Blocks
    if (tolower(b_dir) %in% c("row", "rows")) {
      grid$Block <- ceiling(grid$Row / b_dim)
    } else if (tolower(b_dir) %in% c("range", "ranges", "col", "cols")) {
      grid$Block <- ceiling(grid$Range / b_dim)
    } else {
      stop(sprintf("Environment '%s': Block_Dir must be 'Row' or 'Range'.", env_name))
    }
    
    num_blocks <- max(grid$Block)
    
    # Calculate uniform check distribution across blocks
    base_checks_per_block <- floor(target_check_plots / num_blocks)
    remainder_checks <- target_check_plots %% num_blocks
    
    env_data <- data.frame()
    
    # 3. Fill and randomize each spatial block
    for (b in 1:num_blocks) {
      block_grid <- grid[grid$Block == b, ]
      block_size <- nrow(block_grid)
      
      needed_checks <- base_checks_per_block + ifelse(b <= remainder_checks, 1, 0)
      needed_lines <- block_size - needed_checks
      
      block_trts <- c()
      
      # Assign Checks 
      for (k in seq_len(needed_checks)) {
        avail_checks <- genos[tolower(genos$Type) == "check" & genos$SeedAvail > 0, ]
        if (nrow(avail_checks) == 0) {
          block_trts <- c(block_trts, "FILLER")
        } else {
          chosen_idx <- sample(1:nrow(avail_checks), 1)
          chosen_name <- avail_checks$Name[chosen_idx]
          block_trts <- c(block_trts, chosen_name)
          
          # Deduct global inventory
          g_idx <- which(genos$Name == chosen_name)
          genos$SeedAvail[g_idx] <- genos$SeedAvail[g_idx] - 1
        }
      }
      
      # Assign Lines (Sample WITHOUT replacement to maximize unique genotypes per block)
      avail_lines <- genos[tolower(genos$Type) != "check" & genos$SeedAvail > 0, ]
      num_to_pick <- min(needed_lines, nrow(avail_lines))
      
      if (num_to_pick > 0) {
        chosen_indices <- sample(1:nrow(avail_lines), num_to_pick, replace = FALSE)
        chosen_names <- avail_lines$Name[chosen_indices]
        block_trts <- c(block_trts, chosen_names)
        
        for(cn in chosen_names) {
          g_idx <- which(genos$Name == cn)
          genos$SeedAvail[g_idx] <- genos$SeedAvail[g_idx] - 1
        }
      }
      
      # Fill any voids
      needed_lines <- needed_lines - num_to_pick
      if (needed_lines > 0) block_trts <- c(block_trts, rep("FILLER", needed_lines))
      
      # Randomize and attach
      block_grid$Treatment <- sample(block_trts) 
      env_data <- rbind(env_data, block_grid)
    }
    
    # Format Output
    env_data <- env_data[order(env_data$Row, env_data$Range), ] 
    env_data$PlotNumber <- 1:nrow(env_data)
    env_data <- env_data[, c("Environment", "PlotNumber", "Row", "Range", "Block", "Treatment")]
    
    all_env_plans[[i]] <- env_data
  }
  
  final_plan <- do.call(rbind, all_env_plans)
  
  # --- Generate Output Summaries ---
  remaining_lines <- genos[tolower(genos$Type) != "check", c("Name", "SeedAvail")]
  colnames(remaining_lines) <- c("Line", "Remaining_Seed_Quantity")
  
  trt_env_counts <- as.data.frame.matrix(table(final_plan$Treatment, final_plan$Environment))
  trt_env_counts$Total_Plots <- rowSums(trt_env_counts)
  trt_env_counts$Treatment <- rownames(trt_env_counts)
  
  summary_df <- merge(genos_raw[, 1:2], trt_env_counts, by.x = 1, by.y = "Treatment", all.y = TRUE)
  colnames(summary_df)[1:2] <- c("Treatment", "Type")
  summary_df$Type[is.na(summary_df$Type)] <- "Filler"
  
  env_cols <- setdiff(names(summary_df), c("Treatment", "Type", "Total_Plots"))
  summary_df <- summary_df[, c("Treatment", "Type", "Total_Plots", env_cols)]
  
  type_order <- ifelse(tolower(summary_df$Type) == "check", 1, ifelse(tolower(summary_df$Type) == "filler", 3, 2))
  summary_df <- summary_df[order(type_order, -summary_df$Total_Plots), ]
  rownames(summary_df) <- NULL
  
  return(list(TrialPlan = final_plan, RemainingInventory = remaining_lines, Summary = summary_df))
}


# --- 2. UI: USER INTERFACE ---
ui <- fluidPage(
  titlePanel("Interactive P-Rep Spatial Trial Designer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Seed Inventory"),
      p("Upload your Genotypes/Inventory CSV."),
      fileInput("file_genos", "Upload Genotypes CSV", accept = c(".csv")),
      
      hr(),
      h4("2. Global Parameters"),
      # NEW P-REP PARAMETER: Check Proportion
      numericInput("check_prop", "Target Check Proportion (e.g., 0.20 for 20%):", value = 0.20, min = 0.01, max = 0.99, step = 0.05),
      numericInput("seed", "Random Seed (for reproducibility):", value = 42),
      
      # Generate Button
      actionButton("generate", "Generate P-Rep Design", class = "btn-primary", style = "width: 100%; margin-top: 15px; font-weight: bold;"),
      
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
                 p("Add and configure your field locations below. ", 
                   tags$span(style = "color: red;", "Note: P-Rep math ignores the 'Reps' column, as total plots are determined strictly by Row x Range.")),
                 
                 fluidRow(
                   column(2, tags$b("Environment")),
                   column(1, tags$b("Reps", style="color:gray;")),
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
        tabPanel("5. Replication Summary", 
                 br(), 
                 p("This table tracks how many times each treatment was planted globally and broken down by environment."),
                 downloadButton("dl_summary", "Download Summary CSV"), br(), br(), DTOutput("summaryTable")
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
          column(1, numericInput(paste0("env_reps_", id), label = NULL, value = 2, min = 1)), # Kept for CSV compatibility
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
      showNotification("Error: Wrong file! Column 2 of the Genotypes file should be text (Check/Line), not numbers.", type = "error", duration = 15)
      return(NULL)
    }
    
    for (id in rv$active_ids) {
      env_name <- input[[paste0("env_name_", id)]]
      row_dim <- input[[paste0("env_row_", id)]]
      range_dim <- input[[paste0("env_range_", id)]]
      b_dim <- input[[paste0("env_bdim_", id)]]
      
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
      prep_design_inventory(
        locs_df = locs_df, 
        genotypes_csv_path = input$file_genos$datapath, 
        check_proportion = input$check_prop, 
        seed = input$seed
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
      labs(title = "P-Rep Multi-Environment Layout", x = "Range (Field Column)", y = "Row", fill = "Spatial Block") +
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
  
  output$summaryTable <- renderDT({
    req(design_data())
    datatable(design_data()$Summary, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })
  
  output$dl_plan <- downloadHandler(
    filename = function() { paste0("PRep_Trial_Plan_", Sys.Date(), ".csv") },
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