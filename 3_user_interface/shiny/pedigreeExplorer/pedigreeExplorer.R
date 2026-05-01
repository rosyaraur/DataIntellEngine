library(shiny)
library(AGHmatrix)
library(visNetwork)
library(plotly)
library(DT)
library(shinycssloaders) 

# ---------------------------------------------------------
# HELPER FUNCTION: PEDIGREE TRIMMER (Ancestry Trace)
# ---------------------------------------------------------
trim_pedigree <- function(ped_data, targets, founder_marks = c("*", "NA", "nan", "", "0", NA)) {
  ped_data[, 1:3] <- lapply(ped_data[, 1:3], as.character)
  colnames(ped_data)[1:3] <- c("Name", "Parent1", "Parent2")
  
  queue <- as.character(targets)
  processed <- character()
  result_list <- list()
  
  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    if (is.na(current) || current %in% processed || current %in% founder_marks) { next }
    processed <- c(processed, current)
    row_idx <- which(ped_data$Name == current)
    
    if (length(row_idx) > 0) {
      p1 <- ped_data$Parent1[row_idx[1]]
      p2 <- ped_data$Parent2[row_idx[1]]
      if (is.na(p1)) p1 <- "0"
      if (is.na(p2)) p2 <- "0"
      
      result_list[[length(result_list) + 1]] <- data.frame(
        Name = current, Parent1 = p1, Parent2 = p2, stringsAsFactors = FALSE
      )
      if (!(p1 %in% founder_marks)) queue <- c(queue, p1)
      if (!(p2 %in% founder_marks)) queue <- c(queue, p2)
    }
  }
  
  if (length(result_list) > 0) {
    final_ped <- unique(do.call(rbind, result_list))
    return(final_ped)
  } else {
    return(data.frame(Name=character(), Parent1=character(), Parent2=character()))
  }
}

# ---------------------------------------------------------
# 1. USER INTERFACE (UI)
# ---------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("Interactive Pedigree & Relationship Matrix Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Data Upload"),
      fileInput("ped_file", "Upload Pedigree (.csv)", 
                accept = c(".csv", "text/csv", "text/comma-separated-values")),
      
      checkboxInput("header", "My CSV has a header row", TRUE),
      
      h4("2. Data Settings"),
      # UPDATED: Added * to the default value string here
      textInput("missing_val", "Missing Value Flags", value = "*, ., NA, unknown, Unknown, "),
      
      h4("3. Filter Ancestry (Optional)"),
      textInput("target_inds", "Target Individuals (comma-separated)", 
                placeholder = "e.g., Pop_HCxAr, AE0234"),
      helpText("These individuals will be highlighted in the plots."),
      
      hr(),
      actionButton("process_btn", "Calculate Matrix & Render Plots", 
                   class = "btn-primary", width = "100%"),
      
      conditionalPanel(
        condition = "output.data_ready == true",
        hr(),
        h4("Export Results"),
        downloadButton("download_ped", "Download Pedigree", class = "btn-success", style = "width:100%; margin-bottom:10px;"),
        downloadButton("download_mat", "Download Matrix", class = "btn-success", style = "width:100%;")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Cleaned Pedigree Data", br(), withSpinner(DTOutput("ped_table"))),
        tabPanel("A-Matrix Heatmap", br(), withSpinner(plotlyOutput("heatmap_plot", height = "750px"))),
        tabPanel("Pedigree Network", br(), withSpinner(visNetworkOutput("network_plot", height = "750px")))
      )
    )
  )
)

# ---------------------------------------------------------
# 2. SERVER LOGIC
# ---------------------------------------------------------
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    clean_data = NULL,
    a_matrix = NULL,
    edges = NULL,
    nodes = NULL,
    targets = character() 
  )
  
  observeEvent(input$process_btn, {
    req(input$ped_file) 
    
    raw_data <- read.csv(input$ped_file$datapath, header = input$header, stringsAsFactors = FALSE)
    ped_data <- raw_data[, 1:3]
    colnames(ped_data) <- c("Name", "Parent1", "Parent2")
    
    # Standardize Missing Values
    missing_flags <- trimws(unlist(strsplit(input$missing_val, ",")))
    ped_data$Parent1[is.na(ped_data$Parent1) | ped_data$Parent1 %in% missing_flags] <- "0"
    ped_data$Parent2[is.na(ped_data$Parent2) | ped_data$Parent2 %in% missing_flags] <- "0"
    
    # Process Trimming/Targets
    raw_targets <- trimws(input$target_inds)
    if (raw_targets != "") {
      rv$targets <- trimws(unlist(strsplit(raw_targets, ",")))
      ped_data <- trim_pedigree(ped_data, rv$targets, founder_marks = "0")
      
      if (nrow(ped_data) == 0) {
        showNotification("No targets found. Check spelling.", type = "error")
        return()
      }
    } else {
      rv$targets <- character()
    }
    
    rv$clean_data <- ped_data
    
    # Calculate Matrix
    tryCatch({
      rv$a_matrix <- Amatrix(ped_data)
    }, error = function(e) {
      showNotification(paste("Matrix Failed:", e$message), type = "error")
    })
    
    # Prepare Plot Data
    edges <- rbind(
      data.frame(from = ped_data$Parent1, to = ped_data$Name, stringsAsFactors = FALSE),
      data.frame(from = ped_data$Parent2, to = ped_data$Name, stringsAsFactors = FALSE)
    )
    edges <- edges[edges$from != "0", ]
    edges <- na.omit(edges)
    
    unique_nodes <- unique(c(edges$from, edges$to))
    
    # Highlight Logic
    nodes <- data.frame(
      id = unique_nodes,
      label = unique_nodes,
      title = paste0("Individual: ", unique_nodes),
      shape = "box",
      color.background = ifelse(unique_nodes %in% rv$targets, "#FF8C00", "#D2E5FF"),
      color.border = ifelse(unique_nodes %in% rv$targets, "#CC7000", "#2B7CE9"),
      font.color = ifelse(unique_nodes %in% rv$targets, "white", "black"),
      stringsAsFactors = FALSE
    )
    
    rv$edges <- edges
    rv$nodes <- nodes
    showNotification("Plots updated with highlights!", type = "message")
  })
  
  # Heatmap Plotly
  output$heatmap_plot <- renderPlotly({
    req(rv$a_matrix)
    mat <- rv$a_matrix
    
    names_vec <- colnames(mat)
    
    formatted_labels <- sapply(names_vec, function(x) {
      if (x %in% rv$targets) {
        return(paste0("<span style='color:#FF8C00;'><b>⭐", x, "</b></span>"))
      } else {
        return(x)
      }
    })
    
    plot_ly(
      x = names_vec, y = names_vec, z = mat, type = "heatmap",
      colors = colorRamp(c("white", "#003366")),
      hovertemplate = "Ind 1: %{x}<br>Ind 2: %{y}<br>Relationship: %{z:.3f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(tickvals = names_vec, ticktext = formatted_labels, tickangle = 45),
        yaxis = list(tickvals = names_vec, ticktext = formatted_labels, autorange = "reversed"),
        margin = list(l = 120, b = 120)
      )
  })
  
  output$network_plot <- renderVisNetwork({
    req(rv$nodes, rv$edges)
    visNetwork(rv$nodes, rv$edges) %>%
      visEdges(arrows = "to", color = list(color = "#848484", highlight = "#FF0000")) %>%
      visHierarchicalLayout(direction = "LR", levelSeparation = 200) %>%
      visInteraction(dragNodes = TRUE, zoomView = TRUE, navigationButtons = TRUE) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE) 
  })
  
  output$ped_table <- renderDT({ req(rv$clean_data); datatable(rv$clean_data) })
  output$data_ready <- reactive({ !is.null(rv$clean_data) })
  outputOptions(output, "data_ready", suspendWhenHidden = FALSE)
  output$download_ped <- downloadHandler(
    filename = function() { "cleaned_pedigree.csv" },
    content = function(file) { write.csv(rv$clean_data, file, row.names = FALSE) }
  )
  output$download_mat <- downloadHandler(
    filename = function() { "A_Matrix.csv" },
    content = function(file) { write.csv(rv$a_matrix, file, row.names = TRUE) }
  )
}

shinyApp(ui, server)