# ==============================================================================
# R Shiny App: Population Genetics & Kinship Explorer
# ==============================================================================
library(shiny)
library(ggplot2)
library(dplyr)

# ==============================================================================
# 1. Core Analytical Functions 
# ==============================================================================

calculate_genetic_distance <- function(data, meta_cols = 3, pop_assignments, method = "Nei") {
  geno <- data[, (meta_cols + 1):ncol(data)]
  valid_pops <- unique(pop_assignments)
  valid_pops <- valid_pops[!is.na(valid_pops) & valid_pops != ""]
  k_pops <- length(valid_pops)
  
  if(k_pops < 2) return(NULL) 
  
  geno_01 <- (geno + 1) / 2
  freq_matrix <- matrix(NA, nrow = nrow(geno_01), ncol = k_pops)
  colnames(freq_matrix) <- valid_pops
  
  for (i in 1:k_pops) {
    pop_idx <- which(pop_assignments == valid_pops[i])
    freq_matrix[, i] <- rowMeans(geno_01[, pop_idx, drop = FALSE], na.rm = TRUE)
  }
  
  if (method == "Euclidean") {
    dist_obj <- dist(t(freq_matrix), method = "euclidean")
    dist_obj[is.na(dist_obj)] <- 0 
    return(dist_obj)
    
  } else if (method == "Nei") {
    dist_mat <- matrix(0, nrow = k_pops, ncol = k_pops)
    rownames(dist_mat) <- colnames(dist_mat) <- valid_pops
    
    for (i in 1:(k_pops - 1)) {
      for (j in (i + 1):k_pops) {
        p1 <- freq_matrix[, i]; p2 <- freq_matrix[, j]
        valid <- !is.na(p1) & !is.na(p2)
        p1 <- p1[valid]; p2 <- p2[valid]
        
        I <- sum(p1 * p2) / sqrt(sum(p1^2) * sum(p2^2))
        I <- min(max(I, 1e-10), 1) 
        
        dist_mat[i, j] <- dist_mat[j, i] <- -log(I)
      }
    }
    return(as.dist(dist_mat))
  }
}

calculate_relationship_matrix <- function(data, meta_cols = 3, method = "VanRaden") {
  geno <- data[, (meta_cols + 1):ncol(data)]
  M <- t(geno)
  
  rownames(M) <- gsub("^NSFTV_", "", rownames(M))
  
  if (method == "VanRaden") {
    M_dose <- M + 1
    p <- colMeans(M_dose, na.rm = TRUE) / 2
    valid_loci <- p > 0 & p < 1
    M_dose <- M_dose[, valid_loci]
    p <- p[valid_loci]
    
    P_matrix <- matrix(2 * p, nrow = nrow(M_dose), ncol = length(p), byrow = TRUE)
    M_dose[is.na(M_dose)] <- P_matrix[is.na(M_dose)]
    
    Z <- M_dose - P_matrix
    denominator <- 2 * sum(p * (1 - p))
    return(tcrossprod(Z) / denominator)
    
  } else if (method == "IBS") {
    M[is.na(M)] <- 0
    L <- ncol(M)
    return((tcrossprod(M) + L) / (2 * L))
  }
}

# ==============================================================================
# 2. Demo Data Generator
# ==============================================================================
generate_demo_data <- function() {
  set.seed(123)
  n_samples <- 60
  sample_ids <- paste0("NSFTV_", 1:n_samples)
  pops <- rep(c("IND", "TEJ", "AUS"), each = 20)
  
  meta <- data.frame(id = paste0("snp", 1:500), chr = 1, position = 1:500)
  geno <- matrix(sample(c(1, -1), 500 * n_samples, replace = TRUE), nrow = 500)
  
  geno[, 1:20] <- ifelse(runif(500*20) > 0.8, 1, geno[, 1:20])
  geno[, 21:40] <- ifelse(runif(500*20) > 0.8, -1, geno[, 21:40])
  
  colnames(geno) <- sample_ids
  geno_df <- cbind(meta, geno)
  disp_df <- data.frame(Sample_ID = 1:n_samples, Group = pops) 
  
  return(list(geno = geno_df, disp = disp_df))
}

# ==============================================================================
# 3. User Interface (UI)
# ==============================================================================
ui <- fluidPage(
  titlePanel("Genetic Structure & Kinship Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      
      h4("1. Data Input"),
      fileInput("file_geno", "Upload Genotype Matrix (CSV)"),
      fileInput("file_disp", "Upload Descriptor Matrix (CSV)"),
      uiOutput("column_mapping_ui"),
      
      hr(),
      h4("2. Analysis Configuration"),
      radioButtons("analysis_type", "Select Analysis:",
                   choices = c("Population Genetic Distance", "Individual Relationship Matrix (GRM)")),
      
      conditionalPanel(
        condition = "input.analysis_type == 'Population Genetic Distance'",
        selectInput("dist_method", "Distance Metric:", choices = c("Nei", "Euclidean")),
        selectInput("dist_plot", "Plot Type:", choices = c("Dendrogram", "PCoA Scatter", "Heatmap Matrix"))
      ),
      
      conditionalPanel(
        condition = "input.analysis_type == 'Individual Relationship Matrix (GRM)'",
        selectInput("rel_method", "Kinship Method:", choices = c("VanRaden", "IBS"))
      ),
      
      hr(),
      actionButton("run_btn", "Run Analysis", class = "btn-primary", style = "width: 100%;"),
      
      # --- NEW: Dynamic Download Button UI ---
      uiOutput("download_ui"),
      
      br(), br(),
      verbatimTextOutput("data_status")
    ),
    
    mainPanel(
      h3("Analysis Output"),
      plotOutput("main_plot", height = "600px", width = "100%")
    )
  )
)

# ==============================================================================
# 4. Server Logic
# ==============================================================================
server <- function(input, output, session) {
  
  app_data <- reactiveVal(NULL)
  
  observe({
    if (!is.null(input$file_geno) && !is.null(input$file_disp)) {
      tryCatch({
        geno <- read.csv(input$file_geno$datapath, stringsAsFactors = FALSE)
        disp <- read.csv(input$file_disp$datapath, stringsAsFactors = FALSE)
        app_data(list(geno = geno, disp = disp, source = "User Uploaded Matrices"))
      }, error = function(e) {
        showNotification("Error reading files.", type = "error")
      })
    } else if (file.exists("rice44K.csv") && file.exists("rice44Kdisp.csv")) {
      geno <- read.csv("rice44K.csv", stringsAsFactors = FALSE)
      disp <- read.csv("rice44Kdisp.csv", stringsAsFactors = FALSE)
      app_data(list(geno = geno, disp = disp, source = "Local Rice44K Files"))
    } else {
      demo <- generate_demo_data()
      demo$source <- "Internal Demo Data"
      app_data(demo)
    }
  })
  
  output$column_mapping_ui <- renderUI({
    req(app_data())
    disp_cols <- colnames(app_data()$disp)
    def_id <- grep("ID|Sample|Name", disp_cols, ignore.case = TRUE, value = TRUE)[1]
    def_pop <- grep("pop|sub|group|cluster", disp_cols, ignore.case = TRUE, value = TRUE)[1]
    
    tagList(
      hr(),
      h5("Map Descriptor Columns"),
      selectInput("id_col", "Which column represents Sample ID?", choices = disp_cols, selected = def_id),
      selectInput("pop_col", "Which column represents Subpopulation?", choices = disp_cols, selected = def_pop)
    )
  })
  
  output$data_status <- renderText({
    req(app_data())
    paste("Data Source Active:\n", app_data()$source, 
          "\nMarkers:", nrow(app_data()$geno),
          "\nSamples:", ncol(app_data()$geno) - 3)
  })
  
  result_obj <- eventReactive(input$run_btn, {
    req(app_data(), input$id_col, input$pop_col) 
    data_list <- app_data()
    
    if (input$analysis_type == "Population Genetic Distance") {
      raw_cols <- colnames(data_list$geno)[-c(1:3)]
      clean_geno_ids <- as.character(gsub("^NSFTV_", "", raw_cols))
      clean_desc_ids <- as.character(data_list$disp[[input$id_col]])
      clean_desc_ids <- as.character(gsub("^NSFTV_", "", clean_desc_ids))
      
      pop_assignments <- as.character(data_list$disp[[input$pop_col]][match(clean_geno_ids, clean_desc_ids)])
      dist_mat <- calculate_genetic_distance(data_list$geno, 3, pop_assignments, input$dist_method)
      
      return(list(type = "dist", obj = dist_mat, plot_type = input$dist_plot, name_prefix = input$dist_method))
      
    } else {
      grm_mat <- calculate_relationship_matrix(data_list$geno, 3, input$rel_method)
      return(list(type = "grm", obj = grm_mat, plot_type = "heatmap", name_prefix = input$rel_method))
    }
  })
  
  # --- NEW: Render the Download Button (Only shows after "Run Analysis" is clicked) ---
  output$download_ui <- renderUI({
    req(result_obj())
    downloadButton("download_matrix", "Download Calculated Matrix", 
                   class = "btn-success", style = "width: 100%; margin-top: 15px;")
  })
  
  # --- NEW: Handle the File Download Generation ---
  output$download_matrix <- downloadHandler(
    filename = function() {
      res <- result_obj()
      if (res$type == "dist") {
        paste0("Genetic_Distance_Matrix_", res$name_prefix, "_", Sys.Date(), ".csv")
      } else {
        paste0("Kinship_Matrix_", res$name_prefix, "_", Sys.Date(), ".csv")
      }
    },
    content = function(file) {
      res <- result_obj()
      # as.matrix() smoothly converts both 'dist' objects and raw matrices into a standard square format
      out_matrix <- as.matrix(res$obj)
      
      # write.csv handles writing the row names (IDs or Populations) and column headers
      write.csv(out_matrix, file, row.names = TRUE)
    }
  )
  
  output$main_plot <- renderPlot({
    res <- result_obj()
    
    validate(
      need(!is.null(res), "Awaiting calculation..."),
      need(!is.null(res$obj), "Calculation failed. Check Sample ID matching and ensure valid subpopulations.")
    )
    
    if (res$type == "dist") {
      if (res$plot_type == "Dendrogram") {
        hc <- hclust(res$obj, method = "ward.D2")
        plot(hc, main = "Subpopulation Hierarchical Clustering", 
             xlab = "Populations", ylab = "Distance", col = "#2C3E50", lwd = 2, cex = 1.2)
        
      } else if (res$plot_type == "PCoA Scatter") {
        pcoa_res <- cmdscale(res$obj, k = 2, eig = TRUE)
        var_exp <- round(pcoa_res$eig / sum(pcoa_res$eig) * 100, 1)
        df <- data.frame(Pop = rownames(pcoa_res$points), PC1 = pcoa_res$points[, 1], PC2 = pcoa_res$points[, 2])
        
        p <- ggplot(df, aes(x = PC1, y = PC2, label = Pop, color = Pop)) +
          geom_point(size = 6, alpha = 0.8) + geom_text(vjust = -1.5, size = 5, fontface = "bold") +
          theme_minimal() + theme(legend.position = "none") +
          labs(title = "Principal Coordinate Analysis", x = paste0("PC1 (", var_exp[1], "%)"), y = paste0("PC2 (", var_exp[2], "%)"))
        print(p)
        
      } else if (res$plot_type == "Heatmap Matrix") {
        dist_mat <- as.matrix(res$obj)
        df <- as.data.frame(as.table(dist_mat))
        df$Var2 <- factor(df$Var2, levels = rev(levels(df$Var2)))
        
        p <- ggplot(df, aes(x = Var1, y = Var2, fill = Freq)) +
          geom_tile(color = "white") + geom_text(aes(label = round(Freq, 3)), color = "black") +
          scale_fill_gradient(low = "#FFFFFF", high = "#E74C3C", name = "Distance") +
          theme_minimal() + labs(title = "Pairwise Distance Matrix", x = "", y = "")
        print(p)
      }
      
    } else if (res$type == "grm") {
      color_palette <- colorRampPalette(c("#3498DB", "white", "#E74C3C"))(50)
      heatmap(res$obj, col = color_palette, 
              main = paste(res$name_prefix, "Kinship Matrix"),
              xlab = "Individuals", ylab = "Individuals",
              margins = c(5,5), scale = "none")
    }
  })
}

shinyApp(ui = ui, server = server)