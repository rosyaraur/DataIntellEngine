#' Generate a p-rep design using the Cullis optimality criterion
#'
#' @param A_matrix A square numeric matrix representing genomic relationships.
#' @param n_rows Integer. Number of rows in the field layout.
#' @param n_cols Integer. Number of columns in the field layout.
#' @param n_envs Integer. Number of environments.
#' @param n_reps_layout Integer. Number of major replications to split the layout into.
#' @param checks Character vector. Names of check genotypes.
#' @param check_reps Integer. Number of plots per check per environment.
#' @param max_reps Integer. Maximum number of plots allowed for a single test line.
#' @return A list containing the final design, replication summaries, and success metrics.
generate_cullis_prep <- function(A_matrix, n_rows, n_cols, n_envs = 1, n_reps_layout = 2,
                                 checks = NULL, check_reps = 2, max_reps = 2) {
  
  n_gen <- nrow(A_matrix)
  genotypes <- rownames(A_matrix)
  
  if (is.null(genotypes)) {
    genotypes <- paste0("G_", 1:n_gen)
    rownames(A_matrix) <- genotypes
    colnames(A_matrix) <- genotypes
  }
  
  n_plots_per_env <- n_rows * n_cols
  
  if (n_plots_per_env %% n_reps_layout != 0) {
    stop(sprintf("Error: Total plots (%d) must be evenly divisible by Replications (%d).", n_plots_per_env, n_reps_layout))
  }
  plots_per_rep <- n_plots_per_env / n_reps_layout
  
  if (is.null(checks) || all(checks == "")) {
    checks <- character(0)
    check_reps <- 0
  }
  total_check_plots <- length(checks) * check_reps
  n_reps_to_add <- n_plots_per_env - n_gen - total_check_plots
  
  if (n_reps_to_add < 0) stop("Error: Field too small for test lines and checks.")
  if (n_reps_to_add > (n_gen * (max_reps - 1))) stop("Error: Field is too large for the specified Max Plots.")
  
  # 1. Precompute A_inv (Cullis Optimization)
  A_inv <- solve(A_matrix + diag(1e-8, n_gen))
  lam <- 1.0 # Assumed Variance ratio (sigma_e^2 / sigma_g^2)
  M <- solve(diag(1, n_gen) + lam * A_inv)
  
  rep_counts <- rep(1, n_gen)
  replicated_indices <- c()
  remaining_indices <- 1:n_gen
  
  if (max_reps <= 1) remaining_indices <- integer(0)
  
  # Greedy selection of optimal test lines
  if (n_reps_to_add > 0) {
    for (rep in 1:n_reps_to_add) {
      if (length(remaining_indices) == 0) break
      
      m_diag <- diag(M)
      m2_diag <- colSums(M^2)
      
      scores <- m2_diag[remaining_indices] / (1 + m_diag[remaining_indices])
      best_idx <- remaining_indices[which.max(scores)]
      
      m_k <- matrix(M[, best_idx], ncol = 1)
      M <- M - (m_k %*% t(m_k)) / (1 + m_diag[best_idx])
      
      replicated_indices <- c(replicated_indices, best_idx)
      rep_counts[best_idx] <- rep_counts[best_idx] + 1
      
      if (rep_counts[best_idx] >= max_reps) {
        remaining_indices <- remaining_indices[remaining_indices != best_idx]
      }
    }
  }
  
  # --- CALCULATE MATRIX OF SUCCESS (KPIs) ---
  # Extract metrics from the final Information Matrix (M) for test lines
  trace_M <- sum(diag(M))
  avg_pev <- mean(diag(M))
  # Structural Cullis Heritability (assuming lambda = 1, sigma2_g = 1)
  # H2_C = 1 - (mean(PEV) / (2 * sigma2_g))
  expected_h2c <- max(0, 1 - (avg_pev / 2)) 
  
  success_metrics <- data.frame(
    Metric = c("A-Optimality Score (Trace)", "Average PEV", "Expected Cullis Heritability (H²c)", "Replication Proportion (p-rep)"),
    Value = c(round(trace_M, 4), round(avg_pev, 4), round(expected_h2c, 4), round(length(replicated_indices)/n_gen, 4)),
    Description = c(
      "Sum of Prediction Error Variances. Lower is better.",
      "Average error variance across all test lines. Lower is better.",
      "Theoretical structural efficiency based on layout and A-matrix (λ=1). Closer to 1.0 is better.",
      "Percentage of the testing population that received >1 plot."
    )
  )
  
  # 2. Sequential Dealing Algorithm
  all_instances <- character(0)
  if (length(checks) > 0) all_instances <- c(all_instances, rep(checks, each = check_reps))
  for (i in 1:n_gen) {
    if (rep_counts[i] > 0) all_instances <- c(all_instances, rep(genotypes[i], rep_counts[i]))
  }
  
  unique_gens <- sample(unique(all_instances))
  shuffled_instances <- unlist(lapply(unique_gens, function(g) rep(g, sum(all_instances == g))))
  
  df_assigned <- data.frame(Genotype = shuffled_instances)
  df_assigned$Rep <- rep(1:n_reps_layout, length.out = nrow(df_assigned))
  
  # 3. PCA for Similarity Spectrum
  pca_res <- prcomp(A_matrix, center = TRUE, scale. = FALSE)
  sim_scores <- data.frame(Genotype = genotypes, Similarity_Spectrum = pca_res$x[, 1], Type = "Test_Line")
  if (length(checks) > 0) {
    sim_scores <- rbind(sim_scores, data.frame(Genotype = checks, Similarity_Spectrum = NA, Type = "Check_Line"))
  }
  
  # 4. Final Physical Grid Layout
  final_design <- data.frame()
  for (env in 1:n_envs) {
    field_grid <- data.frame(
      Environment = paste0("Env_", env),
      Plot = 1:n_plots_per_env,
      Row = rep(1:n_rows, each = n_cols),
      Col = rep(1:n_cols, times = n_rows)
    )
    
    field_grid$Replication_ID <- rep(1:n_reps_layout, each = plots_per_rep)
    field_grid$Genotype <- NA
    
    for (r in 1:n_reps_layout) {
      rep_genotypes <- df_assigned$Genotype[df_assigned$Rep == r]
      rep_genotypes <- sample(rep_genotypes) 
      field_grid$Genotype[field_grid$Replication_ID == r] <- rep_genotypes
    }
    
    field_grid$Replication <- paste0("Rep_", field_grid$Replication_ID)
    field_grid$Replication_ID <- NULL
    final_design <- rbind(final_design, field_grid)
  }
  
  final_design <- merge(final_design, sim_scores, by = "Genotype", all.x = TRUE)
  final_design <- final_design[order(final_design$Environment, final_design$Plot), ]
  
  rep_summary <- as.data.frame(table(final_design$Genotype[final_design$Environment == "Env_1"]))
  colnames(rep_summary) <- c("Genotype", "Plots_Allocated")
  rep_summary <- merge(rep_summary, sim_scores[, c("Genotype", "Type")], by = "Genotype")
  rep_summary <- rep_summary[order(-rep_summary$Plots_Allocated, rep_summary$Type), ]
  
  return(list(Design = final_design, ReplicationSummary = rep_summary, SuccessMetrics = success_metrics))
}

library(shiny)
library(DT)
library(ggplot2)

library(shiny)
library(DT)
library(ggplot2)

# --- UI ---
ui <- fluidPage(
  titlePanel("Optimal p-rep Design Engine"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Kinship Data Input"),
      fileInput("a_matrix_file", "Upload A Matrix (CSV)", accept = c(".csv")),
      actionButton("load_demo_btn", "Load Wheat Demo Data", icon = icon("seedling"), 
                   class = "btn-info", style="width:100%; margin-bottom: 15px;"),
      verbatimTextOutput("data_status"),
      hr(),
      
      h4("2. Grid & Replication Split"),
      fluidRow(
        column(6, numericInput("n_rows", "Field Rows:", value = 36, min = 1)),
        column(6, numericInput("n_cols", "Field Columns:", value = 20, min = 1))
      ),
      fluidRow(
        column(6, numericInput("n_envs", "Environments:", value = 1, min = 1)),
        column(6, numericInput("n_reps_layout", "Replications (Splits):", value = 2, min = 1))
      ),
      helpText("Note: Total plots must be divisible by the number of Replications."),
      hr(),
      
      h4("3. Optimization Constraints"),
      numericInput("max_reps", "Max Plots per Test Line:", value = 2, min = 1),
      textInput("checks", "Check Lines (Comma Separated):", value = "Check_A, Check_B"),
      numericInput("check_reps", "Plots per Check (Total):", value = 4, min = 1),
      
      actionButton("generate_btn", "Generate Optimal Design", class = "btn-primary", 
                   style="width:100%; margin-top: 15px; font-weight: bold;"),
      hr(),
      
      downloadButton("download_design", "Download Design CSV", style="width:100%; margin-bottom: 5px;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Matrix of Success (KPIs)", 
                 br(),
                 h3("Structural Efficiency Metrics"),
                 p("These metrics are derived directly from the inverse information matrix at the conclusion of the Cullis optimization. They represent the theoretical efficiency of the field layout prior to data collection."),
                 uiOutput("kpi_dashboard"),
                 hr(),
                 h4("Detailed Metric Breakdown"),
                 DTOutput("metrics_table")
        ),
        tabPanel("Field Heatmap", br(), plotOutput("heatmap_plot", height = "750px")),
        tabPanel("Design Layout (Data)", br(), DTOutput("design_table")),
        tabPanel("Replication Summary", br(), DTOutput("rep_summary_table"))
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  A_matrix_data <- reactiveVal(NULL)
  design_results <- reactiveVal(NULL)
  
  observeEvent(input$a_matrix_file, {
    req(input$a_matrix_file)
    tryCatch({
      df <- read.csv(input$a_matrix_file$datapath, row.names = 1, check.names = FALSE)
      A_matrix_data(as.matrix(df))
    }, error = function(e) showNotification(e$message, type = "error"))
  })
  
  observeEvent(input$load_demo_btn, {
    if (file.exists("wheat.A.csv")) {
      df <- read.csv("wheat.A.csv", row.names = 1, check.names = FALSE)
      A_matrix_data(as.matrix(df))
      updateNumericInput(session, "n_rows", value = 36)
      updateNumericInput(session, "n_cols", value = 20)
      updateNumericInput(session, "n_reps_layout", value = 2) 
    } else {
      showNotification("Demo file 'wheat.A.csv' not found.", type = "error")
    }
  })
  
  output$data_status <- renderText({
    mat <- A_matrix_data()
    if (is.null(mat)) return("Status: Awaiting Matrix Upload...")
    paste("Matrix Active | Test Lines:", nrow(mat))
  })
  
  observeEvent(input$generate_btn, {
    mat <- A_matrix_data()
    if (is.null(mat)) return(showNotification("Provide A matrix first.", type = "warning"))
    
    check_list <- trimws(unlist(strsplit(input$checks, ",")))
    check_list <- check_list[check_list != ""]
    
    n_plots <- input$n_rows * input$n_cols
    if (n_plots %% input$n_reps_layout != 0) {
      return(showNotification(sprintf("Cannot divide %d total plots into %d Replications.", n_plots, input$n_reps_layout), type = "error"))
    }
    
    tryCatch({
      results <- generate_cullis_prep(
        A_matrix = mat, n_rows = input$n_rows, n_cols = input$n_cols, 
        n_envs = input$n_envs, n_reps_layout = input$n_reps_layout,
        checks = check_list, check_reps = input$check_reps, max_reps = input$max_reps
      )
      design_results(results)
      showNotification("Optimization Complete.", type = "message")
    }, error = function(e) { showNotification(e$message, type = "error", duration = 8) })
  })
  
  # --- Render Outputs ---
  
  # KPI Dashboard UI
  output$kpi_dashboard <- renderUI({
    req(design_results())
    metrics <- design_results()$SuccessMetrics
    
    fluidRow(
      column(4, wellPanel(
        h4("A-Optimality Score", style = "color: #2c3e50; font-weight: bold;"),
        h2(metrics$Value[1], style = "color: #18bc9c;"),
        p("Trace of M")
      )),
      column(4, wellPanel(
        h4("Average PEV", style = "color: #2c3e50; font-weight: bold;"),
        h2(metrics$Value[2], style = "color: #3498db;"),
        p("Mean Error Variance")
      )),
      column(4, wellPanel(
        h4("Expected Cullis H²", style = "color: #2c3e50; font-weight: bold;"),
        h2(metrics$Value[3], style = "color: #e74c3c;"),
        p("Structural Reliability (λ=1)")
      ))
    )
  })
  
  output$metrics_table <- renderDT({
    req(design_results())
    datatable(design_results()$SuccessMetrics, options = list(dom = 't', pageLength = 5), rownames = FALSE)
  })
  
  output$heatmap_plot <- renderPlot({
    req(design_results())
    df <- design_results()$Design
    p <- ggplot(df, aes(x = Col, y = Row, fill = Similarity_Spectrum)) +
      geom_tile(color = "black", linewidth = 0.3) +
      scale_y_reverse(breaks = seq(1, input$n_rows, by = max(1, floor(input$n_rows/10)))) +
      scale_x_continuous(breaks = seq(1, input$n_cols, by = max(1, floor(input$n_cols/10)))) +
      scale_fill_viridis_c(option = "turbo", name = "Genetic\nSimilarity", na.value = "#FFFFFF") +
      facet_wrap(~ Environment) + 
      labs(title = "Contiguous Field Layout: Kinship Proximity Heatmap",
           subtitle = "Red lines demarcate major Replications. White tiles are Check Lines.",
           x = "Column", y = "Row") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 14, face = "bold", background = element_rect(fill="lightgray", color="black")),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
      )
    
    plots_per_rep <- (input$n_rows * input$n_cols) / input$n_reps_layout
    rows_per_rep <- plots_per_rep / input$n_cols
    if (rows_per_rep == floor(rows_per_rep) && input$n_reps_layout > 1) {
      boundaries <- seq(rows_per_rep, input$n_rows - 1, by = rows_per_rep)
      p <- p + geom_hline(yintercept = boundaries + 0.5, color = "red", linewidth = 1.2)
    }
    return(p)
  })
  
  output$design_table <- renderDT({
    req(design_results())
    df <- design_results()$Design
    if(!is.null(df$Similarity_Spectrum)) df$Similarity_Spectrum <- round(df$Similarity_Spectrum, 4)
    datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  output$rep_summary_table <- renderDT({
    req(design_results())
    datatable(design_results()$ReplicationSummary, options = list(pageLength = 20), rownames = FALSE)
  })
  
  output$download_design <- downloadHandler(
    filename = function() { paste0("Prep_Design_Contiguous_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(design_results()$Design, file, row.names = FALSE) }
  )
}

shinyApp(ui = ui, server = server)