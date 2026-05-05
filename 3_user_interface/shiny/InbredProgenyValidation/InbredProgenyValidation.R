library(shiny)
library(DT)
library(ggplot2)

# =====================================================================
# 1. CORE FUNCTIONS (Biologically Informed)
# =====================================================================

# --- Preprocessing: LD Pruning ---
prune_markers_ld <- function(genotype_matrix, ld_threshold = 0.8) {
  mat <- t(as.matrix(genotype_matrix))
  corr_matrix <- cor(mat, use = "pairwise.complete.obs")
  r2_matrix <- corr_matrix^2
  n_markers <- ncol(r2_matrix)
  keep <- rep(TRUE, n_markers)
  for (i in 1:(n_markers - 1)) {
    if (!keep[i]) next
    high_ld <- which(r2_matrix[(i + 1):n_markers, i] > ld_threshold) + i
    keep[high_ld] <- FALSE
  }
  return(which(keep))
}

# --- Core Engine: Pedigree Verification ---
verify_biparental_progeny <- function(p1_vector, p2_vector, population_matrix, 
                                      generation = 6, alpha = 0.01, error_buffer = 0.02) {
  n_total <- nrow(population_matrix)
  
  # Define Loci
  mono_idx <- which(p1_vector == p2_vector & !is.na(p1_vector) & !is.na(p2_vector))
  poly_idx <- which(p1_vector != p2_vector & !is.na(p1_vector) & !is.na(p2_vector))
  
  # 1. Monomorphic Check
  obs_mono <- rowSums(!is.na(population_matrix[, mono_idx, drop=FALSE]))
  matches_mono <- rowSums(sweep(population_matrix[, mono_idx, drop=FALSE], 2, p1_vector[mono_idx], `==`), na.rm = TRUE)
  mono_error_rate <- 1 - (matches_mono / obs_mono)
  
  # 2. Polymorphic Check
  obs_poly <- rowSums(!is.na(population_matrix[, poly_idx, drop=FALSE]))
  match_p1 <- rowSums(sweep(population_matrix[, poly_idx, drop=FALSE], 2, p1_vector[poly_idx], `==`), na.rm = TRUE) / obs_poly
  het_rate <- rowSums(population_matrix[, poly_idx, drop=FALSE] == 1, na.rm = TRUE) / obs_poly
  
  # 3. Stats
  exp_het <- (1/2)^(generation - 1)
  exp_p1 <- (1 - exp_het) / 2
  sd_p1 <- sqrt(exp_p1 * (1 - exp_p1) / obs_poly)
  z_p1 <- (match_p1 - exp_p1) / sd_p1
  p_val_p1 <- 2 * pnorm(-abs(z_p1))
  
  data.frame(
    Individual_ID = rownames(population_matrix),
    Call_Rate = round(rowSums(!is.na(population_matrix)) / ncol(population_matrix), 3),
    Mono_Error = round(mono_error_rate, 4),
    Poly_P1 = round(match_p1, 4),
    Obs_Het = round(het_rate, 4),
    Status = ifelse(mono_error_rate > (error_buffer + 0.03), "REJECT: OUTCROSS", 
                    ifelse(p_val_p1 < alpha, "REJECT: SKEWED", "VERIFIED")),
    stringsAsFactors = FALSE
  )
}

# --- Noise Simulation Helpers ---
apply_genotyping_error <- function(mat, error_rate) {
  if (error_rate == 0) return(mat)
  n_elements <- length(mat); n_errors <- round(n_elements * error_rate)
  error_indices <- sample(1:n_elements, n_errors)
  shifts <- sample(c(1, 2), n_errors, replace = TRUE)
  mat[error_indices] <- (mat[error_indices] + shifts) %% 3
  return(mat)
}

apply_missingness <- function(mat, missing_rate) {
  if (missing_rate == 0) return(mat)
  n_elements <- length(mat); n_na <- round(n_elements * missing_rate)
  na_indices <- sample(1:n_elements, n_na)
  mat[na_indices] <- NA
  return(mat)
}

# =====================================================================
# 2. UI
# =====================================================================
ui <- navbarPage("Pedigree QA/QC Production Suite",
                 tabPanel("1. Data Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("file_matrix", "Upload Matrix (Markers=Rows)"),
                              actionButton("load_demo", "Load Demo Data", class="btn-info"),
                              uiOutput("mapping_ui"),
                              hr(),
                              numericInput("ld_thresh", "LD Threshold (r²)", 0.8),
                              actionButton("run_prune", "Prune Markers", class="btn-warning", width="100%"),
                              textOutput("prune_msg"),
                              hr(),
                              numericInput("gen", "Generation (Fn)", 6),
                              numericInput("err_buf", "Lab Error Tolerance", 0.02, step=0.01),
                              actionButton("run_qa", "Run Verification", class="btn-primary", width="100%")
                            ),
                            mainPanel(DTOutput("qa_table"))
                          )
                 ),
                 tabPanel("2. Power Stress Test",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("sim_error", "Genotyping Error (%)", 0, 5, 1, step=0.5),
                              sliderInput("sim_na", "Missing Data (%)", 0, 60, 10),
                              textInput("densities", "Test Densities", "50, 200, 1000, 5000"),
                              numericInput("z_thresh", "Z-Score Threshold", 4.0),
                              actionButton("run_adequacy", "Run Stress Test", class="btn-success", width="100%")
                            ),
                            mainPanel(plotOutput("power_plot"), hr(), DTOutput("adequacy_table"))
                          )
                 )
)

# =====================================================================
# 3. SERVER
# =====================================================================
server <- function(input, output, session) {
  raw_data <- reactiveVal(NULL)
  pruned_indices <- reactiveVal(NULL)
  
  observeEvent(input$file_matrix, { 
    raw_data(read.csv(input$file_matrix$datapath, row.names=1, check.names=F)) 
    pruned_indices(NULL)
  })
  
  observeEvent(input$load_demo, {
    set.seed(42); n_m <- 1000; p1 <- sample(c(0,2), n_m, T); p2 <- p1; diff <- runif(n_m) < 0.4
    p2[diff] <- ifelse(p1[diff]==0, 2, 0)
    f6 <- matrix(rep(p1, 50), 50, byrow=T); seg <- which(diff)
    for(i in 1:50) { f6[i, seg] <- p1[seg] + sample(c(0,1,2), length(seg), T, c(0.48, 0.04, 0.48)) * (p2[seg]-p1[seg])/2 }
    demo <- data.frame(P1=p1, P2=p2, t(f6)); colnames(demo)[3:52] <- paste0("Line_", 1:50)
    raw_data(demo); pruned_indices(NULL)
  })
  
  output$mapping_ui <- renderUI({
    req(raw_data()); cols <- colnames(raw_data())
    tagList(
      selectInput("p1", "Parent 1 Column", cols, cols[1]),
      selectInput("p2", "Parent 2 Column", cols, cols[2]),
      selectizeInput("prog", "Test Progenies", cols, cols[3:length(cols)], multiple=T)
    )
  })
  
  observeEvent(input$run_prune, {
    req(raw_data())
    idx <- prune_markers_ld(raw_data(), input$ld_thresh)
    pruned_indices(idx)
    output$prune_msg <- renderText({ sprintf("Kept %d markers after pruning.", length(idx)) })
  })
  
  observeEvent(input$run_qa, {
    validate(
      need(raw_data(), "Please load data first."),
      need(input$p1, "Select Parent 1."),
      need(input$p2, "Select Parent 2."),
      need(length(input$prog) > 0, "Select at least one progeny.")
    )
    
    dat <- raw_data(); idx <- if(!is.null(pruned_indices())) pruned_indices() else 1:nrow(dat)
    p1v <- as.numeric(dat[idx, input$p1]); p2v <- as.numeric(dat[idx, input$p2])
    pop <- t(as.matrix(dat[idx, input$prog, drop=F]))
    
    res <- verify_biparental_progeny(p1v, p2v, pop, input$gen, error_buffer = input$err_buf)
    
    output$qa_table <- renderDT({ 
      datatable(res) %>% formatStyle('Status', target='row', backgroundColor=styleEqual(
        c('VERIFIED', 'REJECT: OUTCROSS', 'REJECT: SKEWED'), c('#d4edda', '#f8d7da', '#fff3cd')))
    })
  })
  
  observeEvent(input$run_adequacy, {
    validate(need(raw_data(), "Load data and map parents in Tab 1 first."))
    dat <- raw_data(); idx <- if(!is.null(pruned_indices())) pruned_indices() else 1:nrow(dat)
    p1v <- as.numeric(dat[idx, input$p1]); p2v <- as.numeric(dat[idx, input$p2]); pop_clean <- t(as.matrix(dat[idx, input$prog, drop=F]))
    densities <- as.numeric(trimws(unlist(strsplit(input$densities, ","))))
    
    results_list <- list()
    withProgress(message = 'Running Stress Test...', value = 0, {
      for (n in densities) {
        if (n > length(p1v)) next
        s_idx <- sample(1:length(p1v), size = n)
        s_p1 <- p1v[s_idx]; s_p2 <- p2v[s_idx]
        s_pop <- apply_genotyping_error(pop_clean[, s_idx, drop=F], input$sim_error/100)
        s_pop <- apply_missingness(s_pop, input$sim_na/100)
        poly <- which(s_p1 != s_p2 & !is.na(s_p1) & !is.na(s_p2))
        if (length(poly) > 1) {
          obs_poly <- rowSums(!is.na(s_pop[, poly, drop=F]))
          match_p1 <- rowSums(s_pop[, poly, drop=F] == matrix(s_p1[poly], nrow(s_pop), length(poly), byrow=T), na.rm=T) / obs_poly
          sd_pop <- sd(match_p1, na.rm=T)
          z_score <- ( (1 - (input$sim_error/100)) - mean(match_p1, na.rm=T) ) / sd_pop
          results_list[[as.character(n)]] <- data.frame(Total_Markers = n, Useful_Poly = length(poly),
                                                        Empirical_SD = round(sd_pop, 4), Separation_Z = round(z_score, 2),
                                                        Status = ifelse(z_score >= input$z_thresh, "ADEQUATE", "RISKY"))
        }
        incProgress(1/length(densities))
      }
    })
    res_df <- do.call(rbind, results_list)
    output$adequacy_table <- renderDT({ datatable(res_df, options = list(dom = 't')) %>% formatStyle('Status', backgroundColor = styleEqual(c('ADEQUATE', 'RISKY'), c('#d4edda', '#f8d7da'))) })
    output$power_plot <- renderPlot({
      ggplot(res_df, aes(x = Total_Markers, y = Separation_Z, color = Status)) +
        geom_line(color = "grey80") + geom_point(size = 5) +
        geom_hline(yintercept = input$z_thresh, linetype = "dashed", color = "red") +
        scale_x_log10() + scale_color_manual(values = c("ADEQUATE" = "#28a745", "RISKY" = "#dc3545")) +
        theme_minimal() + labs(title = "Stress Test Power Curve", x = "Markers (Log Scale)", y = "Z-Score")
    })
  })
}

shinyApp(ui, server)