library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)

# =====================================================================
# 1. CORE MABC FUNCTIONS
# =====================================================================

mabc_analysis_engine <- function(pop_mat, map_df, trait_list, epsilon = 0.01, rp_val = 0, buffer_cM = 5) {
  n_ind <- nrow(pop_mat)
  all_targets <- unlist(trait_list)
  
  # 1. Background RPG% Calculation
  exclude_idx <- unique(unlist(lapply(all_targets, function(id) {
    t_idx <- which(map_df$Marker_ID == id)
    if(length(t_idx) == 0) return(NULL)
    t_pos <- map_df$Pos_cM[t_idx]; t_chr <- map_df$Chr[t_idx]
    which(map_df$Chr == t_chr & abs(map_df$Pos_cM - t_pos) <= buffer_cM)
  })))
  
  bg_indices <- setdiff(1:nrow(map_df), exclude_idx)
  bg_subset <- pop_mat[, bg_indices, drop = FALSE]
  
  obs_bg <- rowSums(!is.na(bg_subset))
  rp_hom <- rowSums(bg_subset == rp_val, na.rm = TRUE)
  hets <- rowSums(bg_subset == 1, na.rm = TRUE)
  
  rpg_raw <- ((rp_hom + (0.5 * hets)) / obs_bg) * 100
  rpg_corrected <- rpg_raw + (epsilon * 25) 
  
  # 2. Foreground Selection (Haplotype vs SNP)
  trait_confidence <- sapply(trait_list, function(ids) {
    trait_mat <- pop_mat[, ids, drop = FALSE]
    obs_t <- rowSums(!is.na(trait_mat))
    matches <- rowSums(trait_mat == 1, na.rm = TRUE)
    matches / obs_t
  })
  
  if(is.matrix(trait_confidence)) {
    full_carrier <- rowSums(trait_confidence >= (1 - epsilon), na.rm=TRUE) == length(trait_list)
  } else {
    full_carrier <- trait_confidence >= (1 - epsilon)
  }
  
  # 3. Linkage Drag (cM)
  drag_per_trait <- lapply(trait_list, function(ids) {
    t_idx_range <- which(map_df$Marker_ID %in% ids)
    t_chr <- map_df$Chr[t_idx_range[1]]
    t_pos_min <- min(map_df$Pos_cM[t_idx_range])
    t_pos_max <- max(map_df$Pos_cM[t_idx_range])
    
    chr_map <- map_df[map_df$Chr == t_chr, ]
    chr_pop <- pop_mat[, map_df$Chr == t_chr, drop = FALSE]
    
    rel_start <- which(chr_map$Marker_ID == map_df$Marker_ID[min(t_idx_range)])
    rel_end <- which(chr_map$Marker_ID == map_df$Marker_ID[max(t_idx_range)])
    
    t(apply(chr_pop, 1, function(row) {
      left_match <- which(row[1:rel_start] == rp_val)
      l_drag <- if(length(left_match) > 0) t_pos_min - chr_map$Pos_cM[max(left_match)] else t_pos_min - min(chr_map$Pos_cM)
      right_match <- which(row[rel_end:ncol(chr_pop)] == rp_val) + (rel_end - 1)
      r_drag <- if(length(right_match) > 0) chr_map$Pos_cM[min(right_match)] - t_pos_max else max(chr_map$Pos_cM) - t_pos_max
      return(l_drag + r_drag)
    }))
  })
  
  total_drag <- Reduce("+", drag_per_trait)
  
  # 4. Result Compilation
  results <- data.frame(
    Individual_ID = rownames(pop_mat),
    Call_Rate = round(rowSums(!is.na(pop_mat)) / ncol(pop_mat), 3),
    RPG_Recovery = round(rpg_corrected, 2),
    Total_Drag_cM = round(as.numeric(total_drag), 2),
    Carrier_Status = full_carrier,
    stringsAsFactors = FALSE
  )
  return(results)
}

rank_mabc_progenies <- function(mabc_results, weight_rpg = 1, weight_drag = 1, min_call_rate = 0.8) {
  candidates <- mabc_results[mabc_results$Carrier_Status == TRUE & 
                               mabc_results$Call_Rate >= min_call_rate, ]
  
  if (nrow(candidates) == 0) return(NULL) 
  
  scale_val <- function(x) {
    if(sd(x, na.rm = TRUE) == 0) return(rep(0, length(x)))
    as.numeric(scale(x))
  }
  
  std_rpg <- scale_val(candidates$RPG_Recovery)
  std_drag <- scale_val(candidates$Total_Drag_cM)
  
  candidates$Selection_Index <- round((weight_rpg * std_rpg) - (weight_drag * std_drag), 3)
  return(candidates[order(-candidates$Selection_Index), ])
}

plot_mabc_chromosome_maps <- function(ind_id, pop_mat, map_df, trait_list) {
  geno <- data.frame(
    Marker_ID = colnames(pop_mat),
    Genotype = as.factor(pop_mat[ind_id, ]),
    stringsAsFactors = FALSE
  ) %>% inner_join(map_df, by = "Marker_ID")
  
  target_markers <- unlist(trait_list)
  geno$Is_Target <- geno$Marker_ID %in% target_markers
  
  # Map 0, 1, 2 to meaningful names
  levels(geno$Genotype) <- c("RP (Homo)", "Carrier (Het)", "Donor (Homo)")
  
  ggplot(geno, aes(x = Pos_cM, y = 1, fill = Genotype)) +
    geom_tile(height = 0.8) +
    geom_vline(data = filter(geno, Is_Target), aes(xintercept = Pos_cM), color = "red", size = 1, alpha = 0.5) +
    facet_wrap(~Chr, ncol = 1, strip.position = "left") +
    scale_fill_manual(values = c("RP (Homo)" = "#2c7bb6", "Carrier (Het)" = "#fdae61", "Donor (Homo)" = "#d7191c"),
                      na.value = "grey90") +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid = element_blank(), legend.position = "bottom") +
    labs(title = paste("Genomic Recovery Idiogram:", ind_id),
         subtitle = "Blue: Recurrent Parent | Orange: Heterozygous (Donor) | Red: Trait",
         x = "Position (cM)", y = "Chromosome")
}

# =====================================================================
# 2. UI
# =====================================================================
ui <- navbarPage("MABC Production Suite",
                 tabPanel("1. Setup & Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Data Input"),
                              actionButton("load_demo", "Load Simulated BC2 Demo", class="btn-info", width="100%"),
                              hr(),
                              fileInput("file_geno", "Upload Genotype Matrix (Rows=Ind, Cols=Markers)"),
                              fileInput("file_map", "Upload Genetic Map (Columns: Marker_ID, Chr, Pos_cM)"),
                              hr(),
                              h4("Trait Definition"),
                              helpText("Format: TraitName: M1, M2... (One trait per line)"),
                              textAreaInput("trait_input", "Target Loci / Haplotypes", 
                                            value = "Trait_A: M_50\nTrait_B: M_245, M_246, M_247, M_248, M_249", rows = 4),
                              hr(),
                              h4("MABC Parameters"),
                              numericInput("eps", "Lab Error Rate (\U03B5)", 0.02, step=0.01),
                              numericInput("buf", "Background Exclusion Buffer (cM)", 10),
                              actionButton("run_analysis", "Run MABC Engine", class="btn-primary", width="100%")
                            ),
                            mainPanel(
                              h4("Population Summary"),
                              DTOutput("raw_table")
                            )
                          )
                 ),
                 tabPanel("2. Selection & Ranking",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Selection Index Weights"),
                              sliderInput("w_rpg", "Weight: RPG Recovery (+)", 0, 5, 1, step=0.5),
                              sliderInput("w_drag", "Weight: Linkage Drag (-)", 0, 5, 2, step=0.5),
                              sliderInput("min_cr", "Min Call Rate Threshold", 0.5, 1.0, 0.85, step=0.05),
                              actionButton("run_rank", "Rank Candidates", class="btn-success", width="100%")
                            ),
                            mainPanel(
                              h4("Top Elite Candidates"),
                              DTOutput("rank_table")
                            )
                          )
                 ),
                 tabPanel("3. Genomic Visualization",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Idiogram Generator"),
                              helpText("Select an individual from the ranked candidates to view their chromosome map."),
                              uiOutput("select_ind_ui")
                            ),
                            mainPanel(
                              plotOutput("chromo_plot", height = "800px")
                            )
                          )
                 ),
                 # --- NEW: Export Results Tab ---
                 tabPanel("4. Export Results",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Download Options"),
                              helpText("Ensure you have run both the Analysis (Tab 1) and Ranking (Tab 2) before downloading."),
                              hr(),
                              downloadButton("dl_raw", "Download Full Population Report (.csv)", class = "btn-block"),
                              br(),
                              downloadButton("dl_ranked", "Download Ranked Candidates (.csv)", class = "btn-block")
                            ),
                            mainPanel(
                              h4("Export Summary"),
                              p("Use these tools to export your MABC data for integration with your organization's breeding database or field planning software."),
                              tags$ul(
                                tags$li(strong("Full Population Report:"), " Contains the raw Call Rate, RPG%, and Linkage Drag for every individual in the uploaded matrix, regardless of carrier status."),
                                tags$li(strong("Ranked Candidates:"), " Contains only the individuals that successfully passed the foreground carrier check and minimum call rate thresholds, sorted by their Selection Index.")
                              )
                            )
                          )
                 )
)

# =====================================================================
# 3. SERVER
# =====================================================================
server <- function(input, output, session) {
  
  # Reactive values to hold data
  rv <- reactiveValues(geno = NULL, map = NULL, traits = NULL, raw_res = NULL, rank_res = NULL)
  
  # --- Helper: Parse Trait Input Box ---
  parse_traits <- function(text) {
    lines <- unlist(strsplit(text, "\n"))
    lines <- lines[lines != ""]
    trait_list <- list()
    for(l in lines) {
      parts <- unlist(strsplit(l, ":"))
      if(length(parts) == 2) {
        name <- trimws(parts[1])
        markers <- trimws(unlist(strsplit(parts[2], ",")))
        trait_list[[name]] <- markers
      }
    }
    return(trait_list)
  }
  
  # --- Load Demo Data ---
  observeEvent(input$load_demo, {
    set.seed(88)
    n_m <- 1000
    demo_map <- data.frame(Marker_ID = paste0("M_", 1:n_m), Chr = rep(1:10, each = 100), Pos_cM = rep(seq(0, 99, 1), 10))
    
    # Simulate BC2 (0 = RP, 1 = Het, 2 = Donor)
    demo_pop <- matrix(sample(c(0, 1), 100 * n_m, replace = TRUE, prob = c(0.875, 0.125)), nrow = 100, ncol = n_m)
    colnames(demo_pop) <- demo_map$Marker_ID
    rownames(demo_pop) <- paste0("Ind_", 1:100)
    
    # Force traits to be Het
    target_markers <- c("M_50", "M_245", "M_246", "M_247", "M_248", "M_249")
    demo_pop[, target_markers] <- 1
    
    # Inject 15% Missing Data and 2% Error
    noise_idx <- sample(1:length(demo_pop), length(demo_pop) * 0.17)
    demo_pop[sample(noise_idx, length(noise_idx)*0.88)] <- NA 
    demo_pop[sample(noise_idx, length(noise_idx)*0.12)] <- 2  
    
    rv$geno <- demo_pop
    rv$map <- demo_map
    updateTextAreaInput(session, "trait_input", value = "Trait_A: M_50\nTrait_B: M_245, M_246, M_247, M_248, M_249")
    showNotification("Demo BC2 Data Loaded (100 Individuals, 1000 Markers)", type="message")
  })
  
  # --- File Uploads ---
  observeEvent(input$file_geno, { rv$geno <- as.matrix(read.csv(input$file_geno$datapath, row.names=1, check.names=F)) })
  observeEvent(input$file_map, { rv$map <- read.csv(input$file_map$datapath) })
  
  # --- Run Analysis Engine ---
  observeEvent(input$run_analysis, {
    validate(need(rv$geno, "Please upload Genotype Matrix or Load Demo."), need(rv$map, "Please upload Genetic Map or Load Demo."))
    
    rv$traits <- parse_traits(input$trait_input)
    validate(need(length(rv$traits) > 0, "Please define at least one trait."))
    
    withProgress(message = 'Running MABC Analysis...', value = 0.5, {
      rv$raw_res <- mabc_analysis_engine(rv$geno, rv$map, rv$traits, epsilon = input$eps, buffer_cM = input$buf)
    })
    
    output$raw_table <- renderDT({
      datatable(rv$raw_res, options = list(pageLength = 10)) %>%
        formatStyle('Carrier_Status', backgroundColor = styleEqual(c(TRUE, FALSE), c('#d4edda', '#f8d7da')))
    })
    showNotification("Analysis Complete. Move to Tab 2 to rank.", type="message")
  })
  
  # --- Run Ranking/Selection ---
  observeEvent(input$run_rank, {
    validate(need(rv$raw_res, "Run Analysis in Tab 1 first."))
    
    res <- rank_mabc_progenies(rv$raw_res, weight_rpg = input$w_rpg, weight_drag = input$w_drag, min_call_rate = input$min_cr)
    
    if(is.null(res)) {
      showNotification("No candidates met the minimum Call Rate and Carrier Status.", type="error")
      rv$rank_res <- NULL
    } else {
      rv$rank_res <- res
      output$rank_table <- renderDT({
        datatable(rv$rank_res, selection = "single", options = list(pageLength = 10)) %>%
          formatStyle('Selection_Index', fontWeight = 'bold', color = 'white', backgroundColor = '#007bff')
      })
    }
  })
  
  # --- Visualization UI & Plot ---
  output$select_ind_ui <- renderUI({
    req(rv$rank_res)
    selectInput("plot_ind", "Choose Top Candidate:", choices = rv$rank_res$Individual_ID)
  })
  
  output$chromo_plot <- renderPlot({
    req(input$plot_ind, rv$geno, rv$map, rv$traits)
    plot_mabc_chromosome_maps(input$plot_ind, rv$geno, rv$map, rv$traits)
  })
  
  # --- NEW: Download Handlers ---
  output$dl_raw <- downloadHandler(
    filename = function() { paste0("mabc_full_population_report_", Sys.Date(), ".csv") },
    content = function(file) {
      req(rv$raw_res)
      write.csv(rv$raw_res, file, row.names = FALSE)
    }
  )
  
  output$dl_ranked <- downloadHandler(
    filename = function() { paste0("mabc_ranked_candidates_", Sys.Date(), ".csv") },
    content = function(file) {
      req(rv$rank_res)
      write.csv(rv$rank_res, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)