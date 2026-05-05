library(shiny)
library(rhandsontable)
library(ggplot2)
library(dplyr)

# =====================================================================
# 1. CORE MATH ENGINE: Introgression / BACKCROSSING (MABC)/ F2
# =====================================================================
calculate_mabc_pop_size <- function(df, target_conf = 0.95, bg_top_pct = 0.05, generation = 1) {
  p_total <- 1.0
  
  for (i in 1:nrow(df)) {
    row <- df[i, ]
    seg_count <- sum(c(row$State1, row$State2) == "Segregating", na.rm = TRUE)
    
    if (seg_count == 0) {
      p_chr <- 1.0
    } else if (seg_count == 1) {
      p_chr <- 0.5
    } else if (seg_count == 2) {
      dist_cM <- abs(as.numeric(row$Position2) - as.numeric(row$Position1))
      r <- 0.5 * (1 - exp(-2 * (dist_cM / 100))) 
      if (row$Phase == "Coupling") {
        p_chr <- 0.5 * (1 - r)
      } else if (row$Phase == "Repulsion") {
        p_chr <- 0.5 * r
      } else stop(paste("Row", i, ": Phase must be Coupling or Repulsion."))
    }
    p_total <- p_total * p_chr
  }
  
  if (p_total == 1.0) return(list(Prob = 1, Base = 1, Final = ceiling(1 / bg_top_pct), Expected_BG = 1 - (0.5)^(generation + 1)))
  if (p_total == 0) return(list(Prob = 0, Base = Inf, Final = Inf, Expected_BG = 0))
  
  base_n <- log(1 - target_conf) / log(1 - p_total)
  final_n <- ceiling(base_n / bg_top_pct)
  
  mean_rpg <- 1 - (0.5)^(generation + 1)
  sd_rpg <- sqrt((mean_rpg * (1 - mean_rpg)) / 100)
  expected_rpg <- min(0.999, mean_rpg + (qnorm(1 - bg_top_pct) * sd_rpg))
  
  return(list(Prob = p_total, Base = ceiling(base_n), Final = final_n, Expected_BG = expected_rpg))
}

# =====================================================================
# 2. CORE MATH ENGINE: SELFING (F2)
# =====================================================================
calculate_selfing_pop_size <- function(df, target_conf = 0.95, bg_top_pct = 0.05) {
  p_total <- 1.0
  
  for (i in 1:nrow(df)) {
    row <- df[i, ]
    seg_count <- sum(c(row$State1, row$State2) == "Segregating", na.rm = TRUE)
    
    if (seg_count == 0) {
      p_gamete <- 1.0
    } else if (seg_count == 1) {
      p_gamete <- 0.5
    } else if (seg_count == 2) {
      dist_cM <- abs(as.numeric(row$Position2) - as.numeric(row$Position1))
      r <- 0.5 * (1 - exp(-2 * (dist_cM / 100)))
      if (row$Phase == "Coupling") {
        p_gamete <- 0.5 * (1 - r)
      } else if (row$Phase == "Repulsion") {
        p_gamete <- 0.5 * r
      } else stop(paste("Row", i, ": Phase must be Coupling or Repulsion."))
    }
    
    # SELFING: Square the gamete probability to achieve Fixation (Homozygosity)
    p_zygote <- p_gamete^2
    p_total <- p_total * p_zygote
  }
  
  if (p_total == 1.0) return(list(Prob = 1, Base = 1, Final = ceiling(1 / bg_top_pct), Expected_BG = 0.5))
  if (p_total == 0) return(list(Prob = 0, Base = Inf, Final = Inf, Expected_BG = 0))
  
  base_n <- log(1 - target_conf) / log(1 - p_total)
  final_n <- ceiling(base_n / bg_top_pct)
  
  mean_bg <- 0.50
  sd_bg <- sqrt((mean_bg * (1 - mean_bg)) / 100) 
  expected_bg <- min(0.999, mean_bg + (qnorm(1 - bg_top_pct) * sd_bg))
  
  return(list(Prob = p_total, Base = ceiling(base_n), Final = final_n, Expected_BG = expected_bg))
}

# =====================================================================
# 3. UI DEFINITION
# =====================================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .kpi-box { padding: 15px; border-radius: 5px; color: white; text-align: center; margin-bottom: 20px; }
      .bg-primary { background-color: #007bff; }
      .bg-success { background-color: #28a745; }
      .bg-danger { background-color: #dc3545; }
      .bg-info { background-color: #17a2b8; }
      .table-controls { margin-top: 10px; margin-bottom: 20px; }
    "))
  ),
  
  titlePanel("Quantitative Introgression Sizing Suite"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Breeding Strategy"),
      radioButtons("strategy", label = NULL, 
                   choices = c("Backcrossing (MABC)" = "mabc", "Selfing (F2 / Pedigree)" = "selfing"), 
                   selected = "mabc"),
      hr(),
      h4("Global Parameters"),
      conditionalPanel(
        condition = "input.strategy == 'mabc'",
        sliderInput("generation", "Backcross Generation (BCn)", min=1, max=6, value=1, step=1)
      ),
      sliderInput("target_conf", "Target Confidence Level", min=0.50, max=0.99, value=0.95, step=0.01),
      sliderInput("bg_top_pct", "Background Selection (Top %)", min=0.01, max=0.50, value=0.05, step=0.01),
      hr(),
      actionButton("add_row", "Add Chromosome Row", icon = icon("plus"), class = "btn-success", width="100%"),
      br(),br(),
      actionButton("del_row", "Remove Last Row", icon = icon("minus"), class = "btn-danger", width="100%")
    ),
    
    mainPanel(
      width = 9,
      h4("Linkage Block Data (Editable)"),
      rHandsontableOutput("hot_table"),
      br(),
      uiOutput("error_ui"),
      
      fluidRow(
        column(3, div(class="kpi-box bg-primary", h5(uiOutput("prob_label")), h3(textOutput("prob_out")))),
        column(3, div(class="kpi-box bg-primary", h5("Base Plants Needed"), h3(textOutput("base_out")))),
        column(3, uiOutput("final_pop_ui")),
        column(3, div(class="kpi-box bg-info", h5(uiOutput("bg_label")), h3(textOutput("bg_out"))))
      ),
      
      hr(),
      h4("Genetic Linkage Map"),
      plotOutput("genetic_map_plot", height = "300px")
    )
  )
)

# =====================================================================
# 4. SERVER LOGIC
# =====================================================================
server <- function(input, output, session) {
  
  # Initialize with user's specific data
  init_data <- data.frame(
    Chromosome = c("1", "2", "3", "4"),
    Locus1 = c("QTL1", "hpd1", "QTL3", "QTL5"),
    Position1 = c(10, 18, 87, 44),
    State1 = factor(c("Homozygous", "Segregating", "Segregating", "Homozygous"), levels = c("Homozygous", "Segregating")),
    Locus2 = c("QTL2", "hr2", "QTL4", "QTL6"),
    Position2 = c(16, 32, 96, 55),
    State2 = factor(c("Segregating", "Segregating", "Segregating", "Homozygous"), levels = c("Homozygous", "Segregating")),
    Phase = factor(c("NA", "Repulsion", "Coupling", "NA"), levels = c("NA", "Coupling", "Repulsion")),
    stringsAsFactors = FALSE
  )
  
  rv <- reactiveValues(data = init_data)
  
  # Capture table edits
  observe({
    if (!is.null(input$hot_table)) rv$data <- hot_to_r(input$hot_table)
  })
  
  # Row Management
  observeEvent(input$add_row, {
    new_row <- data.frame(
      Chromosome = paste(nrow(rv$data) + 1), Locus1 = "", Position1 = 0, 
      State1 = factor("Homozygous", levels = c("Homozygous", "Segregating")),
      Locus2 = "", Position2 = 0, 
      State2 = factor("Homozygous", levels = c("Homozygous", "Segregating")),
      Phase = factor("NA", levels = c("NA", "Coupling", "Repulsion")),
      stringsAsFactors = FALSE
    )
    rv$data <- rbind(rv$data, new_row)
  })
  
  observeEvent(input$del_row, {
    if (nrow(rv$data) > 1) rv$data <- rv$data[-nrow(rv$data), ]
  })
  
  # Render the editable grid with strict dropdowns
  output$hot_table <- renderRHandsontable({
    rhandsontable(rv$data, rowHeaders = FALSE, stretchH = "all") %>%
      hot_col("Position1", type = "numeric") %>%
      hot_col("Position2", type = "numeric") %>%
      hot_col("State1", type = "dropdown", source = c("Homozygous", "Segregating"), strict = TRUE) %>%
      hot_col("State2", type = "dropdown", source = c("Homozygous", "Segregating"), strict = TRUE) %>%
      hot_col("Phase", type = "dropdown", source = c("NA", "Coupling", "Repulsion"), strict = TRUE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })
  
  # Route the calculation based on selected strategy
  calc_results <- reactive({
    req(rv$data)
    tryCatch({
      if (input$strategy == "mabc") {
        res <- calculate_mabc_pop_size(rv$data, input$target_conf, input$bg_top_pct, input$generation)
      } else {
        res <- calculate_selfing_pop_size(rv$data, input$target_conf, input$bg_top_pct)
      }
      list(status = "success", data = res)
    }, error = function(e) list(status = "error", message = e$message))
  })
  
  # Dynamic Labels based on Strategy
  output$prob_label <- renderUI({ ifelse(input$strategy == "mabc", "Gamete Probability", "Zygote Probability") })
  output$bg_label <- renderUI({ ifelse(input$strategy == "mabc", "Expected RPG", "Expected F2 BG") })
  
  output$error_ui <- renderUI({
    res <- calc_results()
    if (res$status == "error") div(style="color: red; font-weight: bold; margin-bottom: 10px;", res$message)
  })
  
  # KPIs
  output$prob_out <- renderText({
    res <- calc_results()
    if (res$status == "error") return("---")
    paste0(signif(res$data$Prob * 100, 3), "%")
  })
  
  output$base_out <- renderText({
    res <- calc_results()
    if (res$status == "error") return("---")
    format(res$data$Base, big.mark=",")
  })
  
  output$bg_out <- renderText({
    res <- calc_results()
    if (res$status == "error") return("---")
    paste0(round(res$data$Expected_BG * 100, 1), "%")
  })
  
  output$final_pop_ui <- renderUI({
    res <- calc_results()
    if (res$status == "error") return(div(class="kpi-box bg-danger", h5("Final Pop Size"), h3("Error")))
    
    n <- res$data$Final
    formatted_n <- format(n, big.mark=",")
    # Tighter threshold for warning when selfing
    warning_threshold <- ifelse(input$strategy == "mabc", 5000, 25000)
    box_class <- ifelse(n > warning_threshold, "kpi-box bg-danger", "kpi-box bg-success")
    div(class=box_class, h5("Final Pop Size"), h3(formatted_n))
  })
  
  # Genetic Map Visualizer
  output$genetic_map_plot <- renderPlot({
    req(rv$data)
    df <- rv$data
    
    # Restructure wide data to long data for ggplot
    df1 <- data.frame(Gene=df$Locus1, Chromosome=df$Chromosome, Position=as.numeric(df$Position1), State=df$State1, Phase=df$Phase)
    df2 <- data.frame(Gene=df$Locus2, Chromosome=df$Chromosome, Position=as.numeric(df$Position2), State=df$State2, Phase=df$Phase)
    plot_df <- rbind(df1, df2)
    plot_df <- plot_df[!is.na(plot_df$Position) & plot_df$Gene != "", ]
    
    ggplot(plot_df, aes(x = Chromosome, y = Position, color = State, shape = Phase)) +
      geom_segment(aes(x = Chromosome, xend = Chromosome, y = 0, yend = max(Position, na.rm=T) + 20), 
                   color = "grey80", size = 3) + 
      geom_point(size = 5) +
      geom_text(aes(label = Gene), vjust = -1.5, size = 4, color = "black") +
      scale_color_manual(values = c("Segregating" = "#dc3545", "Homozygous" = "#28a745")) +
      coord_flip() + 
      theme_minimal() +
      labs(y = "Position (cM)", x = "Chromosome") +
      theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 12, face = "bold"))
  })
}

shinyApp(ui = ui, server = server)