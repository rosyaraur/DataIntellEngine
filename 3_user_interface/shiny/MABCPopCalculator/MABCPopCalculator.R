
install.packages(c("shiny", "rhandsontable", "ggplot2", "dplyr"))

library(shiny)
library(rhandsontable)

# =====================================================================
# 1. CORE MATH ENGINE (WIDE FORMAT + RPG EXPECTATION)
# =====================================================================
calculate_wide_pop_size <- function(df, target_conf = 0.95, bg_top_pct = 0.05, generation = 1) {
  
  p_total <- 1.0
  
  # Foreground Selection Math
  for (i in 1:nrow(df)) {
    row <- df[i, ]
    state1 <- row$State1
    state2 <- row$State2
    seg_count <- sum(c(state1, state2) == "Segregating", na.rm = TRUE)
    
    if (seg_count == 0) {
      p_chr <- 1.0
    } else if (seg_count == 1) {
      p_chr <- 0.5
    } else if (seg_count == 2) {
      dist_cM <- abs(as.numeric(row$Position2) - as.numeric(row$Position1))
      r <- 0.5 * (1 - exp(-2 * (dist_cM / 100))) 
      phase <- row$Phase
      if (phase == "Coupling") {
        p_chr <- 0.5 * (1 - r)
      } else if (phase == "Repulsion") {
        p_chr <- 0.5 * r
      } else {
        stop(paste("Row", i, ": Phase must be Coupling or Repulsion."))
      }
    }
    p_total <- p_total * p_chr
  }
  
  # Background Selection & RPG Math
  # 1. Calculate base N
  if (p_total == 1.0) {
    base_n <- 1
    final_n <- ceiling(1 / bg_top_pct)
  } else if (p_total == 0) {
    return(list(Target_Gamete_Probability = 0, Base_Carriers_Needed = Inf, Final_Pop_Size = Inf, Expected_RPG = 0))
  } else {
    base_n <- log(1 - target_conf) / log(1 - p_total)
    final_n <- ceiling(base_n / bg_top_pct)
  }
  
  # 2. Calculate Expected RPG %
  mean_rpg <- 1 - (0.5)^(generation + 1)
  z_score <- qnorm(1 - bg_top_pct) # Get Z-score for the top percentile
  
  # Approximate genome variance (assuming ~100 independent effective chromosomal segments)
  sd_rpg <- sqrt((mean_rpg * (1 - mean_rpg)) / 100)
  
  # Calculate max expected, capped at 99.9%
  expected_rpg <- min(0.999, mean_rpg + (z_score * sd_rpg))
  
  return(list(
    Target_Gamete_Probability = p_total,
    Base_Carriers_Needed = ceiling(base_n),
    Final_Pop_Size = final_n,
    Expected_RPG = expected_rpg
  ))
}

# =====================================================================
# 2. UI DEFINITION
# =====================================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML(".kpi-box { padding: 15px; border-radius: 5px; color: white; text-align: center; margin-bottom: 20px; }
                     .bg-primary { background-color: #007bff; }
                     .bg-success { background-color: #28a745; }
                     .bg-danger { background-color: #dc3545; }
                     .bg-info { background-color: #17a2b8; }"))
  ),
  
  titlePanel("MABC Population Sizing & RPG Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Global Parameters"),
      sliderInput("generation", "Backcross Generation (BCn)", min=1, max=6, value=1, step=1),
      sliderInput("target_conf", "Target Confidence Level", min=0.50, max=0.99, value=0.95, step=0.01),
      sliderInput("bg_top_pct", "Background Selection (Top %)", min=0.01, max=0.50, value=0.05, step=0.01),
      br(),
      actionButton("add_row", "Add Chromosome Row", icon = icon("plus"), class = "btn-success", width="100%"),
      br(),br(),
      actionButton("del_row", "Remove Last Row", icon = icon("minus"), class = "btn-danger", width="100%")
    ),
    
    mainPanel(
      width = 9,
      h4("Linkage Block Data"),
      rHandsontableOutput("hot_table"),
      br(),
      uiOutput("error_ui"),
      fluidRow(
        column(3, div(class="kpi-box bg-primary", h5("Gamete Prob"), h3(textOutput("prob_out")))),
        column(3, div(class="kpi-box bg-primary", h5("Base Carriers"), h3(textOutput("base_out")))),
        column(3, uiOutput("final_pop_ui")),
        column(3, div(class="kpi-box bg-info", h5("Expected RPG"), h3(textOutput("rpg_out"))))
      )
    )
  )
)

# =====================================================================
# 3. SERVER LOGIC
# =====================================================================
server <- function(input, output, session) {
  
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
  
  observe({
    if (!is.null(input$hot_table)) rv$data <- hot_to_r(input$hot_table)
  })
  
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
  
  output$hot_table <- renderRHandsontable({
    rhandsontable(rv$data, rowHeaders = FALSE, stretchH = "all") %>%
      hot_col("Position1", type = "numeric") %>%
      hot_col("Position2", type = "numeric") %>%
      hot_col("State1", type = "dropdown", source = c("Homozygous", "Segregating"), strict = TRUE) %>%
      hot_col("State2", type = "dropdown", source = c("Homozygous", "Segregating"), strict = TRUE) %>%
      hot_col("Phase", type = "dropdown", source = c("NA", "Coupling", "Repulsion"), strict = TRUE)
  })
  
  calc_results <- reactive({
    req(rv$data)
    tryCatch({
      res <- calculate_wide_pop_size(rv$data, input$target_conf, input$bg_top_pct, input$generation)
      list(status = "success", data = res)
    }, error = function(e) list(status = "error", message = e$message))
  })
  
  output$error_ui <- renderUI({
    res <- calc_results()
    if (res$status == "error") div(style="color: red; font-weight: bold;", res$message)
  })
  
  output$prob_out <- renderText({
    res <- calc_results()
    if (res$status == "error") return("---")
    paste0(signif(res$data$Target_Gamete_Probability * 100, 3), "%")
  })
  
  output$base_out <- renderText({
    res <- calc_results()
    if (res$status == "error") return("---")
    format(res$data$Base_Carriers_Needed, big.mark=",")
  })
  
  output$final_pop_ui <- renderUI({
    res <- calc_results()
    if (res$status == "error") return(div(class="kpi-box bg-danger", h5("Final Pop Size"), h3("Error")))
    n <- res$data$Final_Pop_Size
    formatted_n <- format(n, big.mark=",")
    box_class <- ifelse(n > 5000, "kpi-box bg-danger", "kpi-box bg-success")
    div(class=box_class, h5("Final Pop Size"), h3(formatted_n))
  })
  
  output$rpg_out <- renderText({
    res <- calc_results()
    if (res$status == "error") return("---")
    paste0(round(res$data$Expected_RPG * 100, 1), "%")
  })
}

shinyApp(ui = ui, server = server)