install.packages(c("shiny", "ggplot2", "lme4", "dplyr", "DT", "bslib"))
library(shiny)
library(ggplot2)
library(lme4)
library(dplyr)
library(DT)
library(bslib)

# Define UI
ui <- page_sidebar(
  title = "Field Trial Simulation & Design Optimizer",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    h4("Simulation Parameters"),
    numericInput("n_geno", "Number of Genotypes:", value = 50, min = 10, max = 500),
    sliderInput("n_reps", "Number of Replications:", min = 3, max = 10, value = 4),
    
    hr(),
    h4("True Variances"),
    numericInput("var_g", "Genotypic Variance (σ²g):", value = 15, min = 1),
    numericInput("var_e", "Random Error Variance (σ²e):", value = 5, min = 1),
    
    hr(),
    h4("Field Heterogeneity"),
    helpText("Simulates a soil fertility gradient across the field."),
    sliderInput("gradient", "Block Gradient Strength:", min = 0, max = 20, value = 10),
    
    hr(),
    actionButton("simulate", "Run Simulation", class = "btn-primary", width = "100%")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Field Visualization", 
               br(),
               plotOutput("fieldPlot", height = "500px")
      ),
      tabPanel("Design Tests (RCBD vs CRD)", 
               br(),
               h4("Impact of Blocking"),
               p("Compares modeling the blocks (RCBD) versus ignoring them (CRD)."),
               tableOutput("blockingTable"),
               plotOutput("blockingPlot", height = "400px")
      ),
      tabPanel("Replication Drop Test", 
               br(),
               h4("Impact of Dropping 1 Replication"),
               p("Compares the full RCBD dataset against a dataset with one block removed."),
               tableOutput("repDropTable")
      ),
      tabPanel("Raw Data", 
               br(),
               DTOutput("dataTable")
      )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Reactive function to run simulation and models when button is clicked
  sim_results <- eventReactive(input$simulate, {
    req(input$n_geno, input$n_reps, input$var_g, input$var_e)
    
    # 1. Simulate Data
    n_geno <- input$n_geno
    reps <- input$n_reps
    
    g_eff <- rnorm(n_geno, 0, sqrt(input$var_g))
    # Create a linear gradient across the blocks
    b_eff <- seq(-input$gradient/2, input$gradient/2, length.out = reps) 
    
    trial_data <- expand.grid(Genotype = factor(paste0("G", 1:n_geno)),
                              Block = factor(paste0("B", 1:reps)))
    
    trial_data$g_eff <- g_eff[match(trial_data$Genotype, paste0("G", 1:n_geno))]
    trial_data$b_eff <- b_eff[match(trial_data$Block, paste0("B", 1:reps))]
    trial_data$e_eff <- rnorm(nrow(trial_data), 0, sqrt(input$var_e))
    trial_data$Phenotype <- 100 + trial_data$g_eff + trial_data$b_eff + trial_data$e_eff
    
    # 2. Helper function to extract H2 safely
    get_h2 <- function(model, n_reps, has_blocks = TRUE) {
      vc <- as.data.frame(VarCorr(model))
      vg <- vc$vcov[vc$grp == "Genotype"]
      ve <- vc$vcov[vc$grp == "Residual"]
      if(length(vg) == 0) vg <- 0 # Handle singular fits
      h2 <- vg / (vg + (ve / n_reps))
      return(c(Var_G = vg, Var_E = ve, H2 = h2))
    }
    
    # 3. Fit Models
    # Model A: Full RCBD
    mod_rcbd <- lmer(Phenotype ~ (1|Block) + (1|Genotype), data = trial_data)
    res_rcbd <- get_h2(mod_rcbd, reps)
    
    # Model B: CRD (Ignore Blocks)
    mod_crd <- lmer(Phenotype ~ (1|Genotype), data = trial_data)
    res_crd <- get_h2(mod_crd, reps, has_blocks = FALSE)
    
    # Model C: RCBD with 1 less rep (Drop last block)
    data_minus_1 <- trial_data %>% filter(Block != paste0("B", reps))
    mod_minus_1 <- lmer(Phenotype ~ (1|Block) + (1|Genotype), data = data_minus_1)
    res_minus_1 <- get_h2(mod_minus_1, reps - 1)
    
    list(
      data = trial_data,
      rcbd = res_rcbd,
      crd = res_crd,
      minus_1 = res_minus_1,
      reps = reps
    )
  }, ignoreNULL = FALSE) # Run once on startup
  
  
  # Output: Field Plot
  output$fieldPlot <- renderPlot({
    res <- sim_results()
    ggplot(res$data, aes(x = Block, y = Phenotype, fill = Block)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.5, color = "darkgray") +
      theme_minimal(base_size = 14) +
      labs(title = "Simulated Phenotypic Distribution Across Blocks",
           subtitle = "Observe how the 'Block Gradient Strength' input shifts these distributions.",
           x = "Spatial Block", y = "Phenotype Value") +
      theme(legend.position = "none")
  })
  
  # Output: Blocking Comparison Table
  output$blockingTable <- renderTable({
    res <- sim_results()
    data.frame(
      Model = c("RCBD (Blocks Included)", "CRD (Blocks Ignored)"),
      Estimated_Var_G = c(res$rcbd["Var_G"], res$crd["Var_G"]),
      Estimated_Var_E = c(res$rcbd["Var_E"], res$crd["Var_E"]),
      Heritability_H2 = c(res$rcbd["H2"], res$crd["H2"])
    )
  }, digits = 3, striped = TRUE, bordered = TRUE, width = "100%")
  
  # Output: Blocking Comparison Plot
  output$blockingPlot <- renderPlot({
    res <- sim_results()
    df_plot <- data.frame(
      Model = factor(c("RCBD", "CRD"), levels = c("RCBD", "CRD")),
      Var_E = c(res$rcbd["Var_E"], res$crd["Var_E"])
    )
    ggplot(df_plot, aes(x = Model, y = Var_E, fill = Model)) +
      geom_col(width = 0.5, alpha = 0.8) +
      geom_text(aes(label = round(Var_E, 2)), vjust = -0.5, size = 5) +
      scale_fill_manual(values = c("#2ecc71", "#e74c3c")) +
      theme_minimal(base_size = 14) +
      labs(title = "Inflation of Error Variance (σ²e) when ignoring blocks",
           y = "Estimated Error Variance") +
      theme(legend.position = "none")
  })
  
  # Output: Rep Drop Table
  output$repDropTable <- renderTable({
    res <- sim_results()
    acc_full <- sqrt(res$rcbd["H2"])
    acc_drop <- sqrt(res$minus_1["H2"])
    
    data.frame(
      Scenario = c(paste(res$reps, "Reps (Full)"), paste(res$reps - 1, "Reps (Dropped 1)")),
      Heritability_H2 = c(res$rcbd["H2"], res$minus_1["H2"]),
      Selection_Accuracy = c(acc_full, acc_drop),
      Accuracy_Loss = c("Baseline", paste0(round((acc_full - acc_drop)/acc_full * 100, 2), "%"))
    )
  }, digits = 3, striped = TRUE, bordered = TRUE, width = "100%")
  
  # Output: Raw Data Table
  output$dataTable <- renderDT({
    datatable(sim_results()$data, 
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = c("g_eff", "b_eff", "e_eff", "Phenotype"), digits = 2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)