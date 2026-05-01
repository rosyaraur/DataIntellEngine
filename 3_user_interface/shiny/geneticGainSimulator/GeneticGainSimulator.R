# Load required libraries
# Load required libraries
# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)

# ==========================================
# 1. THE SIMULATION FUNCTION (Backend Logic)
# ==========================================
simulate_replicated_met <- function(n_lines, n_env, n_reps, n_cycles, 
                                    base_mean, var_g, var_env, var_gxe, var_e, 
                                    sel_frac, gain_pct, var_decay) {
  
  all_obs_data <- list()
  all_line_means <- list()
  
  withProgress(message = 'Simulating Breeding Cycles...', value = 0, {
    for (cycle in 0:n_cycles) {
      current_mean <- base_mean * ((1 + gain_pct) ^ cycle)
      current_var_g <- var_g * ((1 - var_decay) ^ cycle)
      
      lines_df <- data.frame(
        Line_ID = paste0("C", cycle, "_L", 1:n_lines), 
        TBV = rnorm(n_lines, mean = current_mean, sd = sqrt(current_var_g))
      )
      env_df <- data.frame(
        Env_ID = paste0("Env_", 1:n_env), 
        Env_Effect = rnorm(n_env, mean = 0, sd = sqrt(var_env))
      )
      gxe_df <- expand_grid(Line_ID = lines_df$Line_ID, Env_ID = env_df$Env_ID) %>%
        mutate(GxE = rnorm(n(), mean = 0, sd = sqrt(var_gxe)))
      
      cycle_obs <- expand_grid(Line_ID = lines_df$Line_ID, Env_ID = env_df$Env_ID, Rep_ID = paste0("Rep_", 1:n_reps)) %>%
        left_join(lines_df, by = "Line_ID") %>%
        left_join(env_df, by = "Env_ID") %>%
        left_join(gxe_df, by = c("Line_ID", "Env_ID")) %>%
        mutate(
          Cycle = cycle,
          Error = rnorm(n(), mean = 0, sd = sqrt(var_e)),
          Phenotype = TBV + Env_Effect + GxE + Error
        )
      
      cycle_obs$Phenotype <- ifelse(cycle_obs$Phenotype < 0, 0, cycle_obs$Phenotype)
      
      line_summary <- cycle_obs %>%
        group_by(Cycle, Line_ID, TBV) %>%
        summarise(Mean_Phenotype = mean(Phenotype), .groups = 'drop')
      
      threshold <- quantile(line_summary$Mean_Phenotype, probs = 1 - sel_frac)
      line_summary <- line_summary %>% mutate(Selected = Mean_Phenotype >= threshold)
      
      all_obs_data[[cycle + 1]] <- cycle_obs
      all_line_means[[cycle + 1]] <- line_summary
      
      incProgress(1 / (n_cycles + 1)) 
    }
  })
  
  full_obs_data <- bind_rows(all_obs_data)
  full_line_means <- bind_rows(all_line_means)
  
  cycle_summary <- full_line_means %>%
    group_by(Cycle) %>%
    summarise(
      Pop_Mean_Pheno = round(mean(Mean_Phenotype), 2),
      Pop_Mean_TBV = round(mean(TBV), 2),
      Mean_TBV_Selected = round(mean(TBV[Selected == TRUE]), 2),
      Realized_Genetic_Gain = round(Mean_TBV_Selected - Pop_Mean_TBV, 2),
      .groups = 'drop'
    )
  
  return(list(Observations = full_obs_data, Line_Means = full_line_means, Summary = cycle_summary))
}

# ==========================================
# 2. USER INTERFACE (UI)
# ==========================================
ui <- fluidPage(
  titlePanel("Breeding Program Simulator: Multi-Environment Trials"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Trial Design"),
      numericInput("n_lines", "Number of Lines per Cycle:", value = 1000, min = 100),
      sliderInput("n_env", "Environments (Locations):", min = 1, max = 20, value = 4),
      sliderInput("n_reps", "Replications per Env:", min = 1, max = 5, value = 2),
      sliderInput("n_cycles", "Selection Cycles:", min = 1, max = 20, value = 10),
      
      h4("Variance Components"),
      numericInput("base_mean", "Base Population Mean Yield:", value = 60),
      numericInput("var_g", "Genetic Variance (Var_G):", value = 600),
      numericInput("var_env", "Environment Variance (Var_Env):", value = 300),
      numericInput("var_gxe", "GxE Variance (Var_GxE):", value = 200),
      numericInput("var_e", "Plot Error Variance (Var_E):", value = 800),
      
      h4("Selection Parameters"),
      sliderInput("sel_frac", "Selection Fraction (Top %):", min = 0.01, max = 0.50, value = 0.10, step = 0.01),
      sliderInput("gain_pct", "Expected % Gain / Cycle:", min = 0, max = 0.10, value = 0.02, step = 0.005),
      sliderInput("var_decay", "% Variance Decay / Cycle:", min = 0, max = 0.15, value = 0.04, step = 0.01),
      
      br(),
      actionButton("run_sim", "Run Simulation", class = "btn-primary", style="width: 100%; font-size: 16px; font-weight: bold;")
    ),
    
    mainPanel(
      tabsetPanel(
        # NEW TABS ADDED HERE
        tabPanel("Horizontal Histograms", 
                 br(),
                 plotOutput("pheno_plot", height = "700px")),
        tabPanel("Box Plots", 
                 br(),
                 plotOutput("box_plot", height = "600px")),
        tabPanel("Regression Analysis", 
                 br(),
                 plotOutput("regression_plot", height = "600px")),
        tabPanel("Genetic Gain (TBV)", 
                 br(),
                 plotOutput("gain_plot", height = "600px")),
        tabPanel("Summary Metrics", 
                 br(),
                 DTOutput("summary_table"))
      )
    )
  )
)

# ==========================================
# 3. SERVER LOGIC
# ==========================================
server <- function(input, output, session) {
  
  sim_results <- eventReactive(input$run_sim, {
    simulate_replicated_met(
      n_lines = input$n_lines, n_env = input$n_env, n_reps = input$n_reps, n_cycles = input$n_cycles,
      base_mean = input$base_mean, var_g = input$var_g, var_env = input$var_env,
      var_gxe = input$var_gxe, var_e = input$var_e, sel_frac = input$sel_frac,
      gain_pct = input$gain_pct, var_decay = input$var_decay
    )
  }, ignoreNULL = FALSE) 
  
  # ---------------------------------------------------------
  # TAB 1: Horizontal Histogram Trajectory (With Cycle Colors)
  # ---------------------------------------------------------
  output$pheno_plot <- renderPlot({
    req(sim_results())
    obs_data <- sim_results()$Observations
    data_list <- split(obs_data$Phenotype, obs_data$Cycle)
    
    check_distribution_progress_horizontal <- function(data_list, main_title = "", cycles_prefix = "Cycle ") {
      num_cycles <- length(data_list)
      
      all_vals <- unlist(data_list)
      global_min <- floor(min(all_vals, na.rm = TRUE))
      global_max <- ceiling(max(all_vals, na.rm = TRUE))
      y_limits <- c(global_min, global_max) 
      
      # Generate a color gradient for the cycles
      cycle_colors <- hcl.colors(num_cycles, palette = "Zissou 1", alpha = 0.85)
      
      par(mfrow = c(1, num_cycles), mar = c(3, 0, 2, 0), oma = c(2, 4, 2, 1))
      
      for (i in 1:num_cycles) {
        cycle_name <- paste0(cycles_prefix, names(data_list)[i])
        hist_data <- hist(data_list[[i]], breaks = 20, plot = FALSE)
        density_data <- density(data_list[[i]])
        
        scale_factor <- max(hist_data$counts) / max(density_data$y)
        scaled_density_y <- density_data$y * scale_factor
        
        # Get the specific color for this cycle
        current_color <- cycle_colors[i]
        
        if (i == 1) {
          plot(hist_data$counts, hist_data$mids, type = "n", 
               xlab = "", ylab = "", main = cycle_name,
               xlim = c(0, max(hist_data$counts) * 1.2),
               ylim = y_limits, yaxt = "n", xaxt = "n")
          axis(2, las = 1)
        } else {
          plot(hist_data$counts, hist_data$mids, type = "n", 
               xlab = "", ylab = "", main = cycle_name,
               xlim = c(0, max(hist_data$counts) * 1.2),
               ylim = y_limits, yaxt = "n", xaxt = "n")
        }
        
        box(col = "gray20", lwd = 1.5)
        
        # Draw bars using the cycle-specific color
        for (j in 1:(length(hist_data$breaks) - 1)) {
          rect(0, hist_data$breaks[j], hist_data$counts[j], hist_data$breaks[j + 1], 
               col = current_color, border = "gray40")
        }
        
        lines(scaled_density_y, density_data$x, col = "black", lwd = 2, lty = 1)
        
        cycle_mean <- mean(data_list[[i]], na.rm = TRUE)
        abline(h = cycle_mean, col = "red", lty = 2, lwd = 2)
      }
      mtext("Phenotypic Yield (bu/ac)", side = 2, outer = TRUE, line = 2, font = 2)
    }
    
    check_distribution_progress_horizontal(data_list)
  })
  
  # ---------------------------------------------------------
  # TAB 2: Standard Box Plots
  # ---------------------------------------------------------
  output$box_plot <- renderPlot({
    req(sim_results())
    obs_data <- sim_results()$Observations
    
    ggplot(obs_data, aes(x = as.factor(Cycle), y = Phenotype, fill = as.factor(Cycle))) +
      geom_boxplot(alpha = 0.8, color = "gray20", outlier.alpha = 0.1, outlier.size = 1) +
      # Use a robust color scale to match cycles
      scale_fill_viridis_d(option = "turbo", alpha = 0.7) + 
      labs(title = "Phenotypic Variance Across Selection Cycles",
           x = "Selection Cycle",
           y = "Phenotypic Yield (bu/ac)") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "none") # Hide legend since X-axis explains it
  })
  
  # ---------------------------------------------------------
  # TAB 3: Plain Regression Plot
  # ---------------------------------------------------------
  output$regression_plot <- renderPlot({
    req(sim_results())
    summary_data <- sim_results()$Summary
    
    ggplot(summary_data, aes(x = Cycle)) +
      # Plot Phenotypic Mean Trend
      geom_point(aes(y = Pop_Mean_Pheno, color = "Phenotypic Mean"), size = 3) +
      geom_smooth(aes(y = Pop_Mean_Pheno, color = "Phenotypic Mean"), method = "lm", se = FALSE, linetype = "dashed") +
      
      # Plot Genotypic (TBV) Mean Trend
      geom_point(aes(y = Pop_Mean_TBV, color = "Genotypic Mean (TBV)"), size = 4) +
      geom_smooth(aes(y = Pop_Mean_TBV, color = "Genotypic Mean (TBV)"), method = "lm", se = TRUE, alpha = 0.2) +
      
      scale_color_manual(values = c("Phenotypic Mean" = "darkgray", "Genotypic Mean (TBV)" = "#d95f02")) +
      labs(title = "Linear Regression of Population Averages Over Time",
           x = "Selection Cycle",
           y = "Mean Yield (bu/ac)",
           color = "Measurement") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom")
  })
  
  # ---------------------------------------------------------
  # TAB 4: Genetic Gain Trajectory (Elite vs Population)
  # ---------------------------------------------------------
  output$gain_plot <- renderPlot({
    req(sim_results())
    summary_data <- sim_results()$Summary
    
    ggplot(summary_data, aes(x = Cycle)) +
      geom_line(aes(y = Pop_Mean_TBV, color = "Population Mean TBV"), size = 1.2) +
      geom_point(aes(y = Pop_Mean_TBV, color = "Population Mean TBV"), size = 3) +
      geom_line(aes(y = Mean_TBV_Selected, color = "Selected Elite Mean TBV"), size = 1.2, linetype = "dashed") +
      geom_point(aes(y = Mean_TBV_Selected, color = "Selected Elite Mean TBV"), size = 3) +
      scale_color_manual(values = c("Population Mean TBV" = "#1b9e77", "Selected Elite Mean TBV" = "#d95f02")) +
      labs(title = "Realized Genetic Gain (Selection Differential)",
           x = "Selection Cycle",
           y = "True Breeding Value (Yield)",
           color = "Metric") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom")
  })
  
  # ---------------------------------------------------------
  # TAB 5: Summary Metrics Data Table
  # ---------------------------------------------------------
  output$summary_table <- renderDT({
    req(sim_results())
    datatable(sim_results()$Summary, 
              options = list(pageLength = 20, dom = 't'),
              rownames = FALSE,
              class = 'cell-border stripe')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)