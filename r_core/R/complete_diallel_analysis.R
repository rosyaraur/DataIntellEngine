#' Complete Diallel Analysis (Half-Diallel)
#'
#' @description
#' Performs Griffing's Half-Diallel Analysis to estimate General Combining Ability (GCA) 
#' and Specific Combining Ability (SCA). It fits a mixed model using the \code{sommer} 
#' package to properly pool parental variances, extracts the Best Linear Unbiased 
#' Predictors (BLUPs), and generates a comprehensive visualization using \code{ggplot2}.
#'
#' @param data A data frame containing the diallel cross data in long format 
#'   (one observation per row).
#' @param p1_var Character string. The name of the column containing Parent 1 identifiers.
#' @param p2_var Character string. The name of the column containing Parent 2 identifiers.
#' @param rep_var Character string. The name of the column containing Replication or Block identifiers.
#' @param resp_var Character string. The name of the column containing the response/trait variable (e.g., Yield).
#' @param trait_name Character string. The human-readable name of the trait used for plot titles. 
#'   Default is "Trait".
#'
#' @return A named list containing five elements:
#' \describe{
#'   \item{\code{Model}}{The fitted mixed model object from \code{sommer::mmer()}.}
#'   \item{\code{GCA_Estimates}}{A data frame containing the GCA estimates (BLUPs) for each parent.}
#'   \item{\code{SCA_Estimates}}{A data frame containing the SCA estimates (BLUPs) for each specific cross.}
#'   \item{\code{Plot_Data}}{A data frame containing the merged GCA and SCA data used to build the heatmap.}
#'   \item{\code{Plot}}{A \code{patchwork} object containing the final arranged GCA/SCA visualizations.}
#' }
#'
#' @import sommer dplyr tidyr ggplot2 patchwork stringr
#'
#' @examples
#' \dontrun{
#' # Assuming 'my_diallel_data' is a data frame in long format:
#' results <- complete_diallel_analysis(
#'   data = my_diallel_data, 
#'   p1_var = "P1", 
#'   p2_var = "P2", 
#'   rep_var = "Rep", 
#'   resp_var = "Yield", 
#'   trait_name = "Grain Yield"
#' )
#' 
#' # Print the extracted GCA estimates
#' print(results$GCA_Estimates)
#' }
#' 
#' @export
complete_diallel_analysis <- function(data, p1_var, p2_var, rep_var, resp_var, trait_name = "Trait") {
  
  # -------------------------------------------------------------------
  # Step 1: Data Preparation
  # -------------------------------------------------------------------
  # Ensure columns are treated as factors
  df <- data %>%
    mutate(
      P1 = as.factor(.data[[p1_var]]),
      P2 = as.factor(.data[[p2_var]]),
      Rep = as.factor(.data[[rep_var]]),
      Cross = as.factor(paste0(P1, "x", P2)),
      Yield = .data[[resp_var]]
    )
  
  # Get a list of unique parents to build the full grid later
  all_parents <- sort(unique(c(as.character(df$P1), as.character(df$P2))))
  num_parents <- length(all_parents)
  
  # -------------------------------------------------------------------
  # Step 2: Fit Mixed Model using sommer
  # -------------------------------------------------------------------
  # sommer's overlay() eliminates the need for manual dummy variables
  cat("Fitting mixed model with sommer...\n")
  
  mod <- mmer(
    fixed = Yield ~ Rep, 
    random = ~ vsr(overlay(P1, P2)) + vsr(Cross),
    rcov = ~ units,
    data = df,
    verbose = FALSE
  )
  
  # -------------------------------------------------------------------
  # Step 3: Extract GCA & SCA Estimates (BLUPs)
  # -------------------------------------------------------------------
  # Extract GCA
  # sommer names the overlay BLUPs based on the parent names
  gca_list <- mod$U[[1]]$Yield
  gca_df <- data.frame(
    Parent = str_replace(names(gca_list), "overlay\\(P1, P2\\)", ""),
    GCA = as.numeric(gca_list)
  )
  
  # Extract SCA
  sca_list <- mod$U[[2]]$Yield
  sca_df <- data.frame(
    Cross = str_replace(names(sca_list), "Cross", ""),
    SCA = as.numeric(sca_list)
  ) %>%
    separate(Cross, into = c("P1", "P2"), sep = "x", remove = FALSE) %>%
    mutate(
      SCA_Label = ifelse(SCA > 0, paste0("+", sprintf("%.1f", SCA)), sprintf("%.1f", SCA))
    )
  
  # -------------------------------------------------------------------
  # Step 4: Build Heatmap Grid & Merge Effects
  # -------------------------------------------------------------------
  # Create full N x N grid to ensure missing crosses show as blanks
  full_grid <- expand.grid(P1 = all_parents, P2 = all_parents) %>%
    # Filter for half-diallel (P2 >= P1)
    filter(as.numeric(P2) >= as.numeric(P1))
  
  # Merge GCA and SCA data into the grid
  plot_data <- full_grid %>%
    left_join(gca_df, by = c("P1" = "Parent")) %>% rename(GCA1 = GCA) %>%
    left_join(gca_df, by = c("P2" = "Parent")) %>% rename(GCA2 = GCA) %>%
    left_join(sca_df, by = c("P1", "P2"))
  
  # -------------------------------------------------------------------
  # Step 5: Define & Render the Plot via ggplot2 & patchwork
  # -------------------------------------------------------------------
  # Center Heatmap
  p_heatmap <- ggplot(plot_data, aes(x = P2, y = P1, fill = SCA)) +
    geom_tile(color = "gray") +
    geom_text(aes(label = SCA_Label), size = 3, fontface = "bold", na.rm = TRUE) +
    scale_fill_gradient2(low = "#D7191C", mid = "#FFFFBF", high = "#1A9641", midpoint = 0, na.value = "white") +
    scale_y_discrete(limits = rev(all_parents)) +
    labs(x = "Parent 2", y = "Parent 1", fill = "SCA Effect") +
    theme_minimal() +
    theme(legend.position = "none", panel.grid = element_blank())
  
  # Top Margin: Parent 2 GCA Bar Chart
  p_gca2 <- ggplot(gca_df, aes(x = Parent, y = GCA)) +
    geom_col(fill = "#428BCA", width = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = NULL, y = "GCA (P2)", title = paste("Combining Ability for", trait_name)) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.minor = element_blank())
  
  # Right Margin: Parent 1 GCA Bar Chart (Flipped)
  p_gca1 <- ggplot(gca_df, aes(x = Parent, y = GCA)) +
    geom_col(fill = "#428BCA", width = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_discrete(limits = rev(all_parents)) +
    coord_flip() +
    labs(x = NULL, y = "GCA (P1)") +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.minor = element_blank())
  
  # Combine using Patchwork layout
  final_plot <- (p_gca2 + plot_spacer() + plot_layout(widths = c(4, 1))) /
    (p_heatmap + p_gca1 + plot_layout(widths = c(4, 1))) +
    plot_layout(heights = c(1, 4))
  
  # Print the plot
  print(final_plot)
  
  # Return the extracted statistics as a list for further analysis
  return(list(
    Model = mod,
    GCA_Estimates = gca_df,
    SCA_Estimates = sca_df,
    Plot_Data = plot_data,
    Plot = final_plot
  ))
}

# 1. Load the required libraries
library(sommer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)

# (Make sure you have already run the complete_diallel_analysis function code here)

# 2. Load the Partial Diallel Data in Wide Format
# This matches the DATALINES from your SAS code
partial_diallel_wide <- read.table(text = "
P1 P2 R1 R2 R3 R4
1 4 128.10 123.84 92.56 115.28
1 5 128.36 119.84 103.24 129.72
1 6 74.40 70.86 60.94 68.00
2 5 91.52 113.96 87.26 106.98
2 6 59.06 65.62 81.62 86.76
2 7 84.16 109.74 102.14 94.52
3 6 109.86 98.16 93.26 102.26
3 7 117.20 100.28 116.16 112.52
3 8 109.68 116.48 123.92 120.86
4 7 53.40 60.86 74.46 69.08
4 8 53.86 48.30 40.64 44.62
5 8 86.62 94.18 90.32 108.16
", header = TRUE)

# 3. Transform to Long Format
# This replaces the SAS DATA step and array loops
partial_diallel_long <- partial_diallel_wide %>%
  pivot_longer(
    cols = starts_with("R"), 
    names_to = "Rep", 
    values_to = "Yield"
  ) %>%
  # Clean up the "Rep" column to just be the number (e.g., "R1" -> 1)
  mutate(Rep = str_remove(Rep, "R"))

# 4. Run the Analysis
cat("Starting Diallel Analysis...\n")
results <- complete_diallel_analysis(
  data = partial_diallel_long, 
  p1_var = "P1", 
  p2_var = "P2", 
  rep_var = "Rep", 
  resp_var = "Yield", 
  trait_name = "Grain Yield"
)

# 5. View the extracted BLUPs
cat("\n--- General Combining Ability (GCA) ---\n")
print(results$GCA_Estimates)

cat("\n--- Specific Combining Ability (SCA) ---\n")
print(results$SCA_Estimates)