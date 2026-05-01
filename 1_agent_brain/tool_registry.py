import os
import sys
import json
import subprocess
import pandas as pd

# ==========================================
# FOLDER ROUTING (The Monorepo Backbone)
# ==========================================
# Get the absolute path to the root DataIntelEngine folder
BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))

# Define paths to other zones
PYTHON_CORE_DIR = os.path.join(BASE_DIR, '2_core_libraries', 'python_core')
WORKSPACE_OUT = os.path.join(BASE_DIR, '4_workspace', 'agent_outputs')

# Ensure the output directory exists so our plots don't fail
os.makedirs(WORKSPACE_OUT, exist_ok=True)

# Tell Python to look in the python_core folder for imports
sys.path.append(PYTHON_CORE_DIR)

# Import your native Python biometric functions
try:
    from sim_multistage_field import sim_multistage_field
    from tetraploidMLPred import tetraploidMLPred
except ImportError as e:
    print(f"[!] Warning: Could not import Python core libraries. Check folder structure. Error: {e}")

# ==========================================
# TOOL 1: R - Complete Diallel Analysis
# ==========================================
def run_diallel_analysis(dataset_path: str, p1_var: str, p2_var: str, rep_var: str, resp_var: str, trait_name: str = "Trait") -> str:
    out_plot = os.path.join(WORKSPACE_OUT, "diallel_plot.png").replace('\\', '/')
    
    # We call the R package and force it to save the plot to the workspace
    r_command = f"""
    suppressPackageStartupMessages(library(DataIntelR))
    
    df <- read.csv('{dataset_path}')
    results <- complete_diallel_analysis(
        data = df, 
        p1_var = '{p1_var}', 
        p2_var = '{p2_var}', 
        rep_var = '{rep_var}', 
        resp_var = '{resp_var}', 
        trait_name = '{trait_name}'
    )
    
    ggplot2::ggsave('{out_plot}', results$Plot, width=10, height=8)
    
    cat("Diallel Analysis Complete.\\n")
    cat("Plot saved to: {out_plot}\\n")
    """
    
    try:
        process = subprocess.run(['Rscript', '-e', r_command], capture_output=True, text=True, check=True)
        return process.stdout 
    except subprocess.CalledProcessError as e:
        return f"R Execution Error:\n{e.stderr}"

# ==========================================
# TOOL 2: Python - Multi-Stage Field Sim
# ==========================================
def run_sim_multistage_field(batch_years: str, n_crosses: int = 100, n_lines: int = 5000, n_env: int = 10, n_rep: int = 3) -> str:
    try:
        # Decode the JSON string back into a Python dictionary
        batch_dict = json.loads(batch_years)
        
        # Run the Python simulation natively
        df_out = sim_multistage_field(
            batch_years=batch_dict,
            n_crosses=n_crosses,
            n_lines=n_lines,
            n_env=n_env,
            n_rep=n_rep
        )
        
        out_file = os.path.join(WORKSPACE_OUT, "simulated_field_data.csv")
        df_out.to_csv(out_file, index=False)
        
        return f"Simulation complete. Generated {len(df_out)} trial records. Data saved to: {out_file}"
    except Exception as e:
        return f"Python Simulation Error: {str(e)}"

# ==========================================
# TOOL 3: Python - Tetraploid ML Prediction
# ==========================================
def run_tetraploid_ml_pred(geno_path: str, pheno_path: str, env: str, pheno_col: str, model_type: str, genetic_model: str = 'additive') -> str:
    try:
        # Load DataFrames (assuming genotype index is the first column)
        dfGeno = pd.read_csv(geno_path, index_col=0) 
        dfPheno = pd.read_csv(pheno_path)
        
        # Trigger the ML script (This will pop up the matplotlib window on your PC)
        model, r, rmse = tetraploidMLPred(
            dfGeno=dfGeno, 
            dfPheno=dfPheno, 
            env=env, 
            pheno_col=pheno_col, 
            model_type=model_type, 
            genetic_model=genetic_model
        )
        
        return f"ML Model ({model_type}) trained successfully for '{pheno_col}'. Pearson Correlation (r): {r:.3f}, RMSE: {rmse:.3f}"
    except Exception as e:
        return f"Python ML Prediction Error: {str(e)}"

# ==========================================
# TOOL 4: R - GGE Biplot (Headless Math Wrapper)
# ==========================================
def run_generate_gge_biplot(dataset_path: str, gen_col: str, scaling: float) -> str:
    out_plot = os.path.join(WORKSPACE_OUT, "gge_biplot.png").replace('\\', '/')
    
    # We extract the pure SVD math from your Shiny App and run it headlessly
    r_command = f"""
    suppressPackageStartupMessages(library(dplyr))
    suppressPackageStartupMessages(library(ggplot2))
    suppressPackageStartupMessages(library(ggrepel))
    
    df <- read.csv('{dataset_path}')
    genotypes <- df[['{gen_col}']]
    
    # Filter to only numeric columns for SVD
    Y_raw <- as.matrix(df[, sapply(df, is.numeric)])
    env_names <- colnames(Y_raw)
    
    # Environment Centering (G+GE)
    Y_centered <- scale(Y_raw, center = TRUE, scale = FALSE)
    svd_res <- svd(Y_centered)
    
    U <- svd_res$u[, 1:2]
    V <- svd_res$v[, 1:2]
    L <- diag(svd_res$d[1:2])
    f <- {scaling}
    
    G_coords <- U %*% (L^f)
    E_coords <- V %*% (L^(1-f))
    
    gen_df <- data.frame(ID = genotypes, PC1 = G_coords[,1], PC2 = G_coords[,2])
    env_df <- data.frame(ID = env_names, PC1 = E_coords[,1], PC2 = E_coords[,2])
    
    # Generate the Plot
    p <- ggplot() +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
        geom_segment(data = env_df, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.2, "cm")), color = "blue", alpha = 0.6) +
        geom_text_repel(data = env_df, aes(x = PC1, y = PC2, label = ID), color = "blue", fontface = "bold") +
        geom_point(data = gen_df, aes(x = PC1, y = PC2), color = "red", size = 3) +
        geom_text_repel(data = gen_df, aes(x = PC1, y = PC2, label = ID), color = "red") +
        theme_minimal() +
        labs(title = paste("GGE Biplot - Scaling f =", f), x = "PC1", y = "PC2") +
        coord_fixed()
        
    ggsave('{out_plot}', p, width=10, height=8)
    
    d <- svd_res$d
    prop <- (d^2) / sum(d^2) * 100
    
    cat("GGE Biplot Generated Successfully.\\n")
    cat("Plot saved to: {out_plot}\\n")
    cat("Percentage Explained by PC1 & PC2: ", round(prop[1]+prop[2], 2), "%\\n", sep="")
    """
    
    try:
        process = subprocess.run(['Rscript', '-e', r_command], capture_output=True, text=True, check=True)
        return process.stdout 
    except subprocess.CalledProcessError as e:
        return f"R Execution Error:\n{e.stderr}"

# ==========================================
# 3. THE MASTER DICTIONARY
# ==========================================
# WARNING: These exact string keys MUST match the "name" fields in tools_schema.json
AVAILABLE_TOOLS = {
    'complete_diallel_analysis': run_diallel_analysis,
    'sim_multistage_field': run_sim_multistage_field,
    'tetraploid_ml_pred': run_tetraploid_ml_pred,
    'generate_gge_biplot': run_generate_gge_biplot
}