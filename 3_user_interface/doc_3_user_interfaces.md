# 🖥️ Internal Documentation: `3_user_interfaces`

## Overview
The `3_user_interfaces` directory is the **Human-in-the-Loop Zone**. While the `1_agent_brain` serves the AI, this folder serves the human researchers, breeders, and stakeholders. 

This directory contains interactive web applications (dashboards) that allow users to explore multi-environment trial (MET) data, visualize spatial field maps, and run predictive models without ever touching the command line or writing a line of code.

**The Prime Directive:** Code in this directory must **never** contain heavy biometric math or complex data-cleaning logic. These apps act strictly as a "presentation layer." All statistical modeling must be imported from the `2_core_libraries` directory. This guarantees that whether a human clicks a button in a dashboard or the AI agent triggers a tool, the exact same underlying math is executed.

---

## 📂 Subfolder Breakdown & Tech Stack

### 1. `shiny/` (R-Based Dashboards)
This folder houses all user interfaces built using the R Shiny framework. It is ideal for highly specialized statistical visualizations and interacting with your custom R packages.

* **Key Applications:**
  * **Breeding Network Spatial Optimizer:** A UI for breeders to visualize incomplete block designs and Alpha designs.
  * **GGE Biplot Explorer:** An interactive tool to perform SVD on environment-centered data, allowing users to toggle between Genotype-focused (f=1) and Environment-focused (f=0) scaling dynamically.
* **Architecture Rules:**
  * Apps in this folder must load your core math via `library(DataIntelR)`. 
  * Do not write loose helper functions inside `app.R` or `server.R`. If a function calculates a value, it belongs in `2_core_libraries/r_core`.

### 2. `streamlit/` (Python-Based Dashboards)
This folder houses user interfaces built with Streamlit. It is the preferred framework for wrapping your machine learning pipelines and complex data simulations.

* **Key Applications:**
  * **Tetraploid ML Predictor UI:** A dashboard where users can upload genotype/phenotype CSVs, select an environment, choose a model type (e.g., Ridge, SVR, RandomForest), and instantly view the Perfect Prediction scatter plots.
  * **Multi-Stage Field Simulator:** A tool to configure `batch_years` and variance components (`G`, `GE`, `e`) via sliders to generate simulated yield data.
* **Architecture Rules:**
  * Streamlit scripts must use `sys.path.append` (similar to the AI registry) to cleanly import functions from `2_core_libraries/python_core`.

---

## 🔌 Connection to the Core Libraries (The "Thin UI" Principle)

To keep your application scalable and bug-free, we enforce a **Thin UI** architecture. This means the UI scripts should be as short as possible. 

Here is an example of how a Streamlit app in this folder should interact with your Python core:

```python
# Inside 3_user_interfaces/streamlit/ml_dashboard.py
import streamlit as st
import sys
import os
import pandas as pd

# 1. Safely route to the core libraries
BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
sys.path.append(os.path.join(BASE_DIR, '2_core_libraries', 'python_core'))

# 2. Import the heavy math
from tetraploidMLPred import tetraploidMLPred

# 3. Build the UI
st.title("Tetraploid Genomic Prediction")
uploaded_geno = st.file_uploader("Upload Genotype Data")
uploaded_pheno = st.file_uploader("Upload Phenotype Data")
model_choice = st.selectbox("Select Model", ["Ridge", "RandomForest", "GBLUP"])

# 4. Execute the imported function (NO MATH WRITTEN HERE)
if st.button("Run Prediction") and uploaded_geno and uploaded_pheno:
    df_geno = pd.read_csv(uploaded_geno, index_col=0)
    df_pheno = pd.read_csv(uploaded_pheno)
    
    with st.spinner('Training model...'):
        # We just pass the UI inputs into our core function!
        model, r, rmse = tetraploidMLPred(df_geno, df_pheno, model_type=model_choice)
        st.success(f"Model trained! RMSE: {rmse:.3f} | Pearson r: {r:.3f}")
```

---

## ⚙️ Developer SOP: Deploying a New Dashboard

When a Data Scientist or Engineer wants to build a new visual tool for the team, follow these steps:

**Step 1: Verify the Math Exists**
* Ensure the function you want to visualize already exists in `2_core_libraries` and works perfectly in the terminal or a Jupyter notebook.

**Step 2: Choose the Framework**
* If the underlying math is in Python (`sim_multistage_field.py`), use **Streamlit**.
* If the underlying math is in R (`complete_diallel_analysis.R`), use **Shiny**.

**Step 3: Build the UI Shell**
* Create a new subfolder for your app (e.g., `3_user_interfaces/shiny/diallel_explorer/`).
* Write the UI logic (buttons, sliders, file uploaders).
* Route the UI inputs directly into your imported core library functions.

**Step 4: Test Data Routing**
* Ensure your UI handles data paths correctly. If the app needs to save a temporary artifact, it should save it to `4_workspace/raw_data/` or a dedicated `temp/` folder, **never** directly inside the `3_user_interfaces` directory.

***

*(If you actually meant a dedicated "inference" folder for production ML model endpoints like FastAPI or Flask, let me know and I will write up an architecture and documentation specifically for that!)*