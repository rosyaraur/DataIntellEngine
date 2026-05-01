# 🔬 Internal Documentation: `5_research`

## Overview
The `5_research` directory is the **Sandbox** of the DataIntelEngine. 

Before any code becomes a formal biometric tool for the AI Orchestrator or a polished dashboard for the UI, it starts here. This folder is designed for exploratory data analysis (EDA), prototyping mathematical models, drafting whitepapers, and testing new hypotheses. 

**The Prime Directive:** This directory is strictly **Air-Gapped from Production**. The AI Agent (`1_agent_brain`) does not have access to this folder, and no UI applications (`3_user_interfaces`) should ever import code from here. This guarantees that you can write messy, experimental, or broken code without ever risking a crash in your main production pipelines.

---

## 📂 Subfolder Breakdown

### 1. `PythonNoteBooks/`
This folder is for Jupyter Notebooks (`.ipynb`) used to prototype Python-based pipelines.
* **Common Uses:** * Testing new scikit-learn or PyTorch architectures before wrapping them into `tetraploidMLPred.py`.
  * Visualizing `matplotlib` or `seaborn` outputs interactively to tune colormaps and layout parameters.
  * Debugging complex Pandas data-wrangling steps.

### 2. `R_Markdown/`
This folder contains `.Rmd` and Quarto documents used for statistical exploration and reproducible research reports.
* **Common Uses:**
  * Testing new mixed-model equations using the `sommer` or `lme4` packages.
  * Compiling final, human-readable PDF or HTML reports of trial results.
  * Testing functions from your `DataIntelR` package to ensure they work properly before the AI agent uses them.

### 3. `SASnotebooks/`
This folder holds legacy SAS scripts, exploratory SAS Studio code, and `.sas` files used for complex trial designs (like PROC GLIMMIX or PROC MIXED) that are currently being benchmarked against your new R/Python tools.

---

## 🔄 The Prototype-to-Production Lifecycle

When you have a new idea for a biometric tool, follow this lifecycle to graduate it from the Sandbox to the AI Engine:

1. **The Draft (`5_research`):** * Open a new Jupyter Notebook or R Markdown file.
   * Hardcode your file paths, load your data, and write the math until it works perfectly.
2. **The Refactor (`2_core_libraries`):** * Strip out all the notebook artifacts, `print()` statements, and hardcoded paths.
   * Wrap the pure math into a clean function and move it to `python_core` or `r_core`.
3. **The Connection (`1_agent_brain`):** * Write the Python wrapper in `tool_registry.py` and document it in `tools_schema.json` so the AI can use it.

---

## ⚙️ Standard Operating Procedures (SOPs) for the Team

### 1. Data Access (The "No Duplication" Rule)
Do not store large datasets (`.csv`, `.vcf`, `.RData`) directly inside this `5_research` folder. 
* If your notebook needs data, it should read it dynamically from the `4_workspace/raw_data/` folder using relative paths.
* **Example (Python):** `pd.read_csv('../4_workspace/raw_data/trial_2026.csv')`
* This keeps the repository lightweight and ensures all team members are analyzing the exact same source-of-truth datasets.

### 2. Version Control (Clearing Outputs)
Jupyter Notebooks store their outputs (including massive inline images and HTML tables) directly in the `.ipynb` file text. If you commit a notebook with a 10MB scatter plot saved inside it, it will bloat the Git repository.
* **CRITICAL:** Before committing code to GitHub/GitLab, always select **"Restart Kernel and Clear All Outputs"** in your Jupyter or R environment. Only commit the raw code, not the generated outputs.

### 3. Naming Conventions
Because this is a sandbox, it can quickly turn into a graveyard of files named `test_1.ipynb` or `final_model_v3_really_final.Rmd`. 
* Use descriptive naming conventions that include the date or specific project goal.
* **Example:** `2026-05-01_GBLUP_Hyperparameter_Tuning.ipynb`
* **Example:** `Spatial_Trend_Analysis_Draft.Rmd`