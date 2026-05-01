# 🧬 Internal Documentation: `2_core_libraries`

## Overview
The `2_core_libraries` directory is the mathematical engine of the DataIntelEngine. This zone is strictly dedicated to biometric algorithms, quantitative genetics models, and multi-environment trial (MET) simulations. 

**The Prime Directive:** Code in this directory is completely agnostic to the AI agent. These scripts do not know that an LLM exists. They are designed to take standard inputs (CSV paths, data frames, numeric parameters) and produce standard outputs (CSV artifacts, plots, model summaries). This ensures that your statistical libraries can power human-facing UI dashboards (like Shiny or Streamlit) just as easily as they power the AI orchestrator.

---

## 📂 Subfolder Breakdown & Installation Procedures

### 1. `python_core/` (Python Biometrics & ML)
This folder houses native Python modules for data simulation, machine learning, and advanced genomic predictions.

* **Key Contents:**
  * `sim_multistage_field.py`: Generates simulated yield data with customizable GxE correlation structures for multi-stage field trials.
  * `tetraploidMLPred.py`: Predictive machine learning wrappers (Ridge, RandomForest, SVR, GBLUP) for tetraploid genomic data.
* **Installation & Dependency Management:**
  * Since this acts as an internal package, dependencies should be managed via your virtual environment. 
  * Ensure the active environment `(agent_env)` has the required data science packages installed:
    ```cmd
    pip install pandas numpy scikit-learn matplotlib seaborn scipy
    ```
  * *Note: If this folder eventually utilizes a `setup.py` file to act as an installable Python module, you would install it locally using `pip install -e .` from within the directory.*

### 2. `r_core/` (The `DataIntelR` Package)
This folder is structured as a formal, system-wide R package. Building it as a package rather than a collection of loose scripts ensures proper namespace management, dependency checking, and lightning-fast execution.

* **Key Contents:**
  * `DESCRIPTION` & `NAMESPACE`: The metadata defining the `DataIntelR` package.
  * `R/complete_diallel_analysis.R`: Fits mixed models (via the `sommer` package) for General and Specific Combining Ability (GCA/SCA) estimation and generates spatial layout heatmaps.
  * `mixed_models/`: Additional directories for specialized algorithms.
* **Installation Protocol (CRITICAL):**
  * Whenever a new `.R` script is added or modified in this folder, you **must** recompile the package so the rest of the system can see the changes.
  * Open your R Console or RStudio and run:
    ```R
    devtools::install("C:/Users/urros/DataIntelEngine/2_core_libraries/r_core")
    ```

### 3. `sas_core/` (SAS Procedures)
Reserved for legacy code or highly specialized SAS routines requiring `PROC MIXED` or `PROC GLIMMIX` for spatial trial optimizations that have not yet been ported to R or Python.

---

## 🔌 Connection to the AI Agent (The "Air Gap")

To protect the integrity of the statistical math, the AI Orchestrator (`1_agent_brain`) is never allowed to directly execute or modify files in `2_core_libraries`. Instead, the connection is handled entirely by `1_agent_brain/tool_registry.py` through two specific bridging methods:

### Method A: The Python Path Injection (For `python_core`)
The AI orchestrator cannot naturally import modules from sibling directories. The registry script forces a connection at runtime using `sys.path`.
1. `tool_registry.py` defines the absolute path to `python_core`.
2. It appends this path to the system: `sys.path.append(PYTHON_CORE_DIR)`.
3. It natively imports your functions (e.g., `from sim_multistage_field import sim_multistage_field`).
4. When the AI calls a tool, the registry executes the imported function and catches any data or exceptions before returning a text summary to the AI.

### Method B: Headless Execution (For `r_core`)
Because R is a completely different runtime environment, the Python registry cannot import R functions directly. It utilizes headless execution via the command line.
1. `tool_registry.py` constructs a raw R script as a multi-line string.
2. The first line of that string is always `suppressPackageStartupMessages(library(DataIntelR))` to load the compiled package quietly.
3. The string passes the AI's requested parameters into your R functions.
4. Python's `subprocess.run` executes the string using `Rscript -e`.
5. Any visual artifacts (like Diallel heatmaps or Alpha design spatial maps) are forcefully saved to `4_workspace/agent_outputs/` using `ggsave()`.
6. The terminal output (stdout) is captured and handed back to the AI.

---

## ⚙️ Developer SOP: Adding a New Biometric Model

When developing a new mathematical tool (e.g., a spatial BLUP calculator or a network optimizer), follow this strict workflow to securely wire it into the engine:

**Step 1: Write the Core Math**
* Write the function in `python_core` or `r_core`. 
* **Rule:** Do not include any input prompts (e.g., `input()`) or GUI pop-ups that require human clicking. The function must accept arguments directly and return/save its output cleanly.

**Step 2: Compile (If R)**
* If writing in R, run `devtools::install()` to update the local `DataIntelR` package.

**Step 3: Build the Bridge (`tool_registry.py`)**
* Navigate to `1_agent_brain/tool_registry.py`.
* Write a wrapper function (e.g., `run_spatial_blup(...)`) that calls your new core library function.
* Ensure the wrapper directs all output files to `WORKSPACE_OUT`.
* Add the wrapper to the `AVAILABLE_TOOLS` dictionary at the bottom of the file.

**Step 4: Update the AI's Brain (`tools_schema.json`)**
* Open `1_agent_brain/tools_schema.json`.
* Add a new JSON block defining the exact parameters your core library function requires. 
* Ensure the schema's `"name"` matches the key you added to the registry dictionary perfectly.