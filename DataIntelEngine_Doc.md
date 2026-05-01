# 🌾 DataIntelEngine: Architecture & Documentation

## Overview
**DataIntelEngine** is a hybrid biometric analytics platform. It combines a state-of-the-art local AI orchestration layer (powered by Ollama) with enterprise-grade statistical computing libraries in Python, R, and SAS. 

The architecture is designed as a **Monorepo**. It strictly decouples the AI's "brain" from the core mathematical "muscles," ensuring that biometric functions can be executed autonomously by the AI or manually by human researchers via UI dashboards.

---

## 📂 High-Level Directory Structure

```text
DataIntelEngine/
│
├── 1_agent_brain/              # The AI Orchestrator & Configuration Zone
├── 2_core_libraries/           # The Biometric Math & Statistics Zone
├── 3_user_interfaces/          # Human-facing Dashboards (Shiny/Streamlit)
├── 4_workspace/                # Dynamic Data Memory (Inputs & Outputs)
├── 5_research/                 # Sandboxed Jupyter/RMarkdown Notebooks
│
├── setup.py                    # Package installer for Python components
└── README.md                   # This documentation file
```

---

## 📁 1_agent_brain/ (The Orchestrator Zone)
This directory is the control center for the local AI agent. It contains the logic required to parse natural language, select tools, and execute scripts. **No biometric mathematics should live in this folder.**

* **`main_orchestrator.py`**
    * **Purpose:** The core "Brain" of the engine. 
    * **Details:** It contains the `while True` loop, loads the system prompt (global rules), and manages the conversation history with the Ollama API. It features an interactive terminal (REPL) for the Lead Data Scientist to submit prompts.
* **`tools_schema.json`**
    * **Purpose:** The AI's "Instruction Manual."
    * **Details:** A JSON file containing the exact definitions, descriptions, and required parameters for every tool the AI is allowed to use. When the AI needs to solve a problem, it reads this file to know what tools exist.
* **`tool_registry.py`**
    * **Purpose:** The Bridge between the AI and the Math.
    * **Details:** This file acts as a switchboard. It maps the string names from `tools_schema.json` to actual Python functions or headless `Rscript -e` execution commands. It uses `sys.path` to securely call functions from `2_core_libraries` without mixing directories.
* **`agent_tools/` (Directory)**
    * **Purpose:** The AI's Utility Belt.
    * **Details:** Contains helper scripts specifically for the AI to interact with the system. Examples include SQL database fetchers, PDF report generators, or data cleaning scripts to handle `NULL` values before passing data to the math libraries.

---

## 📁 2_core_libraries/ (The Statistics Zone)
This directory contains the pure scientific code. Scripts here are completely agnostic to the AI—they do not know the AI exists. They take strict inputs (like CSV paths) and return strict outputs (like DataFrames or saved plots).

* **`python_core/`**
    * **Purpose:** Native Python biometric functions.
    * **Key Files:**
        * `sim_multistage_field.py`: Generates simulated yield data with customizable GxE correlation structures.
        * `tetraploidMLPred.py`: Machine learning wrappers (Ridge, SVR, RandomForest) for tetraploid potato genomic prediction.
* **`r_core/`**
    * **Purpose:** Native R statistical functions structured as an installable R package (`DataIntelR`).
    * **Key Files:**
        * `complete_diallel_analysis.R`: Fits mixed models (via `sommer`) for GCA/SCA estimation and generates layout heatmaps.
* **`sas_core/`**
    * **Purpose:** Legacy or highly-specialized SAS procedures.

---

## 📁 3_user_interfaces/ (The Human Zone)
This folder is for human-in-the-loop applications.
* **`shiny/`**: Contains apps like the Breeding Network Spatial Optimizer or the interactive GGE Biplot explorer. These apps import math directly from `2_core_libraries`.
* **`streamlit/`**: Contains Python-based interactive dashboards.

---

## 📁 4_workspace/ (The Memory Zone)
This folder acts as the living memory for both the human user and the AI agent.
* **`raw_data/`**: Where the user or SQL connectors drop raw trial inputs (CSVs, Genotype matrices).
* **`agent_outputs/`**: The strictly defined output directory where the `tool_registry.py` saves all generated artifacts, such as `gge_biplot.png`, `diallel_plot.png`, or simulated dataset CSVs.

---

## 📁 5_research/ (The Sandbox)
Isolated environment for drafting, testing, and debugging.
* Contains Jupyter Notebooks, R Markdown files, and SAS notebooks. 
* **Rule:** The AI agent does not have access to this folder to prevent experimental code from crashing production pipelines.

---

## 🛠️ Developer Guide: How to Add a New Tool
When adding a new biometric capability to the engine, follow this strict 3-step pipeline:

1.  **Write the Core Math (`2_core_libraries`)**
    * Write your Python function or R script. Ensure it accepts arguments via command line or standard function parameters. (If R, run `devtools::install()` to update the local package).
2.  **Build the Wrapper (`1_agent_brain/tool_registry.py`)**
    * Create a wrapper function that maps the file inputs, executes the script, and formats the output into a simple string for the AI to read. Add it to the `AVAILABLE_TOOLS` dictionary.
3.  **Update the Instruction Manual (`1_agent_brain/tools_schema.json`)**
    * Add a new JSON block defining the tool's name, description, and required parameters so the LLM knows it exists and how to format the data.

## 🧠 Architectural Rules: Where Do "Rules" Belong?
To prevent the LLM from becoming overwhelmed, system rules are placed strategically based on flexibility:

* **Rule Type A: Hardcoded Standards (Python/R Code)**
    * *Example:* "Heatmaps must always use the 'YlGnBu' colormap."
    * *Where it goes:* Hardcode this directly into the `python_core` visualization scripts or the `tool_registry.py` R-wrappers. Do not force the AI to think about it.
* **Rule Type B: Flexible Instructions (JSON Schema)**
    * *Example:* "Default to an additive genetic model, unless the user specifically asks for dominance."
    * *Where it goes:* In the `tools_schema.json` under the parameter description.
* **Rule Type C: Global AI Personality (System Prompt)**
    * *Example:* "Never perform calculations yourself; rely entirely on the tools."
    * *Where it goes:* In the `messages` array inside `main_orchestrator.py`.