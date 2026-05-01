

# ⚙️ Data Intelligence Engine (DataIntellEngine)

> **A unified computational engine providing standardized datasets, statistical macros, and database management tools for robust data science operations.**

The **DataIntellEngine** is a comprehensive, cross-language repository built to standardize and accelerate advanced quantitative analysis. This engine serves as a centralized intelligence hub, housing curated datasets, statistical macros, and modular codebases written in Python, R, and SAS. 

Designed with dual-accessibility in mind, it provides robust, reproducible methodologies for human data scientists while exposing clean, tool-callable functions for Agentic AI. Whether powering complex mixed-model analyses, database schema integrations, or automated data cleaning pipelines, the DataIntellEngine ensures consistent, high-fidelity analytical outputs.

---

## 🌟 Key Features
* **Hybrid Architecture:** A true Monorepo supporting native Python, R, and SAS execution under a single orchestrated pipeline.
* **Agentic AI Orchestrator:** Features a localized LLM loop (via Ollama) capable of dynamically selecting and executing biometric tools based on natural language prompts.
* **Strict Decoupling:** Employs an "Air-Gapped" design where the AI brain is completely separated from the core mathematical libraries to ensure scientific integrity.
* **Human-in-the-Loop UI:** Includes R Shiny and Python Streamlit dashboards for interactive data exploration without requiring command-line access.

---

## 🗂️ Architecture and Directory Structure

The repository is modularized by language and function, ensuring that analytical logic remains isolated but universally accessible.

```text
DataIntelEngine/
│
├── 1_agent_brain/              # The AI Orchestrator & Configuration Zone
│   ├── main_orchestrator.py    # The core LLM execution loop 
│   ├── tools_schema.json       # The AI's instruction manual
│   ├── tool_registry.py        # The Python-to-Math execution bridge
│   └── agent_tools/            # Database connectors and AI utilities
│
├── 2_core_libraries/           # The Biometric Math & Statistics Zone
│   ├── python_core/            # Native Python ML and simulation functions
│   ├── sas_core/               # Legacy/Specialized SAS scripts
│   └── r_core/                 # The installable `DataIntelR` package
│
├── 3_user_interfaces/          # Human-facing Dashboards
│   ├── shiny/                  # R-based UIs (e.g., GGE Biplot Explorer)
│   └── streamlit/              # Python-based UIs
│
├── 4_workspace/                # Dynamic Data Memory (Git-ignored contents)
│   ├── raw_data/               # Intake zone for CSVs and database dumps
│   └── agent_outputs/          # Designated outbox for AI-generated plots/data
│
├── 5_research/                 # The Sandbox (Air-gapped from production)
│   ├── PythonNoteBooks/        # Jupyter Notebooks
│   └── R_Markdown/             # Exploratory Rmd files
│
├── setup.py                    # Package installer for Python components
└── README.md                   # This documentation
```

---

## 🚀 Getting Started

### Prerequisites
To run the full DataIntellEngine, you will need the following installed on your machine:
* **Python 3.9+**
* **R 4.2+** (with the `sommer`, `ggplot2`, and `devtools` packages)
* **Ollama** (Running locally with the `llama3.2:3b` model installed)

### Installation

**1. Clone the repository:**
```bash
git clone [https://github.com/your-organization/DataIntelEngine.git](https://github.com/your-organization/DataIntelEngine.git)
cd DataIntelEngine
```

**2. Set up the Python Environment:**
Create and activate your virtual environment, then install the core dependencies.
```bash
python -m venv agent_env
# Windows: agent_env\Scripts\activate
# Mac/Linux: source agent_env/bin/activate
pip install -r requirements.txt
```

**3. Install the Core R Package:**
The `r_core` directory functions as an installable R package. Open your R console and run:
```R
devtools::install("path/to/DataIntelEngine/2_core_libraries/r_core")
```

---

## 🧠 Running the AI Orchestrator

The engine features an interactive terminal (REPL) for the Lead Data Scientist to submit prompts directly to the local AI.

1. Ensure Ollama is running in the background.
2. Navigate to the `1_agent_brain` directory.
3. Launch the orchestrator:
```bash
cd 1_agent_brain
python main_orchestrator.py
```

---

## 🛠️ Developer Guidelines

To maintain the integrity of the monorepo, please adhere to the following rules when contributing:
1. **The Core Library Rule:** Never place data-cleaning logic or statistics/biometric math inside the UI (`3_user_interfaces`) or the AI Brain (`1_agent_brain`). All statistical functions belong strictly in `2_core_libraries`.
2. **The Output Rule:** All executable scripts and AI tools must save their generated artifacts (plots, CSVs) exclusively to `4_workspace/agent_outputs`.
3. **Workspace Discipline:** Never commit large datasets from `4_workspace` or output plots to version control. Ensure your `.gitignore` is properly configured.


