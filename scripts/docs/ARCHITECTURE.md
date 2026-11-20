# DataIntellEngine Technical Architecture

This document details the five-phase design, data grounding mechanics, multi-agent prompt guardrails, and diagnostic tooling behind **DataIntellEngine** (originally developed as the **AIBioStatAgent** local RAG analytical pipeline).

---

## 1. Infrastructure & Environment Design

DataIntellEngine is engineered for headless, local-first execution on compact Ubuntu Linux compute nodes (e.g., Beelink MINI S13) managed remotely over SSH/SFTP, ensuring proprietary or clinical datasets remain strictly on-premise.

* **Inference Engine**: Ollama running as a `systemd` service (`llama3.2` for code generation and interpretation; `nomic-embed-text` for dense text embeddings).
* **Python Runtime**: Isolated virtual environment (`venv`) running LangChain (`langchain-chroma`, `langchain-ollama`), ChromaDB, `pandas`, `scikit-learn`, `matplotlib`, and `seaborn`.
* **R Runtime**: System-wide R installation combining pre-compiled Ubuntu binaries (`r-cran-tidyverse`, `r-cran-matrix`) for heavy C++ dependencies with CRAN installations (`lme4`, `survival`, `survminer`, `lattice`, `reshape2`, `forecast`).

---

## 2. Knowledge Base & Vector Indexing

### Document Schema (`scripts/generate_r_rag_docs.py`)
Each of the 20 Markdown files in `docs/` follows a uniform template so the Coder Agent receives complete, runnable context:
1. **Function Header (`# Function: ...`)**
2. **Description**: Statistical purpose and plot type.
3. **Arguments**: Expected data types and formula structure.
4. **R Code Block**: Self-contained wrapper function with library imports, statistical modeling, structured visualization, and printed summary return.

### Embedding & Persistence (`scripts/build_vector_db.py`)
* Iterates through `./docs/*.md`, wrapping each file in a `langchain_core.documents.Document` with `metadata={"source": filename}`.
* Computes embeddings via `OllamaEmbeddings(model="nomic-embed-text")` and persists the collection to `./r_chroma_bd`.

### Retrieval Diagnostics (`scripts/check_vector_db.py`)
Validates embedding quality before running full analyses:
* **PCA Semantic Map**: Projects stored document vectors onto 2 principal components (`sklearn.decomposition.PCA`) to verify that related statistical domains cluster together.
* **Query Similarity Bar Chart**: Runs `similarity_search_with_score(query, k=8)` and plots L2 distances (lower score = closer semantic match) to `./output/vector_db_analysis.png`.
* **Top Match Inspection**: Prints the top document's filename, L2 score, and full text payload sent to the LLM.

---

## 3. Synthetic Clinical Dataset & Schema Grounding

### Synthetic Data Specification (`scripts/simulate_data.py`)
Generates a reproducible (`np.random.seed(42)`) 200-patient dataset (`clinical_trial_data.csv`) engineered to exercise all statistical families:

| Column | Type | Distribution / Generation Logic | Target Analyses |
| :--- | :--- | :--- | :--- |
| `patient_id` | Integer | `1..200` | Identifier |
| `treatment` | Categorical | `Drug_A`, `Drug_B`, `Placebo` | T-test, ANOVA, Mixed Models, Survival |
| `gender` | Categorical | `M`, `F` | T-test, Chi-Square, Wilcoxon |
| `age` | Continuous | $\mathcal{N}(50, 10)$, rounded | Regression, Logistic, Mixed Models |
| `clinic_site` | Categorical | `Site_1`..`Site_4` (random effects: `+2, -2, +5, -5`) | Linear Mixed-Effects Models (`lme4`) |
| `biomarker_1` | Continuous | $\mathcal{N}(100, 15)$ | PCA, Correlation, Logistic |
| `biomarker_2` | Continuous | $0.5 \cdot \text{biomarker\_1} + \mathcal{N}(0, 10)$ | PCA, Correlation Matrix |
| `biomarker_3` | Continuous | $-0.3 \cdot \text{biomarker\_1} + \mathcal{N}(50, 5)$ | PCA, Correlation Matrix |
| `bp_reduction` | Continuous | $5 + \beta_{\text{trt}} + u_{\text{site}} + 0.1 \cdot \text{age} + \epsilon$ | Linear, Polynomial, ANOVA, LMM |
| `adverse_event` | Binary (`0/1`) | Logistic function of `age` and `biomarker_1` | Logistic Regression, Chi-Square |
| `survival_time` | Continuous | Exponential ($\lambda=0.1$ Placebo, $0.05$ Active) | Kaplan-Meier, Cox PH |
| `survival_status` | Binary (`0/1`) | Binomial ($p=0.7$ observed, $30\%$ censored) | Kaplan-Meier, Cox PH |

### Live Header Injection (`scripts/rag_orchestrator.py`)
To bridge abstract R documentation (`y ~ x`) with real CSV files, the orchestrator executes `pd.read_csv(dataset_path, nrows=0)` prior to code generation and injects the comma-separated header list directly into the Coder Agent prompt as `AVAILABLE DATASET COLUMNS`.

---

## 4. Multi-Agent Prompt Guardrails

### Coder Agent (`llama3.2`, `temperature=0.1`)
Controlled by 9 explicit rules in `rag_orchestrator.py`:
1. **Strict Function Reuse**: Must include and call the exact retrieved R function when applicable rather than inventing plotting utilities.
2. **Fallback Autonomy**: Permitted to write custom R code if retrieved context does not satisfy the statistical requirement (e.g., multivariable Cox regression).
3. **Console Capture**: Prints all statistical summaries to `STDOUT`.
4. **Plot Persistence**: Directs all plots (`ggsave()` or `pdf()/dev.off()`) into the active `session_dir`.
5. **Clean Code Extraction**: Returns strictly fenced ```` ```r ```` blocks parsed via regex (`r'```[rR]\n(.*?)\n```'`).
6–9. **Schema Alignment & Anti-Placeholder Rules**: Enforces exact column matching, autonomous typo correction, explicit Response ($Y$) vs. Predictor ($X$) identification, and prohibits literal `y` or `x` placeholders unless present in the CSV header.

### Interpreter Agent (`llama3.2`, `temperature=0.5`)
Receives the user prompt alongside the full `execution_log.txt`:
* **Zero-Hallucination Failure Mode**: When `subprocess.run` raises `CalledProcessError`, the orchestrator logs the exit code, `STDOUT`, and `STDERR`. The Interpreter Agent is strictly instructed never to invent p-values or parameter estimates on failure, and instead to diagnose the exact R error.
* **Clinical Synthesis**: On success (`Exit Code 0`), translates raw R output into a structured Markdown report (`analysis_report.md`) covering methodology, key statistical findings, and actionable interpretation.

---

## 5. Execution Lifecycle (`scripts/rag_orchestrator.py`)

```text
CLI Input (--prompt, --data)
  │
  ├──► Create ./output/session_YYYYMMDD_HHMMSS/
  ├──► Read CSV header (pandas, nrows=0)
  ├──► Query ChromaDB (top k=3 docs via nomic-embed-text)
  ├──► Pass 1: Coder LLM ──► Regex extract ──► analysis_script.R
  ├──► Execute: subprocess.run(["Rscript", script_path]) ──► execution_log.txt + plots
  └──► Pass 2: Interpreter LLM ──► analysis_report.md
```