## AIBioStatAgent: RAG Analytical Pipeline Development Outline

1. **Phase 1: Infrastructure & Environment Provisioning**

* **Hardware & OS Configuration:** Establish the Beelink MINI S13 server running Ubuntu, ensuring stable network routing (Ethernet/Netplan) and remote management via SSH and SFTP from macOS.


* **Local AI Runtime:** Install and serve Ollama locally to manage open-source models without API dependencies. Pull `llama3.2` for reasoning/coding and `nomic-embed-text` for vector embeddings.


* **Environment Isolation:** Construct a dedicated Python virtual environment (`venv`) at `~/analyticalRAG` populated with `langchain`, `chromadb`, `pandas`, and `scikit-learn`.


* **R Engine Setup:** Install the global R environment and essential biostatistical packages via pre-compiled binaries (`apt install r-cran-tidyverse`, `r-cran-matrix`) and CRAN (`lme4`, `survival`).



2. **Phase 2: Knowledge Base & Vector Database Construction**

* **Document Engineering:** Author 20 enhanced R function documentations as Markdown files (e.g., `enhanced_mixed_model.md`, `enhanced_survival_km.md`), containing theoretical context, parameter definitions, and structured plotting functions (e.g., `dotplot` via `lattice`).


* **Embedding Pipeline (`build_vector_db.py`):** Ingest the Markdown files, process them through `nomic-embed-text`, and persist the high-dimensional vectors locally in ChromaDB (`./r_chroma_bd`).


* **Vector Visualization (`check_vector_db.py`):** Develop a diagnostic tool using PCA clustering to map the semantic relationships of the R functions and validate prompt similarity distances via horizontal bar charts.



3. **Phase 3: Data Simulation & Grounding**


* **Clinical Trial Simulation (`simulate_data.py`):** Generate robust, complex synthetic datasets (e.g., 200 patients) containing continuous variables, categorical groups, random clinic effects, binary outcomes, and time-to-event survival data to adequately test all statistical nodes.


* **Data Injection Strategy:** Integrate `pandas` to dynamically read the CSV headers (the `AVAILABLE DATASET COLUMNS`) and inject them directly into the LLM context, bridging the gap between abstract code and real data.


4. **Phase 4: Multi-Agent Prompt Engineering & Guardrails**


* **Coder Agent Guardrails:** Implement strict LLM constraints to prevent hallucination. Key rules enforce identifying Response (Y) and Predictors (X) from the prompt, mapping them to exact dataset columns (with typo correction), and explicitly utilizing provided R documentation functions rather than fabricating generic alternatives.

* **Fallback Autonomy:** Engineer the prompt to allow the Coder LLM to write original R code (e.g., Cox Proportional Hazards) if the retrieved context is statistically inadequate for the user's request.

* **Interpreter Agent Guardrails:** Enforce strict instructions to analyze raw terminal output (`STDOUT`/`STDERR`). If an error is caught (e.g., missing package), the Interpreter must explain the failure instead of fabricating fake statistical summaries.


5. **Phase 5: Execution Orchestration (`rag_orchestrator.py`)**

* **Context Retrieval:** Intercept the user's natural language biostatistics prompt and execute a ChromaDB similarity search to fetch the top 3 most relevant Markdown documents.

* **Script Generation:** Pass the retrieved context, prompt, and exact dataset columns to the `llama3.2` Coder LLM to generate a valid, runnable R script.

* **Subprocess Execution:** Execute the generated R script locally via Python's `subprocess`, capturing all terminal outputs, warnings, and error codes.

* **Session Management:** Automatically generate timestamped session directories (`./output/session_YYYYMMDD_HHMMSS`) to archive the generated `.R` script, the raw `execution_log.txt`, and any visual plots (`.png`/`.pdf`).

* **Biostatistical Reporting:** Pass the execution logs to the `llama3.2` Interpreter LLM to generate a comprehensive, human-readable Markdown report detailing the methodology, results, interpretation, and actionable clinical insights.


### Phase 1: Infrastructure & Environment Provisioning (Expanded)

This phase establishes the foundational hardware, networking, and software stacks required to run local LLMs and biostatistical models seamlessly on your Beelink server, managed remotely from your M1 MacBook Air.

#### 1.1 Hardware & OS Configuration

The host machine is a Beelink MINI S13 running Ubuntu Server. Initial Wi-Fi routing issues were resolved by binding the connection to a wired Ethernet interface (`enp2s0`) using Netplan.

To manage the server headless from your Mac and transfer files via SFTP:

```bash
# Retrieve the Beelink's local IP address for SSH/FileZilla access
hostname -I

```

*Note: In FileZilla on your Mac, use `sftp://[IP_ADDRESS]` with your `rosyaraur` credentials on Port 22 to establish the drag-and-drop workspace connection.*

#### 1.2 Local AI Runtime Setup (Ollama)

Ollama serves as the local inference engine, ensuring your proprietary clinical trial data never leaves the local network. It runs as a background service on Ubuntu.

```bash
# Start the Ollama service (if not already running on boot)
sudo systemctl start ollama

# Pull the primary reasoning and coding model
ollama pull llama3.2

# Pull the embedding model for ChromaDB vectorization
ollama pull nomic-embed-text

```

#### 1.3 Python Environment Isolation & Orchestration Stack

To prevent system-level package conflicts, the LangChain and RAG components are isolated within a Python virtual environment inside the project root directory.

```bash
# Navigate to the project root
cd ~/analyticalRAG

# Create and activate the virtual environment
python3 -m venv venv
source venv/bin/activate

# Install all required data science, visualization, and RAG orchestration libraries
pip install langchain-chroma langchain-ollama chromadb pandas scikit-learn matplotlib seaborn

```

*Verification: The terminal prompt must display `(venv) rosyaraur@beelink-compute:~/analyticalRAG$` before executing any python scripts.*

#### 1.4 Global R Environment & Biostatistical Packages

Because compiling R packages like `tidyverse` and `Matrix` from source (CRAN) on an Ubuntu server often fails due to missing C++ libraries, these heavy dependencies are installed globally as pre-compiled Ubuntu binaries via `apt`. Pure R packages are installed via CRAN.

```bash
# 1. Install complex system-level R packages using Ubuntu's package manager
sudo apt update 
sudo apt install r-cran-tidyverse r-cran-matrix -y

# 2. Install specialized biostatistics and plotting packages via CRAN
sudo Rscript -e "install.packages(c('lme4', 'survival', 'survminer', 'lattice'), repos='http://cran.rstudio.com/')"

```

#### 1.5 Project Directory Architecture

With the environment configured, the file structure is scaffolded to support the multi-agent pipeline:

```bash
# Create the necessary subdirectories
mkdir -p ~/analyticalRAG/docs ~/analyticalRAG/scripts ~/analyticalRAG/output ~/analyticalRAG/r_chroma_bd

```

* **`docs/`**: Holds the 20 authored R function markdown files (e.g., `enhanced_mixed_model.md`).

* **`scripts/`**: Houses `rag_orchestrator.py`, `build_vector_db.py`, `check_vector_db.py`, and `simulate_data.py`.

* **`output/`**: The destination for timestamped session folders containing execution logs, R scripts, reports, and plots.
* **`r_chroma_bd/`**: The persistent local vector database generated by LangChain.


### Phase 2: Knowledge Base & Vector Database Construction (Expanded)

This phase transforms your curated biostatistical knowledge into a machine-readable vector database. By embedding the R documentation, the LangChain orchestrator can perform semantic similarity searches to retrieve the exact statistical function needed for the user's prompt.

#### 2.1 Document Engineering (The Knowledge Base)

The foundation of the RAG system is the `docs/` directory, which contains 20 manually authored, enhanced Markdown files. Each file (e.g., `enhanced_mixed_model.md`, `enhanced_survival_km.md`, `enhanced_pca.md`) acts as a highly structured template for the LLM.

Each Markdown file must contain:

1. **Theoretical Context:** A brief explanation of when to use the test.

2. **Parameters:** Explicit definitions of the expected inputs (Response Y, Predictor X, Random Effects, etc.).

3. **The R Function:** A complete, runnable R wrapper function that includes the core statistical model (e.g., `lmer()`), structured plotting code (e.g., `dotplot()`), and a printed summary return.

#### 2.2 Building the Vector Database (`build_vector_db.py`)

This script ingests the Markdown documents, converts the text into high-dimensional numerical vectors using Ollama's `nomic-embed-text` model, and saves them locally into a persistent ChromaDB directory (`r_chroma_bd`).

Create this script at `scripts/build_vector_db.py`:

```python
import os
from langchain_community.document_loaders import DirectoryLoader, TextLoader
from langchain_chroma import Chroma
from langchain_ollama import OllamaEmbeddings

def build_database():
    print("Loading Markdown documentation from ./docs...")
    
    # Load all markdown files from the docs folder
    loader = DirectoryLoader('./docs', glob="**/*.md", loader_cls=TextLoader)
    documents = loader.load()
    
    if not documents:
        print("No markdown files found in ./docs. Please add your R functions.")
        return

    print(f"Found {len(documents)} documents. Initializing Ollama embeddings...")
    embeddings = OllamaEmbeddings(model="nomic-embed-text")

    # Generate embeddings and persist the database
    print("Building Chroma vector database (this may take a moment)...")
    vector_store = Chroma.from_documents(
        documents=documents,
        embedding=embeddings,
        persist_directory="./r_chroma_bd"
    )
    
    print(f"Success! Vector database saved to ./r_chroma_bd with {len(documents)} embedded functions.")

if __name__ == "__main__":
    build_database()

```

**Execution:** `python3 scripts/build_vector_db.py`

#### 2.3 Vector Database Validation & Visualization (`check_vector_db.py`)

Because embeddings exist in a mathematical space, we need a visual diagnostic tool to ensure the LLM is retrieving the correct files. This script tests a prompt against the database, outputs the raw data fed to the LLM, and generates a visual PCA map to show how the functions cluster semantically.

*(This incorporates the NumPy multi-dimensional array fix to prevent ambiguous truth-value crashes).*

Create this script at `scripts/check_vector_db.py`:

```python
import argparse
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.decomposition import PCA
from langchain_chroma import Chroma
from langchain_ollama import OllamaEmbeddings
import numpy as np

def analyze_database(query):
    # 1. Connect to the local vector database
    embeddings = OllamaEmbeddings(model="nomic-embed-text")
    vector_store = Chroma(persist_directory="./r_chroma_bd", embedding_function=embeddings)
    
    collection = vector_store._collection
    data = collection.get(include=['embeddings', 'metadatas'])
    
    # Check for empty database safely (fixes the truth value array error)
    if data.get('embeddings') is None or len(data['embeddings']) == 0:
        print("Error: The database is empty.")
        return

    print(f"Successfully loaded {len(data['embeddings'])} function embeddings from ChromaDB.")

    # Prepare plot canvas
    plt.figure(figsize=(16, 7))

    # ==========================================
    # PLOT 1: PCA Cluster Map
    # ==========================================
    X = np.array(data['embeddings'])
    labels = [meta['source'].replace('.md', '') for meta in data['metadatas']]
    
    pca = PCA(n_components=2)
    X_pca = pca.fit_transform(X)
    
    plt.subplot(1, 2, 1)
    sns.scatterplot(x=X_pca[:, 0], y=X_pca[:, 1], s=100, color="dodgerblue")
    
    for i, label in enumerate(labels):
        plt.text(X_pca[i, 0] + 0.02, X_pca[i, 1] + 0.02, label, fontsize=9, alpha=0.8)
        
    plt.title("Semantic Clustering of R Functions (PCA)")
    plt.xlabel("Principal Component 1")
    plt.ylabel("Principal Component 2")

    # ==========================================
    # PLOT 2: Query Similarity Ranking
    # ==========================================
    plt.subplot(1, 2, 2)
    print(f"\nTesting Query: '{query}'")
    
    results = vector_store.similarity_search_with_score(query, k=8)
    doc_names = [res[0].metadata['source'].replace('.md', '') for res in results]
    distances = [res[1] for res in results]
    
    sns.barplot(x=distances, y=doc_names, hue=doc_names, palette="viridis", legend=False)
    plt.title(f"Search Results: Distance to Query\n(Lower Score = More Similar Match)")
    plt.xlabel("L2 Distance Score")
    plt.ylabel("R Function Documentation")
    
    output_file = "./output/vector_db_analysis.png"
    plt.tight_layout()
    plt.savefig(output_file, dpi=300)
    print(f"\nVisualizations saved to {output_file}")

    # ==========================================
    # STEP 3: Display Top Vector Content
    # ==========================================
    top_match = results[0][0]
    top_score = results[0][1]
    
    print("\n" + "="*50)
    print(f"TOP MATCH INSPECTION")
    print("="*50)
    print(f"Source File   : {top_match.metadata['source']}")
    print(f"Vector Score  : {top_score:.4f} (Lower is better)")
    print("-" * 50)
    print("RAW CONTENT FED TO LLM:\n")
    print(top_match.page_content)
    print("="*50 + "\n")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Visualize ChromaDB embeddings and test RAG similarity.")
    parser.add_argument("--query", type=str, default="Fit a linear mixed model with random effects and plot it.", help="Test prompt to check retrieval accuracy.")
    args = parser.parse_args()
    
    analyze_database(args.query)

```

**Execution:** `python3 scripts/check_vector_db.py --query "I need to analyze time-to-event data and plot a survival curve with confidence intervals."`

### Phase 3: Data Simulation & Grounding (Expanded)

This phase ensures your RAG pipeline is tested against statistically robust data and that the LLM is securely "grounded" in reality. Without this phase, small toy datasets cause complex models (like mixed-effects or survival models) to crash due to statistical inadequacy, and the LLM hallucinates generic variable names (like `y` and `x`) instead of using your actual clinical data.

#### 3.1 Clinical Trial Simulation (`simulate_data.py`)

To properly evaluate the 20 R functions in your database, we must generate a complex, 200-patient synthetic clinical trial dataset. This dataset is engineered to contain specific statistical structures:

* **Continuous Responses:** For T-tests, ANOVA, and Linear Models.
* **Random Effects:** Clustered variance across different clinic sites for Mixed Models.
* **Correlated Features:** Multiple biomarkers for Principal Component Analysis (PCA).
* **Binary Outcomes:** For Logistic Regression.
* **Censored Time-to-Event Data:** For Kaplan-Meier and Cox Proportional Hazards survival analysis.

Create this script at `scripts/simulate_data.py`:

```python
import pandas as pd
import numpy as np

def generate_clinical_data():
    # Set seed for reproducibility
    np.random.seed(42)
    n = 200

    # Base demographics and groupings
    data = {
        'patient_id': range(1, n + 1),
        'treatment': np.random.choice(['Drug_A', 'Drug_B', 'Placebo'], n),
        'gender': np.random.choice(['M', 'F'], n),
        'age': np.random.normal(50, 10, n).round(0),
        'clinic_site': np.random.choice(['Site_1', 'Site_2', 'Site_3', 'Site_4'], n),
        'biomarker_1': np.random.normal(100, 15, n),
    }
    df = pd.DataFrame(data)

    # Add correlated variables for PCA / Correlation Matrix
    df['biomarker_2'] = df['biomarker_1'] * 0.5 + np.random.normal(0, 10, n)
    df['biomarker_3'] = df['biomarker_1'] * -0.3 + np.random.normal(50, 5, n)

    # Define effects for modeling
    treatment_effect = df['treatment'].map({'Drug_A': 10, 'Drug_B': 5, 'Placebo': 0})
    site_effect = df['clinic_site'].map({'Site_1': 2, 'Site_2': -2, 'Site_3': 5, 'Site_4': -5})

    # Continuous Response
    df['bp_reduction'] = 5 + treatment_effect + site_effect + df['age']*0.1 + np.random.normal(0, 3, n)

    # Binary Outcome (Adverse Event)
    logit = -5 + 0.05 * df['age'] + 0.02 * df['biomarker_1']
    prob = 1 / (1 + np.exp(-logit))
    df['adverse_event'] = np.random.binomial(1, prob)

    # Time-to-Event Data (Survival Analysis)
    hazard = np.where(df['treatment'] == 'Placebo', 0.1, 0.05)
    df['survival_time'] = np.random.exponential(1 / hazard).round(1)
    df['survival_status'] = np.random.binomial(1, 0.7, n) # 70% observed events (1), 30% censored (0)

    # Save to CSV
    output_file = "clinical_trial_data.csv"
    df.to_csv(output_file, index=False)
    print(f"Successfully generated {n} rows of complex clinical data in '{output_file}'")

if __name__ == "__main__":
    generate_clinical_data()

```

**Execution:** `python3 scripts/simulate_data.py`

#### 3.2 Data Grounding & Injection Strategy (Giving the LLM "Eyes")

One of the biggest challenges in Agentic AI coding is that the LLM will blindly copy the generic formulas from your R documentation (e.g., `y ~ x + (1|random_effect)`) because it cannot "see" your dataset. If it writes a script asking for `y`, R will crash.

To solve this, we use a Data Injection Strategy in the orchestrator. Before the LLM generates the script, Python uses `pandas` to read just the header row of your CSV file. It then formats these column names into a string and injects them directly into the LLM's system prompt as `AVAILABLE DATASET COLUMNS`.

Here is the exact mechanism integrated into the top of your orchestrator:

```python
# Extract column names to ground the LLM
try:
    # Read only the header row to save memory and time
    df = pd.read_csv(dataset_path, nrows=0)
    dataset_columns = ", ".join(df.columns.tolist())
except Exception:
    dataset_columns = "Could not load columns."

# Later in the script, this is injected into the LLM prompt:
coder_prompt = f"""
...
AVAILABLE DATASET COLUMNS: {dataset_columns}
...
"""

```

By dynamically injecting `patient_id, treatment, gender, age, clinic_site, biomarker_1, ... bp_reduction, survival_time`, the LLM maps the user's prompt (e.g., "Predict blood pressure reduction using age") to the exact column names (e.g., `bp_reduction ~ age`), successfully bridging the gap between theoretical code and your physical data structure.

### Phase 4: Multi-Agent Prompt Engineering & Guardrails (Expanded)

This phase focuses on the "brain" of your AIBioStatAgent. By designing strict, multi-agent prompt guardrails, we prevent the two most common failure modes in Agentic AI: code hallucination (inventing fake functions or variables) and data hallucination (inventing fake statistical results when code fails).

#### 4.1 Coder Agent Guardrails (Anti-Hallucination & Mapping)

The Coder LLM (`llama3.2` at a strict `temperature=0.1`) is responsible for writing the R script. Left to its own devices, an LLM will often blindly copy abstract placeholders (like `y ~ x`) from your documentation or invent fake plotting functions (like `catplot()` instead of `dotplot()`).

To enforce strict mapping to your `AVAILABLE DATASET COLUMNS` and provided documentation, the orchestrator injects nine highly specific rules into the system prompt. The most critical guardrails include:

```python
RULES:
1. USE THE PROVIDED FUNCTION: If the R DOCUMENTATION contains a function relevant to the request, you MUST include its exact definition in your script and call it. Do not invent fake plotting functions.
6. EXACTLY match the column names from the AVAILABLE DATASET COLUMNS.
7. SMART MATCHING: If the user's prompt contains a slight typo in a variable name, autonomously correct it to the exact matching column name.
8. IDENTIFY RESPONSE (Y) AND PREDICTORS (X): Explicitly analyze the USER REQUEST to determine which variable is the outcome/response (y) and which are the features/predictors (x). 
9. NO PLACEHOLDERS: When constructing formulas or passing arguments, strictly use the actual mapped column names (e.g., 'bp_reduction ~ age'). NEVER use the literal letters 'y' or 'x' in your code unless they exist in the AVAILABLE DATASET COLUMNS.

```

#### 4.2 Fallback Autonomy (Out-of-Scope Requests)

A robust RAG system must know what it *doesn't* know. If a user asks for a statistical model that is explicitly missing from your 20 Markdown files (for example, a Cox Proportional Hazards model with continuous predictors, which isn't covered by your Kaplan-Meier function), the system must not force a mismatched function.

We engineered **Rule 2** to grant the agent autonomy to bypass the database and write original code using its internal knowledge:

* `2. If the context does NOT meet the requirements, write your own R code to solve the problem.`

#### 4.3 Interpreter Agent Guardrails (Error Handling & Interpretation)

The Interpreter LLM (`llama3.2` at a more creative `temperature=0.5`) is responsible for reading the terminal output and writing the final clinical report.

Early in development, if the R script crashed (e.g., due to a missing package like `tidyverse` or a typo), the terminal output would be empty. In its eagerness to fulfill the prompt, the LLM would hallucinate perfectly formatted but entirely fake statistical results (e.g., `p-value = 0.1234`).

To fix this, Python's `subprocess` was updated to capture **both** standard output (`STDOUT`) and standard error (`STDERR`). We then pass both directly to the Interpreter LLM with these strict constraints:

```python
interpreter_prompt = f"""You are a Lead Biostatistician reviewing the output of an analysis.
Based on the user's original request and the raw R terminal output, write a professional documentation report.

CRITICAL RULES:
1. If the RAW R OUTPUT contains an error, DO NOT make up or hallucinate any statistical numbers. State clearly that the analysis failed and explain the error.
2. If the output is successful, summarize the findings, explain what the numbers mean, and conclude with actionable insights.

USER REQUEST: {user_prompt}

RAW R OUTPUT:
{r_output}
"""

```

By explicitly exposing the LLM to the `STDERR` stack traces (like `Exit Code: 1` or `could not find function`), the Interpreter transforms from a blind text-generator into a helpful diagnostic assistant that accurately explains *why* an analysis failed without inventing numbers.

### Phase 5: Execution Orchestration (`rag_orchestrator.py`) (Expanded)

This final phase represents the complete, end-to-end integration of the AIBioStatAgent. The orchestrator script serves as the central nervous system of the pipeline, seamlessly passing data between the vector database, the Coder LLM, the local execution environment, and the Interpreter LLM.

#### 5.1 Context Retrieval (Connecting RAG to the User)

When the user submits a natural language request, the orchestrator intercepts the prompt and converts it into a vector using the same `nomic-embed-text` model used during database construction. It then performs a similarity search against the ChromaDB collection to retrieve the top 3 most statistically relevant Markdown documents.

```python
# Initialize local models
embeddings = OllamaEmbeddings(model="nomic-embed-text")

# Retrieve Context from ChromaDB
print("Retrieving context from ChromaDB...")
vector_store = Chroma(persist_directory="./r_chroma_bd", embedding_function=embeddings)
retrieved_docs = vector_store.similarity_search(user_prompt, k=3)

# Concatenate the retrieved documents into a single text block for the LLM
context = "\n\n".join([doc.page_content for doc in retrieved_docs])

```

#### 5.2 Script Generation & Code Extraction

The orchestrator combines the retrieved context, the exact `AVAILABLE DATASET COLUMNS`, the strict guardrail rules, and the user's prompt into the `coder_prompt`. This is passed to the Coder LLM (`llama3.2`, temperature 0.1).

Once the LLM replies, the orchestrator uses Regular Expressions (Regex) to cleanly extract only the executable R code, stripping away any conversational text or markdown formatting before saving it as `analysis_script.R`.

```python
script_response = llm_coder.invoke(coder_prompt)

# Extract Code cleanly using Regex
match = re.search(r'```[rR]\n(.*?)\n```', script_response, re.DOTALL)
r_code = match.group(1) if match else script_response.replace("```r", "").replace("```", "").strip()

script_path = os.path.join(session_dir, "analysis_script.R")
with open(script_path, "w") as f:
    f.write(r_code)

```

#### 5.3 Subprocess Execution & Session Management

To ensure data from different prompts doesn't overwrite itself, the orchestrator creates a unique, timestamped session folder for every run (e.g., `session_20261109_143000`).

It then uses Python's `subprocess` module to execute the generated R script natively on the Beelink server. This module is configured to capture the raw terminal output—both successes (`STDOUT`) and crashes (`STDERR`)—and logs them sequentially into an `execution_log.txt` file inside the session folder.

```python
# Execute R Script locally and capture outputs
r_output = ""
try:
    result = subprocess.run(["Rscript", script_path], capture_output=True, text=True, check=True)
    r_output = f"=== SUCCESS: STDOUT ===\n{result.stdout}\n"
    print("Execution successful.")
except subprocess.CalledProcessError as e:
    r_output = f"=== ERROR EXECUTING R SCRIPT ===\nExit Code: {e.returncode}\n\n=== STDOUT ===\n{e.stdout}\n\n=== STDERR ===\n{e.stderr}\n"
    print("Execution failed. Capturing error for interpretation.")
    
# Save execution log
log_path = os.path.join(session_dir, "execution_log.txt")
with open(log_path, "w") as f:
    f.write(r_output)

```

#### 5.4 Biostatistical Interpretation & Reporting

Finally, the orchestrator passes the raw, captured terminal output (`r_output`) to the Interpreter LLM (`llama3.2`, temperature 0.5) alongside the `interpreter_prompt`.

If the script succeeded, the LLM translates the complex statistical summaries (e.g., REML criteria, fixed effects, p-values, t-statistics) into a human-readable clinical report, providing actionable insights. If the script failed, the LLM reads the error traceback and explains the point of failure.

This final output is saved directly into the session folder as `analysis_report.md`.

```python
# Generate the final report
report = llm_interpreter.invoke(interpreter_prompt)

report_path = os.path.join(session_dir, "analysis_report.md")
with open(report_path, "w") as f:
    f.write(report)
    
print(f"\nFinal Interpretation saved to {report_path}")

```

---

With all 5 phases fully expanded, you now have a comprehensive architectural document detailing the exact mechanisms, hardware context, and coding logic behind the **AIBioStatAgent**. Let me know if you would like to package this text into a specific format (like a PDF report or a markdown file) for your Master's capstone repository!

