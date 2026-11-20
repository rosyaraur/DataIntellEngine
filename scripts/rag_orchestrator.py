import os
import re
import subprocess
import argparse
import datetime
import pandas as pd
from langchain_chroma import Chroma
from langchain_ollama import OllamaEmbeddings, OllamaLLM

def run_agent(user_prompt, dataset_path):
    # Create a unique session directory based on current date and time
    session_id = datetime.datetime.now().strftime("session_%Y%m%d_%H%M%S")
    session_dir = f"./output/{session_id}"
    os.makedirs(session_dir, exist_ok=True)
    print(f"Starting new session: {session_dir}")

    # Extract column names to ground the LLM
    try:
        df = pd.read_csv(dataset_path, nrows=0)
        dataset_columns = ", ".join(df.columns.tolist())
    except Exception:
        dataset_columns = "Could not load columns."

    embeddings = OllamaEmbeddings(model="nomic-embed-text")
    llm_coder = OllamaLLM(model="llama3.2", temperature=0.1) # Strict for coding
    llm_interpreter = OllamaLLM(model="llama3.2", temperature=0.5) # Creative for analysis

    # 1. Retrieve Context
    print("Retrieving context from ChromaDB...")
    vector_store = Chroma(persist_directory="./r_chroma_bd", embedding_function=embeddings)
    retrieved_docs = vector_store.similarity_search(user_prompt, k=3)
    context = "\n\n".join([doc.page_content for doc in retrieved_docs])

    # 2. LLM Pass 1: Generate R Script
    print("Generating R execution script...")
    coder_prompt = f"""You are an expert R Biostatistician. 
    Write a complete, runnable R script to solve the user's request using the dataset at '{dataset_path}'.
    
    AVAILABLE DATASET COLUMNS: {dataset_columns}
    
    RULES:
    1. USE THE PROVIDED FUNCTION: If the R DOCUMENTATION contains a function relevant to the request, you MUST include its exact definition in your script and call it. Do not invent fake plotting functions.
    2. If the context does NOT meet the requirements, write your own R code to solve the problem.
    3. Print all statistical results to the console so they can be captured.
    4. Save any plots to the '{session_dir}/' directory using ggsave() or pdf()/dev.off().
    5. Return ONLY valid R code enclosed in ```r and ``` markdown blocks.
    6. EXACTLY match the column names from the AVAILABLE DATASET COLUMNS.
    7. SMART MATCHING: If the user's prompt contains a slight typo in a variable name, autonomously correct it to the exact matching column name.
    8. IDENTIFY RESPONSE (Y) AND PREDICTORS (X): Explicitly analyze the USER REQUEST to determine which variable is the outcome/response (y) and which are the features/predictors (x). 
    9. NO PLACEHOLDERS: When constructing formulas or passing arguments, strictly use the actual mapped column names (e.g., 'bp_reduction ~ age'). NEVER use the literal letters 'y' or 'x' in your code unless they exist in the AVAILABLE DATASET COLUMNS.

    R DOCUMENTATION CONTEXT:
    {context}

    USER REQUEST: 
    {user_prompt}
    """
    
    script_response = llm_coder.invoke(coder_prompt)
    
    # Extract Code
    match = re.search(r'```[rR]\n(.*?)\n```', script_response, re.DOTALL)
    r_code = match.group(1) if match else script_response.replace("```r", "").replace("```", "").strip()
    
    script_path = os.path.join(session_dir, "analysis_script.R")
    with open(script_path, "w") as f:
        f.write(r_code)
    print(f"Script saved to {script_path}")

    # 3. Execute R Script
    print("Executing R script locally...")
    r_output = ""
    try:
        result = subprocess.run(["Rscript", script_path], capture_output=True, text=True, check=True)
        r_output = f"=== SUCCESS: STDOUT ===\n{result.stdout}\n"
        print("Execution successful.")
    except subprocess.CalledProcessError as e:
        r_output = f"=== ERROR EXECUTING R SCRIPT ===\nExit Code: {e.returncode}\n\n=== STDOUT ===\n{e.stdout}\n\n=== STDERR ===\n{e.stderr}\n"
        print("Execution failed. Capturing error for interpretation.")
        
    # Save execution log (both outputs and errors)
    log_path = os.path.join(session_dir, "execution_log.txt")
    with open(log_path, "w") as f:
        f.write(r_output)
    print(f"Execution log saved to {log_path}")

    # 4. LLM Pass 2: Interpret Results
    print("Interpreting statistical results...")
    interpreter_prompt = f"""You are a Lead Biostatistician reviewing the output of an analysis.
    Based on the user's original request and the raw R terminal output, write a professional documentation report.
    
    CRITICAL RULES:
    1. If the RAW R OUTPUT contains an error, DO NOT make up or hallucinate any statistical numbers. State clearly that the analysis failed and explain the error.
    2. If the output is successful, summarize the findings, explain what the numbers mean, and conclude with actionable insights.

    USER REQUEST: {user_prompt}
    
    RAW R OUTPUT:
    {r_output}
    """
    
    report = llm_interpreter.invoke(interpreter_prompt)
    
    report_path = os.path.join(session_dir, "analysis_report.md")
    with open(report_path, "w") as f:
        f.write(report)
        
    print(f"\nFinal Interpretation saved to {report_path}:\n")
    print(report)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--prompt", type=str, required=True, help="The statistical task to perform")
    parser.add_argument("--data", type=str, default="data.csv", help="Path to the dataset")
    args = parser.parse_args()
    
    run_agent(args.prompt, args.data)
