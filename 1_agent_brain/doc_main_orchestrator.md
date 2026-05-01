# 🧠 Internal Documentation: `main_orchestrator.py`

## Overview
The `main_orchestrator.py` script serves as the central nervous system of the DataIntelEngine. It is responsible for initializing the local AI model (Llama 3.2 via Ollama), enforcing global behavioral rules, parsing user requests, and autonomously triggering the correct biometric scripts through the `tool_registry.py` bridge.

Because of the engine's modular architecture, **this file rarely needs to be edited once deployed**. It dynamically loads its behavioral rules from a plain text file and its capabilities from a JSON schema, eliminating the need for hardcoded updates.

---

## 🏗️ Architecture & Component Breakdown

The script is divided into three distinct logical blocks:

### 1. The Dynamic Loader (Initialization)
This section wires the AI's brain to its utility belt and rulebook.
* **Tool Registry Import:** It securely imports the `AVAILABLE_TOOLS` dictionary from `tool_registry.py`. If the registry is missing or misconfigured, the script catches the `ImportError` and gracefully terminates before the AI can hallucinate.
* **Schema Loading:** It reads `tools_schema.json` into memory. This dynamically updates the AI's "instruction manual" every time the engine boots up, ensuring the LLM is always aware of the latest R and Python tools added by the team.
* **System Prompt Loading:** It reads `system_prompt.txt` into memory. This entirely decouples the agent's core personality and global instructions from the Python codebase, allowing data scientists to tweak AI behavior using plain text.

### 2. The `run_agent(user_prompt)` Function
This is the core orchestration loop that gives the AI its autonomy.
* **The System Prompt Injection:** Before the AI processes the user's input, the global text loaded from `system_prompt.txt` is injected as the very first invisible message. This establishes the absolute laws of the engine (e.g., "Do not perform complex biometric calculations yourself," "Use the exact numbers returned by tools").
* **The Agentic Loop (`while True`):**
    1. **Evaluation:** The LLM evaluates the `messages` history against the available tools.
    2. **Execution:** If the LLM requests a tool call, the script extracts the `function_name` and `arguments`, looks them up in `AVAILABLE_TOOLS`, and executes the local Python/R script.
    3. **Error Handling:** The execution is wrapped in a `try/except` block. If an R script fails (e.g., a matrix singularity error) or a Python script crashes, the orchestrator catches the traceback and feeds it *back* to the LLM. This allows the AI to read the error and suggest a fix to the user, rather than crashing the entire application.
    4. **Memory Append:** The result of the local tool execution is appended to the `messages` list, and the loop restarts so the AI can evaluate the new data.
    5. **Exit Condition:** If the LLM decides no more tools are needed, it generates a final text response to the user, and the loop breaks.

### 3. The Interactive Terminal (REPL)
This block (`if __name__ == "__main__":`) transforms the script from a background process into a live, interactive command-line interface.
* It captures human input via a standard `while True` input loop.
* It filters out blank submissions (accidental 'Enter' presses).
* It provides a clean, safe exit strategy via the `quit`/`exit` commands or standard `Ctrl+C` keyboard interrupts, ensuring background subprocesses are properly released.

---

## 🔄 The Data Flow Lifecycle
When a user types a command into the terminal, the following sequence occurs:

1. **User Input:** *"Run a diallel analysis on crosses.csv."*
2. **Context Assembly:** `main_orchestrator.py` combines the loaded Text Prompt, the JSON schemas, and the User Input.
3. **LLM Inference:** The package is sent to the local Ollama instance.
4. **Tool Selection:** Ollama responds with a JSON payload requesting `complete_diallel_analysis` with the argument `dataset_path: "crosses.csv"`.
5. **Sandbox Execution:** `main_orchestrator.py` triggers the function in `tool_registry.py`, which executes the R package in the background.
6. **Result Capture:** R completes the math and saves the plot. The registry returns the success message to the orchestrator.
7. **Synthesis:** The orchestrator hands the success message back to Ollama. Ollama realizes the task is complete and generates the final output: *"The diallel analysis is complete and the plot has been saved."*

---

## ⚙️ Standard Operating Procedures (SOPs) for the Team

### Modifying Agent Behavior
If the team notices the AI behaving inconsistently (e.g., it is being too chatty, or it keeps trying to write its own Python code instead of using the tools), **do not adjust the Python code or the individual tools**. 

Instead, open the **`system_prompt.txt`** file located in the `1_agent_brain` directory and update the plain text rules. 

For example, you can simply add a new line to the text file:
> *5. Always format numeric outputs to three decimal places.*

Save the file and restart the orchestrator. The engine will automatically adopt this new behavior on its next startup.

### Handling Context Limits
If you are analyzing massive, multi-stage field trials and executing dozens of tool calls in a single session, the local 3B parameter model may begin to hallucinate as its context window fills up. 
* **Recommendation:** Instruct users to frequently restart the engine using `quit` to clear the `messages` list and refresh the context window between entirely distinct research tasks.