
# 📘 Internal Documentation: `tools_schema.json`

## Overview
The `tools_schema.json` file is the **Instruction Manual** for the DataIntelEngine's AI Orchestrator. 

Because the local Llama 3.2 model cannot physically "see" our Python or R code, it relies entirely on this JSON file to understand what capabilities it possesses. This file defines the tools, explains when to use them, and strictly enforces the parameters the AI must gather before triggering a background script. 

By keeping this schema decoupled from `main_orchestrator.py`, the team can dynamically add, remove, or modify tools without ever touching the core Python loop.

---

## 🏗️ Anatomy of a Tool Schema

Every tool in the JSON array follows a strict OpenAI-style function-calling format. Understanding the four core components of a schema block is critical for prompt engineering the agent.

### 1. `name` (The Identifier)
* **What it is:** The programmatic name of the tool (e.g., `"name": "generate_alpha_design"`).
* **The Golden Rule:** This string **MUST** perfectly match a key in the `AVAILABLE_TOOLS` dictionary inside `tool_registry.py`. If there is a typo here, the AI will try to call a tool that the Python switchboard cannot find.

### 2. `description` (The Micro-Prompt)
* **What it is:** A human-readable summary of what the tool does.
* **Why it matters:** This is the most important field in the file. The AI reads this description to decide *which* tool solves the user's current problem. 
* **Best Practice:** Write this defensively. Don't just say "Designs a trial." Say: *"Generates an Alpha design for breeding trials. Use this when the user needs to randomize genotypes into incomplete blocks. Do not use this for complete block designs."*

### 3. `parameters` (The Data Contract)
* **What it is:** A nested JSON object defining the exact inputs the Python/R scripts expect (e.g., integers, strings, booleans).
* **Why it matters:** This forces the AI to translate conversational English into strict data types.
* **Descriptions act as guardrails:** You can include descriptions inside the parameters to enforce formatting. For example:
  ```json
  "color_theme": {
    "type": "string", 
    "description": "The color map to use. Default to 'YlGnBu'. Never use red."
  }
  ```

### 4. `required` (The Interrogation Flag)
* **What it is:** A list of parameter names that the tool absolutely cannot function without (e.g., `"required": ["dataset_path", "genotypes"]`).
* **Why it matters:** If the user says, *"Generate a field map,"* but forgets to provide the number of genotypes, the AI checks this `required` list. Realizing a required variable is missing, the AI will pause tool execution and ask the user: *"How many genotypes are in this trial?"*

---

## 🔄 The Data Flow Lifecycle

1. **Initialization:** When the engine boots up, `main_orchestrator.py` loads the JSON array and hands it to Ollama.
2. **Evaluation:** When a prompt arrives (e.g., *"Run a multi-stage field sim for 2014"*), the AI scans the `description` of every tool in the schema.
3. **Selection:** It selects `"sim_multistage_field"` because the description matches the user's intent.
4. **Parameter Extraction:** It extracts "2014" from the prompt, formats it according to the schema's parameter rules, and outputs a structured JSON tool call.
5. **Handoff:** The orchestrator reads the tool call and passes the parameters to the registry.

---

## ⚙️ Standard Operating Procedures (SOPs) for the Team

### 1. Adding a New Tool
When a Data Scientist builds a new script and adds the wrapper to `tool_registry.py`, they must immediately document it in `tools_schema.json`.
1. Copy an existing JSON block to ensure formatting is correct.
2. Update the `name` to match the registry.
3. Write a clear, highly specific `description`.
4. Map out every required parameter, double-checking the `type` (e.g., `number` vs. `integer` vs. `string`).

### 2. Handling "Nested" Data (The JSON String Hack)
Small local LLMs struggle to output complex, deeply nested JSON objects. 
* **The Problem:** If an R script requires a complex dictionary (like a mapping of years to locations), the AI might format it incorrectly, crashing the script.
* **The Solution:** Define the parameter type as a simple `"string"` in the schema, and instruct the AI to output stringified JSON. 
  ```json
  "batch_years": {
    "type": "string",
    "description": "A JSON-formatted string mapping batch names to lists of years. Example: '{\"2014\": [2018, 2019]}'."
  }
  ```
  Your Python registry can then safely parse that string using `json.loads()` before passing it to the core math library.

### 3. Debugging Hallucinations
If the AI is consistently choosing the wrong tool for a specific prompt:
* **Do not change the Python code.**
* Open `tools_schema.json` and rewrite the `description` for the abused tool to be more restrictive (e.g., *"ONLY use this tool if..."*).
* Alternatively, update the description of the *correct* tool to be more appealing to the AI's logic.