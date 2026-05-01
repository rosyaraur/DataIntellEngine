import json
import ollama
import sys
import os

# ==========================================
# 1. LOAD THE REGISTRY, SCHEMAS & PROMPTS
# ==========================================
# Import your custom tools bridge
try:
    from tool_registry import AVAILABLE_TOOLS
except ImportError:
    print("Error: Could not find tool_registry.py. Ensure you are running this from the 1_agent_brain folder.")
    sys.exit(1)

# Load the LLM Instruction Manual (Schema)
schema_path = 'tools_schema.json'
try:
    with open(schema_path, 'r') as file:
        tool_schemas = json.load(file)
except FileNotFoundError:
    print(f"Error: Could not find {schema_path}. Make sure it is in the same folder.")
    sys.exit(1)

# Load the System Prompt from the text file
prompt_path = 'system_prompt.txt'
try:
    with open(prompt_path, 'r') as file:
        GLOBAL_SYSTEM_PROMPT = file.read()
except FileNotFoundError:
    print(f"Error: Could not find {prompt_path}. Make sure it is in the same folder.")
    sys.exit(1)

# ==========================================
# 2. THE AI BRAIN (Orchestration Loop)
# ==========================================
def run_agent(user_prompt):
    # The System Prompt: Absolute rules for the agent loaded from text file
    messages = [
        {'role': 'system', 'content': GLOBAL_SYSTEM_PROMPT},
        {'role': 'user', 'content': user_prompt}
    ]

    while True:
        print("\n[Engine is thinking...]")
        
        # Ask the LLM what to do
        try:
            response = ollama.chat(
                model='llama3.2:3b',
                messages=messages,
                tools=tool_schemas
            )
        except Exception as e:
            print(f"\n[!] Ollama Error: {e}")
            print("Make sure Ollama is running in the background.")
            break

        # Add the model's response to the conversation history
        messages.append(response['message'])

        # EXIT STRATEGY: If it didn't call a tool, it is completely finished!
        if not response['message'].get('tool_calls'):
            print("\n" + "="*50)
            print("🧠 FINAL AGENT ANSWER:")
            print("="*50)
            print(response['message']['content'])
            print("="*50)
            break 

        # TOOL EXECUTION: Execute the chosen tools locally
        for tool_call in response['message']['tool_calls']:
            function_name = tool_call['function']['name']
            arguments = tool_call['function']['arguments']
            
            print(f" -> Agent chose tool: {function_name}()")
            print(f" -> With arguments: {arguments}")
            
            # Look up the function in our registry
            function_to_call = AVAILABLE_TOOLS.get(function_name)
            
            if function_to_call:
                print(f" -> Executing local script...")
                try:
                    tool_output = function_to_call(**arguments)
                    print(f" -> Sandbox Execution Result: Success (Data returned to Agent)")
                except Exception as e:
                    tool_output = f"Execution Error: {str(e)}"
                    print(f" -> Sandbox Execution Result: {tool_output}")
                
                # Send the raw result back to the LLM so it can decide the next step
                messages.append({
                    'role': 'tool',
                    'content': str(tool_output),
                    'name': function_name
                })
            else:
                error_msg = f"Error: Tool '{function_name}' not found in tool_registry.py."
                print(f" -> {error_msg}")
                messages.append({
                    'role': 'tool', 
                    'content': error_msg, 
                    'name': function_name
                })

# ==========================================
# 3. THE INTERACTIVE TERMINAL (REPL)
# ==========================================
if __name__ == "__main__":
    print("\n" + "="*50)
    print("🌾 Welcome to DataIntelEngine 🌾")
    print("   AI Orchestrator Initialized.")
    print("   Type 'exit' or 'quit' to close.")
    print("="*50)

    # This loop keeps your terminal open forever, waiting for your commands
    while True:
        try:
            # Wait for the user to type something
            user_input = input("\nLead Data Scientist: ")
            
            # Give the user a way to escape
            if user_input.strip().lower() in ['exit', 'quit']:
                print("Shutting down engine. Goodbye!")
                break
                
            # Ignore empty accidental 'Enter' presses
            if not user_input.strip():
                continue
                
            # Pass whatever you typed directly into the AI Brain
            run_agent(user_input)
            
        except KeyboardInterrupt:
            # Catches Ctrl+C to exit gracefully instead of crashing
            print("\nShutting down engine. Goodbye!")
            break