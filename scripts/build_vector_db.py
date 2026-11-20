import os
from langchain_chroma import Chroma
from langchain_ollama import OllamaEmbeddings
from langchain_core.documents import Document

embeddings = OllamaEmbeddings(model="nomic-embed-text")
docs_dir = "./docs"
r_docs = []

# Read all markdown files from the docs directory
for filename in os.listdir(docs_dir):
    if filename.endswith(".md"):
        with open(os.path.join(docs_dir, filename), "r") as f:
            r_docs.append(Document(page_content=f.read(), metadata={"source": filename}))

# Create and persist the database to r_chroma_bd
vector_store = Chroma.from_documents(
    documents=r_docs,
    embedding=embeddings,
    persist_directory="./r_chroma_bd"
)
print(f"Successfully embedded {len(r_docs)} R documentation files into ./r_chroma_bd")
