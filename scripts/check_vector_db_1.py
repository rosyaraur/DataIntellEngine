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
