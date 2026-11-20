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
    
    # Extract raw data (embeddings and metadata) directly from Chroma's underlying collection
    collection = vector_store._collection
    data = collection.get(include=['embeddings', 'metadatas'])
    
if data.get('embeddings') is None or len(data['embeddings']) == 0:   
        print("Error: The database is empty.")
        return

    print(f"Successfully loaded {len(data['embeddings'])} function embeddings from ChromaDB.")

    # Prepare plot canvas
    plt.figure(figsize=(16, 7))

    # ==========================================
    # PLOT 1: PCA Cluster Map of All Functions
    # ==========================================
    # Convert embeddings to a numpy matrix and reduce to 2 dimensions for plotting
    X = np.array(data['embeddings'])
    labels = [meta['source'].replace('.md', '') for meta in data['metadatas']]
    
    pca = PCA(n_components=2)
    X_pca = pca.fit_transform(X)
    
    plt.subplot(1, 2, 1)
    sns.scatterplot(x=X_pca[:, 0], y=X_pca[:, 1], s=100, color="dodgerblue")
    
    # Add function names as text labels to the scatter plot
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
    
    # Perform similarity search (returns documents and their L2 distance scores)
    # Note: In ChromaDB's default L2 metric, a LOWER score means it is MORE similar/closer.
    results = vector_store.similarity_search_with_score(query, k=8)
    
    doc_names = [res[0].metadata['source'].replace('.md', '') for res in results]
    distances = [res[1] for res in results]
    
    # Plot horizontal bar chart
    sns.barplot(x=distances, y=doc_names, hue=doc_names, palette="viridis", legend=False)
    plt.title(f"Search Results: Distance to Query\n(Lower Score = More Similar Match)")
    plt.xlabel("L2 Distance Score")
    plt.ylabel("R Function Documentation")
    
    # Save the output visualization
    output_file = "./output/vector_db_analysis.png"
    plt.tight_layout()
    plt.savefig(output_file, dpi=300)
    print(f"\nAnalysis complete! Visualizations saved to {output_file}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Visualize ChromaDB embeddings and test RAG similarity.")
    parser.add_argument("--query", type=str, default="Fit a linear mixed model with random effects and plot it.", help="Test prompt to check retrieval accuracy.")
    args = parser.parse_args()
    
    analyze_database(args.query)
