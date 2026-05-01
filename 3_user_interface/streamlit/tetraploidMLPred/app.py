import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import pearsonr
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.metrics import mean_squared_error
from sklearn.ensemble import RandomForestRegressor
from sklearn.svm import SVR
from sklearn.linear_model import RidgeCV
from sklearn.kernel_ridge import KernelRidge
from sklearn.neural_network import MLPRegressor
import warnings

warnings.filterwarnings("ignore")

# ==========================================
# CORE MACHINE LEARNING FUNCTION
# ==========================================
def tetraploidMLPred(dfGeno, dfPheno, env, pheno_col, model_type, genetic_model):
    # 1. Filter Phenotype Data
    dfPheno_env = dfPheno[dfPheno['env'] == env].copy()
    dfPheno_env = dfPheno_env.set_index('id')
    
    # 2. Transpose Genotype Data
    dfGeno_T = dfGeno.T
    
    # 3. Find Intersecting Individuals
    common_rows = dfPheno_env.index.intersection(dfGeno_T.index)
    dfPheno_sub = dfPheno_env.loc[common_rows]
    dfGeno_sub = dfGeno_T.loc[common_rows]
    
    # Drop rows missing the target phenotype
    dfPheno_sub = dfPheno_sub.dropna(subset=[pheno_col])
    common_rows = dfPheno_sub.index
    dfGeno_sub = dfGeno_sub.loc[common_rows]
    
    # Convert Genotype data strictly to numeric
    dfGeno_sub = dfGeno_sub.apply(pd.to_numeric, errors='coerce').fillna(0)
    
    # 4. Apply Genetic Model Transformation
    if genetic_model == 'dominance':
        dfGeno_sub = pd.DataFrame(np.where(dfGeno_sub > 0, 4, 0), 
                                  index=dfGeno_sub.index, 
                                  columns=dfGeno_sub.columns)
        
    # 5. Scale and Split Data
    X_scaled = StandardScaler().fit_transform(dfGeno_sub)
    y = dfPheno_sub[pheno_col].values
    
    X_train, X_test, y_train, y_test = train_test_split(X_scaled, y, test_size=0.2, random_state=42)
    
    # 6. Model Dispatcher
    if model_type == 'RandomForest':
        model = RandomForestRegressor(n_estimators=100, random_state=42)
        model.fit(X_train, y_train)
        y_pred = model.predict(X_test)
        
    elif model_type == 'SVR_linear':
        model = SVR(kernel='linear', C=1.0, epsilon=0.1)
        model.fit(X_train, y_train)
        y_pred = model.predict(X_test)
        
    elif model_type == 'SVR_rbf':
        model = SVR(kernel='rbf', C=100, gamma=0.1, epsilon=0.1)
        model.fit(X_train, y_train)
        y_pred = model.predict(X_test)
        
    elif model_type == 'Ridge':
        alphas = np.logspace(0, 5, 100)
        model = RidgeCV(alphas=alphas, cv=5)
        model.fit(X_train, y_train)
        y_pred = model.predict(X_test)
        st.write(f"**Optimal Ridge Alpha:** {model.alpha_:.2f}")
        
    elif model_type == 'GBLUP':
        p = X_train.shape[1]
        GRM_train = np.dot(X_train, X_train.T) / p
        GRM_test = np.dot(X_test, X_train.T) / p
        
        alphas = np.logspace(-2, 3, 100)
        param_grid = {'alpha': alphas}
        model = GridSearchCV(KernelRidge(kernel='precomputed'), param_grid, cv=5)
        model.fit(GRM_train, y_train)
        y_pred = model.predict(GRM_test)
        st.write(f"**Optimal GBLUP Alpha:** {model.best_params_['alpha']:.2f}")
        
    elif model_type == 'NeuralNetwork':
        param_grid = {'hidden_layer_sizes': [(100,), (50, 50)], 'alpha': [0.0001, 0.01, 0.1, 1.0], 'max_iter': [500]}
        mlp = MLPRegressor(random_state=42, early_stopping=True)
        model = GridSearchCV(mlp, param_grid, cv=5, n_jobs=-1, scoring='neg_mean_squared_error')
        model.fit(X_train, y_train)
        y_pred = model.predict(X_test)
        st.write(f"**Optimal NN Architecture:** {model.best_estimator_.hidden_layer_sizes}")
    
    # 7. Evaluate Metrics
    r, p_val = pearsonr(y_test, y_pred)
    mse = mean_squared_error(y_test, y_pred)
    rmse = np.sqrt(mse)
    
    # 8. Visualization Setup for Streamlit
    fig, ax = plt.subplots(figsize=(8, 5))
    ax.scatter(y_test, y_pred, color='indigo', edgecolor='k', zorder=2)
    min_val, max_val = min(y_test), max(y_test)
    ax.plot([min_val, max_val], [min_val, max_val], 'r--', alpha=0.7, label='Perfect Prediction')
    
    ax.set_title(f"{model_type} ({genetic_model.capitalize()}) - Predicting '{pheno_col}'")
    ax.set_xlabel(f"True Observed {pheno_col}")
    ax.set_ylabel(f"Predicted {pheno_col}")
    ax.legend()
    ax.grid(True, linestyle='--', alpha=0.5)
    
    return r, p_val, rmse, fig

# ==========================================
# STREAMLIT UI BUILDER
# ==========================================
st.set_page_config(page_title="Potato Genomic Prediction", layout="wide")

st.title("🥔 Tetraploid Potato Genomic Prediction")
st.markdown("Upload your genotype and phenotype CSV files to automatically evaluate different machine learning models.")

# --- Sidebar for File Uploads ---
st.sidebar.header("1. Upload Data")
geno_file = st.sidebar.file_uploader("Upload Genotype CSV", type=["csv"])
pheno_file = st.sidebar.file_uploader("Upload Phenotype CSV", type=["csv"])

if geno_file is not None and pheno_file is not None:
    # Load data into memory
    dfGeno = pd.read_csv(geno_file)
    dfPheno = pd.read_csv(pheno_file)
    
    st.sidebar.success("Data loaded successfully!")
    
    # --- Sidebar for Parameters ---
    st.sidebar.header("2. Configure Prediction")
    
    # Dynamically grab environments and phenotype columns
    available_envs = dfPheno['env'].dropna().unique().tolist()
    available_phenos = [col for col in dfPheno.columns if col not in ['id', 'env']]
    
    selected_env = st.sidebar.selectbox("Select Environment", available_envs)
    selected_pheno = st.sidebar.selectbox("Select Phenotype Trait", available_phenos)
    
    selected_model = st.sidebar.selectbox(
        "Select Machine Learning Model", 
        ['Ridge', 'GBLUP', 'RandomForest', 'SVR_linear', 'SVR_rbf', 'NeuralNetwork']
    )
    
    selected_genetics = st.sidebar.radio(
        "Select Genetic Model",
        ['additive', 'dominance'],
        help="Additive: Uses raw dosage (0-4). Dominance: Any presence (>0) becomes 4."
    )
    
    run_button = st.sidebar.button("Run Prediction", type="primary")
    
    # --- Main Screen Execution ---
    if run_button:
        with st.spinner(f"Training {selected_model} model... This may take a moment."):
            try:
                # Run the prediction function
                r, p_val, rmse, fig = tetraploidMLPred(
                    dfGeno=dfGeno, 
                    dfPheno=dfPheno, 
                    env=selected_env, 
                    pheno_col=selected_pheno, 
                    model_type=selected_model, 
                    genetic_model=selected_genetics
                )
                
                # Display Results in columns
                col1, col2, col3 = st.columns(3)
                col1.metric("Pearson Correlation (r)", f"{r:.3f}")
                col2.metric("P-Value", f"{p_val:.4f}")
                col3.metric("RMSE", f"{rmse:.3f}")
                
                # Render the matplotlib figure in Streamlit
                st.pyplot(fig)
                
            except Exception as e:
                st.error(f"An error occurred during modeling: {str(e)}")

else:
    st.info("👈 Please upload both the Genotype and Phenotype CSV files in the sidebar to begin.")
