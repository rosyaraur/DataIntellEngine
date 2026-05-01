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

def tetraploidMLPred(dfGeno, dfPheno, 
                     env='Hancock20', 
                     pheno_col='vine.maturity', 
                     model_type='Ridge', 
                     genetic_model='additive'):
    """
    Predictive Machine Learning wrapper for Tetraploid Potato Genomic Data.
    
    Parameters:
    - dfGeno: Raw genotype pandas DataFrame.
    - dfPheno: Raw phenotype pandas DataFrame.
    - env: Environment to filter by (default 'Hancock20').
    - pheno_col: The phenotypic trait to predict (default 'vine.maturity').
    - model_type: 'RandomForest', 'SVR_linear', 'SVR_rbf', 'Ridge', 'GBLUP', or 'NeuralNetwork'.
    - genetic_model: 'additive' (0,1,2,3,4) or 'dominance' (>0 becomes 4).
    """
    
    print(f"\n--- Initializing {model_type} ({genetic_model} model) for trait: '{pheno_col}' ---")
    
    # 1. Filter Phenotype Data
    dfPheno_env = dfPheno[dfPheno['env'] == env].copy()
    dfPheno_env = dfPheno_env.set_index('id')
    
    # 2. Transpose Genotype Data
    dfGeno_T = dfGeno.T
    
    # 3. Find Intersecting Individuals
    common_rows = dfPheno_env.index.intersection(dfGeno_T.index)
    dfPheno_sub = dfPheno_env.loc[common_rows]
    dfGeno_sub = dfGeno_T.loc[common_rows]
    
    # Drop rows missing the target phenotype to avoid math errors
    dfPheno_sub = dfPheno_sub.dropna(subset=[pheno_col])
    common_rows = dfPheno_sub.index
    dfGeno_sub = dfGeno_sub.loc[common_rows]
    
    # Convert Genotype data strictly to numeric (avoids string/object errors)
    dfGeno_sub = dfGeno_sub.apply(pd.to_numeric, errors='coerce').fillna(0)
    
    # 4. Apply Genetic Model Transformation
    if genetic_model == 'dominance':
        # Anything greater than 0 becomes 4, else remains 0
        dfGeno_sub = pd.DataFrame(np.where(dfGeno_sub > 0, 4, 0), 
                                  index=dfGeno_sub.index, 
                                  columns=dfGeno_sub.columns)
    elif genetic_model != 'additive':
        raise ValueError("genetic_model must be strictly 'additive' or 'dominance'")
        
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
        print(f"Optimal Alpha Selected: {model.alpha_:.2f}")
        
    elif model_type == 'GBLUP':
        # Calculate Genomic Relationship Matrix (GRM)
        p = X_train.shape[1]
        GRM_train = np.dot(X_train, X_train.T) / p
        GRM_test = np.dot(X_test, X_train.T) / p
        
        alphas = np.logspace(-2, 3, 100)
        param_grid = {'alpha': alphas}
        model = GridSearchCV(KernelRidge(kernel='precomputed'), param_grid, cv=5)
        model.fit(GRM_train, y_train)
        y_pred = model.predict(GRM_test)
        print(f"Optimal Alpha Selected: {model.best_params_['alpha']:.2f}")
        
    elif model_type == 'NeuralNetwork':
        print("Tuning Neural Network architecture (this may take a moment)...")
        param_grid = {
            'hidden_layer_sizes': [(100,), (50, 50), (100, 50)],
            'alpha': [0.0001, 0.01, 0.1, 1.0],
            'max_iter': [500]
        }
        mlp = MLPRegressor(random_state=42, early_stopping=True)
        model = GridSearchCV(mlp, param_grid, cv=5, n_jobs=-1, scoring='neg_mean_squared_error')
        model.fit(X_train, y_train)
        y_pred = model.predict(X_test)
        print(f"Optimal Architecture: {model.best_estimator_.hidden_layer_sizes}")
        print(f"Optimal L2 Penalty (Alpha): {model.best_estimator_.alpha}")
        
    else:
        available = ['RandomForest', 'SVR_linear', 'SVR_rbf', 'Ridge', 'GBLUP', 'NeuralNetwork']
        raise ValueError(f"Unknown model_type. Choose from: {available}")
        
    # 7. Evaluate Metrics
    r, p_val = pearsonr(y_test, y_pred)
    mse = mean_squared_error(y_test, y_pred)
    rmse = np.sqrt(mse)
    
    print(f"RMSE: {rmse:.3f}")
    print(f"Pearson Correlation: {r:.3f} (p-value: {p_val:.3f})")
    
    # 8. Visualization
    plt.figure(figsize=(8, 5))
    plt.scatter(y_test, y_pred, color='indigo', edgecolor='k', zorder=2)
    
    # Plot a 'Perfect Prediction' line y=x
    min_val, max_val = min(y_test), max(y_test)
    plt.plot([min_val, max_val], [min_val, max_val], 'r--', alpha=0.7, label='Perfect Prediction Line')
    
    plt.title(f"{model_type} ({genetic_model.capitalize()}) - Predicting '{pheno_col}'")
    plt.xlabel(f"True Observed {pheno_col}")
    plt.ylabel(f"Predicted {pheno_col}")
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.5)
    plt.show()
    
    return model, r, rmse
