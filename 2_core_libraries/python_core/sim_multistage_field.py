import numpy as np
import pandas as pd

def sim_multistage_field(
    batch_years, 
    n_crosses=100, 
    n_lines=5000, 
    n_env=10, 
    n_rep=3,
    var_components={'G': 0.30, 'GE': 0.25, 'e': 0.45},
    rho_within=(0.5, 0.9),
    var_between_prop=0.4
):
    """
    Simulates yield data with customizable genetic and environmental correlation structures.
    
    Parameters
    ----------
    batch_years : dict
        Maps batch names to a list of years they are tested in the field 
        (e.g., {'2014': [2018, 2019, 2020]}).
    n_crosses : int, default=100
        Number of distinct bi-parental families (crosses) generated per batch.
    n_lines : int, default=5000
        Total number of individual progeny lines tested across the entire batch. 
        Lines per cross is calculated as `n_lines // n_crosses`.
    n_env : int, default=10
        Number of distinct testing environments (locations) utilized per year.
    n_rep : int, default=3
        Number of replications (blocks) for each line within a specific year-location combo.
    var_components : dict, default={'G': 0.30, 'GE': 0.25, 'e': 0.45}
        Total variance components representing additive genetic variance ('G'), 
        genotype-by-environment variance ('GE'), and residual error ('e').
    rho_within : tuple (float, float), default=(0.5, 0.9)
        The (min, max) range for the intra-family genetic correlation. Controls the 
        amount of Mendelian segregation (within-family variance) for each cross.
    var_between_prop : float, default=0.4
        The proportion (0.0 to 1.0) of the total genetic variance ('G') that dictates 
        the variance between the different family means (across-family variance).
        
    Returns
    -------
    pd.DataFrame
        A simulated dataset containing columns: ['Batch', 'Year', 'Location', 'Rep', 
        'Cross', 'Line', 'Yield'].
    """
    all_batches = []
    lines_per_cross = n_lines // n_crosses
    
    # Pre-calculate IDs
    cross_ids = np.repeat(np.arange(n_crosses), lines_per_cross)
    line_ids = np.arange(n_lines)

    for batch_name, years in batch_years.items():
        print(f"Simulating Batch: {batch_name}...")
        
        # 1. Simulate Across-Family Variance (Cross Means)
        cross_var = var_components['G'] * var_between_prop
        cross_means = np.random.normal(0, np.sqrt(cross_var), n_crosses)
        
        # 2. Simulate Within-Family Variance based on correlation (rho)
        rho_crosses = np.random.uniform(rho_within[0], rho_within[1], n_crosses)
        covariances = var_components['G'] * rho_crosses
        
        G = np.zeros(n_lines)
        for i in range(n_crosses):
            idx = np.where(cross_ids == i)[0]
            within_var = max(0, var_components['G'] - covariances[i])
            G[idx] = cross_means[i] + np.random.normal(0, np.sqrt(within_var), len(idx))

        # 3. Simulate Environments and GxE
        records = []
        for year in years:
            year_effect = np.random.normal(0, 0.1) 
            
            for loc in range(n_env):
                loc_effect = np.random.normal(0, 0.2) + year_effect
                GE_eff = np.random.normal(0, np.sqrt(var_components['GE']), n_lines)
                
                for rep in range(n_rep):
                    epsilon = np.random.normal(0, np.sqrt(var_components['e']), n_lines)
                    yields = G + loc_effect + GE_eff + epsilon
                    
                    batch_data = pd.DataFrame({
                        'Batch': batch_name,
                        'Year': year,
                        'Location': loc,
                        'Rep': rep,
                        'Cross': cross_ids,
                        'Line': line_ids,
                        'Yield': yields
                    })
                    records.append(batch_data)
        
        all_batches.append(pd.concat(records, ignore_index=True))

    return pd.concat(all_batches, ignore_index=True)

# --- Example Execution ---
#trial_params = {
#    '2014': [2018, 2019, 2020],
#    '2015': [2019, 2020, 2021],
#    '2016': [2020, 2021, 2022]
#}

#df_all = sim_multistage_field(trial_params)

