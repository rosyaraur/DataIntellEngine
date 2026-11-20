import pandas as pd
import numpy as np

# Set seed for reproducibility
np.random.seed(42)
n = 200

# Base demographics and groupings
data = {
    'patient_id': range(1, n + 1),
    'treatment': np.random.choice(['Drug_A', 'Drug_B', 'Placebo'], n),
    'gender': np.random.choice(['M', 'F'], n),
    'age': np.random.normal(50, 10, n).round(0),
    'clinic_site': np.random.choice(['Site_1', 'Site_2', 'Site_3', 'Site_4'], n),
    'biomarker_1': np.random.normal(100, 15, n),
}
df = pd.DataFrame(data)

# Add correlated variables for PCA / Correlation Matrix
df['biomarker_2'] = df['biomarker_1'] * 0.5 + np.random.normal(0, 10, n)
df['biomarker_3'] = df['biomarker_1'] * -0.3 + np.random.normal(50, 5, n)

# Define effects for modeling
treatment_effect = df['treatment'].map({'Drug_A': 10, 'Drug_B': 5, 'Placebo': 0})
site_effect = df['clinic_site'].map({'Site_1': 2, 'Site_2': -2, 'Site_3': 5, 'Site_4': -5})

# Continuous Response (For T-test, ANOVA, Linear & Mixed Models)
df['bp_reduction'] = 5 + treatment_effect + site_effect + df['age']*0.1 + np.random.normal(0, 3, n)

# Binary Outcome (For Logistic Regression, Chi-Square)
logit = -5 + 0.05 * df['age'] + 0.02 * df['biomarker_1']
prob = 1 / (1 + np.exp(-logit))
df['adverse_event'] = np.random.binomial(1, prob)

# Time-to-Event Data (For Kaplan-Meier Survival)
hazard = np.where(df['treatment'] == 'Placebo', 0.1, 0.05)
df['survival_time'] = np.random.exponential(1 / hazard).round(1)
df['survival_status'] = np.random.binomial(1, 0.7, n) # 70% observed events (1), 30% censored (0)

# Save to CSV
output_file = "clinical_trial_data.csv"
df.to_csv(output_file, index=False)
print(f"Successfully generated {n} rows of complex clinical data in '{output_file}'")
