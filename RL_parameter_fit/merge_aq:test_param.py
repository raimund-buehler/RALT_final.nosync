import pandas as pd
import numpy as np
from scipy import stats
import matplotlib.pyplot as plt
import statsmodels.formula.api as smf
from thefuzz import process, fuzz

#Read and adjust data
df = pd.read_csv("RL_parameter_fit/best_params_20_l.csv").rename(columns={'Participant_ID': 'participant'})
df["participant"] = df["participant"].str.lower()
df = df.sort_values(by="participant")
df["participant"].str.lower()

AQ = pd.read_csv("data_full.csv")[["participant", "AQ_score", "Gender"]].drop_duplicates()

#Matching Function for Merging Data
def get_closest_match(x, choices, scorer, cutoff):
    match = process.extractOne(x, choices, scorer=scorer, score_cutoff=cutoff)
    return match[0] if match else None

df['MatchedID'] = df['participant'].apply(
    lambda x: get_closest_match(x, AQ['participant'], scorer= fuzz.ratio, cutoff=80)
)

merged_df = pd.merge(AQ, df, left_on='participant', right_on='MatchedID', how='inner')

AQ["participant"].nunique()
AQ["participant"].unique()

df["participant"].nunique()
df["participant"].unique()

merged_df["MatchedID"].nunique()
merged_df["MatchedID"].unique()

mask = ~AQ['participant'].isin(df['participant'])
#unique to AQ
AQ[mask]

mask = ~df['participant'].isin(AQ['participant'])
df[mask].nunique()

# create Index of Rho_Win - Rho_Loss in the social and nonsocial blocks
merged_df["Rho_Index"] = merged_df["Rho_Win"] - merged_df["Rho_Loss"]

# function definition for creating Ambi_Index for Alpha and Rho
def create_ambi_index(param, df: pd.DataFrame):
    ambi_index = df.loc[df["BlockType"] == "nonsocial", param].values - df.loc[df["BlockType"] == "social", param].values
    return np.repeat(ambi_index, 2)

# create ambi_index for Alpha
merged_df["Ambi_Index_Alpha_Win"] = create_ambi_index("Alpha_Win", merged_df)
merged_df["Ambi_Index_Alpha_Loss"] = create_ambi_index("Alpha_Loss", merged_df)

# create ambi_index for Rho
merged_df["Ambi_Index_Rho_Win"] = create_ambi_index("Rho_Win", merged_df)
merged_df["Ambi_Index_Rho_Loss"] = create_ambi_index("Rho_Loss", merged_df)

#drop participant and MatchedID columns from merged_df
merged_df = merged_df.drop(columns=["participant", "MatchedID"])

#print all ambi_index columns
print(merged_df[["Ambi_Index_Alpha_Win", "Ambi_Index_Alpha_Loss", "Ambi_Index_Rho_Win", "Ambi_Index_Rho_Loss"]])

merged_df.to_csv("merged_df.csv", index=False)

def t_test_param(param, df: pd.DataFrame, test_type='t-test'):
    # Function mapping
    test_functions = {
        't-test': stats.ttest_rel,
        'mann-whitney': lambda s, ns: stats.mannwhitneyu(s, ns, alternative='two-sided'),
        'wilcoxon': stats.wilcoxon
    }

    # Get the series for each group
    ns = df[df['BlockType'] == 'nonsocial'][param]
    s = df[df['BlockType'] == 'social'][param]

    # Retrieve the appropriate test function and execute it
    if test_type in test_functions:
        test_func = test_functions[test_type]
        test_result = test_func(s, ns)
        return param, test_result
    else:
        raise ValueError("Invalid test type specified. Choose 't-test', 'mann-whitney', or 'wilcoxon'.")

params = [ "Alpha_Win", "Theta_Win", "Rho_Win", "Alpha_Loss", "Theta_Loss", "Rho_Loss"]

for param in params:
    print(t_test_param(param, merged_df, test_type = "t-test"))
    print(t_test_param(param, merged_df, test_type = "wilcoxon"))

def residuals_plot(result):
    residuals = result.resid

    # Histogram
    plt.hist(residuals)
    plt.show()

    # Q-Q plot
    stats.probplot(residuals, plot=plt)
    plt.show()

    # Shapiro-Wilk test
    shapiro_test = stats.shapiro(residuals)
    print(f"Shapiro test statistic: {shapiro_test.statistic}, p-value: {shapiro_test.pvalue}")

# correlate parameters with AQ scores with block type as a factor
for param in params:
    model = smf.ols(f'{param} ~ AQ_score * C(BlockType)', data=merged_df)
    result = model.fit()
    print(result.summary())
    # Calculate residuals
    residuals_plot(result)

# correlate the Index of Rho_Win - Rho_Loss with AQ scores with block type as a factor
model = smf.ols('Rho_Index ~ AQ_score * C(BlockType)', data=merged_df)
print(model.fit().summary())
residuals_plot(model.fit())


#Spearmans Correlation of AQ Score with Rho Index
rho, p = stats.spearmanr(merged_df["AQ_score"], merged_df["Rho_Index"])
print(f"Spearman's correlation coefficient: {rho}, p-value: {p}")