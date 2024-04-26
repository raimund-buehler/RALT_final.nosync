import pandas as pd
from scipy import stats
from thefuzz import process, fuzz

#Read and adjust data
df = pd.read_csv("all_participants_results.csv").rename(columns={'Participant_ID': 'participant'})
df["participant"] = df["participant"].str.lower()
df = df.sort_values(by="participant")
df["participant"].str.lower()

AQ = pd.read_csv("data_full.csv")[["participant", "AQ_score"]].drop_duplicates()

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
df[mask]

def test_param(param, test_type='t-test'):
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
    test_param(param, test_type = "t-test")
    test_param(param, test_type = "wilcoxon")
