import pandas as pd
import numpy as np
import os
from param_fit import param_fit
from histograms import hist
from sklearn.model_selection import ParameterGrid
from scipy.stats import shapiro

def process_data(file_path, lambda1, lambda2):
    # Read data
    df = pd.read_csv(file_path)

    # Recoding of Answer
    df["ParticipantAnswer"] = np.where(df["ParticipantAnswer"] == "A", 0, 1)

    model_fits = []

    # Process by BlockType
    for block_type in df['BlockType'].unique():
        block_data = df[df['BlockType'] == block_type]

        # Collect data for all 4 stimuli in the block
        choice_data = []
        reward_data = []

        for stim in block_data['stim'].unique():
            stim_data = block_data[block_data['stim'] == stim]
            choice_data.append(np.array(stim_data["ParticipantAnswer"]))
            reward_data.append(np.array(stim_data["Reward"]))

        # Concatenate arrays across all stimuli for the block type
        c = np.concatenate(choice_data)
        r = np.concatenate(reward_data)

        # Call param_fit function
        res_nll, params, BIC = param_fit(c, r, lambda1, lambda2)

        # Extract participant ID from file name
        participant_id = os.path.basename(file_path).split('.')[0]

        # Append results
        model_fits.append([participant_id, block_type, params[0], params[1], params[2], res_nll, BIC])

    return model_fits

# Specify the directory containing the data files
data_directory = "/Users/raimundbuehler/Documents/UNIDOCS/Papers/RALT/Analysis/Python_RL_model_code_gpt/RL_data"

# List all CSV files in the directory
files = [os.path.join(data_directory, file) for file in os.listdir(data_directory) if file.endswith('.csv')]

all_participants_results = []

# Elastic Net parameters
# (Fixed lambda values now automatically optimized)
lambda1 = 0.1  # L1 regularization strength
lambda2 = 0.1 # L2 regularization strength

for file in files:
    results = process_data(file, lambda1, lambda2)
    all_participants_results.extend(results)

# Convert results to DataFrame and save
columns = ["Participant_ID", "BlockType", "Alpha", "Theta", "Rho", "neg_ll", "BIC"]
all_participants_results = pd.DataFrame(all_participants_results, columns=columns)

# lambdas = list(ParameterGrid({'lambda1': np.linspace(0.01, 1, 10), 'lambda2': np.linspace(0.01, 1, 10)}))
# best_score = -float('inf')
# best_params = None
# best_lambda = None

# for lambda_dict in lambdas:
#     all_participants_results = []
            
#     lambda1 = float(lambda_dict['lambda1'])
#     lambda2 = float(lambda_dict['lambda2'])
#     # Process each file
#     for file in files:
#         results = process_data(file, lambda1, lambda2)
#         all_participants_results.extend(results)

#     # Convert results to DataFrame and save
#     columns = ["Participant_ID", "BlockType", "Alpha", "Theta", "Rho", "neg_ll", "BIC"]
#     final_results = pd.DataFrame(all_participants_results, columns=columns)

#     _, p_alpha = shapiro(final_results['Alpha'])
#     _, p_theta = shapiro(final_results['Theta'])
#     _, p_rho = shapiro(final_results['Rho'])

#     score = p_alpha + p_theta + p_rho

#     if score > best_score:
#         best_score = score
#         best_lambda = (lambda1, lambda2)
#         best_result = final_results
    
# print("best_lambda: ", best_lambda, "best_score: ", best_score)

all_participants_results.to_csv("all_participants_results.csv", index=False)

hist("/Users/raimundbuehler/Documents/UNIDOCS/Papers/RALT/Analysis/Python_RL_model_code_gpt/all_participants_results.csv")