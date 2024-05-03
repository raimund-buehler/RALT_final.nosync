import pandas as pd
import numpy as np
import os
from param_fit import param_fit
from histograms import hist

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

        # Extract participant ID from file name
        participant_id = os.path.basename(file_path).split('.')[0]

        # Call param_fit function
        res_nll, (alpha_hat_win, theta_hat_win, rho_hat_win, alpha_hat_loss, theta_hat_loss, rho_hat_loss), BIC = param_fit(c, r, lambda1, lambda2)

        # Append results
        model_fits.append([participant_id, block_type, alpha_hat_win, theta_hat_win, rho_hat_win, alpha_hat_loss, theta_hat_loss, rho_hat_loss, res_nll, BIC])

    return model_fits

# Example for running the process_data function
data_directory = "/Users/raimundbuehler/Documents/UNIDOCS/Papers/RALT/Analysis/Python_RL_model_code_gpt/RL_data"
files = [os.path.join(data_directory, file) for file in os.listdir(data_directory) if file.endswith('.csv')]
all_participants_results = []

lambda1 = 0.1  # L1 regularization strength
lambda2 = 0.1  # L2 regularization strength

# Initialize a dictionary to store the best parameter values for each participant and block type
best_params = {}

# Iterate over each file
for file in files:
    # Process the data for the current file multiple times
    num_runs = 20  # Number of times to run the process_data function
    for _ in range(num_runs):
        results = process_data(file, lambda1, lambda2)
        
        # Find the parameters with the lowest negative log likelihood for each participant and block type
        for participant_result in results:
            participant_id = participant_result[0]
            block_type = participant_result[1]
            nll = participant_result[8]
            if (participant_id, block_type) not in best_params or nll < best_params[(participant_id, block_type)][8]:
                best_params[(participant_id, block_type)] = participant_result
    
    # Extend the results to the all_participants_results list
    all_participants_results.extend(results)

# Convert results to DataFrame and save
columns = ["Participant_ID", "BlockType", "Alpha_Win", "Theta_Win", "Rho_Win", "Alpha_Loss", "Theta_Loss", "Rho_Loss", "Neg_LL", "BIC"]
all_participants_results = pd.DataFrame(all_participants_results, columns=columns)

# Save the all_participants_results DataFrame to a CSV file
all_participants_results.to_csv("all_participants_results.csv", index=False)

# Create a DataFrame with the best parameter values for each participant and block type
best_params_df = pd.DataFrame(list(best_params.values()), columns=columns)

# Save the best_params_df DataFrame to a CSV file
best_params_df.to_csv("best_params_20.csv", index=False)

best_params_df.to_csv("best_params_20_l.csv", index=False)

# Optional: Generate histograms or other visualizations
hist("all_participants_results.csv")
