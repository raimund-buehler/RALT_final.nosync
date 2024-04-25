import pandas as pd
import numpy as np
import os

from scipy.optimize import minimize
from scipy.stats import shapiro, norm
from sklearn.model_selection import ParameterGrid
from negll import negll_RescorlaWagner
from histograms import hist

def logistic_transform(x):
    return 1 / (1 + np.exp(-x))

def negll_RescorlaWagner_transformed(params, c, r, lambda1, lambda2):
    num_stimuli = 4
    trials_per_stimulus = len(c) // num_stimuli
    total_negll = 0

    for i in range(num_stimuli):
        start_idx = i * trials_per_stimulus
        end_idx = start_idx + trials_per_stimulus
        c_stim = c[start_idx:end_idx]
        r_stim = r[start_idx:end_idx]
        alpha = logistic_transform(float(params[0])); theta = float(params[1]); rho = logistic_transform(float(params[2])); params = [float(p) for p in params]
        total_negll += negll_RescorlaWagner([alpha, theta, rho], c_stim, r_stim)

    elastic_net_reg = (lambda1 * (np.abs(params[0]) + np.abs(params[2])) + 
                       lambda2 * (params[0]**2 + params[2]**2))
    return total_negll + elastic_net_reg

def param_fit(c, r):
    lambdas = list(ParameterGrid({'lambda1': np.linspace(0.01, 1, 10), 'lambda2': np.linspace(0.01, 1, 10)}))
    best_score = float('inf')
    best_params = None
    best_lambda = None

    for lambda_dict in lambdas:
        lambda1 = float(lambda_dict['lambda1'])
        lambda2 = float(lambda_dict['lambda2'])

        # Ensure initial guesses are within the bounds
        initial_guess = np.array([np.random.normal(0, 1), np.random.uniform(1, 50), np.random.normal(0, 1)])
        initial_guess[0] = np.clip(initial_guess[0], -10, 10)  # Clipping alpha and rho within [-10, 10]
        initial_guess[2] = np.clip(initial_guess[2], -10, 10)
        initial_guess[1] = np.clip(initial_guess[1], 1, 50)   # Clipping theta within [1, 50]

        result = minimize(
            negll_RescorlaWagner_transformed,
            initial_guess,
            args=(c, r, lambda1, lambda2),
            method='TNC',
            bounds=((-10, 10), (1, 50), (-10, 10))
        )
        alpha_hat, theta_hat, rho_hat = logistic_transform(result.x[0]), result.x[1], logistic_transform(result.x[2])
        p_alpha, p_theta, p_rho = shapiro([alpha_hat]), shapiro([theta_hat]), shapiro([rho_hat])
        score = -(p_alpha[1] + p_theta[1] + p_rho[1])

        if score < best_score:
            best_score = score
            best_params = (alpha_hat, theta_hat, rho_hat)
            best_lambda = (lambda1, lambda2)

    BIC = 3 * np.log(len(c)) + 2 * result.fun
    return result.fun, best_params, BIC, best_lambda


def process_data(file_path):
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
        res_nll, params, BIC = param_fit(c, r)

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

# Process each file
for file in files:
    results = process_data(file)
    all_participants_results.extend(results)

# Convert results to DataFrame and save
columns = ["Participant_ID", "BlockType", "Alpha", "Theta", "Rho", "neg_ll", "BIC"]
final_results = pd.DataFrame(all_participants_results, columns=columns)
final_results.to_csv("all_participants_results.csv", index=False)

hist("/Users/raimundbuehler/Documents/UNIDOCS/Papers/RALT/Analysis/Python_RL_model_code_gpt/all_participants_results.csv")