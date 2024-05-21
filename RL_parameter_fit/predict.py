import numpy as np
import pandas as pd
import os

#function to predict the choices made by the participant
def predict_choice(params_win, params_loss, c, r):
    """
    Predict the choices made by the participant using the Rescorla-Wagner model
    with separate parameters for wins and losses.

    :param params_win: Parameters to use when the outcome is a win [alpha_win, theta_win, rho_win].
    :param params_loss: Parameters to use when the outcome is a loss [alpha_loss, theta_loss, rho_loss].
    :param c: Choices made by the participant (0 or 1 indicating choice A or B).
    :param r: Outcomes (rewards), where 1 indicates a win and 0 indicates a loss.
    :return: Predicted choices.
    """
    Q = [0.5] * 2  # Initialize Q values for choices A and B
    T = len(c)
    predicted_choices = []

    for t in range(T):
        if np.isnan(r[t]) or np.isnan(c[t]):
            predicted_choices.append(np.nan)
            continue

        if r[t] == 1:  # Win
            alpha, theta, rho = params_win
        else:  # Loss
            alpha, theta, rho = params_loss

        # Compute choice probabilities using softmax
        p0 = np.exp(theta * Q[0]) / (np.exp(theta * Q[0]) + np.exp(theta * Q[1]))
        p = [p0, 1 - p0]

        # Predict the choice with the highest probability
        predicted_choices.append(np.argmax(p))

        # Update values
        delta = rho * r[t] - Q[c[t]]
        Q[c[t]] = Q[c[t]] + alpha * delta

    return predicted_choices

#create processed_data function
def process_data(file_path, df_p):
    # Read data
    df = pd.read_csv(file_path)

    # Recoding of Answer and high_prob_choice
    df["ParticipantAnswer"] = np.where(df["ParticipantAnswer"] == "A", 0, 1)
    df["high_prob_choice"] = np.where(df["high_prob_choice"] == "A", 0, 1)

    participant_id = os.path.basename(file_path).split('.')[0]

    print(f"Processing data for participant {participant_id}...")

    # empty df to store all stims
    all_stims = pd.DataFrame()

    # Process by BlockType
    for block_type in df['BlockType'].unique():
        block_data = df[df['BlockType'] == block_type]

        try:
            #read parameters from the file/parameter analysis/merged_df_final.csv and block type in current loop
            params_win = df_p[(df_p['participant_x'] == participant_id) & (df_p['BlockType'] == block_type)][['Alpha_Win', 'Theta_Win', 'Rho_Win']].values[0]
            params_loss = df_p[(df_p['participant_x'] == participant_id) & (df_p['BlockType'] == block_type)][['Alpha_Loss', 'Theta_Loss', 'Rho_Loss']].values[0]


        except IndexError:
            continue

        for stim in block_data['stim'].unique():
            stim_data = block_data[block_data['stim'] == stim].copy()

            c = np.array(stim_data["ParticipantAnswer"])
            r = np.array(stim_data["Reward"])

            pred = predict_choice(params_win, params_loss, c, r)
            # Append pred as column to stim_data
            stim_data["PredictedChoice"] = pred

            #check if predicted choice is equal to high_prob_choice
            stim_data["CorrectPrediction"] = np.where(stim_data["PredictedChoice"] == stim_data["high_prob_choice"], 1, 0)

            # add AQ_score to stim_data from df_p (distinct for each participant)
            stim_data["AQ_score"] = df_p[df_p['participant_x'] == participant_id]['AQ_score'].values[0]

            # Append stim_data to all_stims
            all_stims = pd.concat([all_stims, stim_data])

    return all_stims

#read all participant data
data_directory = "RL_parameter_fit/RL_data"
files = [os.path.join(data_directory, file) for file in os.listdir(data_directory) if file.endswith('.csv')]


#read parameters from the file /parameter analysis/merged_df_final.csv
df_p = pd.read_csv('RL_parameter_fit/merged_df_final.csv')

#create empty dataframe to store all processed data
all_choices = pd.DataFrame()

#process all files
for file in files:
    choices = process_data(file, df_p)

    # add new trial number, consecutive for each stimulus
    try:
        choices['TrialNumber'] = choices.groupby('stim').cumcount() + 1
    except KeyError:
        continue

    # Append choices to all_data
    all_choices = pd.concat([all_choices, choices])

    # Save all_choices to a new CSV file
    all_choices.to_csv('RL_parameter_fit/all_choices.csv', index=False)

    # save to plot_choice_vs_predict/all_choices.csv
    all_choices.to_csv('plot_choice_vs_predict/all_choices.csv', index=False)

print("All data processed successfully.")

