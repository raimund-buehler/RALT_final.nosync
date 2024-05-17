import os
import pandas as pd
import numpy as np

def calculate_reward(row):
    if pd.isna(row['Congr/Incongr']):
        return np.nan
    elif (row['CorrectAns'] == 'Yes' and row['Congr/Incongr'] == 'Congruent'):
        return 1
    elif (row['CorrectAns'] == 'No' and row['Congr/Incongr'] == 'Congruent'):
        return 0
    elif (row['CorrectAns'] == 'Yes' and row['Congr/Incongr'] == 'Incongruent'):
        return 0
    elif (row['CorrectAns'] == 'No' and row['Congr/Incongr'] == 'Incongruent'):
        return 1
    return np.nan  # Just in case there are unexpected values

def transform_data(file_path, output_directory):
    # Extract file name from file_path right at the beginning
    file_name = os.path.basename(file_path)
    participant_id = file_name[:6].lower()
    
    try:
        # Load raw data file
        df = pd.read_csv(file_path)
        
        # Assign participant ID based on filename
        df['participant'] = participant_id
        
        # Skip the first 12 rows to remove training trials
        df = df.iloc[12:]
        
        if 'Reward' not in df.columns:
            df['Reward'] = df.apply(calculate_reward, axis=1)

        # Process RunNumber, assuming 'cycles.thisN' exists and might have NaNs
        df['RunNumber'] = df['cycles.thisN'].bfill()
        df['BlockNumber'] = df['blocks.thisN'].bfill()
        df.drop(columns=['cycles.thisN'], inplace=True)
        df.drop(columns=['blocks.thisN'], inplace=True)
        
        # Select and rename the necessary columns
        transformed_df = df[['stim', 'ParticipantAnswer', 'CorrCat', 'Reward', 'CorrectAns', 'BlockType', 'participant', 'trials.thisN', 'RunNumber', 'BlockNumber']].copy()
        transformed_df.rename(columns={
            'participant': 'ParticipantID',
            'trials.thisN': 'TrialNumber',
            'CorrectAns': 'CorrectAns_participant',
            'CorrCat': 'high_prob_choice'
        }, inplace=True)

        # Recode CorrectAns to True/False
        transformed_df['CorrectAns_participant'] = transformed_df['CorrectAns_participant'].str.lower() == 'yes'


        transformed_df.dropna(subset="stim", inplace=True)

        # Convert numerical columns to integers, filling NaNs with 0
        num_cols = ['stim', 'TrialNumber', 'RunNumber', 'Reward']
        for col in num_cols:
            transformed_df[col] = pd.to_numeric(transformed_df[col], errors='coerce').astype('Int64')

        # Create a new file name for the transformed data
        new_file_name = f"{participant_id}.csv"
        # Save the transformed data to the specified output directory
        transformed_df.to_csv(os.path.join(output_directory, new_file_name), index=False)
        print(f"Successfully transformed {file_name} to {new_file_name}")
    except KeyError as e:
        print(f"Error processing {file_name}: Missing column {e}")

def process_directory(directory_path, output_directory):
    # List all CSV files in the directory
    csv_files = [f for f in os.listdir(directory_path) if f.endswith('.csv')]
    
    # Transform each file
    for file in csv_files:
        file_path = os.path.join(directory_path, file)
        transform_data(file_path, output_directory)
    print("Data transformation complete for all files.")

# Example usage:
directory_path = 'RL_parameter_fit/RALT_csv_raw'  # Directory containing your raw data CSVs
output_directory = 'RL_parameter_fit/RL_data'  # Directory where you want to save the transformed data

# Call the function to process all CSV files in the directory
process_directory(directory_path, output_directory)
