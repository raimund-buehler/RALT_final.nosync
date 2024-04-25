import numpy as np
import pandas as pd
import random
from sklearn import preprocessing

# Set up Parameters
trials = 6
alpha = 0.1
eta = 2
beta = 10
phi = 0.25

# Initialize Arrays for looping
TrialN = []
Stimulus = []
Outcome = []

Pcar1wins = []
Pcar2wins = []

EUcar1 = []
EUcar2 = []

Rcar1wins = []
Rcar2wins = []


Pcar1chosen = []
Pcar2chosen = []
Pcar1chosen_Add = []

#Fill the arrays
for x in range(trials):
    # Use this if clause for volatile environments/changing probability inbetween trials
    # if x < trials/2:
    TrialN.append(x)
    Outcome.append(np.random.choice([0, 1], p=[0.25, 0.75]))
    # else:
    # Outcome.append(np.random.choice([0, 1], p=[0.8, 0.2]))
    Pcar1wins.append(0.5)
    Pcar2wins.append(0.5)

    Pcar1chosen.append(0.5)
    Pcar2chosen.append(0.5)
    Pcar1chosen_Add.append(0.5)

    # Keep Reward magnitude fixed by replacing "random.randrange(60, 80)" with a number, i.e. 80
    Rcar1wins.append(random.randrange(60, 80))
    Rcar2wins.append(random.randrange(120, 140))

PPE = np.zeros(trials)

EUcar1 = np.zeros(trials)
EUcar2 = np.zeros(trials)

#For Additive Selector:
DeltaProb = np.zeros(trials)
RewardNorm = np.zeros(trials)

#Calculate PPE, Probability of Win, Utility and Probability of choice for both cars
for t in range(trials):
    PPE[t] = Outcome[t] - Pcar1wins[t]

    if t < trials - 1:
        Pcar1wins[t + 1] = Pcar1wins[t] + alpha * PPE[t]

    Pcar2wins[t] = 1 - Pcar1wins[t]

    EUcar1[t] = max(min((eta * (Pcar1wins[t] - 0.5)) + 0.5, 1), 0) * Rcar1wins[t]
    EUcar2[t] = max(min((eta * (Pcar2wins[t] - 0.5)) + 0.5, 1), 0) * Rcar2wins[t]

    Pcar1chosen[t] = 1 / (1 + np.exp(-beta*(EUcar1[t]-EUcar2[t])))
    Pcar2chosen[t] = 1 - Pcar1chosen[t]

    #Calculate Additve Selector
    DeltaProb[t] = Pcar1wins[t] - Pcar2wins[t]
    RewardNorm[t] = Rcar1wins[t] - Rcar2wins[t]

RewardNorm = preprocessing.normalize([RewardNorm])[0]

for t in range(trials):
    Pcar1chosen_Add[t] = 1 / (1 + np.exp(-beta * (phi * DeltaProb[t]) + (1-phi) * RewardNorm[t]))

#set up dictionary
data = {"Trial": TrialN, "Outcome": Outcome, "PPE": PPE, "Pcar1wins": Pcar1wins, "Pcar2wins": Pcar2wins,
        "Rcar1wins": Rcar1wins, "Rcar2wins": Rcar2wins, "EUcar1": EUcar1, "EUcar2": EUcar2,
        "Pcar1chosen": Pcar1chosen, "Pcar2chosen": Pcar2chosen, "Pcar1chosen_Add": Pcar1chosen_Add}

#create pandas dataframe
df = pd.DataFrame(data, columns=["Trial", "Outcome", "PPE", "Pcar1wins", "Pcar2wins", "Rcar1wins", "Rcar2wins",
                                 "EUcar1", "EUcar2", "Pcar1chosen", "Pcar2chosen", "Pcar1chosen_Add"])

# Save dataframe to csv to use in R for plotting
# ADJUST PATH ACCORDING TO YOUR WHERE YOU WANT TO SAVE THE FILE!
df.to_csv(r'/Users/raimundbuehler/Documents/UNIDOCS/Journal Club Python/ReinforcementLearning.csv', index=False)

print(df)
