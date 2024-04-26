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

# Initialize Arrays
TrialN = np.array(range(trials))
# Stimulus = []
R = np.zeros(trials, dtype=int)

for x in range(trials):
    R[x] = np.random.choice([0, 1], p=[0.25, 0.75])

# Q-Value associated with choice option
Q_a = np.zeros(trials, dtype=float)
Q_a[0] = 0.5
Q_b = np.zeros(trials, dtype=float)
Q_b[0] = 0.5

# Reward received on trial
R_a = np.zeros(trials, dtype=float)
R_b = np.zeros(trials, dtype=float)

# Prediction Error
PE = np.zeros(trials, dtype=float)

#Probability of choosing option
P_a = np.zeros(trials, dtype=float)
P_b = np.zeros(trials, dtype=float)