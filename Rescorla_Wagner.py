from scipy.optimize import minimize # finding optimal params in models
from scipy import stats
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt     # plotting

T = 100
K = 2
mu = [0.2, 0.8]

def simulate_RescorlaWagner(params, T, mu, noisy_choice=True):
    alpha, theta = params

    c = np.zeros((T), dtype=int)
    r = np.zeros((T), dtype=int)

    Q_stored = np.zeros((2, T), dtype=float)
    Q = [0.5, 0.5]

    for t in range(T):

        # store values for Q_{t+1}
        Q_stored[:, t] = Q

        # compute choice probabilities
        p0 = np.exp(theta * Q[0]) / (np.exp(theta * Q[0]) + np.exp(theta * Q[1]))
        p1 = 1 - p0

        #alternative choice rule
        pA = 1 / (1 + np.exp(-theta*(Q[0]-Q[1])))
        pB = 1 - pA

        # make choice according to choice probababilities
        # as weighted coin flip to make a choice
        # choose stim 0 if random number is in the [0 p0] interval
        # and 1 otherwise
        if noisy_choice:
            if np.random.random_sample(1) < p0:
                c[t] = 0
            else:
                c[t] = 1
        else:  # make choice without noise
            c[t] = np.argmax([p0, p1])

        # generate reward based on reward probability
        r[t] = np.random.rand() < mu[c[t]]

        # update values
        delta = r[t] - Q[c[t]]
        Q[c[t]] = Q[c[t]] + alpha * delta

    return c, r, Q_stored


def negll_RescorlaWagner(params, c, r):
    alpha, theta, rho = params

    Q = [0.5, 0.5]
    T = len(c)
    choiceProb = np.zeros((T), dtype=float)

    for t in range(T):
        # compute choice probabilities for k=2
        p0 = np.exp(theta * Q[0]) / (np.exp(theta * Q[0]) + np.exp(theta * Q[1]))
        p = [p0, 1 - p0]

        #alternative choice rule
        pA = 1 / (1 + np.exp(-theta * (Q[0] - Q[1])))
        pB = 1 - pA

        # compute choice probability for actual choice
        choiceProb[t] = p[c[t]]

        # update values
        delta = rho * r[t] - Q[c[t]]
        Q[c[t]] = Q[c[t]] + alpha * delta

    negLL = -np.sum(np.log(choiceProb))

    return negLL

alpha = 0.1
theta = 1.5
params = [alpha, theta]

c3, r3, Q = simulate_RescorlaWagner(params=params, T=T, mu=mu)

negll_RescorlaWagner(params, c3, r3)

#BRUTE FORCE
nLL = []
alpha_vals = np.linspace(0,0.5,1000)
for alpha_val in alpha_vals:
    nLL.append(negll_RescorlaWagner([alpha_val, theta], c3, r3))

# plt.plot(alpha_vals, nLL, '-')
# plt.plot(alpha_vals[np.argmin(nLL)], nLL[np.argmin(nLL)], 'X', label=r'optimal $\hat \alpha$')
# plt.ylabel('negative log likelihood')
# plt.xlabel(fr'learning rate, $\hat \alpha$')
# plt.title(f'Rescorla-Wagner Learning')
# plt.legend()
# plt.show()

res_nll = np.inf  # set initial neg LL to be inf

# guess several different starting points for alpha
for alpha_guess in np.linspace(0, 1, 10):
    for theta_guess in np.linspace(1, 25, 10):

        # guesses for alpha, theta will change on each loop
        init_guess = (alpha_guess, theta_guess)

        # minimize neg LL
        result = minimize(negll_RescorlaWagner,
                          init_guess,
                          (c3, r3),
                          bounds=((0, 1), (1, 50)))

        # if current negLL is smaller than the last negLL,
        # then store current data
        if result.fun < res_nll:
            res_nll = result.fun
            param_fits = result.x

# also, compute BIC
# note: we don't need the -1 because
# we already have the negative log likelihood!
BIC = len(init_guess) * np.log(len(c3)) + 2*res_nll

print(fr'alpha_hat = {param_fits[0]:.2f}, theta_hat = {param_fits[1]:.2f}')
print(fr'BIC = {BIC:.2f}')