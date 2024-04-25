import numpy as np

def negll_RescorlaWagner(params, c, r):
    alpha, theta, rho = params
    Q = [0.5] * 2  # Initialize Q values for choices A and B
    T = len(c)
    choiceProb = np.zeros((T), dtype=float)

    for t in range(T):
        # Compute choice probabilities using softmax
        p0 = np.exp(theta * Q[0]) / (np.exp(theta * Q[0]) + np.exp(theta * Q[1]))
        p = [p0, 1 - p0]
        
        # Compute choice probability for the actual choice
        choiceProb[t] = p[c[t]]

        # Update values
        delta = rho * r[t] - Q[c[t]]
        Q[c[t]] = Q[c[t]] + alpha * delta

    negLL = -np.sum(np.log(choiceProb))
    return negLL
