import numpy as np

def negll_RescorlaWagner(params_win, params_loss, c, r):
    """
    Calculate the negative log-likelihood for the Rescorla-Wagner model
    with separate parameters for wins and losses.

    :param params_win: Parameters to use when the outcome is a win [alpha_win, theta_win, rho_win].
    :param params_loss: Parameters to use when the outcome is a loss [alpha_loss, theta_loss, rho_loss].
    :param c: Choices made by the participant (0 or 1 indicating choice A or B).
    :param r: Outcomes (rewards), where 1 indicates a win and 0 indicates a loss.
    :return: Total negative log-likelihood.
    """
    Q = [0.5] * 2  # Initialize Q values for choices A and B
    T = len(c)
    choiceProb = [] # List that only includes the choice probabilities for non-nan trials

    for t in range(T):

        if np.isnan(r[t]) or np.isnan(c[t]):
            continue

        if r[t] == 1:  # Win
            alpha, theta, rho = params_win
        else:  # Loss
            alpha, theta, rho = params_loss

        # Compute choice probabilities using softmax
        p0 = np.exp(theta * Q[0]) / (np.exp(theta * Q[0]) + np.exp(theta * Q[1]))
        p = [p0, 1 - p0]
        
        # Compute choice probability for the actual choice
        choiceProb.append(p[c[t]])

        # Update values
        delta = rho * r[t] - Q[c[t]]
        Q[c[t]] = Q[c[t]] + alpha * delta
    
    negLL = -np.sum(np.log(choiceProb))
    return negLL
