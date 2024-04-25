from scipy.optimize import minimize
import numpy as np
from negll import negll_RescorlaWagner
from scipy.stats import shapiro
from sklearn.model_selection import ParameterGrid


def logistic_transform(x):
    return 1 / (1 + np.exp(-x))

def negll_RescorlaWagner_transformed(params, c, r, lambda1, lambda2):
    # Assuming params include the number of stimuli and trials per stimulus
    num_stimuli = 4  # fixed number as per your task description
    trials_per_stimulus = len(c) // num_stimuli
    total_negll = 0

    # Process each stimulus' data
    for i in range(num_stimuli):
        start_idx = i * trials_per_stimulus
        end_idx = start_idx + trials_per_stimulus
        c_stim = c[start_idx:end_idx]
        r_stim = r[start_idx:end_idx]
        alpha, theta, rho = logistic_transform(params[0]), params[1], logistic_transform(params[2])
        total_negll += negll_RescorlaWagner([alpha, theta, rho], c_stim, r_stim)

    # Elastic Net parameters
    #(Fixed lambda values now automatically optimized)
    #lambda1 = 0.1  # L1 regularization strength
    #lambda2 = 0.025  # L2 regularization strength
    # Add Elastic Net regularization
    elastic_net_reg = (lambda1 * (np.abs(params[0]) + np.abs(params[2])) + 
                       lambda2 * (params[0]**2 + params[2]**2))

    return total_negll  + elastic_net_reg

def param_fit(c, r, lambda1, lambda2):
    # Optimize with initial guesses and bounds
    init_guess = np.random.normal(0, 1, 3)  # logistic params
    init_guess[1] = np.random.uniform(1, 50)  # theta untransformed
    result = minimize(
        negll_RescorlaWagner_transformed,
        init_guess,
        args=(c, r, lambda1, lambda2),
        method='TNC',
        bounds=((-10, 10), (1, 50), (-10, 10))
    )

    # Transform parameters back
    alpha_hat, theta_hat, rho_hat = logistic_transform(result.x[0]), result.x[1], logistic_transform(result.x[2])
    BIC = 3 * np.log(len(c)) + 2 * result.fun  # Compute BIC
    return result.fun, (alpha_hat, theta_hat, rho_hat), BIC
