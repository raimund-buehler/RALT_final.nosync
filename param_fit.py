from scipy.optimize import minimize
import numpy as np
from negll import negll_RescorlaWagner
from scipy.stats import shapiro
from sklearn.model_selection import ParameterGrid


def logistic_transform(x):
    return 1 / (1 + np.exp(-x))

def negll_RescorlaWagner_transformed(params, c, r, lambda1, lambda2):
    num_stimuli = 4  # Assuming a fixed number of stimuli
    trials_per_stimulus = len(c) // num_stimuli
    total_negll = 0

    # Transform parameters and separate them for wins and losses
    alpha_win, theta_win, rho_win = logistic_transform(params[0]), params[1], logistic_transform(params[2])
    alpha_loss, theta_loss, rho_loss = logistic_transform(params[3]), params[4], logistic_transform(params[5])

    params_win = [alpha_win, theta_win, rho_win]
    params_loss = [alpha_loss, theta_loss, rho_loss]

    # Iterate over each segment corresponding to different stimuli
    for i in range(num_stimuli):
        start_idx = i * trials_per_stimulus
        end_idx = start_idx + trials_per_stimulus
        c_stim = c[start_idx:end_idx]
        r_stim = r[start_idx:end_idx]

        # Calculate NLL using the appropriate parameter set for each trial
        total_negll += negll_RescorlaWagner(params_win, params_loss, c_stim, r_stim)

    # Elastic Net regularization for all parameters
    elastic_net_reg = (lambda1 * np.sum(np.abs(params[:6])) + lambda2 * np.sum(params[:6]**2))

    return total_negll + elastic_net_reg

def param_fit(c, r, lambda1, lambda2):
    # Optimize with initial guesses and bounds for both wins and losses
    init_guess = np.random.normal(0, 1, 6)  # logistic params for both win and loss
    init_guess[1] = init_guess[4] = np.random.uniform(1, 50)  # theta untransformed for both win and loss
    result = minimize(
        negll_RescorlaWagner_transformed,
        init_guess,
        args=(c, r, lambda1, lambda2),
        method='TNC',
        bounds=((-10, 10), (1, 50), (-10, 10), (-10, 10), (1, 50), (-10, 10))
    )

    # Transform parameters back
    alpha_hat_win, theta_hat_win, rho_hat_win = logistic_transform(result.x[0]), result.x[1], logistic_transform(result.x[2])
    alpha_hat_loss, theta_hat_loss, rho_hat_loss = logistic_transform(result.x[3]), result.x[4], logistic_transform(result.x[5])
    BIC = 6 * np.log(len(c)) + 2 * result.fun  # Compute BIC considering 6 parameters

    return result.fun, (alpha_hat_win, theta_hat_win, rho_hat_win, alpha_hat_loss, theta_hat_loss, rho_hat_loss), BIC
