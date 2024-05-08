library(tidyverse)
library(glmmTMB)
library(emmeans)
library(DHARMa)

# Load the data
merged_df <- read_csv("merged_df_final.csv")

# directory to save plots
plot_dir <- "plots"

# set theme for ggplot
theme_set(theme_classic())

#### glmmTMB####
# Fit Beta glmmTMB model for each parameter in merged_df

# param_cols without Theta_Win and Theta_Loss
param_cols <- c("Alpha_Win", "Rho_Win", "Alpha_Loss", "Rho_Loss")


# WITHOUT interaction
model_list <- list() # Create an empty list to store the models
for (param in param_cols) {
  formula <- as.formula(paste(param, "~ scale(AQ_score) + BlockType + (1 | participant_x)"))
  model <- glmmTMB(formula,
    data = merged_df,
    family = beta_family(link = "logit")
  )
  model_list[[param]] <- model # Save the model to the list with the parameter name as the key
  print(paste("################################", param, "################################"))
  print(summary(model))
}

# convert estimates to odds ratios with 95% CI for Rho_Win model
model <- model_list[["Rho_Win"]]
# confidence intervals
conf_ints <- confint(model, method = "Wald")
# Calculating 95% CI for Odds Ratios
odds_ratios_ci <- exp(conf_ints)

odds_ratios_ci %>% round(2)

print(summary(model))


# WITH interaction
model_list <- list() # Create an empty list to store the models
for (param in param_cols) {
  formula <- as.formula(paste(param, "~ scale(AQ_score) *
  BlockType + (1 | participant_x)"))
  model <- glmmTMB(formula,
    data = merged_df,
    family = beta_family(link = "logit")
  )
  model_list[[param]] <- model # Save the model to the list with the parameter name as the key
  print(paste("################################", param, "################################"))
  print(summary(model))
}

# emtrends for the effect of for rho_win model
model <- model_list[["Rho_Win"]]

# check interaction using emmeans for rho_win model
trends <- emtrends(model, ~BlockType, var = "AQ_score", infer = c(TRUE, TRUE))
trends <-
  trends %>%
  as.tibble() %>%
  select(BlockType, AQ_score.trend, asymp.LCL, asymp.UCL)

# convert AQ_score.trend of emtrends to odds ratios with 95% CI for Rho_Win model
trends %>%
  mutate_if(is.numeric, exp)




##### ASSUMPTIONS#####
# Evaluate model assumptions using DHARMa for each model in model_list
for (param in param_cols) {
  model <- model_list[[param]]
  simulation_output <- simulateResiduals(model)
  test_output <- testResiduals(simulation_output)
  print(paste("DHARMa test for", param))
  print(test_output)

  plot_filename <- paste("DHARMA_plot_for_", param, ".png", sep = "")

  # Open a PNG device to save the plot to a file
  png(file.path(plot_dir, plot_filename))

  # Create the plot
  plot(simulation_output, main = paste("DHARMa plot for", param))

  # Close the device to save the plot to the file
  dev.off()

  plot_filename <- paste("Dispersion_plot_for_", param, ".png", sep = "")

  # Open a PNG device to save the plot to a file
  png(file.path(plot_dir, plot_filename))
  testDispersion(simulation_output)

  dev.off()
}
