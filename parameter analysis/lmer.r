library(tidyverse)
library(ggplot2)
library(lmerTest)
library(glmmTMB)
library(effectsize)
library(DHARMa)
library(emmeans)
library(patchwork)
library(car)
library(httpgd)

# Load the data
merged_df <- read_csv("merged_df_final.csv")

# parameter columns
param_cols <- c(
  "Alpha_Win", "Rho_Win", "Theta_Win",
  "Alpha_Loss", "Rho_Loss", "Theta_Loss"
)

# Theta parameter not that interesting and not significant

# directory to save plots
plot_dir <- "plots"

# set theme for ggplot
theme_set(theme_classic())

#### LMMS####

# Four lists to store models
# blocktype models
model_list_blocktype <- list()
# AQ_score models
model_list_AQ <- list() # nolint: object_name_linter.
# gender models
model_list_gender <- list()
# interaction models
model_list_inter <- list()

# Fit LMER model for each parameter in merged_df of varying complexity
for (param in param_cols) {
  # BlockType model
  formula_blocktype <- as.formula(paste(param, "~ BlockType + (1 | participant_x)"))
  model_blocktype <- lmer(formula_blocktype, data = merged_df)
  model_list_blocktype[[param]] <- model_blocktype
  # AQ_score model
  formula_AQ <- as.formula(paste(param, "~ scale(AQ_score) + BlockType + (1 | participant_x)"))
  model_AQ <- lmer(formula_AQ, data = merged_df) # nolint: object_name_linter.
  model_list_AQ[[param]] <- model_AQ # nolint: object_name_linter.
  # Gender model
  formula_gender <- as.formula(paste(param, "~ scale(AQ_score) + BlockType + Gender + (1 | participant_x)"))
  model_gender <- lmer(formula_gender, data = merged_df)
  model_list_gender[[param]] <- model_gender
  # Interaction model
  formula_inter <- as.formula(paste(param, "~ scale(AQ_score) * BlockType + Gender + (1 | participant_x)"))
  model_inter <- lmer(formula_inter, data = merged_df)
  model_list_inter[[param]] <- model_inter
}

# Compare models for each parameter using anova
for (param in param_cols) {
  model_blocktype <- model_list_blocktype[[param]]
  model_AQ <- model_list_AQ[[param]] # nolint: object_name_linter.
  model_gender <- model_list_gender[[param]]
  model_inter <- model_list_inter[[param]]
  print(paste("################################", param, "################################"))
  print(anova(model_blocktype, model_AQ, model_gender, model_inter))
}

# Display results from model_AQ
for (param in param_cols) {
  model_AQ <- model_list_AQ[[param]] # nolint: object_name_linter.
  print(paste("################################", param, "################################"))
  print(summary(model_AQ))
}


# calculate effect size for block type for alpha win and rho win
for (param in c("Alpha_Win", "Rho_Win")) {
  model <- model_list_blocktype[[param]]
  # save summary of model
  summary_df <- summary(model)
  print(summary(model))
  # extract t value
  t <- summary_df$coefficients[, "t value"][2]
  df <- summary_df$coefficients[, "df"][2]
  # calculate effect size
  print(paste("Effect size for", param))
  print(t_to_d(t, df))
}

# calculate effect size for AQ score for rho win and theta loss
for (param in c("Rho_Win", "Theta_Loss")) {
  model <- model_list_AQ[[param]]
  # save summary of model
  summary_df <- summary(model)
  print(summary(model))
  # extract t value
  t <- summary_df$coefficients[, "t value"][2]
  df <- summary_df$coefficients[, "df"][2]
  # calculate effect size
  print(paste("Effect size for", param))
  print(t_to_r(t, df))
}

#### TRENDS AND CONTRASTS####
# list for trends
trends_list <- list()
# list for contrasts
contrast_list <- list()

for (param in param_cols) {
  model <- model_list_inter[[param]]
  print(paste("################################", param, "################################"))
  trends <- emtrends(model, ~BlockType, var = "AQ_score", infer = c(TRUE, TRUE))
  # save trends in trends_list per model
  trends_list[[param]] <- trends
  print(trends)
  # contrast social vs nonsocial pairwise using emmeans
  contrast <- emmeans(model, pairwise ~ BlockType)
  # save means in means_list per model
  contrast_list[[param]] <- contrast
  print(contrast)
}

# convert t to r for rho win trend
trends <- trends_list[["Rho_Win"]]
trends <-
  trends %>%
  as.tibble() %>%
  select(BlockType, AQ_score.trend, df, t.ratio, p.value)

# convert t value for AQ_score.trend of emtrends to r for rho win model
trends %>%
  mutate(r = t_to_r(t.ratio, df))

#### ASSUMPTIONS####
# Function definition to create QQplot with GGplot2
create_qqplot <- function(model, param) {
  qqplot <- ggplot(data.frame(resid = resid(model)), aes(sample = resid)) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(paste("QQ Plot for", param)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  return(qqplot)
}

# Create a grid of QQ plots for each model (no interaction models)
qqplot_grid <- purrr::map2(model_list_AQ, param_cols, create_qqplot) %>%
  patchwork::wrap_plots(nrow = 2, ncol = 3)

qqplot_grid

# Save the grid of QQ plots in the plot_dir
ggsave(file.path(plot_dir, "qqplot_grid.png"), qqplot_grid)

# further assumptions for lmms
# Check for homoscedasticity (no interaction models)
plot_resid_vs_fitted <- purrr::map2(model_list_AQ, param_cols, function(model, param) {
  ggplot(data.frame(fitted = fitted(model), resid = resid(model)), aes(x = fitted, y = resid)) +
    geom_point() +
    geom_smooth(method = "loess") +
    ggtitle(paste("Residuals vs Fitted for", param)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
})
plot_resid_vs_fitted_grid <- patchwork::wrap_plots(plotlist = plot_resid_vs_fitted, nrow = 2, ncol = 3)
plot_resid_vs_fitted_grid
# Save the grid of Residuals vs Fitted plots in the plot_dir
ggsave(file.path(plot_dir, "resid_vs_fitted_grid.png"), plot_resid_vs_fitted_grid)

# Check for multicollinearity using VIF (only for interaction models)
vif_list <- list()
for (param in param_cols) {
  model <- model_list_inter[[param]]
  vif <- car::vif(model)
  vif_list[[param]] <- vif
  print(paste("VIF for", param))
  print(vif)
}
