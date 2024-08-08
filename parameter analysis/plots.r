library(tidyverse)
library(lmerTest)
library(glmmTMB)
library(effectsize)
library(httpgd)

# Set up the httpgd server
hgd()
hgd_browse()

# Load the data
merged_df <- read_csv("parameter analysis/merged_df_final.csv")

# parameter columns
param_cols <- c("Alpha_Win", "Rho_Win", "Theta_Win", "Alpha_Loss", "Rho_Loss", "Theta_Loss")

# Theta parameter not that interesting and not significant
# param_cols <- c("Theta_Win", "Theta_Loss")

# directory to save plots
plot_dir <- "./parameter analysis/plots/"

# set theme for ggplot
theme_set(theme_classic())

# Create a grid of histograms for each parameter, ordered in the same way as param_cols
histogram_grid <- merged_df %>%
    gather(key = "parameter", value = "value", param_cols) %>%
    mutate(parameter = factor(parameter, levels = param_cols)) %>%
    group_by(parameter) %>%
    mutate(
        median = median(value),
        mean = mean(value)
    ) %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15, fill = "lightblue", color = "black") +
    geom_vline(aes(xintercept = median), color = "red", linetype = "dashed", size = 1) +
    # geom_vline(aes(xintercept = mean), color = "blue", linetype = "dashed", size = 1) +
    facet_wrap(~parameter, scales = "free") +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "none"
    )

histogram_grid

# save histogram_grid as svg in plot_dir, square aspect ratio
ggsave(file.path(plot_dir, "histogram_grid.svg"), histogram_grid, width = 10, height = 10)

# Calculate mean and median for each parameter
for (param in param_cols) {
    mean_val <- mean(merged_df[[param]])
    mode_val <- as.numeric(density(merged_df[[param]])$x[which.max(density(merged_df[[param]])$y)])
    print(paste("Mean for", param))
    print(mean_val)
    print(paste("Median for", param))
    print(mode_val)
}

# Calculate skewness and kurtosis for each parameter
for (param in param_cols) {
    skewness_val <- skewness(merged_df[[param]])
    kurtosis_val <- kurtosis(merged_df[[param]])
    print(paste("Skewness for", param))
    print(skewness_val)
    print(paste("Kurtosis for", param))
    print(kurtosis_val)
}


# Wilcoxon test for each win vs loss parameter
for (param in c("Alpha", "Rho", "Theta")) {
    win_col <- paste0(param, "_Win")
    loss_col <- paste0(param, "_Loss")
    test <- wilcox.test(merged_df[[win_col]], merged_df[[loss_col]])
    print(paste("Wilcoxon test for", param, "Win vs Loss"))
    print(test)
}

# shapiro test for each parameter
for (param in param_cols) {
    test <- shapiro.test(merged_df[[param]])
    print(paste("Shapiro test for", param))
    print(test)
}

# Wilcoxon test for each parameter between social and non-social blocks
for (param in param_cols) {
    social <- merged_df %>%
        filter(BlockType == "social") %>%
        pull(param)
    non_social <- merged_df %>%
        filter(BlockType == "nonsocial") %>%
        pull(param)
    test <- wilcox.test(social, non_social)
    print(paste("Wilcoxon test for", param, "Social vs Non-social"))
    print(test)
}

# t-test for each parameter between social and non-social blocks
for (param in param_cols) {
    social <- merged_df %>%
        filter(BlockType == "social") %>%
        pull(param)
    non_social <- merged_df %>%
        filter(BlockType == "nonsocial") %>%
        pull(param)
    test <- t.test(social, non_social)
    print(paste("T-test for", param, "Social vs Non-social"))
    print(test)
}

# betaGLMM for each parameter vs BlockType
model_list <- list() # Create an empty list to store the models
param_cols <- c("Alpha_Win", "Rho_Win", "Alpha_Loss", "Rho_Loss")
for (param in param_cols) {
    formula <- as.formula(paste(param, "~ BlockType + (1 | participant_x)"))
    model <- glmmTMB(formula,
        data = merged_df,
        family = beta_family(link = "logit")
    )
    model_list[[param]] <- model # Save the model to the list with the parameter name as the key
    print(paste("################################", param, "################################"))
    print(summary(model))
}

model <- model_list[["Rho_Win"]]
summary(model)
exp(confint(model)) %>% round(2)

model <- model_list[["Alpha_Win"]]
summary(model)
exp(confint(model)) %>% round(2)

# lmer for theta
model_list <- list() # Create an empty list to store the models
param_cols <- c("Theta_Win", "Theta_Loss")
for (param in param_cols) {
    formula <- as.formula(paste(param, "~ BlockType + (1 | participant_x)"))
    model <- lmer(formula,
        data = merged_df
    )
    model_list[[param]] <- model # Save the model to the list with the parameter name as the key
    print(paste("################################", param, "################################"))
    print(summary(model))
}

# Plot violin plot for social vs non-social blocks for each parameter
grid_plot_violin <- merged_df %>%
    gather(key = "parameter", value = "value", param_cols) %>%
    mutate(parameter = factor(parameter, levels = param_cols)) %>%
    ggplot(aes(x = BlockType, y = value, fill = BlockType)) +
    geom_violin(trim = FALSE, scale = "width", width = 0.8, alpha = 0.8) +
    geom_boxplot(width = 0.2, fill = "white", color = "black", alpha = 0.8) +
    facet_wrap(~parameter, scales = "free") +
    ggtitle("Violin plot for Parameters") +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "top"
    )

grid_plot_violin

# save grid_plot as svg in plot_dir, sqaure aspect ratio
ggsave(file.path(plot_dir, "violin_grid.svg"), grid_plot_violin, width = 10, height = 10)

# Grid plot for AQ_score vs each parameter
grid_plot_scatter <- merged_df %>%
    gather(key = "parameter", value = "value", param_cols) %>%
    mutate(parameter = factor(parameter, levels = param_cols)) %>%
    ggplot(aes(x = AQ_score, y = value)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm") +
    facet_wrap(~parameter, scales = "free") +
    ggtitle("Scatter plot for AQ_score vs Parameters") +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "top",
        aspect.ratio = 1
    )

grid_plot_scatter

# save grid_plot as svg in plot_dir, square aspect ratio
ggsave(file.path(plot_dir, "scatter_grid.svg"), grid_plot_scatter, width = 10, height = 10)

# Save grid_plot in plot_dir
ggsave(file.path(plot_dir, "scatter_grid.png"), grid_plot_scatter)

# Scatter plot for Rho_win for social vs non-social blocks
scatter_plot <- merged_df %>%
    ggplot(aes(x = AQ_score, y = Rho_Win, color = BlockType)) +
    geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
    geom_smooth(method = "lm") +
    ggtitle("Scatter plot for Rho_Win vs AQ_score") +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "top",
        aspect.ratio = 1
    )

# save scatter_plot as svg in plot_dir, square aspect ratio
ggsave(file.path(plot_dir, "scatter_plot.svg"), scatter_plot, width = 10, height = 10)

# save scatter_plot in plot_dir
ggsave(file.path(plot_dir, "scatter_plot.png"), scatter_plot)
