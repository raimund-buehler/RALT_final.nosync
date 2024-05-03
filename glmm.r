library(tidyverse)
library(lmerTest)
library(glmmTMB)
library(effectsize)
library(DHARMa)
library(gamlss)
library(gamlss.dist)

# Load the data
merged_df <- read_csv("merged_df_rho_win_sig.csv")

#parameter columns
param_cols <- c("Alpha_Win", "Rho_Win", "Theta_Win", "Alpha_Loss", "Rho_Loss", "Theta_Loss")

#Theta parameter not that interesting and not significant
#param_cols <- c("Theta_Win", "Theta_Loss")

#directory to save plots
plot_dir <- "plots"

#set theme for ggplot
theme_set(theme_classic())

# Create a grid of histograms for each parameter, ordered in the same way as param_cols

histogram_grid <- merged_df %>%
    gather(key = "parameter", value = "value", param_cols) %>%
    mutate(parameter = factor(parameter, levels = param_cols)) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15, fill = "lightblue", color = "black") +
    facet_wrap(~parameter, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                strip.background = element_blank(),
                strip.text = element_text(face = "bold"),
                legend.position = "none")  # Center the plot title


histogram_grid

#save histogram_grid in plot_dir
ggsave(file.path(plot_dir, "histogram_grid.png"), histogram_grid)


#Wilcoxon test for each win vs loss parameter
for (param in c("Alpha", "Rho")) {
    win_col <- paste0(param, "_Win")
    loss_col <- paste0(param, "_Loss")
    test <- wilcox.test(merged_df[[win_col]], merged_df[[loss_col]])
    print(paste("Wilcoxon test for", param, "Win vs Loss"))
    print(test)
}

#Plot violin plot for social vs non-social blocks for each parameter
grid_plot <- merged_df %>%
    gather(key = "parameter", value = "value", param_cols) %>%
    mutate(parameter = factor(parameter, levels = param_cols)) %>%
    ggplot(aes(x = BlockType, y = value, fill = BlockType)) +
    geom_violin(trim = FALSE, scale = "width", width = 0.8, alpha = 0.8) +
    geom_boxplot(width = 0.2, fill = "white", color = "black", alpha = 0.8) +
    facet_wrap(~parameter, scales = "free") +
    ggtitle("Violin plot for Parameters") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                strip.background = element_blank(),
                strip.text = element_text(face = "bold"),
                legend.position = "top")

grid_plot
# Save grid_plot in plot_dir
ggsave(file.path(plot_dir, "violin_grid.png"), grid_plot)

#Grid plot for AQ_score vs each parameter
grid_plot <- merged_df %>%
    gather(key = "parameter", value = "value", param_cols) %>%
    mutate(parameter = factor(parameter, levels = param_cols)) %>%
    ggplot(aes(x = AQ_score, y = value)) +
    geom_point(position = position_jitter(width = 0.2, height = 0.2)) +
    geom_smooth(method = "lm") +
    facet_wrap(~parameter, scales = "free") +
    ggtitle("Scatter plot for AQ_score vs Parameters") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                strip.background = element_blank(),
                strip.text = element_text(face = "bold"),
                legend.position = "top")

grid_plot

# Save grid_plot in plot_dir
ggsave(file.path(plot_dir, "scatter_grid.png"), grid_plot)
#Plot scatter plot for AQ_score vs each parameter and regression line
for (param in param_cols) {
    plot <- ggplot(merged_df, aes(x = AQ_score, y = !!sym(param))) +
        geom_point() +
        geom_smooth(method = "lm") +
        ggtitle(paste("Scatter plot for", param))
    #save plot in plot_dir
    ggsave(file.path(plot_dir, "scatters", paste0(param, "_scatter.png")), plot)
}

#Function definition to create QQplot with GGplot2
create_qqplot <- function(model) {
    qqplot <- ggplot(data.frame(resid = resid(model)), aes(sample = resid)) +
        stat_qq() +
        stat_qq_line() +
        ggtitle("QQ Plot")
    return(qqplot)
}

#Fit LMER model for each parameter in merged_df
for (param in param_cols){
    formula <- as.formula(paste(param, "~ AQ_score + BlockType + (1 | participant_x)"))
    model <- lmer(formula, data = merged_df)
    print(paste("################################", param, "################################"))
    print(summary(model))

    #QQplot
    qqplot <- create_qqplot(model)
    #save qqplot in plot_dir
    #ggsave(file.path(plot_dir, "qqs", paste0(param, "_qqplot.png")), qqplot)
}

# Fit lm() model for Rho_Win for social and nonsocial blocks separately
model <- lm(Rho_Win ~ AQ_score, data = merged_df %>% filter(BlockType == "social"))
print(summary(model))

#non-social block
model <- lm(Rho_Win ~ AQ_score, data = merged_df %>% filter(BlockType == "nonsocial"))
print(summary(model))

####glmmTMB####
# Fit Beta glmmTMB model for each parameter in merged_df
model_list <- list()  # Create an empty list to store the models
for (param in param_cols) {
    formula <- as.formula(paste(param, "~ AQ_score + BlockType"))
    model <- glmmTMB(formula, 
                    data = merged_df,
                    family = beta_family(link = "logit"))
    model_list[[param]] <- model  # Save the model to the list with the parameter name as the key
    print(paste("################################", param, "################################"))
    print(summary(model))
}

summary(model_list$Rho_Win)

# Evaluate model assumptions using DHARMa for each model in model_list
for (param in param_cols) {
    model <- model_list[[param]]
    simulation_output <- simulateResiduals(model)
    test_output <- testResiduals(simulation_output)
    print(paste("DHARMa test for", param))
    print(test_output)

    plot_filename <- paste("DHARMA_plot_for_", param, ".png", sep = "")

    # Open a PNG device
    png(filename = plot_filename, width = 700, height = 700, res = 100)

    # Create the plot
    plot(simulation_output, main = paste("DHARMa plot for", param))

    # Close the device to save the plot to the file
    dev.off()
}

plot(simulation_output)

# Fit Beta glmmTMB model for Rho_Win for social and nonsocial blocks separately
model <- glmmTMB(Rho_Win ~ AQ_score, 
                  data = merged_df %>% filter(BlockType == "social"))
print(summary(model))

# Calculate residuals
residuals_df <- data.frame(Residuals = residuals(model),
                           Fitted = fitted(model))

#Residuals for Rho_Win in social block
qqplot <- create_qqplot(model)
qqplot

#histogram of residuals
histogram <- ggplot(residuals_df, aes(x = Residuals)) +
    geom_histogram(binwidth = 0.1) +
    ggtitle("Histogram of Residuals")
histogram

# Extracting coefficients
coefs <- coef(summary(model))$cond[, "Estimate"]

# Calculating Odds Ratios
odds_ratios <- exp(coefs)

odds_ratios


#nonsocial block
model <- glmmTMB(Rho_Win ~ AQ_score, 
                  data = merged_df %>% filter(BlockType == "nonsocial"), 
                  family = beta_family(link = "logit"))
print(summary(model))


####GAMLSS####

# Fit BE gamlss model for each parameter in merged_df
for (param in param_cols) {
    formula <- as.formula(paste(param, "~ AQ_score + BlockType"))
    model <- gamlss(formula, data = merged_df, family = BE())
    print(paste("################################", param, "################################"))
    print(summary(model))
}

model <- gamlss(response ~ predictors, family = BE(), data = data)



####INDEX ANALYSIS####

####Win - Loss####
#Plot correlation for AQ_score vs Rho_Index for each BlockType
for (block in c("social", "nonsocial")) {
    plot <- ggplot(merged_df %>% filter(BlockType == block), aes(x = AQ_score, y = Rho_Index)) +
        geom_point() +
        geom_smooth(method = "lm") +
        ggtitle(paste("Scatter plot for AQ_score vs Rho_Index in", block, "condition"))
    #save plot in plot_dir
    ggsave(file.path(plot_dir, paste0("Rho_Index_", block, "_scatter.png")), plot)
}

#Fit lmer model for Rho_Index with AQ_score and BlockType
model <- lmer(Rho_Index ~ AQ_score + BlockType + (1 | participant_x), data = merged_df)
print(summary(model))

#QQplot
qqplot <- create_qqplot(model)
ggsave(file.path(plot_dir, "Rho_Index_qqplot.png"), qqplot)


#Fit lm model for Rho_Index, but only in social condition
model <- lm(Rho_Index ~ AQ_score, data = merged_df %>% filter(BlockType == "social"))
print(summary(model))
#QQplot
create_qqplot(model)

#Fit lm model for Rho_Index, but only in non-social condition
model <- lm(Rho_Index ~ AQ_score, data = merged_df %>% filter(BlockType == "nonsocial"))
print(summary(model))

#QQplot
create_qqplot(model)


####Ambi_Index####
#Filter data so that each participant is unique, then plot correlation for AQ_score vs Ambi_Index_Win
unique_participants <- merged_df %>% distinct(participant_x, .keep_all = TRUE)

#index columns
index_cols <- c("Ambi_Index_Alpha_Win", "Ambi_Index_Alpha_Loss", "Ambi_Index_Rho_Win", "Ambi_Index_Rho_Loss")

#Plot correlation for AQ_score vs each Ambi_Index
for (index in index_cols) {
    plot <- ggplot(unique_participants, aes(x = AQ_score, y = !!sym(index))) +
        geom_point() +
        geom_smooth(method = "lm") +
        ggtitle(paste("Scatter plot for AQ_score vs", index))
    #save plot in plot_dir
    ggsave(file.path(plot_dir, paste0(index, "_scatter.png")), plot)
}

#Fit Linear model for AQ_score vs each Ambi_Index
for (index in index_cols) {
    formula <- as.formula(paste(index, "~ AQ_score"))
    model <- lm(formula, data = unique_participants)
    print(index)
    print(summary(model))
}
