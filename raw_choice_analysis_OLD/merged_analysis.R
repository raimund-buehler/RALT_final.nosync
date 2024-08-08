library(tidyverse)
library(lmerTest)
library(emmeans)
library(interactions)
library(DHARMa)
library(car)
library(ROCR)
library(sjPlot)
library(httpgd)

# set working directory to raw_choice_analysis
setwd("raw_choice_analysis")

# open graphics browser
hgd()
hgd_browse()

#### DATA IMPORT####
data_full <- read_csv("csv_merged/data_full.csv")

data_full <- data_full %>% arrange(Sub_ID, block_n, run_n, trial_n)

# Plots
data_plot <- read_csv("csv_merged/data_plot.csv")
# RL
data_RL <- read_csv("csv_merged/data_RL.csv")

#### DESCRIPTIVES####

# Age and AQ
data_full %>%
  distinct(participant, .keep_all = TRUE) %>%
  summarise(across(
    .cols = c(Age, AQ_score),
    .fns = c(mean, sd, min, max)
  ))

# Gender
table(distinct(data_full, participant, .keep_all = TRUE)$Gender)

#### PLOTS####

#### Learning in 3 groups####
data_plot_ind <-
  data_full %>%
  select(Sub_ID, AQ_score, AQ_group, BlockType, run_n, CorrectAns) %>%
  group_by(Sub_ID, BlockType, run_n) %>%
  mutate("mean_corr" = mean(CorrectAns, na.rm = TRUE)) %>%
  distinct(Sub_ID, BlockType, .keep_all = T) %>%
  arrange(Sub_ID, BlockType, run_n) %>%
  ungroup()

data_plot_group <-
  data_full %>%
  select(AQ_group, BlockType, run_n, CorrectAns) %>%
  group_by(AQ_group, BlockType, run_n) %>%
  mutate(
    "mean_corr" = mean(CorrectAns, na.rm = T),
    "se" = sd(CorrectAns, na.rm = TRUE) / sqrt(n())
  ) %>%
  distinct(AQ_group, BlockType, .keep_all = T) %>%
  arrange(AQ_group, BlockType, run_n) %>%
  ungroup()

learning_3_groups <-
  ggplot(data_plot_group, aes(x = as.integer(run_n) + 1, y = mean_corr, color = AQ_group)) +
  # geom_line(data = data_plot_ind, aes(group = Sub_ID), stat="smooth", method = "lm", formula = y ~ x,
  #           size = 0.5,
  #           linetype ="dashed",
  #           alpha = 0.5) +
  geom_line(size = 1.5) +
  # geom_smooth() +
  geom_hline(
    data = mean_correct_answers_rate,
    aes(yintercept = mean_correct_rate / 100, color = NULL), linetype = "dashed"
  ) +
  facet_wrap("BlockType") +
  # geom_errorbar(aes(ymin = mean_corr - se,
  #                   ymax = mean_corr + se),
  #               size = 1, width = 0.5,
  #               alpha = 0.5) +
  geom_ribbon(aes(ymin = mean_corr - se, ymax = mean_corr + se, fill = AQ_group), alpha = 0.1) +
  scale_x_continuous(breaks = 1:6) +
  labs(x = "Run", y = "% Correct Answers", color = "AQ Score", fill = "AQ Score", title = "Learning over Runs") +
  theme_classic() +
  theme(legend.position = "top")

learning_3_groups
ggsave("svg/learning_3_groups.svg", learning_3_groups)

#### Learning in 3 groups - over trials####

# grouping variable - med_split (2) or AQ_group (3)

data_plot_group_trial <-
  data_full %>%
  select(BlockType, AQ_group, trial_n, CorrectAns) %>%
  group_by(BlockType, AQ_group, trial_n) %>%
  mutate(
    "mean_corr" = mean(CorrectAns, na.rm = T),
    "se" = sd(CorrectAns, na.rm = TRUE) / sqrt(n())
  ) %>%
  distinct(BlockType, AQ_group, trial_n, .keep_all = T) %>%
  arrange(BlockType, AQ_group, trial_n) %>%
  ungroup()

mean_AQ_Block <-
  data_full %>%
  group_by(BlockType, AQ_group) %>%
  summarise(mean_overall = mean(CorrectAns, na.rm = T)) %>%
  ungroup()

learning_3_groups_trial <-
  ggplot(data_plot_group_trial, aes(x = as.integer(trial_n) + 1, y = mean_corr, color = AQ_group)) +
  geom_line(size = 1) +
  geom_smooth(method = "loess", se = T) +
  geom_hline(data = mean_AQ_Block, aes(yintercept = mean_overall, color = NULL), linetype = "dashed") +
  facet_grid(AQ_group ~ BlockType) +
  geom_ribbon(aes(ymin = mean_corr - se, ymax = mean_corr + se), alpha = 0.1) +
  labs(x = "Trial", y = "% Correct Answers", color = "AQ Score", fill = "AQ Score", title = "Learning over Trials") +
  theme_classic() +
  theme(legend.position = "top")

learning_3_groups_trial


#### Learning in 2 groups over trials using med_split ####
data_plot_med_split_trial <-
  data_full %>%
  select(BlockType, med_split, trial_n, CorrectAns) %>%
  group_by(BlockType, med_split, trial_n) %>%
  mutate(
    "mean_corr" = mean(CorrectAns, na.rm = T),
    "se" = sd(CorrectAns, na.rm = TRUE) / sqrt(n())
  ) %>%
  distinct(BlockType, med_split, trial_n, .keep_all = T) %>%
  arrange(BlockType, med_split, trial_n) %>%
  ungroup()

mean_med_split_Block <-
  data_full %>%
  group_by(BlockType, med_split) %>%
  summarise(mean_overall = mean(CorrectAns, na.rm = T)) %>%
  ungroup()

learning_2_groups_trial <-
  ggplot(data_plot_med_split_trial, aes(x = as.integer(trial_n) + 1, y = mean_corr, color = med_split)) +
  geom_line(size = 1) +
  geom_smooth(method = "loess", se = T) +
  geom_hline(data = mean_med_split_Block, aes(yintercept = mean_overall, color = NULL), linetype = "dashed") +
  facet_grid(med_split ~ BlockType) +
  geom_ribbon(aes(ymin = mean_corr - se, ymax = mean_corr + se), alpha = 0.1) +
  labs(x = "Trial", y = "% Correct Answers", color = "AQ Score", fill = "AQ Score", title = "Learning over Trials") +
  theme_classic() +
  theme(legend.position = "top")

learning_2_groups_trial


#### Histogram####
histogram <- distinct(data_full, participant, .keep_all = TRUE) %>%
  ggplot(aes(x = AQ_score, y = ..density..)) +
  geom_histogram(aes(fill = AQ_group), position = "identity", alpha = 0.4) +
  # Ensure y is mapped to density for accurate comparison
  geom_density(aes(y = ..density.. * 4), alpha = 0.5, size = 0.5, color = "grey", bw = 1) +
  # Added density line, adjust alpha and color as needed
  geom_vline(xintercept = 17, color = "red") +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12)) +
  labs(
    x = "AQ Score",
    y = "Count",
    title = "Histogram of AQ scores",
    fill = "AQ Group"
  ) + # Assuming AQ is the fill variable's name
  scale_fill_discrete(labels = c("Median", "Q1", "Q3")) +
  theme_classic()

histogram
ggsave("svg/histogram.svg", dpi = 700)




#### MODELS####
#### GLM####

data_full$AQ_score <- scale(data_full$AQ_score, scale = F)

AQ_full <- glmer(
  data = data_full,
  CorrectAns ~ AQ_score *
    BlockType + run_n +
    (1 + run_n + BlockType | participant),
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa")
)

summary(AQ_full)

Anova(AQ_full)

##### Model Comparison####
AQ_full_no_random_slopes <- glmer(
  data = data_full,
  CorrectAns ~ AQ_score *
    BlockType + run_n +
    (1 | participant),
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa")
)

anova(AQ_full, AQ_full_no_random_slopes)


#### Effect Sizes####
odds_ratios <- exp(fixef(AQ_full))
# Compute confidence intervals for fixed effects using Wald method
conf_intervals_fixed <- exp(confint(AQ_full, method = "Wald"))[4:8, ]
# Combine into a data frame for neat presentation
summary_table <- data.frame(
  Odds_Ratio = odds_ratios,
  CI_Lower = conf_intervals_fixed[, 1],
  CI_Upper = conf_intervals_fixed[, 2]
)
print(summary_table)




#### AQ and run Interaction in Social conditon --> not significant####
AQ_run <- glmer(
  data = data_full %>% filter(BlockType == "social"),
  CorrectAns ~ AQ_score * run_n +
    (1 + run_n | participant),
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa")
)

summary(AQ_run)
Anova(AQ_run)

plot_model(AQ_run,
  type = "eff",
  terms = c("run_n", "AQ_score [4.58, 9.52, 14.47, 19.41]"),
  title = "Marginal effect of interaction between AQ score and run number: social condition",
  legend.title = "conditional on: \nAQ score",
  axis.title = c("run number", "percentage of successful trials (%)")
)
#### Effect Sizes AQ_run####
odds_ratios <- exp(fixef(AQ_run))
# Compute confidence intervals for fixed effects using Wald method
conf_intervals_fixed <- exp(confint(AQ_run, method = "Wald"))[4:7, ]
# Combine into a data frame for neat presentation
summary_table <- data.frame(
  Odds_Ratio = odds_ratios,
  CI_Lower = conf_intervals_fixed[, 1],
  CI_Upper = conf_intervals_fixed[, 2]
)
print(summary_table %>% round(2))
#### Model Comparison AQ_run####
AQ_run_nr <- glmer(
  data = data_full %>% filter(BlockType == "social"),
  CorrectAns ~ AQ_score * run_n +
    (1 | participant),
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa")
)


anova(AQ_run, AQ_run_nr)
# Interaction

result <- summary(emtrends(AQ_full, pairwise ~ BlockType | run_n, var = "AQ_score"),
  infer = c(T, T)
)

#### INT table####
# Extract the emtrends part and calculate odds ratios
emtrends_df <- as.data.frame(result$emtrends)
emtrends_df$Odds.Ratio <- round(exp(emtrends_df$AQ_score.trend), 2)
emtrends_df$Lower.OR.CI <- round(exp(emtrends_df$asymp.LCL), 2)
emtrends_df$Upper.OR.CI <- round(exp(emtrends_df$asymp.UCL), 2)

# Prepare and format the data frame for output
output_df <- data.frame(
  BlockType = emtrends_df$BlockType,
  Odds.Ratio = emtrends_df$Odds.Ratio,
  Lower.OR.CI = emtrends_df$Lower.OR.CI,
  Upper.OR.CI = emtrends_df$Upper.OR.CI,
  Z.ratio = emtrends_df$z.ratio,
  P.value = emtrends_df$p.value
)

# Format the table using knitr::kable
library(knitr)
kable(output_df,
  caption = "Odds Ratios for Trends of AQ Scores by Block Type",
  col.names = c("Block Type", "Odds Ratio", "Lower CI", "Upper CI", "Z-ratio", "P-value"),
  align = "c"
)

#### PLOT####
interact_plot <- interact_plot(AQ_full,
  pred = AQ_score, modx = BlockType, interval = T,
  legend.main = "Block Type", x = "AQ", y = "%Correct"
) +
  theme_classic()

interact_plot
ggsave("svg/interact_plot.svg")

#### ASSUMPTIONS####
# Distribution of random effects
# slopes
ranef(AQ_full)[[1]][, 2] %>% hist()
# intercepts
ranef(AQ_full)[[1]][, 1] %>% hist()
# Overdispersion
testDispersion(AQ_full)

simulationOutput <- simulateResiduals(fittedModel = AQ_full)
plot(simulationOutput)

simulationOutput_grouped <- recalculateResiduals(simulationOutput,
  group = data_full$participant
)
plot(simulationOutput_grouped)

simulationOutput_by_run <- recalculateResiduals(simulationOutput,
  group = data_full$run_n
)
plot(simulationOutput_by_run)

# RL model
plot(alpha)
#### ROC####
#### ROC and AUC####

# calculate prediction and performance

pred <- prediction(
  predict(AQ_full, type = "response"),
  data_full$CorrectAns
)
perf <- performance(pred, "tpr", "fpr")

# plot ROC curve
plot(perf, xaxs = "i", yaxs = "i", lwd = 2, col = "blue")
abline(a = 0, b = 1)
legend(
  x = "topleft", legend = c("Model Performance", "Chance Level"),
  col = c("blue", "black"), lty = 1
)

# calculate AUC
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
