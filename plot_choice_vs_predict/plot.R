library(tidyverse)
library(httpgd)

# Set up the httpgd server
hgd()
hgd_browse()

# Load data
all_choices <- read_csv("plot_choice_vs_predict/all_choices.csv")

# create AQ group column using ntile() (3 groups for AQ_score, Low, Medium, High)
all_choices <- all_choices %>%
  mutate(AQ_group = ntile(AQ_score, 3)) %>%
  mutate(AQ_group = case_when(
    AQ_group == 1 ~ "Low",
    AQ_group == 2 ~ "Medium",
    AQ_group == 3 ~ "High"
  )) %>%
  # adjust the order of AQ_group
  mutate(AQ_group = factor(AQ_group, levels = c("Low", "Medium", "High")))

# Med split for AQ_score using ntile()
all_choices <- all_choices %>%
  mutate(Med_Split = ntile(AQ_score, 2)) %>%
  mutate(Med_Split = case_when(
    Med_Split == 1 ~ "Low",
    Med_Split == 2 ~ "High"
  ))

# save csv
write_csv(all_choices, "plot_choice_vs_predict/all_choices_split.csv")

#### Histogram####
histogram <- distinct(all_choices, ParticipantID, .keep_all = TRUE) %>%
  ggplot(aes(x = AQ_score, y = ..density..)) +
  geom_histogram(aes(fill = AQ_group), position = "identity", alpha = 0.4) +
  # Ensure y is mapped to density for accurate comparison
  # geom_density(aes(y = ..density.. * 4), alpha = 0.5, size = 0.5, color = "grey", bw = 1) +
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
  theme_minimal()

histogram

# Compare block types
all_choices_blocks <- all_choices %>%
  group_by(ParticipantID, BlockType) %>%
  summarise(
    part_mean_ov = mean(CorrectAns_participant),
    pred_mean_ov = mean(CorrectPrediction),
    part_ov_se = sd(CorrectAns_participant) / sqrt(n()),
    pred_ov_se = sd(CorrectPrediction) / sqrt(n())
  ) %>%
  ungroup()

# plot part_mean between block types, violin plot for all_choices_blocks
ggplot(data = all_choices_blocks, aes(x = BlockType, y = part_mean_ov, fill = BlockType)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  labs(
    title = "Correct Answers between Block Types",
    x = "Block Type",
    y = "Mean Correct Answers"
  ) +
  theme_minimal()

#### Plot Overall####

# calculate overall means and standard errors for each BlockType and TrialNumber
all_choices_means_overall <- all_choices %>%
  group_by(BlockType, TrialNumber) %>%
  summarise(
    part_mean_ov = mean(CorrectAns_participant),
    pred_mean_ov = mean(CorrectPrediction),
    part_ov_se = sd(CorrectAns_participant) / sqrt(n()),
    pred_ov_se = sd(CorrectPrediction) / sqrt(n())
  ) %>%
  ungroup()

# plot the means over 12 trials for each BlockType, once for part_mean and once for pred_mean
# scatter plots for each participant in all_choices_means
# line plots for the overall means in all_choices_means_overall, with standard errors
ggplot(data = all_choices_means_overall, aes(x = as.factor(TrialNumber), y = part_mean, color = BlockType)) +
  # geom_boxplot() +
  geom_line(aes(y = part_mean_ov, group = BlockType, linetype = "Observed", color = BlockType)) +
  geom_line(aes(y = pred_mean_ov, group = BlockType, linetype = "Predicted", color = BlockType)) +
  geom_errorbar(aes(y = part_mean_ov, ymin = part_mean_ov - part_ov_se, ymax = part_mean_ov + part_ov_se, color = BlockType, linetype = "Observed"), width = 0.1) +
  geom_errorbar(aes(y = pred_mean_ov, ymin = pred_mean_ov - pred_ov_se, ymax = pred_mean_ov + pred_ov_se, color = BlockType, linetype = "Predicted"), width = 0.1) +
  labs(
    title = "Correct Answers over Trials",
    x = "Trial Number",
    y = "Mean Correct Answers",
    linetype = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_linetype_manual(name = "Type", values = c("Observed" = "solid", "Predicted" = "dashed"))


#### Plot by AQ group####
# calculate overall means and standard errors for each BlockType, TrialNumber and AQ_group
all_choices_means_AQ_overall <- all_choices %>%
  group_by(BlockType, TrialNumber, AQ_group) %>%
  summarise(
    part_mean_ov = mean(CorrectAns_participant),
    pred_mean_ov = mean(CorrectPrediction),
    part_ov_se = sd(CorrectAns_participant) / sqrt(n()),
    pred_ov_se = sd(CorrectPrediction) / sqrt(n())
  ) %>%
  ungroup()

# means for blocktype and AQ_group only
all_choices_means_blocks_AQ <- all_choices %>%
  group_by(BlockType, AQ_group) %>%
  summarise(
    part_mean_ov = mean(CorrectAns_participant),
    pred_mean_ov = mean(CorrectPrediction)
  ) %>%
  ungroup()


# plot the means over 12 trials for each BlockType and AQ_group, once for part_mean and once for pred_mean
# scatter plots for each participant in all_choices_means_AQ
# line plots for the overall means in all_choices_means_AQ_overall, with standard errors
# facet wrap by AQ_group
ggplot(data = all_choices_means_AQ_overall, aes(x = as.factor(TrialNumber), y = part_mean, color = BlockType)) +
  geom_line(aes(y = part_mean_ov, group = BlockType, linetype = "Observed", color = BlockType)) +
  # geom_line(aes(y = pred_mean_ov, group = BlockType, linetype = "Predicted", color = BlockType)) +
  geom_errorbar(aes(y = part_mean_ov, ymin = part_mean_ov - part_ov_se, ymax = part_mean_ov + part_ov_se, color = BlockType, linetype = "Observed"), width = 0.5) +
  # geom_errorbar(aes(y = pred_mean_ov, ymin = pred_mean_ov - pred_ov_se, ymax = pred_mean_ov + pred_ov_se, color = BlockType, linetype = "Predicted"), width = 0.5) +
  geom_hline(data = all_choices_means_blocks_AQ, aes(yintercept = part_mean_ov, linetype = "Overall Mean"), color = "darkgray", linetype = "dashed") +
  labs(
    title = "Correct Answers over Trials by AQ group",
    x = "Trial Number",
    y = "Mean Correct Answers",
    linetype = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_linetype_manual(name = "Type", values = c("Observed" = "solid", "Predicted" = "dashed", "Overall Mean" = "dotdash")) +
  facet_grid(cols = vars(AQ_group), rows = vars(BlockType))


#### Plot by Med_Split (same picture)####
# calculate means for CorrectAns_participant and CorrectPrediction for each TrialNumber, grouped by ParticipantID, BlockType and Med_Split
all_choices_means_Med <- all_choices %>%
  group_by(ParticipantID, BlockType, TrialNumber, Med_Split) %>%
  mutate(
    part_mean = mean(CorrectAns_participant),
    pred_mean = mean(CorrectPrediction)
  ) %>%
  distinct(ParticipantID, BlockType, TrialNumber, Med_Split, .keep_all = TRUE) %>%
  ungroup()

# calculate overall means and standard errors for each BlockType, TrialNumber and Med_Split
all_choices_means_Med_overall <- all_choices %>%
  group_by(BlockType, TrialNumber, Med_Split) %>%
  summarise(
    part_mean_ov = mean(CorrectAns_participant),
    pred_mean_ov = mean(CorrectPrediction),
    part_ov_se = sd(CorrectAns_participant) / sqrt(n()),
    pred_ov_se = sd(CorrectPrediction) / sqrt(n())
  ) %>%
  ungroup()

# plot the means over 12 trials for each BlockType and Med_Split, once for part_mean and once for pred_mean
# scatter plots for each participant in all_choices_means_Med
# line plots for the overall means in all_choices_means_Med_overall, with standard errors
# facet wrap by Med_Split
ggplot(data = all_choices_means_Med_overall, aes(x = as.factor(TrialNumber), y = part_mean, color = BlockType)) +
  geom_line(aes(y = part_mean_ov, group = BlockType, linetype = "Observed", color = BlockType)) +
  geom_line(aes(y = pred_mean_ov, group = BlockType, linetype = "Predicted", color = BlockType)) +
  geom_errorbar(aes(y = part_mean_ov, ymin = part_mean_ov - part_ov_se, ymax = part_mean_ov + part_ov_se, color = BlockType, linetype = "Observed"), width = 0.1) +
  geom_errorbar(aes(y = pred_mean_ov, ymin = pred_mean_ov - pred_ov_se, ymax = pred_mean_ov + pred_ov_se, color = BlockType, linetype = "Predicted"), width = 0.1) +
  labs(
    title = "Correct Answers over Trials by Med_Split",
    x = "Trial Number",
    y = "Mean Correct Answers",
    linetype = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_linetype_manual(name = "Type", values = c("Observed" = "solid", "Predicted" = "dashed")) +
  facet_grid(rows = vars(Med_Split))
