library(tidyverse)
library(httpgd)

# Set up the httpgd server
hgd()
hgd_browse()

# Load data
all_choices <- read_csv("plot_choice_vs_predict/all_choices.csv")

# calculate means for CorrectAns_participant and CorrectPrediction for each TrialNumber, grouped by ParticipantID and BlockType
all_choices_means <- all_choices %>%
  group_by(ParticipantID, BlockType, TrialNumber) %>%
  mutate(
    part_mean = mean(CorrectAns_participant),
    pred_mean = mean(CorrectPrediction)
  ) %>%
  distinct(ParticipantID, BlockType, TrialNumber, .keep_all = TRUE) %>%
  ungroup()


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

ggplot(data = all_choices_means, aes(x = as.factor(TrialNumber), y = part_mean, color = BlockType)) +
  # geom_boxplot() +
  geom_line(data = all_choices_means_overall, aes(y = part_mean_ov, group = BlockType, linetype = "Observed", color = BlockType)) +
  geom_line(data = all_choices_means_overall, aes(y = pred_mean_ov, group = BlockType, linetype = "Predicted", color = BlockType)) +
  geom_errorbar(data = all_choices_means_overall, aes(y = part_mean_ov, ymin = part_mean_ov - part_ov_se, ymax = part_mean_ov + part_ov_se, color = BlockType, linetype = "Observed"), width = 0.1) +
  geom_errorbar(data = all_choices_means_overall, aes(y = pred_mean_ov, ymin = pred_mean_ov - pred_ov_se, ymax = pred_mean_ov + pred_ov_se, color = BlockType, linetype = "Predicted"), width = 0.1) +
  labs(
    title = "Correct Answers over Trials",
    x = "Trial Number",
    y = "Mean Correct Answers",
    linetype = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_linetype_manual(name = "Type", values = c("Observed" = "solid", "Predicted" = "dashed"))
