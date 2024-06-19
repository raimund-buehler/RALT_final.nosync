library(tidyverse)
library(lmerTest)
library(car)
library(emmeans)
library(interactions)

# Load data
data_full <- read.csv("plot_choice_vs_predict/all_choices_split.csv")

#### MODELS####
#### GLM####

data_full$AQ_score <- scale(data_full$AQ_score, scale = F)

AQ_full <- glmer(
  data = data_full,
  CorrectAns_participant ~ AQ_score *
    BlockType * TrialNumber +
    (1 + BlockType + TrialNumber | ParticipantID),
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
)

summary(AQ_full)

Anova(AQ_full)

#### Effect Sizes####
odds_ratios <- exp(fixef(AQ_full))
# Compute confidence intervals for fixed effects using Wald method
conf_intervals_fixed <- exp(confint(AQ_full, method = "Wald"))[7:14, ]
# Combine into a data frame for neat presentation
summary_table <- data.frame(
  Odds_Ratio = odds_ratios,
  CI_Lower = conf_intervals_fixed[, 1],
  CI_Upper = conf_intervals_fixed[, 2]
)
print(summary_table) %>% round(2)


# specify the levels of TrialNumber
levels <- list(TrialNumber = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

# compare block types for different values of trial number using emmeans
emmeans(AQ_full, pairwise ~ BlockType | TrialNumber, at = levels)

emmeans(AQ_full, pairwise ~ BlockType)

# check effect of AQ_score in each block type using emtrends
result <- summary(emtrends(AQ_full, pairwise ~ BlockType, var = "AQ_score"),
  infer = c(T, T)
)

result

trend_estimates <- result$emtrends

trend_estimates <- trend_estimates %>%
  mutate(
    odds_ratio = exp(AQ_score.trend),
    lower_ci = exp(asymp.LCL),
    upper_ci = exp(asymp.UCL)
  )

trend_estimates

trend_estimates %>%
  select_if(is.numeric) %>%
  round(2)
# get odds ratios for AQ_score in each block type

# check effect of AQ_score in each block type and trial number using emtrends
result <- summary(emtrends(AQ_full, pairwise ~ BlockType | TrialNumber, at = levels, var = "AQ_score"),
  infer = c(T, T)
)

result

interact_plot <- interact_plot(AQ_full,
  pred = AQ_score, modx = BlockType, interval = T,
  legend.main = "Block Type", x = "AQ", y = "%Correct"
) +
  theme_classic() +
  facet_wrap(~ as.factor(TrialNumber), scales = "free")

interact_plot
