library(tidyverse)
library(lmerTest)
library(car)
library(emmeans)

# Load data
data_full <- read.csv("plot_choice_vs_predict/all_choices_split.csv")

#### MODELS####
#### GLM####

data_full$AQ_score <- scale(data_full$AQ_score, scale = F)

AQ_full <- glmer(
  data = data_full,
  CorrectAns_participant ~ AQ_score *
    BlockType * as.factor(TrialNumber) +
    (1 | ParticipantID),
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
)

summary(AQ_full)

Anova(AQ_full)

# specify the levels of TrialNumber you're interested in
levels <- list(TrialNumber = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

# compare block types for different values of trial number using emmeans
emmeans(AQ_full, pairwise ~ BlockType | TrialNumber, at = levels)

emmeans(AQ_full, pairwise ~ BlockType)

# check effect of AQ_score in each block type using emtrends
result <- summary(emtrends(AQ_full, pairwise ~ BlockType, var = "AQ_score"),
  infer = c(T, T)
)

result

# check effect of AQ_score in each block type and trial number using emtrends
result <- summary(emtrends(AQ_full, pairwise ~ BlockType | TrialNumber, var = "AQ_score"),
  infer = c(T, T)
)

result
