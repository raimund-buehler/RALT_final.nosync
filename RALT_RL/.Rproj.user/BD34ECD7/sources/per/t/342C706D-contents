library(tidyverse)

data <- read_csv("all_stims_rho.csv")

#Dirty
dirty <- data %>% 
  filter(BlockType == "social") %>% 
  select(Alpha, Theta, Rho, Sub_ID)

#Clean
r_clean <-
  dirty %>% filter(!(Rho == 0| Rho == 1)) %>% 
  select(Sub_ID, Rho) %>% 
  group_by(Sub_ID) %>% 
  summarise(avg_r = mean(Rho))

a_clean <-
  dirty %>% filter(!(Alpha == 0| Alpha == 1)) %>% 
  select(Sub_ID, Alpha) %>% 
  group_by(Sub_ID) %>% 
  summarise(avg_a = mean(Alpha))

t_clean <-
  dirty %>% filter(!(Theta == 1| Theta == 50)) %>% 
  select(Sub_ID, Theta) %>% 
  group_by(Sub_ID) %>% 
  summarise(avg_t = mean(Theta))

avg <- 
  dirty %>% 
    group_by(Sub_ID) %>% 
    summarise(avg_alpha = mean (Alpha),
              avg_theta = mean(Theta), 
              avg_rho = mean(Rho))

alpha_social <- avg %>% filter(BlockType == "social") %>% pull(avg_alpha)
theta_social <- avg %>% filter(BlockType == "social") %>% pull(avg_theta)
rho_social
alpha_nsocial <- avg %>% filter(BlockType == "nonsocial") %>% pull(avg_alpha)
theta_nsocial <- avg %>% filter(BlockType == "nonsocial") %>% pull(avg_theta)

t.test(alpha_social, alpha_nsocial)

filtered_data <- 
  data %>% 
    column_to_rownames("X1") %>% 
    filter(!(Alpha == 0 & Theta ==1)) %>% 
    group_by(Sub_ID) %>% 
    summarise(avg_alpha = mean (Alpha),
              avg_theta = mean(Theta))

saveRDS(avg, "learning_rates.RDS")  

saveRDS(avg, "avg.RDS")
saveRDS(a_clean, "a_clean.RDS")
saveRDS(t_clean, "t_clean.RDS")
saveRDS(r_clean, "r_clean.RDS")



