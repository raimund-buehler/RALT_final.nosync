library(tidyverse)

all_files <- list.files("RALT_csv")

for(file in all_files){
  
  df <- read_csv(paste("RALT_csv/", file, sep = ""))
  
  Sub <- str_sub(file, start = 1, end = 6)
  
  tryCatch({
    # All stims
    stims <- 
      df %>% 
        select(stim, ParticipantAnswer, Reward) %>%
        slice_tail(n = -12) %>% 
        distinct(stim) %>%
        na.omit() %>% 
        pull(stim)
    
    R <- tibble(stim = stims)
    i = 1
    
    for(stim in stims){
      temp <- 
      df %>% 
        select(stim, ParticipantAnswer, Reward, BlockType) %>% 
        slice_tail(n = -12) %>% 
        filter(stim == stim)
      R <- nest_join(R, temp)
    }
    
    R <- unnest(R, cols = c(temp))
    
    write_csv(R, paste("RL_data_debug/", Sub, ".csv", sep = ""))
  },
  error = function(cond){
    message(paste("Failed for subject:", Sub))
    message(paste("Original Error:", cond))
  })
}
