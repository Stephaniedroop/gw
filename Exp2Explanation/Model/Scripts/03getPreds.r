# ==============================================================================
## Calculate CESM for each of the 64 worlds in gridworld
# ==============================================================================


library(here)
library(tidyverse)
load(here('Exp2Explanation', 'Model', 'Data', 'pChoice.rda'))
load(here('Exp2Explanation', 'Model', 'Data', 'posteriors.rda')) # from getPosts.R

# Needed as input from the previous function
# - params that lists the base rates and strengths of exog noise u vars
# - a df of all the world combos with probs

# what would params look like here?

# Create expanded params_path  
params_path <- rbind(
  data.frame('0' = c(0.5,0.5), '1' = c(0.5,0.5), row.names = names(active_path)),
  data.frame('0' = 1 - active_path, '1' = active_path, row.names = paste0(names(active_path), "u"))
)
# Rename columns to be actually 0 and 1
colnames(params_path) <- c('0', '1')

# Create expanded params_food
params_food <- rbind(
  data.frame('0' = c(0.5,0.5,0.5,0.5), '1' = c(0.5,0.5,0.5,0.5), row.names = names(active_food)),
  data.frame('0' = 1 - active_food, '1' = active_food, row.names = paste0(names(active_food), "u"))
)
# Rename columns to be actually 0 and 1
colnames(expanded_food) <- c('0', '1')



