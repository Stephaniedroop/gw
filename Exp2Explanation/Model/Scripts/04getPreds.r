# ==============================================================================
## Calculate CESM for all worlds in gridworld
# ==============================================================================

library(here)
library(tidyverse)
#load(here('Exp2Explanation', 'Model', 'Data', 'pChoice.rda'))
load(here('Exp2Explanation', 'Model', 'Data', 'scenarios.rda')) # loads all_food and all_path, from getProbs.R?
# Load data from the previous getParams script
load(here('Exp2Explanation', 'Model', 'Data', 'params.rda'))

source(here('Exp2Explanation', 'Model', 'Scripts', 'cesmUtils.R')) # Functions for running the cesm model

# Other values set outside for now
N_cf <- 50000L # How many counterfactual samples to draw
#modelruns <- 5
s <- 0.7

set.seed(12)

# Test with 10k sims
path_preds <- get_cesm(all_path, structure = "path", params = params_path)
food_preds <- get_cesm(all_food, structure = "food", params = params_food) # saved as 'foodtest.csv' to check

# If it all worked, then restructure and tidy getParams

# Now doing analysis script for how to check if it worked. viz etc.

# The thing is, that for the 4-way outcome, the same condition, eg 'c00000' the last 0 means either path or food.
# It is not the same as the 6-character situation tag, where it is Food then Path
# Remember that when merging
# To change it, need to change the 'structure' part of semUtils and cesmUtils

# This is with 50k sims and only 1 runs
save(
  food_preds,
  path_preds,
  file = here('Exp2Explanation', 'Model', 'Data', 'modelData.rda')
) # 1440 of 27
