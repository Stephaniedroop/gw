# ==============================================================================
## Calculate CESM for each of the 64 worlds in gridworld
# ==============================================================================


library(here)
library(tidyverse)
load(here('Exp2Explanation', 'Model', 'Data', 'pChoice.rda'))
load(here('Exp2Explanation', 'Model', 'Data', 'posteriors.rda')) # from getPosts.R

# Import causal models from Exp1
load(here('Exp1Prediction', 'Model', 'Data', 'model.rda'))

# Needed as input from the previous function
# - params that lists the base rates and strengths of exog noise u vars
# - a df of all the world combos with probs