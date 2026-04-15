# ==============================================================================
## 1. Get parameters: p(var==0) and p(var==1) for each var and u-var
## 2. Get all_combos, all_path_probs, all_food_probs
# ==============================================================================

library(here)
library(tidyverse)

# Import causal models from Exp1
load(here('Exp1Prediction', 'Model', 'Data', 'modelSimple.rda')) # from exp1 03processModel.r: six dfs of best path, food and their params, plus active_path and active_food


# ------- Step 1: Get params ---------------

gofood <- best_food_params[2:5]

params_food <- rbind(
  data.frame(
    '0' = rep(.5, length(gofood)),
    '1' = rep(.5, length(gofood)),
    row.names = names(gofood)
  ),
  data.frame(
    '0' = 1 - gofood,
    '1' = gofood,
    row.names = paste0(names(gofood), "u")
  )
)

params_food <- rbind(
  params_food,
  data.frame(
    '0' = 1 - best_food_params['br'],
    '1' = best_food_params['br'],
    row.names = 'br'
  )
)

# Rename columns to be actually 0 and 1
colnames(params_food) <- c('0', '1')

# ----- Step 2: get path params -------

gopath <- best_path_params[2:5]

params_path <- rbind(
  data.frame(
    '0' = rep(.5, length(gopath)),
    '1' = rep(.5, length(gopath)),
    row.names = names(gopath)
  ),
  data.frame(
    '0' = 1 - gopath,
    '1' = gopath,
    row.names = paste0(names(gopath), "u")
  )
)

params_path <- rbind(
  params_path,
  data.frame(
    '0' = 1 - best_path_params['br'],
    '1' = best_path_params['br'],
    row.names = 'br'
  )
)

# Rename columns to be actually 0 and 1
colnames(params_path) <- c('0', '1')


# Save all_combos etc whatever
save(
  params_food,
  params_path,
  file = here('Exp2Explanation', 'Model', 'Data', 'paramsSimple.rda')
)
