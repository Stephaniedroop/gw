# ==============================================================================
## Get likelihood and conditional probs, sem etc
# ==============================================================================

library(here)
library(tidyverse)


source(here('Exp2Explanation', 'Model', 'Scripts', 'semUtils.R')) # determinative functions and some variable name vectors

# Load data from the previous getParams script
load(here('Exp2Explanation', 'Model', 'Data', 'params.rda'))


# ----- Step 1: Get all_combos ---------------

# Get combination grid using the vars defined in semUtils
all_combos <- as.data.frame(expand.grid(replicate(
  length(vars),
  c(0, 1),
  simplify = FALSE
)))
colnames(all_combos) <- vars

# Add interactions determinatively
all_combos <- add_interactions(all_combos, base_causes)

# ------ Step 2: copy, replace positions in all_combos with the param/probs for both path and food -------

# Now split all_combos into food and path
#all_path <- all_combos # |> rename(br_p = br, KSu_p = KSu)

#all_food <- all_combos # |> rename(br_f = br, KSu_f = KSu)

all_path_probs <- all_combos

# Now replace each cell with its corresponding prob from all_params - takes a minute
for (i in 1:nrow(all_path_probs)) {
  for (j in 1:ncol(all_path_probs)) {
    var <- colnames(all_path_probs)[j]
    val <- all_path_probs[i, j]
    all_path_probs[i, j] <- params_path[var, as.character(val)]
  }
}

all_food_probs <- all_combos

# Now replace each cell with its corresponding prob from all_params - takes a minute
for (i in 1:nrow(all_food_probs)) {
  for (j in 1:ncol(all_food_probs)) {
    var <- colnames(all_food_probs)[j]
    val <- all_food_probs[i, j]
    all_food_probs[i, j] <- params_food[var, as.character(val)]
  }
}

# Save these because they take 5 mins to run
save(
  all_food_probs,
  all_path_probs,
  all_combos,
  file = here('Exp2Explanation', 'Model', 'Data', 'probscombos.rda')
)
