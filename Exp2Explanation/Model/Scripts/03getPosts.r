# ==============================================================================
## Get posteriors for different combinations of unobserved vars
# ==============================================================================

library(here)
library(tidyverse)

load(here('Exp2Explanation', 'Model', 'Data', 'pChoice.rda'))

# Some questions over how to use priors for lesioned models...

# Before we can use the cesm, we need all possible combinations of unobserevd vars, for each situation
# This means the u vars that go with the edges in the model (both causal models - the Path and Food ones)

# Import causal models from Exp1
load(here('Exp1Prediction', 'Model', 'Data', 'model.rda'))
# CAn get active edges from active_path and active_food
# The associated params ARE the needed ones for the u-vars

# So are we understanding K & Ku needs to be on to get the path Effect? It is not determinative so enumerate all 0 and 1 combos of each
# So we need to get all combinations of 0 and 1 for the u-vars in each model
u_vars_path <- paste0(names(active_path), "u")
u_vars_food <- paste0(names(active_food), "u")
n_u_path <- length(u_vars_path)
n_u_food <- length(u_vars_food)

# Get all combinations of 0 and 1 for n_u_path and n_u_food
u_combos_path <- as.data.frame(expand.grid(replicate(n_u_path, c(0, 1), simplify = FALSE)))
colnames(u_combos_path) <- u_vars_path

u_combos_path <- u_combos_path |> 
  mutate(combo_id = row_number())

u_combos_food <- as.data.frame(expand.grid(replicate(n_u_food, c(0, 1), simplify = FALSE)))
colnames(u_combos_food) <- u_vars_food

u_combos_food <- u_combos_food |> 
  mutate(combo_id = row_number())

# Split pChoice into path and food components
# pChoice has 64 rows, one for each situation x food x path combination
# Get just the 16 situations x the two values of path
# and then the 16 situations x the four values of food

# -------- Either this way or the next section not both ------------

pChoice_path <- pChoice |> 
  select(situationVerbose, situation, Path, p_short, p_long)
pChoice_path <- unique(pChoice_path) # 32 obs of 5
pChoice_path <- merge(pChoice_path, u_combos_path) # 128 obs of 8

pChoice_food <- pChoice |> 
  select(situationVerbose, situation, Food, p_pizza, p_hotdog)
pChoice_food <- unique(pChoice_food) # 32 obs of 5


pChoice_food <- merge(pChoice_food, u_combos_food) # 512 obs of 10


# First we need the actual probs of the vars that obtain, before getting all the combinations



# -------- Test other way, only separately combine 64 composite pChoice with u_combos_path and u_combos_food -------------
# Now we need to combine these with pChoice, so that for each situation we have all combinations of u-vars for both models
# First combine pChoice with u_combos_path
#pChoice_path2 <- merge(pChoice, u_combos_path) # 256: 64 x 2^2, of 13 vars

# Separately combine pChoice with u_combos_food
#pChoice_food2 <- merge(pChoice, u_combos_food) # 1024: 64 x 2^4, of 15 vars because 4 model vars instead of path's two

# --------- Moving on -----------
# Now attach the values from active_path and active_food to these dataframes, without replacing
# with the corresponding values when the u-var is 1, and 1-value when the u-var is 0
# Add a new column for each u-var, with the corresponding param value from active_path 
for (i in 1:n_u_path) {
  var <- u_vars_path[i]
  new_var <- paste0(var, "_var")
  pChoice_path[[new_var]] <- rep(NA, nrow(pChoice_path))
  param <- active_path[names(active_path) == sub("u$", "", var)]
  pChoice_path[[new_var]] <- ifelse(pChoice_path[[var]] == 1, param, 1 - param)
}

for (i in 1:n_u_food) {
  var <- u_vars_food[i]
  new_var <- paste0(var, "_var")
  # pChoice_food[[new_var]] <- NA
  pChoice_food[[new_var]] <- rep(NA, nrow(pChoice_food))
  param <- active_food[names(active_food) == sub("u$", "", var)]
  pChoice_food[[new_var]] <- ifelse(pChoice_food[[var]] == 1, param, 1 - param)
}

# Get the observed vars too: get the var names from active_path

# Add names from active_path to pChoice_path with param of .5 for each
for (i in 1:length(active_path)) {
  var <- names(active_path)[i]
  if (!(var %in% colnames(pChoice_path))) {
    pChoice_path[[var]] <- rep(0.5, nrow(pChoice_path))
  }
}
  
# Add names from active_food to pChoice_food with param of .5 for each
for (i in 1:length(active_food)) {
  var <- names(active_food)[i]
  if (!(var %in% colnames(pChoice_food))) {
    pChoice_food[[var]] <- rep(0.5, nrow(pChoice_food))
  }
}

# get product of the cols with _var in the names for PrUn, the prior of the unobserved vars
pChoice_path$PrUn <- apply(pChoice_path[, grepl("_var$", colnames(pChoice_path))], 1, prod)

# Not actually sure when we need this but get it anyway for now
# Also divide by 1/16 for the overall Prior because there are 16 worlds with equal prior probs - the other vars like K and KS are wrapped into this
pChoice_path$Pr <- pChoice_path$PrUn * (1/16)

# AND also multiply by the p_short or p_long depending on Path
pChoice_path$PrUn <- pChoice_path$PrUn * ifelse(pChoice_path$Path == "Short", pChoice_path$p_short, pChoice_path$p_long)

# Do same for FOOD
pChoice_food$PrUn <- apply(pChoice_food[, grepl("_var$", colnames(pChoice_food))], 1, prod)

pChoice_food$Pr <- pChoice_food$PrUn * (1/16)

pChoice_food$PrUn <- pChoice_food$PrUn * ifelse(pChoice_food$Food == "Pizza", pChoice_food$p_pizza, pChoice_food$p_hotdog)


# ------- That's the priors, now for posteriors -----------

# There is not the same way of grouping by Effect as in collider
# Group by the observed vars and the outcome var - Path or Food - then get posterior
# PATH - 128 obs of 15: 16 situations x 2 paths x 2^2 u-vars
path_post <- pChoice_path |> 
  group_by(situation) |> 
  mutate(posterior = PrUn/sum(PrUn)) |>
  ungroup()

# Check it sums to 1 in each situation 
# check_path <- path_post |> 
#   group_by(situation) |> 
#   summarise(total = sum(posterior))

# FOOD - 512 obs of 21: 16 situations x 2 foods x 2^4 u-vars
food_post <- pChoice_food |> 
  group_by(situation) |> 
  mutate(posterior = PrUn/sum(PrUn)) |>
  ungroup()

# Check it sums to 1 in each situation
# check_food <- food_post |> 
#   group_by(situation) |> 
#   summarise(total = sum(posterior))

# Think now we can get the CESM values for these...?
# save these dataframes for later
save(path_post, food_post, file=here('Exp2Explanation', 'Model', 'Data', 'posteriors.rda'))


#write.csv(path_post, 'path_post.csv', row.names=FALSE)
#write.csv(food_post, 'food_post.csv', row.names=FALSE)


