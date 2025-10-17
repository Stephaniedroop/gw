# ==============================================================================
## Get parameters: p(var==0) and p(var==1) for each var and u-var
# ==============================================================================


library(here)
library(tidyverse)
#load(here('Exp2Explanation', 'Model', 'Data', 'pChoice.rda')) # loads 64 obs of 10 - might not be needed here, since I made separate ones in 02getPosts
#load(here('Exp2Explanation', 'Model', 'Data', 'posteriors.rda')) # from getPosts.R - food_post and path_post - but might put them together next

# Import causal models from Exp1
load(here('Exp1Prediction', 'Model', 'Data', 'model.rda'))

# Script to get params for CESM, ie p(var==0) and p(var==1) for each of the vars and u-vars
# Takes input of 'active_path' and 'active_food' which are the active edges and their params from Exp1

# Get a list of all vars - need it later and also here jsut for some names 
allvars <- c(names(best_path), paste0(names(best_path), "u")) # best_food or best_path: it's the same varnames. There are 20
# Also a version with allvars and 'path' and 'food' outcomes: this used in adjacency matrix
allvars2 <- c(allvars, 'Path', 'Food')

# ------------- Some variable names we might need later ----------

# active_path_varnames <- names(active_path)
# active_food_varnames <- names(active_food)
# active_path_uvarnames <- paste0(names(active_path), "u")
# active_food_uvarnames <- paste0(names(active_food), "u")

#active_varnames <- c(active_path_varnames, active_food_varnames) # Need to rename one
active_varnames <- rownames(all_params)
active_path_varnames <- rownames(all_params[1:5,]) # first 5 rows are path vars
active_food_varnames <- rownames(all_params[6:14,]) # next 8 rows are food vars
obs_and_outcomes <- rownames(all_params[c(1:2,6:9,5,14),]) # observed vars and outcomes
active_obs_vars <- rownames(all_params[c(1:2,6:9),])
active_uvarnames <- rownames(all_params[c(3:5,10:14),])
active_uvar <- rownames(all_params[c(3:4,10:13),])
path_obs <- rownames(all_params[c(1:2),])
food_obs <- rownames(all_params[c(6:9),])
path_unobs <- rownames(all_params[c(3:5),])
food_unobs <- rownames(all_params[c(10:14),])
path_uvar <- rownames(all_params[c(3:4),])
food_uvar <- rownames(all_params[c(10:13),])
brs <- rownames(all_params[c(5,14),])

# Now all_combos can be reordered and grouped using these names

# ---------------- Get params for 0,1 with separate names for each outcome -------------

# Create expanded params_path  
params_path <- rbind(
  data.frame('0' = c(0.5,0.5), '1' = c(0.5,0.5), row.names = names(active_path)),
  data.frame('0' = 1 - active_path, '1' = active_path, row.names = paste0(names(active_path), "u"))
)

pathparamnames <- rownames(params_path) # 4
notpathparams <- allvars[!allvars %in% pathparamnames] # 16

# Rename rownames of just KS and KSu as KS_p and KSu_p
rownames(params_path)[rownames(params_path) %in% c("KS", "KSu")] <- c("KS_p", "KSu_p")
# Add a row called 'br', where its value for 1 is the br value from best_path_params, and the value for 0 is 1- that
params_path <- rbind(params_path, 
                     data.frame(
                       '0' = 1 - best_path_params['br'], 
                       '1' = best_path_params['br'], 
                       row.names = 'br_p'))

# Rename columns to be actually 0 and 1
colnames(params_path) <- c('0', '1')

# Create expanded params_food
params_food <- rbind(
  data.frame('0' = c(0.5,0.5,0.5,0.5), '1' = c(0.5,0.5,0.5,0.5), row.names = names(active_food)),
  data.frame('0' = 1 - active_food, '1' = active_food, row.names = paste0(names(active_food), "u"))
)

foodparamnames <- rownames(params_food) # 8
notfoodparams <- allvars[!allvars %in% foodparamnames] # 12

# Rename rownames of just KS and KSu as KS_f and KSu_f
rownames(params_food)[rownames(params_food) %in% c("KS", "KSu")] <- c("KS_f", "KSu_f")
# Add a row called 'br', where its value for 1 is the br value from best_path_params, and the value for 0 is 1- that
params_food <- rbind(params_food, 
                     data.frame(
                       '0' = 1 - best_food_params['br'], 
                       '1' = best_food_params['br'], 
                       row.names = 'br_f'))

# Rename columns to be actually 0 and 1
colnames(params_food) <- c('0', '1')

# Now store union of params_path and params_food
all_params <- rbind(params_path, params_food) # 14 of 2. 


# ---------------- A section to get all combinations of all 'worlds' - all params and their probs -----------------

# Several versions, depending on whether we want just the states, states+probs, and whether to include outcomes in that
# AND whether the inactive vars are included or not, AND whether the base rates are included or not, AND whether the separate versions of the 
# Expand grid of all combinations of all_params and their values, simplify=FALSE otherwise it gives only 1 vector
# This is 2^14 = 16384 rows (12 vars, 2 brs)
all_combos <- as.data.frame(expand.grid(replicate(nrow(all_params), c(0, 1), simplify = FALSE))) 
colnames(all_combos) <- rownames(all_params)

# And what about with outcomes 'Path' and 'Food'? A new version of all combos, with two extra vars Path and Food. But now the number of rows is doubled to 32768 and then again to 
# 65536
# allcombosOutcome <- as.data.frame(expand.grid(replicate(nrow(all_params)+2, c(0, 1), simplify = FALSE))) 
# colnames(allcombosOutcome) <- c(rownames(all_params), 'Path', 'Food') # NEED THIS BUT WITHOUT BASERATE. Then when 000000 but outcome happens, it happens iwth the baserate prob 
# So allcombos is still the right one, but consider the value of the br to basicaly BE the outcome

# Reorder all_combos to be active_path_varnames first, then active_food_varnames
all_combos2 <- all_combos2[, c(path_obs, path_uvar, food_obs, food_uvar, brs)] # Or choose some others from above

# A copy with the actual rates rather than the var's state
all_combos_probs <- all_combos2
# Now replace each cell with its corresponding prob from all_params
for (i in 1:nrow(all_combos_probs)) {
  for (j in 1:ncol(all_combos_probs)) {
    var <- colnames(all_combos_probs)[j]
    val <- all_combos_probs[i, j]
    all_combos_probs[i, j] <- all_params[var, as.character(val)]
  }
}

# Combine the state df with a column of the product of the probs across each row
all_combos2 <- cbind(all_combos2, prob = apply(all_combos_probs, 1, prod))

# Give a group identifier to each combination of observed vars and outcome - there are 256
all_combos2 <- all_combos2 |> 
  mutate(obsGroupId = paste0(
    all_combos2[, path_obs[1]], 
    all_combos2[, path_obs[2]],
    all_combos2[, food_obs[1]],
    all_combos2[, food_obs[2]],
    all_combos2[, food_obs[3]],
    all_combos2[, food_obs[4]],
    # Remove these last two if we don't need outcome
    all_combos2[, brs[1]],
    all_combos2[, brs[2]]
  ))

# Now group by obsGroupId and summarise the probs
allworldsP <- all_combos2 |> 
  group_by(obsGroupId) |> # or obs_and_outcomes - effectively same thing
  summarise(prob = sum(prob))

# Add a column to all_combos2 which is a product of all the unobserved vars' probs from all_combos_probs (ie columns which are in path_unobs and food_unobs)
# Get the columns of unobserved vars
# unobs_cols <- c(path_uvar, food_uvar) # Use c(path_unobs, food_unobs) if you want the br as well
# Get the product of these columns in all_combos_probs
all_combos2$PrUn <- apply(all_combos_probs[, active_uvar], 1, prod) # Prior of all unobserved vars

# Now get the posterior of each row's unobserved vars within its obsGroupId
# BUT NOT WORKING - GIVING THE POSTERIOR = PRIOR FOR EACH SET OF UNOBS VARS (ie the combos all show up equally often always come to same total)

posterior <- all_combos2 |> 
  group_by(obsGroupId) |> 
  mutate(posterior = PrUn/sum(PrUn)) |> 
  ungroup()


# Sums to 1
# uvartest <- all_combos2 |> 
#   group_by(across(all_of(active_uvar))) |> # a way to group by multiple columns named elsewhere
#   summarise(PrUn = sum(prob))


# Putting them all together, without the row product. Currently [,15:28] are the probs and have '.1' after their name, might need better name
allworlds <- cbind(all_combos, all_combos_probs)
# And add a column of the product of the probs across each row
# Get product of cols 15:28 only
allworlds$prob <- apply(allworlds[,15:28], 1, prod)
# Check
allworlds$prob==allworldsPrior$prob

print(max(allworlds$prob)) # 0.0008502827
print(min(allworlds$prob)) # 3.811407e-08

# It's possible we need the marginal probs of each var across all worlds, ie summing the probs of all worlds where var==1

# ----------------- Reorder and marginalise the allcombos ------------------

allworldsP <- 


# ------------ In the pursuit of all combos, first get the totally inactive ones -------------

# These can sit as an inert block and get added on to anything

# Totally inactive vars are the intersection of notpathparams and notfoodparams 
inactive_vars <- intersect(notpathparams, notfoodparams) # 10

# Expand grid of 0,1 to get all combinations of these 10 inactive vars
inactive_combos <- as.data.frame(expand.grid(replicate(length(inactive_vars), c(0, 1), simplify = FALSE))) # 1024 of 10. Needs simplify=False otherwise it gives only length 20,1 
colnames(inactive_combos) <- inactive_vars # 10 vars, ie 2^10 = 1024 rows

# Also get union of foodparamnames and pathparamnames
union_paramnames <- union(foodparamnames, pathparamnames) # the 10 not in allvars. This is before the renaming of KS and KSu to specific ones
unionOutcome <- c(union_paramnames, 'Path', 'Food')

# ----------------- Get possible states of activevars, not using probs ---------------

# States: Expand grid of all combinations of unionOutcome and their values, simplify=FALSE:
allStates <- as.data.frame(expand.grid(replicate(length(unionOutcome), c(0, 1), simplify = FALSE))) # 4096 (2^12)
colnames(allStates) <- unionOutcome

# The probs that govern these are in all_params. Some outcomes are governed by more than 1 param; some only 1 like when all 0000000 then just base rate


# BUT!! We do need a version of all these, because we are talking about edges being ON, not nodes, so we do need the separate edges to path and food

# We also need a version just grouped by observed vars








# So if there are c.8000 worlds (2^13) - that is 4 path, 8 food, then 1 base rate. But still don't know how to combine
# I proceeded on idea there is 2^14, inc. 2 brs and 6 latents.

# So then, what to do with these latents?

# allworlds is actually only the worlds that can occur, ie with non-zero prob. 
# There are another 1024 (2^10) worlds that are combinations of the inactive vars, which can be added on to any of the active ones
# The actual total things that can happen is 2^22 = 4,194,304 = 4.2million
# This is made of: 10 that can't happen, 2 base rates, and then either 10 union, or 4 path and 8 food if you count their duplicates KS and KSu separately

# ----------- ADJACENCY MATRICES ---------------

# QUESTION: in getting 'priors of all latents whether in model or not', when a var is not in either model, 
# which prior do we take? (How to combine the two best fitting. asked neil on slack on 16 oct)

# --------- Simple edge matrix ---------
adj_matrix <- matrix(0, nrow=length(allvars2), ncol=length(allvars2))
rownames(adj_matrix) <- allvars2
colnames(adj_matrix) <- allvars2

# Now populate, according to the following rules:
# For each var in best_path, if it has a 1 in best_path, set adj_matrix[var, Path] = 1
# and if it has a -1 in best_path, set adj_matrix[Path, var] = -1
for (var in names(best_path)) {
  if (best_path[var] == 1) {
    adj_matrix[var, 'Path'] <- 1
  }
  if (best_path[var] == -1) {
    adj_matrix[var, 'Path'] <- -1
  }
}

# For each var in best_food, if it has a 1 in best_food, set adj_matrix[var, 'Food'] = 1
for (var in names(best_food)) {
  if (best_food[var] == 1) {
    adj_matrix[var, 'Food'] <- 1
  }
  if (best_food[var] == -1) {
    adj_matrix[var, 'Food'] <- -1
  }
}

# Also inherit the same numbers as the vars, for the u-vars
for (var in names(best_path)) {
  if (best_path[var] == 1) {
    adj_matrix[paste0(var, "u"), 'Path'] <- 1
  }
  if (best_path[var] == -1) {
    adj_matrix[paste0(var, "u"), 'Path'] <- -1
  }
}

for (var in names(best_food)) {
  if (best_food[var] == 1) {
    adj_matrix[paste0(var, "u"), 'Food'] <- 1
  }
  if (best_food[var] == -1) {
    adj_matrix[paste0(var, "u"), 'Food'] <- -1
  }
}

# -------------- Strengths matrix --------------

# THIS IS LIKELY ALL WRONG


# Also make an adjacency matrix with the actual params, ie probs
param_matrix <- matrix(0, nrow=length(allvars2), ncol=length(allvars2))
rownames(param_matrix) <- allvars2
colnames(param_matrix) <- allvars2


# Now populate, according to the following rules:
# For each var in best_path, if it has a 1 or -1 in best_path, set param_matrix[varu, Path] = best_path_params[var]
# (ie put the var's param in the u-var row)
# Then also for each var in best_path, if it has 1 or -1 in best_path, set param_matrix[var, Path] = 0.5
for (var in names(best_path)) {
  if (best_path[var] == 1 || best_path[var] == -1) {
    param_matrix[paste0(var, "u"), 'Path'] <- best_path_params[var]
    param_matrix[var, 'Path'] <- 0.5
  }
}

# For each var in best_food, if it has a 1 or -1 in best_food, set param_matrix[varu, Food] = best_food_params[var]
# (ie put the var's param in the u-var row)
# Then also also for each var in best_food, if it has 1 or -1 in best_food, set param_matrix[var, Food] = 0.5
for (var in names(best_food)) {
  if (best_food[var] == 1 || best_food[var] == -1) {
    param_matrix[paste0(var, "u"), 'Food'] <- best_food_params[var]
    param_matrix[var, 'Food'] <- 0.5
  }
}







# -------------- Save everything --------------

# Save everything we might need. Still to get the posteriors of combinations of uvars. Might then not need getPosts
save(all_params, 
     adj_matrix, 
     param_matrix, 
     allworlds, 
     union_paramnames, 
     inactive_combos,
     file=here('Exp2Explanation', 'Model', 'Data', 'params.rda'))