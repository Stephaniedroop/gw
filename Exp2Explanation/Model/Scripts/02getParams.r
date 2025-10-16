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
all_params <- rbind(params_path, params_food) # 14 of 2. This now gets cesm'd 

# Expand grid of all combinations of all_params and their values, simplify=FALSE otherwise it gives only 1 vector
# This is 2^14 = 16384 rows (12 vars, 2 brs)
all_combos <- as.data.frame(expand.grid(replicate(nrow(all_params), c(0, 1), simplify = FALSE))) 
colnames(all_combos) <- rownames(all_params)

# So, all_combos is all possible things that could happen, but without probabilities.
# To get the probabilities, we need to multiply the relevant probs from all_params

# A copy with the actual rates rather than the var's state
all_combos_probs <- all_combos
# Now replace each cell with its corresponding prob from all_params
for (i in 1:nrow(all_combos_probs)) {
  for (j in 1:ncol(all_combos_probs)) {
    var <- colnames(all_combos_probs)[j]
    val <- all_combos_probs[i, j]
    all_combos_probs[i, j] <- all_params[var, as.character(val)]
  }
}

# Combine the state df with a column of the product of the probs across each row
allworldsPrior <- cbind(all_combos, prob = apply(all_combos_probs, 1, prod))

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

# ------------ In the pursuit of all combos, first get the totally inactive ones -------------

# These can sit as an inert block and get added on to anything

# Totally inactive vars are the intersection of notpathparams and notfoodparams 
inactive_vars <- intersect(notpathparams, notfoodparams) # 10

# Expand grid of 0,1 to get all combinations of these 10 inactive vars
inactive_combos <- as.data.frame(expand.grid(replicate(length(inactive_vars), c(0, 1), simplify = FALSE))) # 1024 of 10. Needs simplify=False otherwise it gives only length 20,1 
colnames(inactive_combos) <- inactive_vars # 10 vars, ie 2^10 = 1024 rows

# Also get union of foodparamnames and pathparamnames
union_paramnames <- union(foodparamnames, pathparamnames) # the 10 not in allvars. This is before the renaming of KS and KSu to specific ones

# So if there are c.8000 worlds (2^13) - that is 4 path, 8 food, then 1 base rate. But still don't know how to combine
# I proceeded on idea there is 2^14, inc. 2 brs and 6 latents.

# So then, what to do with these latents?

# allworlds is actually only the worlds that can occur, ie with non-zero prob. 
# There are another 1024 (2^10) worlds that are combinations of the inactive vars, which can be added on to any of the active ones
# The actual total things that can happen is 2^22 = 4,194,304 = 4.2million
# This is made of: 10 that can't happen, 2 base rates, and then either 10 union, or 4 path and 8 food if you count their duplicates KS and KSu separately

# ----------- ADJACENCY MATRICES ---------------

# Also need prior of all latents whether in model or not. This could go in this or the params script

# A new version with allvars and 'path' and 'food' outcomes
allvars2 <- c(allvars, 'Path', 'Food')


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

# Save everything we might need. Still to get the posteriors of combinations of uvars. Might then not need getPosts
save(all_params, 
     adj_matrix, 
     param_matrix, 
     allworlds, 
     union_paramnames, 
     inactive_combos,
     file=here('Exp2Explanation', 'Model', 'Data', 'params.rda'))