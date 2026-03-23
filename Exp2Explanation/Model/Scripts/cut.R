#load(here('Exp2Explanation', 'Model', 'Data', 'pChoice.rda')) # loads 64 obs of 10 - might not be needed here, since I made separate ones in 02getPosts
#load(here('Exp2Explanation', 'Model', 'Data', 'posteriors.rda')) # from getPosts.R - food_post and path_post - but might put them together next

# This is a good section and I might still use it...

# Script to get params for CESM, ie p(var==0) and p(var==1) for each of the vars and u-vars
# Takes input of 'active_path' and 'active_food' which are the active edges and their params from Exp1

# Get a list of all vars - need it later and also here jsut for some names
allvars <- c(names(best_path), paste0(names(best_path), "u")) # best_food or best_path: it's the same varnames. There are 20
# Also a version with allvars and 'path' and 'food' outcomes: this used in adjacency matrix
allvars2 <- c(allvars, 'Path', 'Food')


# ---------------- Get params for 0,1 -------------

# (put pathparams bit back in the script)

pathparamnames <- rownames(params_path) # 4
notpathparams <- allvars[!allvars %in% pathparamnames] # 16

# Rename rownames of just KS and KSu as KS_p and KSu_p
rownames(params_path)[rownames(params_path) %in% c("KSu")] <- c(
  "KSu_p"
)

# Add a row called 'br', where its value for 1 is the br value from best_path_params, and the value for 0 is 1- that
params_path <- rbind(
  params_path,
  data.frame(
    '0' = 1 - best_path_params['br'],
    '1' = best_path_params['br'],
    row.names = 'br_p'
  )
)

# Rename columns to be actually 0 and 1
colnames(params_path) <- c('0', '1')


# ..... (ditto put back the bare params bit)

foodparamnames <- rownames(params_food) # 8
notfoodparams <- allvars[!allvars %in% foodparamnames] # 12

# Rename rownames of just KS and KSu as KS_f and KSu_f
rownames(params_food)[rownames(params_food) %in% c("KSu")] <- c(
  "KSu_f"
)

# Add a row called 'br', where its value for 1 is the br value from best_path_params, and the value for 0 is 1- that
params_food <- rbind(
  params_food,
  data.frame(
    '0' = 1 - best_food_params['br'],
    '1' = best_food_params['br'],
    row.names = 'br_f'
  )
)

# Rename columns to be actually 0 and 1
colnames(params_food) <- c('0', '1')

# Now store params_path and params_food
all_params <- rbind(params_path, params_food) # 14 of 2.
# IT IS POSSIBLE WE NEED TO RESAVE SO KS ONLY SINGLE

# Rename KS_p as just KS
rownames(all_params)[rownames(all_params) == "KS_p"] <- "KS"
# Remove KS_f
all_params <- all_params[!rownames(all_params) %in% c("KS_f", "KS1"), ]


# ------------- Some variable names we might need later ----------

# For a while we thought we needed all the obs vars? But not clear if we do
# obs_vars <- allvars[1:10]
# obs_vars_noint <- allvars[1:4]

#active_varnames <- c(active_path_varnames, active_food_varnames) # Need to rename one
# active_varnames <- rownames(all_params)
# active_path_varnames <- rownames(all_params[1:5, ]) # first 5 rows are path vars
# active_food_varnames <- rownames(all_params[6:14, ]) # next 8 rows are food vars
# obs_and_outcomes <- rownames(all_params[c(1:2, 6:9, 5, 14), ]) # observed vars and outcomes
# active_obs_vars <- rownames(all_params[c(1:2, 6:9), ])
# active_uvarnames <- rownames(all_params[c(3:5, 10:14), ])
# active_uvar <- rownames(all_params[c(3:4, 10:13), ])
# path_obs <- rownames(all_params[c(1:2), ])
# food_obs <- rownames(all_params[c(6:9), ])
# path_unobs <- rownames(all_params[c(3:5), ])
# food_unobs <- rownames(all_params[c(10:14), ])
path_uvar <- rownames(all_params[c(3:4), ])
# food_uvar <- rownames(all_params[c(10:13), ])
brs <- rownames(all_params[c(5, 13), ])
# path_effect <- c("K", "KS", "Path")
# food_effect <- c("P", "PS", "CS", "KS", "Food")
# Just use these, it's getting too complicated
# pathvars <- c("K", "KS", "S", "Path", "Ku", "KSu_p", "br_p")
# foodvars <- c(
#   "P",
#   "CS",
#   "KS",
#   "PS",
#   "S",
#   "Food",
#   "Pu",
#   "CSu",
#   "KSu_f",
#   "PSu",
#   "br_f"
# )

# If we decide the interaction terms don't contribute then use these lines, otherwise use the full ones up there
#path_u_forlik <- rownames(all_params[c(1,3,5),])
#food_u_forlik <- rownames(all_params[c(6,10,14),])

# Run the deterministic parts - THIS AND OTHERS WILL BE TIDIED OUT AS NOW HAVE A FUNCTION
# all_combos$PK <- as.numeric(all_combos$P & all_combos$K)
# all_combos$PC <- as.numeric(all_combos$P & all_combos$C)
# all_combos$PS <- as.numeric(all_combos$P & all_combos$S)
# all_combos$KC <- as.numeric(all_combos$K & all_combos$C)
# all_combos$KS <- as.numeric(all_combos$K & all_combos$S)
# all_combos$CS <- as.numeric(all_combos$C & all_combos$S)

# NEW FOOD OBS ------------------

#
#
# # NODE KS CAN ONLY BE ONE
# node_names <- rownames(all_params) # 14
#
# # Remove elements KS_p and KS_f
# node_names <- node_names[!node_names %in% c("KS_p", "KS_f")]
# # And add a plain KS
# node_names <- c(node_names, "S", "Food", "Path") # 16

# Now apply these two functions to all_combos to get the likelihoods
path_combos$path_sem <- apply(path_combos, 1, pathlik)
food_combos$food_sem <- apply(food_combos, 1, foodlik)

# Set cols path_poss and food_poss: 'POSSIBLESET'
path_combos$path_poss <- path_combos$path_sem == path_combos$Path
food_combos$food_poss <- food_combos$food_sem == food_combos$Food

# But also need step to remove the ones where the interaction can't be true, right?
# Set path_combos$path_poss to FALSE if KS does not equal K and S
path_combos$path_poss <- ifelse(
  (path_combos$KS == 1 & path_combos$K == 0) |
    (path_combos$KS == 1 & path_combos$S == 0) |
    (path_combos$KS == 0 & path_combos$K == 1) |
    (path_combos$KS == 0 & path_combos$S == 1),
  FALSE,
  path_combos$path_poss
)

# Set food_combos$food_poss to FALSE if PS does not equal P and S
food_combos$food_poss <- ifelse(
  (food_combos$PS == 1 & food_combos$P == 0) |
    (food_combos$PS == 1 & food_combos$S == 0) |
    (food_combos$PS == 0 & food_combos$P == 1) |
    (food_combos$PS == 0 & food_combos$S == 1),
  FALSE,
  food_combos$food_poss
)


# Perhaps we shouldn't do this bit and should keep the P=0 ones
pathposs <- path_combos |> # 64 of 9; no, 16
  filter(path_poss == TRUE)

foodposs <- food_combos |> # 1024 of 13; no, 256
  filter(food_poss == TRUE)


# Get summaries
pathposs <- pathposs |>
  group_by(K, KS, Path) |>
  mutate(group = cur_group_id()) |>
  ungroup()

pathsum <- pathposs |> # 8 groups, from 2 to 6 rows
  group_by(group) |>
  summarise(n = n())

foodposs <- foodposs |>
  group_by(P, CS, KS, PS, Food) |>
  mutate(group = cur_group_id()) |>
  ungroup()

foodsum <- foodposs |> # 32 groups, from 4 to 31 rows
  group_by(group) |>
  summarise(n = n())

# Get a list of all 20 vars from the model just loaded
#vars <- c(names(best_path), paste0(names(best_path), "u")) # best_food or best_path: it's the same 20 names. If rearrange alphabetically it will disturb
#vars_br <- c(vars, 'br')

# So, by looking in foodsum and pathsum, we see that under each combination of observed vars, there are variable numbers of combos of unobs vars that can give rise to it.
# So, now to bring in the probabilities

# A copy with the actual rates rather than the var's state
all_combos_probs <- all_combos
# Now replace each cell with its corresponding prob from all_params - takes a minute
for (i in 1:nrow(all_combos_probs)) {
  for (j in 1:ncol(all_combos_probs)) {
    var <- colnames(all_combos_probs)[j]
    val <- all_combos_probs[i, j]
    all_combos_probs[i, j] <- all_params[var, as.character(val)]
  }
}


# Now apply these two functions to all_combos to get the likelihoods
all_combos$path_sem <- apply(all_combos, 1, pathlik)
all_combos$food_sem <- apply(all_combos, 1, foodlik)

# Set cols path_poss and food_poss: 'POSSIBLESET'
all_combos$path_poss <- all_combos$path_sem == all_combos$Path
all_combos$food_poss <- all_combos$food_sem == all_combos$Food

# Which worlds are possible?
all_combos$possible <- all_combos$path_poss & all_combos$food_poss

# Get the prior for unobs
all_combos$pathPrior <- apply(all_combos_probs[, path_unobs], 1, prod) # Prior of all path unobserved vars - if just path_unobs then it gives strange numbers like .429
all_combos$foodPrior <- apply(all_combos_probs[, food_unobs], 1, prod) # Prior of all food unobserved vars
#all_combos$pathlik <- all_combos$path_sem * all_combos$pathPrior
#all_combos$foodlik <- all_combos$food_sem * all_combos$foodPrior

# pathposs <- all_combos |> #16384
#   filter(path_poss == TRUE)
# Or the column vars could be split up into just what pertains to path

pathposs <- all_combos |> #16384
  filter(path_poss == TRUE) |>
  select(K, KS, Path, Ku, KSu_p, br_p, path_sem, pathPrior, pathlik)

# foodposs <- all_combos |> #16384
#   filter(food_poss == TRUE)

foodposs <- all_combos |> #16384
  filter(food_poss == TRUE) |>
  select(
    P,
    CS,
    KS,
    PS,
    Food,
    Pu,
    CSu,
    KSu_f,
    PSu,
    br_f,
    food_sem,
    foodPrior,
    foodlik
  )

# possworlds <- all_combos |> #8192
#   filter(possible == TRUE)

# Assign a group number
pathposs <- pathposs |>
  group_by(K, KS, Path) |>
  mutate(group = cur_group_id()) |>
  ungroup()


# All combos does seem like what we need. Now we need to filter the actual pChoice for how it comapres to this?

# Test - NOT WORKING BECAUSE THE SUMS ARE ALL 512 OR 4. This is because of all the other 1024 combinations of inactive vars.
# Can divide by 1024 - but can we do that blanketly, or do the inactive edged have a param?
test <- all_combos |>
  #filter(path_sem==TRUE) |>
  group_by(K, KS_p) |>
  summarise(sum = sum(pathPrior))
# This way it gives 2^9 and I have only two vars so I am not including enough vars, it should be 0. Need outcome as a factor too?
# Or group by all the observed vars and interactions, but not by outcome? Try tomorrow

# Combine the state df with a column of the product of the probs across each row
all_combos2 <- cbind(all_combos, prob = apply(all_combos_probs, 1, prod))

# Give a group identifier to each combination of observed vars and outcome - there are 256: 2 PATH 4 FOOD 2 OUTCOME
all_combos2 <- all_combos2 |>
  mutate(
    obsGroupId = paste0(
      all_combos2[, path_obs[1]],
      all_combos2[, path_obs[2]],
      all_combos2[, food_obs[1]],
      all_combos2[, food_obs[2]],
      all_combos2[, food_obs[3]],
      all_combos2[, food_obs[4]],
      # Remove these last two if we don't need outcome
      all_combos2[, brs[1]],
      all_combos2[, brs[2]]
    )
  )

# Now group by obsGroupId and summarise the probs
# allworldsP <- all_combos2 |>
#   group_by(obsGroupId) |> # or obs_and_outcomes - effectively same thing
#   summarise(prob = sum(prob))

# Add a column to all_combos2 which is a product of all the unobserved vars' probs from all_combos_probs (ie columns which are in path_unobs and food_unobs)
all_combos2$PrUn <- apply(all_combos_probs[, active_uvar], 1, prod) # Prior of all unobserved vars

# Now get the posterior of each row's unobserved vars within its obsGroupId
posterior <- all_combos2 |>
  group_by(obsGroupId) |>
  mutate(posterior = PrUn / sum(PrUn)) |>
  ungroup()

# Now the PrUn sums to 1 in each obsGroupId

# Putting them all together, without the row product. Currently [,15:28] are the probs and have '.1' after their name, might need better name
allworlds <- cbind(all_combos, all_combos_probs)
# And add a column of the product of the probs across each row
# Get product of cols 15:28 only
allworlds$prob <- apply(allworlds[, 15:28], 1, prod)
# Check
#allworlds$prob==allworldsPrior$prob

print(max(allworlds$prob)) # 0.0008502827
print(min(allworlds$prob)) # 3.811407e-08

# It's possible we need the marginal probs of each var across all worlds, ie summing the probs of all worlds where var==1

# ----------------- Reorder and marginalise the allcombos ------------------

# ------------ In the pursuit of all combos, first get the totally inactive ones -------------

# These can sit as an inert block and get added on to anything

# Totally inactive vars are the intersection of notpathparams and notfoodparams
inactive_vars <- intersect(notpathparams, notfoodparams) # 10

# Expand grid of 0,1 to get all combinations of these 10 inactive vars
inactive_combos <- as.data.frame(expand.grid(replicate(
  length(inactive_vars),
  c(0, 1),
  simplify = FALSE
))) # 1024 of 10. Needs simplify=False otherwise it gives only length 20,1
colnames(inactive_combos) <- inactive_vars # 10 vars, ie 2^10 = 1024 rows

# Also get union of foodparamnames and pathparamnames
union_paramnames <- union(foodparamnames, pathparamnames) # the 10 not in allvars. This is before the renaming of KS and KSu to specific ones
unionOutcome <- c(union_paramnames, 'Path', 'Food')

# ----------------- Get possible states of activevars, not using probs ---------------

# States: Expand grid of all combinations of unionOutcome and their values, simplify=FALSE:
allStates <- as.data.frame(expand.grid(replicate(
  length(unionOutcome),
  c(0, 1),
  simplify = FALSE
))) # 4096 (2^12)
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

# (However, for both adj_matrix and param_matrix, there should also be the intermediate vars 1 on way to main path or food effect)

# QUESTION: in getting 'priors of all latents whether in model or not', when a var is not in either model,
# which prior do we take? (How to combine the two best fitting. asked neil on slack on 16 oct)

# --------- Simple edge matrix ---------
adj_matrix <- matrix(0, nrow = length(allvars2), ncol = length(allvars2))
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
param_matrix <- matrix(0, nrow = length(allvars2), ncol = length(allvars2))
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

# (However, for both adj_matrix and param_matrix, there should also be the intermediate vars 1 on way to main path or food effect)
# So this is not yet finished

# -------------- Save everything --------------

# Save everything we might need. Still to get the posteriors of combinations of uvars. Might then not need getPosts
save(
  all_params,
  adj_matrix,
  param_matrix,
  allworlds,
  union_paramnames,
  inactive_combos,
  file = here('Exp2Explanation', 'Model', 'Data', 'params.rda')
)

# all_food$prior_unobs <- apply(all_food_probs2[, 7:10], 1, prod)
# all_path$prior_unobs <- apply(all_path_probs2[, 5:6], 1, prod)
#
# all_food$prior <- apply(all_food_probs2[, 7:22], 1, prod)
# all_path$prior <- apply(all_path_probs2[, 5:22], 1, prod)

all_path$sem <- apply(all_path, 1, pathlik) # 32.7k each
all_food$sem <- apply(all_food, 1, foodlik) # 32.7k each
all_path_probs$sem <- apply(all_path, 1, pathlik) # 32.7k each
all_food_probs$sem <- apply(all_food, 1, foodlik) # 32.7k each


# Re-order the cols so that the observed vars are first, then the outcomes, then the unobs vars, then the brs, then the rest of the params
all_food <- all_food[, c(food_obs, 'sem', food_uvar, brs[2], notfoodparams)] |>
  select(-S.1) # remove the S.1 column which is just a duplicate of S
all_path <- all_path[, c(path_obs, 'sem', path_uvar, brs[1], notpathparams)] |>
  select(-S.1)

all_path_probs2 <- all_path_probs[, c(
  path_obs,
  'sem',
  path_uvar,
  brs[1],
  notpathparams
)] |>
  select(-S.1)
all_food_probs2 <- all_food_probs[, c(
  food_obs,
  'sem',
  food_uvar,
  brs[2],
  notfoodparams
)] |>
  select(-S.1)

# ---- We do actually need the individual params -----

# CHOOSE ONE AND DELETE THE OTHER

# If we only need the active ones:

# params_path <- rbind(
#   data.frame(
#     '0' = c(0.5, 0.5),
#     '1' = c(0.5, 0.5),
#     row.names = names(active_path)
#   ),
#   data.frame(
#     '0' = 1 - active_path,
#     '1' = active_path,
#     row.names = paste0(names(active_path), "u")
#   )
# )
#
# params_food <- rbind(
#   data.frame(
#     '0' = c(0.5, 0.5, 0.5, 0.5),
#     '1' = c(0.5, 0.5, 0.5, 0.5),
#     row.names = names(active_food)
#   ),
#   data.frame(
#     '0' = 1 - active_food,
#     '1' = active_food,
#     row.names = paste0(names(active_food), "u")
#   )
# )

# ---- Below this not sure yet -----

# unusedvars <- c("S", "Su", "C", "Cu", "PK", "PKu", "PC", "PCu", "KC", "KCu")
#
# unusedparams <- as.data.frame(
#   matrix(
#     0.5,
#     nrow = 10,
#     ncol = 2,
#     dimnames = list(
#       c("S", "Su", "C", "Cu", "PK", "PKu", "PC", "PCu", "KC", "KCu"),
#       c('0', '1')
#     )
#   )
# )

# all_params_full <- rbind(
#   all_params,
#   unusedparams
# )

# All good
# all_path$newsem <- pathlik_vec(all_path)
# all_path$check <- all_path$newsem == all_path$sem
# all_path |> filter(check == FALSE) # should be none

# pathlik <- function(row) {
#   # Get the values of the path observed vars
#   p1 <- row[path_obs[1]]
#   p2 <- row[path_obs[2]]
#   # Get the values of the path unobserved vars
#   pu1 <- row[path_uvar[1]]
#   pu2 <- row[path_uvar[2]]
#   # Get the base rate
#   br <- row[brs[1]]
#
#   # Gen part: active cause or br
#   gen <- br | (p1 & pu1)
#   # preventive part: "no active preventive cause": broad preventative scope
#   no_prevent <- !(p2 & pu2)
#   pathlik <- gen & no_prevent
#   return(pathlik)
# }
#
# foodlik <- function(row) {
#   # Get the values of the food observed vars
#   f1 <- row[food_obs[1]]
#   f2 <- row[food_obs[2]]
#   f3 <- row[food_obs[3]]
#   f4 <- row[food_obs[4]]
#   # Get the values of the food unobserved vars
#   fu1 <- row[food_uvar[1]]
#   fu2 <- row[food_uvar[2]]
#   fu3 <- row[food_uvar[3]]
#   fu4 <- row[food_uvar[4]]
#   # Get the base rate
#   r <- row[brs[2]]
#
#   foodlik <- (f1 & fu1) | (f2 & fu2) | (f3 & fu3) | (f4 & fu4) | r
#   # But it's weird to think about 0 outcomes. In collider it was all combinations: eg 000
#   return(foodlik)
# }

# [to here - next do groups and then get posteriors]

# Take a copy for playing BUT CHNAGE BACK LATER
food_copy <- all_food
path_copy <- all_path

# Test - if it works, go back and put in the right place - this should be the right ONE
food_copy$prior <- Reduce(`*`, all_food_probs2[all_food_uvar]) # NOT CLEAR IF SHOUL DBE ALL UVAR OR ALL UNOBSERVED IE NOT IN CAUSAL MODEL
path_copy$prior <- Reduce(`*`, all_path_probs2[all_path_uvar])

# Now group by observed+outcome, give a group id
food_copy <- food_copy |>
  group_by(P, PS, CS, KS, sem) |>
  mutate(group = cur_group_id()) |>
  ungroup()

path_copy <- path_copy |>
  group_by(K, KS, sem) |>
  mutate(group = cur_group_id()) |>
  ungroup()


# ------ Get groupings - can only be done after we get the sem ---------

# Get a condition tag
# path_copy <- path_copy |>
#   group_by(P, K, C, S, sem) |>
#   mutate(condition = paste0('c', P, K, C, S, as.numeric(sem))) |>
#   ungroup()
#
# path_copy$condition <- as.factor(path_copy$condition)
#
# food_copy <- food_copy |>
#   group_by(P, K, C, S, sem) |>
#   mutate(condition = paste0('c', P, K, C, S, as.numeric(sem))) |>
#   ungroup()
#
# food_copy$condition <- as.factor(food_copy$condition)

# Is one setting 64 rows? (see group 17) There is only one setting of the participating unobserved vars
# But the 12 'unused' vars are not combinatorially all allowed, as they set the interactions
# Check some other groups: group 15 is two groups of 64
# S, C, K, PK, PC, KC are all constrained to one value, and the unobs are allowed to vary
observed_food_category <- food_copy |> # 18 groups
  group_by(group) |>
  summarise(n = n())

# 2048 is min in group 3: not allowed to vary is S, PS, CS. Allowed to vary is
# P, C, + 8 unobserved = 2^10 = 1024. It varies from 2:8 settings in a group

observed_path_category <- path_copy |> # 6 groups
  group_by(group) |>
  summarise(n = n())

path_copy <- path_copy |>
  group_by(group, Ku, KSu_p, br_p) |>
  mutate(unobs_group = cur_group_id()) |>
  ungroup()

path_copy <- path_copy |>
  group_by(Ku, KSu_p, br_p) |>
  mutate(unobs_group2 = cur_group_id()) |>
  ungroup()

observed_path_category2 <- path_copy |> # 6 groups
  group_by(unobs_group) |>
  summarise(n = n())

observed_path_category3 <- path_copy |> # 6 groups
  group_by(group, unobs_group2) |>
  summarise(n = n())

print(sum(observed_path_category2$n)) # 16384, so all rows are in a group
print(sum(observed_path_category$n))

food_copy <- food_copy |>
  group_by(group, Pu, PSu, CSu, KSu_f, br_f) |>
  mutate(unobs_group = cur_group_id()) |>
  ungroup()

food_copy <- food_copy |>
  group_by(Pu, PSu, CSu, KSu_f, br_f) |>
  mutate(unobs_group2 = cur_group_id()) |>
  ungroup()

observed_food_category <- food_copy |> # 18 groups
  group_by(group) |>
  summarise(n = n())

observed_food_category2 <- food_copy |> # 288 groups
  group_by(unobs_group) |>
  summarise(n = n())

observed_food_category3 <- food_copy |> # 288 groups
  group_by(group, unobs_group2) |>
  summarise(n = n())

observed_food_category4 <- food_copy |> # 288 groups
  group_by(group) |>
  summarise(n = count())

# ---- NOW get conditional probabilities! -------
# This prior was for all vars not in the causal model: observed and unobserved. This is maybe not right?
food_copy <- food_copy |>
  group_by(condition) |> # or group! depends what you decide later on
  mutate(unobs_post = prior / sum(prior)) |> # IF IT IS PRIO OF UNOBSERVED ONLY THEN IN 05PROCESS THE GETPOST NEEDS THE WHOLE CONDIITON
  ungroup()

path_copy <- path_copy |>
  group_by(condition) |> # or group!
  mutate(unobs_post = prior / sum(prior)) |>
  ungroup()

getpost <- path_copy |>
  #filter(!node2 %in% c('A', 'B')) |>
  group_by(condition, node3, .drop = F) |> # or group?! Think it must be condition.
  summarise(post = sum(unobs_post), prior = sum(prior))

food_posterior <- food_copy |>
  group_by(group, unobs_group) |>
  summarise(unobs_post = sum(unobs_post))

path_posterior <- path_copy |>
  group_by(group, unobs_group) |>
  summarise(unobs_post = sum(unobs_post))


rel_food_uvar <- c("Pu", "PSu", "CSu", "KSu_f")
rel_path_uvar <- c("Ku", "KSu_p")

all_path_uvar <- c(
  "Pu",
  "Ku",
  "Cu",
  "Su",
  "PKu",
  "PCu",
  "PSu",
  "KCu",
  "KSu_p",
  "CSu",
  "br_p"
)
all_food_uvar <- c(
  "Pu",
  "Ku",
  "Cu",
  "Su",
  "PKu",
  "PCu",
  "PSu",
  "KCu",
  "KSu_f",
  "CSu",
  "br_f"
)

# These also need path_obs, path_uvar, food_obs, food_uvar, brs to be defined
# path_obs <- c("K", "KS", "S")
# path_uvar <- c("Ku", "KSu_p")
# food_obs <- c("P", "PS", "CS", "KS")
# food_uvar <- c("Pu", "PSu", "CSu", "KSu_f")
# brs <- c("br_p", "br_f")

# 17 in total
params <- c(
  "P",
  "K",
  "C",
  "S",
  "Pu",
  "Ku",
  "Cu",
  "Su",
  "PKu",
  "PCu",
  "KCu",
  "KSu_p",
  "PSu",
  "CSu",
  "KSu_f",
  "br_p",
  "br_f"
)

# 15
fparams <- c(
  "P",
  "K",
  "C",
  "S",
  "Pu",
  "Ku",
  "Cu",
  "Su",
  "PKu",
  "PCu",
  "KCu",
  "PSu",
  "CSu",
  "KSu_f",
  "br_f"
)
