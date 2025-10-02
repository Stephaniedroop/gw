###############################################################################################
############ Tidy best-fitting causal model and export for plotting #############
###############################################################################################

library(here)
source(here('Exp1Prediction', 'Model', 'Scripts', 'modelUtils.R'))

# Load fitted models from `01findModel.R`: 2 dfs of 59049 rows of 13 vars each: path and destination
load(here('Exp1Prediction', 'Model', 'Data', 'fitted.rda'))
load(here('Exp1Prediction', 'Model', 'Data', 'targetDist.rda')) # from before - still need those

# Sometimes use this before we can get it out of the long brute force causal model selection
situations <- as.factor(td_sd$Situation)


# ---------- Rename destination to food -------------
# Just makes it simpler later
fitted_food_mods <- fitted_destination_mods # rename everywhere! Simpler to do it earlier rather than later

# Count number of edges in each structure
fitted_path_mods$n_edge <- rowSums(structures!=0)
fitted_food_mods$n_edge <- rowSums(structures!=0)

# -------- Complexity penalisation -------------

# Penalise complexity (number of edges) to avoid overfitting
complexity_penalisation <- 0.003 # Tried variety, see below, settled on .003 as both path and dest have become stable there

# With complexity 0
# bpix was 25874 which gives structure 0, 1, -1, 0, 0, 0, 1, 1, -1, 0
# bdix was 58713 which gives structure 1, 0, 0, 1, 0, 0, 1, 1, 1, 1

# With complexity .001
# bpix was 31679 which gives structure 0  1 -1 -1  0  0  0  1  0  0
# bdix was 58713 which gives structure 1  0  0  1  0  0  1  1  1  1 

# With complexity .002
# bpix was 22967 which gives structure 0  1  0  0  0  0  0  0 -1  0
# bdix was 58713 which gives structure 1  0  0  1  0  0  1  1  1  1

# With complexity .003
# bpix was 22967 which gives structure 0  1  0  0  0  0  0  0 -1  0 same as at .002
# bdix was 56499 which gives structure 1  0  0  0  0  0  1  0  1  1 few less edges

# With complexity .004
# bpix was 22967 which gives structure 0  1  0  0  0  0  0  0 -1  0
# bdix was 56499 which gives structure 1  0  0  0  0  0  1  0  1  1


# Make the KL bigger for each edge so to penalise complexity and so avoid saturated fully connected model 
bpix <- which.min(fitted_path_mods$kl + fitted_path_mods$n_edge*complexity_penalisation) # 22967
bfix <- which.min(fitted_food_mods$kl + fitted_food_mods$n_edge*complexity_penalisation) # 58713

# Btw, some reporting stats about this distribution:
mean(fitted_path_mods$kl) # .43
sd(fitted_path_mods$kl) # .17
mean(fitted_food_mods$kl) # .21
sd(fitted_food_mods$kl) # .08
mean(fitted_path_mods$n_edge) # 6.7
sd(fitted_path_mods$n_edge) # 1.5
mean(fitted_food_mods$n_edge) # 6.7
sd(fitted_food_mods$n_edge) # 1.5

# Find the best fitting structures and their parameters
best_path <- unlist(structures[bpix,]) # 01000000-10
best_path_params <- unlist(fitted_path_mods[bpix,])


# rbind best_path and the params that have a name in best_path

#pathmod <- rbind(best_path, best_path_params[2:11])
# Get the names of best_path_params that are in best_path

best_food <- unlist(structures[bfix,]) # 1 0 0 0 0 0 1 0 1 1
best_food_params <- unlist(fitted_food_mods[bfix,])

# Get model predictions for each situation
tmp <- fitted_path_mods[bpix,] # There must be a better way because these are basically the same but best_path_params is unlisted?!
# The function wants a list with s as a named vector of the 10 strengths, and only then br and tau
fitted_path_params <- list(s = c(P = tmp$P,
                                 K = tmp$K,
                                 C = tmp$C,
                                 S = tmp$S,
                                 PK=tmp$PK,
                                 PC=tmp$PC,
                                 PS=tmp$PS,
                                 KC=tmp$KC,
                                 KS=tmp$KS,
                                 CS=tmp$CS),
                           br = tmp$br,
                           tau = tmp$tau)

tmp <- fitted_food_mods[ bfix,]

fitted_food_params <- list(s = c(P = tmp$P,
                                        K = tmp$K,
                                        C = tmp$C,
                                        S = tmp$S,
                                        PK=tmp$PK,
                                        PC=tmp$PC,
                                        PS=tmp$PS,
                                        KC=tmp$KC,
                                        KS=tmp$KS,
                                        CS=tmp$CS),
                                  br = tmp$br,
                                  tau = tmp$tau)

# Call function 1 from `modelUtils` file 
mpp <- get_mod_pred(structures[bpix,], fitted_path_params)
mpf <- get_mod_pred(structures[bfix,], fitted_food_params)

df.m <- data.frame(situation = situations, 
                   td_path = td_path, 
                   td_food = td_destination, 
                   mp_path = mpp, 
                   mp_food = mpf)


# Results in 2 dfs, each of 59049 obs of 13 vars
save(df.m, 
     best_path,
     best_path_params,
     best_food,
     best_food_params,
     mpp,
     mpf,
     file = here('Exp1Prediction', 'Model', 'Data', 'model.rda'))
