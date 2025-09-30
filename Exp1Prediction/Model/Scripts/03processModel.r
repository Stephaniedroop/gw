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

# Count number of edges in each structure
fitted_path_mods$n_edge <- rowSums(structures!=0)
fitted_destination_mods$n_edge <- rowSums(structures!=0)

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
bdix <- which.min(fitted_destination_mods$kl + fitted_destination_mods$n_edge*complexity_penalisation) # 58713

# Btw, some reporting stats about this distribution:
mean(fitted_path_mods$kl) # .43
sd(fitted_path_mods$kl) # .17
mean(fitted_destination_mods$kl) # .21
sd(fitted_destination_mods$kl) # .08
mean(fitted_path_mods$n_edge) # 6.7
sd(fitted_path_mods$n_edge) # 1.5
mean(fitted_destination_mods$n_edge) # 6.7
sd(fitted_destination_mods$n_edge) # 1.5

# Find the best fitting structures and their parameters
best_path <- unlist(structures[bpix,]) # 01000000-10
best_path_params <- unlist(fitted_path_mods[bpix,])


# rbind best_path and the params that have a name in best_path

#pathmod <- rbind(best_path, best_path_params[2:11])
# Get the names of best_path_params that are in best_path

best_destination <- unlist(structures[bdix,]) # 1 0 0 0 0 0 1 0 1 1
best_destination_params <- unlist(fitted_destination_mods[bdix,])

# Create the best fitting models for display  
tmp <- fitted_path_mods[ which.min(fitted_path_mods$kl),]
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

tmp <- fitted_destination_mods[ which.min(fitted_destination_mods$kl),]

fitted_destination_params <- list(s = c(P = tmp$P,
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

# I don't know what fitted_destination_params are!

# Call function 1 from `modelUtils` file 
mpp <- get_mod_pred(structures[bpix,], fitted_path_params)
mpd <- get_mod_pred(structures[bdix,], fitted_destination_params)

df.m <- data.frame(situation = situations, 
                   td_path = td_path, 
                   td_destination = td_destination, 
                   mp_path = mpp, 
                   mp_destination = mpd)




# Results in 2 dfs, each of 59049 obs of 13 vars
save(df.m, 
     best_path,
     best_path_params,
     best_destination,
     best_destination_params,
     mpp,
     mpd,
     file = here('Exp1Prediction', 'Model', 'Data', 'model.rda'))
