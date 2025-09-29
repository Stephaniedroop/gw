###############################################################################################
############ Tidy best-fitting causal model and export for plotting #############
###############################################################################################

library(here)
source(here('Exp1Prediction', 'Model', 'Scripts', 'modelUtils.R'))

# Load fitted models from `01findModel.R`: 2 dfs of 59049 rows of 13 vars each: path and destination
load(here('Exp1Prediction', 'Model', 'Data', 'fitted.rda'))

# Count number of edges in each structure
fitted_path_mods$n_edge <- rowSums(structures!=0)
fitted_destination_mods$n_edge <- rowSums(structures!=0)

# Penalise complexity (number of edges) to avoid overfitting
complexity_penalisation <- 0.002 # This was picked arbitrarily: 
#TO DO  justify by plotting how stable it is at different vals of complexity? TO DO free parameter hand fit.

# Find which model has the lowest KL divergence - that's the one we want!
which.min(fitted_path_mods$kl) # 31717 for first td but redone as 32204 - no - 25874

# Make the KL bigger for each edge so to penalise complexity and so avoid saturated fully connected model
bpix <- which.min(fitted_path_mods$kl + fitted_path_mods$n_edge*complexity_penalisation) # 22967
bdix <- which.min(fitted_destination_mods$kl + fitted_destination_mods$n_edge*complexity_penalisation) # 58713

# Find the best fitting structures and their parameters
best_path <- structures[bpix,] # 00100-10000 - no - 01000000-10
best_path_params <- fitted_path_mods[bpix,]

best_destination <- structures[bdix,] # 10011-11100 - no - 1001001111
best_destination_params <- fitted_destination_mods[bdix,]

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

# Call function 1 from `modelUtils` file 
mpp <- get_mod_pred(structures[bpix,], fitted_path_params)
mpd <- get_mod_pred(structures[bdix,], fitted_destination_params)

df.m <- data.frame(situation = df$SituationVerbose[1:16], 
                   td_path = td_path, 
                   td_destination = td_destination, 
                   mp_path = mpp, 
                   mp_destination = mpd)


# Results in 2 dfs, each of 59049 obs of 13 vars
save(df.m, 
     file = here('Exp1Prediction', 'Model', 'Data', 'model.rda'))
