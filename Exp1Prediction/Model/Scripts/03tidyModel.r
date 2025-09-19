###############################################################################################
############ Tidy best-fitting causal model and export for plotting #############
###############################################################################################

library(tidyverse)
rm(list=ls())


# Load fitted models from `01findModel.R`: 2 dfs of 59049 rows of 13 vars each, one for path and one for destination
load('../Data/fitted.rdata', verbose = 5)

# Count number of edges in each structure
fitted_path_mods$n_edge <- rowSums(structures!=0)
fitted_destination_mods$n_edge <- rowSums(structures!=0)

# Penalise complexity (number of edges) to avoid overfitting
complexity_penalisation <- 0.002 # This was picked arbitarily: TO DO  justify by plotting how stable it is at different vals of complexity? TO DO free parameter hand fit.

# Find which model has the lowest KL divergence - that's the one we want!
which.min(fitted_path_mods$kl) # 31717 for first td but redone as 32204

# Make the KL bigger for each edge so to penalise complexity and so avoid saturated fully connected model
bpix <- which.min(fitted_path_mods$kl + fitted_path_mods$n_edge*complexity_penalisation) # 29291
bdix <- which.min(fitted_destination_mods$kl + fitted_destination_mods$n_edge*complexity_penalisation) # 32793

# Find the best fitting structures and their parameters
best_path <- structures[bpix,] # 00100-10000
best_path_params <- fitted_path_mods[bpix,]

best_destination <- structures[bdix,] # 10011-11100
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


mpp <- get_mod_pred(structures[bpix,], fitted_path_params)
mpd <- get_mod_pred(structures[bdix,], fitted_destination_params)

df.m <- data.frame(situation = df$SituationVerbose[1:16], td_path = td_path, td_destination = td_destination, mp_path = mpp, mp_destination = mpd)

save(df.m, file = '../Data/model.rda') # Run again once we're happy. Also add in best_path 
#write.csv(df.m, 'model.csv') # 16 obs of 5 vars

