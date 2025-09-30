###############################################################################################
############ Select best-fitting causal model by minimising K-L divergence #############
###############################################################################################

library(here)
source(here('Exp1Prediction', 'Model', 'Scripts', 'modelUtils.R'))

# ==============================================================================
# Path and Destination Choice Model Analysis
# ==============================================================================
# This script analyzes experimental data from a path/destination choice task where
# participants rated the likelihood of different routes and destinations based on
# various factors:
#
# Variables:
# - P (Preference): Whether the person has a food preference (0=Absent, 1=Hotdog)
# - K (Knowledge): Whether they know about the hotdog stand (0=No, 1=Yes)  
# - C (Character): Person's character type (0=Lazy, 1=Sporty)
# - S (Start): What's visible at the start (0=See_pizza, 1=See_hotdog)
#
# The script:
# 1. Loads target distributions
# 2. Fits causal models to predict path choices (short vs long)
# 3. Fits causal models to predict destination choices (pizza vs hotdog)
# 4. Uses model comparison to find the best causal structure
# 5. Visualizes the results and model predictions


# Load target distributions for path and destination choices
load(here('Exp1Prediction', 'Model', 'Data', 'targetDist.rda')) # td is the main one; also marginals td_path and td_destination 

# ==============================================================================
# Fit models to data
# ==============================================================================

# Test model fitting functionality and parameter transformations
wrapper(par =  rep(.5,12), struct = unlist(structures[10000,]), td = cbind(1-td_path, td_path)) # .88
wrapper(par =  rep(.5,12), struct = unlist(structures[10000,]), td = cbind(1-td_destination, td_destination)) # 1.08

out <- optim(wrapper, par = rep(.5,12), struct = unlist(structures[5,]), td = cbind(1-td_path, td_path))
out 
s <- 58941
out <- optim(wrapper, par = rep(.5,12), struct=unlist(structures[s,]), td = cbind(1-td_destination, td_destination))
out 

transform_params(par = out$par)
wrapper(par =  out$par, struct=unlist(structures[5,]), td = cbind(1-td_path, td_path))
# TO DO - test and understand this wrapper better

# Now actually do it: set up empty place to put results
n_structs <- nrow(structures)
fitted_path_mods <- matrix(NA, n_structs, 13)
fitted_destination_mods <- matrix(NA, n_structs, 13)

# Loop over all structures, fitting each to path and destination data
# Currently takes a few hours so set up progress bar
pb <- txtProgressBar(min = 0, max = n_structs, style = 3)

# Calls function 4 which calls 2 which calls 1
for (s in 1:n_structs) {
  fitted_path_mods[s, ] <- fit_structure(s, cbind(1 - td_path, td_path))
  fitted_destination_mods[s, ] <- fit_structure(s, cbind(1 - td_destination, td_destination))
  setTxtProgressBar(pb, s)
}
close(pb)
colnames(fitted_path_mods) <- colnames(fitted_destination_mods) <- c("kl", state_names, "br", "tau")
fitted_path_mods <- as.data.frame(fitted_path_mods)
fitted_destination_mods <- as.data.frame(fitted_destination_mods)

# Results in 2 dfs, each of 59049 obs of 13 vars
save(fitted_path_mods, 
     fitted_destination_mods, 
     structures, 
     file = here('Exp1Prediction', 'Model', 'Data', 'fitted.rda'))

