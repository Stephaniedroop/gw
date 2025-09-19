###############################################################################################
############ Select best-fitting causal model by minimising K-L divergence #############
###############################################################################################

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

rm(list=ls())
library(tidyverse)

# Load target distributions for path and destination choices
load('../Data/targetDist.rda') # td is the main one; also marginals td_path and td_destination 



###############################################################################################
# Define model structures and parameters
###############################################################################################

# This section creates all possible causal structures (3^10 combinations)
# and sets up initial parameters for model fitting

# Define all possible causal structures
#Each variable can be:
#  1: positive influence
#  0: no influence
# -1: negative influence

state_names <- c("P", "K", "C", "S", "PK", "PC", "PS", "KC", "KS", "CS")

# 59049 obs of 10 vars (3^10)
structures <- expand.grid(P = -1:1,
                          K = -1:1,
                          C = -1:1,
                          S = -1:1, 
                          PK = -1:1,
                          PC = -1:1,
                          PS = -1:1,
                          KC = -1:1,
                          KS = -1:1,
                          CS = -1:1)

init_full_par <- list(s = c(P = .5,
                          K = .5,
                          C = .5,
                          S = .5,
                          PK = .5,
                          PC = .5,
                          PS = .5,
                          KC = .5,
                          KS = .5,
                          CS = .5),
                    br = .5, # Base rate
                    tau = 1
)

# Create indexing grid for all possible situations - 16 obs of 4
ix <- expand.grid(S = 0:1, 
                  C = 0:1,
                  K = 0:1,
                  P = 0:1)

# Keep it in same order as rest of code
ix <- ix[, c("P", "K", "C", "S")]
rownames(ix) <- NULL


# ==============================================================================
# Model Prediction Functions  
# ==============================================================================


#--- 1. Function to predict probabilities for outcomes given a causal structure and parameters -----

# The model uses a noisy-OR/noisy-AND-NOT combination:
# - Positive influences combine via noisy-OR
# - Negative influences combine via noisy-AND-NOT  
# - These combine by NOISY-AND-NOT(NOISY-OR(positive_causes), preventative_causes)
# - Final probability is transformed via softmax

# @param struct Vector indicating causal influences (-1=negative, 0=none, 1=positive)
# @param par List of parameters (strength values and temperature)
# @return Vector of probabilities for each situation

get_mod_pred <- function(struct, par) {
  # Use the predefined ix (created outside)
  states <- ix
  states$PK <- states$P * states$K
  states$PC <- states$P * states$C
  states$PS <- states$P * states$S
  states$KC <- states$K * states$C
  states$KS <- states$K * states$S
  states$CS <- states$C * states$S

  state_mat <- as.matrix(states[, state_names])
  
  # Precompute indices
  nor_idx <- which(struct == 1)
  nandnot_idx <- which(struct == -1)
  one_minus_s <- 1 - par$s
  
  # Vectorized calculations
  p <- sapply(1:nrow(state_mat), function(i) {
    state <- state_mat[i, ]
    nor <- c(1 - par$br, (one_minus_s[nor_idx] ^ state[nor_idx]))
    nandnot <- (one_minus_s[nandnot_idx] ^ state[nandnot_idx])
    (1 - prod(nor)) * prod(nandnot)
  })
  
  # Softmax
  out <- data.frame(p0 = 1 - p, p1 = p)
  out <- exp(as.matrix(out) / par$tau)
  out <- out / rowSums(out)
  out[, "p1"]
}

#--- 2. Wrapper for optimization -----
# Objective function wrapper for parameter optimization
# Transforms unconstrained parameters, computes model predictions, and calculates KL divergence
# @param par Unconstrained parameter vector (logit-transformed strengths + log tau)
# @param struct Causal structure specification 
# @param td Target distribution matrix (observed probabilities)
# @return KL divergence between target and model distributions

wrapper <- function(par, struct, td) {
  s_vals <- plogis(par[1:10])
  names(s_vals) <- state_names

  br_val <- plogis(par[11])
  tau_val <- exp(par[12])

  input_pars <- list(
    s = s_vals,
    br = br_val,
    tau = tau_val
  )

  model_pred <- get_mod_pred(struct, input_pars)
  model_probs <- cbind(1 - model_pred, model_pred)
  kl_div <- sum(rowSums(td * log(td / model_probs)))

  return(kl_div)
}

#-------  3. Function to transform parameters back to original scale --------
transform_params <- function(par) {
  c(plogis(par[1:11]), exp(par[12]))
}

# ==============================================================================
# Fit models to data
# ==============================================================================

# Test model fitting functionality and parameter transformations
wrapper(par =  rep(.5,12), struct = unlist(structures[10000,]), td = cbind(1-td_path, td_path))
wrapper(par =  rep(.5,12), struct = unlist(structures[10000,]), td = cbind(1-td_destination, td_destination))

out <- optim(wrapper, par = rep(.5,12), struct = unlist(structures[5,]), td = cbind(1-td_path, td_path))
out 
s <- 58941
out <- optim(wrapper, par = rep(.5,12), struct=unlist(structures[s,]), td = cbind(1-td_destination, td_destination))
out 

tmp <- c(1/(1+exp(-out$par[1:11])),
       exp(out$par[12]))
tmp

transform_params(par = out$par)
wrapper(par =  out$par, struct=unlist(structures[5,]), td = cbind(1-td_path, td_path))


#----  4. Function to fit a specific causal structure to target distribution data -----
# Takes a structure row index and target distribution, returns KL divergence and fitted parameters
# @param struct_row Row index in structures dataframe specifying which causal structure to fit
# @param td Target distribution matrix (2-column: probability of outcome 0 and 1)
# @return Named vector with KL divergence and transformed parameters (strengths, base rate, temperature)
fit_structure <- function(struct_row, td) {
  optim_res <- optim(
    wrapper,
    par = rep(.5, 12),
    struct = unlist(structures[struct_row, ]),
    td = td
  )
  params <- transform_params(optim_res$par)
  c(kl = optim_res$value, setNames(params, c(state_names, "tau")))
}

# Now actually do it: set up empty place to put results
n_structs <- nrow(structures)
fitted_path_mods <- matrix(NA, n_structs, 13)
fitted_destination_mods <- matrix(NA, n_structs, 13)

# Loop over all structures, fitting each to path and destination data
# Currently takes a few hours so set up progress bar
pb <- txtProgressBar(min = 0, max = n_structs, style = 3)
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
save(file='../Data/fitted.rdata', fitted_path_mods, fitted_destination_mods, structures) 

