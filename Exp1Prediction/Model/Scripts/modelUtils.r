# ==============================================================================
# Model Prediction Functions  
# ==============================================================================

# --------- 0. Static vars ---------
# First create indexing grid for all possible situations - 16 obs of 4
ix <- expand.grid(S = 0:1, 
                  C = 0:1,
                  K = 0:1,
                  P = 0:1)

# Keep it in same order as rest of code
ix <- ix[, c("P", "K", "C", "S")]
rownames(ix) <- NULL

# Add interaction columns
state_names <- c("P", "K", "C", "S", "PK", "PC", "PS", "KC", "KS", "CS")

# Use the predefined ix
states <- ix
states$PK <- states$P * states$K
states$PC <- states$P * states$C
states$PS <- states$P * states$S
states$KC <- states$K * states$C
states$KS <- states$K * states$S
states$CS <- states$C * states$S

state_mat <- as.matrix(states[, state_names])


# ----------- Define model structures and initial parameters -------------
# This section creates all possible causal structures (3^10 combinations)
# and sets up initial parameters for model fitting

# Define all possible causal structures
#Each variable can be:
#  1: positive influence
#  0: no influence
# -1: negative influence

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
  # Precompute indices
  nor_idx <- which(struct == 1)
  nandnot_idx <- which(struct == -1)
  one_minus_s <- 1 - par$s
  
  # Calculations
  p <- sapply(1:nrow(state_mat), function(i) {
    state <- state_mat[i, ]
    nor <- c(1 - par$br, (one_minus_s[nor_idx] ^ state[nor_idx]))
    nandnot <- (one_minus_s[nandnot_idx] ^ state[nandnot_idx])
    (1 - prod(nor)) * prod(nandnot) # the first part from prod(nor) is the base rate taken forwards to the preventative. we assume gen first then prevent after 
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



#----  4. Function to fit a specific causal structure to target distribution data -----
# Takes a structure row index and target distribution, returns KL divergence and fitted parameters
# @param struct_row Row index in structures dataframe specifying which causal structure to fit
# @param td Target distribution matrix (2-column: probability of outcome 0 and 1)
# @return Named vector with KL divergence and transformed parameters (strengths, base rate, temperature)
fit_structure <- function(struct_row, td) {
  optim_res <- optim(
    wrapper, # Function 2
    par = rep(.5, 12),
    struct = unlist(structures[struct_row, ]),
    td = td
  )
  params <- transform_params(optim_res$par)
  c(kl = optim_res$value, setNames(params, c(state_names, "tau")))
}
