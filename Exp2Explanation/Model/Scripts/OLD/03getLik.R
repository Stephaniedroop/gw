# ==============================================================================
## Get likelihood of each setting of unobserved vars under each combination of observed vars
# ==============================================================================

library(here)
library(tidyverse)

load(here('Exp2Explanation', 'Model', 'Data', 'params.rda'))
# This was saved in 02getParams - that is currently the latest work on this (late Oct)

# Exp1 modelutils has the function to calculate effect using each functional form
# I need a way to apply the get_mod_pred general function to combinations of vars,
# now that the separate path and food causal models have been derived

# So does the observed functional form for path look like:
# struct = the path and food causal models: vectors of which vars are 111 -1
# so we need to pull out just the allworlds that are 1 and -1?
# and make an 'absolute' functional form from these, to compare all other rows to?

source(here('Exp1Prediction', 'Model', 'Scripts', 'modelUtils.R'))
# In that, state_mat is all static combinations of 0,1 for just the four experiment vars and all interactions
# There are no u-vars and no causal strenghts
# to the whole 1:16, 1:10 matrix is applied the functional form using which of the STRUCTURE (is best fitting causal model from dags) is 1 and -1
# So this structure can be defined once now and kept as that, and then matched to each row of the state_mat
# But we need to bring in the matching supporting u vars too

struct <- best_food == 1
nor_idx <- which(struct == 1) # or which(best_food == 1)
base_names <- names(nor_idx)
u_names <- c(paste0(base_names, "u"))

# Then get all the combos of base_names, and all the combos of u_names, as logical

state_mat_base <- expand.grid(replicate(
  length(base_names),
  c(0, 1),
  simplify = FALSE
))
#colnames(state_mat_base) <- base_names

# Do the same for the u vars
state_mat_u <- expand.grid(replicate(
  length(u_names),
  c(0, 1),
  simplify = FALSE
))
#colnames(state_mat_u) <- u_names

# For example this gives the intermediate results of the conjunction pairs, as long as boht inputs same size
pair_list <- Map(
  function(x, y) as.logical(x) & as.logical(y),
  state_mat_base,
  state_mat_u
)

# Then THESE are the input to the noisy-or/and not.

# So what are the inputs to this - it is all the

nor_idx <- which(struct == 1)
# I know we can just do the logical directly if we have a separate vector of the u vars and their valeus

nandnot_idx <- which(struct == -1)
one_minus_s <- 1 - par$s

#here state_mat will be not just the expt vars but also all u vars, like from eg allworlds.
# This is, best_food is just the four expt vars and their interactions,
# but we need a functional form with the u vars too
# So run the determinative conjunctions first and then go into the noisy-or etc

# Calculations
p <- sapply(1:nrow(state_mat), function(i) {
  state <- state_mat[i, ]
  nor <- c(1 - par$br, (one_minus_s[nor_idx]^state[nor_idx]))
  nandnot <- (one_minus_s[nandnot_idx]^state[nandnot_idx])
  (1 - prod(nor)) * prod(nandnot) # the first part from prod(nor) is the base rate taken forwards to the preventative. we assume gen first then prevent after
})

# In the following code, we compute the likelihood of each possible setting of the unobserved
# variables (i.e., the variables not in obs_vars) under each combination of the observed
# variables (i.e., the variables in obs_vars).
