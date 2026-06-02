# -------------------------------------------------------
# ----- DATA AND FUNCTIONS FOR DETERMINATIVE SEMS ------
# -------------------------------------------------------

# A rewrite mid April 2026, using a causal model without interactions.

# Causal structure from model selection, needed to know which vars are causally active for the sem. Unusual in a utils file but seems best way
load(here('Exp1Prediction', 'Model', 'Data', 'modelSimple.rda')) # loads best_food, best_path


base_causes <- c("P", "K", "C", "S") # probably rewrite alphabetically? If want to do that, also go back to much earlier Exp1 model


uvars <- c(
  "Pu",
  "Ku",
  "Cu",
  "Su",
  "br"
) # change when decide whether rename _p and _f

vars <- c("P", "Pu", "K", "Ku", "C", "Cu", "S", "Su", "br")

# --------- Define causal structures as data -----------

# ---------- Structure model to pairs ----------

structure_to_pairs <- function(structure, uvar_suffix = "u") {
  gen_pairs <- lapply(names(structure)[structure == 1], function(v) {
    c(obs = v, uvar = paste0(v, uvar_suffix))
  })
  prev_pairs <- lapply(names(structure)[structure == -1], function(v) {
    c(obs = v, uvar = paste0(v, uvar_suffix))
  })

  list(gen = gen_pairs, prev = prev_pairs)
}

food_pairs <- structure_to_pairs(best_food)
path_pairs <- structure_to_pairs(best_path)

# --------- Likelihood functions: deterministic sem ------

# @Inputs of a list of generative pairs and a list of preventative pairs,
# where each pair is a list with "obs" and "uvar" keys, and the name of the base rate column (default "br")

sem_lik <- function(
  df,
  gen_pairs = list(),
  prev_pairs = list(),
  br_col = "br"
) {
  # Gen: the init puts the base rate even when no generative part
  r <- df[[br_col]]
  gen_part <- Reduce(
    `|`,
    lapply(gen_pairs, function(p) df[[p["obs"]]] & df[[p["uvar"]]]),
    init = r
  )

  # Prev: this needs conditional else will error on nothing
  if (length(prev_pairs) == 0) {
    return(gen_part)
  }
  prev_active <- Reduce(
    `|`,
    lapply(prev_pairs, function(p) df[[p["obs"]]] & df[[p["uvar"]]])
  )
  gen_part & !prev_active
}
