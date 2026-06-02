# -------------------------------------------------------
# ----- DATA AND FUNCTIONS FOR DETERMINATIVE SEMS ------
# -------------------------------------------------------

base_causes <- c("P", "K", "C", "S") # probably rewrite alphabetically? If want to do that, also go back to much earlier Exp1 model


# Used more in getPreds than in getProbs
causes1 <- c(
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
  "KSu",
  "PSu",
  "CSu",
  "br"
)

# These definitely needed for the sem outcomes
path_obs <- c("K", "KS")
food_obs <- c("P", "PS", "CS", "KS")
food_uvar <- c("Pu", "PSu", "CSu", "KSu")
path_uvar <- c("Ku", "KSu")


uvars <- c(
  "Pu",
  "Ku",
  "Cu",
  "Su",
  "PKu",
  "PCu",
  "PSu",
  "KCu",
  "KSu",
  "CSu",
  "br"
) # change when decide whether rename _p and _f

orig_nodes <- c("P", "Pu", "K", "Ku", "C", "Cu", "S", "Su", "br") # br and interaction will be renamed in the specific ones
interaction_u <- c("PKu", "PCu", "PSu", "KCu", "KSu", "CSu")
vars <- c(orig_nodes, interaction_u)

add_interactions <- function(df, base_causes) {
  combs <- combn(base_causes, 2)
  new_cols <- apply(combs, 2, function(pair) {
    as.numeric(df[[pair[1]]] & df[[pair[2]]])
  })
  colnames(new_cols) <- apply(combs, 2, paste0, collapse = "")
  cbind(df, new_cols)
}


# --------- Likelihood functions: deterministic sem ------

foodlik_vec <- function(df) {
  obs <- df[food_obs]
  unobs <- df[food_uvar]
  r <- df$br
  active_causes <- Reduce(`|`, Map(`&`, obs, unobs))
  active_causes | r
}

pathlik_vec <- function(df) {
  gen_obs <- df[[path_obs[1]]]
  gen_unobs <- df[[path_uvar[1]]]
  prev_obs <- df[[path_obs[2]]]
  prev_unobs <- df[[path_uvar[2]]]
  r <- df$br
  gen_part <- r | (gen_obs & gen_unobs)
  prevent_active <- (prev_obs & prev_unobs)
  gen_part & !prevent_active
}
