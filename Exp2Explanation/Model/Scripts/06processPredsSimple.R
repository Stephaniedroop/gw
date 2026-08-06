# ==============================================================================
## Process cesm data and do posterior marg/inference step
# ==============================================================================

library(here)
library(tidyverse)

load(here('Exp2Explanation', 'Model', 'Data', 'modelDataSimpleU.rda')) # from getPreds.R: food_preds and path_preds, each 512 of 32

# Also uses a var, uvar, defined in semUtilsSimple, but set it again here
uvars <- c(
  "Pu",
  "Ku",
  "Cu",
  "Su",
  "br"
)


# ------------ PATH processing ---------------

# Put node names from col to in a col. 4608 of 25
pathlong <- path_preds |>
  pivot_longer(
    cols = c(Pces:brces),
    names_to = c('node', '.value'),
    names_sep = -3
  )

# 4608 of 15
pathlong <- pathlong |>
  select(-(Pcfs:E_count))

# Node and value together for labelling
pathlong$node3 <- apply(pathlong, 1, function(r) {
  paste0(r["node"], "=", r[r["node"]])
})

# --------- FOOD processing -----------

# Put node names from col to in a col. 4608 of 25
foodlong <- food_preds |>
  pivot_longer(
    cols = c(Pces:brces),
    names_to = c('node', '.value'),
    names_sep = -3
  )

# 4608 of 15
foodlong <- foodlong |>
  select(-(Pcfs:E_count))

# Node and value together for labelling
foodlong$node3 <- apply(foodlong, 1, function(r) {
  paste0(r["node"], "=", r[r["node"]])
})

# ------

# A variable which is paste0 the value of all uvars in that row, eg 00000
# This is needed for the normalisation step, otherwise each ces score is made of 16 different ones - check with Neil
pathlong$uvars <- apply(pathlong, 1, function(r) {
  paste0(r[uvars], collapse = "")
})

foodlong$uvars <- apply(foodlong, 1, function(r) {
  paste0(r[uvars], collapse = "")
})
#
# # Now an id of each grouping of uvars
# pathlong$uvars_id <- as.numeric(factor(pathlong$uvars))
# foodlong$uvars_id <- as.numeric(factor(foodlong$uvars))

# Actually do need an id of grouping of unobs vars otherwise each ces score is made of 16 different ones - check with Neil

# Add on a column for lesioned models
pathlong$noSelect <- 1
foodlong$noSelect <- 1


# ------ get S_hat (first softmax step) -----

path_S_hat <- pathlong |>
  group_by(condition, uvars) |>
  mutate(
    s_hat_ces = exp(ces / .25) / sum(exp(ces / .25)),
    s_hat_noSelect = exp(noSelect / .25) / sum(exp(noSelect / .25))
  ) |>
  ungroup()

food_S_hat <- foodlong |>
  group_by(condition, uvars) |>
  mutate(
    s_hat_ces = exp(ces / .25) / sum(exp(ces / .25)),
    s_hat_noSelect = exp(noSelect / .25) / sum(exp(noSelect / .25))
  ) |>
  ungroup()

# This is combined S' and S~
path_ces <- path_S_hat |>
  group_by(condition, sem, node3) |>
  summarise(
    prior = sum(prior),
    uprior = sum(uprior),
    post = sum(posterior),
    postces = sum(posterior * s_hat_ces),
    postns = sum(posterior * s_hat_noSelect),
    noInf = sum(prior * s_hat_ces),
    noInf_ns = sum(prior * s_hat_noSelect)
  ) |>
  ungroup()

food_ces <- food_S_hat |>
  group_by(condition, sem, node3) |>
  summarise(
    prior = sum(prior),
    uprior = sum(uprior),
    post = sum(posterior),
    postces = sum(posterior * s_hat_ces),
    postns = sum(posterior * s_hat_noSelect),
    noInf = sum(prior * s_hat_ces),
    noInf_ns = sum(prior * s_hat_noSelect)
  ) |>
  ungroup()

# HERE would be info gain if we do it

# Might not need these?
# 418
# path_ces <- path_ces |>
#   group_by(condition) |>
#   mutate(
#     n_postces = postces / sum(postces),
#     n_postns = postns / sum(postns),
#     n_noInf = noInf / sum(noInf),
#     n_noInf_ns = noInf_ns / sum(noInf_ns)
#   ) |>
#   ungroup()
#
# # 406
# food_ces <- food_ces |>
#   group_by(condition) |>
#   mutate(
#     n_postces = postces / sum(postces),
#     n_postns = postns / sum(postns),
#     n_noInf = noInf / sum(noInf),
#     n_noInf_ns = noInf_ns / sum(noInf_ns)
#   ) |>
#   ungroup()

# How many rows are in each condition? (This was for writing up the computational step for s_hat)
# pc <- path_ces |>
#   group_by(condition) |>
#   summarise(n = n()) |>
#   ungroup()
#
# fc <- food_ces |>
#   group_by(condition) |>
#   summarise(n = n()) |>
#   ungroup()
#
# # Now count how many times each N occurs: this is the N in the first softmax, for S_hat
# pcc <- pc |>
#   group_by(n) |>
#   summarise(count = n()) |>
#   ungroup()
#
# fcc <- fc |>
#   group_by(n) |>
#   summarise(count = n()) |>
#   ungroup()

# meanrawpath <- pathlong |>
#   group_by(condition, node3) |>
#   summarise(meances = mean(ces)) |>
#   ungroup()

save(
  path_ces,
  food_ces,
  file = here('Exp2Explanation', 'Model', 'Data', 'ces_sepSimpleN.rda')
)
