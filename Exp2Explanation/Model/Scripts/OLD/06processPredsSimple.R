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

# Actually do need an id of grouping of unobs vars otherwise each ces score is made of 16 different ones - check with Neil

# Add on a column for lesioned models
pathlong$noSelect <- 1
foodlong$noSelect <- 1


# New section, testing it out, teking a new bit to a separate call script, and 6,7 become fucntions it calls

save(
  pathlong,
  foodlong,
  file = here('Exp2Explanation', 'Model', 'Data', 'preds_long.rda')
)


# OLD>>>>> haven't decided whether to keep yet

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
    # prior = sum(prior),
    # uprior = sum(uprior),
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
    # prior = sum(prior),
    # uprior = sum(uprior),
    post = sum(posterior),
    postces = sum(posterior * s_hat_ces),
    postns = sum(posterior * s_hat_noSelect),
    noInf = sum(prior * s_hat_ces),
    noInf_ns = sum(prior * s_hat_noSelect)
  ) |>
  ungroup()

# Intersperse with unmodell-able node3 vals: some ppl did select them (but in a principled way not noise, so won't use eps)
nodes <- unique(sub("=.*", "", pathlong$node3))
all_node3 <- paste0(rep(nodes, each = 2), "=", 0:1)

path_ces <- path_ces |>
  group_by(condition) |>
  complete(
    node3 = all_node3,
    fill = list(
      # prior = 0,
      # uprior = 0,
      post = 0,
      postces = 0,
      postns = 0,
      noInf = 0,
      noInf_ns = 0
    )
  ) |>
  ungroup()

food_ces <- food_ces |>
  group_by(condition) |>
  complete(
    node3 = all_node3,
    fill = list(
      # prior = 0,
      # uprior = 0,
      post = 0,
      postces = 0,
      postns = 0,
      noInf = 0,
      noInf_ns = 0
    )
  ) |>
  ungroup()

save(
  path_ces,
  food_ces,
  file = here('Exp2Explanation', 'Model', 'Data', 'ces_sepSimpleN.rda')
)
