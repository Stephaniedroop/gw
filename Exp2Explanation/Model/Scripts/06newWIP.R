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


source(here('Exp2Explanation', 'Model', 'Scripts', 'run_les.R'))

TAU1 <- 1

path_ces <- run_ces(pathlong, TAU1)
food_ces <- run_ces(foodlong, TAU1)

save(
  path_ces,
  food_ces,
  file = here('Exp2Explanation', 'Model', 'Data', 'ces_sepSimpleN.rda')
)


# New section, testing it out, teking a new bit to a separate call script, and 6,7 become fucntions it calls

save(
  pathlong,
  foodlong,
  file = here('Exp2Explanation', 'Model', 'Data', 'preds_long.rda')
)
