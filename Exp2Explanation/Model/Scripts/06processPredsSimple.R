# ==============================================================================
## Reorganise and represent cesm data raedy for plotting
# ==============================================================================

library(here)
library(tidyverse)

load(here('Exp2Explanation', 'Model', 'Data', 'modelDataSimple.rda')) # from getPreds.R: food_preds and path_preds, each 512 of 32

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

# Now an id of each grouping of uvars
pathlong$uvars_id <- as.numeric(factor(pathlong$uvars))
foodlong$uvars_id <- as.numeric(factor(foodlong$uvars))


# Actually do need an id of grouping of unobs vars otherwise each ces score is made of 16 different ones - check with Neil

# ------ get S_hat (first normalisation step) -----

path_S_hat <- pathlong |>
  group_by(condition, uvars) |>
  mutate(s_hat = exp(ces / .25) / sum(exp(ces / .25))) |>
  ungroup()

food_S_hat <- foodlong |>
  group_by(condition, uvars) |>
  mutate(s_hat = exp(ces / .25) / sum(exp(ces / .25))) |>
  ungroup()


# Get the marginalised ces scores for each variables - S~ from collider paper
path_ces <- path_S_hat |>
  group_by(condition, sem, node3) |>
  summarise(postces = sum(posterior * s_hat)) |>
  ungroup()

path_ces <- path_ces |>
  group_by(condition) |>
  mutate(postces_norm = postces / sum(postces)) |>
  ungroup()

food_ces <- food_S_hat |>
  group_by(condition, sem, node3) |>
  summarise(postces = sum(posterior * s_hat)) |>
  ungroup()

food_ces <- food_ces |>
  group_by(condition) |>
  mutate(postces_norm = postces / sum(postces)) |>
  ungroup()

# meanrawpath <- pathlong |>
#   group_by(condition, node3) |>
#   summarise(meances = mean(ces)) |>
#   ungroup()

save(
  path_ces,
  food_ces,
  file = here('Exp2Explanation', 'Model', 'Data', 'ces_sepSimple.rda')
)


# A version with no unobserved interactions vars
# or do it right there in script 8

# Get ig [[[LATER]]]

getpostp <- pathlong |>
  #filter(!node2 %in% c('A', 'B')) |>
  group_by(condition, node3, .drop = F) |> # or condition?!
  summarise(prior = sum(prior), post = sum(posterior), ces = sum(ces)) # can't just sum the prior without dividing out the unused ones

getpostf <- foodlong |>
  #filter(!node2 %in% c('A', 'B')) |>
  group_by(condition, node3, .drop = F) |> # or condition?!
  summarise(prior = sum(prior), post = sum(posterior), ces = sum(ces))

# These then treat further: multiply post and ces
getpostf <- getpostf |>
  mutate(postces = post * ces)

getpostp <- getpostp |>
  mutate(postces = post * ces)

# And normalise. But this can't be right, cos the ces is not meant to have options with negative ces?!
getpostf <- getpostf |>
  group_by(condition) |>
  mutate(postces_norm = postces / sum(postces)) |>
  ungroup()

# 16 Mar the problem is how to marginalise. get the code from collider - where the first normalisation happens - were some scores there not constrained to 1?
# find the note to Neil about posterior normalisation in slack
# If we were really following the collider paper, there would be the first softmax here: for each combination of vars even not allowable, normalise the ces
# Try from collider for the 'goOptim.rda' and see how it is done there.

# ---- Later .... ------

# Simple ig of each pair of unobserved vars
# unobs_igp <- getpostp |>
#   group_by(condition, node3) |> # what about u_set as well
#   summarise(
#     prior_entropy = round(-sum(prior * log2(prior + 1e-10)), 3),
#     post_entropy = round(-sum(post * log2(post + 1e-10)), 3),
#     ig = round(prior_entropy - post_entropy, 3)
#   ) |>
#   ungroup()
#
# # This will be 288 obs, same size as data and ppts, in the eventual likelihood, remember to save it with mp
# ig <- unobs_ig |>
#   select(condition, ig)

# Other considerations
# - then map to participant data
# - set actual?
# - all this before any kind of plotting

# Think along lines of structure it eventually as the four observed variables and the two outcomes: 64
# For presenting is different than the causal modelling.

# The two outcomes then get MERGED as a multiplication of p=1*p=1 etc

# This might happen before the optimisation etc?

# ----------- FOOD -----------------
