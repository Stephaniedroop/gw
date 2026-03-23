# ==============================================================================
## Reorganise and represent cesm data raedy for plotting
# ==============================================================================

library(here)
library(tidyverse)

load(here('Exp2Explanation', 'Model', 'Data', 'modelData.rda')) # from getPreds.R: food_preds and path_preds, each 32.7k of 57
#path <- read.csv('pathtest.csv')

# Just this time: merge in all_food and all_path with the right posterior: next time it will be correct from start
# Replace column 'prior' in path_preds with column 'prior' from all_path
path_preds$prior <- all_path$prior
food_preds$prior <- all_food$prior
path_preds$posterior <- all_path$posterior
food_preds$posterior <- all_food$posterior

# check the data in cols P:CS are the same for both food_preds and path_preds
check <- all(path_preds[, 1:21] == food_preds[, 1:21])

all_preds


# ------------ PATH processing ---------------

# There are always 4+4+6+1 vars getting a score
pathlong <- path_preds |>
  pivot_longer(
    cols = c(Pces:brces),
    names_to = c('node', '.value'),
    names_sep = -3
  )

pathlong <- pathlong |>
  select(-(Pcfs:E_count))

pathlong$node3 <- apply(pathlong, 1, function(r) {
  paste0(r["node"], "=", r[r["node"]])
})

# --------- FOOD processing -----------

foodlong <- food_preds |>
  pivot_longer(
    cols = c(Pces:brces),
    names_to = c('node', '.value'),
    names_sep = -3
  )

foodlong <- foodlong |>
  select(-(Pcfs:E_count))

foodlong$node3 <- apply(foodlong, 1, function(r) {
  paste0(r["node"], "=", r[r["node"]])
})


# Get the marginalised ces scores for each variables
path_ces <- pathlong |>
  group_by(condition, sem, node3) |>
  summarise(postces = sum(posterior * ces)) |>
  ungroup()

path_ces <- path_ces |>
  group_by(condition) |>
  mutate(postces_norm = postces / sum(postces)) |>
  ungroup()

food_ces <- foodlong |>
  group_by(condition, sem, node3) |>
  summarise(postces = sum(posterior * ces)) |>
  ungroup()

food_ces <- food_ces |>
  group_by(condition) |>
  mutate(postces_norm = postces / sum(postces)) |>
  ungroup()


save(
  path_ces,
  food_ces,
  file = here('Exp2Explanation', 'Model', 'Data', 'ces_sep.rda')
)


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
unobs_igp <- getpostp |>
  group_by(condition, node3) |> # what about u_set as well
  summarise(
    prior_entropy = round(-sum(prior * log2(prior + 1e-10)), 3),
    post_entropy = round(-sum(post * log2(post + 1e-10)), 3),
    ig = round(prior_entropy - post_entropy, 3)
  ) |>
  ungroup()

# This will be 288 obs, same size as data and ppts, in the eventual likelihood, remember to save it with mp
ig <- unobs_ig |>
  select(condition, ig)

# Other considerations
# - then map to participant data
# - set actual?
# - all this before any kind of plotting

# Think along lines of structure it eventually as the four observed variables and the two outcomes: 64
# For presenting is different than the causal modelling.

# The two outcomes then get MERGED as a multiplication of p=1*p=1 etc

# This might happen before the optimisation etc?

# ----------- FOOD -----------------
