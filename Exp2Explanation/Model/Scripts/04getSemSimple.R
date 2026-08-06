# ==============================================================================
## Get likelihood and conditional probs, sem etc
# ==============================================================================

library(here)
library(tidyverse)


source(here('Exp2Explanation', 'Model', 'Scripts', 'semUtilsSimple.R')) # determinative functions and some variable name vectors

# Load data from the previous getParams script
load(here('Exp2Explanation', 'Model', 'Data', 'probscombosSimple.rda')) # all_combos, all_food_probs, all_path_probs, from 03getProbsSimple.r
# Load model
#load(here('Exp1Prediction', 'Model', 'Data', 'modelSimple.rda')) # from exp1 03processModel.r . This alradyloaded in semUtils

# food_pairs <- structure_to_pairs(best_food)
# path_pairs <- structure_to_pairs(best_path)

all_path <- all_combos
all_food <- all_combos

# --------- Apply sem and condition tags ---------

all_food$sem <- sem_lik(
  all_food,
  gen_pairs = food_pairs$gen,
  prev_pairs = food_pairs$prev
)

all_path$sem <- sem_lik(
  all_path,
  gen_pairs = path_pairs$gen,
  prev_pairs = path_pairs$prev
)

# Need two priors: one for the whole situation, and one for the uvars
all_food$prior <- Reduce(`*`, all_food_probs[vars]) # whole situation
all_path$prior <- Reduce(`*`, all_path_probs[vars])

#all_food$baseprior <- Reduce(`*`, all_food_probs[base_causes]) # whole situation
#all_path$baseprior <- Reduce(`*`, all_path_probs[base_causes])

all_food$uprior <- Reduce(`*`, all_food_probs[uvars]) # just uvars
all_path$uprior <- Reduce(`*`, all_path_probs[uvars])

# Get condition tag
all_path <- all_path |>
  group_by(P, K, C, S, sem) |>
  mutate(condition = paste0('c', P, K, C, S, as.numeric(sem))) |>
  ungroup()

all_path$condition <- as.factor(all_path$condition)

all_food <- all_food |>
  group_by(P, K, C, S, sem) |>
  mutate(condition = paste0('c', P, K, C, S, as.numeric(sem))) |>
  ungroup()

all_food$condition <- as.factor(all_food$condition)


# Now group by condition to give id for unobs setting and also get posterior
# THIS posterior, still separate, is what goes to 06processPreds for calc with s_hat
all_food <- all_food |>
  group_by(condition) |> # or group! depends what you decide later on
  mutate(posterior = uprior / sum(uprior)) |> # IF IT IS PRIO OF UNOBSERVED ONLY THEN IN 05PROCESS THE GETPOST NEEDS THE WHOLE CONDIITON
  ungroup()

all_path <- all_path |>
  group_by(condition) |> # or group! depends what you decide later on
  mutate(posterior = uprior / sum(uprior)) |> # IF IT IS PRIO OF UNOBSERVED ONLY THEN IN 05PROCESS THE GETPOST NEEDS THE WHOLE CONDIITON
  ungroup()


#  so column 1:
#pFood*pPath, column 2: pFood*(1-pPath), column 3: (1-pFood)*pPath, column 4: (1-pFood)*(1-pPath)

# Also we need pChoice to get surprisingness
# This is a different prior from before: this is just for the whole situation, not individual variables
pFood <- all_food |>
  group_by(P, K, C, S, sem) |>
  summarise(prior = sum(uprior)) |>
  ungroup()

pPath <- all_path |>
  group_by(P, K, C, S, sem) |>
  summarise(prior = sum(uprior)) |>
  ungroup()

allP <- merge(
  pFood,
  pPath,
  by = c('P', 'K', 'C', 'S'),
  suffixes = c('_food', '_path')
)
allP$pChoice <- allP$prior_food * allP$prior_path

# allP <- allP |>
#   group_by(P, K, C, S) |>
#   mutate(pChoiceNorm = pChoice / sum(pChoice)) |>
#   ungroup()

allP$condObs <- paste0(allP$P, allP$K, allP$C, allP$S)

# If sem_food==FALSE & sem_path==FALSE then choice==ShortPizza, if sem_food==TRUE & sem_path==TRUE then choice LongHotdog, if one is true and the other false then choice is .5
allP$choice <- if_else(
  allP$sem_food == FALSE & allP$sem_path == FALSE,
  'ShortPizza',
  if_else(
    allP$sem_food == FALSE & allP$sem_path == TRUE,
    'LongPizza',
    if_else(
      allP$sem_food == TRUE & allP$sem_path == FALSE,
      'ShortHotdog',
      if_else(
        allP$sem_food == TRUE & allP$sem_path == TRUE,
        'LongHotdog',
        'Other'
      )
    )
  )
)

# Merge all_path and all_food on observed base variables only? Nah - cant' do this by merging because there are different numbers of each so some get repeated
# all_sem <- merge(
#   all_path,
#   all_food,
#   by = c('P', 'K', 'C', 'S'),
#   suffixes = c('_path', '_food')
# )
#
# all_sem <- all_sem |>
#   group_by(P, K, C, S) |>
#   mutate(
#     pFood = sum(prior_food),
#     pPath = sum(prior_path),
#     postFood = sum(posterior_food),
#     postPath = sum(posterior_path)
#   ) |>
#   ungroup()

# Save. The full dfs are 32.7k obs, which is 2^15, ie combinations of:
# - observed vars P, C, K, S, their unobs vars (4), br, and unobs vars of interactions (6)
# but NOT outcome (1) or the observed interactions (6) which are set by sem
save(
  all_path,
  all_food,
  allP,
  file = here('Exp2Explanation', 'Model', 'Data', 'scenariosSimple.rda')
)
