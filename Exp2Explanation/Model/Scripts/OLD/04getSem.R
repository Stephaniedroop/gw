# ==============================================================================
## Get likelihood and conditional probs, sem etc
# ==============================================================================

library(here)
library(tidyverse)


source(here('Exp2Explanation', 'Model', 'Scripts', 'semUtils.R')) # determinative functions and some variable name vectors

# Load data from the previous getParams script
load(here('Exp2Explanation', 'Model', 'Data', 'probscombos.rda'))


all_path <- all_combos
all_food <- all_combos

# --------- Apply sem and condition tags ---------

all_path$sem <- pathlik_vec(all_path)
all_food$sem <- foodlik_vec(all_food)

# Get prior of unobs settings for both GET NUMBER
all_food$prior <- Reduce(`*`, all_food_probs[causes1]) # QUESTION: all sampled vars, or just the unobserved?!
all_path$prior <- Reduce(`*`, all_path_probs[causes1])

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
all_food <- all_food |>
  group_by(condition) |> # or group! depends what you decide later on
  mutate(posterior = prior / sum(prior)) |> # IF IT IS PRIO OF UNOBSERVED ONLY THEN IN 05PROCESS THE GETPOST NEEDS THE WHOLE CONDIITON
  ungroup()

all_path <- all_path |>
  group_by(condition) |> # or group! depends what you decide later on
  mutate(posterior = prior / sum(prior)) |> # IF IT IS PRIO OF UNOBSERVED ONLY THEN IN 05PROCESS THE GETPOST NEEDS THE WHOLE CONDIITON
  ungroup()


# Made an id for each setting of 'relevant' u-vars but this might not be a good var because it is hard coded and also doesn't distinguish vars well
all_path <- all_path |>
  group_by(condition, Ku, KSu, br) |>
  mutate(u_set = cur_group_id()) |>
  ungroup()

all_food <- all_food |>
  group_by(condition, Pu, PSu, CSu, KSu, br) |>
  mutate(u_set = cur_group_id()) |>
  ungroup()


# Save. The full dfs are 32.7k obs, which is 2^15, ie combinations of:
# - observed vars P, C, K, S, their unobs vars (4), br, and unobs vars of interactions (6)
# but NOT outcome (1) or the observed interactions (6) which are set by sem
save(
  all_path,
  all_food,
  file = here('Exp2Explanation', 'Model', 'Data', 'scenarios.rda')
)
