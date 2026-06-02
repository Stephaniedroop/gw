# ==============================================================================
## Reorganise cesm data to combine path and food scores
# ==============================================================================

library(here)
library(tidyverse)
library(ggplot2)
library(stringr)


#load(here('Exp2Explanation', 'Model', 'Data', 'tag_counts.Rda'))
load(here('Exp2Explanation', 'Model', 'Data', 'ces_sepSimpleN.Rda')) # 406-418 of 5 vars came from script 6 nothing in between
#df <- read.csv(here('Exp2Explanation', 'Experiment', 'Data', 'maincoded.csv')) # 1887 of 11
# All the vars are there but they are not called the same things as in annotated data

# 2. ------ Process ces scores --------

# Categorise food_ces: if condition ends in 0 then put 'Pizza', if ends in 1 then 'Hotdog'
food_ces <- food_ces |>
  mutate(
    cat = case_when(
      str_ends(condition, '0') ~ 'Pizza',
      str_ends(condition, '1') ~ 'Hotdog',
      TRUE ~ 'other'
    ),
    condObs = substr(condition, 2, 5),
    # removes the '=0' etc; still not sure what it does or if we need it, to decide
    present_causes = sub("=.*", "", node3) # this will give us the present causes in the same format as the tag_counts, but we will need to split it by '.' and then paste together the ones that are 1s
  )

path_ces <- path_ces |>
  mutate(
    cat = case_when(
      str_ends(condition, '0') ~ 'Short',
      str_ends(condition, '1') ~ 'Long',
      TRUE ~ 'other'
    ),
    condObs = substr(condition, 2, 5),
    present_causes = sub("=.*", "", node3)
  )

# We have br and others in BOTH Path and Food so get them separate
# Which ones changes if the model changes: it is whichever have two edges coming out in the graph
# aka which has 1/-1 in same position in both best_path and best_food in modelSimple.rda

path_ces$node3 <- gsub("^(br|Pu|Ku)=", "\\1_p=", path_ces$node3)
food_ces$node3 <- gsub("^(br|Pu|Ku)=", "\\1_f=", food_ces$node3)


pizza_ces <- food_ces |>
  filter(cat == 'Pizza')

hotdog_ces <- food_ces |>
  filter(cat == 'Hotdog')

short_ces <- path_ces |>
  filter(cat == 'Short')

long_ces <- path_ces |>
  filter(cat == 'Long')

# RENAME NODE3 TO BE SPECIFIC AT THIS POINT??!

# Or - One unobserved state will always have a negative CES (it is an explanation against, not for, what happened)
# and one a positive one. Be charitable and presume they never refer to the the anti-explanatory variable state
# if they don’t mention the state explicitly but only the variable.

longPizza_ces <- merge(
  long_ces,
  pizza_ces,
  by = c('condObs', 'node3'),
  all = TRUE
) |>
  mutate(across(where(is.numeric), \(x) replace(x, is.na(x), 0)))

shortPizza_ces <- merge(
  short_ces,
  pizza_ces,
  by = c('condObs', 'node3'),
  all = TRUE
) |>
  mutate(across(where(is.numeric), \(x) replace(x, is.na(x), 0)))

longHotdog_ces <- merge(
  long_ces,
  hotdog_ces,
  by = c('condObs', 'node3'),
  all = TRUE
) |>
  mutate(across(where(is.numeric), \(x) replace(x, is.na(x), 0)))

shortHotdog_ces <- merge(
  short_ces,
  hotdog_ces,
  by = c('condObs', 'node3'),
  all = TRUE
) |>
  mutate(across(where(is.numeric), \(x) replace(x, is.na(x), 0)))

# CURRENT PROBLEM 12 MAY
# The annotation has different ratings for food and path br and vars. This must be solved before merging them, otherwise they are being equated

# Combine ces scores for path and food. Decision here to add but could also multiply - change if necessary?
longPizza_ces$newProb <- longPizza_ces$postces.x * longPizza_ces$postces.y
shortPizza_ces$newProb <- shortPizza_ces$postces.x * shortPizza_ces$postces.y
longHotdog_ces$newProb <- longHotdog_ces$postces.x * longHotdog_ces$postces.y
shortHotdog_ces$newProb <- shortHotdog_ces$postces.x * shortHotdog_ces$postces.y

# longPizza_ces[is.na(longPizza_ces)] <- 0
# shortPizza_ces[is.na(shortPizza_ces)] <- 0
# longHotdog_ces[is.na(longHotdog_ces)] <- 0
# shortHotdog_ces[is.na(shortHotdog_ces)] <- 0

# SET THIS BY HAND: .01 is way flat and equal. .001 is very peaky
tau <- .005

longPizza_ces <- longPizza_ces |>
  group_by(condObs) |>
  mutate(postces_norm = exp(newProb / tau) / sum(exp(newProb / tau))) |>
  ungroup()

shortPizza_ces <- shortPizza_ces |>
  group_by(condObs) |>
  mutate(postces_norm = exp(newProb / tau) / sum(exp(newProb / tau))) |>
  ungroup()

longHotdog_ces <- longHotdog_ces |>
  group_by(condObs) |>
  mutate(postces_norm = exp(newProb / tau) / sum(exp(newProb / tau))) |>
  ungroup()

shortHotdog_ces <- shortHotdog_ces |>
  group_by(condObs) |>
  mutate(postces_norm = exp(newProb / tau) / sum(exp(newProb / tau))) |>
  ungroup()

# Save
save(
  longPizza_ces,
  shortPizza_ces,
  longHotdog_ces,
  shortHotdog_ces,
  file = here('Exp2Explanation', 'Model', 'Data', 'ces4Outcomes.rda')
)
