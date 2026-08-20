# -----------------------------------------------
# -----  Split annotated explanations by outcome  -------------
# -----------------------------------------------

library(here)
library(tidyverse)


# Script takes 'df_fixed' (the processed annotations from processAnnotations.R)
# splits it into 4 separate dataframes, one for each outcome (LongPizza, ShortPizza, LongHotdog, ShortHotdog).
# It also counts the number of explanations for each outcome.

# Output goes to '08joinCESwithAnns' for modelling, in Model folder

# Load df_fixed - 1991 obs of 13 vars - anns_fixed is the processed annotations from annsProcess.R
load(here('Exp2Explanation', 'Annotation', 'Data', 'annsFixed.rda'))

# ----- Filter df into the 4 separate ones and make it long -------

df_longPizza <- df_fixed |>
  filter(
    str_ends(as.character(tag), '01')
  ) |>
  mutate(cond = 'LongPizza')

df_shortPizza <- df_fixed |>
  filter(
    str_ends(as.character(tag), '00')
  ) |>
  mutate(cond = 'ShortPizza')

df_longHotdog <- df_fixed |>
  filter(
    str_ends(as.character(tag), '11')
  ) |>
  mutate(cond = 'LongHotdog')

df_shortHotdog <- df_fixed |>
  filter(
    str_ends(as.character(tag), '10')
  ) |>
  mutate(cond = 'ShortHotdog')


# Need counts of number of explanations; this (in proportion form) is actually what will be modeled

counts_longPizza <- df_longPizza |>
  group_by(condObs, node3) |> # , response, index, State --- replace with these if we want the actual explanations
  summarise(count = n()) |>
  ungroup()

counts_shortPizza <- df_shortPizza |>
  group_by(condObs, node3) |>
  summarise(count = n()) |>
  ungroup()

counts_longHotdog <- df_longHotdog |>
  group_by(condObs, node3) |>
  summarise(count = n()) |>
  ungroup()

counts_shortHotdog <- df_shortHotdog |>
  group_by(condObs, node3) |> # ,
  summarise(count = n()) |>
  ungroup()


# Remember to save this later if it works
save(
  #df_fixed,
  counts_longPizza,
  counts_shortPizza,
  counts_longHotdog,
  counts_shortHotdog,
  file = here('Exp2Explanation', 'Annotation', 'Data', 'counts.rda')
)
