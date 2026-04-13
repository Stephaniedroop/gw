# -----------------------------------------------
# -----  plot part shitty ratings   -------------
# -----------------------------------------------

annotated <- read.csv(here(
  'Exp2Explanation',
  'Experiment',
  'Data',
  'annotated.csv'
)) # 1734 because this was after took out the training and test set

# Replace NAs with 0
annotated[is.na(annotated)] <- 0

# New columns for sum cols P:S and Pu:Su
annotated <- annotated |>
  mutate(obs = rowSums(across(c(P:S))), unobs = rowSums(across(c(Pu:Su))))

annotated <- annotated |>
  mutate(conj = obs & unobs)

ann2 <- annotated |> # 1464
  filter(conj == F & obs < 3 & unobs < 2)

# This is a simple and crappy data set but is still ok

save(ann2, file = here('Exp2Explanation', 'Experiment', 'Data', 'ann2.rda')) # Btw this was on the data that had the test and training removed. Could do it on all

# What can I model?
# - one variable of any type
# - two observed variables

# What can I not model?
# - combination of obs and unobs
# - interactions of two unobs
