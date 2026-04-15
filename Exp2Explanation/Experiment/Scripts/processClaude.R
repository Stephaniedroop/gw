# -----------------------------------------------
# -----  Format claude ratings   -------------
# -----------------------------------------------

library(here)
library(tidyverse)

df <- read.csv(here('Exp2Explanation', 'Experiment', 'Data', 'maincoded.csv')) # 1887 of 11

# The claude ratings were obtained manually to stop it from trying to write a rule based python script and thus losing its fuzzy llm powers.
# I gave it the ratings instructions as pinned context (see notes made in overleaf doc) and then copied in the explanations in batches of 100.
# Then I copied out the batch ratings into the csv file. I kept the text twice to make sure it matched. This maincoded is the result.

split <- strsplit(as.character(df$X), "_")
left <- sapply(split, function(x) trimws(x[1]))
right <- sapply(split, function(x) trimws(x[2]))

df <- df |>
  mutate(left = left) |>
  mutate(right = right)

df <- df |>
  mutate(condObs = substr(as.character(tag), 2, 5))

#write.csv(df, 'claude1887.csv') - just a check
# Remove rows where column right contains Unclear
df <- df |>
  filter(!str_detect(as.character(right), 'Unclear'))

df <- df |>
  rename(node3 = right)

df$Food <- as.factor(df$digit5)
df$Path <- as.factor(df$digit6)

df$mindsCode <- as.factor(df$mindsCode)
df$tag <- as.factor(df$tag)
df$node3 <- as.factor(df$node3)
df$condObs <- as.factor(df$condObs)

df$Pref <- as.factor(df$digit1)
df$Know <- as.factor(df$digit2)
df$Char <- as.factor(df$digit3)
df$Start <- as.factor(df$digit4)

# How many instances of each cited cause? some are v small
df |> count(node3) |> arrange(n)

#df |> count(node3) |> arrange(n) |> print(n = 16)

# Cu=0 is weird; very few
df |>
  filter(node3 == "Cu=0") |>
  count(Pref, Know, Char, Start, Food, Path)

# And the converse: which conditions never produce Cu=0? (hint: Pref=0 has all 5, so it LOOKS like pref=1 perfectly predicts absence of Cu=0)
df |>
  group_by(Pref) |>
  summarise(n_cu0 = sum(node3 == "Cu=0"))

# Then save these for later
save(
  df,
  file = here('Exp2Explanation', 'Experiment', 'Data', 'claudeProcessed.rda')
)
