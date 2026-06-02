# -----------------------------------------------
# -----  Standardise Exp2 ratings  -------------
# -----------------------------------------------

library(here)
library(tidyverse)
library(forcats)


# This is the EXP1 RATINGS, tagged with situation in PKCS form. Gives a distribution of the likert ratings for each of the 16 situations
load(here('Exp1Prediction', 'Experiment', 'Data', 'gwExp1data.Rda')) # df: 1421 of 27
load(here('Exp2Explanation', 'Experiment', 'Data', 'ratedExplans.Rda')) # rated_explans: 2040 of 19. situation is tagged as 't123456'
# There are 8 lettered ratings for each: a:h and unclear
# Use PCKS (note difference) then those four disruptors
#cats <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
# Put column c before b and g before f
rated_explans <- rated_explans |>
  relocate(c, .before = b) |>
  relocate(g, .before = f)

# Rename cols a:h as: c('P', 'K', 'C', 'S', 'Pu', 'Ku', 'Cu', 'Su')
colnames(rated_explans)[grep('^[a-h]$', colnames(rated_explans))] <-
  c('P', 'K', 'C', 'S', 'Pu', 'Ku', 'Cu', 'Su')

# Concatenate those columns
rated_explans <- rated_explans |>
  mutate(
    situation = paste0(P, K, C, S, Pu, Ku, Cu, Su)
  )

rated_explans$situation <- as.factor(rated_explans$situation)

# A new column  that is just the names of any of the 8 causes that are present, concatenated together. So for example if P=1, K=0, C=1, S=0, Pu=0, Ku=1, Cu=0, Su=0 then this column would be "P.Ku". If all 0s then "".
rated_explans <- rated_explans |>
  rowwise() |>
  mutate(
    present_causes = paste0(
      c("P", "K", "C", "S", "Pu", "Ku", "Cu", "Su", "unc")[
        c(P, K, C, S, Pu, Ku, Cu, Su, unc) == 1
      ],
      collapse = "."
    )
  )

rated_explans$present_causes <- as.factor(rated_explans$present_causes)

# Put tag as factor
rated_explans$tag <- factor(
  rated_explans$tag
)

# Group by tag and count the number of each present_cause
tag_counts <- rated_explans |>
  group_by(tag, present_causes) |>
  summarise(count = n()) |>
  ungroup()

cause_counts <- rated_explans |>
  group_by(present_causes) |>
  summarise(count = n()) |>
  ungroup()

# Split tag_counts by whether the tag factor ends in '-00', '-01', '-10', or '-11'
tag_counts <- tag_counts |>
  mutate(
    tag_chr = as.character(tag),
    tag_group = case_when(
      str_ends(tag_chr, '00') ~ 'PizzaShort',
      str_ends(tag_chr, '01') ~ 'PizzaLong',
      str_ends(tag_chr, '10') ~ 'HotdogShort',
      str_ends(tag_chr, '11') ~ 'HotdogLong',
      TRUE ~ 'other'
    )
  ) |>
  select(-tag_chr)

tag_counts <- tag_counts |>
  group_by(tag, tag_group) |>
  mutate(normed_count = count / sum(count)) |>
  ungroup()

save(
  tag_counts,
  cause_counts,
  file = here('Exp2Explanation', 'Model', 'Data', 'tag_counts.rda')
)
