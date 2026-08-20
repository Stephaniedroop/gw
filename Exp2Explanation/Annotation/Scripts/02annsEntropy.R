# -----------------------------------------------
# -----  Entropy of annotated explanations   -------------
# -----------------------------------------------

# The entropy stuff from processAnnotations is a separate analysis. Uses the full anns df_fixed before it is split

library(here)
library(tidyverse)
library(ggplot2)

# Need pChoice from allP from Model/04getSEMsimple = scanriosSimple
load(here('Exp2Explanation', 'Model', 'Data', 'scenariosSimple.rda')) # allpath, allfood, allP
load(here('Exp2Explanation', 'Annotation', 'Data', 'annsFixed.rda')) # df_fixed, 1991 of 14


# Load anns_fixed.csv - 1991 obs of 13 vars
# df_fixed <- read.csv(here(
#   'Exp2Explanation',
#   'Annotation',
#   'Data',
#   'anns_fixed.csv'
# )) # anns_fixed is the processed annotations from processAnnotations.R

counts_condObs <- df_fixed |>
  group_by(condObs, choice, node3) |>
  summarise(count = n()) |>
  ungroup() |>
  group_by(condObs, choice) |>
  mutate(prop = count / sum(count))

ent <- counts_condObs |>
  group_by(condObs, choice) |>
  summarise(entropy = -sum(prop * log2(prop))) |>
  ungroup()

ent2 <- ent |>
  left_join(
    allP,
    by = c("condObs", "choice")
  )

ent2 <- ent2 |>
  select(condObs, choice, entropy, pChoice)


# Now scatter, with pChoiceNorm on x and entropy on y, and maybe add a line of best fit?
entplot <- ggplot(ent2, aes(x = pChoice, y = entropy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Entropy vs. Normalized Choice Probability",
    x = "Normalized Choice Probability",
    y = "Entropy"
  ) +
  theme_classic()

entplot

ggsave(
  filename = "entplot.pdf", #
  plot = entplot,
  path = here('Exp2Explanation', 'Annotation', 'Figures'),
  width = 6,
  height = 6,
  units = "in"
)

# Now a version of entplot coloued by choice
entplot_col <- ggplot(ent2, aes(x = pChoice, y = entropy, color = choice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Entropy vs. Normalized Choice Probability",
    x = "Normalized Choice Probability",
    y = "Entropy"
  ) +
  theme_classic()

entplot_col


# Now a version of entplot coloured by condObs
entplot_col2 <- ggplot(
  ent2,
  aes(x = pChoice, y = entropy, color = condObs)
) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Entropy vs. Normalized Choice Probability",
    x = "Normalized Choice Probability",
    y = "Entropy"
  ) +
  theme_classic()

entplot_col2

# --------- OLD ---------

# If this is what we need, then still need to save and put in the paper

# counts_tag <- df_fixed |>
#   group_by(tag, node3) |>
#   summarise(count = n()) |>
#   ungroup()

counts_tag <- counts |>
  group_by(tag) |>
  mutate(prop = count / sum(count)) |>
  ungroup()

# This just for visual when making the appendix text table manually

counts_tagO <- counts_tag |>
  group_by(tag) |>
  filter(count > 2) |>
  arrange(desc(count), .by_group = TRUE)

load(here('Exp2Explanation', 'Model', 'Data', 'all.rda')) # all and forplot
# want to merge counts_tag and

# Rename tag to condObs and outcome for merging with all

# Need a new df which is entropy per 64 tags, and the p of that choice
# entropy comes from the counts
# ent <- counts_tag |>
#   group_by(tag) |>
#   summarise(entropy = -sum(prop * log2(prop))) |>
#   ungroup()

# -sum(vector x log2 vector)

# counts_tag10 <- counts_tag |>
#   filter(count >= 10) |>
#   group_by(tag)

counts_all <- df_fixed |>
  group_by(node3) |>
  summarise(count = n()) |>
  ungroup()

# Add a column for proportion of total overall
counts_all <- counts_all |>
  mutate(prop = count / sum(count))

# And order descending
# counts_tag <- counts_tag |>
#   group_by(tag, node3) |>
#   arrange(desc(count))
# Write this as a table for appendix

# But only the top 8, and the rest as "Other"
# counts_all2 <- counts_all |>
#   mutate(node3 = ifelse(row_number() <= 10, as.character(node3), "Other")) |>
#   group_by(node3) |>
#   summarise(count = sum(count), prop = sum(prop)) |>
#   ungroup()
#
# counts_all2 <- counts_all2 |>
#   arrange(desc(count))

# For reporting descriptives

xtable(counts_all2, digits = 3)
xtable(counts_all, digits = 3)
