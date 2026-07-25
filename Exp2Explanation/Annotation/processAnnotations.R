# -----------------------------------------------
# -----  process Annotations   -------------
# -----------------------------------------------

library(here)
library(tidyverse)
library(stringr)
library(xtable)
#library(mclogit)
library(vcd)

# 2040 of 11
df <- read.csv(here(
  'Exp2Explanation',
  'Annotation',
  'Data',
  'annotatedNewModelall.csv'
)) # annotatedall is from the .py script call to api. with 2 is manually through chat obtaining rest when my fund ran out
# 'annotatedNewModel' is partway manually reconciled - the api call missed some

# ---------- 1. Process df the claude rated explanations -----------------

df <- df |> # 2040
  mutate(condObs = substr(as.character(tag), 2, 5)) |>
  rename(node3 = annotation)

df$condObs <- as.factor(df$condObs)

# Add a column of the outcome in words:
df <- df |>
  mutate(
    choice = case_when(
      str_ends(as.character(tag), '00') ~ 'ShortPizza',
      str_ends(as.character(tag), '01') ~ 'LongPizza',
      str_ends(as.character(tag), '10') ~ 'ShortHotdog',
      str_ends(as.character(tag), '11') ~ 'LongHotdog',
      TRUE ~ 'Other'
    )
  )

# Remove rows where column right contains Unclear - now 1993 - 47 Unclear
df <- df |>
  filter(!str_detect(as.character(node3), 'Unclear'))


# --------

# rows with full node3 (e.g. Pu=0, Pu=1) 1937
full_rows <- df |>
  filter(grepl("=", node3))

counts <- full_rows |>
  group_by(tag, node3) |>
  summarise(count = n()) |>
  ungroup()

# rows with bare node3 (e.g. Pu) - 56
bare_rows <- df |>
  filter(!grepl("=", node3))

# within each cond+variable, find which level (=0 or =1) has higher count
winners <- counts |>
  mutate(var_base = sub("=.*", "", node3)) |>
  group_by(tag, var_base) |>
  slice_max(count, n = 1, with_ties = FALSE) |>
  select(tag, var_base, winning_node3 = node3) |>
  ungroup()

# join winners onto bare rows and replace node3
bare_rows_fixed <- bare_rows |>
  left_join(winners, by = c("tag", "node3" = "var_base")) |>
  mutate(node3 = coalesce(winning_node3, node3)) |>
  select(-winning_node3)

# Not everything was caught by this step - how many were not? JUst 2 let's just remove them
fixed_not <- bare_rows_fixed |>
  filter(!grepl("=", node3))

# Remove fixed_not from bare_rows_fixed
bare_rows_fixed <- bare_rows_fixed |>
  filter(grepl("=", node3))

# recombine to get 1991 rows
df_fixed <- bind_rows(full_rows, bare_rows_fixed)

df_fixed <- df_fixed |>
  mutate(
    node3 = factor(node3),
    condObs = factor(condObs),
    choice = factor(choice),
    mindsCode = factor(mindsCode),
    tag = factor(tag),
    State = factor(State)
  )

# ---------- Quick check of signal ----------
tab <- table(df_fixed$tag, df_fixed$node3) # Print this in appendices if necessary? A good way of showing it.

annotation_summary <- as.data.frame.matrix(tab)

# Save as csv in Data folder
write.csv(
  annotation_summary,
  here('Exp2Explanation', 'Annotation', 'Data', 'annotation_summary.csv'),
  row.names = TRUE
)

chisq.test(tab, simulate.p.value = TRUE, B = 10000) # 4990.6, df = NA, p-value = 9.999e-05 - can't really interpret

assocstats(tab) # to get Cramer's V


# ---- this if we don't split out br -------

# Add column Obs/Unobs based on whether node3 has one letter before = or more than 1
# df_fixed <- df_fixed |>
#   mutate(State = ifelse(grepl("^[a-zA-Z]{1}=", node3), "Obs", "UnObs")) # 1001 v 990

#  simple barplot of counts of Obs vs Unobs
# ggplot(df_fixed, aes(x = State)) +
#   geom_bar() +
#   labs(title = "Counts of Observed vs Unobserved Explanations", x = "State Type", y = "Count") +
#   theme_minimal()

# ------ this if we do split out br --------

# What about br?
df_fixed <- df_fixed |>
  mutate(
    State = case_when(
      grepl("^br", node3, ignore.case = TRUE) ~ "br",
      grepl("^[a-zA-Z]=", node3) ~ "Obs",
      TRUE ~ "UnObs"
    )
  )

quicp <- df_fixed |>
  mutate(State = factor(State, levels = c("Obs", "UnObs", "br"))) |>
  ggplot(aes(x = State)) +
  geom_bar() +
  labs(
    title = "Counts of Observed vs Unobserved Explanations",
    x = "State Type",
    y = "Count"
  ) +
  theme_classic()

ggsave(
  filename = "quicp.pdf", #
  plot = quicp,
  path = here('Exp2Explanation', 'Annotation', 'Figures'),
  width = 6,
  height = 6,
  units = "in"
)

# Add in forplot to df_fixed
#pChoice <- merge(df_fixed, forplot$pChoiceNorm, by.x = c("condObs", "choice"), all.x = TRUE)

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
  group_by(condObs, node3) |>
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
  group_by(condObs, node3) |> # , condVerb, cond,  don't actually need these
  summarise(count = n()) |>
  ungroup()

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

# Now merge in forplot $pChoiceNorm by condObs and choice
# ent <- ent |>
#   left_join(forplot |>
#               select(condObs, choice, pChoiceNorm), by = c("condObs", "choice"))

forent <- forplot |>
  distinct(condObs, choice, pChoiceNorm)

ent <- ent |>
  left_join(
    forent,
    by = c("condObs", "choice")
  )

# Now scatter, with pChoiceNorm on x and entropy on y, and maybe add a line of best fit?
entplot <- ggplot(ent, aes(x = pChoiceNorm, y = entropy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Entropy vs. Normalized Choice Probability",
    x = "Normalized Choice Probability",
    y = "Entropy"
  ) +
  theme_classic()

ggsave(
  filename = "entplot.pdf", #
  plot = entplot,
  path = here('Exp2Explanation', 'Annotation', 'Figures'),
  width = 6,
  height = 6,
  units = "in"
)

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


# Remember to save this later if it works
save(
  df_fixed,
  counts_longPizza,
  counts_shortPizza,
  counts_longHotdog,
  counts_shortHotdog,
  file = here('Exp2Explanation', 'Annotation', 'Data', 'counts.rda')
)
