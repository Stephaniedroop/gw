# -----------------------------------------------
# -----  Unattached analyses and figs of annotation  -------------
# -----------------------------------------------

#
library(here)
library(tidyverse)


# Load df_fixed.csv - 1991 obs of 13 vars - the processed annotations from processAnnotations.R
load(here('Exp2Explanation', 'Annotation', 'Data', 'annsFixed.rda'))


# df_fixed <- read.csv(here(
#   'Exp2Explanation',
#   'Annotation',
#   'Data',
#   'anns_fixed.csv'
# )) #

# Load key got from who knows where
key <- read.csv(here(
  'Exp2Explanation',
  'Experiment',
  'Data',
  'key.csv'
)) # key is the mapping of the condition tags to verbose


# ---------- Quick check of signal ----------
tab <- table(df_fixed$tag, df_fixed$node3) # Print this in appendices if necessary? A good way of showing it.
tab2 <- table(annsumm$condition_verbose, df_fixed$node3) #
annotation_summary <- as.data.frame.matrix(tab)
annotation_summary2 <- as.data.frame.matrix(tab2)

write.csv(
  annotation_summary2,
  here('Exp2Explanation', 'Annotation', 'Data', 'annotation_summary2.csv'),
  row.names = TRUE
)

annsumm <- df_fixed

## ---- Direct lookup and replace ----
annsumm <- annsumm |>
  left_join(key, by = c("tag" = "tag")) |>
  rename(condition_verbose = label)

annsumm2 <- annsumm |>
  select(tag, condition_verbose, response, node3)

write.csv(
  annsumm2,
  here('Exp2Explanation', 'Annotation', 'Data', 'annotations_verbose_long.csv'),
  row.names = FALSE
)

annsumm3 <- annsumm2 |>
  group_by(tag, condition_verbose, node3) |>
  summarise(count = n()) |>
  ungroup()

write.csv(
  annsumm3,
  here(
    'Exp2Explanation',
    'Annotation',
    'Data',
    'annotations_verbose_summary.csv'
  ),
  row.names = FALSE
)


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
