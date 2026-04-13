# -----------------------------------------------
# -----  plot Initial Separate CES   -------------
# -----------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)


#load(here('Exp2Explanation', 'Model', 'Data', 'tag_counts.Rda'))
load(here('Exp2Explanation', 'Model', 'Data', 'ces_sep.Rda')) # 791-804 of 5 vars
df <- read.csv(here('Exp2Explanation', 'Experiment', 'Data', 'maincoded.csv')) # 1887 of 11

# df[is.na(df)] <- 0

split <- strsplit(as.character(df$X), "_")
left <- sapply(split, function(x) trimws(x[1]))
right <- sapply(split, function(x) trimws(x[2]))

df <- df |>
  mutate(left = left) |>
  mutate(right = right)

df <- df |>
  mutate(condObs = substr(as.character(tag), 2, 5))

#write.csv(df, 'claude1887.csv')
# Remove rows where column right contains Unclear
df <- df |>
  filter(!str_detect(as.character(right), 'Unclear'))


# Some processing to remove the unobserved interactions because we're not doing that now, then renormalise
vector <- c(
  'PKu=0',
  'PKu=1',
  'PCu=0',
  'PCu=1',
  'PSu=0',
  'PSu=1',
  'KCu=0',
  'KCu=1',
  'KSu=0',
  'KSu=1',
  'CSu=0',
  'CSu=1',
  'br=0',
  'br=1'
)

# Remove rows where node3 is in vector
food_ces <- food_ces |>
  filter(!node3 %in% vector)

path_ces <- path_ces |>
  filter(!node3 %in% vector)


# Categorise food_ces: if condition ends in 0 then put 'Pizza', if ends in 1 then 'Hotdog'
food_ces <- food_ces |>
  mutate(
    cat = case_when(
      str_ends(condition, '0') ~ 'Pizza',
      str_ends(condition, '1') ~ 'Hotdog',
      TRUE ~ 'other'
    ),
    condObs = substr(condition, 2, 5),
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

pizza_ces <- food_ces |>
  filter(cat == 'Pizza')

hotdog_ces <- food_ces |>
  filter(cat == 'Hotdog')

short_ces <- path_ces |>
  filter(cat == 'Short')

long_ces <- path_ces |>
  filter(cat == 'Long')

longPizza_ces <- merge(
  long_ces,
  pizza_ces,
  by = c('condObs', 'node3'),
  all = TRUE
)

shortPizza_ces <- merge(
  short_ces,
  pizza_ces,
  by = c('condObs', 'node3'),
  all = TRUE
)

longHotdog_ces <- merge(
  long_ces,
  hotdog_ces,
  by = c('condObs', 'node3'),
  all = TRUE
)

shortHotdog_ces <- merge(
  short_ces,
  hotdog_ces,
  by = c('condObs', 'node3'),
  all = TRUE
)

# STILL TO DO FOR ALL THE OTHERS AND IS NULTIPLICATION OK???
longPizza_ces$newProb <- longPizza_ces$postces.x + longPizza_ces$postces.y
shortPizza_ces$newProb <- shortPizza_ces$postces.x + shortPizza_ces$postces.y
longHotdog_ces$newProb <- longHotdog_ces$postces.x + longHotdog_ces$postces.y
shortHotdog_ces$newProb <- shortHotdog_ces$postces.x + shortHotdog_ces$postces.y

longPizza_ces[is.na(longPizza_ces)] <- 0
shortPizza_ces[is.na(shortPizza_ces)] <- 0
longHotdog_ces[is.na(longHotdog_ces)] <- 0
shortHotdog_ces[is.na(shortHotdog_ces)] <- 0

longPizza_ces <- longPizza_ces |>
  group_by(condObs) |>
  mutate(postces_norm = exp(newProb / .005) / sum(exp(newProb / .005))) |>
  ungroup()

shortPizza_ces <- shortPizza_ces |>
  group_by(condObs) |>
  mutate(postces_norm = exp(newProb / .005) / sum(exp(newProb / .005))) |>
  ungroup()

longHotdog_ces <- longHotdog_ces |>
  group_by(condObs) |>
  mutate(postces_norm = exp(newProb / .005) / sum(exp(newProb / .005))) |>
  ungroup()

shortHotdog_ces <- shortHotdog_ces |>
  group_by(condObs) |>
  mutate(postces_norm = exp(newProb / .005) / sum(exp(newProb / .005))) |>
  ungroup()


# ----- Filter df -------

df_longPizza <- df |>
  filter(
    str_ends(as.character(tag), '01')
  ) |>
  mutate(cond = 'LongPizza')

df_shortPizza <- df |>
  filter(
    str_ends(as.character(tag), '00')
  ) |>
  mutate(cond = 'ShortPizza')

df_longHotdog <- df |>
  filter(
    str_ends(as.character(tag), '11')
  ) |>
  mutate(cond = 'LongHotdog')

df_shortHotdog <- df |>
  filter(
    str_ends(as.character(tag), '10')
  ) |>
  mutate(cond = 'ShortHotdog')


# ----

counts_longPizza <- df_longPizza |>
  group_by(condObs, right) |>
  summarise(count = n()) |>
  ungroup() |>
  rename(node3 = right)

counts_shortPizza <- df_shortPizza |>
  group_by(condObs, right) |>
  summarise(count = n()) |>
  ungroup() |>
  rename(node3 = right)

counts_longHotdog <- df_longHotdog |>
  group_by(condObs, right) |>
  summarise(count = n()) |>
  ungroup() |>
  rename(node3 = right)

counts_shortHotdog <- df_shortHotdog |>
  group_by(condObs, right) |>
  summarise(count = n()) |>
  ungroup() |>
  rename(node3 = right)

# counts_longPizza <- counts_longPizza |>
#   group_by(condObs) |>
#   mutate(normed_count = count / sum(count)) |>
#   ungroup() |>
#   rename(node3 = right)

longPizza_join <- longPizza_ces |>
  left_join(counts_longPizza, by = c("condObs", "node3"))

shortPizza_join <- shortPizza_ces |>
  left_join(counts_shortPizza, by = c("condObs", "node3"))

longHotdog_join <- longHotdog_ces |>
  left_join(counts_longHotdog, by = c("condObs", "node3"))

shortHotdog_join <- shortHotdog_ces |>
  left_join(counts_shortHotdog, by = c("condObs", "node3"))

# -----

longPizza_join <- longPizza_join |>
  group_by(condObs) |>
  mutate(count_norm = count / sum(count, na.rm = T)) |>
  ungroup()

shortPizza_join <- shortPizza_join |>
  group_by(condObs) |>
  mutate(count_norm = count / sum(count, na.rm = T)) |>
  ungroup()

longHotdog_join <- longHotdog_join |>
  group_by(condObs) |>
  mutate(count_norm = count / sum(count, na.rm = T)) |>
  ungroup()

shortHotdog_join <- shortHotdog_join |>
  group_by(condObs) |>
  mutate(count_norm = count / sum(count, na.rm = T)) |>
  ungroup()

# longHotdog_join |>
#   group_by(condObs) |>
#   summarise(
#     total_ces = sum(postces_norm),
#     total_count = sum(count_norm, na.rm = T)
#   )

longPizza_join$condObs <- as.factor(longPizza_join$condObs)
longPizza_join$node3 <- as.factor(longPizza_join$node3)
shortPizza_join$condObs <- as.factor(shortPizza_join$condObs)
shortPizza_join$node3 <- as.factor(shortPizza_join$node3)

longHotdog_join$condObs <- as.factor(longHotdog_join$condObs)
longHotdog_join$node3 <- as.factor(longHotdog_join$node3)
shortHotdog_join$condObs <- as.factor(shortHotdog_join$condObs)
shortHotdog_join$node3 <- as.factor(shortHotdog_join$node3)


# Get correlations for each condObs
cor_longPizza <- longPizza_join |>
  group_by(condObs) |>
  summarise(corr = cor(count_norm, postces_norm, use = "complete.obs"))

corlP <- cor_longPizza |>
  summarise(mean_corr = mean(corr), sd_corr = sd(corr))

cor_shortPizza <- shortPizza_join |>
  group_by(condObs) |>
  summarise(corr = cor(count_norm, postces_norm, use = "complete.obs"))

corsP <- cor_shortPizza |>
  summarise(mean_corr = mean(corr), sd_corr = sd(corr))

cor_longHotdog <- longHotdog_join |>
  group_by(condObs) |>
  summarise(corr = cor(count_norm, postces_norm, use = "complete.obs"))

corlH <- cor_longHotdog |>
  summarise(mean_corr = mean(corr), sd_corr = sd(corr))

cor_shortHotdog <- shortHotdog_join |>
  group_by(condObs) |>
  summarise(corr = cor(count_norm, postces_norm, use = "complete.obs"))

corsH <- cor_shortHotdog |>
  summarise(mean_corr = mean(corr), sd_corr = sd(corr))

# ------

# pLongPizza
pLongPizza <- longPizza_join |>
  ggplot(aes(x = node3, y = count_norm)) +
  geom_bar(stat = 'identity') +
  geom_point(aes(x = node3, y = postces_norm)) +
  facet_wrap(~condObs) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "LongPizza"
  )

pLongPizza

pShortPizza <- shortPizza_join |>
  ggplot(aes(x = node3, y = count_norm)) +
  geom_bar(stat = 'identity') +
  geom_point(aes(x = node3, y = postces_norm)) +
  facet_wrap(~condObs) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "ShortPizza"
  )

pShortPizza

# pLongHotdog
pLongHotdog <- longHotdog_join |>
  ggplot(aes(x = node3, y = count_norm)) +
  geom_bar(stat = 'identity') +
  geom_point(aes(x = node3, y = postces_norm)) +
  facet_wrap(~condObs) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "LongPizza"
  )

pLongHotdog

pShortHotdog <- shortHotdog_join |>
  ggplot(aes(x = node3, y = count_norm)) +
  geom_bar(stat = 'identity') +
  geom_point(aes(x = node3, y = postces_norm)) +
  facet_wrap(~condObs) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "ShortPizza"
  )

pShortHotdog


ggsave(
  filename = "pShortPizza.pdf", # FIG 3 IN PAPER
  plot = pShortPizza,
  path = here('Exp2Explanation', 'Model', 'Figures'),
  width = 12,
  height = 12,
  units = "in"
)

ggsave(
  filename = "pLongPizza.pdf", # FIG 3 IN PAPER
  plot = pLongPizza,
  path = here('Exp2Explanation', 'Model', 'Figures'),
  width = 12,
  height = 12,
  units = "in"
)

ggsave(
  filename = "pShortHotdog.pdf", # FIG 3 IN PAPER
  plot = pShortHotdog,
  path = here('Exp2Explanation', 'Model', 'Figures'),
  width = 12,
  height = 12,
  units = "in"
)

ggsave(
  filename = "pLongHotdog.pdf", # FIG 3 IN PAPER
  plot = pLongHotdog,
  path = here('Exp2Explanation', 'Model', 'Figures'),
  width = 12,
  height = 12,
  units = "in"
)

# The big 64-facet is too big to see what's going on. In fact the outcomes are differnet anyway, for path0/1 food/01
# so what if I do 4 different facet plots, for path0, path1, food0, path1?
# That would also be easier to get the ces predictions on it
