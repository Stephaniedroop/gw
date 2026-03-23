# -----------------------------------------------
# -----  plot Initial Separate CES   -------------
# -----------------------------------------------

library(here)
library(tidyverse)


load(here('Exp2Explanation', 'Model', 'Data', 'tag_counts.Rda'))
load(here('Exp2Explanation', 'Model', 'Data', 'ces_sep.Rda'))

# Can get these from the previous script without reaggregating the four, depends if we decide 2 way or 4 way
pizza <- tag_counts |>
  filter(tag_group %in% c('PizzaShort', 'PizzaLong')) |>
  mutate(cat = 'Pizza')

hotdog <- tag_counts |>
  filter(tag_group %in% c('HotdogShort', 'HotdogLong')) |>
  mutate(cat = 'Hotdog')

short <- tag_counts |>
  filter(tag_group %in% c('PizzaShort', 'HotdogShort')) |>
  mutate(cat = 'Short')

long <- tag_counts |>
  filter(tag_group %in% c('PizzaLong', 'HotdogLong')) |>
  mutate(cat = 'Long')

# Categorise food_ces: if condition ends in 0 then put 'Pizza', if ends in 1 then 'Hotdog'
food_ces <- food_ces |>
  mutate(
    cat = case_when(
      str_ends(condition, '0') ~ 'Pizza',
      str_ends(condition, '1') ~ 'Hotdog',
      TRUE ~ 'other'
    )
  )

path_ces <- path_ces |>
  mutate(
    cat = case_when(
      str_ends(condition, '0') ~ 'Short',
      str_ends(condition, '1') ~ 'Long',
      TRUE ~ 'other'
    )
  )

pizza_ces <- food_ces |>
  filter(cat == 'Pizza')

hotdog_ces <- food_ces |>
  filter(cat == 'Hotdog')

short_ces <- path_ces |>
  filter(cat == 'Short')

long_ces <- path_ces |>
  filter(cat == 'Long')

# Now merge BUT CANT TIL WE HAVE A CES SCORE for the powerset of variables. ugh

# TO GET POWERSET:
# select the counterfactual simulations for which the combined set of variables,
# and count how many of them the effect occurs; select all the other counterfactual simulations
# and count how many of them the effect occurs. calculate the cor from that, exactly as you did
# when you just selected based on a single variable

# OR in the meantime: use the rated set from claude?

# -------- PLOTS ---------- Initial 4-way

# Split tag_counts into the four tag_groups, and for each one do a plot like p1
pPizzaShort <- tag_counts |>
  filter(tag_group == 'PizzaShort') |>
  ggplot(aes(x = present_causes, y = count)) +
  geom_col() +
  facet_wrap(~tag) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "Pizza Short"
  )

pPizzaLong <- tag_counts |>
  filter(tag_group == 'PizzaLong') |>
  ggplot(aes(x = present_causes, y = count)) +
  geom_col() +
  facet_wrap(~tag) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "Pizza Long"
  )

pHotdogShort <- tag_counts |>
  filter(tag_group == 'HotdogShort') |>
  ggplot(aes(x = present_causes, y = count)) +
  geom_col() +
  facet_wrap(~tag) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "Hotdog Short"
  )

pHotdogLong <- tag_counts |>
  filter(tag_group == 'HotdogLong') |>
  ggplot(aes(x = present_causes, y = count)) +
  geom_col() +
  facet_wrap(~tag) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "Hotdog Long"
  )

# Now a plot using ggplot. tag is facet and present_causes is bar
# p1 <- ggplot(tag_counts, aes(x = present_causes, y = count)) +
#   geom_col() +
#   facet_wrap(~tag) +
#   #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
#   #theme(axis.text.x = element_text(size = 7)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3)) +
#   labs(
#     x = "Present Causes",
#     y = "Number of Rating",
#     title = "Present Causes by Situation"
#   )
#
# p1

ggsave(
  filename = "pPizzaShort.pdf", # FIG 3 IN PAPER
  plot = pPizzaShort,
  path = here('Exp2Explanation', 'Model', 'Figures'),
  width = 12,
  height = 12,
  units = "in"
)

ggsave(
  filename = "pPizzaLong.pdf", # FIG 3 IN PAPER
  plot = pPizzaLong,
  path = here('Exp2Explanation', 'Model', 'Figures'),
  width = 12,
  height = 12,
  units = "in"
)

ggsave(
  filename = "pHotdogShort.pdf", # FIG 3 IN PAPER
  plot = pHotdogShort,
  path = here('Exp2Explanation', 'Model', 'Figures'),
  width = 12,
  height = 12,
  units = "in"
)

ggsave(
  filename = "pHotdogLong.pdf", # FIG 3 IN PAPER
  plot = pHotdogLong,
  path = here('Exp2Explanation', 'Model', 'Figures'),
  width = 12,
  height = 12,
  units = "in"
)

# The big 64-facet is too big to see what's going on. In fact the outcomes are differnet anyway, for path0/1 food/01
# so what if I do 4 different facet plots, for path0, path1, food0, path1?
# That would also be easier to get the ces predictions on it
