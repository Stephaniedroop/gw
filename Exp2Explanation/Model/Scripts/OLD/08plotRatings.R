# -----------------------------------------------
# -----  plot Exp2 ratings       -------------
# -----------------------------------------------

library(here)
library(tidyverse)


load(here('Exp2Explanation', 'Experiment', 'Data', 'tag_counts.Rda'))


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
