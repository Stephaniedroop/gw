# -----------------------------------------------
# -----  plot Initial Separate CES   -------------
# -----------------------------------------------

# Import
library(here)
library(tidyverse)
library(ggplot2)


#load(here('Exp2Explanation', 'Model', 'Data', 'joined.rda'))
load(here('Exp2Explanation', 'Model', 'Data', 'all.rda'))

# To check if plot works annotaitng iwth prob like this. If it does, go back and cleanly load the ealrier allP ratehr than messily merging it with all in script 9
annotLP <- forplot |>
  filter(choice == "LongPizza") |>
  distinct(condVerb, pChoiceNorm)

annotSP <- forplot |>
  filter(choice == "ShortPizza") |>
  distinct(condVerb, pChoiceNorm)

annotLH <- forplot |>
  filter(choice == "LongHotdog") |>
  distinct(condVerb, pChoiceNorm)

annotSH <- forplot |>
  filter(choice == "ShortHotdog") |>
  distinct(condVerb, pChoiceNorm)


# pLongPizza

pLongPizza <- forplot |>
  filter(choice == "LongPizza") |>
  ggplot(aes(x = cause, y = ppts)) +
  geom_bar(stat = "identity") +
  geom_point(aes(x = cause, y = model)) +
  facet_wrap(~condVerb) +
  geom_text(
    data = annotLP,
    aes(label = paste0("prob = ", signif(pChoiceNorm, 3))),
    x = Inf,
    y = Inf,
    hjust = 1.1,
    vjust = 1.4,
    inherit.aes = FALSE,
    size = 3
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "Choice: Pizza, Long"
  )

pLongPizza

pShortPizza <- forplot |>
  filter(choice == "ShortPizza") |>
  ggplot(aes(x = cause, y = ppts)) +
  geom_bar(stat = 'identity') +
  geom_point(aes(x = cause, y = model)) +
  facet_wrap(~condVerb) +
  geom_text(
    data = annotSP,
    aes(label = paste0("prob = ", signif(pChoiceNorm, 3))),
    x = Inf,
    y = Inf,
    hjust = 1.1,
    vjust = 1.4,
    inherit.aes = FALSE,
    size = 3
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "Choice: Pizza, Short"
  )

pShortPizza

# pLongHotdog
pLongHotdog <- forplot |>
  filter(choice == "LongHotdog") |>
  ggplot(aes(x = cause, y = ppts)) +
  geom_bar(stat = 'identity') +
  geom_point(aes(x = cause, y = model)) +
  facet_wrap(~condVerb) +
  geom_text(
    data = annotLH,
    aes(label = paste0("prob = ", signif(pChoiceNorm, 3))),
    x = Inf,
    y = Inf,
    hjust = 1.1,
    vjust = 1.4,
    inherit.aes = FALSE,
    size = 3
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "Choice: Hotdog, Long"
  )

pLongHotdog

pShortHotdog <- forplot |>
  filter(choice == "ShortHotdog") |>
  ggplot(aes(x = cause, y = ppts)) +
  geom_bar(stat = 'identity') +
  geom_point(aes(x = cause, y = model)) +
  facet_wrap(~condVerb) +
  geom_text(
    data = annotSH,
    aes(label = paste0("prob = ", signif(pChoiceNorm, 3))),
    x = Inf,
    y = Inf,
    hjust = 1.1,
    vjust = 1.4,
    inherit.aes = FALSE,
    size = 3
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "Choice: Hotdog, Short"
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
