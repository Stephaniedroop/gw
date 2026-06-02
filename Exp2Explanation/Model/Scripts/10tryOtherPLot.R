# -----------------------------------------------
# -----  plot altogether  -------------
# -----------------------------------------------

# Import
library(here)
library(tidyverse)
library(ggplot2)


load(here('Exp2Explanation', 'Model', 'Data', 'all.rda'))

# Try faceting one of the 16 initial conditions at a time

p1111 <- all |>
  filter(condObs == "1111") |>
  ggplot(aes(x = cause, y = count_norm)) +
  geom_col(position = "dodge") +
  #geom_bar(stat = 'identity') +
  geom_point(aes(x = cause, y = postces_norm)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "1111"
  )

p1111 # don't like it - best to go back to facet by outcome


# pLongPizza
pLongPizza <- longPizza_join |>
  filter(Relevant == T) |>
  mutate(node3 = droplevels(node3)) |> # purge unused levels
  ggplot(aes(x = node3, y = count_norm)) +
  #geom_col() +
  geom_bar(stat = 'identity') +
  geom_point(aes(x = node3, y = postces_norm)) +
  facet_wrap(~condObs) + #, scales = "free_x"
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "LongPizza"
  )

pLongPizza
