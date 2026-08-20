# -----------------------------------------------
# -----  plot unobs - obs   -------------
# -----------------------------------------------

# Import
library(here)
library(tidyverse)
library(ggplot2)

# Need pChoice from allP from Model/04getSEMsimple = scanriosSimple
load(here('Exp2Explanation', 'Model', 'Data', 'scenariosSimple.rda')) # allpath, allfood, allP
load(here('Exp2Explanation', 'Annotation', 'Data', 'annsFixed.rda')) # df_fixed, 1991 of 14


counts_Obs <- df_fixed |>
  group_by(condObs, choice, State) |>
  summarise(count = n()) |>
  ungroup() |>
  group_by(condObs, choice) |>
  mutate(prop = count / sum(count))

countObs <- counts_Obs |>
  left_join(
    allP |> select(condObs, choice, pChoice), # keep the join keys plus the columns you want
    by = c("condObs", "choice")
  )

# NOW PLOT! and get cor

#
# Now scatter, with pChoiceNorm on x and entropy on y, and maybe add a line of best fit?
obsplot <- countObs |>
  #filter(someColumn %in% c("value1", "value2")) |>
  ggplot(aes(x = pChoice, y = prop, color = State)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Participants cite unobserved causes more when outcome \nis incongruent with observed inputs",
    x = "Normalized Choice Probability",
    y = "Propotion unobserved"
  ) +
  theme_classic()

obsplot


# Now save
ggsave(
  filename = "unobs.pdf", #
  plot = obsplot,
  path = here('Exp2Explanation', 'Annotation', 'Figures'),
  width = 6,
  height = 6,
  units = "in"
)
