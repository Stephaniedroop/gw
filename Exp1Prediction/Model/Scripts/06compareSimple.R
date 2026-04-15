# ==============================================================================
# Compare SIMPLE and INTERACTION KL Model Predictions
# ==============================================================================

library(ggplot2)
library(tidyverse)

old_env <- new.env()
new_env <- new.env()

load(here('Exp1Prediction', 'Model', 'Data', 'model.rda'), envir = old_env)
load(
  here('Exp1Prediction', 'Model', 'Data', 'modelSimple.rda'),
  envir = new_env
)


comparison <- old_env$df.m |>
  select(situationVerbose, situation, td_path, td_food) |>
  mutate(
    old_mp_path = old_env$df.m$mp_path,
    old_mp_food = old_env$df.m$mp_food,
    new_mp_path = new_env$df.m$mp_path,
    new_mp_food = new_env$df.m$mp_food
  )

# Plot it to see: basically only situation 1000 has any difference between them (off diagonal) and anyway the td is half way between them
comparison |>
  pivot_longer(
    cols = c(td_path, td_food),
    names_to = "outcome",
    names_prefix = "td_",
    values_to = "td"
  ) |>
  mutate(
    old = ifelse(outcome == "path", old_mp_path, old_mp_food),
    new = ifelse(outcome == "path", new_mp_path, new_mp_food)
  ) |>
  ggplot(aes(x = old, y = new, label = situation)) +
  geom_point() +
  geom_text(nudge_y = 0.01, size = 2.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~outcome) +
  labs(x = "Old model", y = "New model")
