# Reordpreds is script 7

ces_long <- reorg_preds(path_ces, food_ces)

# Sanity: probabilities sum to 1 in every cell, for every variant
ces_long |>
  group_by(choice, condObs, model) |>
  summarise(tot = sum(newProb), .groups = "drop") |>
  summarise(worst = max(abs(tot - 1)))

# Sanity: the four variants differ (if any pair is identical, script 06 is
# writing the same numbers into two columns)
ces_long |>
  group_by(model) |>
  summarise(mean_newProb = mean(newProb), sd_newProb = sd(newProb))


# old <- new.env()
# load(here('Exp2Explanation','Model','Data','ces4Outcomes.rda'), envir = old)
# check_against_original(ces_long, old$shortPizza_ces)

save(
  ces_long,
  file = here('Exp2Explanation', 'Model', 'Data', 'ces4OutcomesLong.rda')
)


source(here('Exp2Explanation', 'Model', 'Scripts', 'run_les.R'))


# --- inputs, at the chosen tau1 -------------------------------------------
TAU1 <- 1
ces_long <- reorg_preds(run_ces(pathlong, TAU1), run_ces(foodlong, TAU1))

dj <- ces_long |>
  left_join(counts_all, by = c("choice", "condObs", "node3")) |>
  mutate(count = replace_na(count, 0))

# --- fit tau2, one per variant --------------------------------------------
fits <- fit_tau2(dj) |>
  mutate(
    N = sum(dj$count[dj$model == "postces"]),
    BIC = -2 * ll + log(N),
    dBIC = BIC - min(BIC)
  ) |>
  arrange(BIC)

# --- predictions at each variant's fitted tau2 ----------------------------
d <- dj |>
  left_join(select(fits, model, tau2_hat), by = "model") |>
  group_by(model, choice, condObs) |>
  mutate(
    z = (newProb - max(newProb)) / tau2_hat,
    logp = z - log(sum(exp(z))),
    p = exp(logp)
  ) |>
  ungroup()

stopifnot(abs(sum(d$p[d$model == "postces"]) - 64) < 1e-8) # 64 cells, each sums to 1
stopifnot(sum(d$count[d$model == "postces"]) == 1991)

save(
  d,
  fits,
  ces_long,
  TAU1,
  file = here('Exp2Explanation', 'Model', 'Data', 'fitted.rda')
)


cellcor <- d |>
  filter(model == "postces") |>
  group_by(choice, condObs) |>
  summarise(
    obs = list(count / sum(count)),
    pred = list(p),
    n = sum(count),
    k_opt = n(),
    .groups = "drop"
  ) |>
  mutate(
    r_pearson = map2_dbl(obs, pred, cor),
    r_spearman = map2_dbl(obs, pred, ~ cor(.x, .y, method = "spearman"))
  ) |>
  select(-obs, -pred) |>
  left_join(allP, by = c("choice", "condObs"))

cellcor |>
  summarise(
    across(c(r_pearson, r_spearman), list(mean = mean, sd = sd)),
    n_na = sum(is.na(r_pearson))
  )


ggplot(cellcor, aes(pChoice, r_pearson)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_point(aes(size = n, colour = choice), alpha = 0.6, stroke = 0) +
  geom_smooth(
    aes(weight = n),
    method = "lm",
    formula = y ~ x,
    colour = "grey25",
    linewidth = 0.6
  ) +
  scale_colour_brewer(palette = "Dark2", name = NULL) +
  scale_size_continuous(range = c(1, 4), name = "responses in cell") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
  labs(
    x = "Probability of the outcome being explained",
    y = "Model–participant correlation within cell",
    title = "Where does the model agree with participants?"
  )


uo |>
  filter(model == "postces") |>
  ggplot(aes(pChoice)) +
  geom_point(
    aes(y = ppts_unobs, size = n),
    colour = "grey25",
    alpha = 0.6,
    stroke = 0
  ) +
  geom_point(
    aes(y = model_unobs, size = n),
    colour = "firebrick",
    alpha = 0.6,
    stroke = 0
  ) +
  geom_smooth(
    aes(y = ppts_unobs, weight = n),
    method = "glm",
    method.args = list(family = quasibinomial),
    colour = "grey25",
    se = FALSE
  ) +
  geom_smooth(
    aes(y = model_unobs, weight = n),
    method = "glm",
    method.args = list(family = quasibinomial),
    colour = "firebrick",
    se = FALSE
  ) +
  theme_bw()
