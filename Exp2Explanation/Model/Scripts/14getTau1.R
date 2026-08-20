# ==============================================================================
# DRIVER
# ==============================================================================
#
source(here('Exp2Explanation', 'Model', 'Scripts', '00fns.R'))
#load(here('Exp2Explanation', 'Model', 'Data', 'ces_sepSimpleN.Rda'))
source(here('Exp2Explanation', 'Model', 'Scripts', 'get_lesions.R')) # this runs softmax1 to get 's_hat' from collider Exp1 paper
source(here('Exp2Explanation', 'Model', 'Scripts', '07reorgPredsGen.R'))
load(here('Exp2Explanation', 'Model', 'Data', 'preds_long.rda'))
load(here('Exp2Explanation', 'Annotation', 'Data', 'counts.rda'))


# Also need pChoice from scenariosSimple from script 04 - if think of a better way put it in separately

#load(here('Exp2Explanation', 'Model', 'Data', 'scenariosSimple.rda'))

# First put the choice label for each of the counts
counts_longPizza <- counts_longPizza |>
  mutate(choice = "LongPizza")

counts_shortPizza <- counts_shortPizza |>
  mutate(choice = "ShortPizza")

counts_longHotdog <- counts_longHotdog |>
  mutate(choice = "LongHotdog")

counts_shortHotdog <- counts_shortHotdog |>
  mutate(choice = "ShortHotdog")


# Now rbind
counts_all <- rbind(
  counts_longPizza,
  counts_shortPizza,
  counts_longHotdog,
  counts_shortHotdog
)


fit_at_tau1 <- function(tau1) {
  reorg_preds(get_lesions(pathlong, tau1), get_lesions(foodlong, tau1)) |>
    left_join(counts_all, by = c("choice", "condObs", "node3")) |>
    mutate(count = replace_na(count, 0)) |>
    fit_tau2() |>
    mutate(tau1 = tau1)
}

# ------ COARSE FIT BY GRID SEARCH ---------

sweep <- map(c(0.1, 0.25, 0.5, 1, 2, 5, 100), fit_at_tau1) |> list_rbind()

ggplot(sweep, aes(tau1, ll, colour = model)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  labs(x = "tau1 (log scale)", y = "log-likelihood at fitted tau2")


# anchor: at tau1 = 100, s_hat_ces -> 1/9, so postces must equal postns
sweep |>
  filter(tau1 == 100) |>
  summarise(gap = ll[model == "postces"] - ll[model == "postns"])

# the two _ns variants must be flat across tau1
sweep |>
  filter(str_detect(model, "_ns$|^postns$")) |>
  group_by(model) |>
  summarise(spread = diff(range(ll)))

# where does postces peak, and does it ever beat postns?
sweep |>
  filter(model %in% c("postces", "postns")) |>
  select(tau1, model, ll) |>
  pivot_wider(names_from = model, values_from = ll) |>
  mutate(diff = postces - postns)


sweep |>
  filter(tau1 == 0.25) |>
  select(model, ll) |>
  pivot_wider(names_from = model, values_from = ll) |>
  mutate(gap_at_025 = postns - postces)


sweep2 <- map(c(100, 1000, 10000), fit_at_tau1) |> list_rbind()

sweep2 |>
  filter(model %in% c("postces", "postns")) |>
  select(tau1, model, ll) |>
  pivot_wider(names_from = model, values_from = ll) |>
  mutate(gap = postces - postns)


# --------- FINE FIT AT TAU1 ---------

fine <- map(seq(0.6, 2.0, by = 0.1), fit_at_tau1) |> list_rbind()

fine |>
  filter(model == "postces") |>
  summarise(
    tau1_hat = tau1[which.max(ll)],
    ll_max = max(ll),
    lo = min(tau1[ll > max(ll) - 1.92]),
    hi = max(tau1[ll > max(ll) - 1.92]),
    ll_at_1 = ll[tau1 == 1],
    drop_at_1 = max(ll) - ll[tau1 == 1]
  )

ggplot(filter(fine, model == "postces"), aes(tau1, ll)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_hline(
    yintercept = max(fine$ll[fine$model == "postces"]) - 1.92,
    linetype = "dotted",
    colour = "grey50"
  ) +
  theme_bw() +
  labs(x = "tau1", y = "log-likelihood at fitted tau2")

# Finer
fine |>
  filter(model == "postces", tau1 %in% c(0.8, 0.9, 1.0, 1.1, 1.2)) |>
  select(tau1, ll) |>
  mutate(drop = max(ll) - ll)

fine |>
  filter(model == "postces", tau1 > 0.75, tau1 < 1.35) |>
  select(tau1, ll) |>
  mutate(drop = max(ll) - ll)

map(seq(0.94, 1.20, by = 0.02), fit_at_tau1) |>
  list_rbind() |>
  filter(model == "postces") |>
  select(tau1, ll) |>
  mutate(drop = max(ll) - ll)


prof1 <- tibble(
  tau1 = c(
    0.94,
    0.96,
    0.98,
    1.00,
    1.02,
    1.04,
    1.06,
    1.08,
    1.10,
    1.12,
    1.14,
    1.16,
    1.18,
    1.20
  ),
  drop = c(
    2.50,
    1.44,
    0.694,
    0.226,
    0.0039,
    0,
    0.188,
    0.544,
    1.05,
    1.68,
    2.42,
    3.26,
    4.18,
    5.17
  )
)

lo <- 0.95
hi <- 1.13 # where drop crosses 1.92, by linear interpolation

ggplot(prof1, aes(tau1, drop)) +
  annotate(
    "rect",
    xmin = lo,
    xmax = hi,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey90"
  ) +
  geom_hline(
    yintercept = 1.92,
    linetype = "dashed",
    colour = "grey40",
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 1,
    linetype = "dotted",
    colour = "grey40",
    linewidth = 0.4
  ) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.6) +
  annotate(
    "text",
    x = hi,
    y = 1.92,
    label = "95% interval",
    hjust = -0.05,
    vjust = -0.5,
    size = 3,
    colour = "grey30"
  ) +
  annotate(
    "text",
    x = 1,
    y = max(prof1$drop),
    label = "τ₁ = 1",
    hjust = -0.15,
    vjust = 1,
    size = 3,
    colour = "grey30"
  ) +
  scale_y_reverse() +
  scale_x_continuous(breaks = seq(0.94, 1.20, by = 0.04)) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank()) +
  labs(
    x = expression(tau[1]),
    y = "Loss in log-likelihood from the profile maximum"
  )

f <- approxfun(prof1$tau1, prof1$drop)
lo <- uniroot(\(x) f(x) - 1.92, c(0.94, 1.04))$root
hi <- uniroot(\(x) f(x) - 1.92, c(1.04, 1.20))$root


# The check later for whether there was a tradeoff between tau1 and tau2.
sweep |>
  filter(model == "postces") |>
  select(tau1, tau2_hat) |>
  mutate(ratio = tau2_hat / tau2_hat[which.min(abs(tau1 - 1.04))])


# Same for noInf

fine_ni <- map(seq(0.6, 2.0, by = 0.1), fit_at_tau1) |> list_rbind() # reuse `fine` if you kept it

fine |>
  filter(model == "noInf") |>
  summarise(
    tau1_hat = tau1[which.max(ll)],
    lo = min(tau1[ll > max(ll) - 1.92]),
    hi = max(tau1[ll > max(ll) - 1.92]),
    drop_at_1 = max(ll) - ll[which.min(abs(tau1 - 1))]
  )


fine_no_p <- map(seq(0.8, 1.4, by = 0.02), fit_at_tau1) |>
  list_rbind() |>
  filter(model == "noInf") |>
  select(tau1, ll) |>
  mutate(drop = max(ll) - ll)
