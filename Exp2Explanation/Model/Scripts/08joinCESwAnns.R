# -----------------------------------------------
# -----  Combine CES and annotations  -------------
# -----------------------------------------------

library(here)
library(tidyverse)
library(stringr)
library(broom)


# Import
load(here('Exp2Explanation', 'Model', 'Data', 'ces4OutcomesLong.rda')) # 4624 of 11 - for OLD, remove 'Long'
load(here('Exp2Explanation', 'Annotation', 'Data', 'counts.rda'))
source(here('Exp2Explanation', 'Model', 'Scripts', '00fns.R'))


# Also need pChoice from scenariosSimple from script 04 - if think of a better way put it in separately

load(here('Exp2Explanation', 'Model', 'Data', 'scenariosSimple.rda')) # for allP

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


# And merge - this is an old version that only does the modelled 1820, not the full 1991
# d <- ces_long |>
#   left_join(counts_all, by = c("choice", "condObs", "node3")) |>
#   mutate(count = replace_na(count, 0)) |>
#   group_by(model, choice, condObs) |>
#   mutate(p = newProb / sum(newProb)) |>
#   ungroup()

# Better version - after completion - uses everything - but at fixed tau, not yet optimised
# d <- ces_long |>
#   left_join(counts_all, by = c("choice", "condObs", "node3")) |>
#   mutate(count = replace_na(count, 0)) |>
#   group_by(model, choice, condObs) |>
#   mutate(
#     z = (newProb - max(newProb)) / 0.25,
#     logp = z - log(sum(exp(z))),
#     p = exp(logp)
#   ) |>
#   ungroup()

#d |> filter(count > 0, p == 0) |> summarise(dead_responses = sum(count))

# d |>
#   group_by(model) |>
#   summarise(nll = -sum(count * logp), .groups = "drop") |>
#   arrange(nll)

# Print what the current .25 setting of tau predicts for how many cases are affected by non-modellability - about 20% ie it way overpredicts
# d |>
#   group_by(model, choice, condObs) |>
#   summarise(
#     n = sum(count),
#     p0 = sum(p[newProb == 0]),
#     obs0 = sum(count[newProb == 0]),
#     .groups = "drop"
#   ) |>
#   group_by(model) |>
#   summarise(
#     predicted = sum(p0 * n) / sum(n),
#     observed = sum(obs0) / sum(n),
#     .groups = "drop"
#   )
#
# # d1 <- d |> # 1822 - only the ppt answers the model can handle. Not the noise
# # filter(model == "postces")
#
# save(
#   d,
#   file = here('Exp2Explanation', 'Model', 'Data', 'd.rda')
# )

# And fit here: instead of d

# loglik_tau <- function(tau2, dat, col = "newProb") {
#   dat |>
#     group_by(choice, condObs) |>
#     mutate(
#       z = (.data[[col]] - max(.data[[col]])) / tau2,
#       logp = z - log(sum(exp(z)))
#     ) |>
#     ungroup() |>
#     summarise(ll = sum(count * logp)) |>
#     pull(ll)
# }

dj <- ces_long |>
  left_join(counts_all, by = c("choice", "condObs", "node3")) |>
  mutate(count = replace_na(count, 0))

tau_grid <- exp(seq(log(1e-3), log(10), length.out = 80))

prof <- dj |>
  group_by(model) |>
  group_modify(
    ~ tibble(
      tau2 = tau_grid,
      ll = vapply(tau_grid, loglik_tau, numeric(1), dat = .x)
    )
  ) |>
  ungroup()

ptau2 <- ggplot(prof, aes(tau2, ll, colour = model)) +
  geom_line() +
  scale_x_log10() +
  coord_cartesian(ylim = c(max(prof$ll) - 300, max(prof$ll))) +
  theme_bw()

# save that
ggsave(
  here('Exp2Explanation', 'Model', 'Figures', 'tau2_profile.pdf'),
  ptau2,
  width = 6,
  height = 6
)


prof |>
  group_by(model) |>
  summarise(tau2_hat = tau2[which.max(ll)], ll = max(ll)) |>
  mutate(dBIC = 2 * (max(ll) - ll)) |>
  arrange(desc(ll))

# Scale check
prof |>
  group_by(model) |>
  summarise(tau2_hat = tau2[which.max(ll)]) |>
  left_join(
    dj |> group_by(model) |> summarise(sd_np = sd(newProb)),
    by = "model"
  ) |>
  mutate(scaled = tau2_hat / sd_np)


fits <- dj |>
  group_by(model) |>
  group_modify(
    ~ {
      o <- optimize(\(lt) -loglik_tau(exp(lt), .x), c(log(1e-3), log(10)))
      tibble(tau2_hat = exp(o$minimum), ll = -o$objective)
    }
  ) |>
  ungroup() |>
  mutate(
    N = sum(counts_all$count),
    BIC = -2 * ll + log(N),
    dBIC = BIC - min(BIC)
  ) |>
  arrange(BIC)

fits

# 1. All 3 lesioned models are better, with v small taus, but why? Is it because they fail to commit?

ll_unif <- dj |>
  group_by(model, choice, condObs) |>
  summarise(ll = sum(count) * log(1 / n()), .groups = "drop") |>
  group_by(model) |>
  summarise(ll_uniform = sum(ll))

ll_unif # yes all four clear uniform, so the ordering is important

# 2. Is the tau diff just scale? No

fits |>
  left_join(
    dj |> group_by(model) |> summarise(sd_np = sd(newProb)),
    by = "model"
  ) |>
  mutate(tau_scaled = tau2_hat / sd_np)

# 3. Ties at the top? Yes - for postns and noInfnons

dj |>
  group_by(model, choice, condObs) |>
  summarise(
    n_at_max = sum(newProb > max(newProb) - 1e-12),
    n_at_zero = sum(newProb == 0),
    n_opt = n(),
    .groups = "drop"
  ) |>
  group_by(model) |>
  summarise(across(c(n_at_max, n_at_zero, n_opt), mean))

# 4. Confirm the peaks are interior

prof |>
  group_by(model) |>
  summarise(
    at_min_edge = which.max(ll) == 1,
    at_max_edge = which.max(ll) == n(),
    tau_hat = tau2[which.max(ll)]
  )


#Check 3 is the real diagnostic - noselect ties at the 4 maximum, uniform over those, but ces chooses and is penalised because softmax wants to hedge over 20

dj |>
  group_by(model, choice, condObs) |>
  mutate(top = newProb > max(newProb) - 1e-12) |>
  summarise(
    obs_in_top = sum(count[top]) / sum(count),
    k_top = sum(top),
    n = sum(count),
    .groups = "drop"
  ) |>
  group_by(model) |>
  summarise(
    obs_in_top = weighted.mean(obs_in_top, w = n),
    mean_k = mean(k_top),
    .groups = "drop"
  )


dj |>
  filter(model == "postns") |>
  group_by(choice, condObs) |>
  filter(newProb > max(newProb) - 1e-12) |>
  ungroup() |>
  count(cause, sort = TRUE)

ts <- dj |>
  group_by(model, choice, condObs) |>
  filter(newProb > max(newProb) - 1e-12) |>
  ungroup() |>
  select(model, choice, condObs, node3)

inner_join(
  filter(ts, model == "postns") |> select(-model),
  filter(ts, model == "noInf_ns") |> select(-model),
  by = c("choice", "condObs", "node3")
) |>
  nrow()

inner_join(
  filter(ts, model == "postces") |> select(-model),
  filter(ts, model == "postns") |> select(-model),
  by = c("choice", "condObs", "node3")
) |>
  nrow() # of 64

# Model v participant split

dj <- dj |>
  left_join(select(fits, model, tau2_hat), by = "model") |>
  group_by(model, choice, condObs) |>
  mutate(
    z = (newProb - max(newProb)) / tau2_hat,
    logp = z - log(sum(exp(z))),
    p = exp(logp)
  ) |>
  ungroup()

dj |>
  group_by(model, choice, condObs) |>
  summarise(tot = sum(p), .groups = "drop") |>
  summarise(worst = max(abs(tot - 1))) # ~1e-15

sum(dj$count[dj$model == "postces"]) # 1991

UNOBS <- c("Pu", "Ku", "Cu", "Su", "br") # check against uvars in script 06

uo <- dj |>
  mutate(unobs = cause %in% UNOBS) |>
  group_by(model, choice, condObs) |>
  summarise(
    model_unobs = sum(p[unobs]),
    ppts_unobs = sum(count[unobs]) / sum(count),
    n = sum(count),
    .groups = "drop"
  ) |>
  left_join(allP, by = c("choice", "condObs")) # brings pChoice

uo |>
  group_by(model) |>
  summarise(
    model_mean = weighted.mean(model_unobs, n),
    ppts_mean = weighted.mean(ppts_unobs, n),
    bias = model_mean - ppts_mean,
    mae = weighted.mean(abs(model_unobs - ppts_unobs), n)
  )


# save uo
save(
  uo,
  file = here('Exp2Explanation', 'Model', 'Data', 'uo.rda')
)


# This shows the models choose slightly more unobserved vars than ppl but not by much
long <- uo |>
  select(model, choice, condObs, n, pChoice, model_unobs, ppts_unobs) |>
  pivot_longer(
    c(model_unobs, ppts_unobs),
    names_to = "source",
    values_to = "share"
  ) |>
  mutate(source = if_else(source == "ppts_unobs", "participants", "model"))

# one variant at a time; the interaction tests whether the slopes differ
m <- glm(
  share ~ pChoice * source,
  family = quasibinomial,
  weights = n,
  data = filter(long, model == "postces")
)
summary(m)

long |>
  group_by(model) |>
  group_modify(
    ~ {
      m <- glm(
        share ~ pChoice * source,
        family = quasibinomial,
        weights = n,
        data = .x
      )
      broom::tidy(m) |> filter(term == "pChoice:sourceparticipants")
    }
  ) |>
  ungroup()


m <- glm(
  share ~ pChoice * source,
  family = quasibinomial,
  weights = n,
  data = filter(long, model == "postces")
)
coef(m)

r <- range(uo$pChoice)
nd <- expand_grid(pChoice = r, source = c("model", "participants"))
bind_cols(nd, fitted = predict(m, nd, type = "response"))

# At lowpChoice, the model fits participants well. At high pChoice they diverge a bit

# ---- all four variants, both coefficients that matter -----------------------
slopes <- long |>
  group_by(model) |>
  group_modify(
    ~ {
      m <- glm(
        share ~ pChoice * source,
        family = quasibinomial,
        weights = n,
        data = .x
      )
      tidy(m) |> filter(term %in% c("pChoice", "pChoice:sourceparticipants"))
    }
  ) |>
  ungroup()

slopes |>
  select(model, term, estimate, std.error, p.value) |>
  pivot_wider(
    names_from = term,
    values_from = c(estimate, std.error, p.value)
  ) |>
  mutate(
    slope_model = `estimate_pChoice`,
    slope_ppts = `estimate_pChoice` + `estimate_pChoice:sourceparticipants`,
    ratio = slope_model / slope_ppts,
    p_model = `p.value_pChoice`,
    p_interact = `p.value_pChoice:sourceparticipants`
  ) |>
  select(model, slope_model, slope_ppts, ratio, p_model, p_interact)


# Predicted proportions at each end of the pChoice range
r <- range(uo$pChoice)

long <- uo |>
  select(model, choice, condObs, n, pChoice, model_unobs, ppts_unobs) |>
  pivot_longer(
    c(model_unobs, ppts_unobs),
    names_to = "source",
    values_to = "share"
  ) |>
  mutate(source = if_else(source == "ppts_unobs", "participants", "predicted"))

long <- long |>
  mutate(source = factor(source, levels = c("predicted", "participants")))

# long |>
#   group_by(model) |>
#   group_modify(
#     ~ {
#       m <- glm(
#         share ~ pChoice * source,
#         family = quasibinomial,
#         weights = n,
#         data = .x
#       )
#       nd <- expand_grid(pChoice = r, source = c("model", "participants"))
#       bind_cols(nd, fit = predict(m, nd, type = "response"))
#     }
#   ) |>
#   ungroup() |>
#   pivot_wider(names_from = source, values_from = fit) |>
#   mutate(gap = model - participants)

unobsbymodel <- ggplot(long, aes(pChoice, share, colour = source)) +
  geom_point(aes(size = n), alpha = 0.45, stroke = 0) +
  geom_smooth(
    aes(weight = n),
    method = "glm",
    se = TRUE,
    method.args = list(family = quasibinomial),
    formula = y ~ x,
    linewidth = 0.7
  ) +
  facet_wrap(~model, nrow = 1) +
  scale_colour_manual(
    values = c(model = "firebrick", participants = "grey25"),
    name = NULL
  ) +
  scale_size_continuous(range = c(0.7, 3.5), name = "responses in cell") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
  labs(
    x = "Probability of the outcome being explained",
    y = "Share of explanations citing unobserved causes",
    title = "Do the variants track how participants shift toward unobserved causes?"
  )

ggsave(
  here('Exp2Explanation', 'Model', 'Figures', 'unobsbymodel.pdf'),
  unobsbymodel,
  width = 6,
  height = 6
)


# m_bin <- glm(
#   share ~ pChoice * source,
#   family = binomial,
#   weights = n,
#   data = filter(long, model == "postces")
# )
# sum(residuals(m_bin, type = "pearson")^2) / df.residual(m_bin)

# ----- Reintegrate below here ---------

# observed proportion within each cell, alongside the model's normalised score
dc <- d |>
  group_by(model, choice, condObs) |>
  mutate(obs = count / sum(count)) |>
  ungroup()

# per-cell correlation, then averaged
percell <- dc |>
  group_by(model, choice, condObs) |>
  summarise(r = cor(obs, p), n = sum(count), k = n(), .groups = "drop")

percell |>
  group_by(model) |>
  summarise(
    mean_r = mean(r, na.rm = TRUE),
    sd_r = sd(r, na.rm = TRUE),
    wtd_r = weighted.mean(r, w = n, na.rm = TRUE),
    n_na = sum(is.na(r)),
    .groups = "drop"
  ) |>
  arrange(desc(mean_r))

# pooled across all cells at once
dc |>
  group_by(model) |>
  summarise(r_pooled = cor(obs, p), .groups = "drop") |>
  arrange(desc(r_pooled))


# ----------

# Combine

longPizza_join <- longPizza_ces |>
  full_join(counts_longPizza, by = c("condObs", "node3"))

shortPizza_join <- shortPizza_ces |>
  full_join(counts_shortPizza, by = c("condObs", "node3"))

longHotdog_join <- longHotdog_ces |>
  full_join(counts_longHotdog, by = c("condObs", "node3"))

shortHotdog_join <- shortHotdog_ces |>
  full_join(counts_shortHotdog, by = c("condObs", "node3"))

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

# Check we've got all 1991 counts and nobody got lost in the join (494+502+495+500 = 1991)
sum(longPizza_join$count, na.rm = T) # 494
sum(shortPizza_join$count, na.rm = T) # 502
sum(longHotdog_join$count, na.rm = T) # 495
sum(shortHotdog_join$count, na.rm = T) # 500

# Tag what gets plotted
longPizza_join <- longPizza_join |> mutate(Relevant = !is.na(postces.x))
shortPizza_join <- shortPizza_join |> mutate(Relevant = !is.na(postces.x))
longHotdog_join <- longHotdog_join |> mutate(Relevant = !is.na(postces.x))
shortHotdog_join <- shortHotdog_join |> mutate(Relevant = !is.na(postces.x))

# And there may be other things needed to tag too - see Neil's otehr questions in the slack

# Add column called choice which has LongPizza in each cell
longPizza_join <- longPizza_join |> mutate(choice = "LongPizza")
shortPizza_join <- shortPizza_join |> mutate(choice = "ShortPizza")
longHotdog_join <- longHotdog_join |> mutate(choice = "LongHotdog")
shortHotdog_join <- shortHotdog_join |> mutate(choice = "ShortHotdog")


# Put together as one big df - 1233 obs
all <- rbind(shortPizza_join, longPizza_join, shortHotdog_join, longHotdog_join)

all$Relevant <- as.factor(all$Relevant)
all$choice <- as.factor(all$choice)


# Is NOW a good time to allocate the tag Exp1?

load(here('Exp1Prediction', 'Model', 'Data', 'targetDist.Rda'))
all$condVerb <- all$condObs
levels(all$condVerb) <- levels(situationsVerbose) # this from the target distribution from Exp1

# Add present_causes.x and present_causes.y
all <- all |>
  mutate(cause = coalesce(present_causes.x, present_causes.y))

allP <- allP |>
  select(condObs, choice, pChoice)

# Merge in the allP
all2 <- merge(
  all,
  allP,
  by = c('condObs', 'choice'),
  all.x = TRUE
)

# This gives several model points per plot so aggregate the model scores
forplot <- all2 |>
  group_by(choice, condObs, pChoice, condVerb, cause) |>
  summarise(
    model = sum(postces_norm, na.rm = T),
    ppts = sum(count_norm, na.rm = T),
  ) |>
  ungroup()

save(
  all,
  all2,
  forplot,
  file = here('Exp2Explanation', 'Model', 'Data', 'all.rda')
)
#write.csv(all, 'all.csv')
