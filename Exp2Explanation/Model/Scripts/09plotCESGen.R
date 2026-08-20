# ==============================================================================
# 09plotCESGen.R
# Generalised replacement for 09plotCESSimple.R.
#
# The original has eight near-identical blocks: four outcomes x {with model,
# without model}, each hardcoding one filter, one annotation frame and one
# title. This version has one plotting function; outcome, variant and the
# with/without-model switch are all arguments.
#
# Input: d  - ces_long joined to counts, with a `model` column and one
#             probability column (unfitted `p`, or fitted via apply_fitted)
#        meta - choice, condObs, condVerb, pChoice  (from allP in script 08)
# ==============================================================================

library(here)
library(tidyverse)


MODEL_LABELS <- c(
  postces = "Full",
  postns = "No selection",
  noInf = "No inference",
  noInf_ns = "Neither"
)


load(here('Exp2Explanation', 'Model', 'Data', 'fitted.rda')) # 6144 of 15 in d, or ces_long
#load(here('Exp2Explanation', 'Annotation', 'Data', 'counts.rda')) #
#load(here('Exp2Explanation', 'Model', 'Data', 'scenariosSimple.rda')) # for allP
load(here('Exp2Explanation', 'Model', 'Data', 'all.rda')) # all, all2, forplot. bit redundnant now but need the condVerb that was put in all2 in old 9 script
# can later tidy that but need it for now

d1 <- d |> # all 1991 answers
  filter(model == "postces")


# ==============================================================================
# 1. Apply fitted taus, one per variant
# ==============================================================================
# `fits` is the table from the profile step: model, tau2_hat.
# Skip this if you want the unfitted p = newProb / sum(newProb).

# apply_fitted <- function(d, fits, col = "newProb") {
#   d |>
#     left_join(select(fits, model, tau2_hat), by = "model") |>
#     group_by(model, choice, condObs) |>
#     mutate(
#       z = (.data[[col]] - max(.data[[col]])) / tau2_hat,
#       p = exp(z) / sum(exp(z))
#     ) |>
#     ungroup()
# }

# ==============================================================================
# 2. Collapse node3 to cause for presentation
# ==============================================================================
# The collapse happens AFTER the softmax, never before: summing probabilities
# across states is linear, the softmax that produced them is not.

make_forplot <- function(d, meta, prob_col = "p") {
  d |>
    rename(pr = all_of(prob_col)) |>
    group_by(model, choice, condObs, cause) |>
    summarise(model_p = sum(pr), count = sum(count), .groups = "drop") |>
    group_by(model, choice, condObs) |>
    mutate(ppts = count / sum(count), n_cell = sum(count)) |>
    ungroup() |>
    left_join(meta, by = c("choice", "condObs")) |>
    mutate(
      model = factor(model, levels = names(MODEL_LABELS), labels = MODEL_LABELS)
    )
}

# ==============================================================================
# 3. Shared pieces
# ==============================================================================

theme_ces <- function(base = 11) {
  theme_bw(base_size = base) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "bottom"
    )
}

# pChoice annotation, as in the original geom_text at lines 40-49
annot_layer <- function(fp, facet_by = "condVerb") {
  a <- fp |> distinct(across(all_of(facet_by)), pChoice)
  geom_text(
    data = a,
    aes(label = paste0("prob = ", signif(pChoice, 3))),
    x = Inf,
    y = Inf,
    hjust = 1.1,
    vjust = 1.4,
    inherit.aes = FALSE,
    size = 3
  )
}

# ==============================================================================
# 4. One model, one outcome  -- replaces all eight blocks
# ==============================================================================
# show_model = FALSE reproduces the *NM versions (lines 63, 121, 179, 235).

plot_model <- function(
  fp,
  which_choice,
  which_model = "Full",
  show_model = TRUE
) {
  dd <- fp |> filter(choice == which_choice, model == which_model)
  stopifnot(nrow(dd) > 0)

  p <- ggplot(dd, aes(x = cause, y = ppts)) +
    geom_col() +
    facet_wrap(~condVerb) +
    annot_layer(dd) +
    theme_ces() +
    labs(
      x = "Present Causes",
      y = "Proportion of explanations",
      title = paste0(
        "Choice: ",
        which_choice,
        if (show_model) paste0("   Model: ", which_model) else ""
      )
    )

  if (show_model) {
    p <- p + geom_point(aes(y = model_p), size = 1.6)
  }
  p
}

# ==============================================================================
# 5. All four variants on one outcome
# ==============================================================================
# Colour separates the variants against the same participant bars, so the
# lesion damage is a within-panel comparison rather than a between-file one.

plot_all_models <- function(fp, which_choice) {
  bars <- fp |> filter(choice == which_choice, model == levels(fp$model)[1])
  pts <- fp |> filter(choice == which_choice)

  ggplot(bars, aes(x = cause, y = ppts)) +
    geom_col(fill = "grey80") +
    geom_point(
      data = pts,
      aes(y = model_p, colour = model, shape = model),
      size = 1.8,
      position = position_dodge(width = 0.5)
    ) +
    facet_wrap(~condVerb) +
    annot_layer(bars) +
    scale_colour_brewer(palette = "Dark2", name = NULL) +
    scale_shape_manual(values = c(16, 17, 15, 3), name = NULL) +
    theme_ces() +
    labs(
      x = "Present Causes",
      y = "Proportion of explanations",
      title = paste0("Choice: ", which_choice, "   All variants")
    )
}

# ==============================================================================
# 6. A pair of variants
# ==============================================================================

plot_pair <- function(fp, model_a, model_b, which_choice) {
  bars <- fp |> filter(choice == which_choice, model == model_a)
  pts <- fp |> filter(choice == which_choice, model %in% c(model_a, model_b))

  ggplot(bars, aes(x = cause, y = ppts)) +
    geom_col(fill = "grey80") +
    geom_line(
      data = pts,
      aes(y = model_p, group = cause),
      colour = "grey45",
      linewidth = 0.3
    ) +
    geom_point(
      data = pts,
      aes(y = model_p, colour = model, shape = model),
      size = 2
    ) +
    facet_wrap(~condVerb) +
    annot_layer(bars) +
    scale_colour_manual(values = c("firebrick", "steelblue"), name = NULL) +
    scale_shape_manual(values = c(16, 17), name = NULL) +
    theme_ces() +
    labs(
      x = "Present Causes",
      y = "Proportion of explanations",
      title = paste0("Choice: ", which_choice, "   ", model_a, " vs ", model_b),
      subtitle = "Grey segments join the two variants' predictions for the same cause"
    )
}

# Where does the pair disagree, and does the disagreement help or hurt?
# x > 0 means model_a puts more probability there than model_b;
# y > 0 means participants chose it more often than model_b predicted.
plot_pair_scatter <- function(fp, model_a, model_b) {
  w <- fp |>
    filter(model %in% c(model_a, model_b)) |>
    select(model, choice, condObs, cause, model_p, ppts, n_cell) |>
    pivot_wider(names_from = model, values_from = model_p)

  ggplot(
    w,
    aes(x = .data[[model_a]] - .data[[model_b]], y = ppts - .data[[model_b]])
  ) +
    geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
    geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.3) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      colour = "grey50"
    ) +
    geom_point(aes(size = n_cell, colour = choice), alpha = 0.5, stroke = 0) +
    scale_colour_brewer(palette = "Dark2", name = NULL) +
    scale_size_continuous(range = c(0.6, 3), name = "responses in cell") +
    theme_ces() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    labs(
      x = paste0(model_a, " minus ", model_b, "  (model)"),
      y = paste0("participants minus ", model_b),
      title = paste0(
        "Does ",
        model_a,
        " move in the direction the data wants?"
      ),
      subtitle = "Points near the dashed line: the shift is exactly what participants did"
    )
}

# ==============================================================================
# DRIVER
# ==============================================================================
meta <- all2 |> distinct(choice, condObs, condVerb, pChoice)
fp <- make_forplot(d, meta)


# Call the four plots separately

# First define the place to save so I dont have to call it every time
fig <- here('Exp2Explanation', 'Model', 'Figures')


p1 <- plot_model(fp, "LongPizza", "Full")
p1
ggsave("pLongPizza.pdf", p1, path = fig, width = 12, height = 12)

p2 <- plot_model(fp, "ShortPizza", "Full")
p2
ggsave("pShortPizza.pdf", p2, path = fig, width = 12, height = 12)

p3 <- plot_model(fp, "ShortHotdog", "Full")
p3
ggsave("pShortHotdog.pdf", p3, path = fig, width = 12, height = 12)

p4 <- plot_model(fp, "LongHotdog", "Full")
p4
ggsave("pLongHotdog.pdf", p4, path = fig, width = 12, height = 12)


# ---------- simpler headline plot ---------

CAUSE_ORDER <- c("P", "K", "C", "S", "Pu", "Ku", "Cu", "Su", "br") # edit to your graph

pooled <- fp |>
  filter(model == "Full") |>
  group_by(choice, cause) |>
  summarise(
    count = sum(count),
    model_p = weighted.mean(model_p, w = n_cell),
    .groups = "drop"
  ) |>
  group_by(choice) |>
  mutate(ppts = count / sum(count)) |>
  ungroup() |>
  mutate(cause = factor(cause, levels = CAUSE_ORDER))

pfullheadline <- ggplot(pooled, aes(x = cause)) +
  geom_col(aes(y = ppts), fill = "grey75", width = 0.7) +
  geom_point(aes(y = model_p), colour = "firebrick", size = 2.2) +
  facet_wrap(~choice, nrow = 1) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Cause cited",
    y = "Proportion of explanations",
    title = "Participants (bars) and model predictions (points)"
  )

ggsave("pFullHeadline.pdf", pfullheadline, path = fig, width = 12, height = 6)


#pooled |> group_by(choice) |> summarise(bars = sum(ppts), points = sum(model_p))

# Older, more complicated

grid <- expand_grid(choice = unique(fp$choice), show = c(TRUE, FALSE))

pwalk(grid, function(choice, show) {
  ggsave(
    paste0("p", choice, if (!show) "NM" else "", ".pdf"),
    plot_model(fp, choice, "Full", show_model = show),
    path = fig,
    width = 12,
    height = 12,
    units = "in"
  )
})

walk(unique(fp$choice), \(ch) {
  ggsave(
    paste0("pAll_", ch, ".pdf"),
    plot_all_models(fp, ch),
    path = fig,
    width = 12,
    height = 12,
    units = "in"
  )
})

ggsave(
  "pPair_full_noSelect.pdf",
  plot_pair(fp, "Full", "No selection", "LongPizza"),
  path = fig,
  width = 12,
  height = 12,
  units = "in"
)

ggsave(
  "pPairScatter_full_noSelect.pdf",
  plot_pair_scatter(fp, "Full", "No selection"),
  path = fig,
  width = 6,
  height = 5,
  units = "in"
)
