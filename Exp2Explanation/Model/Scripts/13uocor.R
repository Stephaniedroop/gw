# Some plots to tease out correlation using uo .rda defined in script 8

load(here('Exp2Explanation', 'Model', 'Data', 'uo.rda')) # btw uo is currently saved in 08joinwanns
# as in, it includes base rates

# Basically none of this is used except the last one, ou4p

# uo1 <- uo |> filter(model == "postces")
#
# ggplot(uo1, aes(pChoice)) +
#   geom_point(
#     aes(y = ppts_unobs, size = n),
#     colour = "grey25",
#     alpha = 0.6,
#     stroke = 0
#   ) +
#   geom_point(
#     aes(y = model_unobs, size = n),
#     colour = "firebrick",
#     alpha = 0.6,
#     stroke = 0
#   ) +
#   geom_smooth(
#     aes(y = ppts_unobs, weight = n),
#     method = "glm",
#     formula = y ~ x,
#     method.args = list(family = quasibinomial),
#     colour = "grey25",
#     se = FALSE
#   ) +
#   geom_smooth(
#     aes(y = model_unobs, weight = n),
#     method = "glm",
#     formula = y ~ x,
#     method.args = list(family = quasibinomial),
#     colour = "firebrick",
#     se = FALSE
#   ) +
#   facet_wrap(~choice, nrow = 1) +
#   theme_bw() +
#   labs(x = "Probability of the outcome", y = "Share citing unobserved causes")

# gap <- uo1 |>
#   mutate(
#     gap = model_unobs - ppts_unobs,
#     condObs = fct_reorder(condObs, str_count(condObs, "1"))
#   )

# ggplot(gap, aes(condObs, gap, fill = pChoice)) +
#   geom_hline(yintercept = 0, colour = "grey40") +
#   geom_col() +
#   facet_wrap(~choice, nrow = 1) +
#   scale_fill_viridis_c(name = "p(outcome)") +
#   coord_flip() +
#   theme_bw() +
#   labs(
#     x = "Observed evidence",
#     y = "Model minus participants, unobserved share"
#   )

# EDIT these to the actual variables at each character position of condObs
# BITS <- c(p1 = "P", p2 = "K", p3 = "C", p4 = "S")
#
# bits <- uo |>
#   filter(model == "postces") |>
#   mutate(
#     p1 = substr(condObs, 1, 1),
#     p2 = substr(condObs, 2, 2),
#     p3 = substr(condObs, 3, 3),
#     p4 = substr(condObs, 4, 4)
#   ) |>
#   pivot_longer(c(p1, p2, p3, p4), names_to = "position", values_to = "state") |>
#   mutate(
#     variable = factor(BITS[position], levels = BITS),
#     state = factor(state, levels = c("0", "1"))
#   )

# bl <- bits |>
#   pivot_longer(
#     c(model_unobs, ppts_unobs),
#     names_to = "source",
#     values_to = "share"
#   ) |>
#   mutate(
#     source = factor(
#       if_else(source == "ppts_unobs", "participants", "model"),
#       levels = c("model", "participants")
#     )
#   )

# summ <- bl |>
#   group_by(variable, state, source) |>
#   summarise(m = weighted.mean(share, n))

# ggplot(summ, aes(state, m, colour = source, group = source)) +
#   geom_line(linewidth = 0.6) +
#   geom_point() +
#   geom_jitter(data = bl, aes(y = share), width = 0.08, alpha = 0.3)
# facet_wrap(~variable, nrow = 1) +
#   scale_colour_manual(
#     values = c(model = "firebrick", participants = "grey25"),
#     name = NULL
#   ) +
#   theme_bw() +
#   labs(x = "Observed state", y = "Share citing unobserved causes")

# bl |>
#   group_by(variable) |>
#   group_modify(
#     ~ {
#       m <- glm(
#         share ~ state * source,
#         family = quasibinomial,
#         weights = n,
#         data = .x
#       )
#       broom::tidy(m) |> filter(term == "state1:sourceparticipants")
#     }
#   ) |>
#   ungroup() |>
#   select(variable, estimate, std.error, p.value)

# bits_cor <- cellcor |>
#   mutate(
#     p1 = substr(condObs, 1, 1),
#     p2 = substr(condObs, 2, 2),
#     p3 = substr(condObs, 3, 3),
#     p4 = substr(condObs, 4, 4)
#   ) |>
#   pivot_longer(c(p1, p2, p3, p4), names_to = "position", values_to = "state") |>
#   mutate(variable = factor(BITS[position], levels = BITS))

# ggplot(bits_cor, aes(state, r_pearson)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_jitter(aes(size = n), width = 0.12, alpha = 0.4, stroke = 0) +
#   facet_wrap(~variable, nrow = 1) +
#   theme_bw() +
#   labs(x = "Observed state", y = "Model–participant correlation")
#
# uo2 <- uo |>
#   filter(model == "postces") |>
#   mutate(
#     condObs = fct_reorder(condObs, str_count(condObs, "1")),
#     over = model_unobs > ppts_unobs
#   )

# ou1p <- ggplot(uo2, aes(x = pChoice)) +
#   geom_segment(
#     aes(xend = pChoice, y = ppts_unobs, yend = model_unobs, colour = over),
#     linewidth = 0.7
#   ) +
#   geom_point(aes(y = ppts_unobs), colour = "grey20", size = 1.5) +
#   geom_point(aes(y = model_unobs), colour = "firebrick", size = 1.5) +
#   facet_grid(condObs ~ choice) +
#   scale_colour_manual(
#     values = c(`TRUE` = "firebrick", `FALSE` = "steelblue"),
#     labels = c(`TRUE` = "model over-reaches", `FALSE` = "model under-reaches"),
#     name = NULL
#   ) +
#   coord_cartesian(ylim = c(0, 1)) +
#   theme_bw(base_size = 8) +
#   theme(
#     panel.grid.minor = element_blank(),
#     strip.text.y = element_text(angle = 0),
#     legend.position = "bottom"
#   ) +
#   labs(
#     x = "Probability of the outcome",
#     y = "Share of explanations citing unobserved causes",
#     title = "Model (red) against participants (grey), by cell"
#   )

# ou1p
#
# ggsave(
#   here('Exp2Explanation', 'Model', 'Figures', 'unobsby64.pdf'),
#   ou1p,
#   width = 7,
#   height = 14
# )

# uo3 <- uo |>
#   filter(model == "postces") |>
#   mutate(
#     path_cat = if_else(str_starts(choice, "Long"), "Long", "Short"),
#     food_cat = if_else(str_detect(choice, "Pizza"), "Pizza", "Hotdog"),
#     hi = substr(condObs, 1, 2),
#     lo = substr(condObs, 3, 4),
#     row_key = paste0(path_cat, " | ", hi),
#     col_key = paste0(food_cat, " | ", lo),
#     over = model_unobs > ppts_unobs
#   )

# EDIT: the four variables, in the order they appear in condObs
VARS <- c("P", "K", "C", "S")

label_bits <- function(x, idx) {
  v <- VARS[idx]
  s <- vapply(idx, function(i) substr(x, i, i), character(1))
  paste(paste0(v, "=", s), collapse = ", ")
}

uo4 <- uo |>
  filter(model == "postces") |>
  rowwise() |>
  mutate(
    path_cat = if_else(str_starts(choice, "Long"), "Long", "Short"),
    food_cat = if_else(str_detect(choice, "Pizza"), "Pizza", "Hotdog"),
    row_key = paste0(path_cat, "  |  ", label_bits(condObs, 1:2)),
    col_key = paste0(food_cat, "  |  ", label_bits(condObs, 3:4)),
    over = model_unobs > ppts_unobs
  ) |>
  ungroup()

ou4p <- ggplot(uo4, aes(x = pChoice)) +
  geom_segment(
    aes(xend = pChoice, y = ppts_unobs, yend = model_unobs, colour = over),
    linewidth = 0.8
  ) +
  geom_point(aes(y = ppts_unobs), colour = "grey20", size = 1.4) +
  geom_point(aes(y = model_unobs), colour = "firebrick", size = 1.4) +
  facet_grid(row_key ~ col_key) +
  scale_colour_manual(
    values = c(`TRUE` = "firebrick", `FALSE` = "steelblue"),
    labels = c(`TRUE` = "model over-reaches", `FALSE` = "model under-reaches"),
    name = NULL
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw(base_size = 8) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text.y = element_text(angle = 0),
    legend.position = "bottom"
  ) +
  labs(x = "Probability of the outcome", y = "Share citing unobserved causes")

ou4p

ggsave(
  here('Exp2Explanation', 'Model', 'Figures', 'unobsby88.pdf'),
  ou4p,
  width = 12,
  height = 12
)
