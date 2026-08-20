# ==============================================================================
# 07reorgPredsGen.R
# Generalised replacement for 07reorgPreds.R.
#
# The original hardcodes `postces` at lines 101-110 and repeats the merge and
# the softmax four times, once per outcome. This version loops over both the
# four outcomes and the four model variants, so adding a fifth lesion means
# adding one string to MODELS and nothing else.
#
# Input:  path_ces, food_ces from ces_sepSimpleN.Rda (built by script 06)
# Output: one long frame, plus a wide helper for backward compatibility with
#         08joinCESwAnns.R and 09plotCESSimple.R
# ==============================================================================

library(here)
library(tidyverse)

MODELS <- c(
  postces = "full", # sum(posterior * s_hat_ces)
  postns = "noSelect", # sum(posterior * s_hat_noSelect)
  noInf = "noInf", # sum(prior     * s_hat_ces)
  noInf_ns = "noInf_noSelect"
)

# Function here called by gettau1.R in the manual fitting of tau1

# ==============================================================================
# The one function
# ==============================================================================

reorg_preds <- function(
  path_ces,
  food_ces,
  models = names(MODELS)
  #tau = 0.25
) {
  # --- one side (path or food), reshaped so the variant is a column ----------
  prep <- function(d, lab0, lab1, dom) {
    missing_cols <- setdiff(models, names(d))
    if (length(missing_cols)) {
      stop("Missing model columns: ", paste(missing_cols, collapse = ", "))
    }

    d <- d |>
      mutate(
        cat = case_when(
          str_ends(condition, "0") ~ lab0,
          str_ends(condition, "1") ~ lab1,
          TRUE ~ "other"
        ),
        condObs = substr(condition, 2, 5),
        # br, Pu and Ku exist in both graphs, so tag which graph this row is from
        node3 = gsub("^(br|Pu|Ku)=", paste0("\\1_", dom, "="), node3)
      )

    # The original merge() joined on (condObs, node3) only. If any other column
    # (sem, for instance) varies within that key, the original was silently
    # duplicating rows. Catch it here rather than downstream.
    dup <- d |> count(cat, condObs, node3) |> filter(n > 1)
    if (nrow(dup)) {
      stop(
        "Duplicate (cat, condObs, node3) keys: ",
        nrow(dup),
        " cases. First: ",
        paste(dup[1, ], collapse = " / ")
      )
    }

    d |>
      select(cat, condObs, node3, all_of(models)) |>
      pivot_longer(all_of(models), names_to = "model", values_to = "score")
  }

  path_long <- prep(path_ces, "Short", "Long", "p")
  food_long <- prep(food_ces, "Pizza", "Hotdog", "f")

  # --- the four outcomes are the Cartesian product of the two graph labels ---
  combos <- expand_grid(
    path_cat = c("Long", "Short"),
    food_cat = c("Pizza", "Hotdog")
  ) |>
    mutate(choice = paste0(path_cat, food_cat))

  out <- pmap(combos, function(path_cat, food_cat, choice) {
    pd <- path_long |>
      filter(cat == path_cat) |>
      select(-cat) |>
      rename(path = score)
    fd <- food_long |>
      filter(cat == food_cat) |>
      select(-cat) |>
      rename(food = score)

    full_join(pd, fd, by = c("condObs", "node3", "model")) |>
      mutate(
        in_path = !is.na(path),
        in_food = !is.na(food),
        path = replace_na(path, 0),
        food = replace_na(food, 0),
        # ADD, not multiply: a variable absent from one graph would otherwise
        # zero out its score in the other
        newProb = (path + food) / 2,
        choice = choice
      )
  }) |>
    list_rbind()

  # --- second softmax, one per (choice, condObs, model) ----------------------
  # The max subtraction is not cosmetic: once tau becomes a fitted parameter the
  # optimiser proposes small values where exp(newProb/tau) overflows to Inf.
  out |>
    group_by(choice, condObs, model) |>
    # mutate(
    #   z = (newProb - max(newProb)) / tau,
    #   pmod = exp(z) / sum(exp(z))
    # ) |>
    ungroup() |>
    mutate(
      cause = sub("_(p|f)$", "", sub("=.*", "", node3)),
      model = factor(model, levels = models)
    ) |>
    select(
      choice,
      condObs,
      node3,
      cause,
      model,
      path,
      food,
      newProb,
      #pmod,
      in_path,
      in_food
    )
}

# ==============================================================================
# Backward compatibility
# ==============================================================================
# 08joinCESwAnns.R expects four separate frames with columns `newProb` and
# `postces_norm`. This rebuilds them for whichever variant you name, so the rest
# of the existing pipeline runs unchanged.

to_wide <- function(long, which_model = "postces") {
  d <- long |>
    filter(model == which_model) |>
    mutate(postces_norm = pmod, Relevant = in_path) |>
    select(choice, condObs, node3, cause, newProb, postces_norm, Relevant)

  split(d, d$choice) # names: LongHotdog, LongPizza, ShortHotdog, ShortPizza
}

# ==============================================================================
# Regression check: does the generalised code reproduce the original?
# ==============================================================================
# Run this once against the saved ces4Outcomes.rda before trusting anything.

check_against_original <- function(long, old_longPizza_ces, tol = 1e-10) {
  new <- long |>
    filter(model == "postces", choice == "ShortPizza") |>
    select(condObs, node3, newProb_new = newProb, norm_new = pmod)

  old <- old_longPizza_ces |>
    select(condObs, node3, newProb_old = newProb, norm_old = postces_norm) |>
    mutate(across(c(condObs, node3), as.character))

  cmp <- full_join(
    new,
    mutate(old, across(c(condObs, node3), as.character)),
    by = c("condObs", "node3")
  )

  list(
    rows_only_new = sum(is.na(cmp$newProb_old)),
    rows_only_old = sum(is.na(cmp$newProb_new)),
    max_diff_newProb = max(
      abs(cmp$newProb_new - cmp$newProb_old),
      na.rm = TRUE
    ),
    max_diff_norm = max(abs(cmp$norm_new - cmp$norm_old), na.rm = TRUE),
    identical = isTRUE(
      max(abs(cmp$newProb_new - cmp$newProb_old), na.rm = TRUE) < tol
    )
  )
}
