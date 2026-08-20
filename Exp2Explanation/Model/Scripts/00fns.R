# 00fns.R
library(tidyverse)

loglik_tau <- function(tau2, dat, col = "newProb") {
  dat |>
    group_by(choice, condObs) |>
    mutate(
      z = (.data[[col]] - max(.data[[col]])) / tau2,
      logp = z - log(sum(exp(z)))
    ) |>
    ungroup() |>
    summarise(ll = sum(count * logp)) |>
    pull(ll)
}

fit_tau2 <- function(dat, lo = 1e-3, hi = 10) {
  dat |>
    group_by(model) |>
    group_modify(
      ~ {
        o <- optimize(\(lt) -loglik_tau(exp(lt), .x), c(log(lo), log(hi)))
        tibble(tau2_hat = exp(o$minimum), ll = -o$objective)
      }
    ) |>
    ungroup()
}
