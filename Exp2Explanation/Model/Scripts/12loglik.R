# -----------------------------------------------
# -----  baseline fit    -------------
# -----------------------------------------------

# Import
library(here)
library(tidyverse)


#load(here('Exp2Explanation', 'Model', 'Data', 'joined.rda'))
load(here('Exp2Explanation', 'Model', 'Data', 'all.rda'))

# Replace NA with 0 for the loglik, not just na.rm because we need to know it was there
all2 <- all2 |> mutate(count = replace_na(count, 0))


# Some simple correlations for checking lesions and fit
# Get correlations for each condObs
corall <- all2 |>
  #group_by(condObs) |>
  summarise(corr = cor(count_norm, postces_norm, use = "complete.obs"))


cor_longPizza <- longPizza_join |>
  group_by(condObs) |>
  summarise(corr = cor(count_norm, postces_norm, use = "complete.obs"))

corlP <- cor_longPizza |>
  summarise(mean_corr = mean(corr), sd_corr = sd(corr))

corlP

cor_shortPizza <- shortPizza_join |>
  group_by(condObs) |>
  summarise(corr = cor(count_norm, postces_norm, use = "complete.obs"))

corsP <- cor_shortPizza |>
  summarise(mean_corr = mean(corr), sd_corr = sd(corr))

corsP

cor_longHotdog <- longHotdog_join |>
  group_by(condObs) |>
  summarise(corr = cor(count_norm, postces_norm, use = "complete.obs"))

corlH <- cor_longHotdog |>
  summarise(mean_corr = mean(corr), sd_corr = sd(corr))

corlH

cor_shortHotdog <- shortHotdog_join |>
  group_by(condObs) |>
  summarise(corr = cor(count_norm, postces_norm, use = "complete.obs"))

corsH <- cor_shortHotdog |>
  summarise(mean_corr = mean(corr), sd_corr = sd(corr))

corsH


# Get a simple loglik before any sort of optim and fitting
loglik <- function(tau2, eps, d, col = "newProb") {
  d |>
    group_by(choice, condObs) |>
    mutate(
      x = .data[[col]],
      ok = !is.na(x),
      z = ifelse(ok, (x - max(x[ok])) / tau2, NA_real_),
      pm = ifelse(ok, exp(z) / sum(exp(z[ok])), 0), # sums to 1 over supported
      p = (1 - eps) * pm + eps / n() # n() counts ALL options
    ) |>
    ungroup() |>
    summarise(nll = -sum(count * log(p))) |> # -sum(log(mpred) * tr$n)
    pull(nll)
}

loglik(0.25, 0.05, all2) # -6127 is the baseline. Then 7833 with all the responses added in ann3

sum(all2$count) # 1991
loglik(0.25, all2) # negative
loglik(0.25, all2) > loglik(1e6, all) # beats uniform: TRUE


chk <- all2 |>
  group_by(choice, condObs) |>
  mutate(
    z = (newProb - max(newProb)) / 0.25,
    p = exp(z) / sum(exp(z)),
    term = count * log(p)
  ) |>
  ungroup()

colSums(is.na(chk[c("newProb", "count", "z", "p", "term")]))

nas <- all2 |>
  filter(is.na(newProb))

nas2 <- nas |>
  group_by(condObs, node3) |>
  summarise(n = n())

# How many of nas2 are P=0 or P=1: 99
countP <- nas2 |>
  filter(node3 %in% c('P=1', 'P=0'))

countC <- nas2 |>
  filter(node3 %in% c('C=1', 'C=0'))
