# ==============================================================================
##  CESM function
# ==============================================================================

# Or call this in the same place you call the cesm functions too: these functions use those ones
source(here('Exp2Explanation', 'Model', 'Scripts', 'semUtils.R')) # for pathlik and foodlik functions

# So.... what is different from the one in collider? (what can I re-use?) also what is differnet from E-CESM

# Needed as input from the previous function
# - params that lists the base rates and strengths of exog noise u vars
# - a df of all the world combos with probs

# Uses all_params_full which is static, can maybe do it outside, like newcols
get_cesm <- function(df, structure, params) {
  # The p_eachvar==1
  #params <- all_params_full[match(causes1, rownames(all_params_full)), ]
  n_causes <- length(causes1)
  p <- params[[2]]
  pvec <- rep(p, times = N_cf) # Turn it into a 40k vec
  mp <- df
  ces_cols <- paste0(causes1, "ces")
  cfcounts <- paste0(causes1, "cfs")
  all_pred_cols <- c(ces_cols, cfcounts)
  mp[all_pred_cols] <- NA
  worlds <- nrow(mp)

  pb <- txtProgressBar(min = 0, max = worlds, style = 3)

  # Loop through possible world settings: 32k for each of path and food
  for (c_ix in 1:worlds) {
    # STABILITY: Generate vector of random numbers. The ones outside stability s are to be resampled. Put T for them
    resample <- runif(n_causes * N_cf) > s # 40k vec, with T for ones higher than the stability param
    # Take the current case as the real world
    case <- mp[c_ix, ]
    # Repeat the cause settings of the current world, to be cf sampled
    cf_csrep <- rep(as.numeric(case[causes1]), times = N_cf)
    #cf_csrep <- rep(as.numeric(case[1:n_causes]), times = N_cf) # vec
    # Now resample from its prior each value whose place in resample was set to TRUE in stability step
    cf_csrep[resample] <- rbinom(sum(resample), size = 1, prob = pvec[resample])
    # Express these generated counterfactuals in tabular form again
    cfs <- data.frame(matrix(cf_csrep, nrow = N_cf, byrow = T))
    colnames(cfs) <- causes1

    # Recalculate determinative effect for these simulated cf worlds: SEM.
    # Can use functions defined in semUtils

    # First the interaction nodes
    cfs <- add_interactions(cfs, base_causes)

    # Recalculate determinative effect for these simulated cf worlds
    if (structure == "path") {
      cfs$E <- pathlik_vec(cfs)
    }
    if (structure == "food") {
      cfs$E <- foodlik_vec(cfs)
    }

    # Add column T/F for whether the Effect in the cf worlds matches the real world
    cfs$Match <- cfs$E == case$sem

    # Set up empty vector of correlations (ie causal effect sizes), one for each cause
    cor_sizes <- rep(NA, n_causes)
    realcfs <- rep(NA, n_causes)
    for (cause in 1:n_causes) {
      # ..And then populate! (the second part sets correlation negative when cause pushes against effect taking state it took)
      cor_sizes[cause] <- cor(
        cfs[[causes1[cause]]],
        cfs$Match,
        method = 'pearson'
      ) *
        (c(-1, 1)[as.numeric(case[[causes1[cause]]]) + 1])
      realcfs[cause] <- sum(cfs[[causes1[cause]]] != case[[causes1[cause]]]) # counts how many cf worlds changed the cause

      setTxtProgressBar(pb, c_ix)
    }
    close(pb)
    # Now put these correlations in the mp df, along with the number of actual cfs simulated, and how many times the Effect matched
    mp[c_ix, ces_cols] <- t(cor_sizes)
    mp[c_ix, cfcounts] <- t(realcfs)
    mp[c_ix, "E_count"] <- sum(cfs$E == case$sem)
  }
  #mp$index <- 1:nrow(mp)
  mp
}
