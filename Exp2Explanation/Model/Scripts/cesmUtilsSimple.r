# ==============================================================================
##  CESM function
# ==============================================================================

# Or call this in the same place you call the cesm functions too: these functions use those ones
#source(here('Exp2Explanation', 'Model', 'Scripts', 'semUtilsSimple.R')) # for pathlik and foodlik functions

# ---- Only save mp --------
get_cesm <- function(df, gen_pairs, prev_pairs, params) {
  n_causes <- length(causes1)
  p <- params[[2]]
  pvec <- rep(p, times = N_cf)
  mp <- df

  # Next 3 rows put empty spaces in the mp df to be filled with the CES and cf counts for each cause, and the E count
  ces_cols <- paste0(causes1, "ces")
  cfcounts <- paste0(causes1, "cfs")
  mp[c(ces_cols, cfcounts)] <- NA

  worlds <- nrow(mp)
  pb <- txtProgressBar(min = 0, max = worlds, style = 3)

  for (c_ix in 1:worlds) {
    resample <- runif(n_causes * N_cf) > s
    case <- mp[c_ix, ]
    cf_csrep <- rep(as.numeric(case[causes1]), times = N_cf)
    cf_csrep[resample] <- rbinom(sum(resample), size = 1, prob = pvec[resample])
    cfs <- data.frame(matrix(cf_csrep, nrow = N_cf, byrow = TRUE))
    colnames(cfs) <- causes1
    cfs$E <- sem_lik(cfs, gen_pairs = gen_pairs, prev_pairs = prev_pairs)
    cfs$Match <- cfs$E == case$sem
    cor_sizes <- rep(NA, n_causes)
    realcfs <- rep(NA, n_causes)

    for (cause in 1:n_causes) {
      cause_vec <- cfs[[causes1[cause]]]
      actual_val <- as.numeric(case[[causes1[cause]]])
      sign_flip <- c(-1, 1)[actual_val + 1]
      realcfs[cause] <- sum(cause_vec != actual_val)
      if (sd(cause_vec) == 0 || sd(cfs$Match) == 0) {
        cor_sizes[cause] <- 0
      } else {
        cor_sizes[cause] <- cor(cause_vec, cfs$Match, method = 'pearson') *
          sign_flip
      }
    }

    mp[c_ix, ces_cols] <- t(cor_sizes)
    mp[c_ix, cfcounts] <- t(realcfs)
    mp[c_ix, "E_count"] <- sum(cfs$E == case$sem)
    setTxtProgressBar(pb, c_ix)
  }

  close(pb)
  mp
}


# ---- A chunky version that saves all the cfs. V heavy -------
# Differences: def cfs_list, set world index, returns cfs as well as mp
get_cesm_cfs <- function(df, gen_pairs, prev_pairs, params) {
  n_causes <- length(causes1)
  p <- params[[2]] # p(var==1)
  pvec <- rep(p, times = N_cf) # Turn it into a 40k vec
  mp <- df
  ces_cols <- paste0(causes1, "ces")
  cfcounts <- paste0(causes1, "cfs")
  all_pred_cols <- c(ces_cols, cfcounts)
  mp[all_pred_cols] <- NA
  worlds <- nrow(mp)

  # Empty list to collect all cfs data.frames
  cfs_list <- vector("list", length = worlds)

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

    # Tag the world index
    cfs$world_index <- c_ix

    cfs$E <- sem_lik(cfs, gen_pairs = gen_pairs, prev_pairs = prev_pairs)

    # Add column T/F for whether the Effect in the cf worlds matches the real world
    cfs$Match <- cfs$E == case$sem

    # Set up empty vector of correlations (ie causal effect sizes), one for each cause
    cor_sizes <- rep(NA, n_causes)
    realcfs <- rep(NA, n_causes)

    # Get the CES (cor)
    for (cause in 1:n_causes) {
      # Extract some key parts so as not to repeat them in the cor() function and make it more readable.
      cause_vec <- cfs[[causes1[cause]]]
      actual_val <- as.numeric(case[[causes1[cause]]])
      # Make the correlation negative when the cause pushes against the effect
      sign_flip <- c(-1, 1)[actual_val + 1]

      # Get actual counts of cfs, to check it works
      realcfs[cause] <- sum(cause_vec != actual_val)

      # Assogn 0 is no cfs instead of NA so it doesn't break the rest; 0 is still maeningful eg in var S when there are other preventions
      if (sd(cause_vec) == 0 || sd(cfs$Match) == 0) {
        cor_sizes[cause] <- 0
      } else {
        cor_sizes[cause] <- cor(cause_vec, cfs$Match, method = 'pearson') *
          sign_flip
      }
    }

    # Now put these correlations in the mp df, along with the number of actual cfs simulated, and how many times the Effect matched
    mp[c_ix, ces_cols] <- t(cor_sizes)
    mp[c_ix, cfcounts] <- t(realcfs)
    mp[c_ix, "E_count"] <- sum(cfs$E == case$sem)
    # NEW: store the full cfs block for this world
    cfs_list[[c_ix]] <- cfs
    setTxtProgressBar(pb, c_ix)
  }
  close(pb)
  # Return both structures
  list(
    mp = mp,
    cfs_list = cfs_list
  )
}
