# ==============================================================================
##  CESM function
# ==============================================================================

# So.... what is different from the one in collider? (what can I re-use?) also what is differnet from E-CESM


# Needed as input from the previous function
# - params that lists the base rates and strengths of exog noise u vars
# - a df of all the world combos with probs

cesm <- function(params, world_combos, df) {
  n_causes <- nrow(params)
  p <- params[,2] # The p_eachvar==1 
  pvec <- rep(p, times = N_cf) # Turn it into a 40k vec
  mp <- df |> relocate(Au, .before = B) # assuming we can do it in one line - reassignment takes a lot of time
  # Add new columns then fill them
  new_cols <- c('mA', 'mAu', 'mB', 'mBu', 'cfsA', 'cfaAu', 'cfsB', 'cfsBu', 'Sum') # do we still need this????
  mp[new_cols] <- NA
  worlds <- nrow(df) #as.integer(nrow(df)/2)
  
  # Loop through possible world settings: it's actually not 16 but 20(c) and 28(d) because we need scores for all combinations even post=0
  for (c_ix in 1:worlds)
  {
    # STABILITY: Generate vector of random numbers. The ones outside stability s are to be resampled. Put T for them
    resample <- runif(n_causes*N_cf) > s # 40k vec, with T for ones higher than the stability param 
    # Take the current case as the real world
    case <- mp[c_ix,] 
    # Repeat the cause settings of the current world, to be cf sampled
    cf_csrep <- rep(as.numeric(case[1:n_causes]), times = N_cf) # 40k vec
    # Now resample from its prior each value whose place in resample was set to TRUE in stability step
    cf_csrep[resample] <- rbinom(sum(resample), size = 1, prob = pvec[resample])  
    # Express these generated counterfactuals in tabular form again
    cfs <- data.frame(matrix(cf_csrep, nrow = N_cf, byrow = T))
    colnames(cfs) <- causes1
    
    # Pull out corresponding outcomes to see their probabilities - HOW MANY ARE THERE??
    
    # (This from old ecesm one)
    
    cf_cases<-pChoice %>% filter(as.numeric(Knowledge)==cf_cs[1],
                                 as.numeric(Preference)==cf_cs[2],
                                 as.numeric(Character)==cf_cs[3],
                                 as.numeric(Start)==cf_cs[4])
    #Sample one outcome according to its probability
    cf_out_ix<-sample(x=1:4, size = 1, p=cf_cases$p_action)
    
    
    
    # Add column T/F for whether the Effect in the cf worlds matches the real world
    cf$Match = cf$Choice==case$Choice & cf$Path==case$Path # had it as both outcomes in the same time before
    
    # Set up empty vector of correlations (ie causal effect sizes), one for each cause
    cor_sizes <- rep(NA, n_causes)
    realcfs <- rep(NA, n_causes)
    for (cause in 1:n_causes)
    {
      # ..And then populate! (the second part sets correlation negative when cause pushes against effect taking state it took)
      cor_sizes[cause] <- cor(cfs[[causes1[cause]]], 
                              cfs$Match, method = 'pearson') * 
        (c(-1,1)[as.numeric(case[[causes1[cause]]])+1])
      realcfs[cause] <- sum(cfs[[causes1[cause]]]!=case[[causes1[cause]]])
    }
    # Now put these correlations in the mp df, along with the number of actual cfs simulated, and how many times the Effect matched 
    mp[c_ix, 18:21] <- t(cor_sizes)
    mp[c_ix, 22:25] <- t(realcfs)
    mp[c_ix, 26] <- sum(cfs$E == case$E.x)
    mp$index <- 1:nrow(mp)
  }
  mp
}
