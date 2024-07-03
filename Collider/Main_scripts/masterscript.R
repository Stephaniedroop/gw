##############################################################
#### Master script for collider within gridworld project #####
##############################################################

library(tidyverse)
rm(list=ls())

#------- 1. Create parameters, run cesm, get model predictions and save them ------------
# All the params we want, put into a list of 4x2 dfs
params1 <- data.frame("0"=c(0.9,0.5,0.2,0.5), "1"=c(0.1,0.5,0.8,0.5))
params2 <- data.frame("0"=c(0.5,0.9,0.5,0.2), "1"=c(0.5,0.1,0.5,0.8))
params3 <- data.frame("0"=c(0.9,0.3,0.2,0.5), "1"=c(0.1,0.7,0.8,0.5))
row.names(params1) <- row.names(params2) <- row.names(params3) <-c ("pA",  "peA", "pB", "peB")
names(params1) <- names(params2) <- names(params3) <- c('0','1')
poss_params <- list(params1, params2, params3)

mod_preds <- vector(mode='list', length=3)

# NOTE - RUN THIS AGAIN FOR REAL
# Loop through the list of param dfs and run a series of scripts, to generate worlds, calculate model preds and plot
for (i in 1:length(poss_params)) { 
  source('general_cesm_a.R')
  # Script contains functions, but we need to explicitly call them as dfs which will be used throughout
  dfd <- world_combos(params = poss_params[[i]], structure = 'disjunctive')
  dfc <- world_combos(params = poss_params[[i]], structure = 'conjunctive')
  mp1d <- generic_cesm(params = poss_params[[i]], df = dfd)
  mp1c <- generic_cesm(params = poss_params[[i]], df = dfc)
  # Let's save these if we need to. This works but need a better structure
  mod_preds[[i]][[1]] <- dfd # PROB. NOT NEEDED
  mod_preds[[i]][[2]] <- dfc
  #mod_preds[[1]][[i]]$dfc <- dfc
  # The next script makes dfs forplotd,forplotc, with model preds used for plotting
  source('unobs_a.R')
  # Save them too, for later 
  mod_preds[[i]][[3]] <- forplotd
  mod_preds[[i]][[4]] <- forplotc
  mod_preds[[i]][[5]] <- wad # If you change what is taken here, it will change the indexing position later
  mod_preds[[i]][[6]] <- wac
  source('collider_plot_a.R')
  # One way of charting the possible values of the unobserved variables, saved under `i`
  # eg 'da1' is disjunctive actual , params setting 1
  dchart <- paste0('~/Documents/GitHub/gw/Collider/figs/model_preds/','da',i,'.pdf')
  ggsave(dchart, plot=pd, width = 7, height = 5, units = 'in')
  cchart <- paste0('~/Documents/GitHub/gw/Collider/figs/model_preds/','ca',i,'.pdf')
  ggsave(cchart, plot=pc, width = 7, height = 5, units = 'in')
  
  # Important to find the right piece of the model predictions - what do we want to do with it?
  # assuming wa (in unobs_a, saved as wad/wac)
}

# Save 
save(mod_preds, file='model_data/modpreds.Rdata')



# Where to put json etc?



# Notes on chart
# If we normalised the bars then it would give the prob matching approach. 
# Need no y axis because it suggests probability when it isn't
# Try normalised so it becomes probability, then plot ppt against this. 
# (For real thing it will have optimised softmax temp but at least we can see)


#--------------- 2. Get ppt data and compare with model predictions -------------------




# ------------- Analysis -----------
# Still wip, see script collider_analysis.R
# (also pilot_preprocessing but I'll finish it elsewhere)
