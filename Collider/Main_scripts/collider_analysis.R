#################################################### 
###### Collider analysis - compare model preds #####
####################################################

# Script takes the processed data from the collider ppt expt 
# cleaned in `pilot_preprocessing.R` and the model predictions
# and puts them together

library(tidyverse)
rm(list=ls())

# Setwd - won't need to do this if still in the masterscript
setwd("/Users/stephaniedroop/Documents/GitHub/gw/Collider/Main_scripts")

# Read in processed ppt data
load('../processed_data/processed_data.rdata', verbose = T)

# Read in model preds # see master script for indexing etc -- if change what is saved in masterscript, it'll affect here
load('../model_data/modpreds.rdata', verbose = T) # list of 3

modpredsc <- mod_preds[[1]][[2]] # 64 obs of 18
modpredsd <-mod_preds[[1]][[1]]

# Or maybe don't need, if plotting before could use multiple dfs at a time, just substitute in
wad <- as.data.frame(modpredsd %>% group_by(trialtype,node3) %>% summarise(predicted = sum(wa)))
wac <- modpredsc %>% group_by(trialtype,node3) %>% summarise(predicted = sum(wa))

# Also filter by prob group - can't filter by prob group because there isn't one in the model predictions!


# Charts for 8 bars of possible.ppts should be bars

pd <- ggplot(pg1prop, aes(x = ansVar3, y = prop,
                           fill = ansVar3)) +
  geom_col(aes(x = ansVar3, y = prop), alpha = 0.4) +
  facet_wrap(~trialtype) + #, scales='free_x'
  geom_point(aes(modpredsd$node3, y = modpredsd$wa), size=3) + # this bit not working - check 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  guides(fill = guide_legend(override.aes = list(size = 0, alpha=0.4))) +
  labs(x='Node', y='Effect size', fill='Weighted average \neffect size', 
       shape='Assuming unobserved \nvariables are...', 
       colour='Assuming unobserved \nvariables are...',
       title = 'Disjunctive collider')
       #subtitle = paste0('pA=',poss_params[[i]][1,2],', pAu=',poss_params[[i]][2,2], 
                         #', pB=',poss_params[[i]][3,2], ', pBu=',poss_params[[i]][4,2]))


pd


# Section to split ppt data into c and d


pg1c <- pg1 %>% filter(grepl("^c", trialtype))
pg1d <- pg1 %>% filter(grepl("^d", trialtype))
pg2c <- pg2 %>% filter(grepl("^c", trialtype))
pg2d <- pg2 %>% filter(grepl("^d", trialtype))
pg3c <- pg3 %>% filter(grepl("^c", trialtype))
pg3d <- pg3 %>% filter(grepl("^d", trialtype))

# Put together for plotting
