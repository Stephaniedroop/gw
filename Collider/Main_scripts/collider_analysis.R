#################################################### 
###### Collider analysis - compare model preds #####
####################################################

# Script takes the processed data from the collider ppt expt 
# cleaned in `pilot_preprocessing.R` and the model predictions
# and puts them together

library(tidyverse)
rm(list=ls())

# Setwd - won't need to do this if still in the masterscript
setwd("/Users/stephaniedroop/Documents/GitHub/gw/Collider")

# Read in processed ppt data
load('processed_data/processed_data.rdata', verbose = T)

# Read in model preds # see master script for indexing etc -- if change what is saved in masterscript, it'll affect here
load('model_data/modpreds.rdata', verbose = T) # list of 3
modpg1c <- mod_preds[[1]][[6]] 
modpg1d <- mod_preds[[1]][[5]]
modpg2c <- mod_preds[[2]][[6]] 
modpg2d <- mod_preds[[2]][[5]]
modpg3c <- mod_preds[[3]][[6]] 
modpg3d <- mod_preds[[3]][[5]]


# Get the indexing right for which prob group is which
# mod_preds[[i]][[5]] <- wad



# Section to split ppt data into c and d


pg1c <- pg1 %>% filter(grepl("^c", trialtype))
pg1d <- pg1 %>% filter(grepl("^d", trialtype))
pg2c <- pg2 %>% filter(grepl("^c", trialtype))
pg2d <- pg2 %>% filter(grepl("^d", trialtype))
pg3c <- pg3 %>% filter(grepl("^c", trialtype))
pg3d <- pg3 %>% filter(grepl("^d", trialtype))

# Put together for plotting
