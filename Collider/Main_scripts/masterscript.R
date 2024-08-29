##############################################################
#### Master script for collider within gridworld project #####
##############################################################

# setwd("/Users/stephaniedroop/Documents/GitHub/gw/Collider/Main_scripts")

#library(tidyverse)
rm(list=ls())

#------- 1. Create parameters, run cesm, get model predictions and save them ------------
source('get_model_preds.R')

#--------------- 2. Get ppt data  -------------------
source('mainbatch_preprocessing.R') # saves data
# Setwd back again - it might still be in the previous script's one 
# (which it needed there to use a nifty one line to get the data docs out and together)
setwd("../Main_scripts")
source('collider_analysis.R') # combine with model predictions

# --------------- 3. Now plot --------------
source('plot_model_to_ppt.R')


# Notes on chart
# If we normalised the bars then it would give the prob matching approach. 
# Need no y axis because it suggests probability when it isn't
# Try normalised so it becomes probability, then plot ppt against this. 
# (For real thing it will have optimised softmax temp but at least we can see)

