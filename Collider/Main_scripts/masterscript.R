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

# --------------- 3. Assess model fit to ppt data --------------
# Facet plots for each condition showing dots against coloured bars
source('plot_model_to_ppt.R')

# Other metrics of model fit
source('morefit.R')
