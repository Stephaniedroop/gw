###############################################################################################
##### Masterscript of model selection and other modelling of gwExp1 data #############
###############################################################################################

# Usual prelims
library(tidyverse)
rm(list = ls())

# Run source scripts
source('01targetDist.R') # obtain target distribution from cleaned Exp1 behavioural data
source('02findModel.R') # to find best fitting causal model from target distibution
source('03tidyModel.R') # to tidy up the model output
source('04graphs.R') # a function to make formulas from the best fitting model structure, to send to dagify, daggity, ggdag
# Done for path but not yet for destination (can prob use same function). Btw I deleted all the old script. Look on gwnotes for graphing or .rmd if you still need it
