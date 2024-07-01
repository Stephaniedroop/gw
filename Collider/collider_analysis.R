#################################################### 
###### Collider analysis - compare model preds######
####################################################

# Script takes the processed data from the collider ppt expt 
# cleaned in `pilot_preprocessing.R` and the model predictions
# and puts them together

library(tidyverse)
rm(list=ls())

# Setwd 
setwd("/Users/stephaniedroop/Documents/GitHub/gw/Collider")

# The operative model preds are 2,1,11, with the probs as follows:
# ['10%', '50%', '80%', '50%'],
# ['50%', '10%', '50%', '80%'],
# ['10%', '70%', '80%', '50%']
# (Haven't yet reversed the probs for the cb==1 group 3,4,1,2)
# I could put a tag for what model preds for each? Seems to solve it, although very different n

# TO DO
# Once got the df in smaller meaningful structure:
# - merge with model predictions - not sure how to do this!
# - is the answer even within the array of tadeg's permissable answers?
# - aim is to plot prop of ppl picking a certain answer, against the model predictions
# - but this is several steps 
# so first plot data and prop against a simplified version of the model preds (eg simple normalised), wehre ppl are bars, model is dots

# Get model preds - at mo just for one, then need to do for all. They are called same atm so problem
load('model_data/collider1.rdata', verbose = T) # less likely these - all the summary work was in the next one, unobsforplot
load('model_data/unobsforplot1.rdata', verbose = T) # Gets eg 'forplotd' - what from that do I want now?
