###############################################################################################
##### Masterscript of model selection and other modelling of gwExp1 data #############
###############################################################################################

# Usual prelims
library(tidyverse)
rm(list = ls())

# Run source scripts

source(knitr::purl('find_modelSD.Rmd')) # to find best fitting causal model from cleaned Exp1 behavioural data

source('01findModel.R') # to find best fitting causal model from cleaned Exp1 behavioural data
source('02tidyModel.R') # to tidy up the model output