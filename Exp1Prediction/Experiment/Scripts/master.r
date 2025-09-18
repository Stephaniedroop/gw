####### 
# Masterscript to run analysis scripts to clean and process Exp1 participant data and get it ready for modelling
#######

# Set working directory to the folder containing this script


# Usual prelims
library(tidyverse)
rm(list = ls())

# Run source scripts
source('01preprocess.R')
