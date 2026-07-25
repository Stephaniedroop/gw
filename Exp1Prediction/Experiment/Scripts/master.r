###################################################################################
# Masterscript to run analysis scripts to clean and process Exp1 participant data and get it ready for modelling
###################################################################################

# Usual prelims
library(tidyverse)

# Run source scripts
source('01preprocess.R') # May changed the order of the factors PKCS so it flows through
source('02describe.R')
source('03extra.R') # extra analyses to check for patterns in the data
