######
# Masterscript to source files needed to process, merge and rate the rated Explanations

# Load libraries
library(tidyverse)

# Source scripts
source("scripts/01processRatings.R")
source("scripts/02mergeRatings.R")
source("scripts/03mergeData.R")