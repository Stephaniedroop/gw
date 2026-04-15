##############################################
# Masterscript to source files needed to process, merge and rate the Explanations from Gridworld experiment 2

# Load libraries
library(tidyverse)

# Source scripts
source("01processData.R") # Input 'rerundata.csv', output 'processedData.rda'. Not affected by any further actions

source("05explorRegress.R") # Go back to - not finished

# Then got claude to do ratings instead. Got forced choice in March 2026

# source("annotatedCES.R") # Seems not used and pointless as later I did IRR on mine and claude's training set in this next script irr:

# TO NUMBER AND INCORPORATE

source("claude_irr.R") # cohen's kappa of 64.9% on the train1 set, which is ok by me,
#so I proceeded to get claude to rate the whole next set. This was then

source("processClaude.R") # just processes and resaves it
# later modelling script on modeling folder splits to series of 4 dfs to do separate modelling on later

# Old and not used
# 'gw_data.R'
# 'gw_irr.R' - used for the V and I
# "annotatedCES.R"
# source("02processRatings.R") # This was V and I ratings; if we don't use that then can ditch this
# source("03mergeRatings.R") # and this
# source("04mergeData.R") # not needed. started with 2040 where dtaa alrady had duplictes removed, then the same had to be performed on V and I so they all ended as 2040
