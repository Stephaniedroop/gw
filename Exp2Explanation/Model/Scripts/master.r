# ==============================================================================
## Master script to run others in the gw Exp2 Model folder - cesm etc
# ==============================================================================

library(here)
library(tidyverse)

set.seed(12)

# Load utils
source(here('Exp2Explanation', 'Model', 'Scripts', 'cesmUtils.R'))

# Running a new series of 'simple' causal model (w/o interactions). Rename scripts if it all works and I decide to go with this as main arc

source(here('Exp2Explanation', 'Model', 'Scripts', '02getParamsSimple.R')) # input modelSimple.rda from Exp1; output paramsSimple.rda
source(here('Exp2Explanation', 'Model', 'Scripts', '03getProbsSimple.R'))
source(here('Exp2Explanation', 'Model', 'Scripts', '04getSemSimple.R'))

# still to do - check these then get predictions

# Run source scripts
# 1 and 2 are equiavalent to get_world_combos in collider proj
#source(here('Exp2Explanation', 'Model', 'Scripts', '01getPs.R')) # gets probability of each of 4 outcomes in each of the 16 worlds, ie 64 combos
# But we dont use it any more, because I could only get it separately, and I'm trying now to do it altogether

# It's likely the structure of all these will change a bit . pchoice is too simplistic and doesnt do likelihood
#
# This doesn't do likelihood either
# Get posteriors half way down, but not owrking yet - # BUT NOT WORKING - GIVING THE POSTERIOR = PRIOR FOR EACH SET OF UNOBS VARS (ie the combos all show up equally often always come to same total)
source(here('Exp2Explanation', 'Model', 'Scripts', '02getParams.R')) # get p(var=0), p(var=1) for each variable in a good format and also save adjacency matrices

# From here on not used yet - or old - all the stuff so far is in 02getParams
source(here('Exp2Explanation', 'Model', 'Scripts', '03getPosts.R')) # get posterior of each combination of unobserved variables for each world

source(here('Exp2Explanation', 'Model', 'Scripts', '04getPreds.R')) # get CESM model predictions using the cesmUtils functions

# Notes and TO DO
# - currently the dags in exp1 model graphs are still separate - make a combined one. Here they are combined in 03getParams but maybe could have been combined earlier?
# - expand out all combinations of vars
# - combine posteriors
# - the posteriors are not the probs needed to actually run the cesm. The posterior is needed instead later when the cesm is summed for each variable or variable combo
