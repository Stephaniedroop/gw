# ==============================================================================
## Master script to run others in the gw Exp2 Model folder - cesm etc
# ==============================================================================

library(here)
library(tidyverse)

set.seed(12)

# Load utils
source(here('Exp2Explanation', 'Model', 'Scripts', 'cesmUtilsSimple.R'))

# Running a new series of 'simple' causal model (w/o interactions). Rename scripts if it all works and I decide to go with this as main arc
# these then in turn redone 7 May to include ces preds for all br and all options given in the annotation schema

source(here('Exp2Explanation', 'Model', 'Scripts', '02getParamsSimple.R')) # input modelSimple.rda from Exp1; output paramsSimple.rda
source(here('Exp2Explanation', 'Model', 'Scripts', '03getProbsSimple.R'))
source(here('Exp2Explanation', 'Model', 'Scripts', '04getSemSimple.R'))

source(here('Exp2Explanation', 'Model', 'Scripts', '05getPredsSimple.R'))
source(here('Exp2Explanation', 'Model', 'Scripts', '06getLong.R')) # saves preds_long, used in

source(here('Exp2Explanation', 'Model', 'Scripts', 'getTau1.R')) # freestanding to get tau1 - calls tauUtils functions and
source(here('Exp2Explanation', 'Model', 'Scripts', 'optimise.R')) # calls optimUtils

# scritp 08 and 09 still have important things, TODO tidy
