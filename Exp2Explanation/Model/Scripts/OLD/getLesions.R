# ==============================================================================
## Process cesm data and do posterior marg/inference step, make it long to later optimize tau1
# ==============================================================================

library(here)
library(tidyverse)

# Actually we don't need this script!!!???

# source(here('Exp2Explanation', 'Model', 'Scripts', 'get_lesions.R'))
# load
#
# TAU1 <- 1 # This was decided in a separate script, 14getTau1.R
#
# path_ces <- get_lesions(pathlong, TAU1)
# food_ces <- get_lesions(foodlong, TAU1)
#
# save(
#   path_ces,
#   food_ces,
#   file = here('Exp2Explanation', 'Model', 'Data', 'ces_sepSimpleN.rda')
# )

# New section, testing it out, teking a new bit to a separate call script, and 6,7 become fucntions it calls
