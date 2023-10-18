#######################################################################
###### HOTDOG PIZZALAND EXP1 LATER MODEL SELECTION (NOT STEPWISE) #####

# Script from OCT 2023, takes Exp1 data generated from script 'pizzaland_parameters.R'
# Like 'stepwise.R', uses a selection package from somebody else to get (static) significant beta slopes
# of what variables influence choice and path in the situation model.
# But, UNLIKE 'stepwise.R', we want to allow interaction between path and food choice, not two independent.

rm(list = ls())

library(glmulti)
library(tidyverse)

load('exp1processed_long.rdata', verbose = T) # 5760 obs of 8 vars

# Example syntax from https://yuzar-blog.netlify.app/posts/2022-05-31-glmulti/
# test_g <- glmulti(mpg ~ hp + drat + wt + qsec + gear, 
#                   data   = mtcars, 
#                   method = "h",       # genetic algorithm approach
#                   crit   = aic,      # AICC corrected AIC for small samples
#                   level  = 2,         # 2 with interactions, 1 without
#                   family = gaussian,
#                   fitfunction = glm,  # Type of model (LM, GLM, GLMER etc.)
#                   confsetsize = 100)  # Keep 100 best models
# 
# test_g2 <- glmulti(mpg ~ hp + drat + wt + qsec + gear, 
#                   data   = mtcars, 
                  # method = "g",       # genetic algorithm approach
                  # crit   = aic,      # AICC corrected AIC for small samples
                  # level  = 2,         # 2 with interactions, 1 without
                  # family = gaussian,
                  # fitfunction = glm,  # Type of model (LM, GLM, GLMER etc.)
                  # confsetsize = 100)  # Keep 100 best models


# Wrapper function to allow fitting mixed effects models from glmer inside glmulti
# lifted from https://yuzar-blog.netlify.app/posts/2022-05-31-glmulti/ probably not to modify
glmer.glmulti <- function(formula, data, random = "", ...){
  glmer(paste(deparse(formula),random),
        data    = data, ...)
}

mixed_model <- glmulti(
  y = outcome ~ Preference + Knowledge + Character + Start,
  random  = c("+(1|mindsCode)",("+1|situTag")),
  crit    = aicc,
  data    = pizpar3_long,
  family  = binomial,
  method  = "g",
  fitfunc = glmer.glmulti,
  marginality = F,
  level   = 2 )
