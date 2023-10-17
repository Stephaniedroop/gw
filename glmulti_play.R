#######################################################################
###### HOTDOG PIZZALAND EXP1 LATER MODEL SELECTION (NOT STEPWISE) #####

# Script from OCT 2023, takes Exp1 data generated from script 'pizzaland_parameters.R'
# Like 'stepwise.R', uses a selection package from somebody else to get (static) significant beta slopes
# of what variables influence choice and path in the situation model.
# But, UNLIKE 'stepwise.R', we want to allow interaction between path and food choice, not two independent.

rm(list = ls())

library(glmulti)
library(tidyverse)

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

# TO DO 18 Oct- load newly-longly-saved data and try these models on it:

# Once the data is ready, rewrite this to put in our y and predictors
glmer.glmulti<-function(formula, data, random = "", ...){
  glmer(paste(deparse(formula),random),
        data    = data, REML = F, ...)
}

mixed_model <- glmulti(
  y = response ~ predictor_1 + predictor_2 + predictor_3,
  random  = "+(1|random_effect)",
  crit    = aicc,
  data    = data,
  family  = binomial,
  method  = "h",
  fitfunc = glmer.glmulti,
  marginality = F,
  level   = 2 )
