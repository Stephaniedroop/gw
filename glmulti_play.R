library(glmulti)


test_g <- glmulti(mpg ~ hp + drat + wt + qsec + gear, 
                  data   = mtcars, 
                  method = "g",       # genetic algorithm approach
                  crit   = aic,      # AICC corrected AIC for small samples
                  level  = 2,         # 2 with interactions, 1 without
                  family = gaussian,
                  fitfunction = glm,  # Type of model (LM, GLM, GLMER etc.)
                  confsetsize = 100)  # Keep 100 best models