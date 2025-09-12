#######################################################################
############## HOTDOG PIZZALAND STEPWISE MODEL SELECTION ##############

# Script from early 2023, takes Exp1 data generated from script 'pizzaland_parameters.R'
# and uses a stepwise selection package from somebody else to get (static) significant beta slopes
# of what variables influence choice of hotdog and short path separately in the situation model.

rm(list = ls())

library(remotes)
# remotes::install_github("timnewbold/StatisticalModels")
library(StatisticalModels)

load('exp1processed.rdata', verbose = T) # 1440 obs of 20 vars

# Model for PROB_SHORT using his syntax

m_new_short <- GLMERSelect(pizpar2,"prob_short","binomial",fixedFactors=c("Preference","Character","Knowledge","Start")
            ,randomStruct="(1|mindsCode)",
                     fitInteractions=TRUE,
                     verbose=TRUE,saveVars=character(0),
                     optimizer="bobyqa",maxIters=10000)

summary(m_new_short$model) 

# Now run model for PROB_HOTDOG using his syntax
m_new_hd <- GLMERSelect(pizpar2,"prob_hotdog","binomial",fixedFactors=c("Preference","Character","Knowledge","Start")
                           ,randomStruct="(1|mindsCode)",
                           fitInteractions=TRUE,
                           verbose=TRUE,saveVars=character(0),
                           optimizer="bobyqa",maxIters=10000)


summary(m_new_hd$model)

# This gives regression betas which are taken into script 'worldsetup.R' to give prob of each action
