##############################################################
#### Master script for collider within gridworld project #####
##############################################################
# Create model predictions 
# 


library(tidyverse)
library(rjson)
rm(list=ls())

# This part may be cropped out elsewhere depending how we end up using the data from the js experiment
worlds <- fromJSON(file = 'worlds.json')
worldsdf <- as.data.frame(worlds) # 8 obs of 132 vars
conds <- fromJSON(file = 'conds.json')
condsdf <- as.data.frame(conds) # 2 obs of 21 vars - remains to see how to get what we need out of this



# Loop over the values in a vector to run the different probs and tidy it to call 1,2,11 as just 1,2,3
# set values then

# Make static source of all params eg 3x8 but others are possible


params1<-data.frame("0"=c(0.9,0,0.1,0), "1"=c(0.1,1,0.9,1))
params2<-data.frame("0"=c(0.9,0,0.1,0), "1"=c(0.1,1,0.9,1))
params3<-data.frame("0"=c(0.9,0,0.1,0), "1"=c(0.1,1,0.9,1))
row.names(params1)<-row.names(params2)<-row.names(params3)<-c("pA",  "peA", "pB", "peB")
names(params1)<-names(params2)<-names(params3)<-c('0','1')
poss_params<-list(params1, params2, params3)

for (i in nrow(poss_params)) {
  source('general_cesm_a.R') # relative file of file 1 inside the loop
  # But - it is currently a function so maybe can call that instead of running whole source file
}

# collect all dfs into list and save it once

# x=1
# y=2
# my_df<-data.frame(x=rep(NA, 10))

mod_preds <- list()
mod_preds[[i]]$conjunctive <- my_df1
mod_preds[[i]][[2]] <- my_df2