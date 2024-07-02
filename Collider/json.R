##############################################################
#### Preprocessing and data wranging for json, collider  #####
##############################################################


library(rjson)


# This part may be cropped out elsewhere depending how we end up using the data from the js experiment
worlds <- fromJSON(file = 'worlds.json')
worldsdf <- as.data.frame(worlds) # 8 obs of 132 vars
conds <- fromJSON(file = 'conds.json')
condsdf <- as.data.frame(conds) # 2 obs of 21 vars - remains to see how to get what we need out of this




load('model_data/modpreds.Rdata', verbose = T) 
