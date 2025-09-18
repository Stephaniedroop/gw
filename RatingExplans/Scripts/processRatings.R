#####################################################
###### Processing of rater data #####################

# This script standardises rater data, names it shortened categories and replaces all numbers with 1s

rm(list = ls())
library(tidyverse) # for select - if packages become a hassle let's do it another way


#---------- Read in data and get it to standard numerical form ------------------
idat <- read.csv('../Data/coder1.csv', na.strings=c(""," ","NA")) 
vdat <-  read.csv('../Data/coder2.csv', na.strings=c(""," ","NA")) # REPLACE CSV WITH WHAT YOU NEED

# vdat cut off last two columns; idat merge last two to be same as v's third last one 'Unclear'
# Q: the merging script puts Unclear any where the raters disagree. 
# For that then think we ignore any called Unclear to begin with? To decide

# Replace NAs with 0s
idat[is.na(idat)] <- 0
vdat[is.na(vdat)] <- 0
# Remove notes, Random, Unclear etc
idat <- idat |> 
  select(-c(10))
vdat <- vdat |> 
  select(-c(10,11,12))

# Remove after row 2064; it is not data
idat <- idat[-2065,]
vdat <- vdat[-c(2065:2071),]


# Shortened category names that were coded for (eg prefgen = Preference general). 
# c('prefgen', 'prefspec', 'chargen', 'charspec', 'know', 'loc', 'disp', 'sit')
# Use letters to make less unwieldy but match same order as the category names
cats <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')

colnames(idat) <- c('response', cats)
colnames(vdat) <- c('response', cats)

# Change cols to numeric on idat because some had question marks and were stored as chars
a <- c(2:9)
idat[ , a] <- apply(idat[ , a], 2,
                    function(x) as.numeric(as.character(x)))

# A version where any other digit >1 (ie which denotes subsidiary reasons) is renamed to 1
idat2 <- idat
idat2[idat==2] <- 1
idat2[idat==3] <- 1

vdat2 <- vdat
vdat2[vdat==2] <- 1
vdat2[vdat==3] <- 1
vdat2[vdat==4] <- 1

save(idat, idat2, vdat, vdat2, cats, file="../Data/ratings.Rda")

# Then go to mergeRatings.R to merge them