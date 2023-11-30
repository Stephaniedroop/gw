######################################################################
################## IRR etc on V and I's coded pilot gw data ##########


# New notes and advice
# Make an empty matrix of 9x9 and loop through each line to add it to the cells. About half should be 0
# The proportion of the total that lies on the diagonal shows how much they agreed but there should be better measures too


# Prelims
library(tidyverse)
library(rje)

#---------- Read in data and get it to standard numerical form ------------------
idat <- read.csv('coded_i.csv', na.strings=c(""," ","NA"))
vdat <-  read.csv('coded_v.csv', na.strings=c(""," ","NA"))

# Replace NAs with 0s
idat[is.na(idat)] <- 0
vdat[is.na(vdat)] <- 0
# Remove notes
idat <- idat %>% select(-X)

# Shortened category names that were coded for (eg prefgen = Preference general). These are the colnames of which to make power set
cats <- c('prefgen', 'prefspec', 'chargen', 'charspec', 'know', 'loc', 'disp', 'sit')

colnames(idat) <- c('reponse', cats)
colnames(vdat) <- c('response', cats)

# Change cols to numeric on idat
i <- c(2:9)
idat[ , i] <- apply(idat[ , i], 2,
                    function(x) as.numeric(as.character(x)))


#---------- Set up 2^8 square matrix for putting the counts in -------------------

# Set up power set (2^8) of cats
ps <- powerSet(cats, m = 8)

# Set up empty df with both cols and rows the size of the power set and named so
df <- data.frame(matrix(nrow = length(ps), ncol = length(ps)))
colnames(df) <- ps
rownames(df) <- ps


#---------- Loop through V and I data and compare, putting a count in correct cell of df ------------
n_exps <- nrow(idat) # We checked idat and vdat are the same length and order elsewhere so ok to assume it here
# THIS BIT IS WIP
for (exp in 1:n_exps)
{
  # Pull out v and i 's whole rows of their separate ratings for the same explanation
  vexp <- vdat[exp,]
  iexp <- idat[exp,]
  # Add a 1 to cell of df corresponding to row of v and col of i
  # But first need a way to express vexp's series of 1s in same form as an element of power set
  vrat <- vexp[2:9]
  irat <- iexp[2:9]
  # need a vector of names of which cols rated 1, to later match with correct place in power set df
  pos <- which(vrat&irat) # This gives positions where v and i gave the same rating
  name <- cats[pos] # HOW TO BUNDLE THIS INTO A C('') WITH ALL ELEMENTS? so then to index the df by it
  
  df
}




# Now the same for factor
# j <- c(2:9)
# idat[ , j] <- apply(idat[ , j], 2,
#                     function(x) as.factor(as.character(x)))
# 
# vdat[ , j] <- apply(vdat[ , j], 2,
#                     function(x) as.factor(as.character(x)))


# A version where any other digit is renamed to 1
idat2 <- idat
idat2[idat==2] <- 1
idat2[idat==3] <- 1

vdat2 <- vdat
vdat2[vdat==2] <- 1
vdat2[vdat==3] <- 1
vdat2[vdat==4] <- 1




idat2 <- idat2 %>% 
  mutate(across(where(is.character), as.factor))

vdat2 <- vdat2 %>% 
  mutate(across(where(is.character), as.factor))

