#######################################################################
##################### HOW GOOD IS STEPWISE MODEL? #####################

# Script builds plot to compare output of the two independent regressions (path) and (food)
# generated in `stepwise.R` with actual participant ratings of how likely each choice/outcome was.
# This will tell us how badly we need to make a new multinomial regression allowing interactions 
# between path and food choice.

# Takes as input the ppt data wrangled in `pizzaland_parameters.R`, and the probabilities of the 
# stepwise model which were processed in `worldsetup.R`


# FIRST THING TO DO - RERUN ALL THIS, THE OTHER FILES NOW RECODED TO F4 1==HOTDOG, AND PROB LONG. REMAKE FIGS



rm(list = ls())

library(tidyverse)


# Stepwise model outputs processed and calculated for the 64 worlds
load('worlds.rdata', verbose = T) # pChoice

# Exp1 ppt crowdsourced probabilities
load('exp1processed_wide.rdata', verbose = T) # loads pizpar2, 1440 obs of 13 vars

# For a 4-way boxplot
load('exp1processed_long.rdata', verbose = T) # loads pizpar3_long, 5760 obs of 8 vars






# Check they sum to 1
# pChoice$check <- rowSums(pChoice[, psh:plp])

# Summarise relevant bits of pChoice just for this
df1 <- pChoice %>% select(situTag, psh:plp)

# Make it longer and give same names as other data to allow putting on same plot
df1 <- df1 %>% pivot_longer(
  cols = psh:plp,
  names_to = "outcome",
  values_to = "probability"
)

# Make it a factor
df1$outcome <- factor(df1$outcome, levels = c('psp', 'plp', 'psh', 'plh'), 
                               labels = c('shortPizza', 'longPizza', 'shortHotdog', 'longHotdog'))



# TO DO
# Make p_long the prediction because atm the 0,1 is wrong way round
# Order the p boxplots the same like 00, 01, 10, 11
# Compare comp (2-way chart) where if both are high, then it implies the 1,1 is the highest, the 0,0 is the lowest, and the others are in between




# Now from pizpar we don't need the 4-way, only the 2-way. Note the probs won't sum to 1 now
df3 <- pizpar2 %>% select(-(prob_short_hotdog:prob_long_pizza))
# Then pivot longer
df3 <- pizpar2 %>% pivot_longer(
  cols = p_long:prob_hotdog,
  names_to = "outcome",
  values_to = "probability"
)

# df3 is ppt data for boxplots, facets are the 16 situs
p1 <- ggplot() +
  geom_boxplot(data = df3, aes(x=situTag, y=probability, fill=outcome)) +
  facet_wrap(~situTag, scale = "free")

p1

# Then overlay horizontal lines for the model predictions
p2 <- p1 +
  geom_hline(data = df1, aes(yintercept = probability, colour=outcome))

p2
ggsave('~/Documents/GitHub/gw/comp.pdf', width = 7, height = 5, units = 'in')

# Keep this for 4-way boxplots overlaid with hlines for model preds
pKEEP <- ggplot() +
  geom_boxplot(data = pizpar3_long, aes(y=probability, fill=outcome)) +
  facet_wrap(~situTag, scale = "free")

pALSOKEEP <- pKEEP +
  geom_hline(data = df1, aes(yintercept = probability, colour=outcome))

pALSOKEEP


# GAHHHH not working

# Neil's suggestion to put point overlay but need to sort the order first
p4 <- p3 +
  geom_point(data = df1, aes(x=outcome, yintercept = probability, colour=outcome), fill = 'black', shape=2)


# ggsave('~/Documents/GitHub/gw/comp3.pdf', width = 7, height = 5, units = 'in')

