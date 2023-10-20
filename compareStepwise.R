#######################################################################
##################### HOW GOOD IS STEPWISE MODEL? #####################

# Script builds plot to compare output of the two independent regressions (path) and (food)
# generated in `stepwise.R` with actual participant ratings of how likely each choice/outcome was.
# This will tell us how badly we need to make a new multinomial regression allowing interactions 
# between path and food choice.

# Takes as input the ppt data wrangled in `pizzaland_parameters.R`, and the probabilities of the 
# stepwise model which were processed in `worldsetup.R`


rm(list = ls())

library(tidyverse)


# Stepwise model outputs processed and calculated for the 64 worlds
load('worlds.rdata', verbose = T) 

# Exp1 ppt crowdsourced probabilities
load('exp1processed_wide.rdata', verbose = T) # loads pizpar2, 1440 obs of 12 vars



# Take the first 4 digits of the 64-world ID to make the 16-situation ID
pChoice$situTag <- str_sub(pChoice$numtag, 1, -3)

# Summarise relevant bits of pChoice just for this
df1 <- pChoice %>% select(situTag, p_short, p_hotdog)

# Make it longer and give same names as other data to allow putting on same plot
df1 <- df1 %>% pivot_longer(
  cols = p_short:p_hotdog,
  names_to = "outcome",
  values_to = "probability"
)

# Now from pizpar we don't need the 4-way, only the 2-way. Note the probs won't sum to 1 now
df3 <- pizpar2 %>% select(-(prob_short_hotdog:prob_long_pizza))
# Then pivot longer
df3 <- df3 %>% pivot_longer(
  cols = prob_short:prob_hotdog,
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

