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

# Exp1 ppt crowdsourced probabilities
load('exp1processed_long.rdata', verbose = T) # 5760 obs of 8 vars
# Stepwise model outputs processed and calculated for the 64 worlds
load('worlds.rdata', verbose = T) 



# Now need a df allowing a plot

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

# Merge the two dfs
df2 <- merge(pizpar3_long, df1, by="situTag")

# Set of boxplots
p1 <- ggplot() +
  geom_boxplot(data = pizpar3_long, aes(x=situTag, y=probability, fill=outcome)) +
  facet_wrap(~situTag, scale = "free")
  
p1


p2 <- p1 +
  geom_hline(data = df1, aes(yintercept = probability, colour=outcome))

p2
ggsave('~/Documents/GitHub/gw/comp.pdf', width = 7, height = 5, units = 'in')
