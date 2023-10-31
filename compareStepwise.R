#######################################################################
##################### HOW GOOD IS STEPWISE MODEL? #####################

# Script builds plot to compare output of the two independent regressions (path) and (food)
# generated in `stepwise.R` with actual participant ratings of how likely each choice/outcome was.
# This will tell us how badly we need to make a new multinomial regression allowing interactions 
# between path and food choice.

# Takes as input the ppt data wrangled in `pizzaland_parameters.R`, and the probabilities of the 
# stepwiseLong model which were processed in `worldsetup_mod.R` and `stepwiseLong.R`


rm(list = ls())

library(tidyverse)

# Stepwise model outputs processed and calculated for the 64 worlds
load('worlds.rdata', verbose = T) # pChoice 64 obs of 15 vars

# Exp1 ppt crowdsourced probabilities
load('exp1processed_wide.rdata', verbose = T) # loads pizpar3, 1440 obs of 13 vars

# ------------------ REWORK PCHOICE ---------------------------
# Summarise relevant bits of pChoice, the regression model preds, just for this 
df1 <- pChoice %>% select(situTag, longHotdog:shortPizza)

# Make it longer and give same names as other data to allow putting on same plot
df1 <- df1 %>% pivot_longer(
  cols = longHotdog:shortPizza,
  names_to = "outcome",
  values_to = "probability"
)

# Make it a factor
df1$outcome <- factor(df1$outcome, levels = c('shortPizza', 'longPizza', 'shortHotdog', 'longHotdog'), 
                               labels = c('shortPizza', 'longPizza', 'shortHotdog', 'longHotdog'))

df1$situTag <- factor(df1$situTag)


# ------------------- REWORK PIZPAR ----------------------------
# Summarise the ppt intuitions of causal strengths from Exp1, just for this
df2 <- pizpar3 %>% pivot_longer(
  cols = prob_short_hotdog:prob_long_pizza,
  names_to = "outcome",
  values_to = "probability"
)

# Make it factors
df2$outcome <- factor(df2$outcome, levels = c('prob_short_pizza', 'prob_long_pizza',
                                                                'prob_short_hotdog', 'prob_long_hotdog'), 
                               labels = c('shortPizza', 'longPizza', 'shortHotdog', 'longHotdog'))


# ------------------ MAKE CHARTS ----------------------------------

p1 <- ggplot(df2, aes(x = outcome, y = probability,
                       fill = outcome)) +
  facet_wrap(~situTag, scale = "free") +
  stat_summary(geom='bar', fun='mean', colour = 'black', position = position_dodge(.9)) + #Plots behavioural data mean as bars (here we have 5 categories of rule as x aesthetic and also 2 bars per rule (fill aesthetic))
  stat_summary(fun.data = 'mean_cl_boot', geom='errorbar', width = .3, position = position_dodge(.9)) +
  stat_summary(data = df1, aes(x = outcome, y = probability),  fun='mean', colour = 'black', 
               position = position_dodge(.9), size = 0.2) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle=270, hjust=1)) +
  scale_x_discrete(labels = c('00', '01', '10', '11'))

p1

ggsave('~/Documents/GitHub/gw/comp.pdf', width = 7, height = 5, units = 'in')

# Then, to make predictions w/o model 
# New vars are just the complement of the existing
pizpar3$prob_short <- 1 - pizpar3$prob_long
pizpar3$prob_pizza <- 1 - pizpar3$prob_hotdog

# Now to remultiply these existing ones
pizpar3 <- pizpar3 %>% mutate(
  predShortPizza = prob_short*prob_pizza,
  predLongPizza = prob_long*prob_pizza,
  predShortHotdog = prob_short*prob_hotdog,
  predLongHotdog = prob_long*prob_hotdog)

# Column for check they sum to 1
pizpar3$check <- rowSums(pizpar3[ , c(15:18)])

# Now pull out the relevant bits and make it long
# df3 <- pizpar3 %>% select(situTag, predShortPizza:predLongHotdog)

df3 <- pizpar3 %>% pivot_longer(
  cols = predShortPizza:predLongHotdog,
  names_to = "outcome",
  values_to = "probability"
)

df3$outcome <- factor(df3$outcome, levels = c('predShortPizza', 'predLongPizza',
                                              'predShortHotdog', 'predLongHotdog'), 
                      labels = c('shortPizza', 'longPizza', 'shortHotdog', 'longHotdog'))


# Now redo diagram
p2 <- ggplot(df2, aes(x = outcome, y = probability,
                      fill = outcome)) +
  facet_wrap(~situTag, scale = "free") +
  stat_summary(geom='bar', fun='mean', colour = 'black', position = position_dodge(.9)) + #Plots behavioural data mean as bars (here we have 5 categories of rule as x aesthetic and also 2 bars per rule (fill aesthetic))
  stat_summary(fun.data = 'mean_cl_boot', geom='errorbar', width = .3, position = position_dodge(.9)) +
  stat_summary(data = df3, aes(x = outcome, y = probability),  fun='mean', colour = '#8d1b1b', 
               position = position_dodge(.5), size = 0.2) +
  stat_summary(data = df3, aes(x = outcome, y = probability),  fun.data='mean_cl_boot', geom='errorbar', width = .3, colour = '#8d1b1b', 
               position = position_dodge(.5), size = 0.2) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle=270, hjust=1)) +
  scale_x_discrete(labels = c('00', '01', '10', '11'))

p2

ggsave('~/Documents/GitHub/gw/comp2.pdf', width = 7, height = 5, units = 'in')
