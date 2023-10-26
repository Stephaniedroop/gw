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

# TO DO - add models preds, get axis labels vertical
#axis.title.x = element_blank(),
#axis.text.x = element_blank(),
#axis.ticks.x = element_blank()


# THIS IS THE OLD WAY - BOXPLOTS WITH HLINE OVERLAID FOR MODEL PRED
# p1 <- ggplot() +
#   geom_boxplot(data = df2, aes(x=situTag, y=probability, fill=outcome)) +
#   facet_wrap(~situTag, scale = "free")
# 
# p1
# 
# # Then overlay horizontal lines for the model predictions
# p2 <- p1 +
#   geom_hline(data = df1, aes(yintercept = probability, colour=outcome))
# 
# p2
# ggsave('~/Documents/GitHub/gw/comp.pdf', width = 7, height = 5, units = 'in')

# df2 is ppts; df1 is model preds

# p3 <- ggplot() +
#   geom_point 
# 
# 
# p4 <- p3 +
#   geom_point(data = df1, aes(x=outcome, yintercept = probability, colour=outcome), fill = 'black', shape=2)


# ggsave('~/Documents/GitHub/gw/comp3.pdf', width = 7, height = 5, units = 'in')

