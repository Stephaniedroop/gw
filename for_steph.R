library(tidyverse)
library(ggplot2)
library(rje)

rm(list=ls())

load('example_data.rdata', verbose = T)

#my_cols<-cubehelix(4)#Create colour palette

#Plot accuracy of generalizations developmental sample

p1 <- ggplot(ch.r, aes(x=ruleix.rev, y=score,
                 fill=factor(experiment, levels = rev(levels(experiment)), labels = c('Adults','Children')) ) ) +
  stat_summary(geom='bar', fun='mean', colour = 'black', position = position_dodge(.9)) + #Plots behavioural data mean as bars (here we have 5 categories of rule as x aesthetic and also 2 bars per rule (fill aesthetic))
  stat_summary(fun.data = 'mean_cl_boot', geom='errorbar', width = .3, position = position_dodge(.9)) + #Plots errorbars for the bars above
  stat_summary(data = chad.pcfg_pred.ho, aes(x=as.numeric(ruleix.rev)-4.9, y=ho.best_guess),  fun='mean', colour = '#0095f0', position = position_dodge(.9), size = 0.2) + #Plots a model on top in blue, shifted a little so it is not on top of the black error bars
  stat_summary(data = chad.pcfg_pred.ho, aes(x=as.numeric(ruleix.rev)-4.9, y=ho.best_guess), 
               fun.data = 'mean_cl_boot', geom='errorbar', width = .3,  colour = '#0095f0', position = position_dodge(.9)) + #Error bars for that model
  stat_summary(data = chad.dd_pred.ho, aes(x=as.numeric(ruleix.rev)-5.1, y=ho.best_guess),  fun='mean', colour = '#8d1b1b', position = position_dodge(.9), size = 0.2) + #Similarly plots a model on top in red
  stat_summary(data = chad.dd_pred.ho, aes(x=as.numeric(ruleix.rev)-5.1, y=ho.best_guess),
               fun.data = 'mean_cl_boot', geom='errorbar', width = .3,  colour = '#8d1b1b', position = position_dodge(.9)) + #Errorbars for that model
  geom_hline(yintercept=4, colour = 'black', size=1) + #Plots a line at chance
  geom_point(aes(x=jitter(5.75-ruleid + 0.5*as.numeric(experiment=='child_sample'))), alpha = .1, size = 2) + #Plots the jittered points at top (this needed some hacking to spread correctly across the two fill bars)
  coord_cartesian(ylim=c(0,8)) + #This just defines the boundaries of the plot (so it runs up to 8 exactly)
  coord_flip() + #This flips everything on the x/y axes to make the plot horizontal (you don't need this, its just so the labels are readable)
  labs(y='Generalization accuracy', x='Rule', fill='Agegroup') + #Set readable labels
  #scale_fill_manual(values = my_cols[2:3]) + #Set the fill colours I wanted for consistency across the paper
  theme_bw() + #Get rid of ugly gray background which is only good for screens and terrible for print
  theme(panel.grid = element_blank()#,
        # axis.text.x = element_text(angle = 90, hjust = 1),
        # legend.position = 'none'
  ) #Get rid of grid lines, other commands commented out for this plot, the first would have rotated the axis labels and teh second omits the legend.

p1