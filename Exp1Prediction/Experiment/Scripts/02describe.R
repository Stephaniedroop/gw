###############################################################################################
############ Some initial plotting of the distribuition of Likert scale ratings #############
###############################################################################################

library(tidyverse)
library(ggplot2)
library(ggbeeswarm)

rm(list=ls())

# Load df 1421 of 27. Each row is one participant's response to one of the 16 situations. Was 1440 but 19 had 0s so were removed.
load('../Data/gwExp1data.Rda') 

# For any plotting we need a long version of participants' actual ratings on a 1:7 Likert scale - 5684
forscatter_long <- df |>
  select(mindsCode, lik_short_pizza:lik_long_hotdog, Situation) |>
  pivot_longer(
    cols = lik_short_pizza:lik_long_hotdog,
    names_to = "Choice",
    values_to = "Rating"
  )


# Instead of faceting, which are further down the page, make a separate ggplot for each situation = extra clear

situations <- unique(forscatter_long$Situation)

# Plotting function
plot_situation <- function(sit) {
  ggplot(forscatter_long |> filter(Situation == sit), aes(x = Choice, y = Rating, color = Choice)) +
    geom_jitter(width = 0.3, height = 0.1, size = 0.5, alpha = 0.6) +
    stat_summary(fun = mean, geom = "point", size = 3, shape = 16, color = 'black') +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = 'black') +
    stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -1.5, hjust = -0.5, color = 'black') +
    labs(x = "Item", y = "Rating", title = sit) +
    theme_minimal() +
    scale_x_discrete(guide = guide_axis(angle = 45))
}
# Generate plots for each situation
plots <- lapply(situations, plot_situation)

# A loop to save each plot 
for(i in 1:length(plots)) {
  ggsave(paste0("../Figures/likert", gsub(" ", "_", situations[i]), ".pdf"), plots[[i]], width=6, height=4.5)
  #print(plots[[i]])
}

# Another option is to do the faceting here, to get 2x8 grid of plots. 
# (Since decided best not to use this, so comment out)
# Split situations into two halves
first_half <- situations[1:8]
second_half <- situations[9:16]

# Create first facet grid (2x4) but text is wayy too small
first_grid <- forscatter_long |> 
  filter(Situation %in% first_half) |>
  ggplot(aes(x = Choice, y = Rating, color = Choice)) +
  geom_jitter(width = 0.3, height = 0.1, size = 0.5, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 16, color = 'black') +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = 'black') +
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 2)), vjust = -1.5, hjust = -0.5, color = 'black') +
  facet_wrap(~Situation, ncol = 2, nrow = 4) +
  labs(x = "Item", y = "Rating") +
  theme_minimal() +
  scale_x_discrete(guide = guide_axis(angle = 45))

# They are not the same - this one trying to get bigger text - if decide to use, need to apply this to the first one. 
second_grid <- forscatter_long |>
  filter(Situation %in% second_half) |>
  ggplot(aes(x = Choice, y = Rating, color = Choice)) +
  geom_jitter(width = 0.3, height = 0.1, size = 0.5, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 16, color = 'black') +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = 'black') +
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 2)), vjust = -1.5, hjust = -0.5, color = 'black') +
  facet_wrap(~Situation, ncol = 2, nrow = 4) +
  labs(x = "Item", y = "Rating") +
  theme_minimal() +
  theme(panel.spacing = unit(1.5, "lines"),
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)) +
  scale_x_discrete(guide = guide_axis(angle = 45))

# Save the faceted plots
ggsave("../Figures/likertFirst.pdf", first_grid, width = 12, height = 16) # Too small
ggsave("../Figures/likertSecond.pdf", second_grid, width = 15, height = 20) # Better?




# -------------------------------------

# I reckon a faceted one is less good because you cant see the individual points. 
# Unlikely to have space for these but maybe in an appendix

# A coloured beeswarm plot showing distribution of ratings, faceted by the 16 situations
ggplot(forscatter_long, aes(x = Choice, y = Rating, color = Choice)) +
  geom_beeswarm(cex = 0.3) +
  facet_wrap(~Situation) +
  labs(x = "Item", y = "Rating") +
  theme_minimal() +
  scale_x_discrete(guide = guide_axis(angle = 45))

# Find how many people gave each rating in each situation
# Popular bins get 30-60, while unpopular get 1-2. So a box or violin is not helpful
counts <- df |>
  select(mindsCode, lik_short_pizza:lik_long_hotdog, Situation) |>
  pivot_longer(
    cols = lik_short_pizza:lik_long_hotdog,
    names_to = "item",
    values_to = "rating"
  ) |>
  group_by(Situation, item, rating) |>
  summarise(count = n(), .groups = "drop")

# Show counts as stacked bars showing proportion - a bit more indicative than the boxplot
ggplot(counts, aes(x = item, y = count, fill = as.factor(rating))) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Situation) +
  labs(x = "Item", y = "Count", fill = "Rating") +
  theme_minimal()

# A violin plot with overlaid points, faceted by situation, still not brilliant
ggplot(forscatter_long, aes(x = Choice, y = Rating)) +
  geom_point() +
  geom_violin() +
  #geom_jitter(width = 0.2, height = 0, size = 1.5, alpha = 0.7) +
  facet_wrap(~Situation) +
  theme_bw() +
  labs(title = "Jittered Scatterplot Faceted by Situation",
       x = "X Axis Label",
       y = "Y Axis Label")
       )