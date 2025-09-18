###############################################################################################
############ Some initial plotting (non-dag) of the best-fitting causal model #############
###############################################################################################

library(tidyverse)
library(ggplot2)
library(ggbeeswarm)

rm(list=ls())

# Load df 1421 of 27. Each row is one participant's response to one of the 16 situations. Was 1440 but 19 had 0s so were removed.
load('../Data/gwExp1data.Rda') 

# To make a scatterplot of participants' actual ratings on a 1:7 Likert scale
forscatter_long <- df |>
  select(mindsCode, lik_short_pizza:lik_long_hotdog, Situation) |>
  pivot_longer(
    cols = lik_short_pizza:lik_long_hotdog,
    names_to = "Choice",
    values_to = "Rating"
  )

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