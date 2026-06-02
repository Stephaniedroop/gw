# -----------------------------------------------
# ----- Exp2b Annotations descriptives   -------------
# -----------------------------------------------

# Let's look at the annotated free text explanations before we even try to model them

library(here)
library(tidyverse)
library(ggplot2)
library(stringr)


load(here('Exp2Explanation', 'Annotation', 'Data', 'counts.rda'))

# Need some ovverall numbers of annotations per category, from df_fixed
