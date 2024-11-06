#################################################### 
###### Collider analysis - compare model preds #####
####################################################

# Script takes the processed data from the collider ppt expt (`DATA.RDATA`)
# (which was cleaned and sliced in `mainbatch_preprocessing.R`) 
# and combines it with the preprocessed model predictions from `modpred_preprocessing.R` (`tidied_preds.csv`)


library(tidyverse)
library(ggnewscale) 
#library(shiny)
rm(list=ls())


s_vals <- seq(0.00, 1.00, 0.05)

# Generate series of RMarkdown reports with plots to assess model fit, for each value of s separately
lapply(unique(s_vals), function(stab.i) {
  rmarkdown::render(input = 'combine_per_s.Rmd',
                    params = list(stab = stab.i),
                    output_file = paste0(stab.i, '.html'))

})

# Questions for 
