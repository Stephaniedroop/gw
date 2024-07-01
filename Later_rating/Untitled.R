# ug data rerun
library(tidyverse)


df <- read.csv('rerundata.csv')

df <- df %>% filter(trialText!="")
df <- df %>% select(response)

write.csv(df, 'to_code.csv')
