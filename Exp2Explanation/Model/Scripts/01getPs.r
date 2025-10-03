# ==============================================================================
# Obtaining pOutcomes from causal model selection from Exp1
# ==============================================================================

library(here)


load(here('Exp1Prediction', 'Model', 'Data', 'model.rda'))

# df.m is what we need, and it has the marginal prob of pathLong and pFoodHotdog for each of the 16 situations
# Expand df.m
# We need p for each of the 4 choices in each situation:
# ShortPizza = (1 - pLong) * (1 - pHotdog)
# LongPizza = pLong * (1 - pHotdog)
# ShortHotdog = (1 - pLong) * pHotdog
# LongHotdog = pLong * pHotdog
# So we need pLong and pHotdog for each situation
# These are in df.m as mp_path and mp_food
# pShort = 1 - mp_path
# pHotdog = mp_food
# pPizza = 1 - mp_food
# So:
# pShortPizza = (1 - mp_path) * (1 - mp_food)
# pLongPizza = mp_path * (1 - mp_food)
# pShortHotdog = (1 - mp_path) * mp_food
# pLongHotdog = mp_path * mp_food
# We can calculate these directly, but we need to expand df.m to have 4 rows
# for each situation, one for each choice
# So we need to repeat each row of df.m 4 times, and add columns for
# Food (Pizza/Hotdog) and Path (Short/Long)
# Then we can calculate p_action for each row
# p_action = pPath * pFood
# where pPath is pShort or pLong depending on Path
# and pFood is pPizza or pHotdog depending on Food

# Expand df.m to have 4 rows for each situation
# Each row of df.m has mp_path and mp_food


# pShort = 1 - mp_path
# pHotdog = mp_food
# pPizza = 1 - mp_food
# So:
# pShortPizza = (1 - mp_path) * (1 - mp_food)
# pLongPizza = mp_path * (1 - mp_food)
# pShortHotdog = (1 - mp_path) * mp_food
# pLongHotdog = mp_path * mp_food
# So we need to calculate these 4 probabilities for each situation
# We can do this by expanding df.m to have 4 rows for each situation
# and adding columns for Food and Path
# Then we can calculate p_action for each row
# p_action = pPath * pFood
# where pPath is pShort or pLong depending on Path
# and pFood is pPizza or pHotdog depending on Food
# So we need to calculate pPath and pFood for each row
# pPath is either pShort or pLong
# pFood is either pPizza or pHotdog
# pShort = 1 - mp_path



probs <- df.m |> 
  mutate(p_short = 1 - mp_path,
         p_long = mp_path,
         p_hotdog = mp_food,
         p_pizza = 1 - mp_food) |> 
  select(situation, p_short, p_long, p_hotdog, p_pizza)


# We need to combine these to get p for each of the 4 choices in each situation
# Make a df of all 16 situations
pChoice <- data.frame(situationVerbose = rep(df.m$situationVerbose, each=4),
                      situation = rep(df.m$situation, each=4),
                      Food = rep(c("Pizza", "Pizza", "Hotdog", "Hotdog"), times=nrow(df.m)),
                      Path = rep(c("Short", "Long", "Short", "Long"), times=nrow(df.m))
)
# So now pChoice has 16x4 = 64 rows, one for each situation

pChoice <- pChoice |>
  mutate(fullSituation = paste0(situation, 
                                ifelse(Food == "Pizza", "0", "1"),
                                ifelse(Path == "Short", "0", "1")))


# Merge probs to pChoice
pChoice <- merge(pChoice, probs, by="situation")
# Now pChoice has p_short, p_long, p_hotdog, p_pizza for each row
# Now calculate p_action for each row
pChoice <- pChoice |>
  mutate(p_action = ifelse(Food == "Pizza" & Path == "Short", p_short * p_pizza,
                           ifelse(Food == "Pizza" & Path == "Long", p_long * p_pizza,
                                  ifelse(Food == "Hotdog" & Path == "Short", p_short * p_hotdog,
                                         p_long * p_hotdog))))

# check they sum to 1
pChoice |> 
  group_by(situation) |> 
  summarise(sum=sum(p_action))

# Btw, these probabilities are all split across the four options, ie. they may look low (the highest .4-something)
# The key is how different from .25 they are, ie. how much they deviate from chance

# save Food, Path and Situation as factors
pChoice <- pChoice |>
  mutate(Food = factor(Food, levels=c("Pizza", "Hotdog")),
         Path = factor(Path, levels=c("Short", "Long")),
         fullSituation = factor(fullSituation, levels=unique(fullSituation)))


# save pChoice
save(pChoice, file=here('Exp2Explanation', 'Model', 'Data', 'pChoice.rda'))
