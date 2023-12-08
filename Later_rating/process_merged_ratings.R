###############################################################################
############## Run bigplot with new merged ratings ############################

load('to_go.rdata', verbose = T)

# Take a copy in case need to go back...
rat_summ <- ratings 

# Decisions to be made: is Preference just General prference or both General and Specific?

# Use vector here
# c('prefgen', 'prefspec', 'chargen', 'charspec', 'know', 'loc', 'disp', 'sit')
# cats <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
# Remember there is also now unc

# To get tags back in
tags <- read.csv('../Exp2Explanation/Data/tags.csv', stringsAsFactors = TRUE)
tags <- tags %>% unite("numtag", Z:U, sep= "", 
                       remove = TRUE)
tags <- tags %>% select(tag, numtag)


ugdata <- read.csv('../Exp2Explanation/Data/ugdata.csv')
ugdata <- ugdata %>% select(X, tag, response)

forchart <- merge(x = ugdata, y = tags, all.y = T)
forchart <- forchart %>% select(-tag) 
forchart <- forchart %>% arrange(X)

# Now merge with ratings - this ok as they are in same order
forchart2 <- cbind(ratings, forchart)
# Then remove the repeat response after checking by eye it is the same all the way down
forchart2 <- forchart2 %>% select(-response, -X)
forchart2 <- forchart2 %>% select(numtag, a:unc)

# prob need it long
forchart3 <- forchart2 %>%
  pivot_longer(!numtag, names_to = "cat", values_to = "count")

forchart3 <- forchart3 %>% filter(count==1)

# Now group by
forchart3 <- forchart3 %>% group_by(numtag, cat) %>% summarise(n=n())
forchart3$numtag <- as.factor(forchart3$numtag) 

p1 <- ggplot(data = forchart3, aes(y=cat, fill))


p1 <- ggplot(forchart3, aes(x=cat, y=n, fill=n)) +
  geom_col() +
  facet_wrap(~numtag)


p1

ggsave('for_neil.svg', width = 15, height = 10) 
  
  # stat_summary(geom='bar', fun='mean', colour="black", position = position_dodge(0.7)) + 
  # stat_summary(fun.data = 'mean_cl_normal', geom='errorbar', position = position_dodge(0.7),
  #              fun.args = list(mult = 1), width = .5) +
  # geom_point(data = df2, aes(y=mp), colour = 'red') +
  # facet_wrap(.~numtag))
#   theme(axis.text.x = element_text(angle=45, hjust=1, size = 10)) +
#   labs(y ='Proportion participants mentioned / Model probability')
# # geom_text(aes(label = count())) + # I never did manage to get n=n() in each facet with annotate
#aes(x = fct_reorder(tag.f, congs$Cong, .desc = T)) +


#------ Later - squeeze

# Squeeze 1 to just the columns we modelled in ecesm
rat_summ$PreferenceRat <- as.numeric(rat_summ$a) 
rat_summ$CharacterRat <- as.numeric(rat_summ$c) 
rat_summ$KnowledgeRat <- as.numeric(rat_summ$e)
rat_summ$StartRat <- as.numeric(rat_summ$f) 
rat_summ$OtherRat <- as.numeric(rat_summ$b|rat_summ$d|rat_summ$g|rat_summ$h|rat_summ$unc) # All the transient ones, Other and Unclear

# Reduce this df 
rat_summ <- rat_summ %>% select(response, PreferenceRat:OtherRat)

# Now merge with ppt data and factored numtags
pilot <- read.csv('../Exp2Explanation/Data/pilot.csv', stringsAsFactors = TRUE)
pilot <- pilot %>% select(-PreferenceRat:OtherRat) # But don't know what to do about numtag - that's the important bit and it got crapped up in csv format