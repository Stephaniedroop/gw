# ---------
# cut
# --------

# Can get these from the previous script without reaggregating the four, depends if we decide 2 way or 4 way
# pizza <- tag_counts |>
#   filter(tag_group %in% c('PizzaShort', 'PizzaLong')) |>
#   mutate(cat = 'Pizza')
#
# hotdog <- tag_counts |>
#   filter(tag_group %in% c('HotdogShort', 'HotdogLong')) |>
#   mutate(cat = 'Hotdog')
#
# short <- tag_counts |>
#   filter(tag_group %in% c('PizzaShort', 'HotdogShort')) |>
#   mutate(cat = 'Short')
#
# long <- tag_counts |>
#   filter(tag_group %in% c('PizzaLong', 'HotdogLong')) |>
#   mutate(cat = 'Long')

# Now each of these: group by condObs, and summarise what there is
# A new column  that is just the names of any of the 8 causes that are present, concatenated together. So for example if P=1, K=0, C=1, S=0, Pu=0, Ku=1, Cu=0, Su=0 then this column would be "P.Ku". If all 0s then "".
# ann_long <- ann_long |>
#   rowwise() |>
#   mutate(
#     present_causes = paste0(
#       c("P", "K", "C", "S", "Pu", "Ku", "Cu", "Su", "unc")[
#         c(P, K, C, S, Pu, Ku, Cu, Su, Unclear) == 1
#       ],
#       collapse = ""
#     )
#   )
#
# interactions <- c('PK', 'PC', 'PS', 'KC', 'KS', 'CS')

# If present_causes is in interactions, add u on the end
# ann_long <- ann_long |>
#   mutate(
#     present_causes = ifelse(
#       present_causes %in% interactions,
#       paste0(present_causes, "u"),
#       present_causes
#     )
#   )

# Add col to df_long called node3 which is whichever column has the 1 in it
df_long <- df |>
  rowwise() |>
  mutate(
    present_causes = paste0(
      c("P", "K", "C", "S", "Pu", "Ku", "Cu", "Su", "unc")[
        c(P, K, C, S, Pu, Ku, Cu, Su, Unclear) == 1
      ],
      collapse = "."
    )
  )

counts_long <- df_long |>
  group_by(condObs, present_causes) |>
  summarise(count = n()) |>
  ungroup()

counts_long <- counts_long |>
  group_by(condObs) |>
  mutate(normed_count = count / sum(count)) |>
  ungroup()

# Merge counts_long with long_ces by condObs and present_causes
long_join <- long_ces |>
  left_join(counts_long, by = c("condObs", "present_causes"))

# It merged in the same number for both =0 and =1 so need to solve that

# Before can plot, need to collapse the separate 0 and 1 ces values, or do the norming later of normed count, otherwise it is counting twice

# NOW MERGE, BUT WHAT ON? CS=0 is C.S, but what is C=1, and what is Cu=1. Just the ones that are there are tehre and everything else left blank?

# NOw do these across all... but first

# TO GET POWERSET:
# select the counterfactual simulations for which the combined set of variables,
# and count how many of them the effect occurs; select all the other counterfactual simulations
# and count how many of them the effect occurs. calculate the cor from that, exactly as you did
# when you just selected based on a single variable

# OR in the meantime: use the rated set from claude?

# this maybe not needed
# food_ces <- food_ces |>
#   group_by(condition) |>
#   mutate(postces_norm = exp(postces) / sum(exp(postces))) |>
#   ungroup()
#
# path_ces <- path_ces |>
#   group_by(condition) |>
#   mutate(postces_norm = exp(postces) / sum(exp(postces))) |>
#   ungroup()

# df_long <- df |>
#   filter(str_ends(as.character(tag), '1')) |>
#   mutate(cond = 'Long')
#
# df_short <- df |>
#   filter(str_ends(as.character(tag), '0')) |>
#   mutate(cond = 'Short')
#
# df_pizza <- df |>
#   filter(
#     str_ends(as.character(tag), '00') | str_ends(as.character(tag), '01')
#   ) |>
#   mutate(cond = 'Pizza')
#
# df_hotdog <- df |>
#   filter(
#     str_ends(as.character(tag), '10') | str_ends(as.character(tag), '11')
#   ) |>
#   mutate(cond = 'Hotdog')
#
#
# # Now for each of those, group by condObs and count right
# counts_long <- df_long |>
#   group_by(condObs, right) |>
#   summarise(count = n()) |>
#   ungroup()
#
# counts_long <- counts_long |>
#   group_by(condObs) |>
#   mutate(normed_count = count / sum(count)) |>
#   ungroup() |>
#   rename(node3 = right)

# ---

# counts_short <- df_short |>
#   group_by(condObs, right) |>
#   summarise(count = n()) |>
#   ungroup()
#
# counts_short <- counts_short |>
#   group_by(condObs) |>
#   mutate(normed_count = count / sum(count)) |>
#   ungroup() |>
#   rename(node3 = right)
#
# # ---
#
# counts_pizza <- df_pizza |>
#   group_by(condObs, right) |>
#   summarise(count = n()) |>
#   ungroup()
#
# counts_pizza <- counts_pizza |>
#   group_by(condObs) |>
#   mutate(normed_count = count / sum(count)) |>
#   ungroup() |>
#   rename(node3 = right)
#
# # ---
#
# counts_hotdog <- df_hotdog |>
#   group_by(condObs, right) |>
#   summarise(count = n()) |>
#   ungroup()
#
# counts_hotdog <- counts_hotdog |>
#   group_by(condObs) |>
#   mutate(normed_count = count / sum(count)) |>
#   ungroup() |>
#   rename(node3 = right)

# Dodgy but replace br with Unclear
# long_ces$node3[long_ces$node3 == 'br=0'] <- 'Unclear'
# long_ces$node3[long_ces$node3 == 'br=1'] <- 'Unclear'
#
# # Now merge each of these with their ces
# long_join <- long_ces |>
#   left_join(counts_long, by = c("condObs", "node3"))
#
# long_join[is.na(long_join)] <- 0
#
# long_join <- long_join |>
#   select(condObs, node3, normed_count, postces_norm)
#
# long_join$condObs <- as.factor(long_join$condObs)
# long_join$node3 <- as.factor(long_join$node3)

# long_join |>
#   group_by(condition) |>
#   summarise(total_count = sum(normed_count), total_ces = sum(postces_norm))

# -------- PLOTS ---------- Initial 4-way

# Split tag_counts into the four tag_groups, and for each one do a plot like p1
pPizzaShort <- tag_counts |>
  filter(tag_group == 'PizzaShort') |>
  ggplot(aes(x = present_causes, y = count)) +
  geom_col() +
  facet_wrap(~tag) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "Pizza Short"
  )

pPizzaLong <- tag_counts |>
  filter(tag_group == 'PizzaLong') |>
  ggplot(aes(x = present_causes, y = count)) +
  geom_col() +
  facet_wrap(~tag) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "Pizza Long"
  )

pHotdogShort <- tag_counts |>
  filter(tag_group == 'HotdogShort') |>
  ggplot(aes(x = present_causes, y = count)) +
  geom_col() +
  facet_wrap(~tag) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "Hotdog Short"
  )

pHotdogLong <- tag_counts |>
  filter(tag_group == 'HotdogLong') |>
  ggplot(aes(x = present_causes, y = count)) +
  geom_col() +
  facet_wrap(~tag) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(
    x = "Present Causes",
    y = "Number of Rating",
    title = "Hotdog Long"
  )

# Now a plot using ggplot. tag is facet and present_causes is bar
# p1 <- ggplot(tag_counts, aes(x = present_causes, y = count)) +
#   geom_col() +
#   facet_wrap(~tag) +
#   #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
#   #theme(axis.text.x = element_text(size = 7)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3)) +
#   labs(
#     x = "Present Causes",
#     y = "Number of Rating",
#     title = "Present Causes by Situation"
#   )
#
# p1
