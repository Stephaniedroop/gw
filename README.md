# Social Explanations in Gridworlds

A long-standing project forming chapter 2 of my thesis.

In much exisiting research on how people explain outcomes and reason about causes, the outcomes do naturally follow from the observed causes. But in real life things are often not so coherent. I wanted to study how people explain outcomes that don't obviously fit what they have seen.

The general universe the studies take place in is a `gridworld` where characters/avatars/agents move around a stylised boxy urban environment to get to various food trucks, where they stop to eat. Everything is a binary variable: the agents start either in view of pizza or hotdog, they go to pizza or hotdog, and they go directly or a roundabout way. We also know various biographical details about the agents: they either know the area or they don't, they either like hotdogs or they don't, they are either sporty or lazy.

These details combine to promote various behaviours: eg. if an agent likes hotdogs and starts in view of hotdogs and is lazy and doesn't know the area, then it might make more sense for them to go straight to the hotdog van they can see. Likewise, if they can't see the pizza but they know the area, then if they go round the corner to the pizza then it makes sense and we can explain their behaviour if we know they know the area. (Sometimes the biographical details don't seem to immediately explain the outcome and then it might get interesting).

Anyway, we know from daily life that people can fluidly brainstorm reasons for what they see, even when the available evidence doesn't fully explain it. (Presumably people have some kind of _world model_ of the kinds of things that tend to cause other events). In that sense it wouldn't be groundbreaking to find scientific evidence for it. However, it's an interesting information problem to solve. That people _can_ do it is perhaps clear; but _how_ they do it is not clear at all.

Enter computational modelling as a way to formalise theories in programs and maths to precisely manage the information flows, and see what types of models can reproduce people's patterns of answers.

This repo has closed end-to-end process of generating our own data and then modelling it:

1. **Experiment 1:** Data and model for a causal model selection task. We showed people all combinations of variables (biohraphies and starting position) and asked them to rate how likely each outcome (agent taking a certain path to a certain food) was. Then we found the best fitting causal structure that governs how variables act in this 'world', i.e. the causal model strengths that drive the next experiment. Can also call this the **prediction** part, because it gives the parameters that allow to say how likely different outcomes are. Like looking forwards.
2. **Experiment 2:** We showed a new set of participants the paths the agents took, and asked each time, 'What's the best explanation for the character's action?'. This generated a dataset of free text explanations. Can also call this the **explanation** part, because we are diagnosing or explaining outcomes we have already seen. Like looking backwards.
3. Qualitative ratings of the free text explanations on various metrics, by other people.

Each of these folders has its own README on the main scripts and how to use them. Summary: it's all in R and each has a masterscript which calls the other scripts.

The output of the whole repo is rated explanations from a rich causal system, which then have to be modelled. This whole chapter was to get the data ready to be modelled, which happens in Chapter 3 of the thesis, and will be new repo [TO DO].

### Later_rating

- `processing_ratings.R`. Script to read in rater ratings data, standardise it (by eg removing comments and question marks, making it numeric etc) and saving a version which replaces numbers >1 with 1. Saves four dfs in `ratings.Rda`. **This last saved S's and V's ratings 20 Feb ready to use for the final version, k=.61**. Then go to `merge_ratings.R` to merge them, or `gw_irr.R` for stats on agreement.
- `gw_irr.R`. Script to calculate inter rater agreement: a homemade function summarises matrix as contingency table then calculates cohen's kappa.
- `merge_ratings.R`. Script to merge the rater ratings into a single file by saving only the intersection, with a new column 'unclear' for all where they disagreed. Saves it as `to_go.rdata`. Next go to
- `process_merged_ratings.R` which generates plot for checking distribution of rating categories and whether they are good or not, and will later do other calcs on these.

### OtherNoCode

Any project admin, pdfs, docs, setup that found their way into this folder.
