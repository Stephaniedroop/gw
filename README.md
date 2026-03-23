# Social Explanations in Gridworlds

A long-standing project forming chapter 2 of my thesis.

In much existing research on how people explain outcomes and reason about causes, the outcomes do naturally follow from the observed causes. But in real life things are often not so coherent. I wanted to study how people explain outcomes that don't obviously fit what they have seen.

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

## Citing packages used

This project makes use of the following R packages. Please cite them if you use this code.

*here*
Müller K (2025). _here: A Simpler Way to Find Your Files_. R package version 1.0.2,
  <https://CRAN.R-project.org/package=here>.
  
*renv*
Ushey K, Wickham H (2025). _renv: Project Environments_. R package version 1.1.5,
  <https://CRAN.R-project.org/package=renv>.
  
*gander*
Couch S (2025). _gander: High Performance, Low Friction Large Language Model Chat_. R package version
  0.1.0, <https://CRAN.R-project.org/package=gander>.
  
*ggplot2*
H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016

*ggdag*
Barrett M (2024). _ggdag: Analyze and Create Elegant Directed Acyclic Graphs_. R package version 0.2.13,
  <https://CRAN.R-project.org/package=ggdag>.
  
*tidyverse*
Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J,
  Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K,
  Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal of Open Source
  Software_, *4*(43), 1686. doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.
  
  
  
  
