# Experiment 1: Prediction

Behavioural experiment and causal model selection, obtains parameters for Experiment 2.

## Summary

Data collected in 2023.

## Authors

- Stephanie Droop
- Neil Bramley
- Tadeg Quillien
- Christopher Lucas

## Files / folders

### Experiment

Scripts and data from behavioural experiment in 2023 to inform situation model: Participants (n=90) saw 16 scenarios and, for each one, rated the 4 possible outcomes (2 food x 2 path) for how likely the character was to make that choice given their biography and starting position.

#### Scripts

- `master.R`
- `01preprocess.R`. Input `dataJan23.csv`; output `gwExp1data.Rda`.
- `02describe.R`. Exploratory faceted beeswarm plots and other to explore distribution of the participant ratings as raw Likert ratings (ie. before normalisation to make the target distribution in the Model section). Saves a series of 16 pdfs which are currently reported as Appendix 2.

### Model

Scripts to find and then plot best causal model of the behavioural experimental data. Saves model output and plots in the same folder.

- `master.R`
- `01targetDist.R` - clean and summarise participant Likert scale rating to obtain normalised target distribution (mean and sd) per situation. Input `gwExp1data.Rda` from Experiment folder. Output `targetDist.rda`.
- `02findModel.R` - causal selection task by brute force minimising KL divergence. Input `targetDist.rda`. Output `fitted.rda`. 
- `03processModel.R` - tidy, preprocess and present the best fitting model. Input `fitted.rda`. Output `model.rda`.
- `04graphs.R` - directed acyclic graph plots of the best fitting model. Input `model.rda`. Output `modelGraphs.pdf`. \\ TO DO


## Interpretation of KL divergence - a note for the model selection task

A note on KL-div:

Small KL (e.g., < 0.01 or < 0.1): Very good fit. The model's predictions are extremely close to the target.

Moderate KL (e.g., 0.1–0.5): Decent fit. The model captures the main features, but there are noticeable differences.

Large KL (e.g., > 1): Poor fit. The model and target distributions are quite different; the model is missing key aspects of the data
