# ==============================================================================
# Path and Destination Choice Model Analysis
# ==============================================================================
# This script analyzes experimental data from a path/destination choice task where
# participants rated the likelihood of different routes and destinations based on
# various factors:
#
# Variables:
# - P (Preference): Whether the person has a food preference (0=Absent, 1=Hotdog)
# - K (Knowledge): Whether they know about the hotdog stand (0=No, 1=Yes)
# - C (Character): Person's character type (0=Lazy, 1=Sporty)
# - S (Start): What's visible at the start (0=See_pizza, 1=See_hotdog)
#
# The script:
# 1. Loads and preprocesses experimental data
# 2. Fits causal models to predict path choices (short vs long)
# 3. Fits causal models to predict destination choices (pizza vs hotdog)
# 4. Uses model comparison to find the best causal structure
# 5. Visualizes the results and model predictions

# Clear workspace
rm(list = ls())

# Load Required Libraries
library(tidyverse)
library(ggplot2)
library(igraph)
library(RColorBrewer)

# ==============================================================================
# Data Loading and Preprocessing
# ==============================================================================
# Load raw experimental data
df.raw <- read.csv("forneil.csv")

# Recode visibility conditions and compute likelihoods
# Each participant rated how likely different path/destination combinations were
df.raw <- df.raw %>%
  mutate(
    lik_short_pizza = if_else(Start == "Hot dogs visible", short_inv, short_vis),
    lik_long_pizza = if_else(Start == "Hot dogs visible", long_inv, long_vis),
    lik_short_hotdog = if_else(Start == "Hot dogs visible", short_vis, short_inv),
    lik_long_hotdog = if_else(Start == "Hot dogs visible", long_vis, long_inv)
  ) %>%
  select(-short_vis, -long_vis, -short_inv, -long_inv)

# Convert raw likelihoods to probabilities
df <- df.raw %>%
  mutate(
    # Sum of likelihoods for normalization
    lik_sum = lik_short_pizza + lik_long_pizza + lik_short_hotdog + lik_long_hotdog,

    # Convert to probabilities
    p_short_pizza = lik_short_pizza / lik_sum,
    p_long_pizza = lik_long_pizza / lik_sum,
    p_short_hotdog = lik_short_hotdog / lik_sum,
    p_long_hotdog = lik_long_hotdog / lik_sum,

    # Marginal probabilities
    p_long = p_long_pizza + p_long_hotdog,
    p_hotdog = p_short_hotdog + p_long_hotdog
  )

# ==============================================================================
# Factor Coding and Situation Setup
# ==============================================================================
# Convert categorical variables to factors and create situation codes
df <- df %>%
  select(-X, -situTag) %>%
  mutate(
    # Main factors with readable labels
    Preference = factor(Preference, levels = c("Absent", "Hot dogs"), labels = c("Absent", "Hotdog")),
    Knowledge = factor(Knowledge, levels = c("No", "Yes")),
    Character = factor(Character, levels = c("Lazy", "Sporty")),
    Start = factor(Start, levels = c("Pizza visible", "Hot dogs visible"), labels = c("See_pizza", "See_hotdog")),

    # Numeric coding of factors (0/1)
    P = factor(Preference, levels = c("Absent", "Hotdog"), labels = 0:1),
    K = factor(Knowledge, levels = c("No", "Yes"), labels = 0:1),
    C = factor(Character, levels = c("Lazy", "Sporty"), labels = 0:1),
    S = factor(Start, levels = c("See_pizza", "See_hotdog"), labels = 0:1),

    # Create situation identifiers
    SituationVerbose = paste0(Preference, Knowledge, Character, Start),
    Situation = paste0(P, K, C, S),

    # Participant ID coding
    mindsCode = factor(mindsCode, levels = unique(mindsCode)),
    id = factor(mindsCode, levels = unique(mindsCode), labels = 1:length(unique(mindsCode)))
  ) %>%
  arrange(id, S, C, K, P)

# ==============================================================================
# Calculate Target Distributions
# ==============================================================================
# Get the complete distribution over all outcomes
td <- df %>%
  group_by(Situation) %>%
  summarise(
    p_short_pizza = mean(p_short_pizza, na.rm = T),
    p_long_pizza = mean(p_long_pizza, na.rm = T),
    p_short_hotdog = mean(p_short_hotdog, na.rm = T),
    p_long_hotdog = mean(p_long_hotdog, na.rm = T)
  ) %>%
  data.frame()
td <- td[, 2:5]

# Calculate marginal probabilities for path and destination
td_path <- (df %>% group_by(Situation) %>% summarise(p_long = mean(p_long, na.rm = T)))$p_long
td_destination <- (df %>% group_by(Situation) %>% summarise(p_hotdog = mean(p_hotdog, na.rm = T)))$p_hotdog

# ==============================================================================
# Model Structure Definition
# ==============================================================================
# Define all possible causal structures
# Each variable can be:
#  1: positive influence
#  0: no influence
# -1: negative influence
structures <- expand.grid(
  # Main effects
  P = -1:1, K = -1:1, C = -1:1, S = -1:1,
  # Interaction effects
  PK = -1:1, PC = -1:1, PS = -1:1, KC = -1:1, KS = -1:1, CS = -1:1
)

# Initialize model parameters
init_full_par <- list(
  s = c(P = .5, K = .5, C = .5, S = .5, PK = .5, PC = .5, PS = .5, KC = .5, KS = .5, CS = .5),
  br = .5, # Base rate
  tau = 1 # Temperature parameter for softmax
)

# Create indexing grid for all possible situations
ix <- expand.grid(S = 0:1, C = 0:1, K = 0:1, P = 0:1)

# ==============================================================================
# Model Prediction Functions
# ==============================================================================
#' Predict probabilities for outcomes given a causal structure and parameters
#'
#' The model uses a noisy-OR/noisy-AND-NOT combination:
#' - Positive influences combine via noisy-OR
#' - Negative influences combine via noisy-AND-NOT
#' - Final probability is transformed via softmax
#'
#' @param struct Vector indicating causal influences (-1=negative, 0=none, 1=positive)
#' @param par List of parameters (strength values and temperature)
#' @return Vector of probabilities for each situation
get_mod_pred <- function(struct, par) {
  out <- data.frame(p0 = rep(NA, nrow(ix)), p1 = rep(NA, nrow(ix)))

  # Loop through all possible situations
  for (P in 0:1) {
    for (K in 0:1) {
      for (C in 0:1) {
        for (S in 0:1) {
          # Create state vector (main effects and interactions)
          state <- c(P, K, S, C, P * K, P * C, P * S, K * C, K * S, C * S)

          # Calculate probability via noisy-OR/AND-NOT combination
          nor <- c((1 - par$br), (1 - par$s[struct == 1])^state[struct == 1]) # Positive influences
          nandnot <- (1 - par$s[struct == -1])^state[struct == -1] # Negative influences
          p <- (1 - prod(nor)) * prod(nandnot) # Combined probability

          # Store probabilities for this situation
          out$p0[ix$P == P & ix$K == K & ix$C == C & ix$S == S] <- 1 - p
          out$p1[ix$P == P & ix$K == K & ix$C == C & ix$S == S] <- p
        }
      }
    }
  }

  # Apply softmax transformation and return probabilities
  out <- exp(out / par$tau)
  out <- sweep(out, 1, rowSums(out), "/")
  return(out$p1)
}

# ==============================================================================
# Model Fitting Functions
# ==============================================================================
#' Optimization wrapper for fitting causal models
#'
#' Converts parameters from optimization space, generates predictions,
#' and calculates KL divergence from target distribution
#'
#' @param par Vector of parameters in optimization space
#' @param struct Causal structure being tested
#' @param td Target distribution to fit
#' @return KL divergence between model and target
wrapper <- function(par, struct, td) {
  # Convert parameters from optimization space
  input_pars <- list(
    s = c(
      P = 1 / (1 + exp(-par[1])), # Preference influence
      K = 1 / (1 + exp(-par[2])), # Knowledge influence
      C = 1 / (1 + exp(-par[3])), # Character influence
      S = 1 / (1 + exp(-par[4])), # Start state influence
      PK = 1 / (1 + exp(-par[5])), # Preference-Knowledge interaction
      PC = 1 / (1 + exp(-par[6])), # Preference-Character interaction
      PS = 1 / (1 + exp(-par[7])), # Preference-Start interaction
      KC = 1 / (1 + exp(-par[8])), # Knowledge-Character interaction
      KS = 1 / (1 + exp(-par[9])), # Knowledge-Start interaction
      CS = 1 / (1 + exp(-par[10])) # Character-Start interaction
    ),
    br = 1 / (1 + exp(-par[11])), # Base rate
    tau = exp(par[12]) # Temperature
  )

  # Get model predictions and calculate KL divergence
  mp <- get_mod_pred(struct, input_pars)
  mp <- cbind(1 - mp, mp)
  kl <- sum(rowSums(td * log(td / mp)))

  return(kl)
}

# ==============================================================================
# Model Fitting Process
# ==============================================================================
# Initialize dataframes to store results
fitted_path_mods <- fitted_destination_mods <- data.frame(
  kl = rep(NA, nrow(structures)), # KL divergence
  P = rep(NA, nrow(structures)), # Parameter values
  K = rep(NA, nrow(structures)),
  C = rep(NA, nrow(structures)),
  S = rep(NA, nrow(structures)),
  PK = rep(NA, nrow(structures)),
  PC = rep(NA, nrow(structures)),
  PS = rep(NA, nrow(structures)),
  KC = rep(NA, nrow(structures)),
  KS = rep(NA, nrow(structures)),
  CS = rep(NA, nrow(structures)),
  br = rep(NA, nrow(structures)), # Base rate
  tau = rep(NA, nrow(structures)) # Temperature
)

# Fit models for path and destination predictions
for (s in 1:nrow(structures)) {
  # Fit path choice model
  out <- optim(wrapper,
    par = rep(.5, 12),
    struct = unlist(structures[s, ]),
    td = cbind(1 - td_path, td_path)
  )
  fitted_path_mods$kl[s] <- out$value
  fitted_path_mods[s, 2:13] <- c(1 / (1 + exp(-out$par[1:11])), exp(out$par[12]))

  # Fit destination choice model
  out <- optim(wrapper,
    par = rep(.5, 12),
    struct = unlist(structures[s, ]),
    td = cbind(1 - td_destination, td_destination)
  )
  fitted_destination_mods$kl[s] <- out$value
  fitted_destination_mods[s, 2:13] <- c(1 / (1 + exp(-out$par[1:11])), exp(out$par[12]))

  if (s / 1000 == round(s / 1000)) {
    print(paste("Fitted", s, "models"))
  }
}

# ==============================================================================
# Model Selection
# ==============================================================================
# Add complexity penalties based on number of edges
fitted_path_mods$n_edge <- rowSums(structures != 0)
fitted_destination_mods$n_edge <- rowSums(structures != 0)
complexity_penalisation <- 0.002

# Find best models accounting for complexity
bpix <- which.min(fitted_path_mods$kl + fitted_path_mods$n_edge * complexity_penalisation)
bdix <- which.min(fitted_destination_mods$kl + fitted_destination_mods$n_edge * complexity_penalisation)

# Extract best models and their parameters
best_path <- structures[bpix, ]
best_path_params <- fitted_path_mods[bpix, ]
best_destination <- structures[bdix, ]
best_destination_params <- fitted_destination_mods[bdix, ]

# ==============================================================================
# Create Fitted Model Objects
# ==============================================================================
# Extract parameters for best path model
tmp <- fitted_path_mods[which.min(fitted_path_mods$kl), ]
fitted_path_params <- list(
  s = c(
    P = tmp$P,
    K = tmp$K,
    C = tmp$C,
    S = tmp$S,
    PK = tmp$PK,
    PC = tmp$PC,
    PS = tmp$PS,
    KC = tmp$KC,
    KS = tmp$KS,
    CS = tmp$CS
  ),
  br = tmp$br,
  tau = tmp$tau
)

# Extract parameters for best destination model
tmp <- fitted_destination_mods[which.min(fitted_destination_mods$kl), ]
fitted_destination_params <- list(
  s = c(
    P = tmp$P,
    K = tmp$K,
    C = tmp$C,
    S = tmp$S,
    PK = tmp$PK,
    PC = tmp$PC,
    PS = tmp$PS,
    KC = tmp$KC,
    KS = tmp$KS,
    CS = tmp$CS
  ),
  br = tmp$br,
  tau = tmp$tau
)

# Generate predictions from best models
mpp <- get_mod_pred(structures[bpix, ], fitted_path_params)
mpd <- get_mod_pred(structures[bdix, ], fitted_destination_params)

# Combine data for plotting
df.m <- data.frame(
  situation = df$SituationVerbose[1:16],
  td_path = td_path, # Target path distribution
  td_destination = td_destination, # Target destination distribution
  mp_path = mpp, # Model path predictions
  mp_destination = mpd # Model destination predictions
)

# ==============================================================================
# Visualization
# ==============================================================================
# Plot path model predictions
ggplot(df.m, aes(y = td_path, x = situation)) +
  geom_bar(stat = "identity") +
  geom_point(aes(y = mp_path), colour = "red") +
  theme_bw() +
  labs(y = "P(long route)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Path model predictions against participant means")
ggsave("path_predictions.pdf", width = 6, height = 4)

# Plot destination model predictions
ggplot(df.m, aes(y = td_destination, x = situation)) +
  geom_bar(stat = "identity") +
  geom_point(aes(y = mp_destination), colour = "blue") +
  theme_bw() +
  labs(y = "P(hotdog)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Destination model predictions against participant means")
ggsave("destination_predictions.pdf", width = 6, height = 4)

# Create joint prediction plot
df.mj <- data.frame(
  # Actual participant data
  target_distribution = unlist(td),
  situation = df$SituationVerbose[1:16],
  # Label predictions
  prediction = factor(
    rep(c("short_pizza", "long_pizza", "short_hotdog", "long_hotdog"), each = 16),
    levels = c("short_pizza", "long_pizza", "short_hotdog", "long_hotdog")
  ),
  # Independent model predictions
  factorised_target = c(
    (1 - td_path) * (1 - td_destination), # P(short & pizza)
    td_path * (1 - td_destination), # P(long & pizza)
    (1 - td_path) * td_destination, # P(short & hotdog)
    td_path * td_destination # P(long & hotdog)
  ),
  # Joint model predictions
  model_prediction = c(
    (1 - mpp) * (1 - mpd), # P(short & pizza)
    mpp * (1 - mpd), # P(long & pizza)
    (1 - mpp) * mpd, # P(short & hotdog)
    mpp * mpd # P(long & hotdog)
  )
)

# Plot joint predictions
ggplot(df.mj, aes(y = target_distribution, x = prediction)) +
  geom_bar(stat = "identity") +
  geom_point(aes(y = factorised_target), colour = "gray") +
  geom_point(aes(y = model_prediction), colour = "green") +
  facet_wrap(~situation, ncol = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Overall predictions against participant means")
ggsave("overall_predictions.pdf", width = 8, height = 10)

# ==============================================================================
# Network Visualization
# ==============================================================================
# Setup graph labels and structure
labs <- c(
  "P", "Pu", "K", "Ku", "C", "Cu", "S", "Su",
  "PK", "PKu", "PC", "PCu", "PS", "PSu",
  "KC", "KCu", "KS", "KSu", "CS", "CSu",
  "Path"
)

# Create base connectivity matrix
fg <- matrix(0, 21, 21, dimnames = list(labs, labs))
for (i in 1:20) {
  fg[i, 21] <- 1 # All nodes connect to outcome
}

# Add connections between variables and their interactions
fg[1, 9] <- fg[1, 11] <- fg[1, 13] <- 1 # P connects to PK, PC, PS
fg[3, 9] <- fg[3, 15] <- fg[3, 17] <- 1 # K connects to PK, KC, KS
fg[5, 11] <- fg[5, 15] <- fg[5, 19] <- 1 # C connects to PC, KC, CS
fg[7, 13] <- fg[7, 17] <- fg[7, 19] <- 1 # S connects to PS, KS, CS

# Setup node positions
locations <- matrix(0, 21, 2)
locations[, 2] <- c(rev(seq(0, 1, length.out = 20)), 0.5) # Vertical positions
locations[, 1] <- c(
  rep(c(0, 0.15), 4), # Horizontal positions
  rep(c(0.4, 0.55), 6),
  1
)

# Create path model graph
pg <- dg <- fg
pg[rep(best_path == 0, each = 2), ] <- 0 # Remove unused edges
keep <- pg[, 21] != 0
keep[21] <- T
pg <- pg[keep, keep]

# Create igraph object and set visual properties
g <- graph_from_adjacency_matrix(pg)
V(g)$name <- V(g)$label <- labs[keep]

# Create color gradient for nodes
cols <- colorRampPalette(c("white", "#003377"), space = "rgb")(100)
path_node_cols <- round(
  c(t(cbind(
    c(rep(0.5, 4), rep(0.25, 6)),
    unlist(best_path_params[2:11])
  )), 0),
  digits = 2
) * 100 + 1
path_node_cols[path_node_cols > 100] <- 100

# Set node properties
V(g)$label.font <- 2
V(g)$size <- 30
V(g)$color <- cols[path_node_cols[keep]]
V(g)$label.cex <- 1
V(g)$label.color <- "black"

# Set edge colors based on influence type
el <- apply(get.edgelist(g), 1, paste, collapse = "-")
elix <- grep("u", el)
green_ones <- c(
  paste0(names(best_path)[which(best_path == 1)], "-Path"),
  paste0(names(best_path)[which(best_path == 1)], "u-Path")
)
red_ones <- c(
  paste0(names(best_path)[which(best_path == -1)], "-Path"),
  paste0(names(best_path)[which(best_path == -1)], "u-Path")
)
edge_col_vec <- rep("black", length(el))

for (i in 1:length(green_ones)) {
  edge_col_vec[el == green_ones[i]] <- "darkgreen" # Positive influences
}
for (i in 1:length(red_ones)) {
  edge_col_vec[el == red_ones[i]] <- "darkred" # Negative influences
}

# Set final graph properties
V(g)$label.family <- "sans"
E(g)$width <- 1
E(g)$curved <- 0
E(g)$color <- edge_col_vec

# Position nodes and save path model graph
these_locs <- locations[keep, ]
these_locs[1:(nrow(these_locs) - 1), 2] <- seq(1, 0, length.out = nrow(these_locs) - 1)
pdf("path_mod.pdf", width = 6, height = 6)
plot(g, layout = these_locs, edge.arrow.size = 1)
dev.off()

# ... similar code for destination model graph ...
