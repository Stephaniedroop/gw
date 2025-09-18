###############################################################################################
############ Select best-fitting causal model by minimising K-L divergence #############
###############################################################################################


rm(list=ls())
library(tidyverse)


load('../../Experiment/Data/gwExp1data.Rda')



## ---------------------------------------------------------------------------------------------------------------
# 
td <- df |> 
  group_by(Situation) |> 
  summarise(p_short_pizza = mean(p_short_pizza, na.rm=T),
  p_long_pizza = mean(p_long_pizza, na.rm=T),
  p_short_hotdog = mean(p_short_hotdog, na.rm=T),
  p_long_hotdog = mean(p_long_hotdog, na.rm=T)) |> data.frame()

td <- td[,2:5] # 16 obs of 4

# Calculate marginal probabilities for path and destination
td_path <- (df |> 
              group_by(Situation) |> 
              summarise(p_long = mean(p_long, na.rm=T)))$p_long

td_destination <- (df |> 
                     group_by(Situation) |> 
                     summarise(p_hotdog = mean(p_hotdog, na.rm=T)))$p_hotdog



## ---------------------------------------------------------------------------------------------------------------
state_names <- c("P", "K", "C", "S", "PK", "PC", "PS", "KC", "KS", "CS")



## ---------------------------------------------------------------------------------------------------------------
# 59049 obs of 10 vars (3^10)
structures <- expand.grid(P = -1:1,
                          K = -1:1,
                          C = -1:1,
                          S = -1:1, 
                          PK = -1:1,
                          PC = -1:1,
                          PS = -1:1,
                          KC = -1:1,
                          KS = -1:1,
                          CS = -1:1)

init_full_par <- list(s = c(P = .5,
                          K = .5,
                          C = .5,
                          S = .5,
                          PK = .5,
                          PC = .5,
                          PS = .5,
                          KC = .5,
                          KS = .5,
                          CS = .5),
                    br = .5, # Base rate
                    tau = 1
)

# Create indexing grid for all possible situations - 16 obs of 4
ix <- expand.grid(S = 0:1, 
                  C = 0:1,
                  K = 0:1,
                  P = 0:1)

# Keep it in same order as rest of code
ix <- ix[, c("P", "K", "C", "S")]
rownames(ix) <- NULL



## ---------------------------------------------------------------------------------------------------------------
get_mod_pred <- function(struct, par) {
  # Use the predefined ix (created outside)
  states <- ix
  states$PK <- states$P * states$K
  states$PC <- states$P * states$C
  states$PS <- states$P * states$S
  states$KC <- states$K * states$C
  states$KS <- states$K * states$S
  states$CS <- states$C * states$S

  state_mat <- as.matrix(states[, state_names])
  
  # Precompute indices
  nor_idx <- which(struct == 1)
  nandnot_idx <- which(struct == -1)
  one_minus_s <- 1 - par$s
  
  # Vectorized calculations
  p <- sapply(1:nrow(state_mat), function(i) {
    state <- state_mat[i, ]
    nor <- c(1 - par$br, (one_minus_s[nor_idx] ^ state[nor_idx]))
    nandnot <- (one_minus_s[nandnot_idx] ^ state[nandnot_idx])
    (1 - prod(nor)) * prod(nandnot)
  })
  
  # Softmax
  out <- data.frame(p0 = 1 - p, p1 = p)
  out <- exp(as.matrix(out) / par$tau)
  out <- out / rowSums(out)
  out[, "p1"]
}




## ---------------------------------------------------------------------------------------------------------------
wrapper <- function(par, struct, td) {
  s_vals <- plogis(par[1:10])
  names(s_vals) <- state_names

  br_val <- plogis(par[11])
  tau_val <- exp(par[12])

  input_pars <- list(
    s = s_vals,
    br = br_val,
    tau = tau_val
  )

  model_pred <- get_mod_pred(struct, input_pars)
  model_probs <- cbind(1 - model_pred, model_pred)
  kl_div <- sum(rowSums(td * log(td / model_probs)))

  return(kl_div)
}




## ---------------------------------------------------------------------------------------------------------------
transform_params <- function(par) {
  c(plogis(par[1:11]), exp(par[12]))
}


## ---------------------------------------------------------------------------------------------------------------
# Test the wrapper
wrapper(par =  rep(.5,12), struct = unlist(structures[10000,]), td = cbind(1-td_path, td_path))
wrapper(par =  rep(.5,12), struct = unlist(structures[10000,]), td = cbind(1-td_destination, td_destination))

out <- optim(wrapper, par = rep(.5,12), struct = unlist(structures[5,]), td = cbind(1-td_path, td_path))
out 
s <- 58941
out <- optim(wrapper, par = rep(.5,12), struct=unlist(structures[s,]), td = cbind(1-td_destination, td_destination))
out 

tmp <- c(1/(1+exp(-out$par[1:11])),
       exp(out$par[12]))
tmp

transform_params(par = out$par)
wrapper(par =  out$par, struct=unlist(structures[5,]), td = cbind(1-td_path, td_path))


## ---------------------------------------------------------------------------------------------------------------
fit_structure <- function(struct_row, td) {
  optim_res <- optim(
    wrapper,
    par = rep(.5, 12),
    struct = unlist(structures[struct_row, ]),
    td = td
  )
  params <- transform_params(optim_res$par)
  c(kl = optim_res$value, setNames(params, c(state_names, "tau")))
}



## ---------------------------------------------------------------------------------------------------------------
n_structs <- nrow(structures)
fitted_path_mods <- matrix(NA, n_structs, 13)
fitted_destination_mods <- matrix(NA, n_structs, 13)

pb <- txtProgressBar(min = 0, max = n_structs, style = 3)
for (s in 1:n_structs) {
  fitted_path_mods[s, ] <- fit_structure(s, cbind(1 - td_path, td_path))
  fitted_destination_mods[s, ] <- fit_structure(s, cbind(1 - td_destination, td_destination))
  setTxtProgressBar(pb, s)
}
close(pb)
colnames(fitted_path_mods) <- colnames(fitted_destination_mods) <- c("kl", state_names, "br", "tau")
fitted_path_mods <- as.data.frame(fitted_path_mods)
fitted_destination_mods <- as.data.frame(fitted_destination_mods)

# 2 dfs, each of 59049 obs of 13 vars
save(file='fitted_situation_modsSD.rdata', fitted_path_mods, fitted_destination_mods) 


## ---------------------------------------------------------------------------------------------------------------
load('fitted_situation_modsSD.rdata', verbose = 5)


fitted_path_mods$n_edge <- rowSums(structures!=0)
fitted_destination_mods$n_edge <- rowSums(structures!=0)

complexity_penalisation <- 0.002 # just picked! justify by plotting how stable it is at different vals of complexity? TO DO free parameter hand fit.
which.min(fitted_path_mods$kl) # 31717 for first td but redone as 32204
# Make the KL bigger for each edge so to penalise complexity and so avoid saturated fully connected model
bpix <- which.min(fitted_path_mods$kl + fitted_path_mods$n_edge*complexity_penalisation) # 29291
bdix <- which.min(fitted_destination_mods$kl + fitted_destination_mods$n_edge*complexity_penalisation) # 32793



best_path <- structures[bpix,] # 00100-10000
best_path_params <- fitted_path_mods[bpix,]

best_destination <- structures[bdix,] # 1001111100
best_destination_params <- fitted_destination_mods[bdix,]


## ---------------------------------------------------------------------------------------------------------------
# Create the best fitting models for display
tmp <- fitted_path_mods[ which.min(fitted_path_mods$kl),]
fitted_path_params <- list(s = c(P = tmp$P,
                               K = tmp$K,
                               C = tmp$C,
                               S = tmp$S,
                               PK=tmp$PK,
                               PC=tmp$PC,
                               PS=tmp$PS,
                               KC=tmp$KC,
                               KS=tmp$KS,
                               CS=tmp$CS),
                         br = tmp$br,
                         tau = tmp$tau)

tmp <- fitted_destination_mods[ which.min(fitted_destination_mods$kl),]

fitted_destination_params <- list(s = c(P = tmp$P,
                                      K = tmp$K,
                                      C = tmp$C,
                                      S = tmp$S,
                                      PK=tmp$PK,
                                      PC=tmp$PC,
                                      PS=tmp$PS,
                                      KC=tmp$KC,
                                      KS=tmp$KS,
                                      CS=tmp$CS),
                                br = tmp$br,
                                tau = tmp$tau)


mpp <- get_mod_pred(structures[bpix,], fitted_path_params)
mpd <- get_mod_pred(structures[bdix,], fitted_destination_params)

df.m <- data.frame(situation = df$SituationVerbose[1:16], td_path = td_path, td_destination = td_destination, mp_path = mpp, mp_destination = mpd)

write.csv(df.m, 'model.csv') # 16 obs of 5 vars


