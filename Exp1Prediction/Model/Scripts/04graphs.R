##########################################################################################
#  Plotting best-fitting causal model
##########################################################################################

library(here)

library(dagitty)
library(ggplot2)
library(ggdag)

# Load the target distribution and the model prediction for each of the 16 situations
load(here('Exp1Prediction', 'Model', 'Data', 'model.Rda'))  # Actually this is not saved yet; it will be but for now use 

# the dependencies between path and length are too subtle to bother with, so we are going to model path x destination
# and this fits our pattern well enough
# the next task is to compute cesm for our situations of interest from the 


# A function to get the causal formulas to send for dagify 
# @param causevector A named integer vector of 0s and 1s indicating which variables are included in the best fitting model

# Function to get edges for dagify, in the form of formulas
# The main logic of the function is that for any variables that are included (1s or -1s in causevector)
# it also needs to get the corresponding '-u' variable (unobserved cause), which is an edge going straight from '-u' to effect
# Then it needs to get the edges from the interaction variables back to their constituent variables (but only the ones that are 1,-1)

# if (is.null(state_names)) {
#   state_names <- paste0("V", seq_along(causevector))
# }

get_formulas <- function(causevector) {
  state_names = names(causevector)
  vars <- state_names[causevector != 0]
  signs <- causevector[causevector != 0]
  singles <- vars[nchar(vars) == 1]
  interactions <- vars[nchar(vars) == 2]
  formulas <- list()
  
  if (length(interactions) > 0 & length(singles) > 0) {
    for (int_var in interactions) {
      constituents <- unlist(strsplit(int_var, split = ""))
      for (constituent in constituents) {
        if (constituent %in% singles) {
          formula <- paste(int_var, "~", constituent)
          formulas <- c(formulas, list(formula))
        }
      }
    }
  }
  # Create corresponding unobserved cause variables
  #vars_u <- paste0(vars, "u")
  
  # Create terms with appropriate signs (only if we need the actual equation for generative v preventative - but we don't)
  # var_terms <- paste(ifelse(signs == 1, "+", "-"), vars)
  # var_u_terms <- paste(ifelse(signs == 1, "+", "-"), vars_u)
  
  #all_terms <- c(var_terms, var_u_terms)
  #varsforform <- paste(all_terms, collapse = " ")

  all_terms <- c(vars, paste0(vars, "u"))
  varsforform <- paste(all_terms, collapse = " + ")
  
  # Change this manually for now
  formula1 <- paste("Destination ~", varsforform)
  formulas <- c(list(formula1), formulas)
  return(formulas)
}


# Run the function - remember to change manually between path and destination for now
# Path
formulaP <- get_formulas(best_path)
print(formulaP) 
# Destination
formulaD <- get_formulas(best_destination)
print(formulaD)  

# It returns a list of character strings, not actual formulas, so turn it to formulas
formula_listP <- lapply(formulaP, as.formula)
formula_listD <- lapply(formulaD, as.formula)


#------------- PATH -------------------
dagP <- do.call(dagify, formula_listP)
# Set coordinates immediately after creating `dagP`
coordinates(dagP) <- list(
  x = c(K = 0, Ku = -0.5, KS = 1, KSu = 1.5, Path = 0.5),
  y = c(K = 1.75, Ku = 1.5, KS = 1.75, KSu = 1.5, Path = 0.5)
)
tidy_dagP <- tidy_dagitty(dagP)
ggdag(tidy_dagP) #+ remove_axes() # try with axes to get the coords first then remove


# ------ Color the edges -------

# ------ Test for path ------------
# Create a mapping of edge colors based on signs
edge_colors <- data.frame(
  name = names(best_path)[best_path != 0],
  sign = best_path[best_path != 0]
)
# Add the corresponding 'u' variables
edge_colors_u <- data.frame(
  name = paste0(edge_colors$name, "u"),
  sign = edge_colors$sign
)

all_edge_colors <- rbind(edge_colors, edge_colors_u)

# Now map these colors to the edges in tidy_dagP - use two $ to get into the nested layers
tidy_dagP$data$edge_color <- 
  ifelse(tidy_dagP$data$name %in% 
           all_edge_colors$name[all_edge_colors$sign == 1], "green",
                                    ifelse(tidy_dagP$data$name %in% 
                                             all_edge_colors$name[all_edge_colors$sign == -1], "red", "black"))

# tidydagP was already defined above, just adding color to the edges now
ggdag(tidy_dagP) + 
  geom_dag_edges(aes(edge_color = edge_color)) +
  scale_color_identity()


#------- Initial stab at annotate edges with parameters -------------
# For any node ending in '-u' attach the corresponding param of the letters before 'u'

# Create a mapping from node names to parameters
param_mapping <- setNames(best_path_params[names(best_path)], names(best_path))

# Add parameter labels to tidy_dagP data
tidy_dagP$data$param_label <- ifelse(
  grepl("u$", tidy_dagP$data$name),
  as.character(round(param_mapping[sub("u$", "", tidy_dagP$data$name)], 3)),
  ""
)

dp <- ggdag(tidy_dagP) + 
  geom_dag_edges(aes(edge_color = edge_color)) +
  scale_color_identity() +
  geom_dag_text(aes(label = param_label), nudge_y = -0.5, color = "blue", size = 3)


#-------------  DESTINATION ------------------
dagD <- do.call(dagify, formula_listD)
# Set coordinates immediately after creating `dagD`
coordinates(dagD) <- list(
    x = c(P = -1.5, Pu = -1.5, PS = -0.5, PSu = 0.25, KS = 1, KSu = 1.25, CS = 2, CSu = 2.25, Destination = 0.5),
    y = c(P = 1.5, Pu = 1, PS = 2, PSu = 1.75, KS = 1.5, KSu = 1.25, CS = 1.5, CSu = 1.25, Destination = 0.5)
)
tidy_dagD <- tidy_dagitty(dagD)
ggdag(tidy_dagD)

# Create a mapping of edge colors based on signs
edge_colors <- data.frame(
  name = names(best_destination)[best_destination != 0],
  sign = best_destination[best_destination != 0]
)
# Add the corresponding 'u' variables
edge_colors_u <- data.frame(
  name = paste0(edge_colors$name, "u"),
  sign = edge_colors$sign
)

all_edge_colors <- rbind(edge_colors, edge_colors_u)

# Now map these colors to the edges in tidy_dagP - use two $ to get into the nested layers
tidy_dagD$data$edge_color <- 
  ifelse(tidy_dagD$data$name %in% 
           all_edge_colors$name[all_edge_colors$sign == 1], "green",
         ifelse(tidy_dagD$data$name %in% 
                  all_edge_colors$name[all_edge_colors$sign == -1], "red", "black"))

# tidydagP was already defined above, just adding color to the edges now
ggdag(tidy_dagD) + 
  geom_dag_edges(aes(edge_color = edge_color)) +
  scale_color_identity()


#------- Initial stab at annotate edges with parameters -------------
# For any node ending in '-u' attach the corresponding param of the letters before 'u'

# Create a mapping from node names to parameters
param_mapping <- setNames(best_destination_params[names(best_destination)], names(best_destination))

# Add parameter labels to tidy_dagP data
tidy_dagD$data$param_label <- ifelse(
  grepl("u$", tidy_dagD$data$name),
  as.character(round(param_mapping[sub("u$", "", tidy_dagD$data$name)], 3)),
  ""
)

dd <- ggdag(tidy_dagD) + 
  geom_dag_edges(aes(edge_color = edge_color)) +
  scale_color_identity() +
  geom_dag_text(aes(label = param_label), nudge_y = -0.5, color = "blue", size = 3)

# SAVE PLOTS
# save dd
ggsave(here("Exp1Prediction", "Model", "Figures", "best_destination_model.pdf"), dd, width = 6, height = 4.5)
# save dp
ggsave(here("Exp1Prediction", "Model", "Figures", "best_path_model.pdf"), dp, width = 6, height = 4.5)


