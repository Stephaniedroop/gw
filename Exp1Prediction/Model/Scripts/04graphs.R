##########################################################################################
#  Plotting best-fitting causal model
##########################################################################################

library(here)

library(dagitty)
library(ggplot2)
library(ggdag)

# Load the target distribution and the model prediction for each of the 16 situations
load(here('Exp1Prediction', 'Model', 'Data', 'model.Rda'))  

# the dependencies between path and length are too subtle to bother with, so we are going to model path x destination
# and this fits our pattern well enough
# the next task is to compute cesm for our situations of interest!


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

  all_terms <- c(vars, paste0(vars, "u"))
  varsforform <- paste(all_terms, collapse = " + ")
  
  # Use substitute to access the argument name used in the function call: Path or Destination. deparse returns the string otherwise substitute would return the object
  obj_name <- deparse(substitute(causevector))
  # Split by underscore and get the second part ('[[1]]' is on the face of it not used but is equivalent to unlist, needed because output is a list)
  part2 <- strsplit(obj_name, "_")[[1]][2]
  # Capitalize the extracted part by taking first letter to upper and rest to lower
  extractedname <- paste0(toupper(substr(part2, 1, 1)), tolower(substr(part2, 2, nchar(part2))))
  
  # OR if want to use package stringr - if we use elsewhere then can use here too
  #str_to_title("path")
  
  formula1 <- paste(extractedname, "~", varsforform)

  formulas <- c(list(formula1), formulas)
  return(formulas)
}


# Path
formulaP <- get_formulas(best_path)
print(formulaP) 
# Food - renamed from Destination
formulaF <- get_formulas(best_food)
print(formulaF)  

# It returns a list of character strings, not actual formulas, so turn it to formulas
formula_listP <- lapply(formulaP, as.formula)
formula_listF <- lapply(formulaF, as.formula)


#------------- PATH -------------------
dagP <- do.call(dagify, formula_listP)
# Set coordinates immediately after creating `dagP`
coordinates(dagP) <- list(
  x = c(K = 0, Ku = -0.5, KS = 1, KSu = 1.5, Path = 0.5),
  y = c(K = 1.75, Ku = 1.5, KS = 1.75, KSu = 1.5, Path = 0.5)
)
tidy_dagP <- tidy_dagitty(dagP)
#ggdag(tidy_dagP) #+ remove_axes() # try with axes to get the coords first then remove


# ------ Color the edges -------

# ------ Test for path ------------
# Create a mapping of edge colors based on signs
Pedge_colors <- data.frame(
  name = names(best_path)[best_path != 0],
  sign = best_path[best_path != 0]
)
# Add the corresponding 'u' variables
Pedge_colors_u <- data.frame(
  name = paste0(Pedge_colors$name, "u"),
  sign = Pedge_colors$sign
)

Pall_edge_colors <- rbind(Pedge_colors, Pedge_colors_u)

# Now map these colors to the edges in tidy_dagP - use two $ to get into the nested layers
tidy_dagP$data$edge_color <- 
  ifelse(tidy_dagP$data$name %in% 
           Pall_edge_colors$name[Pall_edge_colors$sign == 1], "limegreen",
                                    ifelse(tidy_dagP$data$name %in% 
                                             Pall_edge_colors$name[Pall_edge_colors$sign == -1], "red", "black"))

#Other green options include: "darkgreen", "green", "forestgreen", "seagreen", "mediumseagreen", "springgreen", "limegreen", "chartreuse".

# I wanted to change node colors (darkgrey for observed, lightgrey for unobserved) but couldn't get it to work
#tidy_dagP$data$node_color <- ifelse(grepl("u$", tidy_dagP$data$name), "lightgrey", "darkgrey")


#------- Initial stab at annotate edges with parameters -------------
# For any node ending in '-u' attach the corresponding param of the letters before 'u'

# Create a mapping from node names to parameters
#param_mapping <- setNames(best_path_params[names(best_path)], names(best_path))



# Also use the parameter values to set edge widths, so stronger parameters have thicker edges
Pparam_mapping <- setNames(best_path_params[names(best_path)], names(best_path))
Pparam_mapping_u <- setNames(Pparam_mapping, paste0(names(Pparam_mapping), "u"))
Pall_param_mapping <- c(Pparam_mapping, Pparam_mapping_u)

tidy_dagP$data$edge_width <- ifelse(tidy_dagP$data$name %in% names(Pall_param_mapping),
                                    abs(Pall_param_mapping[tidy_dagP$data$name]), 0.5)
tidy_dagP$data$edge_width <- scales::rescale(tidy_dagP$data$edge_width, to = c(0.5, 4))

# Add parameter labels to tidy_dagP data
tidy_dagP$data$param_label <- ifelse(
  grepl("u$", tidy_dagP$data$name),
  sub("^(-?)0\\.", ".", sprintf("%.2g", Pparam_mapping[sub("u$", "", tidy_dagP$data$name)])),
  ""
)

dp <- ggdag(tidy_dagP) + 
  geom_dag_edges(aes(edge_color = edge_color, edge_width = edge_width)) +
  scale_color_identity() +
  geom_dag_text(aes(label = param_label), nudge_y = -0.2, color = "blue", size = 5) +
  theme_void()

dp


#-------------  FOOD ------------------
dagF <- do.call(dagify, formula_listF)
# Set coordinates immediately after creating `dagF`
coordinates(dagF) <- list(
    x = c(P = -1.5, Pu = -1.5, PS = -0.5, PSu = 0.25, KS = 1, KSu = 1.25, CS = 2, CSu = 2.25, Food = 0.5),
    y = c(P = 1.5, Pu = 1, PS = 2, PSu = 1.75, KS = 1.5, KSu = 1.25, CS = 1.5, CSu = 1.25, Food = 0.5)
)
tidy_dagF <- tidy_dagitty(dagF)

# Create a mapping of edge colors based on signs
Fedge_colors <- data.frame(
  name = names(best_food)[best_food != 0],
  sign = best_food[best_food != 0]
)
# Add the corresponding 'u' variables
Fedge_colors_u <- data.frame(
  name = paste0(Fedge_colors$name, "u"),
  sign = Fedge_colors$sign
)

Fall_edge_colors <- rbind(Fedge_colors, Fedge_colors_u)

# Now map these colors to the edges in tidy_dagF - use two $ to get into the nested layers
tidy_dagF$data$edge_color <- 
  ifelse(tidy_dagF$data$name %in% 
           Fall_edge_colors$name[Fall_edge_colors$sign == 1], "limegreen",
         ifelse(tidy_dagF$data$name %in% 
                  Fall_edge_colors$name[Fall_edge_colors$sign == -1], "red", "black"))


#------- Initial stab at annotate edges with parameters -------------
# For any node ending in '-u' attach the corresponding param of the letters before 'u'

# Also use the parameter values to set edge widths, so stronger parameters have thicker edges
Fparam_mapping <- setNames(best_food_params[names(best_food)], names(best_food))
Fparam_mapping_u <- setNames(Fparam_mapping, paste0(names(Fparam_mapping), "u"))
Fall_param_mapping <- c(Fparam_mapping, Fparam_mapping_u)

tidy_dagF$data$edge_width <- ifelse(tidy_dagF$data$name %in% names(Fall_param_mapping),
                                    abs(Fall_param_mapping[tidy_dagF$data$name]), 0.5)

tidy_dagF$data$edge_width <- scales::rescale(tidy_dagF$data$edge_width, to = c(0.5, 4))



# Add parameter labels to tidy_dagD data, and make it the observed param's label added to the unobserved var, and remove trailing 0
tidy_dagF$data$param_label <- ifelse(
  grepl("u$", tidy_dagF$data$name),
  sub("^(-?)0\\.", ".", sprintf("%.2g", Fparam_mapping[sub("u$", "", tidy_dagF$data$name)])),
  ""
)


fd <- ggdag(tidy_dagF) + 
  geom_dag_edges(aes(edge_color = edge_color, edge_width = edge_width)) +
  scale_color_identity() +
  geom_dag_text(aes(label = param_label), nudge_x = 0.2, nudge_y = 0.2, color = "blue", size = 5) +
  theme_void()

fd


# SAVE PLOTS
# save fd
ggsave(here("Exp1Prediction", "Model", "Figures", "best_food_model.pdf"), fd, width = 6, height = 4.5)
# save dp
ggsave(here("Exp1Prediction", "Model", "Figures", "best_path_model.pdf"), dp, width = 6, height = 4.5)


