##########################################################################################
#  Plotting best-fitting causal model
##########################################################################################


rm(list = ls())
library(tidyverse)
library(ggdag)
library(dagitty)
library(igraph)
library(RColorBrewer)

# Load the target distribution and the model prediction for each of the 16 situations
load("../../Experiment/Data/model.Rda")  # Actually this is not saved yet; it will be but for now use 

df <- read.csv("../Data/model.csv")  # 16 of 6


# Got best_path manually just now: check the output from the model fitting later
# The heuristic is that everything that is 1 in best_path has an edge between it: 
# and the direction goes from the single variable to the interaction variable (earlier to later)
# Then bring in each of these variables' unobserved variables
# and put an edge from all of these to Path

best_path <- c(0, 0, 1, 0, 0, 1, 0, 0, 0, 0)  
state_names <- c("P", "K", "C", "S", "PK", "PC", "PS", "KC", "KS", "CS")

# A function to get the causal formulas to send for dagify 
# @param causevector A vector of 0s and 1s indicating which variables are included in the best fitting model
# @param state_names A vector of the names of the states corresponding to the causevector
get_formulas <- function(causevector, state_names) {
  vars <- state_names[causevector == 1]  # Get the names of the vars that are 1 in best_path
  # set up an empty list of formulas
  formulas <- list()
  # If there is more than 1 cause variables, add a formula between them, to represent an edge
  if (length(vars) > 1) {
    for (i in 1:(length(vars) - 1)) {
      from_var <- vars[i]
      to_var <- vars[i + 1]
      # set a formula to add to the formulas list of to_var being predicted by from_var
      formula <- paste(to_var, "~", from_var)
      # Add this formula to the list of formulas
      if (i == 1) {
        formulas <- list(formula)
      } else {
        formulas <- c(formulas, list(formula))
    }
  }
  vars_u <- paste0(vars, "u")  # Add 'u' to each of these names
  all_vars <- c(vars, vars_u)  # Combine the two sets of names
  varsforform <- paste(all_vars, collapse = " + ")  # Create a string with all names separated by ' + '
  formula1 <- paste("Path ~", varsforform)  # Create the first, path formula string
  formulas <- c(formula1, formulas)  # Combine into a list of formulas
  }
  return(formulas)
}

# Test the function get_formulas
formula <- get_formulas(best_path, state_names)
print(formula)  # "Path ~ C + PC + Cu + PCu" and "PC ~ C"

# It returns a list of character strings, not actual formulas, so turn it to formulas
formula_list <- lapply(formula, as.formula)

#------------ Using dagify, ggdag and dagitty -----------------

dag <- do.call(dagify, formula_list)
# Now analyse or plot this with ggdag or dagitty
tidy_dag <- tidy_dagitty(dag)
ggdag(tidy_dag)  # Then add ggplot2 layers for more customisation

#------- Test a made up dag for how to position nodes --------------
# This was just to see how to position nodes manually - DELETE LATER
# Define coordinates for each node
coord_dag <- list(
  x = c(P = 0, K = 1, C = 2, S = 3, Path = 2), 
  y = c(P = 2, K = 2, C = 1, S = 0, Path = 1))

# Create DAG with manual coordinates
my_dag <- dagify(
  K ~ P, 
  C ~ K, 
  S ~ C, 
  Path ~ C + S, 
  coords = coord_dag)

# Plot with ggdag
ggdag(my_dag) + ggplot2::theme_minimal()

# ------- Customise layout of our actual path dag -----------------
# This was by manual trial and error
coordinates(dag) <- list(
  x = c(C = -1, Cu = -1.8, PC = 0, PCu = 0.5, Path = -0.5), 
  y = c(C = 2, Cu = 1.5, PC = 1.25,
  PCu = 0.5, Path = -1))

ggdag(dag)

# Making it tidy has no later apparent effect on the spacing, but looks like we need it for setting the colours
tidy_dag <- tidy_dagitty(dag)
ggdag(tidy_dag) + remove_axes()

# It now looked ok enough to save for now. Still need to do the destination one
