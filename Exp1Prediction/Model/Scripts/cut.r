# ---------- NOTES AND CUT MATERIAL --------------


# from get_formula function
# if (length(vars) > 1) {
#   for (i in 1:(length(vars) - 1)) {
#     from_var <- vars[i]
#     to_var <- vars[i + 1]
#     formula <- paste(to_var, "~", from_var)
#     formulas <- c(formulas, list(formula))
#   }
# }


get_formulas <- function(causevector) {
  state_names = names(causevector)
  vars <- state_names[causevector != 0]
  singles <- vars[nchar(vars) == 1]
  interactions <- vars[nchar(vars) == 2]
  formulas <- list()
  # This next 'if' section gets the edges between single vars and interaction vars 
  # If there is at least one interaction var and single vars, then for each interaction var have a formula where the interaction var predicts its constituent single vars
  if (length(interactions) > 0 & length(singles) > 0)
  {
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
  vars_u <- paste0(vars, "u")
  # Anything in causevector, plus its unobserved counterpart, goes to Path
  all_vars <- c(vars, vars_u)
  varsforform <- paste(all_vars, collapse = " + ")
  # Remove the first trailing plus sig
  # Change this manually between 'Path' and 'Destination' for now
  formula1 <- paste("Destination ~", varsforform)
  formulas <- c(list(formula1), formulas)
  return(formulas)
}

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