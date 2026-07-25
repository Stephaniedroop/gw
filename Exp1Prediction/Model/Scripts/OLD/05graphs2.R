##########################################################################################
#  Plotting best-fitting causal model
##########################################################################################

library(here)

library(dagitty)
library(ggplot2)
library(ggraph)
library(ggdag)
library(ggforce)

# Load the target distribution and the model prediction for each of the 16 situations
load(here('Exp1Prediction', 'Model', 'Data', 'model.Rda'))

# A TOTAL MESS BECAUSE:
# I CHANGED TO GGPLOT, GOT MOST OF THE WAY WITH PATH, BUT THEN COULDNT GET EDGE ARROW HEADS
# TRIED GGRAPH AND GGFORCE??? BUT BAD ALL OVER

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
  singles <- vars[nchar(vars) == 1]
  interactions <- vars[nchar(vars) == 2]

  formulas <- list()

  # ----- ORIGINAL interaction edges (RESTORED) -----
  if (length(interactions) > 0 & length(singles) > 0) {
    for (int_var in interactions) {
      constituents <- unlist(strsplit(int_var, split = ""))
      for (constituent in constituents) {
        if (constituent %in% singles) {
          formulas <- c(
            formulas,
            list(
              paste(int_var, "~", constituent)
            )
          )
        }
      }
    }
  }

  # ----- outcome name -----
  obj_name <- deparse(substitute(causevector))
  part2 <- strsplit(obj_name, "_")[[1]][2]
  outcome <- paste0(
    toupper(substr(part2, 1, 1)),
    tolower(substr(part2, 2, nchar(part2)))
  )

  # ----- AND nodes -----
  and_nodes <- paste0(vars, "_and_", vars, "u")

  # AND-node formulas: X_and_Xu ~ X + Xu
  and_formulas <- mapply(
    function(x, xu, andnode) {
      paste(andnode, "~", x, "+", xu)
    },
    vars,
    paste0(vars, "u"),
    and_nodes,
    SIMPLIFY = FALSE
  )

  # Outcome depends on AND nodes
  outcome_formula <- paste(
    outcome,
    "~",
    paste(and_nodes, collapse = " + ")
  )

  formulas <- c(list(outcome_formula), formulas, and_formulas)
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
  x = c(
    K = 0,
    Ku = -0.5,
    KS = 1,
    KSu = 1.5,
    K_and_Ku = -0.25,
    KS_and_KSu = 1.25,
    Path = 0.5
  ),
  y = c(
    K = 1.75,
    Ku = 1.5,
    KS = 1.75,
    KSu = 1.5,
    K_and_Ku = 1.25,
    KS_and_KSu = 1.25,
    Path = 0.5
  )
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
  ifelse(
    tidy_dagP$data$name %in%
      Pall_edge_colors$name[Pall_edge_colors$sign == 1],
    "limegreen",
    ifelse(
      tidy_dagP$data$name %in%
        Pall_edge_colors$name[Pall_edge_colors$sign == -1],
      "red",
      "black"
    )
  )


# testdp <- ggraph(tidy_dagP) +  # Or 'graphopt', 'nicely'
#   geom_edge_link(
#     arrow = arrow(length = unit(4, 'mm'), type = 'closed'),
#     end_cap = circle(3, 'mm')  # Adjust to node size/2
#   ) +
#   geom_node_point(size = 6, color = 'steelblue') +  # Nodes ~6mm diameter
#   geom_node_text(aes(label = name), repel = TRUE) +
#   theme_void()

#testdp

# tidy_dagP$data$node_size <- ifelse(
#   grepl("_and_", tidy_dagP$data$name),
#   5,
#   15
# )

# tidy_dagP$data$node_color <- ifelse(
#   grepl("_and_", tidy_dagP$data$name),
#   "grey95",
#   "grey50"
# )

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

tidy_dagP$data$edge_width <- ifelse(
  tidy_dagP$data$name %in% names(Pall_param_mapping),
  abs(Pall_param_mapping[tidy_dagP$data$name]),
  0.5
)
tidy_dagP$data$edge_width <- scales::rescale(
  tidy_dagP$data$edge_width,
  to = c(0.5, 4) # or try to = c(1.0, 6.0) ??
)

# Add parameter labels to tidy_dagP data
tidy_dagP$data$param_label <- ifelse(
  grepl("u$", tidy_dagP$data$name),
  sub(
    "^(-?)0\\.",
    ".",
    sprintf("%.2g", Pparam_mapping[sub("u$", "", tidy_dagP$data$name)])
  ),
  ""
)


tidy_dagP$data$node_size <- with(
  tidy_dagP$data,
  ifelse(
    grepl("_and_", name),
    8, # conjunction nodes
    ifelse(
      grepl("u$", name),
      30, # noise nodes
      30
    )
  ) # observed nodes
)


tidy_dagP$data$node_fill <- ifelse(
  grepl("_and_", tidy_dagP$data$name),
  "grey75",
  ifelse(
    grepl("u$", tidy_dagP$data$name),
    "grey95", # unobserved noise nodes
    "grey50" # observed nodes
  )
)

coordinates(dagP)$x["K_and_Ku"] <- mean(coordinates(dagP)$x[c("K", "Ku")])
coordinates(dagP)$y["K_and_Ku"] <- coordinates(dagP)$y["K"] - 0.3

coordinates(dagP)$x["KS_and_KSu"] <- mean(coordinates(dagP)$x[c("KS", "KSu")])
coordinates(dagP)$y["KS_and_KSu"] <- coordinates(dagP)$y["KS"] - 0.3


testdp <- ggraph(tidy_dagP, layout = 'grid') +

  # ---- edges ----
  geom_edge_link(
    arrow = arrow(length = unit(4, 'mm'), type = 'closed'),
    end_cap = circle(3, 'mm') # Adjust to node size/2
  ) +
  # geom_segment(
  #   data = tidy_dagP$data[!is.na(tidy_dagP$data$xend), ],
  #   aes(
  #     x = x,
  #     y = y,
  #     xend = xend,
  #     yend = yend,
  #     color = edge_color,
  #     size = edge_width
  #   ),
  #   arrow = arrow(length = unit(6, "pt")),
  #   lineend = "round"
  # ) +

  # geom_link2(
  #   data = tidy_dagP$data[!is.na(tidy_dagP$data$xend), ],
  #   aes(
  #     x = x,
  #     y = y,
  #     xend = xend,
  #     yend = yend,
  #     color = edge_color,
  #     linewidth = edge_width
  #   ),
  #   arrow = arrow(length = unit(6, "pt"), type = "closed"),
  #   #end_cap = ggforce:::circle(radius = unit(5, "pt")),
  #   lineend = "round"
  # ) +

  # ---- nodes ----
  geom_point(
    data = tidy_dagP$data,
    aes(x = x, y = y, size = node_size, fill = node_fill),
    shape = 21,
    color = "black",
    stroke = 0.8
  ) +

  # ---- node labels ----
  # geom_text(
  #   data = tidy_dagP$data,
  #   aes(x = x, y = y, label = name),
  #   size = 5
  # ) +
  geom_text(
    data = subset(tidy_dagP$data, !grepl("_and_", name)),
    aes(x = x, y = y, label = name),
    size = 4.5
  ) +

  # ---- parameter labels ----
  geom_text(
    data = tidy_dagP$data,
    aes(x = x, y = y, label = param_label),
    nudge_y = -0.25,
    color = "blue",
    size = 5
  ) +

  scale_color_identity() +
  scale_fill_identity() +
  scale_size_identity() +
  theme_void()

testdp


dp <- ggplot() +

  # ---- edges ----
  geom_segment(
    data = tidy_dagP$data[!is.na(tidy_dagP$data$xend), ],
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      color = edge_color,
      size = edge_width
    ),
    arrow = arrow(length = unit(6, "pt")),
    lineend = "round"
  ) +

  # geom_link2(
  #   data = tidy_dagP$data[!is.na(tidy_dagP$data$xend), ],
  #   aes(
  #     x = x,
  #     y = y,
  #     xend = xend,
  #     yend = yend,
  #     color = edge_color,
  #     linewidth = edge_width
  #   ),
  #   arrow = arrow(length = unit(6, "pt"), type = "closed"),
  #   #end_cap = ggforce:::circle(radius = unit(5, "pt")),
  #   lineend = "round"
  # ) +

  # ---- nodes ----
  geom_point(
    data = tidy_dagP$data,
    aes(x = x, y = y, size = node_size, fill = node_fill),
    shape = 21,
    color = "black",
    stroke = 0.8
  ) +

  # ---- node labels ----
  # geom_text(
  #   data = tidy_dagP$data,
  #   aes(x = x, y = y, label = name),
  #   size = 5
  # ) +
  geom_text(
    data = subset(tidy_dagP$data, !grepl("_and_", name)),
    aes(x = x, y = y, label = name),
    size = 4.5
  ) +

  # ---- parameter labels ----
  geom_text(
    data = tidy_dagP$data,
    aes(x = x, y = y, label = param_label),
    nudge_y = -0.25,
    color = "blue",
    size = 5
  ) +

  scale_color_identity() +
  scale_fill_identity() +
  scale_size_identity() +
  theme_void()

dp


#-------------  FOOD ------------------
dagF <- do.call(dagify, formula_listF)
# Set coordinates immediately after creating `dagF`
coordinates(dagF) <- list(
  x = c(
    P = -1.5,
    Pu = -1.5,
    PS = -0.5,
    PSu = 0.25,
    KS = 1,
    KSu = 1.25,
    CS = 2,
    CSu = 2.25,
    Food = 0.5
  ),
  y = c(
    P = 1.5,
    Pu = 1,
    PS = 2,
    PSu = 1.75,
    KS = 1.5,
    KSu = 1.25,
    CS = 1.5,
    CSu = 1.25,
    Food = 0.5
  )
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
  ifelse(
    tidy_dagF$data$name %in%
      Fall_edge_colors$name[Fall_edge_colors$sign == 1],
    "limegreen",
    ifelse(
      tidy_dagF$data$name %in%
        Fall_edge_colors$name[Fall_edge_colors$sign == -1],
      "red",
      "black"
    )
  )


#------- Initial stab at annotate edges with parameters -------------
# For any node ending in '-u' attach the corresponding param of the letters before 'u'

# Also use the parameter values to set edge widths, so stronger parameters have thicker edges
Fparam_mapping <- setNames(best_food_params[names(best_food)], names(best_food))
Fparam_mapping_u <- setNames(Fparam_mapping, paste0(names(Fparam_mapping), "u"))
Fall_param_mapping <- c(Fparam_mapping, Fparam_mapping_u)

tidy_dagF$data$edge_width <- ifelse(
  tidy_dagF$data$name %in% names(Fall_param_mapping),
  abs(Fall_param_mapping[tidy_dagF$data$name]),
  0.5
)

tidy_dagF$data$edge_width <- scales::rescale(
  tidy_dagF$data$edge_width,
  to = c(0.5, 4)
)


# Add parameter labels to tidy_dagD data, and make it the observed param's label added to the unobserved var, and remove trailing 0
tidy_dagF$data$param_label <- ifelse(
  grepl("u$", tidy_dagF$data$name),
  sub(
    "^(-?)0\\.",
    ".",
    sprintf("%.2g", Fparam_mapping[sub("u$", "", tidy_dagF$data$name)])
  ),
  ""
)


fd <- ggdag(tidy_dagF) +
  geom_dag_edges(aes(edge_color = edge_color, edge_width = edge_width)) +
  scale_color_identity() +
  geom_dag_text(
    aes(label = param_label),
    nudge_x = 0.2,
    nudge_y = 0.2,
    color = "blue",
    size = 5
  ) +
  theme_void()

fd


# SAVE PLOTS
# save fd
ggsave(
  here("Exp1Prediction", "Model", "Figures", "best_food_model.pdf"),
  fd,
  width = 6,
  height = 4.5
)
# save dp
ggsave(
  here("Exp1Prediction", "Model", "Figures", "best_path_model.pdf"),
  dp,
  width = 6,
  height = 4.5
)
