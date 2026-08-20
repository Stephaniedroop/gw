# ------------------------------------------
# ------- Function to get lesioned models -------
# ---- Like s_hat with tau1 in the collider project -------
# ------------------------------------------

get_lesions <- function(d, tau1) {
  nodes <- unique(sub("=.*", "", d$node3))
  all_node3 <- paste0(rep(nodes, each = 2), "=", 0:1)

  d |>
    group_by(condition, uvars) |>
    mutate(
      s_hat_ces = {
        z <- (ces - max(ces)) / tau1
        exp(z) / sum(exp(z))
      },
      s_hat_noSelect = 1 / n()
    ) |>
    group_by(condition, sem, node3) |>
    summarise(
      pr = sum(prior),
      post = sum(posterior),
      postces = sum(posterior * s_hat_ces),
      postns = sum(posterior * s_hat_noSelect),
      noInf = sum(prior * s_hat_ces),
      noInf_ns = sum(prior * s_hat_noSelect),
      .groups = "drop"
    ) |>
    group_by(condition) |>
    complete(
      node3 = all_node3,
      fill = list(
        pr = 0,
        post = 0,
        postces = 0,
        postns = 0,
        noInf = 0,
        noInf_ns = 0
      )
    ) |>
    ungroup()
}
