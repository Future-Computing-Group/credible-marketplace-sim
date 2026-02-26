suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Experiment G: Sensitivity Analysis ───────────────────────────
##
## Varies key parameters to assess robustness:
##   1. Penalty multiplier (blockchain/regulatory)
##   2. Tatonnement learning rate (eta)
##   3. Task deadline distribution
##   4. Audit delay (tau_audit for blockchain)
##
## Uses medium load, tree topology as baseline; shows that main
## results are robust to parameter choices.

expG_design <- function() {
  # Penalty sensitivity
  penalty_conds <- expand.grid(
    param_name  = "penalty",
    param_value = c(2, 5, 10, 20, 50),
    dag_type    = c("tree", "entangled"),
    stringsAsFactors = FALSE
  )

  # Learning rate sensitivity
  eta_conds <- expand.grid(
    param_name  = "eta",
    param_value = c(0.01, 0.05, 0.1, 0.2, 0.5),
    dag_type    = c("tree", "entangled"),
    stringsAsFactors = FALSE
  )

  # Audit delay sensitivity
  tau_conds <- expand.grid(
    param_name  = "tau_audit",
    param_value = c(1, 2, 5, 10, 20),
    dag_type    = c("tree", "entangled"),
    stringsAsFactors = FALSE
  )

  # Regulatory detection probability sensitivity
  pdet_conds <- expand.grid(
    param_name  = "p_detect",
    param_value = c(0.1, 0.3, 0.5, 0.7, 0.9),
    dag_type    = c("tree", "entangled"),
    stringsAsFactors = FALSE
  )

  conditions <- bind_rows(penalty_conds, eta_conds, tau_conds, pdet_conds) %>%
    as_tibble() %>%
    mutate(condition_id = row_number())

  conditions
}


expG_run_single <- function(condition, n_rounds, n_agents, seed) {
  # Base: inflator operator, medium load
  cred_type <- switch(condition$param_name,
    penalty   = "regulatory",
    eta       = "none",
    tau_audit = "blockchain",
    p_detect  = "regulatory",
    "none"
  )

  cred_params <- list()
  eta_val <- 0.1

  switch(condition$param_name,
    penalty = {
      cred_params$penalty <- condition$param_value
    },
    eta = {
      eta_val <- condition$param_value
    },
    tau_audit = {
      cred_params$tau_audit <- condition$param_value
    },
    p_detect = {
      cred_params$p_detect <- condition$param_value
    }
  )

  run_simulation(
    n_rounds         = n_rounds,
    n_agents         = n_agents,
    dag_type         = condition$dag_type,
    load_level       = 1.0,
    operator_type    = "inflator",
    operator_params  = list(mu_markup = 0.2),
    credibility_type = cred_type,
    credibility_params = cred_params,
    seed             = seed,
    eta              = eta_val
  ) %>%
    mutate(
      param_name  = condition$param_name,
      param_value = condition$param_value
    )
}


expG_run_all <- function(conditions, n_rounds, n_agents, n_seeds) {
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expG_run_single(cond, n_rounds, n_agents, seed = s) %>%
        mutate(condition_id = cond$condition_id)
    })
  })
  results
}


expG_aggregate <- function(results) {
  summary <- results %>%
    group_by(dag_type, param_name, param_value, seed) %>%
    summarise(
      mean_welfare     = mean(welfare, na.rm = TRUE),
      mean_drop_rate   = mean(drop_rate, na.rm = TRUE),
      mean_net_surplus = mean(net_op_surplus, na.rm = TRUE),
      detection_rate   = mean(penalty_applied > 0, na.rm = TRUE),
      price_volatility = mean(price_sd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(dag_type, param_name, param_value) %>%
    summarise(
      welfare_mean    = mean(mean_welfare, na.rm = TRUE),
      welfare_ci      = list(boot_ci(mean_welfare)),
      surplus_mean    = mean(mean_net_surplus, na.rm = TRUE),
      detection_mean  = mean(detection_rate, na.rm = TRUE),
      price_vol_mean  = mean(price_volatility, na.rm = TRUE),
      drop_rate_mean  = mean(mean_drop_rate, na.rm = TRUE),
      n_obs           = n(),
      .groups = "drop"
    ) %>%
    mutate(
      welfare_ci_lo = map_dbl(welfare_ci, ~ .x["lo"]),
      welfare_ci_hi = map_dbl(welfare_ci, ~ .x["hi"])
    ) %>%
    select(-welfare_ci)

  summary
}
