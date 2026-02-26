suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Experiment F: Domain Separation Validation (Proposition 1) ───
##
## Tests that when the marketplace operator earns only fixed
## per-transaction fees (φ), VCG becomes credible because any
## deviation reducing allocation reduces fee income.
##
## Compares:
##   1. Stake operator: operator has ownership stake in resources
##      (incentive to inflate prices / reduce allocation)
##   2. Separated operator: operator earns fixed fee per transaction
##      (deviation unprofitable by Proposition 1)
##
## Varies operator strategies to show that domain separation
## neutralises all deviation types.

expF_design <- function(
  topologies     = c("tree", "sp", "entangled"),
  load_levels    = c(low = 0.5, medium = 1.0, high = 1.5),
  operator_types = c("misreporter", "inflator", "ghost_bidder"),
  fee_modes      = c("stake", "separated")
) {
  conditions <- expand.grid(
    dag_type      = topologies,
    load_name     = names(load_levels),
    operator_type = operator_types,
    fee_mode      = fee_modes,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(
      load_level   = load_levels[load_name],
      condition_id = row_number()
    )

  conditions
}


expF_run_single <- function(condition, n_rounds, n_agents, seed) {
  # Under "separated" mode, use exchange credibility (which forces
  # operator surplus to 0). Under "stake" mode, no credibility enforcement.
  cred_type <- if (condition$fee_mode == "separated") "exchange" else "none"

  run_simulation(
    n_rounds         = n_rounds,
    n_agents         = n_agents,
    dag_type         = condition$dag_type,
    load_level       = condition$load_level,
    operator_type    = condition$operator_type,
    credibility_type = cred_type,
    seed             = seed
  ) %>%
    mutate(fee_mode = condition$fee_mode)
}


expF_run_all <- function(conditions, n_rounds, n_agents, n_seeds) {
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expF_run_single(cond, n_rounds, n_agents, seed = s) %>%
        mutate(condition_id = cond$condition_id,
               load_name    = cond$load_name)
    })
  })
  results
}


expF_aggregate <- function(results) {
  # Truthful baseline: separated mode (exchange) results serve as baseline
  baseline <- results %>%
    filter(fee_mode == "separated") %>%
    group_by(dag_type, load_level, operator_type, seed) %>%
    summarise(
      baseline_welfare = mean(welfare, na.rm = TRUE),
      .groups = "drop"
    )

  summary <- results %>%
    group_by(dag_type, load_level, load_name, operator_type,
             fee_mode, seed) %>%
    summarise(
      mean_welfare    = mean(welfare, na.rm = TRUE),
      mean_drop_rate  = mean(drop_rate, na.rm = TRUE),
      mean_op_surplus = mean(operator_surplus, na.rm = TRUE),
      mean_net_surplus = mean(net_op_surplus, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(baseline,
              by = c("dag_type", "load_level", "operator_type", "seed")) %>%
    mutate(
      welfare_ratio    = mean_welfare / baseline_welfare,
      surplus_extracted = mean_net_surplus
    ) %>%
    group_by(dag_type, load_name, operator_type, fee_mode) %>%
    summarise(
      welfare_ratio_mean  = mean(welfare_ratio, na.rm = TRUE),
      welfare_ratio_ci    = list(boot_ci(welfare_ratio)),
      surplus_mean        = mean(surplus_extracted, na.rm = TRUE),
      surplus_ci          = list(boot_ci(surplus_extracted)),
      drop_rate_mean      = mean(mean_drop_rate, na.rm = TRUE),
      n_obs               = n(),
      .groups = "drop"
    ) %>%
    mutate(
      welfare_ratio_ci_lo = map_dbl(welfare_ratio_ci, ~ .x["lo"]),
      welfare_ratio_ci_hi = map_dbl(welfare_ratio_ci, ~ .x["hi"]),
      surplus_ci_lo       = map_dbl(surplus_ci, ~ .x["lo"]),
      surplus_ci_hi       = map_dbl(surplus_ci, ~ .x["hi"])
    ) %>%
    select(-welfare_ratio_ci, -surplus_ci)

  summary
}
