suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Experiment L: Domain Separation Knife-Edge ────────────────────────
##
## Quantifies the knife-edge nature of Proposition 1 (domain separation).
## When the operator holds a pure fee-only incentive (stake_fraction = 0),
## no deviation from VCG is profitable.  Any positive ownership stake
## (stake_fraction > 0) breaks the guarantee and enables surplus extraction.
##
## Design:  stake_fraction × operator × topology
## Key tests:
##   1. stake_fraction = 0 → surplus = 0   (Prop 1 confirmed)
##   2. stake_fraction > 0 → surplus > 0   (knife-edge confirmed)
##   3. Surplus scales approximately linearly with stake_fraction

expL_design <- function(
  stake_fractions = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 1.0),
  operators       = c("ghost_bidder", "inflator", "misreporter"),
  topologies      = c("tree", "sp", "entangled")
) {
  conditions <- expand.grid(
    stake_fraction = stake_fractions,
    operator       = operators,
    dag_type       = topologies,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(condition_id = row_number())

  conditions
}


expL_run_single <- function(condition, n_rounds, seed) {
  run_simulation(
    n_rounds         = n_rounds,
    n_agents         = 40,
    dag_type         = condition$dag_type,
    load_level       = 1.0,
    operator_type    = condition$operator,
    credibility_type = "exchange",
    credibility_params = list(stake_fraction = condition$stake_fraction),
    seed             = seed
  ) %>%
    mutate(
      stake_fraction = condition$stake_fraction
    )
}


expL_run_all <- function(conditions, n_rounds = 100, n_seeds = 5) {
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expL_run_single(cond, n_rounds, seed = s) %>%
        mutate(condition_id = cond$condition_id)
    })
  })
  results
}


expL_aggregate <- function(results) {
  # Baseline: truthful operator, stake = 0
  # (Not needed for this experiment since we measure raw surplus,
  #  but compute for welfare ratio if desired.)

  per_seed <- results %>%
    group_by(dag_type, operator_type, stake_fraction, seed) %>%
    summarise(
      mean_welfare     = mean(welfare, na.rm = TRUE),
      mean_op_surplus  = mean(operator_surplus, na.rm = TRUE),
      mean_net_surplus = mean(net_op_surplus, na.rm = TRUE),
      .groups = "drop"
    )

  summary <- per_seed %>%
    group_by(dag_type, operator_type, stake_fraction) %>%
    summarise(
      surplus_mean     = mean(mean_net_surplus, na.rm = TRUE),
      surplus_ci       = list(boot_ci(mean_net_surplus)),
      gross_surplus    = mean(mean_op_surplus, na.rm = TRUE),
      pct_profitable   = mean(mean_net_surplus > 0, na.rm = TRUE) * 100,

      welfare_mean     = mean(mean_welfare, na.rm = TRUE),
      welfare_ci       = list(boot_ci(mean_welfare)),

      n_obs            = n(),
      .groups = "drop"
    ) %>%
    mutate(
      surplus_ci_lo  = map_dbl(surplus_ci, ~ .x["lo"]),
      surplus_ci_hi  = map_dbl(surplus_ci, ~ .x["hi"]),
      welfare_ci_lo  = map_dbl(welfare_ci, ~ .x["lo"]),
      welfare_ci_hi  = map_dbl(welfare_ci, ~ .x["hi"])
    ) %>%
    select(-surplus_ci, -welfare_ci)

  summary
}
