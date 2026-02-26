suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Experiment J: Credibility--Competition Interaction ────────────
##
## Tests how credibility mechanisms interact with integrator competition.
## Fills the C x K cell of the ablation matrix.
##
## Research question:
##   Does competition substitute for credibility, complement it, or
##   are the effects independent?
##
## Design:
##   credibility_type x k_integrators x topology (load = medium, fixed)
##   Operator: ghost_bidder (the trilemma-relevant deviation)
##
## Expected findings (theory-driven):
##   - Competition alone (none, k>1): reduces markup but ghost-bid
##     remains profitable (competition disciplines pricing, not credibility)
##   - Credibility alone (broadcast, k=1): deters ghost-bid even under
##     monopoly
##   - C x K interaction: super-additive surplus reduction (competition
##     reduces deviation gains while credibility increases deviation cost)

expJ_design <- function(
  topologies  = c("tree", "sp", "entangled"),
  cred_types  = c("none", "broadcast", "blockchain", "exchange"),
  k_values    = c(1, 2, 3, 5)
) {
  conditions <- expand.grid(
    dag_type    = topologies,
    credibility = cred_types,
    k           = k_values,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(condition_id = row_number())

  conditions
}


expJ_run_single <- function(condition, n_rounds, n_agents, seed) {
  run_simulation(
    n_rounds          = n_rounds,
    n_agents          = n_agents,
    dag_type          = condition$dag_type,
    load_level        = 1.0,  # medium load, fixed
    operator_type     = "ghost_bidder",
    credibility_type  = condition$credibility,
    integrator_k      = condition$k,
    integrator_strategy = "competitive",
    seed              = seed
  )
}


expJ_run_all <- function(conditions, n_rounds, n_agents, n_seeds) {
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expJ_run_single(cond, n_rounds, n_agents, seed = s) %>%
        mutate(condition_id = cond$condition_id)
    })
  })
  results
}


expJ_aggregate <- function(results) {
  # Baseline: broadcast + k=5 (best credibility + full competition)
  baseline <- results %>%
    filter(credibility == "broadcast", k_integrators == 5) %>%
    group_by(dag_type, seed) %>%
    summarise(
      baseline_welfare = mean(welfare, na.rm = TRUE),
      .groups = "drop"
    )

  # Per-seed aggregation
  per_seed <- results %>%
    group_by(dag_type, credibility, k_integrators, seed) %>%
    summarise(
      mean_welfare     = mean(welfare, na.rm = TRUE),
      mean_op_surplus  = mean(operator_surplus, na.rm = TRUE),
      mean_net_surplus = mean(net_op_surplus, na.rm = TRUE),
      mean_slice_adj   = mean(slice_price_adj, na.rm = TRUE),
      detection_rate   = mean(penalty_applied > 0, na.rm = TRUE),
      mean_drop_rate   = mean(drop_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(baseline, by = c("dag_type", "seed")) %>%
    mutate(
      welfare_ratio   = mean_welfare / baseline_welfare,
      profitable      = (mean_net_surplus > 0)
    )

  # Summary across seeds
  agg <- per_seed %>%
    group_by(dag_type, credibility, k_integrators) %>%
    summarise(
      welfare_ratio_mean   = mean(welfare_ratio, na.rm = TRUE),
      welfare_ratio_ci     = list(boot_ci(welfare_ratio)),
      net_surplus_mean     = mean(mean_net_surplus, na.rm = TRUE),
      net_surplus_ci       = list(boot_ci(mean_net_surplus)),
      op_surplus_mean      = mean(mean_op_surplus, na.rm = TRUE),
      slice_adj_mean       = mean(mean_slice_adj, na.rm = TRUE),
      detection_rate_mean  = mean(detection_rate, na.rm = TRUE),
      pct_profitable       = mean(profitable, na.rm = TRUE),
      drop_rate_mean       = mean(mean_drop_rate, na.rm = TRUE),
      n_obs                = n(),
      .groups = "drop"
    ) %>%
    mutate(
      welfare_ratio_ci_lo = map_dbl(welfare_ratio_ci, ~ .x["lo"]),
      welfare_ratio_ci_hi = map_dbl(welfare_ratio_ci, ~ .x["hi"]),
      net_surplus_ci_lo   = map_dbl(net_surplus_ci, ~ .x["lo"]),
      net_surplus_ci_hi   = map_dbl(net_surplus_ci, ~ .x["hi"])
    ) %>%
    select(-welfare_ratio_ci, -net_surplus_ci)

  agg
}
