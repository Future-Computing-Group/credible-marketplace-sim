suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Experiment D: Two-tier architecture with heterogeneous trust ──
##
## Varies: Level 1 trust × Level 2 trust × topology × N agents
## Now models L2 operator as a separate agent with domain-separation
## credibility (Proposition 1): L2 operator earns fixed fees,
## so deviations are structurally unprofitable.
##
## Key metrics: end-to-end welfare, latency, price volatility,
##              compliance rate

expD_design <- function(
  topologies   = c("tree", "sp", "entangled"),
  n_agents_set = c(25, 50, 100),
  l1_trust     = c("none", "broadcast", "blockchain", "exchange"),
  l2_trust     = c("none", "broadcast", "exchange")
) {
  conditions <- expand.grid(
    dag_type  = topologies,
    n_agents  = n_agents_set,
    l1_trust  = l1_trust,
    l2_trust  = l2_trust,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(condition_id = row_number())

  conditions
}


expD_run_single <- function(condition, n_rounds, seed) {
  # Level 2 operator model: domain-separated, earns fixed fee.
  # Under domain separation (Proposition 1), the L2 operator has
  # no incentive to deviate because deviating reduces served tasks
  # and thus fee income.
  #
  # L2 strategic behavior: if l2_trust == "none", model a small
  # L2 operator deviation (mu_markup = 0.05, mild inefficiency).
  # For any other l2_trust, domain separation or credibility
  # mechanism prevents deviation.

  l2_has_deviation <- (condition$l2_trust == "none")
  l2_welfare_factor <- if (l2_has_deviation) 0.97 else 1.0
  l2_latency_overhead <- switch(condition$l2_trust,
    none       = 0,
    broadcast  = 0,
    exchange   = 0,
    blockchain = 3,
    0
  )

  # L1 operator: strategic inflator at Level 1
  sim <- run_simulation(
    n_rounds         = n_rounds,
    n_agents         = condition$n_agents,
    dag_type         = condition$dag_type,
    load_level       = 1.0,
    operator_type    = "inflator",
    operator_params  = list(mu_markup = 0.2),
    credibility_type = condition$l1_trust,
    seed             = seed
  )

  # Apply L2 effects
  sim <- sim %>%
    mutate(
      welfare          = welfare * l2_welfare_factor,
      latency_overhead = latency_overhead + l2_latency_overhead,
      l1_trust         = condition$l1_trust,
      l2_trust         = condition$l2_trust,
      n_agents_cond    = condition$n_agents,
      l2_deviation     = l2_has_deviation
    )

  sim
}


expD_run_all <- function(conditions, n_rounds, n_seeds) {
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expD_run_single(cond, n_rounds, seed = s) %>%
        mutate(condition_id = cond$condition_id)
    })
  })
  results
}


expD_aggregate <- function(results) {
  # Baseline: both tiers use exchange (no deviation possible)
  baseline <- results %>%
    filter(l1_trust == "exchange", l2_trust == "exchange") %>%
    group_by(dag_type, n_agents_cond, seed) %>%
    summarise(
      baseline_welfare = mean(welfare, na.rm = TRUE),
      .groups = "drop"
    )

  summary <- results %>%
    group_by(dag_type, n_agents_cond, l1_trust, l2_trust, seed) %>%
    summarise(
      mean_welfare     = mean(welfare, na.rm = TRUE),
      mean_drop_rate   = mean(drop_rate, na.rm = TRUE),
      mean_latency_oh  = mean(latency_overhead, na.rm = TRUE),
      price_volatility = mean(price_sd, na.rm = TRUE),
      compliance_rate  = 1 - mean(net_op_surplus > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(baseline,
              by = c("dag_type", "n_agents_cond", "seed")) %>%
    mutate(
      welfare_ratio = mean_welfare / baseline_welfare
    ) %>%
    group_by(dag_type, n_agents_cond, l1_trust, l2_trust) %>%
    summarise(
      welfare_ratio_mean  = mean(welfare_ratio, na.rm = TRUE),
      welfare_ratio_sd    = sd(welfare_ratio, na.rm = TRUE),
      welfare_ratio_ci    = list(boot_ci(welfare_ratio)),
      latency_oh_mean     = mean(mean_latency_oh, na.rm = TRUE),
      price_vol_mean      = mean(price_volatility, na.rm = TRUE),
      compliance_mean     = mean(compliance_rate, na.rm = TRUE),
      drop_rate_mean      = mean(mean_drop_rate, na.rm = TRUE),
      n_obs               = n(),
      .groups = "drop"
    ) %>%
    mutate(
      welfare_ratio_ci_lo = map_dbl(welfare_ratio_ci, ~ .x["lo"]),
      welfare_ratio_ci_hi = map_dbl(welfare_ratio_ci, ~ .x["hi"])
    ) %>%
    select(-welfare_ratio_ci)

  summary
}
