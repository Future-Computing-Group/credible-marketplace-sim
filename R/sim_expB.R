suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Experiment B: Credibility mechanisms comparison ───────────────
##
## Varies: credibility mechanism × operator strategy × DAG topology × load
## Now includes ghost_bidder to test trilemma directly
## Key metrics: welfare recovery (vs exchange baseline), latency overhead,
##              price volatility, detection rate of deviations

expB_design <- function(
  topologies      = c("tree", "sp", "entangled"),
  load_levels     = c(low = 0.5, medium = 1.0, high = 1.5),
  operator_types  = c("misreporter", "inflator", "ghost_bidder"),
  cred_types      = c("none", "broadcast", "blockchain", "exchange", "regulatory")
) {
  conditions <- expand.grid(
    dag_type        = topologies,
    load_name       = names(load_levels),
    operator_type   = operator_types,
    credibility     = cred_types,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(
      load_level   = load_levels[load_name],
      condition_id = row_number()
    )

  conditions
}


expB_run_single <- function(condition, n_rounds, n_agents, seed) {
  run_simulation(
    n_rounds         = n_rounds,
    n_agents         = n_agents,
    dag_type         = condition$dag_type,
    load_level       = condition$load_level,
    operator_type    = condition$operator_type,
    credibility_type = condition$credibility,
    seed             = seed
  )
}


expB_run_all <- function(conditions, n_rounds, n_agents, n_seeds) {
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expB_run_single(cond, n_rounds, n_agents, seed = s) %>%
        mutate(condition_id = cond$condition_id,
               load_name    = cond$load_name)
    })
  })
  results
}


expB_aggregate <- function(results) {
  # Baseline: exchange mechanism (prevents deviation entirely)
  exchange_baseline <- results %>%
    filter(credibility == "exchange") %>%
    group_by(dag_type, load_level, operator_type, seed) %>%
    summarise(
      exchange_welfare = mean(welfare, na.rm = TRUE),
      .groups = "drop"
    )

  summary <- results %>%
    group_by(dag_type, load_level, load_name, operator_type,
             credibility, seed) %>%
    summarise(
      mean_welfare     = mean(welfare, na.rm = TRUE),
      mean_drop_rate   = mean(drop_rate, na.rm = TRUE),
      mean_penalty     = mean(penalty_applied, na.rm = TRUE),
      mean_latency_oh  = mean(latency_overhead, na.rm = TRUE),
      mean_net_surplus = mean(net_op_surplus, na.rm = TRUE),
      price_volatility = mean(price_sd, na.rm = TRUE),
      detection_rate   = mean(penalty_applied > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(exchange_baseline,
              by = c("dag_type", "load_level", "operator_type", "seed")) %>%
    mutate(
      welfare_recovery = mean_welfare / exchange_welfare
    ) %>%
    group_by(dag_type, load_name, operator_type, credibility) %>%
    summarise(
      welfare_recovery_mean = mean(welfare_recovery, na.rm = TRUE),
      welfare_recovery_sd   = sd(welfare_recovery, na.rm = TRUE),
      welfare_recovery_ci   = list(boot_ci(welfare_recovery)),
      latency_oh_mean       = mean(mean_latency_oh, na.rm = TRUE),
      price_vol_mean        = mean(price_volatility, na.rm = TRUE),
      detection_rate_mean   = mean(detection_rate, na.rm = TRUE),
      net_surplus_mean      = mean(mean_net_surplus, na.rm = TRUE),
      n_obs                 = n(),
      .groups = "drop"
    ) %>%
    mutate(
      welfare_recovery_ci_lo = map_dbl(welfare_recovery_ci, ~ .x["lo"]),
      welfare_recovery_ci_hi = map_dbl(welfare_recovery_ci, ~ .x["hi"])
    ) %>%
    select(-welfare_recovery_ci)

  summary
}
