suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## -- Experiment I: Imperfect Broadcast (Credibility Degradation) ----------
##
## Tests how credibility degrades when the broadcast channel is imperfect
## (messages delayed or lost).  Parameterises the broadcast detection
## probability rather than assuming perfect detection.
##
## The key question: what is the minimum broadcast reliability (p_broadcast)
## at which ghost-bid deviation becomes unprofitable?
##
## Factors: dag_type x p_broadcast
##   - dag_type in {tree, sp, entangled}
##   - p_broadcast in {0.0, 0.3, 0.5, 0.7, 0.9, 0.95, 1.0}
##   - operator_type = "ghost_bidder" (always; the undetectable deviation
##     from the credibility trilemma)
##
## Implementation:
##   p_broadcast = 0.0  -> credibility_type = "none"
##   p_broadcast = 1.0  -> credibility_type = "broadcast" (perfect)
##   otherwise          -> credibility_type = "regulatory" with
##                         p_detect = p_broadcast, penalty = 10
##
## The regulatory audit mechanism in sim_credibility.R applies probabilistic
## per-round detection with no delay, which is functionally identical to an
## imperfect broadcast channel where each round's deviation is independently
## detected with probability p.
##
## n_rounds = 100, n_agents = 40, n_seeds = 5

expI_design <- function(
  topologies       = c("tree", "sp", "entangled"),
  p_broadcast_values = c(0.0, 0.3, 0.5, 0.7, 0.9, 0.95, 1.0)
) {
  conditions <- expand.grid(
    dag_type    = topologies,
    p_broadcast = p_broadcast_values,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(condition_id = row_number())

  conditions
}


expI_run_single <- function(condition, n_rounds, n_agents, seed) {
  # Map p_broadcast to credibility parameters
  pb <- condition$p_broadcast

  if (pb == 0.0) {
    cred_type   <- "none"
    cred_params <- list()
  } else if (pb == 1.0) {
    cred_type   <- "broadcast"
    cred_params <- list()
  } else {
    cred_type   <- "regulatory"
    cred_params <- list(p_detect = pb, penalty = 10)
  }

  run_simulation(
    n_rounds           = n_rounds,
    n_agents           = n_agents,
    dag_type           = condition$dag_type,
    load_level         = 1.0,
    operator_type      = "ghost_bidder",
    credibility_type   = cred_type,
    credibility_params = cred_params,
    seed               = seed
  ) %>%
    mutate(p_broadcast = pb)
}


expI_run_all <- function(conditions, n_rounds, n_agents, n_seeds) {
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expI_run_single(cond, n_rounds, n_agents, seed = s) %>%
        mutate(condition_id = cond$condition_id)
    })
  })
  results
}


expI_aggregate <- function(results) {

  # -- Baseline: perfect broadcast (p_broadcast = 1.0) -----------------------
  broadcast_baseline <- results %>%
    filter(p_broadcast == 1.0) %>%
    group_by(dag_type, seed) %>%
    summarise(
      baseline_welfare = mean(welfare, na.rm = TRUE),
      .groups = "drop"
    )

  # -- Per-seed summary ------------------------------------------------------
  per_seed <- results %>%
    group_by(dag_type, p_broadcast, seed) %>%
    summarise(
      mean_welfare     = mean(welfare, na.rm = TRUE),
      mean_net_surplus = mean(net_op_surplus, na.rm = TRUE),
      mean_op_surplus  = mean(operator_surplus, na.rm = TRUE),
      detection_rate   = mean(penalty_applied > 0, na.rm = TRUE),
      mean_drop_rate   = mean(drop_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(broadcast_baseline,
              by = c("dag_type", "seed")) %>%
    mutate(
      welfare_ratio = mean_welfare / baseline_welfare,
      profitable    = (mean_net_surplus > 0)
    )

  # -- Aggregated summary (across seeds) ------------------------------------
  summary <- per_seed %>%
    group_by(dag_type, p_broadcast) %>%
    summarise(
      # Welfare ratio relative to perfect broadcast
      welfare_ratio_mean  = mean(welfare_ratio, na.rm = TRUE),
      welfare_ratio_ci    = list(boot_ci(welfare_ratio)),

      # Operator net surplus (after penalties)
      net_surplus_mean    = mean(mean_net_surplus, na.rm = TRUE),
      net_surplus_ci      = list(boot_ci(mean_net_surplus)),

      # Detection rate (empirical)
      detection_rate_mean = mean(detection_rate, na.rm = TRUE),

      # Profitability rate: fraction of seeds where deviation is profitable
      profitability_rate  = mean(profitable, na.rm = TRUE),

      # Drop rate
      drop_rate_mean      = mean(mean_drop_rate, na.rm = TRUE),

      n_obs = n(),
      .groups = "drop"
    ) %>%
    mutate(
      welfare_ratio_ci_lo = map_dbl(welfare_ratio_ci, ~ .x["lo"]),
      welfare_ratio_ci_hi = map_dbl(welfare_ratio_ci, ~ .x["hi"]),
      net_surplus_ci_lo   = map_dbl(net_surplus_ci, ~ .x["lo"]),
      net_surplus_ci_hi   = map_dbl(net_surplus_ci, ~ .x["hi"])
    ) %>%
    select(-welfare_ratio_ci, -net_surplus_ci)

  # -- Profitability threshold: minimum p_broadcast where deviation is ------
  #    unprofitable (net surplus crosses zero), per dag_type
  threshold <- per_seed %>%
    group_by(dag_type, p_broadcast) %>%
    summarise(
      mean_net_surplus = mean(mean_net_surplus, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(mean_net_surplus <= 0) %>%
    group_by(dag_type) %>%
    summarise(
      profitability_threshold = min(p_broadcast),
      .groups = "drop"
    )

  list(
    summary   = summary,
    threshold = threshold
  )
}
