suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## -- Experiment N: Markov Broadcast Channel ----------------------------------
##
## Tests how correlated (non-i.i.d.) channel failures affect the broadcast
## profitability threshold identified in Experiment I (Paper Exp 8).
##
## Markov channel model:
##   2-state chain: Good (G) and Bad (B).
##   In state G: detection probability = p_high = min(1, p_stationary * 1.5)
##   In state B: detection probability = p_low  = max(0, p_stationary * 0.5)
##
##   Transition probabilities parameterised by autocorrelation rho:
##     alpha = P(G -> B) = (1 - p_stationary) * (1 - rho)
##     beta  = P(B -> G) = p_stationary * (1 - rho)
##
##   Stationary distribution: pi_G = beta / (alpha + beta) = p_stationary
##   so the long-run average detection rate equals the i.i.d. rate.
##   rho = 0 recovers i.i.d.; rho -> 1 gives long runs of same state.
##
## The key question: does autocorrelation shift the profitability threshold
## (minimum detection rate at which ghost-bid deviation becomes unprofitable)?
##
## Factors: topology x channel_model x p_stationary
##   - topology:      {tree, sp, entangled}
##   - channel_model: {iid, markov_low (rho=0.3), markov_med (rho=0.7),
##                     markov_high (rho=0.9)}
##   - p_stationary:  {0.3, 0.5, 0.7}
##   - operator:      ghost_bidder (always)
##
## n_rounds = 100, n_agents = 40, n_seeds = 5

expN_design <- function(
  topologies        = c("tree", "sp", "entangled"),
  channel_models    = c("iid", "markov_low", "markov_med", "markov_high"),
  p_stationary_vals = c(0.3, 0.5, 0.7)
) {
  # Map channel models to autocorrelation values
  rho_map <- c(iid = 0, markov_low = 0.3, markov_med = 0.7, markov_high = 0.9)

  expand.grid(
    dag_type      = topologies,
    channel_model = channel_models,
    p_stationary  = p_stationary_vals,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(
      rho = rho_map[channel_model],
      condition_id = row_number()
    )
}


expN_run_single <- function(condition, n_rounds, n_agents, seed) {

  set.seed(seed)

  dag    <- make_dag(condition$dag_type)
  env    <- make_env()
  agents <- make_agents(n_agents, seed = seed)

  load_level <- 1.0
  env$capacity <- env$capacity / load_level

  # Operator: always ghost_bidder
  operator <- make_operator(type = "ghost_bidder")

  # -- Markov channel parameters --------------------------------------------
  p_stat <- condition$p_stationary
  rho    <- condition$rho

  # Detection probabilities in each state
  p_high <- min(1.0, p_stat * 1.5)
  p_low  <- max(0.0, p_stat * 0.5)

  # Transition probabilities
  alpha <- (1 - p_stat) * (1 - rho)  # P(G -> B)
  beta  <- p_stat * (1 - rho)        # P(B -> G)

  # Initialise channel state: draw from stationary distribution
  channel_good <- (runif(1) < p_stat)

  # Penalty multiplier (same as Exp I)
  penalty_mult <- 10.0

  # -- Market state ---------------------------------------------------------
  prices  <- NULL
  history <- vector("list", n_rounds)

  for (r in seq_len(n_rounds)) {

    # -- Channel state transition -------------------------------------------
    if (channel_good) {
      if (runif(1) < alpha) channel_good <- FALSE
    } else {
      if (runif(1) < beta)  channel_good <- TRUE
    }

    p_detect_round <- if (channel_good) p_high else p_low

    # -- Generate tasks -----------------------------------------------------
    tasks <- generate_tasks(agents, lambda = 1.0,
                            deadlines = c(100, 150, 200),
                            seed = seed * 1000 + r)

    # -- VCG allocation -----------------------------------------------------
    allocation       <- vcg_allocate(tasks, env, dag)
    broadcast_prices <- NULL

    truthful_allocation <- allocation

    # -- Operator distortion (ghost_bidder) ---------------------------------
    payments <- allocation$vcg_payment
    operator_result <- apply_operator_strategy(
      operator, allocation, payments, env,
      tibble(agent_id = unique(tasks$agent_id)),
      dag = dag
    )

    allocation$vcg_payment <- operator_result$payments
    if ("allocation" %in% names(operator_result) &&
        "realised_value" %in% names(operator_result$allocation)) {
      allocation$realised_value <- operator_result$allocation$realised_value
      allocation$allocated <- operator_result$allocation$allocated
    }

    # -- Credibility enforcement (Markov channel detection) -----------------
    detected <- FALSE
    penalty_applied <- 0

    if (operator_result$surplus > 0 && runif(1) < p_detect_round) {
      detected <- TRUE
      penalty_applied <- operator_result$surplus * penalty_mult
    }

    net_surplus <- operator_result$surplus - penalty_applied

    # If detected, revert to truthful allocation
    if (detected) {
      allocation  <- truthful_allocation
      net_surplus <- -penalty_applied
    }

    # -- Price adjustment ---------------------------------------------------
    if (is.null(prices)) {
      prices <- setNames(rep(1.0, length(env$tiers)), env$tiers)
    }

    demand_per_tier <- sapply(env$tiers, function(t) {
      sum(allocation$allocated & grepl(t, allocation$tier), na.rm = TRUE) *
        dag$tier_demand[t]
    })

    new_prices <- tatonnement_step(prices, demand_per_tier, env$capacity,
                                   eta = 0.1)
    prices <- new_prices

    # -- Compute welfare ----------------------------------------------------
    utilisation <- demand_per_tier / env$capacity
    welfare     <- compute_welfare(allocation, utilisation = utilisation)

    # -- Record round result ------------------------------------------------
    history[[r]] <- tibble(
      round            = r,
      welfare          = welfare,
      drop_rate        = mean(!allocation$allocated),
      operator_surplus = operator_result$surplus,
      penalty_applied  = penalty_applied,
      net_op_surplus   = net_surplus,
      latency_overhead = 0,
      mean_price       = mean(new_prices),
      price_sd         = sd(new_prices),
      n_tasks          = nrow(tasks),
      n_allocated      = sum(allocation$allocated),
      channel_good     = channel_good,
      p_detect_round   = p_detect_round,
      detected         = detected
    )
  }

  bind_rows(history) %>%
    mutate(
      seed          = seed,
      dag_type      = condition$dag_type,
      channel_model = condition$channel_model,
      p_stationary  = condition$p_stationary,
      rho           = condition$rho
    )
}


expN_run_all <- function(conditions, n_rounds, n_agents, n_seeds) {
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expN_run_single(cond, n_rounds, n_agents, seed = s) %>%
        mutate(condition_id = cond$condition_id)
    })
  })
  results
}


expN_aggregate <- function(results) {

  # -- Baseline: i.i.d. channel at p_stationary = 1.0 is not tested,
  #    so use i.i.d. at each p_stationary level as the baseline.
  iid_baseline <- results %>%
    filter(channel_model == "iid") %>%
    group_by(dag_type, p_stationary, seed) %>%
    summarise(
      baseline_welfare    = mean(welfare, na.rm = TRUE),
      baseline_net_surplus = mean(net_op_surplus, na.rm = TRUE),
      .groups = "drop"
    )

  # -- Per-seed summary -----------------------------------------------------
  per_seed <- results %>%
    group_by(dag_type, channel_model, p_stationary, rho, seed) %>%
    summarise(
      mean_welfare     = mean(welfare, na.rm = TRUE),
      mean_net_surplus = mean(net_op_surplus, na.rm = TRUE),
      mean_op_surplus  = mean(operator_surplus, na.rm = TRUE),
      detection_rate   = mean(detected, na.rm = TRUE),
      mean_p_detect    = mean(p_detect_round, na.rm = TRUE),
      frac_good_state  = mean(channel_good, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(iid_baseline, by = c("dag_type", "p_stationary", "seed")) %>%
    mutate(
      welfare_ratio = mean_welfare / baseline_welfare,
      profitable    = (mean_net_surplus > 0)
    )

  # -- Aggregated summary (across seeds) ------------------------------------
  summary <- per_seed %>%
    group_by(dag_type, channel_model, p_stationary, rho) %>%
    summarise(
      # Net surplus
      net_surplus_mean    = mean(mean_net_surplus, na.rm = TRUE),
      net_surplus_ci      = list(boot_ci(mean_net_surplus)),

      # Welfare ratio relative to i.i.d.
      welfare_ratio_mean  = mean(welfare_ratio, na.rm = TRUE),
      welfare_ratio_ci    = list(boot_ci(welfare_ratio)),

      # Detection rate
      detection_rate_mean = mean(detection_rate, na.rm = TRUE),

      # Profitability rate
      profitability_rate  = mean(profitable, na.rm = TRUE),

      # Channel statistics
      frac_good_mean      = mean(frac_good_state, na.rm = TRUE),

      n_obs = n(),
      .groups = "drop"
    ) %>%
    mutate(
      net_surplus_ci_lo   = map_dbl(net_surplus_ci, ~ .x["lo"]),
      net_surplus_ci_hi   = map_dbl(net_surplus_ci, ~ .x["hi"]),
      welfare_ratio_ci_lo = map_dbl(welfare_ratio_ci, ~ .x["lo"]),
      welfare_ratio_ci_hi = map_dbl(welfare_ratio_ci, ~ .x["hi"])
    ) %>%
    select(-net_surplus_ci, -welfare_ratio_ci)

  # -- Profitability threshold: minimum p_stationary where deviation is -----
  #    unprofitable, per channel_model and topology
  threshold <- per_seed %>%
    group_by(dag_type, channel_model, rho, p_stationary) %>%
    summarise(
      mean_net_surplus = mean(mean_net_surplus, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(mean_net_surplus <= 0) %>%
    group_by(dag_type, channel_model, rho) %>%
    summarise(
      profitability_threshold = min(p_stationary),
      .groups = "drop"
    )

  list(
    summary   = summary,
    threshold = threshold
  )
}
