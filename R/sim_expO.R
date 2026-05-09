suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## -- Experiment O: Non-Stationary Service Supply -----------------------------
##
## Tests whether credibility mechanisms remain effective when infrastructure
## capacity varies over time.
##
## Capacity models:
##   static:  capacity(t) = base_capacity (current behaviour)
##   cyclic:  capacity(t) = base * (1 + A * sin(2*pi*t/T))
##            A = 0.3 (30% amplitude), T = 25 rounds
##            Models maintenance cycles and diurnal demand patterns.
##   shock:   Each round, with probability p_shock = 0.05, the capacity
##            multiplier redraws from U(0.5, 1.5); otherwise it persists.
##            Models equipment failures and elastic autoscaling events.
##
## Operator deviation incentive is capacity-dependent: scarce rounds
## have more surplus to extract (higher VCG payments due to binding
## capacity constraints).
##
## Factors: topology x capacity_model x credibility
##   - topology:       {tree, sp, entangled}
##   - capacity_model: {static, cyclic, shock}
##   - credibility:    {none, broadcast, exchange}
##   - operator:       ghost_bidder (always)
##
## n_rounds = 100, n_agents = 40, n_seeds = 5

expO_design <- function(
  topologies      = c("tree", "sp", "entangled"),
  capacity_models = c("static", "cyclic", "shock"),
  cred_types      = c("none", "broadcast", "exchange")
) {
  expand.grid(
    dag_type       = topologies,
    capacity_model = capacity_models,
    credibility    = cred_types,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(condition_id = row_number())
}


expO_run_single <- function(condition, n_rounds, n_agents, seed) {

  set.seed(seed)

  dag    <- make_dag(condition$dag_type)
  env    <- make_env()
  agents <- make_agents(n_agents, seed = seed)

  load_level <- 1.0
  base_capacity <- env$capacity / load_level  # save base capacity

  # Operator: always ghost_bidder
  operator <- make_operator(type = "ghost_bidder")

  # Credibility mechanism
  credibility <- make_credibility_mechanism(type = condition$credibility)

  # -- Capacity model parameters --------------------------------------------
  cap_model <- condition$capacity_model
  amplitude <- 0.3     # cyclic: 30% variation
  period    <- 25      # cyclic: 25-round period
  p_shock   <- 0.05    # shock: 5% per-round shock probability
  shock_multiplier <- 1.0  # shock: current multiplier (persists between rounds)

  # -- Market state ---------------------------------------------------------
  prices  <- NULL
  history <- vector("list", n_rounds)

  for (r in seq_len(n_rounds)) {

    # -- Compute capacity multiplier for this round -------------------------
    if (cap_model == "static") {
      cap_mult <- 1.0
    } else if (cap_model == "cyclic") {
      cap_mult <- 1.0 + amplitude * sin(2 * pi * r / period)
    } else if (cap_model == "shock") {
      if (runif(1) < p_shock) {
        shock_multiplier <- runif(1, 0.5, 1.5)
      }
      cap_mult <- shock_multiplier
    } else {
      cap_mult <- 1.0
    }

    # Apply capacity multiplier
    env$capacity <- base_capacity * cap_mult

    # -- Generate tasks -----------------------------------------------------
    tasks <- generate_tasks(agents, lambda = 1.0,
                            deadlines = c(100, 150, 200),
                            seed = seed * 1000 + r)

    # -- Allocation (ascending if broadcast, else VCG) ----------------------
    use_ascending <- (credibility$type == "broadcast")

    if (use_ascending) {
      asc_result       <- ascending_clinch_allocate(tasks, env, dag)
      allocation       <- asc_result$allocation
      broadcast_prices <- asc_result$price_history
    } else {
      allocation       <- vcg_allocate(tasks, env, dag)
      broadcast_prices <- NULL
    }

    truthful_allocation <- allocation

    # -- Operator distortion ------------------------------------------------
    if (credibility$type == "exchange" && credibility$stake_fraction == 0) {
      operator_result <- list(
        allocation = allocation,
        payments   = allocation$vcg_payment,
        surplus    = 0,
        detectable = FALSE
      )
    } else {
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
    }

    # -- Credibility enforcement --------------------------------------------
    cred_result <- enforce_credibility(
      credibility, operator_result, r,
      broadcast_prices = broadcast_prices
    )

    if (cred_result$detected &&
        credibility$type %in% c("broadcast", "blockchain")) {
      allocation <- truthful_allocation
      cred_result$net_operator_surplus <- -cred_result$penalty_applied
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
      round              = r,
      welfare            = welfare,
      drop_rate          = mean(!allocation$allocated),
      operator_surplus   = operator_result$surplus,
      penalty_applied    = cred_result$penalty_applied,
      net_op_surplus     = cred_result$net_operator_surplus,
      latency_overhead   = cred_result$latency_overhead,
      mean_price         = mean(new_prices),
      price_sd           = sd(new_prices),
      n_tasks            = nrow(tasks),
      n_allocated        = sum(allocation$allocated),
      capacity_multiplier = cap_mult
    )
  }

  bind_rows(history) %>%
    mutate(
      seed           = seed,
      dag_type       = condition$dag_type,
      capacity_model = condition$capacity_model,
      credibility    = condition$credibility
    )
}


expO_run_all <- function(conditions, n_rounds, n_agents, n_seeds) {
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expO_run_single(cond, n_rounds, n_agents, seed = s) %>%
        mutate(condition_id = cond$condition_id)
    })
  })
  results
}


expO_aggregate <- function(results) {

  # -- Baseline: static capacity + exchange (truthful, no deviations) -------
  exchange_baseline <- results %>%
    filter(capacity_model == "static", credibility == "exchange") %>%
    group_by(dag_type, seed) %>%
    summarise(
      baseline_welfare = mean(welfare, na.rm = TRUE),
      .groups = "drop"
    )

  # -- Per-seed summary -----------------------------------------------------
  per_seed <- results %>%
    group_by(dag_type, capacity_model, credibility, seed) %>%
    summarise(
      mean_welfare        = mean(welfare, na.rm = TRUE),
      mean_net_surplus    = mean(net_op_surplus, na.rm = TRUE),
      mean_op_surplus     = mean(operator_surplus, na.rm = TRUE),
      detection_rate      = mean(penalty_applied > 0, na.rm = TRUE),
      mean_cap_multiplier = mean(capacity_multiplier, na.rm = TRUE),
      sd_cap_multiplier   = sd(capacity_multiplier, na.rm = TRUE),
      mean_drop_rate      = mean(drop_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(exchange_baseline, by = c("dag_type", "seed")) %>%
    mutate(
      welfare_ratio = mean_welfare / baseline_welfare
    )

  # -- Aggregated summary (across seeds) ------------------------------------
  summary <- per_seed %>%
    group_by(dag_type, capacity_model, credibility) %>%
    summarise(
      welfare_ratio_mean    = mean(welfare_ratio, na.rm = TRUE),
      welfare_ratio_ci      = list(boot_ci(welfare_ratio)),
      net_surplus_mean      = mean(mean_net_surplus, na.rm = TRUE),
      net_surplus_ci        = list(boot_ci(mean_net_surplus)),
      op_surplus_mean       = mean(mean_op_surplus, na.rm = TRUE),
      detection_rate_mean   = mean(detection_rate, na.rm = TRUE),
      drop_rate_mean        = mean(mean_drop_rate, na.rm = TRUE),
      cap_volatility_mean   = mean(sd_cap_multiplier, na.rm = TRUE),
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

  summary
}
