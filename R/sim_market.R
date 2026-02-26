suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Two-tier marketplace engine ───────────────────────────────────
##
## Models the two-tier architecture:
##   Level 2 (local):  within-domain VCG allocation over tier capacities
##   Level 1 (slice):  cross-domain integrator slice marketplace
##
## The operator (or neutral exchange) mediates Level 1.

## ── Polymatroid greedy allocation ────────────────────────────────────
## Given tasks sorted by value density, greedily allocate respecting
## capacity constraints on all tiers of the critical path.
## Optimised: pre-allocated vectors, no per-row tibble construction.

greedy_allocate <- function(tasks, capacity, dag, env) {
  n <- nrow(tasks)
  if (n == 0) {
    return(tibble(
      task_id = character(), agent_id = character(),
      allocated = logical(), tier = character(),
      latency = numeric(), realised_value = numeric()
    ))
  }

  remaining <- capacity
  tiers <- dag$critical_path_tiers
  tier_demands <- dag$tier_demand[tiers]
  tier_str <- paste(tiers, collapse = "+")

  # Pre-allocate result vectors
  out_allocated <- logical(n)
  out_latency   <- rep(NA_real_, n)
  out_rv        <- numeric(n)

  for (i in seq_len(n)) {
    # Check all tiers have enough capacity
    can_allocate <- TRUE
    for (j in seq_along(tiers)) {
      if (remaining[tiers[j]] < tier_demands[j]) {
        can_allocate <- FALSE
        break
      }
    }

    if (can_allocate) {
      for (j in seq_along(tiers)) {
        remaining[tiers[j]] <- remaining[tiers[j]] - tier_demands[j]
      }
      utilisation <- 1 - remaining / env$capacity
      lat <- compute_critical_path_latency(dag, env, utilisation)
      rv  <- task_value(tasks$value[i], lat, tasks$deadline[i], tasks$lambda_l[i])

      out_allocated[i] <- TRUE
      out_latency[i]   <- lat
      out_rv[i]        <- rv
    }
  }

  tibble(
    task_id        = tasks$task_id,
    agent_id       = as.character(tasks$agent_id),
    allocated      = out_allocated,
    tier           = ifelse(out_allocated, tier_str, NA_character_),
    latency        = out_latency,
    realised_value = out_rv
  )
}

## ── Fast welfare-only greedy allocation ─────────────────────────────
## Returns only total welfare (scalar), no tibble construction.
## Used inside VCG payment loop for speed.

greedy_welfare_only <- function(tasks, capacity, dag, env) {
  n <- nrow(tasks)
  if (n == 0) return(0)

  remaining <- capacity
  tiers <- dag$critical_path_tiers
  tier_demands <- dag$tier_demand[tiers]
  total_welfare <- 0

  for (i in seq_len(n)) {
    can_allocate <- TRUE
    for (j in seq_along(tiers)) {
      if (remaining[tiers[j]] < tier_demands[j]) {
        can_allocate <- FALSE
        break
      }
    }
    if (can_allocate) {
      for (j in seq_along(tiers)) {
        remaining[tiers[j]] <- remaining[tiers[j]] - tier_demands[j]
      }
      utilisation <- 1 - remaining / env$capacity
      lat <- compute_critical_path_latency(dag, env, utilisation)
      rv  <- task_value(tasks$value[i], lat, tasks$deadline[i], tasks$lambda_l[i])
      total_welfare <- total_welfare + rv
    }
  }
  total_welfare
}


## ── VCG allocation with proper externality payments ─────────────────
## Payment_i = (welfare of others without i) - (welfare of others with i)
## Optimised: uses greedy_welfare_only for the per-agent re-allocations,
## and vectorised lookups instead of dplyr filter chains.

vcg_allocate <- function(tasks, env, dag) {
  if (nrow(tasks) == 0) {
    return(tibble(
      task_id = character(), agent_id = character(),
      allocated = logical(), tier = character(),
      latency = numeric(), realised_value = numeric(),
      vcg_payment = numeric()
    ))
  }

  # Sort by value density (value / deadline, descending)
  tasks <- tasks[order(-tasks$value / tasks$deadline), ]

  # Full allocation with all agents
  full_alloc <- greedy_allocate(tasks, env$capacity, dag, env)

  # Pre-compute per-agent welfare in the full allocation (vectorised)
  agent_ids <- full_alloc$agent_id
  alloc_mask <- full_alloc$allocated
  rv <- full_alloc$realised_value
  total_others_welfare <- sum(rv)  # includes all agents

  allocated_agents <- unique(agent_ids[alloc_mask])
  n_alloc <- length(allocated_agents)

  # Pre-compute each agent's total value in full allocation
  agent_value_map <- tapply(rv[alloc_mask], agent_ids[alloc_mask], sum)

  payments <- rep(0, nrow(full_alloc))

  for (aid in allocated_agents) {
    # Welfare of others with i present
    others_welfare_with_i <- total_others_welfare - agent_value_map[aid]

    # Re-run allocation excluding agent i (welfare-only, no tibble)
    mask_without_i <- agent_ids != aid
    tasks_without_i <- tasks[mask_without_i, ]
    others_welfare_without_i <- greedy_welfare_only(tasks_without_i, env$capacity, dag, env)

    # VCG externality
    agent_externality <- others_welfare_without_i - others_welfare_with_i

    # Distribute proportionally across agent's allocated tasks
    agent_mask <- alloc_mask & (agent_ids == aid)
    agent_rv <- rv[agent_mask]
    total_agent_value <- sum(agent_rv)

    if (total_agent_value > 0) {
      payments[agent_mask] <- pmax(0, agent_externality * agent_rv / total_agent_value)
    }
  }

  full_alloc$vcg_payment <- payments
  full_alloc
}


## ── Ascending clinching auction (simplified) ─────────────────────────
## Models the public-price ascending mechanism from Theorem 2(i).
## Prices rise in clock rounds; allocations ("clinches") occur at
## announced prices. Any discrepancy is observable by all participants.

ascending_clinch_allocate <- function(tasks, env, dag, n_clock_rounds = 20) {
  n <- nrow(tasks)
  if (n == 0) {
    return(list(
      allocation = tibble(
        task_id = character(), agent_id = character(),
        allocated = logical(), tier = character(),
        latency = numeric(), realised_value = numeric(),
        vcg_payment = numeric(), clinch_round = integer()
      ),
      price_history = numeric()
    ))
  }

  tasks <- tasks[order(-tasks$value / tasks$deadline), ]

  remaining <- env$capacity
  tiers <- dag$critical_path_tiers
  tier_demands <- dag$tier_demand[tiers]
  tier_str <- paste(tiers, collapse = "+")
  base_latency_sum <- sum(env$latency[tiers])

  clock_price <- 0.01
  price_step <- max(tasks$value) / n_clock_rounds
  price_history <- numeric(n_clock_rounds)

  # Pre-allocate result vectors
  out_allocated    <- logical(n)
  out_latency      <- rep(NA_real_, n)
  out_rv           <- numeric(n)
  out_vcg_payment  <- numeric(n)
  out_clinch_round <- rep(NA_integer_, n)

  # Pre-compute willingness to pay for each task
  wtp <- tasks$value * latency_discount(base_latency_sum, tasks$lambda_l)

  for (cr in seq_len(n_clock_rounds)) {
    clock_price <- clock_price + price_step
    price_history[cr] <- clock_price

    for (i in seq_len(n)) {
      if (out_allocated[i]) next
      if (clock_price > wtp[i]) next

      # Check capacity
      can_allocate <- TRUE
      for (j in seq_along(tiers)) {
        if (remaining[tiers[j]] < tier_demands[j]) {
          can_allocate <- FALSE
          break
        }
      }

      if (can_allocate) {
        for (j in seq_along(tiers)) {
          remaining[tiers[j]] <- remaining[tiers[j]] - tier_demands[j]
        }
        utilisation <- 1 - remaining / env$capacity
        lat <- compute_critical_path_latency(dag, env, utilisation)
        rv  <- task_value(tasks$value[i], lat, tasks$deadline[i], tasks$lambda_l[i])

        out_allocated[i]    <- TRUE
        out_latency[i]      <- lat
        out_rv[i]           <- rv
        out_vcg_payment[i]  <- clock_price
        out_clinch_round[i] <- as.integer(cr)
      }
    }
  }

  list(
    allocation = tibble(
      task_id        = tasks$task_id,
      agent_id       = as.character(tasks$agent_id),
      allocated      = out_allocated,
      tier           = ifelse(out_allocated, tier_str, NA_character_),
      latency        = out_latency,
      realised_value = out_rv,
      vcg_payment    = out_vcg_payment,
      clinch_round   = out_clinch_round
    ),
    price_history = price_history
  )
}


## ── Tatonnement price adjustment ───────────────────────────────────

tatonnement_step <- function(prices, demand, capacity, eta = 0.1) {
  excess <- demand - capacity
  new_prices <- prices + eta * excess / capacity
  pmax(new_prices, 0.01)
}


## ── Run one round of the two-tier market ──────────────────────────

run_market_round <- function(tasks, env, dag, operator, credibility,
                             integrator_market = NULL, prices = NULL,
                             round = 1, eta = 0.1) {

  # ── Level 2: VCG allocation ──────────────────────────────────────
  use_ascending <- (credibility$type == "broadcast")

  if (use_ascending) {
    asc_result <- ascending_clinch_allocate(tasks, env, dag)
    allocation <- asc_result$allocation
    broadcast_prices <- asc_result$price_history
  } else {
    allocation <- vcg_allocate(tasks, env, dag)
    broadcast_prices <- NULL
  }

  # ── Level 1: Integrator slice pricing (if applicable) ────────────
  slice_price_adj <- 0
  if (!is.null(integrator_market)) {
    cap_per_integrator <- min(env$capacity) / integrator_market$k
    slice_info <- compute_slice_prices(
      integrator_market,
      demand       = nrow(tasks),
      capacity_per_integrator = cap_per_integrator
    )
    slice_price_adj <- slice_info$price - integrator_market$marginal_cost

    # Apply integrator effect on allocation welfare
    allocation <- allocation %>%
      mutate(realised_value = realised_value * integrator_market$efficiency)
  }

  # ── Operator distortion ──────────────────────────────────────────
  # Under exchange (domain separation, Proposition 1), the neutral exchange
  # runs the mechanism directly — the operator cannot distort.
  # Under broadcast with detected deviation, allocation is reverted.

  truthful_allocation <- allocation  # save for potential reversion

  if (credibility$type == "exchange") {
    # Neutral exchange: operator earns only fixed fee, no distortion possible
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

  # ── Credibility enforcement ──────────────────────────────────────
  cred_result <- enforce_credibility(
    credibility, operator_result, round,
    broadcast_prices = broadcast_prices
  )

  # If deviation is detected (broadcast/blockchain), revert to truthful allocation.
  # The operator does not receive the surplus from the reverted deviation.
  if (cred_result$detected && credibility$type %in% c("broadcast", "blockchain")) {
    allocation <- truthful_allocation
    # Operator got 0 from the reverted allocation, but still pays penalty
    cred_result$net_operator_surplus <- -cred_result$penalty_applied
  }

  # ── Price adjustment (tatonnement) ───────────────────────────────
  if (is.null(prices)) {
    prices <- setNames(rep(1.0, length(env$tiers)), env$tiers)
  }

  demand_per_tier <- sapply(env$tiers, function(t) {
    sum(allocation$allocated & grepl(t, allocation$tier), na.rm = TRUE) *
      dag$tier_demand[t]
  })

  new_prices <- tatonnement_step(prices, demand_per_tier, env$capacity, eta)

  # ── Compute welfare ──────────────────────────────────────────────
  utilisation <- demand_per_tier / env$capacity
  welfare <- compute_welfare(allocation, utilisation = utilisation)

  list(
    allocation      = allocation,
    welfare         = welfare,
    prices          = new_prices,
    operator_surplus = operator_result$surplus,
    cred_result     = cred_result,
    slice_price_adj = slice_price_adj,
    utilisation     = utilisation,
    drop_rate       = mean(!allocation$allocated)
  )
}


## ── Run full simulation (n_rounds) ─────────────────────────────────

run_simulation <- function(
  n_rounds, n_agents, dag_type, load_level,
  operator_type = "truthful", operator_params = list(),
  credibility_type = "none", credibility_params = list(),
  integrator_k = NULL, integrator_strategy = "competitive",
  seed = 1, eta = 0.1
) {

  set.seed(seed)

  dag    <- make_dag(dag_type)
  env    <- make_env()
  agents <- make_agents(n_agents, seed = seed)

  # Scale capacity by load level
  env$capacity <- env$capacity / load_level

  # Operator
  op_args <- c(list(type = operator_type), operator_params)
  operator <- do.call(make_operator, op_args)

  # Credibility mechanism
  cr_args <- c(list(type = credibility_type), credibility_params)
  credibility <- do.call(make_credibility_mechanism, cr_args)

  # Integrator market (optional)
  integrator_market <- NULL
  if (!is.null(integrator_k)) {
    integrator_market <- make_integrator_market(
      k = integrator_k,
      strategy = integrator_strategy
    )
  }

  # Run rounds
  prices  <- NULL
  history <- vector("list", n_rounds)

  for (r in seq_len(n_rounds)) {
    tasks <- generate_tasks(agents, lambda = 1.0,
                            deadlines = c(100, 150, 200),
                            seed = seed * 1000 + r)

    round_result <- run_market_round(
      tasks, env, dag, operator, credibility,
      integrator_market, prices, round = r, eta = eta
    )

    prices <- round_result$prices
    history[[r]] <- tibble(
      round            = r,
      welfare          = round_result$welfare,
      drop_rate        = round_result$drop_rate,
      operator_surplus = round_result$operator_surplus,
      penalty_applied  = round_result$cred_result$penalty_applied,
      net_op_surplus   = round_result$cred_result$net_operator_surplus,
      latency_overhead = round_result$cred_result$latency_overhead,
      slice_price_adj  = round_result$slice_price_adj,
      mean_price       = mean(round_result$prices),
      price_sd         = sd(round_result$prices),
      n_tasks          = nrow(tasks),
      n_allocated      = sum(round_result$allocation$allocated)
    )
  }

  bind_rows(history) %>%
    mutate(
      seed          = seed,
      dag_type      = dag_type,
      load_level    = load_level,
      operator_type = operator_type,
      credibility   = credibility_type,
      k_integrators = integrator_k %||% NA_integer_,
      integrator_strategy = if (!is.null(integrator_k)) integrator_strategy else NA_character_
    )
}
