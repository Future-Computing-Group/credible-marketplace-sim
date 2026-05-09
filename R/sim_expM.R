suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## -- Experiment M: Strategic Agent Adaptation --------------------------------
##
## Tests whether credibility mechanisms remain effective when agents can
## strategically exit the marketplace in response to operator manipulation.
##
## Agent exit model:
##   Each agent tracks a rolling-window (10 rounds) average of per-task
##   net utility (realised_value - vcg_payment).  If this average falls
##   below an outside-option threshold (theta = 0), the agent exits with
##   probability p_exit = 0.5 per round.  An exited agent stops generating
##   tasks for the remainder of the simulation.
##
## Under truthful mechanism execution (broadcast, exchange), agents earn
## positive surplus and remain; under operator deviation (none), surplus
## extraction pushes agents below threshold, triggering exits.
##
## Operator model: flat surcharge (0.8 per allocated task).
## Neither the ghost-bidder nor proportional inflator work well here:
##   - Ghost-bidder: VCG individual rationality ensures per-task net
##     utility >= 0; the ~0.02 per-agent impact is too small.
##   - Proportional inflator (mu * vcg_payment): VCG payments are nearly
##     zero on tree/SP topologies (excess capacity), so multiplying
##     near-zero by any factor still gives near-zero.
## A flat surcharge of 0.8 per task affects all topologies equally,
## independent of capacity congestion.  With mean realised_value ~ 1.0,
## agents with below-median values see negative net utility and
## eventually exit, while high-value agents remain marginally positive.
##
## Factors: topology x credibility x agent_mode
##   - topology:    {tree, sp, entangled}
##   - credibility: {none, broadcast, exchange}
##   - agent_mode:  {passive, exit_sensitive}
##   - operator:    flat surcharge (0.8/task, always)
##
## n_rounds = 100, n_agents = 40, n_seeds = 5

expM_design <- function(
  topologies  = c("tree", "sp", "entangled"),
  cred_types  = c("none", "broadcast", "exchange"),
  agent_modes = c("passive", "exit_sensitive")
) {
  expand.grid(
    dag_type    = topologies,
    credibility = cred_types,
    agent_mode  = agent_modes,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(condition_id = row_number())
}


expM_run_single <- function(condition, n_rounds, n_agents, seed) {

  set.seed(seed)

  dag    <- make_dag(condition$dag_type)
  env    <- make_env()
  agents <- make_agents(n_agents, seed = seed)

  load_level <- 1.0
  env$capacity <- env$capacity / load_level
  base_capacity <- env$capacity  # save for reference

  # Operator: flat surcharge (0.8 per allocated task)
  # Neither ghost-bidder (~0.02/agent) nor proportional inflator
  # (VCG payments ~0 on tree/SP) is strong enough to trigger exits.
  # A flat surcharge of 0.8 per task is independent of VCG payment
  # magnitude, affecting all topologies equally.
  flat_surcharge <- 0.8

  # Credibility mechanism
  credibility <- make_credibility_mechanism(type = condition$credibility)

  # -- Agent state ----------------------------------------------------------
  is_exit_sensitive <- (condition$agent_mode == "exit_sensitive")
  window_size <- 10L
  theta       <- 0.0    # outside-option threshold (net utility per task)
  p_exit      <- 0.5    # exit probability per round when below threshold

  agent_active     <- rep(TRUE, n_agents)
  # Rolling window of net utility per task for each agent
  agent_utility_window <- vector("list", n_agents)
  for (i in seq_len(n_agents)) {
    agent_utility_window[[i]] <- numeric(0)
  }

  # -- Market state ---------------------------------------------------------
  prices  <- NULL
  history <- vector("list", n_rounds)

  for (r in seq_len(n_rounds)) {

    # -- Generate tasks for active agents only ------------------------------
    active_agents <- agents[agent_active, ]
    n_active <- sum(agent_active)

    if (n_active == 0) {
      # All agents have exited
      history[[r]] <- tibble(
        round            = r,
        welfare          = 0,
        drop_rate        = 1,
        operator_surplus = 0,
        penalty_applied  = 0,
        net_op_surplus   = 0,
        latency_overhead = 0,
        mean_price       = NA_real_,
        price_sd         = NA_real_,
        n_tasks          = 0L,
        n_allocated      = 0L,
        fraction_active  = 0,
        n_exits_this_round = 0L
      )
      next
    }

    tasks <- generate_tasks(active_agents, lambda = 1.0,
                            deadlines = c(100, 150, 200),
                            seed = seed * 1000 + r)

    if (nrow(tasks) == 0) {
      history[[r]] <- tibble(
        round            = r,
        welfare          = 0,
        drop_rate        = 1,
        operator_surplus = 0,
        penalty_applied  = 0,
        net_op_surplus   = 0,
        latency_overhead = 0,
        mean_price       = NA_real_,
        price_sd         = NA_real_,
        n_tasks          = 0L,
        n_allocated      = 0L,
        fraction_active  = n_active / n_agents,
        n_exits_this_round = 0L
      )
      next
    }

    # -- VCG allocation (or ascending if broadcast) -------------------------
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

    # -- Operator distortion: flat surcharge ---------------------------------
    if (credibility$type == "exchange" && credibility$stake_fraction == 0) {
      # Exchange (domain separation): operator has no deviation incentive
      operator_result <- list(
        allocation = allocation,
        payments   = allocation$vcg_payment,
        surplus    = 0,
        detectable = FALSE
      )
    } else {
      # Add flat surcharge to each allocated task's payment
      surcharge_vec <- ifelse(allocation$allocated, flat_surcharge, 0)
      inflated <- allocation$vcg_payment + surcharge_vec
      surplus  <- sum(surcharge_vec, na.rm = TRUE)

      operator_result <- list(
        allocation = allocation,
        payments   = inflated,
        surplus    = surplus,
        detectable = FALSE
      )

      allocation$vcg_payment <- inflated
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

    # -- Compute per-agent net utility and apply exit rule ------------------
    n_exits <- 0L

    if (is_exit_sensitive && r >= window_size) {
      # Compute per-agent net utility for this round
      agent_round_utility <- allocation %>%
        group_by(agent_id) %>%
        summarise(
          total_value   = sum(realised_value, na.rm = TRUE),
          total_payment = sum(vcg_payment, na.rm = TRUE),
          n_tasks_agent = n(),
          .groups = "drop"
        ) %>%
        mutate(
          net_utility_per_task = (total_value - total_payment) /
            pmax(n_tasks_agent, 1)
        )

      # Update rolling windows for active agents
      for (aid in which(agent_active)) {
        agent_row <- agent_round_utility %>%
          filter(agent_id == aid)
        if (nrow(agent_row) > 0) {
          nu <- agent_row$net_utility_per_task[1]
        } else {
          nu <- 0  # no tasks this round
        }

        w <- agent_utility_window[[aid]]
        w <- c(w, nu)
        if (length(w) > window_size) w <- tail(w, window_size)
        agent_utility_window[[aid]] <- w

        # Apply exit rule only after full window
        if (length(w) >= window_size) {
          rolling_avg <- mean(w)
          if (rolling_avg < theta && runif(1) < p_exit) {
            agent_active[aid] <- FALSE
            n_exits <- n_exits + 1L
          }
        }
      }
    } else if (is_exit_sensitive) {
      # Before window is full, just accumulate utility
      agent_round_utility <- allocation %>%
        group_by(agent_id) %>%
        summarise(
          total_value   = sum(realised_value, na.rm = TRUE),
          total_payment = sum(vcg_payment, na.rm = TRUE),
          n_tasks_agent = n(),
          .groups = "drop"
        ) %>%
        mutate(
          net_utility_per_task = (total_value - total_payment) /
            pmax(n_tasks_agent, 1)
        )

      for (aid in which(agent_active)) {
        agent_row <- agent_round_utility %>%
          filter(agent_id == aid)
        if (nrow(agent_row) > 0) {
          nu <- agent_row$net_utility_per_task[1]
        } else {
          nu <- 0
        }
        w <- agent_utility_window[[aid]]
        w <- c(w, nu)
        if (length(w) > window_size) w <- tail(w, window_size)
        agent_utility_window[[aid]] <- w
      }
    }

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
      fraction_active    = sum(agent_active) / n_agents,
      n_exits_this_round = n_exits
    )
  }

  bind_rows(history) %>%
    mutate(
      seed        = seed,
      dag_type    = condition$dag_type,
      credibility = condition$credibility,
      agent_mode  = condition$agent_mode
    )
}


expM_run_all <- function(conditions, n_rounds, n_agents, n_seeds) {
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expM_run_single(cond, n_rounds, n_agents, seed = s) %>%
        mutate(condition_id = cond$condition_id)
    })
  })
  results
}


expM_aggregate <- function(results) {

  # -- Baseline: exchange + passive (truthful, no exits) --------------------
  exchange_baseline <- results %>%
    filter(credibility == "exchange", agent_mode == "passive") %>%
    group_by(dag_type, seed) %>%
    summarise(
      baseline_welfare = mean(welfare, na.rm = TRUE),
      .groups = "drop"
    )

  # -- Per-seed summary -----------------------------------------------------
  per_seed <- results %>%
    group_by(dag_type, credibility, agent_mode, seed) %>%
    summarise(
      mean_welfare       = mean(welfare, na.rm = TRUE),
      mean_net_surplus   = mean(net_op_surplus, na.rm = TRUE),
      mean_fraction_active = mean(fraction_active, na.rm = TRUE),
      final_fraction_active = last(fraction_active),
      total_exits        = sum(n_exits_this_round, na.rm = TRUE),
      detection_rate     = mean(penalty_applied > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(exchange_baseline, by = c("dag_type", "seed")) %>%
    mutate(
      welfare_ratio = mean_welfare / baseline_welfare
    )

  # -- Aggregated summary (across seeds) ------------------------------------
  summary <- per_seed %>%
    group_by(dag_type, credibility, agent_mode) %>%
    summarise(
      welfare_ratio_mean    = mean(welfare_ratio, na.rm = TRUE),
      welfare_ratio_ci      = list(boot_ci(welfare_ratio)),
      net_surplus_mean      = mean(mean_net_surplus, na.rm = TRUE),
      net_surplus_ci        = list(boot_ci(mean_net_surplus)),
      fraction_active_mean  = mean(final_fraction_active, na.rm = TRUE),
      fraction_active_ci    = list(boot_ci(final_fraction_active)),
      total_exits_mean      = mean(total_exits, na.rm = TRUE),
      n_obs = n(),
      .groups = "drop"
    ) %>%
    mutate(
      welfare_ratio_ci_lo   = map_dbl(welfare_ratio_ci, ~ .x["lo"]),
      welfare_ratio_ci_hi   = map_dbl(welfare_ratio_ci, ~ .x["hi"]),
      net_surplus_ci_lo     = map_dbl(net_surplus_ci, ~ .x["lo"]),
      net_surplus_ci_hi     = map_dbl(net_surplus_ci, ~ .x["hi"]),
      fraction_active_ci_lo = map_dbl(fraction_active_ci, ~ .x["lo"]),
      fraction_active_ci_hi = map_dbl(fraction_active_ci, ~ .x["hi"])
    ) %>%
    select(-welfare_ratio_ci, -net_surplus_ci, -fraction_active_ci)

  # -- Per-round trajectory (for agent retention plot) ----------------------
  trajectory <- results %>%
    group_by(dag_type, credibility, agent_mode, round) %>%
    summarise(
      fraction_active_mean = mean(fraction_active, na.rm = TRUE),
      fraction_active_se   = sd(fraction_active, na.rm = TRUE) / sqrt(n()),
      welfare_mean         = mean(welfare, na.rm = TRUE),
      welfare_se           = sd(welfare, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  list(
    summary    = summary,
    trajectory = trajectory
  )
}
