suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## -- Experiment H: Adaptive (Learning) Operator --------------------------
##
## Tests whether credibility mechanisms remain effective against a
## sophisticated, non-myopic operator that uses an epsilon-greedy bandit
## strategy to learn the optimal deviation magnitude over time.
##
## The adaptive operator maintains an arm set of deviation magnitudes:
##   {0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3}
##   Arm 0 = truthful (no deviation)
##   Arms >0 = ghost-bid deviation with that fraction of mean task value
##
## Each round:
##   1. Choose arm via epsilon-greedy (epsilon decays over rounds)
##   2. Run one market round with the chosen deviation magnitude
##   3. Observe net surplus (after credibility penalties)
##   4. Update arm running-average statistics
##
## Factors: credibility_type x topology x n_agents
## n_rounds = 200 (longer to show convergence behaviour)
##
## Epsilon decay schedule:
##   epsilon(t) = max(0.05, 0.3 * (1 - t / n_rounds))

expH_design <- function(
  topologies  = c("tree", "sp", "entangled"),
  n_agents_set = c(10, 25, 50),
  cred_types  = c("none", "broadcast", "blockchain", "exchange")
) {
  conditions <- expand.grid(
    dag_type    = topologies,
    n_agents    = n_agents_set,
    credibility = cred_types,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(condition_id = row_number())

  conditions
}


expH_run_single <- function(condition, n_rounds, seed) {

  set.seed(seed)

  dag    <- make_dag(condition$dag_type)
  env    <- make_env()
  agents <- make_agents(condition$n_agents, seed = seed)

  # Medium load (load_level = 1.0, no capacity scaling)
  load_level <- 1.0
  env$capacity <- env$capacity / load_level

  # Credibility mechanism
  credibility <- make_credibility_mechanism(type = condition$credibility)

  # -- Bandit state -------------------------------------------------------
  arms       <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3)
  n_arms     <- length(arms)
  arm_reward <- rep(0, n_arms)   # cumulative reward per arm

  arm_pulls  <- rep(0L, n_arms)  # number of pulls per arm
  arm_mean   <- rep(0, n_arms)   # running average reward per arm

  # -- Market state -------------------------------------------------------
  prices  <- NULL
  history <- vector("list", n_rounds)

  for (r in seq_len(n_rounds)) {

    # -- Epsilon-greedy arm selection ------------------------------------
    epsilon_r <- max(0.05, 0.3 * (1 - r / n_rounds))

    if (runif(1) < epsilon_r) {
      chosen_arm <- sample.int(n_arms, 1)
    } else {
      # Exploit: pick arm with highest running average (break ties randomly)
      best_val  <- max(arm_mean)
      best_arms <- which(arm_mean == best_val)
      chosen_arm <- if (length(best_arms) == 1) best_arms else sample(best_arms, 1)
    }
    deviation_magnitude <- arms[chosen_arm]

    # -- Generate tasks for this round -----------------------------------
    tasks <- generate_tasks(agents, lambda = 1.0,
                            deadlines = c(100, 150, 200),
                            seed = seed * 1000 + r)

    # -- VCG allocation (or ascending if broadcast) ----------------------
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

    # -- Apply ghost-bid deviation (if arm > 0) --------------------------
    if (credibility$type == "exchange") {
      # Neutral exchange: no distortion possible regardless of arm
      operator_result <- list(
        allocation = allocation,
        payments   = allocation$vcg_payment,
        surplus    = 0,
        detectable = FALSE
      )
    } else if (deviation_magnitude == 0) {
      # Truthful: no deviation
      operator_result <- list(
        allocation = allocation,
        payments   = allocation$vcg_payment,
        surplus    = 0,
        detectable = FALSE
      )
    } else {
      # Ghost-bid deviation: inject a ghost bid calibrated to
      # deviation_magnitude * mean(task values)
      payments        <- allocation$vcg_payment
      allocated_tasks <- allocation %>% filter(allocated)

      if (nrow(allocated_tasks) == 0) {
        operator_result <- list(
          allocation = allocation,
          payments   = payments,
          surplus    = 0,
          detectable = FALSE
        )
      } else {
        # Ghost value: deviation_magnitude * mean task value
        mean_task_val <- mean(tasks$value, na.rm = TRUE)
        ghost_val     <- deviation_magnitude * mean_task_val

        # Displace the marginal (last) allocated task
        marginal_idx   <- which(allocation$allocated)
        displaced_idx  <- marginal_idx[length(marginal_idx)]

        new_alloc <- allocation
        new_alloc$allocated[displaced_idx]      <- FALSE
        new_alloc$realised_value[displaced_idx]  <- 0

        # Remaining allocated agents pay more (ghost raises externality)
        still_allocated <- which(new_alloc$allocated)
        new_payments    <- payments

        if (length(still_allocated) > 0) {
          payment_increase <- ghost_val / length(still_allocated)
          new_payments[still_allocated] <- payments[still_allocated] + payment_increase
        }
        new_payments[displaced_idx] <- 0

        surplus <- sum(new_payments[still_allocated] - payments[still_allocated],
                       na.rm = TRUE)

        operator_result <- list(
          allocation = new_alloc,
          payments   = new_payments,
          surplus    = max(0, surplus),
          detectable = FALSE
        )

        # Update allocation to reflect the distorted outcome
        allocation$vcg_payment    <- new_payments
        allocation$realised_value <- new_alloc$realised_value
        allocation$allocated      <- new_alloc$allocated
      }
    }

    # -- Credibility enforcement -----------------------------------------
    cred_result <- enforce_credibility(
      credibility, operator_result, r,
      broadcast_prices = broadcast_prices
    )

    # If deviation detected (broadcast/blockchain), revert to truthful
    if (cred_result$detected &&
        credibility$type %in% c("broadcast", "blockchain")) {
      allocation <- truthful_allocation
      cred_result$net_operator_surplus <- -cred_result$penalty_applied
    }

    # -- Tatonnement price adjustment ------------------------------------
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

    # -- Compute welfare -------------------------------------------------
    utilisation <- demand_per_tier / env$capacity
    welfare     <- compute_welfare(allocation, utilisation = utilisation)

    # -- Observe net surplus (reward for this arm) -----------------------
    net_surplus <- cred_result$net_operator_surplus

    # -- Update bandit state ---------------------------------------------
    arm_pulls[chosen_arm]  <- arm_pulls[chosen_arm] + 1L
    arm_reward[chosen_arm] <- arm_reward[chosen_arm] + net_surplus
    arm_mean[chosen_arm]   <- arm_reward[chosen_arm] / arm_pulls[chosen_arm]

    # -- Record round result ---------------------------------------------
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
      chosen_arm         = chosen_arm,
      deviation_magnitude = deviation_magnitude,
      epsilon            = epsilon_r,
      best_arm           = arms[which.max(arm_mean)],
      best_arm_mean      = max(arm_mean)
    )
  }

  bind_rows(history) %>%
    mutate(
      seed          = seed,
      dag_type      = condition$dag_type,
      n_agents_cond = condition$n_agents,
      credibility   = condition$credibility
    )
}


expH_run_all <- function(conditions, n_rounds, n_seeds) {
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expH_run_single(cond, n_rounds, seed = s) %>%
        mutate(condition_id = cond$condition_id)
    })
  })
  results
}


expH_aggregate <- function(results) {

  # -- Truthful baseline: exchange mechanism (no deviation possible) ------
  exchange_baseline <- results %>%
    filter(credibility == "exchange") %>%
    group_by(dag_type, n_agents_cond, seed) %>%
    summarise(
      baseline_welfare = mean(welfare, na.rm = TRUE),
      .groups = "drop"
    )

  # -- Per-seed summary ---------------------------------------------------
  # Use last 20 rounds for converged metrics
  n_tail <- 20

  per_seed <- results %>%
    group_by(dag_type, n_agents_cond, credibility, seed) %>%
    mutate(
      is_tail = (round > max(round) - n_tail)
    ) %>%
    summarise(
      # Full-run metrics
      cumulative_surplus     = sum(net_op_surplus, na.rm = TRUE),
      mean_welfare_all       = mean(welfare, na.rm = TRUE),
      # Last-20-round (converged) metrics
      mean_welfare_tail      = mean(welfare[is_tail], na.rm = TRUE),
      mean_deviation_tail    = mean(deviation_magnitude[is_tail], na.rm = TRUE),
      mean_net_surplus_tail  = mean(net_op_surplus[is_tail], na.rm = TRUE),
      # Arm convergence: modal arm in last 20 rounds
      converged_arm          = as.numeric(names(sort(table(
        deviation_magnitude[is_tail]), decreasing = TRUE))[1]),
      # Fraction of last 20 rounds spent on arm 0 (truthful)
      pct_truthful_tail      = mean(deviation_magnitude[is_tail] == 0, na.rm = TRUE),
      detection_rate         = mean(penalty_applied > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(exchange_baseline,
              by = c("dag_type", "n_agents_cond", "seed")) %>%
    mutate(
      welfare_ratio_all  = mean_welfare_all / baseline_welfare,
      welfare_ratio_tail = mean_welfare_tail / baseline_welfare
    )

  # -- Aggregated summary (across seeds) ----------------------------------
  summary <- per_seed %>%
    group_by(dag_type, n_agents_cond, credibility) %>%
    summarise(
      # Final welfare ratio (last 20 rounds)
      final_welfare_ratio_mean = mean(welfare_ratio_tail, na.rm = TRUE),
      final_welfare_ratio_ci   = list(boot_ci(welfare_ratio_tail)),

      # Operator arm convergence
      operator_arm_convergence = as.numeric(names(sort(table(converged_arm),
                                                       decreasing = TRUE))[1]),

      # Mean deviation magnitude (last 20 rounds)
      mean_deviation_magnitude = mean(mean_deviation_tail, na.rm = TRUE),
      mean_deviation_ci        = list(boot_ci(mean_deviation_tail)),

      # Cumulative surplus over all rounds
      cumulative_surplus_mean  = mean(cumulative_surplus, na.rm = TRUE),
      cumulative_surplus_ci    = list(boot_ci(cumulative_surplus)),

      # Detection rate
      detection_rate_mean      = mean(detection_rate, na.rm = TRUE),

      # Fraction of tail rounds where operator chose truthful
      pct_truthful_mean        = mean(pct_truthful_tail, na.rm = TRUE),

      n_obs = n(),
      .groups = "drop"
    ) %>%
    mutate(
      final_welfare_ci_lo    = map_dbl(final_welfare_ratio_ci, ~ .x["lo"]),
      final_welfare_ci_hi    = map_dbl(final_welfare_ratio_ci, ~ .x["hi"]),
      mean_deviation_ci_lo   = map_dbl(mean_deviation_ci, ~ .x["lo"]),
      mean_deviation_ci_hi   = map_dbl(mean_deviation_ci, ~ .x["hi"]),
      cumulative_surplus_lo  = map_dbl(cumulative_surplus_ci, ~ .x["lo"]),
      cumulative_surplus_hi  = map_dbl(cumulative_surplus_ci, ~ .x["hi"])
    ) %>%
    select(-final_welfare_ratio_ci, -mean_deviation_ci, -cumulative_surplus_ci)

  # -- Per-round trajectory (for plotting convergence) --------------------
  trajectory <- results %>%
    left_join(
      exchange_baseline,
      by = c("dag_type", "n_agents_cond", "seed")
    ) %>%
    mutate(
      welfare_ratio = welfare / baseline_welfare
    ) %>%
    group_by(dag_type, n_agents_cond, credibility, round) %>%
    summarise(
      welfare_ratio_mean     = mean(welfare_ratio, na.rm = TRUE),
      welfare_ratio_se       = sd(welfare_ratio, na.rm = TRUE) / sqrt(n()),
      deviation_magnitude_mean = mean(deviation_magnitude, na.rm = TRUE),
      deviation_magnitude_se   = sd(deviation_magnitude, na.rm = TRUE) / sqrt(n()),
      net_surplus_mean       = mean(net_op_surplus, na.rm = TRUE),
      best_arm_mean          = mean(best_arm, na.rm = TRUE),
      epsilon_mean           = mean(epsilon, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    summary    = summary,
    trajectory = trajectory
  )
}
