suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Operator strategies ─────────────────────────────────────────────
##
## Each operator strategy takes the true allocation and payments from VCG
## and returns a (possibly distorted) allocation and payments.
##
## Operator types:
##   - truthful:       executes VCG exactly
##   - misreporter:    re-runs allocation on reduced capacity (ghost capacity)
##   - inflator:       adds markup to VCG payments
##   - discriminator:  favours affiliated agents by re-ordering priority
##   - ghost_bidder:   injects fictitious bid to extract surplus (trilemma)

make_operator <- function(
  type = c("truthful", "misreporter", "inflator", "discriminator", "ghost_bidder"),
  epsilon   = 0.2,    # capacity misreporting factor
  mu_markup = 0.2,    # price inflation factor
  alpha     = 0.2,    # fraction of affiliated agents
  ghost_value = NULL,  # ghost bid value (auto-calibrated if NULL)
  seed      = NULL
) {
  type <- match.arg(type)

  list(
    type        = type,
    epsilon     = epsilon,
    mu_markup   = mu_markup,
    alpha       = alpha,
    ghost_value = ghost_value,
    seed        = seed
  )
}


## Apply operator strategy to distort the market outcome
apply_operator_strategy <- function(operator, allocation, payments, env, agents,
                                    dag = NULL) {
  switch(operator$type,

    truthful = list(
      allocation = allocation,
      payments   = payments,
      surplus    = 0,
      detectable = FALSE
    ),

    misreporter = {
      # Re-run greedy allocation with reduced capacity, mimicking the
      # operator claiming lower internal max-flow
      reduced_cap <- env$capacity * (1 - operator$epsilon)
      tasks_df <- allocation %>%
        arrange(desc(realised_value)) %>%
        mutate(
          density = ifelse(allocated, realised_value, 0)
        )

      # Re-allocate with reduced capacity
      remaining <- reduced_cap
      new_alloc <- allocation
      new_alloc$allocated <- FALSE
      new_alloc$realised_value <- 0

      sorted_idx <- order(-allocation$realised_value)
      tiers <- if (!is.null(dag)) dag$critical_path_tiers else env$tiers

      for (i in sorted_idx) {
        if (!allocation$allocated[i]) next
        can_fit <- all(vapply(tiers, function(t) {
          td <- if (!is.null(dag)) dag$tier_demand[t] else 1
          remaining[t] >= td
        }, logical(1)))

        if (can_fit) {
          for (t in tiers) {
            td <- if (!is.null(dag)) dag$tier_demand[t] else 1
            remaining[t] <- remaining[t] - td
          }
          new_alloc$allocated[i] <- TRUE
          new_alloc$realised_value[i] <- allocation$realised_value[i]
        }
      }

      # Re-compute VCG payments on reduced allocation
      # Higher payments due to artificial scarcity
      allocated_mask <- new_alloc$allocated
      n_alloc <- sum(allocated_mask)
      total_cap <- sum(reduced_cap)
      scarcity_factor <- 1 + operator$epsilon * (n_alloc / max(1, sum(allocation$allocated)))
      new_payments <- payments
      new_payments[allocated_mask] <- payments[allocated_mask] * scarcity_factor
      new_payments[!allocated_mask] <- 0

      truthful_revenue <- sum(payments[allocation$allocated], na.rm = TRUE)
      new_revenue <- sum(new_payments[allocated_mask], na.rm = TRUE)

      list(
        allocation = new_alloc,
        payments   = new_payments,
        surplus    = new_revenue - truthful_revenue,
        detectable = FALSE  # agents see only own outcome
      )
    },

    inflator = {
      inflated <- payments * (1 + operator$mu_markup)
      list(
        allocation = allocation,
        payments   = inflated,
        surplus    = sum(inflated - payments, na.rm = TRUE),
        detectable = FALSE
      )
    },

    discriminator = {
      # Re-allocate: affiliated agents get priority in the greedy ordering
      n_affiliated <- ceiling(nrow(agents) * operator$alpha)
      if (!is.null(operator$seed)) set.seed(operator$seed)
      affiliated_ids <- as.character(sample(agents$agent_id, n_affiliated))

      # Re-sort: affiliated first (within each group, by original density)
      new_alloc <- allocation
      is_affil <- allocation$agent_id %in% affiliated_ids
      new_alloc$realised_value[!is_affil & allocation$allocated] <-
        allocation$realised_value[!is_affil & allocation$allocated] * 0.85
      # Welfare loss from misallocation
      welfare_loss <- sum(allocation$realised_value[allocation$allocated]) -
                      sum(new_alloc$realised_value[new_alloc$allocated])

      list(
        allocation = new_alloc,
        payments   = payments,
        surplus    = 0,
        detectable = FALSE,
        welfare_loss = max(0, welfare_loss)
      )
    },

    ghost_bidder = {
      # Implements the ghost-bid deviation from the credibility trilemma proof.
      # Operator injects a fictitious agent bid, re-runs VCG, and pockets
      # the payment difference.  Each real agent sees an outcome consistent
      # with *some* truthful execution, so the deviation is undetectable.

      allocated_tasks <- allocation %>% filter(allocated)
      if (nrow(allocated_tasks) == 0) {
        return(list(allocation = allocation, payments = payments,
                    surplus = 0, detectable = FALSE,
                    ghost_detected = FALSE,
                    deviation_amplitude = NA_real_))
      }

      # Ghost bid calibrated to displace the marginal allocated task
      marginal_idx <- which(allocation$allocated)
      if (length(marginal_idx) == 0) {
        return(list(allocation = allocation, payments = payments,
                    surplus = 0, detectable = FALSE,
                    ghost_detected = FALSE,
                    deviation_amplitude = NA_real_))
      }
      marginal_task <- allocation[marginal_idx[length(marginal_idx)], ]

      ghost_val <- operator$ghost_value %||%
        (marginal_task$realised_value * 1.1)

      # New allocation: remove the marginal task (displaced by ghost)
      new_alloc <- allocation
      displaced_idx <- marginal_idx[length(marginal_idx)]
      new_alloc$allocated[displaced_idx] <- FALSE
      new_alloc$realised_value[displaced_idx] <- 0

      # VCG payments increase for remaining agents because the ghost bid
      # raises their externality
      still_allocated <- which(new_alloc$allocated)
      new_payments <- payments
      if (length(still_allocated) > 0) {
        # Each remaining agent's payment increases by the ghost bid's
        # marginal contribution (shared across agents)
        payment_increase <- ghost_val / length(still_allocated)
        new_payments[still_allocated] <- payments[still_allocated] + payment_increase
      }
      new_payments[displaced_idx] <- 0

      surplus <- sum(new_payments[still_allocated] - payments[still_allocated], na.rm = TRUE)

      # Per-agent detectability: each agent observes only their own
      # (allocation, payment) pair. The new payment is consistent with
      # a world where demand was higher, so individually undetectable.
      per_agent_consistent <- TRUE

      list(
        allocation          = new_alloc,
        payments            = new_payments,
        surplus             = max(0, surplus),
        detectable          = FALSE,    # sealed-bid: individually undetectable
        ghost_detected      = FALSE,    # no commitment device
        ghost_value         = ghost_val,
        displaced_task      = marginal_task$task_id,
        # SDS forward-calibration logging: amplitude of the operator's
        # perturbation in valuation space (= ghost-bid magnitude).
        deviation_amplitude = ghost_val
      )
    }
  )
}
