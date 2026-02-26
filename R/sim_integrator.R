suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Integrator competition model ────────────────────────────────────
##
## Models k integrators competing to offer slices for the same service
## path. Agents choose among integrators based on price.
##
## Competition types:
##   - monopoly:     k=1, profit-maximising pricing
##   - competitive:  k>=2, Bertrand price competition with differentiation
##   - collusive:    k>=2, coordinated pricing (cartel)

make_integrator_market <- function(
  k         = 2,
  strategy  = c("competitive", "collusive"),
  marginal_cost = 0.5,
  efficiency    = 0.8
) {
  strategy <- match.arg(strategy)

  list(
    k             = k,
    strategy      = strategy,
    marginal_cost = marginal_cost,
    efficiency    = efficiency
  )
}


## Compute per-integrator slice prices under competition
compute_slice_prices <- function(integrator_market, demand, capacity_per_integrator) {
  k  <- integrator_market$k
  mc <- integrator_market$marginal_cost

  if (k == 1) {
    # Monopoly: markup ~ 50% over marginal cost
    price <- mc * 1.5
  } else if (integrator_market$strategy == "competitive") {
    # Bertrand competition with differentiated slices:
    # Markup decreasing in k, ~ O(1/k) per Proposition 2
    markup <- 0.5 / k
    price <- mc * (1 + markup)
  } else {
    # Collusive cartel: coordinate at near-monopoly level
    # Discount from monopoly reflects imperfect enforcement
    monopoly_price <- mc * 1.5
    stability_discount <- 0.1  # cartel instability
    price <- monopoly_price * (1 - stability_discount)
  }

  # Per-integrator prices (slight differentiation for k > 1)
  prices <- rep(price, k)
  if (k > 1 && integrator_market$strategy == "competitive") {
    # Add small quality differentiation
    prices <- prices * (1 + seq(-0.02, 0.02, length.out = k))
  }

  list(
    price              = price,
    prices_per_integrator = prices,
    profit_per_unit    = price - mc,
    total_capacity     = capacity_per_integrator * k,
    effective_capacity = capacity_per_integrator * k * integrator_market$efficiency
  )
}


## Agent choice among integrators based on price and expected quality
## Each agent chooses the integrator that maximises (expected_value - price).
## This drives Bertrand convergence: agents switch to cheaper integrators.
agent_choose_integrator <- function(tasks, integrator_prices, integrator_capacities) {
  k <- length(integrator_prices)
  if (k == 1) {
    return(rep(1L, nrow(tasks)))
  }

  # Each agent picks the lowest-price integrator with remaining capacity
  remaining_cap <- integrator_capacities
  choices <- integer(nrow(tasks))

  # Sort tasks by value (highest value agents choose first)
  task_order <- order(-tasks$value)

  for (idx in task_order) {
    # Agent sees all prices and picks cheapest with capacity
    affordable <- which(integrator_prices < tasks$value[idx] & remaining_cap > 0)
    if (length(affordable) == 0) {
      choices[idx] <- NA_integer_
    } else {
      best <- affordable[which.min(integrator_prices[affordable])]
      choices[idx] <- best
      remaining_cap[best] <- remaining_cap[best] - 1
    }
  }
  choices
}
