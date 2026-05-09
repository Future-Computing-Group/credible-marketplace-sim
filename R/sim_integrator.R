suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Integrator competition model ────────────────────────────────────
##
## Models k integrators competing to offer slices for the same service
## path.  Two regimes:
##
##   transport_cost = 0  (legacy):  Bertrand with hard-coded markup
##   transport_cost > 0  (Salop):   Circular-city differentiated-product
##                                  model (Proposition 2)
##
## Competition types:
##   - monopoly:     k=1, profit-maximising pricing
##   - competitive:  k>=2, Bertrand/Salop Nash equilibrium
##   - collusive:    k>=2, coordinated pricing (cartel)


## ── Circular distance on [0,1) ──────────────────────────────────────

circular_distance <- function(a, b) {
  d <- abs(a - b)
  pmin(d, 1 - d)
}


make_integrator_market <- function(
  k              = 2,
  strategy       = c("competitive", "collusive"),
  marginal_cost  = 0.5,
  efficiency     = 0.8,
  transport_cost = 0
) {
  strategy <- match.arg(strategy)

  # Equidistant positions on the unit circle
  positions <- if (transport_cost > 0) {
    seq(0, 1 - 1/k, length.out = k)
  } else {
    NULL
  }

  list(
    k              = k,
    strategy       = strategy,
    marginal_cost  = marginal_cost,
    efficiency     = efficiency,
    transport_cost = transport_cost,
    positions      = positions
  )
}


## Compute per-integrator slice prices under competition
compute_slice_prices <- function(integrator_market, demand, capacity_per_integrator) {
  k  <- integrator_market$k
  mc <- integrator_market$marginal_cost
  tc <- integrator_market$transport_cost %||% 0

  if (tc > 0) {
    ## ── Salop pricing ───────────────────────────────────────────────
    if (k == 1) {
      # Monopoly on circle: p = mc + t (extract full transport surplus)
      price <- mc + tc
    } else if (integrator_market$strategy == "competitive") {
      # Salop Nash equilibrium: p* = mc + t/k
      price <- mc + tc / k
    } else {
      # Collusive: coordinate near monopoly, discounted by instability
      monopoly_price <- mc + tc
      stability_discount <- 0.1
      price <- monopoly_price * (1 - stability_discount)
    }
    # Under Salop, differentiation comes from transport cost, not price noise
    prices <- rep(price, k)

  } else {
    ## ── Legacy pricing (backward compat for Exp J) ──────────────────
    if (k == 1) {
      price <- mc * 1.5
    } else if (integrator_market$strategy == "competitive") {
      markup <- 0.5 / k
      price <- mc * (1 + markup)
    } else {
      monopoly_price <- mc * 1.5
      stability_discount <- 0.1
      price <- monopoly_price * (1 - stability_discount)
    }
    prices <- rep(price, k)
    if (k > 1 && integrator_market$strategy == "competitive") {
      prices <- prices * (1 + seq(-0.02, 0.02, length.out = k))
    }
  }

  list(
    price                 = price,
    prices_per_integrator = prices,
    profit_per_unit       = price - mc,
    total_capacity        = capacity_per_integrator * k,
    effective_capacity    = capacity_per_integrator * k * integrator_market$efficiency
  )
}


## Agent choice among integrators
##
## Legacy (transport_cost == 0 or integrator_market NULL):
##   Returns integer vector of integrator indices (NA = not served).
##
## Salop (transport_cost > 0):
##   Returns list(choices = integer_vec, transport_costs = numeric_vec).
##   Total cost for agent at location l choosing integrator j:
##     price_j + t * circular_distance(l, position_j)
##   Agent excluded if min(total_cost) > agent value.
agent_choose_integrator <- function(tasks, integrator_prices, integrator_capacities,
                                    integrator_market = NULL) {
  tc <- if (!is.null(integrator_market)) integrator_market$transport_cost %||% 0 else 0

  if (tc > 0) {
    ## ── Salop choice model ────────────────────────────────────────
    k <- integrator_market$k
    positions <- integrator_market$positions
    t_cost <- integrator_market$transport_cost

    n <- nrow(tasks)
    remaining_cap <- integrator_capacities
    choices <- integer(n)
    transport_costs <- numeric(n)
    task_order <- order(-tasks$value)

    for (idx in task_order) {
      loc <- tasks$location[idx]
      val <- tasks$value[idx]

      # Compute total cost to each integrator
      dists <- circular_distance(loc, positions)
      total_costs <- integrator_prices + t_cost * dists
      # Only consider integrators with remaining capacity
      available <- which(remaining_cap > 0)

      if (length(available) == 0) {
        choices[idx] <- NA_integer_
        transport_costs[idx] <- NA_real_
        next
      }

      best_avail <- available[which.min(total_costs[available])]
      best_cost <- total_costs[best_avail]

      if (best_cost > val) {
        # Agent excluded: cheapest option exceeds value
        choices[idx] <- NA_integer_
        transport_costs[idx] <- NA_real_
      } else {
        choices[idx] <- best_avail
        transport_costs[idx] <- t_cost * dists[best_avail]
        remaining_cap[best_avail] <- remaining_cap[best_avail] - 1
      }
    }

    return(list(choices = choices, transport_costs = transport_costs))

  } else {
    ## ── Legacy choice model (unchanged) ──────────────────────────
    k <- length(integrator_prices)
    if (k == 1) {
      return(rep(1L, nrow(tasks)))
    }

    remaining_cap <- integrator_capacities
    choices <- integer(nrow(tasks))
    task_order <- order(-tasks$value)

    for (idx in task_order) {
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
}
