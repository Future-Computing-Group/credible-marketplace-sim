suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── DAG topologies ──────────────────────────────────────────────────
## Each topology defines per-tier demand multipliers reflecting
## structural properties of the service-dependency graph.

make_dag <- function(type = c("tree", "sp", "entangled")) {
  type <- match.arg(type)
  switch(type,
    tree = list(
      type = "tree",
      tier_demand = c(device = 1.0, edge = 1.0, cloud = 1.0),
      n_services  = 3,
      critical_path_tiers = c("device", "edge", "cloud")
    ),
    sp = list(
      type = "sp",
      tier_demand = c(device = 0.8, edge = 1.0, cloud = 1.5),
      n_services  = 4,
      critical_path_tiers = c("device", "edge", "cloud")
    ),
    entangled = list(
      type = "entangled",
      tier_demand = c(device = 1.8, edge = 1.2, cloud = 1.0),
      n_services  = 5,
      critical_path_tiers = c("device", "edge", "cloud", "device")
    )
  )
}


## ── Environment ─────────────────────────────────────────────────────

make_env <- function(
  tier_capacities = c(device = 200, edge = 300, cloud = 500),
  tier_latencies  = c(device = 5, edge = 15, cloud = 50)
) {
  list(
    tiers     = names(tier_capacities),
    capacity  = tier_capacities,
    latency   = tier_latencies
  )
}


## ── Agents ──────────────────────────────────────────────────────────

make_agents <- function(n_agents, seed = 1) {
  set.seed(seed)
  tibble(
    agent_id = seq_len(n_agents),
    trust    = rep(0.8, n_agents),
    budget   = runif(n_agents, 5, 15)
  )
}


## ── Tasks ───────────────────────────────────────────────────────────

generate_tasks <- function(agents, lambda = 1.0, deadlines = c(100, 150, 200),
                           value_support = c(1, 2), seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- nrow(agents)
  n_tasks <- rpois(n, lambda)

  tasks <- map2_dfr(agents$agent_id, n_tasks, function(aid, nt) {
    if (nt == 0) return(tibble())
    tibble(
      agent_id = aid,
      task_id  = paste0(aid, "_", seq_len(nt)),
      value    = runif(nt, value_support[1], value_support[2]),
      deadline = sample(deadlines, nt, replace = TRUE),
      lambda_l = 0.005
    )
  })
  tasks
}


## ── Latency model ───────────────────────────────────────────────────

compute_latency <- function(tier, utilisation, env) {
  base <- env$latency[tier]
  rho  <- min(0.99, max(0, utilisation))
  # M/M/1 mean sojourn time: 1/(mu - lambda) = 1/(mu*(1 - rho))
  # Scaled to ms: base_service_time / (1 - rho)
  congestion <- min(500, base / (1 - rho) - base)
  base + congestion
}

compute_critical_path_latency <- function(dag, env, utilisation) {
  tiers <- dag$critical_path_tiers
  sum(vapply(tiers, function(t) compute_latency(t, utilisation[t], env), numeric(1)))
}


## ── Value computation ───────────────────────────────────────────────

latency_discount <- function(latency, lambda_l = 0.005) {
  exp(-lambda_l * latency)
}

task_value <- function(base_value, latency, deadline, lambda_l = 0.005) {
  if (latency > deadline) return(0)
  base_value * latency_discount(latency, lambda_l)
}


## ── Welfare computation ─────────────────────────────────────────────

compute_welfare <- function(task_outcomes, gamma_c = 0.05, utilisation = NULL) {
  total_value <- sum(task_outcomes$realised_value, na.rm = TRUE)
  congestion_penalty <- 0
  if (!is.null(utilisation)) {
    congestion_penalty <- gamma_c * sum(pmax(utilisation - 1, 0)^2)
  }
  total_value - congestion_penalty
}


## ── Bootstrap confidence interval helper ────────────────────────────

boot_ci <- function(x, n_boot = 1000, conf = 0.95) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(c(lo = NA_real_, hi = NA_real_))
  means <- replicate(n_boot, mean(sample(x, replace = TRUE)))
  alpha <- (1 - conf) / 2
  qs <- quantile(means, c(alpha, 1 - alpha))
  c(lo = unname(qs[1]), hi = unname(qs[2]))
}
