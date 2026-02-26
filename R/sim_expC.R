suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Experiment C: Integrator competition and market power ─────────
##
## Varies: k integrators × integrator strategy × DAG topology × load
## Now includes active agent choice among integrators to drive
## Bertrand convergence (Proposition 2).
## Key metrics: slice prices (vs competitive benchmark), welfare,
##              agent surplus, integrator profit

expC_design <- function(
  topologies       = c("tree", "sp", "entangled"),
  load_levels      = c(low = 0.5, medium = 1.0, high = 1.5),
  k_values         = c(1, 2, 3, 5),
  strategies       = c("competitive", "collusive")
) {
  # k=1 is always monopoly
  mono <- expand.grid(
    dag_type   = topologies,
    load_name  = names(load_levels),
    k          = 1,
    strategy   = "competitive",
    stringsAsFactors = FALSE
  ) %>% as_tibble()

  # k>=2: both competitive and collusive
  multi <- expand.grid(
    dag_type   = topologies,
    load_name  = names(load_levels),
    k          = k_values[k_values >= 2],
    strategy   = strategies,
    stringsAsFactors = FALSE
  ) %>% as_tibble()

  conditions <- bind_rows(mono, multi) %>%
    mutate(
      load_level   = load_levels[load_name],
      condition_id = row_number()
    )

  conditions
}


## Custom run function that implements agent choice among integrators
expC_run_single <- function(condition, n_rounds, n_agents, seed) {
  set.seed(seed)

  dag    <- make_dag(condition$dag_type)
  env    <- make_env()
  agents <- make_agents(n_agents, seed = seed)
  env$capacity <- env$capacity / condition$load_level

  int_market <- make_integrator_market(
    k = condition$k,
    strategy = condition$strategy
  )

  prices  <- NULL
  history <- vector("list", n_rounds)

  for (r in seq_len(n_rounds)) {
    tasks <- generate_tasks(agents, lambda = 1.0,
                            deadlines = c(100, 150, 200),
                            seed = seed * 1000 + r)

    if (nrow(tasks) == 0) {
      history[[r]] <- tibble(
        round = r, welfare = 0, drop_rate = 1,
        operator_surplus = 0, penalty_applied = 0,
        net_op_surplus = 0, latency_overhead = 0,
        slice_price_adj = 0, mean_price = 0, price_sd = 0,
        n_tasks = 0, n_allocated = 0, mean_agent_payment = 0
      )
      next
    }

    # Compute integrator prices
    cap_per_int <- min(env$capacity) / int_market$k
    slice_info <- compute_slice_prices(
      int_market, demand = nrow(tasks),
      capacity_per_integrator = cap_per_int
    )

    # Agent choice among integrators
    int_cap <- rep(floor(cap_per_int), int_market$k)
    choices <- agent_choose_integrator(
      tasks, slice_info$prices_per_integrator, int_cap
    )

    # Tasks that found an integrator proceed to allocation
    tasks$integrator <- choices
    served_tasks <- tasks %>% filter(!is.na(integrator))
    unserved_tasks <- tasks %>% filter(is.na(integrator))

    # Run allocation on served tasks
    allocation <- vcg_allocate(served_tasks, env, dag)

    # Apply integrator efficiency
    allocation <- allocation %>%
      mutate(realised_value = realised_value * int_market$efficiency)

    # Add unserved tasks
    if (nrow(unserved_tasks) > 0) {
      unserved_alloc <- tibble(
        task_id        = unserved_tasks$task_id,
        agent_id       = as.character(unserved_tasks$agent_id),
        allocated      = FALSE,
        tier           = NA_character_,
        latency        = NA_real_,
        realised_value = 0,
        vcg_payment    = 0
      )
      allocation <- bind_rows(allocation, unserved_alloc)
    }

    slice_price_adj <- slice_info$price - int_market$marginal_cost

    # Tatonnement
    if (is.null(prices)) {
      prices <- setNames(rep(1.0, length(env$tiers)), env$tiers)
    }
    demand_per_tier <- sapply(env$tiers, function(t) {
      sum(allocation$allocated & grepl(t, allocation$tier), na.rm = TRUE) *
        dag$tier_demand[t]
    })
    prices <- tatonnement_step(prices, demand_per_tier, env$capacity)

    utilisation <- demand_per_tier / env$capacity
    welfare <- compute_welfare(allocation, utilisation = utilisation)

    # Agent payment = VCG payment + integrator slice markup per allocated task
    alloc_tasks  <- allocation %>% filter(allocated)
    total_payment <- sum(alloc_tasks$vcg_payment) +
                     slice_price_adj * nrow(alloc_tasks)
    mean_agent_payment <- if (nrow(alloc_tasks) > 0) {
      total_payment / n_distinct(alloc_tasks$agent_id)
    } else 0

    history[[r]] <- tibble(
      round              = r,
      welfare            = welfare,
      drop_rate          = mean(!allocation$allocated),
      operator_surplus   = 0,
      penalty_applied    = 0,
      net_op_surplus     = 0,
      latency_overhead   = 0,
      slice_price_adj    = slice_price_adj,
      mean_price         = mean(prices),
      price_sd           = sd(prices),
      n_tasks            = nrow(tasks),
      n_allocated        = sum(allocation$allocated),
      mean_agent_payment = mean_agent_payment
    )
  }

  bind_rows(history) %>%
    mutate(
      seed          = seed,
      dag_type      = condition$dag_type,
      load_level    = condition$load_level,
      operator_type = "truthful",
      credibility   = "none",
      k_integrators = condition$k,
      integrator_strategy = condition$strategy
    )
}


expC_run_all <- function(conditions, n_rounds, n_agents, n_seeds) {
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expC_run_single(cond, n_rounds, n_agents, seed = s) %>%
        mutate(condition_id = cond$condition_id,
               load_name    = cond$load_name)
    })
  })
  results
}


expC_aggregate <- function(results) {
  competitive_bench <- results %>%
    filter(k_integrators == 5, integrator_strategy == "competitive") %>%
    group_by(dag_type, load_level, seed) %>%
    summarise(
      bench_welfare = mean(welfare, na.rm = TRUE),
      bench_price   = mean(slice_price_adj, na.rm = TRUE),
      .groups = "drop"
    )

  summary <- results %>%
    group_by(dag_type, load_level, load_name,
             k_integrators, integrator_strategy, seed) %>%
    summarise(
      mean_welfare    = mean(welfare, na.rm = TRUE),
      mean_drop_rate  = mean(drop_rate, na.rm = TRUE),
      mean_slice_adj  = mean(slice_price_adj, na.rm = TRUE),
      mean_price      = mean(mean_price, na.rm = TRUE),
      price_vol       = mean(price_sd, na.rm = TRUE),
      mean_agent_pay  = mean(mean_agent_payment, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(competitive_bench,
              by = c("dag_type", "load_level", "seed")) %>%
    mutate(
      welfare_ratio   = mean_welfare / bench_welfare,
      price_markup    = mean_slice_adj / pmax(bench_price, 0.01)
    ) %>%
    group_by(dag_type, load_name, k_integrators, integrator_strategy) %>%
    summarise(
      welfare_ratio_mean   = mean(welfare_ratio, na.rm = TRUE),
      welfare_ratio_sd     = sd(welfare_ratio, na.rm = TRUE),
      welfare_ratio_ci     = list(boot_ci(welfare_ratio)),
      price_markup_mean    = mean(price_markup, na.rm = TRUE),
      agent_payment_mean   = mean(mean_agent_pay, na.rm = TRUE),
      drop_rate_mean       = mean(mean_drop_rate, na.rm = TRUE),
      n_obs                = n(),
      .groups = "drop"
    ) %>%
    mutate(
      welfare_ratio_ci_lo = map_dbl(welfare_ratio_ci, ~ .x["lo"]),
      welfare_ratio_ci_hi = map_dbl(welfare_ratio_ci, ~ .x["hi"])
    ) %>%
    select(-welfare_ratio_ci)

  summary
}
