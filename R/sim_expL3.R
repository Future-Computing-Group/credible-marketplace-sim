suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Experiment L3: Bilinear (λ, η) Surface ───────────────────────────
##
## Empirically populates the interior of `cor:regime-bilinear-conc` in
## Trilogy 2B (the bilinear CoNC surface in stake λ and escrow fraction
## η at fixed τ → ∞, i.e. audit-free).
##
## Theory prediction (Mediator-Revenue Regime Classification, Trilogy 2B):
##   CoNC^op(λ, η) ∝ (1 − η) · λ · Γ / E[rev*]
## where Γ is the polymatroid's aggregate non-modularity gap and the
## constant is set by the operator's per-round extraction amplitude.
##
## Each cell measures the operator's per-round retained surplus under the
## ghost-bid adversary with credibility_type = "regulatory_sds" at a τ
## large enough that audits effectively never fire (τ → ∞ corner).
##
## Design:  stake_fraction × escrow_fraction × dag_type.

EXPL3_BETA_AUDIT  <- 2
EXPL3_V_BAR       <- 1.0
EXPL3_N_AGENTS    <- 40
EXPL3_TAU_AUDIT   <- 12         # Trilogy 2B Exp 8 sweeps two audit periods,
                                # tau in {12, 28} (both in the Exp 9 grid), shown
                                # side-by-side in Fig 2(a) to display the bang-bang
                                # boundary (1-eta)lambda = (1-e^-1) v_bar n /
                                # (tau^2 eps_max) sweeping with audit frequency:
                                # tau=12 -> threshold 0.351 (deployable-dominant);
                                # tau=28 -> threshold 0.065 (active-dominant).
                                # This default (12) is used by single-tau callers;
                                # the run driver overrides per-tau.


expL3_design <- function(
  stake_fractions  = c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50),
  escrow_fractions = c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90),
  topologies       = c("tree", "sp", "entangled"),
  operators        = c("ghost_bidder_bb")
) {
  conditions <- expand.grid(
    stake_fraction  = stake_fractions,
    escrow_fraction = escrow_fractions,
    operator        = operators,
    dag_type        = topologies,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(condition_id = row_number())
  conditions
}


expL3_run_single <- function(condition, n_rounds, seed) {
  ## Run a single (λ, η, dag) cell. The audit period is taken from the
  ## condition if present (multi-τ sweep), else the global default — so a
  ## design grid may include a tau_audit column to sweep τ.
  tau_cell <- if ("tau_audit" %in% names(condition)) condition$tau_audit else EXPL3_TAU_AUDIT
  res <- run_simulation(
    n_rounds         = n_rounds,
    n_agents         = EXPL3_N_AGENTS,
    dag_type         = condition$dag_type,
    load_level       = 1.0,
    operator_type    = condition$operator,
    operator_params  = list(escrow_fraction = condition$escrow_fraction),
    credibility_type = "regulatory_sds",
    credibility_params = list(
      stake_fraction = condition$stake_fraction,
      tau_audit      = tau_cell,
      beta_audit     = EXPL3_BETA_AUDIT
    ),
    value_support    = c(0, EXPL3_V_BAR),
    force_all_active = TRUE,
    seed             = seed
  )

  res %>%
    mutate(
      stake_fraction  = condition$stake_fraction,
      escrow_fraction = condition$escrow_fraction,
      tau_audit       = tau_cell
    )
}


expL3_run_all <- function(conditions, n_rounds = 100, n_seeds = 5, cores = 1) {
  ## Each (condition, seed) run calls set.seed(seed) inside run_simulation, so
  ## results are deterministic regardless of execution order — parallelizing
  ## across conditions is safe (identical output to the serial path).
  run_one_condition <- function(i) {
    cond <- conditions[i, , drop = FALSE]
    map_dfr(seq_len(n_seeds), function(s) {
      expL3_run_single(cond, n_rounds, seed = s) %>%
        mutate(condition_id = cond$condition_id)
    })
  }
  idx <- seq_len(nrow(conditions))
  if (cores > 1) {
    parts <- parallel::mclapply(idx, run_one_condition, mc.cores = cores,
                                mc.preschedule = FALSE)
    ## Surface any worker error instead of silently returning a try-error.
    errs <- vapply(parts, function(p) inherits(p, "try-error"), logical(1))
    if (any(errs)) stop("expL3_run_all: ", sum(errs), " worker(s) failed; first: ",
                        conditionMessage(attr(parts[[which(errs)[1]]], "condition")))
    dplyr::bind_rows(parts)
  } else {
    dplyr::bind_rows(lapply(idx, run_one_condition))
  }
}


expL3_aggregate <- function(results,
                            beta    = EXPL3_BETA_AUDIT %||% 2,
                            v_bar   = EXPL3_V_BAR     %||% 1.0,
                            n_agents = EXPL3_N_AGENTS %||% 40,
                            tau     = EXPL3_TAU_AUDIT %||% 12,
                            eps_max = NULL) {
  ## Per (stake × escrow × dag), report mean per-round net surplus and the
  ## closed-form bang-bang prediction at the detector scale ε_max = 1/β
  ## (cf. SmallestDetectableStake.tex, rem:detector-scale).
  ## Π(ε_max) = (1−η)·λ·ε_max − (1 − e^{−β·ε_max})·v̄·n/τ²
  ## Credibility-deployable iff Π(ε_max) ≤ 0 (rational adversary abstains).
  if (is.null(eps_max)) eps_max <- 1.0 / beta
  hazard_at_eps_max <- 1 - exp(-beta * eps_max)
  loss <- hazard_at_eps_max * v_bar * n_agents / (tau ^ 2)

  summary <- results %>%
    group_by(dag_type, stake_fraction, escrow_fraction) %>%
    summarise(
      mean_op_surplus    = mean(net_op_surplus, na.rm = TRUE),
      mean_dev_amplitude = mean(deviation_amplitude, na.rm = TRUE),
      n_obs              = n(),
      .groups = "drop"
    ) %>%
    mutate(
      predicted_profit_per_round =
        (1 - escrow_fraction) * stake_fraction * eps_max - loss,
      predicted_credible = predicted_profit_per_round <= 0,
      empirical_credible = mean_op_surplus <= 1e-6,
      ## Retained for backward-compat in any downstream plot code:
      predicted_surplus_proportional = (1 - escrow_fraction) * stake_fraction
    )
  summary
}
