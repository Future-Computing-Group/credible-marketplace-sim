suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Experiment L4: 3D Credibility-Deployable Surface (λ, η, τ) ────────
##
## Empirically populates the interior of `cor:credibility-deployable-surface`
## in Trilogy 2B.  The 3D surface combines the bilinear (λ, η) regime
## classification of L3 with the SDS audit dimension τ of L2.
##
## Theory: the operator's per-round expected net profit is
##   Π(λ, η, τ) ≈ (1 − η)·λ·ε − (β·v̄·n / τ) · (1 − e^{−βε})
## and ε-credibility is achievable iff Π ≤ ε.  We test this by sweeping
## a coarse 3D grid and identifying the surface where Π crosses zero
## (per τ slice).
##
## Design:  stake_fraction × escrow_fraction × tau_audit × dag_type.
## Grid: 6 × 6 × 6 × 3 = 648 conditions. The η dimension is denser
## than the original 4-point coarse grid and now includes η = 1
## explicitly so the audit-free knife-edge (Trilogy 2B Exp. 5)
## appears as a row in the heatmap rather than a referenced corner
## that is not in the data.

EXPL4_BETA_AUDIT  <- 2
EXPL4_V_BAR       <- 1.0
EXPL4_N_AGENTS    <- 40


expL4_design <- function(
  stake_fractions  = c(0.01, 0.05, 0.1, 0.2, 0.35, 0.5),
  escrow_fractions = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
  tau_values       = c(2, 5, 12, 28, 80, 1e6),       # last value ≈ τ→∞
  topologies       = c("tree", "sp", "entangled"),
  operators        = c("ghost_bidder_bb")
) {
  conditions <- expand.grid(
    stake_fraction  = stake_fractions,
    escrow_fraction = escrow_fractions,
    tau_audit       = tau_values,
    operator        = operators,
    dag_type        = topologies,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(condition_id = row_number())
  conditions
}


expL4_run_single <- function(condition, n_rounds, seed) {
  res <- run_simulation(
    n_rounds         = n_rounds,
    n_agents         = EXPL4_N_AGENTS,
    dag_type         = condition$dag_type,
    load_level       = 1.0,
    operator_type    = condition$operator,
    operator_params  = list(escrow_fraction = condition$escrow_fraction),
    credibility_type = "regulatory_sds",
    credibility_params = list(
      stake_fraction = condition$stake_fraction,
      tau_audit      = condition$tau_audit,
      beta_audit     = EXPL4_BETA_AUDIT
    ),
    value_support    = c(0, EXPL4_V_BAR),
    force_all_active = TRUE,
    seed             = seed
  )

  res %>%
    mutate(
      stake_fraction  = condition$stake_fraction,
      escrow_fraction = condition$escrow_fraction,
      tau_audit       = condition$tau_audit
    )
}


expL4_run_all <- function(conditions, n_rounds = 100, n_seeds = 5) {
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expL4_run_single(cond, n_rounds, seed = s) %>%
        mutate(condition_id = cond$condition_id)
    })
  })
  results
}


expL4_aggregate <- function(results,
                            beta = EXPL4_BETA_AUDIT,
                            v_bar = EXPL4_V_BAR,
                            n_agents = EXPL4_N_AGENTS,
                            eps_max = NULL) {
  ## Per (stake × escrow × tau × dag), report mean net surplus, plus the
  ## closed-form predicted credibility-region boundary under the BANG-BANG
  ## rational adversary at ε_max = 1/β (detector scale where linear and
  ## saturating regimes of the hazard cross; cf. SmallestDetectableStake.tex,
  ## bang-bang derivation).
  if (is.null(eps_max)) eps_max <- 1.0 / beta
  hazard_at_eps_max <- 1 - exp(-beta * eps_max)   # = 1 - e^{-1} for β=2, ε_max=0.5
  summary <- results %>%
    group_by(dag_type, stake_fraction, escrow_fraction, tau_audit) %>%
    summarise(
      mean_op_surplus = mean(net_op_surplus, na.rm = TRUE),
      n_obs           = n(),
      .groups = "drop"
    ) %>%
    mutate(
      ## Closed-form per-round expected operator profit at the bang-bang
      ## amplitude ε_max = 1/β:
      ##   Π(ε_max) = (1 − η)·λ·ε_max − (1 − exp(−β·ε_max))·v̄·n / τ²
      ## Credibility-deployable iff Π(ε_max) ≤ 0; outside this region the
      ## rational adversary plays ε_max and operator captures Π(ε_max).
      predicted_profit_per_round =
        (1 - escrow_fraction) * stake_fraction * eps_max -
          hazard_at_eps_max * v_bar * n_agents / (tau_audit ^ 2),
      predicted_credible = predicted_profit_per_round <= 0,
      empirical_credible = mean_op_surplus <= 1e-6
    )
  summary
}
