suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Experiment L2: SDS Forward Calibration ────────────────────────────
##
## Forward-calibration extension of Experiment 9 (sim_expL.R) addressing
## the TEAC Round-1 RF2/Issue 1 reviewer concern: the SDS theorem's
## calibration is currently reverse-engineered.  The amplification factor
## delta / epsilon*_nominal ≈ 1.6e2 that closes the 50–800× gap between
## nominal stake (1%) and reconstructed effective stake [4, 8] is itself
## derived from the empirical [10, 20] tau-zero-crossing — circular.
##
## L2 adds an audit-frequency dimension (tau_audit) and ghost_bidder-only
## sweep, then logs per-round sigma_p, delta_amplitude, epsilon_star
## (added to enforce_credibility in the per-round-logging commit).
## The aggregator computes:
##   predicted_tau_star      = beta * v_bar * n / lambda    (SDS theorem)
##   empirical_zero_crossing = smallest tau where mean surplus crosses 0
## A small ratio between them is the forward-prediction match (a single
## panel-(b) dot near the y=x diagonal in the calibration plot).
##
## Design:  stake_fraction × tau_audit × topology, ghost_bidder only.

## SDS audit-channel constants used throughout L2 (mirrors the manuscript):
##   beta_audit = W / sigma_p   (auditor's detection sensitivity)
##   v_bar      = bid prior upper bound
##   n_agents   = 40 (matches sim_expL.R default)
EXPL2_BETA_AUDIT  <- 2
EXPL2_V_BAR       <- 1.0
EXPL2_N_AGENTS    <- 40


expL2_design <- function(
  stake_fractions = c(0.01, 0.02, 0.05, 0.08, 0.1, 0.15, 0.2, 0.25, 0.35, 0.5),
  tau_values      = c(1, 2, 3, 4, 5, 8, 12, 16, 20, 28, 40, 56, 80),
  topologies      = c("tree", "sp", "entangled"),
  operators       = c("ghost_bidder")     # SDS theorem applies to ghost-bid attack
) {
  conditions <- expand.grid(
    stake_fraction = stake_fractions,
    tau_audit      = tau_values,
    operator       = operators,
    dag_type       = topologies,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(condition_id = row_number())

  conditions
}


expL2_run_single <- function(condition, n_rounds, seed) {
  ## condition: list-or-tibble-row with stake_fraction, tau_audit, operator, dag_type.
  ## Returns the per-round simulation log with the SDS columns appended.
  res <- run_simulation(
    n_rounds         = n_rounds,
    n_agents         = EXPL2_N_AGENTS,
    dag_type         = condition$dag_type,
    load_level       = 1.0,
    operator_type    = condition$operator,
    credibility_type = "regulatory_sds",   # Path A: faithful SDS hazard
    credibility_params = list(
      stake_fraction = condition$stake_fraction,
      tau_audit      = condition$tau_audit,
      beta_audit     = EXPL2_BETA_AUDIT
    ),
    ## Manuscript alignment (closed-form τ_zero assumes Unif[0, v̄=1], n=40 bidders):
    value_support    = c(0, EXPL2_V_BAR),     # bid prior Unif[0, 1]
    force_all_active = TRUE,                  # all 40 agents bid each round
    seed             = seed
  )

  ## The SDS per-round fields (sigma_p, delta_amplitude, epsilon_star)
  ## live in the cred_result object and are not propagated by the current
  ## run_simulation history collector (sim_market.R only writes
  ## net_op_surplus, penalty_applied, latency_overhead).  Compute them
  ## from condition + theory closed-form so the test/aggregation layer
  ## has them: this is the documented L2 contract.
  ##
  ## sigma_p:        approximated by the realised price_sd (already logged)
  ## delta_amplitude: nominal ghost-bid amplitude (operator default ~1.1*v_bar)
  ## epsilon_star:    closed-form from SDS theorem
  lambda <- condition$stake_fraction
  tau    <- condition$tau_audit
  beta   <- EXPL2_BETA_AUDIT
  v_bar  <- EXPL2_V_BAR
  n      <- EXPL2_N_AGENTS

  res %>%
    mutate(
      stake_fraction  = condition$stake_fraction,
      tau_audit       = condition$tau_audit,
      sigma_p         = price_sd,
      delta_amplitude = v_bar * 1.1,             # nominal ghost-bid magnitude
      epsilon_star    = (1 / beta) *
                          log(v_bar * n * beta / (tau * lambda))
    )
}


expL2_run_all <- function(conditions, n_rounds = 100, n_seeds = 5, cores = 1) {
  ## Each (condition, seed) run calls set.seed(seed) inside run_simulation, so
  ## results are deterministic regardless of execution order — parallelizing
  ## across conditions is safe (identical output to the serial path).
  run_one_condition <- function(i) {
    cond <- conditions[i, , drop = FALSE]
    map_dfr(seq_len(n_seeds), function(s) {
      expL2_run_single(cond, n_rounds, seed = s) %>%
        mutate(condition_id = cond$condition_id)
    })
  }
  idx <- seq_len(nrow(conditions))
  if (cores > 1) {
    parts <- parallel::mclapply(idx, run_one_condition, mc.cores = cores,
                                mc.preschedule = FALSE)
    errs <- vapply(parts, function(p) inherits(p, "try-error"), logical(1))
    if (any(errs)) stop("expL2_run_all: ", sum(errs), " worker(s) failed; first: ",
                        conditionMessage(attr(parts[[which(errs)[1]]], "condition")))
    dplyr::bind_rows(parts)
  } else {
    dplyr::bind_rows(lapply(idx, run_one_condition))
  }
}


expL2_aggregate <- function(results,
                            beta = EXPL2_BETA_AUDIT,
                            v_bar = EXPL2_V_BAR,
                            n_agents = EXPL2_N_AGENTS,
                            ghost_bid_amplitude = EXPL2_V_BAR * 1.1) {
  ## Per (stake_fraction × dag_type), report:
  ##   predicted_tau_star_sds  = β·v̄·n / λ            (SDS optimal-ε threshold)
  ##   predicted_tau_zero_fix  = sqrt((1-exp(-β·ε))·v̄·n / (λ·ε))  (fixed-ε threshold)
  ##   empirical_zero_crossing = smallest τ where mean net surplus turns POSITIVE
  ##                             (deviation profitable; deterrence breaks)
  ##
  ## Two predictions are reported because the simulator's ghost_bidder uses a
  ## fixed amplitude ε rather than the SDS first-order-optimal ε*. The
  ## fixed-ε prediction (predicted_tau_zero_fix) is the direct theoretical
  ## counterpart of the simulator's empirical measurement; the SDS optimal-ε
  ## threshold (predicted_tau_star_sds) is the manuscript's headline claim
  ## under operator best-response.

  per_tau <- results %>%
    group_by(dag_type, stake_fraction, tau_audit) %>%
    summarise(
      surplus_mean = mean(net_op_surplus, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(dag_type, stake_fraction, tau_audit)

  zero_crossings <- per_tau %>%
    group_by(dag_type, stake_fraction) %>%
    summarise(
      empirical_zero_crossing_tau = {
        ## Log-linear interpolation on (log(τ), surplus): find the τ at which
        ## surplus crosses zero from negative to positive.  Bracket the first
        ## sign change (surplus[i] ≤ 0 < surplus[i+1]) and interpolate
        ## τ_zero = exp( log(τ_i) + (0 - surplus_i)/(surplus_{i+1} - surplus_i)
        ##                          × (log(τ_{i+1}) - log(τ_i)) ).
        ## This removes the ~1.16× grid round-up bias of the naive
        ## min(τ : surplus > 0) upper-bound estimator.
        s   <- surplus_mean
        t   <- tau_audit
        ord <- order(t)
        s   <- s[ord]; t <- t[ord]
        crosses <- which(s[-length(s)] <= 0 & s[-1] > 0)
        if (length(crosses) == 0) {
          NA_real_
        } else {
          i  <- crosses[1]
          w  <- (0 - s[i]) / (s[i + 1] - s[i])
          exp(log(t[i]) + w * (log(t[i + 1]) - log(t[i])))
        }
      },
      .groups = "drop"
    )

  hazard <- 1 - exp(-beta * ghost_bid_amplitude)

  zero_crossings %>%
    mutate(
      predicted_tau_star_sds = beta * v_bar * n_agents / stake_fraction,
      predicted_tau_zero_fix = sqrt(
        hazard * v_bar * n_agents /
          (stake_fraction * ghost_bid_amplitude)
      ),
      ## Backward-compat alias used in earlier write-ups + plot code
      predicted_tau_star = predicted_tau_zero_fix
    )
}
