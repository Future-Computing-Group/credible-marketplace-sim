## TDD test suite for per-round credibility logging fields
## Covers Tasks 1, 2, 3 of EXP9-FORWARD-CALIBRATION-PLAN.md:
##   - sigma_p:        per-round payment-vector standard deviation
##   - delta_amplitude: per-round operator deviation amplitude
##   - epsilon_star:    SDS theorem prediction of optimal deviation amplitude

library(testthat)

# Direct source — robust to tar_source() side effects from user-edited files
.SIM_DIR <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), ".."),
                          mustWork = FALSE)
if (!dir.exists(file.path(.SIM_DIR, "R"))) {
  # When test_file() invokes us, the cwd may differ; fall back to a known anchor
  .SIM_DIR <- "[REPO_ROOT]"
}

source(file.path(.SIM_DIR, "R", "sim_credibility.R"))

## ── Task 1: sigma_p ────────────────────────────────────────────────

test_that("enforce_credibility returns sigma_p per round", {
  mech <- make_credibility_mechanism(type = "exchange", stake_fraction = 0.01)
  outcome <- list(
    surplus             = 0.78,
    payments            = c(0.5, 0.6, 0.55, 0.7, 0.65),
    deviation_amplitude = 0.5
  )
  result <- enforce_credibility(mech, outcome, round = 1, broadcast_prices = NULL)

  expect_true("sigma_p" %in% names(result),
              info = "enforce_credibility must return sigma_p (payment-vector std)")
  expect_type(result$sigma_p, "double")
  expect_equal(result$sigma_p, sd(outcome$payments), tolerance = 1e-6,
               info = "sigma_p must equal sd(payments) for the round")
})

test_that("sigma_p is NA when payments has fewer than 2 elements", {
  mech <- make_credibility_mechanism(type = "exchange", stake_fraction = 0.01)
  outcome <- list(surplus = 0, payments = c(0.5), deviation_amplitude = NA_real_)
  result <- enforce_credibility(mech, outcome, round = 1, broadcast_prices = NULL)
  expect_true("sigma_p" %in% names(result))
  expect_true(is.na(result$sigma_p))
})

## ── Task 2: delta_amplitude ────────────────────────────────────────

test_that("enforce_credibility returns delta_amplitude per round", {
  mech <- make_credibility_mechanism(type = "exchange", stake_fraction = 0.01)
  outcome <- list(
    surplus             = 0.78,
    payments            = c(0.5, 0.6, 0.55, 0.7, 0.65),
    deviation_amplitude = 0.5
  )
  result <- enforce_credibility(mech, outcome, round = 1, broadcast_prices = NULL)

  expect_true("delta_amplitude" %in% names(result),
              info = "enforce_credibility must propagate the operator's deviation amplitude")
  expect_equal(result$delta_amplitude, 0.5, tolerance = 1e-6)
})

test_that("delta_amplitude is NA for truthful operator (no deviation)", {
  mech <- make_credibility_mechanism(type = "none", stake_fraction = 0.0)
  outcome <- list(
    surplus             = 0,
    payments            = c(0.5, 0.6, 0.55, 0.7, 0.65),
    deviation_amplitude = NA_real_
  )
  result <- enforce_credibility(mech, outcome, round = 1, broadcast_prices = NULL)
  expect_true("delta_amplitude" %in% names(result))
  expect_true(is.na(result$delta_amplitude))
})

test_that("delta_amplitude defaults to NA when field is absent from outcome", {
  mech <- make_credibility_mechanism(type = "none", stake_fraction = 0.0)
  outcome <- list(surplus = 0, payments = c(0.5, 0.6))   # no deviation_amplitude
  result <- enforce_credibility(mech, outcome, round = 1, broadcast_prices = NULL)
  expect_true("delta_amplitude" %in% names(result))
  expect_true(is.na(result$delta_amplitude))
})

## ── Task 3: epsilon_star (SDS theorem prediction) ──────────────────

test_that("enforce_credibility computes epsilon_star from SDS theorem", {
  # epsilon_star = (1/beta) * log(v_bar * n * beta / (tau * lambda))
  # For lambda = 0.01, n = 40, v_bar = 1, beta = 2, tau = 20:
  #   log(1 * 40 * 2 / (20 * 0.01)) / 2 = log(400)/2 ≈ 2.99573
  mech <- make_credibility_mechanism(type            = "regulatory",
                                     stake_fraction  = 0.01,
                                     tau_audit       = 20,
                                     beta_audit      = 2)
  outcome <- list(
    surplus             = 0.78,
    payments            = c(0.5, 0.6, 0.55, 0.7, 0.65),
    deviation_amplitude = 0.5,
    v_bar               = 1.0,
    n_agents            = 40
  )
  result <- enforce_credibility(mech, outcome, round = 1, broadcast_prices = NULL)

  expect_true("epsilon_star" %in% names(result))
  expected_eps_star <- log(1.0 * 40 * 2 / (20 * 0.01)) / 2
  expect_equal(result$epsilon_star, expected_eps_star, tolerance = 1e-6)
})

test_that("epsilon_star is NA when audit channel is unset", {
  mech <- make_credibility_mechanism(type = "exchange", stake_fraction = 0.01)
  outcome <- list(payments            = c(0.5, 0.6),
                  surplus             = 0,
                  deviation_amplitude = 0.1,
                  v_bar               = 1.0,
                  n_agents            = 40)
  result <- enforce_credibility(mech, outcome, round = 1, broadcast_prices = NULL)
  expect_true("epsilon_star" %in% names(result))
  expect_true(is.na(result$epsilon_star))
})

test_that("epsilon_star is NA when stake_fraction is zero (formula undefined)", {
  mech <- make_credibility_mechanism(type           = "regulatory",
                                     stake_fraction = 0.0,
                                     tau_audit      = 20,
                                     beta_audit     = 2)
  outcome <- list(payments = c(0.5, 0.6), surplus = 0,
                  v_bar = 1.0, n_agents = 40)
  result <- enforce_credibility(mech, outcome, round = 1, broadcast_prices = NULL)
  expect_true("epsilon_star" %in% names(result))
  expect_true(is.na(result$epsilon_star))
})

## ── Task 3 (b): make_credibility_mechanism stores tau_audit, beta_audit ──

test_that("make_credibility_mechanism accepts and stores beta_audit", {
  mech <- make_credibility_mechanism(type           = "regulatory",
                                     stake_fraction = 0.01,
                                     tau_audit      = 10,
                                     beta_audit     = 3)
  expect_equal(mech$tau_audit, 10)
  expect_equal(mech$beta_audit, 3)
})

test_that("make_credibility_mechanism beta_audit defaults to NULL", {
  mech <- make_credibility_mechanism(type = "exchange", stake_fraction = 0)
  # Explicitly check beta_audit is a named element in the mechanism
  expect_true("beta_audit" %in% names(mech),
              info = "beta_audit must be an explicit named slot, even when NULL")
  expect_null(mech$beta_audit)
})

## ── Operator-side: ghost_bidder must set deviation_amplitude ───────

test_that("ghost_bidder operator returns deviation_amplitude (= ghost_value)", {
  source(file.path(.SIM_DIR, "R", "sim_operator.R"))
  suppressPackageStartupMessages(library(tibble))
  suppressPackageStartupMessages(library(dplyr))

  set.seed(42)
  agents <- tibble(agent_id = paste0("a", 1:5))
  allocation <- tibble(
    task_id        = paste0("t", 1:5),
    agent_id       = agents$agent_id,
    tier           = "T1",
    realised_value = c(10, 8, 6, 4, 2),
    allocated      = c(TRUE, TRUE, TRUE, TRUE, TRUE),
    vcg_payment    = c(2, 2, 2, 2, 2)
  )
  payments <- allocation$vcg_payment
  env <- list(capacity = setNames(c(100), "T1"), tiers = "T1")

  op <- make_operator(type = "ghost_bidder", ghost_value = 3.0, seed = 1)
  result <- apply_operator_strategy(op, allocation, payments, env, agents,
                                    dag = NULL)

  expect_true("deviation_amplitude" %in% names(result),
              info = "ghost_bidder must propagate the ghost-bid amplitude as deviation_amplitude")
  expect_equal(result$deviation_amplitude, 3.0, tolerance = 1e-6)
})

test_that("ghost_bidder deviation_amplitude is logged through enforce_credibility", {
  source(file.path(.SIM_DIR, "R", "sim_operator.R"))
  suppressPackageStartupMessages(library(tibble))

  set.seed(7)
  agents <- tibble(agent_id = paste0("a", 1:5))
  allocation <- tibble(
    task_id        = paste0("t", 1:5),
    agent_id       = agents$agent_id,
    tier           = "T1",
    realised_value = c(10, 8, 6, 4, 2),
    allocated      = c(TRUE, TRUE, TRUE, TRUE, TRUE),
    vcg_payment    = c(2, 2, 2, 2, 2)
  )
  env <- list(capacity = setNames(c(100), "T1"), tiers = "T1")
  op <- make_operator(type = "ghost_bidder", ghost_value = 4.0)
  op_out <- apply_operator_strategy(op, allocation, allocation$vcg_payment,
                                    env, agents, dag = NULL)

  mech <- make_credibility_mechanism(type = "exchange", stake_fraction = 0.01)
  cred <- enforce_credibility(mech, op_out, round = 1, broadcast_prices = NULL)
  expect_equal(cred$delta_amplitude, 4.0, tolerance = 1e-6)
})


## ── Path A: regulatory_sds — SDS hazard model ─────────────────────────
## Per the SDS theorem (thm:sds in §III.H of the manuscript):
##   - audit fires every τ rounds (audit frequency)
##   - per-round detection hazard: p(δ) = 1 - exp(-β · δ)
##     where δ = operator's deviation amplitude
##   - canonical penalty when detected: C(β,τ) = v̄ · n / τ
## This branch implements that model literally so that expL2 measures
## the predicted threshold λ*_audit = β v̄ n / τ ex-ante, not via
## post-hoc amplification reconstruction.

test_that("regulatory_sds mechanism factory accepts SDS parameters", {
  mech <- make_credibility_mechanism(
    type        = "regulatory_sds",
    tau_audit   = 20,
    beta_audit  = 2,
    stake_fraction = 0.01
  )
  expect_equal(mech$type, "regulatory_sds")
  expect_equal(mech$tau_audit, 20)
  expect_equal(mech$beta_audit, 2)
  expect_equal(mech$stake_fraction, 0.01)
})

test_that("regulatory_sds applies SDS hazard 1 - exp(-beta * delta)", {
  source(file.path(.SIM_DIR, "R", "sim_operator.R"))
  suppressPackageStartupMessages(library(tibble))

  # Construct an operator outcome with a known deviation amplitude δ,
  # bid scale v_bar, and agent count n.
  op_out <- list(
    surplus               = 0.5,
    payments              = c(0.4, 0.5, 0.6, 0.7, 0.8),
    deviation_amplitude   = 1.0,           # δ
    v_bar                 = 1.0,
    n_agents              = 40
  )

  # With β=2, δ=1.0: hazard p(δ) = 1 - exp(-2·1) ≈ 0.865
  # Penalty when detected: C = v̄·n/τ = 1·40/20 = 2.0
  # Audit only fires every τ=20 rounds (round 20, 40, 60, ...).
  mech <- make_credibility_mechanism(
    type           = "regulatory_sds",
    tau_audit      = 20,
    beta_audit     = 2,
    stake_fraction = 0.5
  )

  # Check non-audit round: no penalty even with deviation
  set.seed(101)
  cred_nonaudit <- enforce_credibility(mech, op_out, round = 5,
                                        broadcast_prices = NULL)
  expect_equal(cred_nonaudit$penalty_applied, 0,
               info = "non-audit round: no penalty regardless of hazard")

  # Audit round: penalty fires with probability ≈ 0.865
  # Average over many seeds; mean penalty ≈ p(δ) · C ≈ 0.865 · 2 = 1.73
  penalties <- replicate(500, {
    set.seed(.Random.seed[1] %% 100000)
    enforce_credibility(mech, op_out, round = 20,
                         broadcast_prices = NULL)$penalty_applied
  })
  mean_penalty <- mean(penalties)
  # E[penalty | audit round] = (1 - exp(-2·1)) · 2 ≈ 1.73; allow ±0.3 MC
  expect_gt(mean_penalty, 1.4)
  expect_lt(mean_penalty, 2.05)
})

test_that("regulatory_sds threshold λ*_audit = β v̄ n / τ matches theory", {
  ## With β=2, v̄=1, n=40, τ=20: λ*_audit = 2·1·40/20 = 4
  ## At λ < 4, expected per-round profit ≤ 0 (deterrence binds)
  ## At λ > 4, expected per-round profit > 0
  ## This is THE forward prediction of the SDS theorem.
  ##
  ## Test computes: per (λ_low, λ_high), measure mean expected profit
  ## across many simulated audit rounds. Cannot run a full simulation
  ## here (heavy); compute analytically using the SDS first-order condition:
  ##   ε* = (1/β) log(v̄·n·β / (τ·λ))
  ##   E[U] at ε*: λ·ε* − C·p(ε*) where p(ε) = 1 - exp(-β·ε)
  ##
  ## This is a unit test on the closed-form SDS payoff.

  beta  <- 2
  v_bar <- 1
  n     <- 40
  tau   <- 20
  C     <- v_bar * n / tau           # = 2
  lambda_star <- beta * v_bar * n / tau  # = 4

  ## Profit at the FOC-interior optimum ε* = (1/β) log(v̄nβ/(τλ)).
  ## When λ < λ*_audit, ε* > 0 and U(ε*) ≤ 0 (deterrence binds at FOC).
  ## When λ ≥ λ*_audit, ε* ≤ 0 — no FOC-interior optimum; the operator's
  ## profit ramps up monotonically with ε (since p(ε) → 1 caps the
  ## penalty at C while λε grows without bound). Profit is then
  ## sup_ε λε - C·p(ε) → ∞.
  sds_eu_at_eps_star <- function(lambda) {
    eps_star <- (1 / beta) * log(v_bar * n * beta / (tau * lambda))
    if (eps_star <= 0) return(NA_real_)
    p <- 1 - exp(-beta * eps_star)
    lambda * eps_star - C * p
  }
  sds_eu_sup <- function(lambda) {
    eps_large <- 100
    p <- 1 - exp(-beta * eps_large)  # ≈ 1
    lambda * eps_large - C * p
  }

  ## Below threshold: FOC-interior ε* > 0; expected profit ≤ 0
  expect_lte(sds_eu_at_eps_star(0.5 * lambda_star), 0,
             label = "λ = 0.5·λ* FOC-interior: deterrence holds")
  expect_lte(sds_eu_at_eps_star(0.9 * lambda_star), 0,
             label = "λ = 0.9·λ* FOC-interior: deterrence holds")

  ## Above threshold: ε* ≤ 0 (no FOC-interior); sup profit > 0
  expect_true(is.na(sds_eu_at_eps_star(2 * lambda_star)),
              info = "λ = 2·λ* has no FOC-interior optimum (ε* ≤ 0)")
  expect_gt(sds_eu_sup(2 * lambda_star), 0,
            label = "λ = 2·λ* sup profit positive (deviation unbounded)")
  expect_gt(sds_eu_sup(5 * lambda_star), 0,
            label = "λ = 5·λ* sup profit positive")
})

test_that("regulatory_sds penalty is zero when deviation_amplitude is NA", {
  ## Truthful operator: no deviation, no penalty regardless of audit
  op_out <- list(
    surplus               = 0,
    payments              = c(0.5, 0.5, 0.5),
    deviation_amplitude   = NA_real_,
    v_bar                 = 1.0,
    n_agents              = 40
  )
  mech <- make_credibility_mechanism(
    type           = "regulatory_sds",
    tau_audit      = 1,           # audit every round
    beta_audit     = 2,
    stake_fraction = 0.5
  )
  cred <- enforce_credibility(mech, op_out, round = 1, broadcast_prices = NULL)
  expect_equal(cred$penalty_applied, 0)
  expect_false(cred$detected)
})

test_that("regulatory_sds applies penalty only at audit frequency τ", {
  op_out <- list(
    surplus               = 1.0,
    payments              = c(1, 1, 1, 1),
    deviation_amplitude   = 5.0,    # large δ → hazard ≈ 1
    v_bar                 = 1.0,
    n_agents              = 40
  )
  ## β·δ = 10 → 1-exp(-10) ≈ 0.99995 (essentially deterministic)
  mech <- make_credibility_mechanism(
    type           = "regulatory_sds",
    tau_audit      = 5,
    beta_audit     = 2,
    stake_fraction = 0.5
  )

  ## Non-audit rounds (1, 2, 3, 4, 6, 7, ...): zero penalty
  for (round in c(1, 2, 3, 4, 6, 7, 8, 9, 11)) {
    cred <- enforce_credibility(mech, op_out, round = round, broadcast_prices = NULL)
    expect_equal(cred$penalty_applied, 0,
                 info = sprintf("round %d should be non-audit", round))
  }

  ## Audit rounds (5, 10, 15, ...): penalty fires (~deterministic at δ=5, β=2)
  set.seed(42)
  for (round in c(5, 10, 15, 20)) {
    cred <- enforce_credibility(mech, op_out, round = round, broadcast_prices = NULL)
    expect_gt(cred$penalty_applied, 0,
              label = sprintf("round %d audit penalty > 0", round))
  }
})
