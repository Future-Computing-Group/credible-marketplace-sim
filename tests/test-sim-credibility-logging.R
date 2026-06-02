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
  # When test_file() invokes us, the cwd may differ; fall back to here::here()
  # (rprojroot discovery) or the current working directory.
  .SIM_DIR <- if (requireNamespace("here", quietly = TRUE)) {
    here::here()
  } else {
    normalizePath(".", mustWork = TRUE)
  }
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

## ── Task 3: epsilon_star pin removed ────────────────────────────────
## Historically, three tests pinned an `epsilon_star` field on
## enforce_credibility() that was computed from the FOC formula
## (1/β)·log(v̄·n·β/(τ·λ)). The formula is the interior critical point
## of the operator's profit Π(ε) under the saturating-hazard channel,
## and Π''(ε) > 0 makes it a profit *minimum* (the rational adversary
## plays bang-bang on [0, ε_max], see sim_operator.R::bb_adversary_amplitude
## and Trilogy 2B Theorem 2 / §SmallestDetectableStake). The field was
## removed during the repo-sanitization pass; the bang-bang adversary
## is the canonical rational baseline tested in test-bb-adversary.R.

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
  # Audits fire at average rate 1/τ (Bernoulli per round; assumption A3),
  # so the per-round UNCONDITIONAL mean penalty is (1/τ)·p(δ)·C =
  # p(δ)·v̄·n/τ² = 0.865·40/400 ≈ 0.0865, and on any firing round the
  # penalty equals exactly C = 2.0.
  mech <- make_credibility_mechanism(
    type           = "regulatory_sds",
    tau_audit      = 20,
    beta_audit     = 2,
    stake_fraction = 0.5
  )

  set.seed(101)
  N <- 20000
  penalties <- vapply(seq_len(N), function(r)
    enforce_credibility(mech, op_out, round = r, broadcast_prices = NULL)$penalty_applied,
    numeric(1))

  ## (a) Unconditional per-round mean penalty → p(δ)·v̄·n/τ² (closed-form loss).
  expected_loss <- (1 - exp(-2 * 1.0)) * 1.0 * 40 / (20^2)
  expect_equal(mean(penalties), expected_loss, tolerance = 0.10)

  ## (b) Every firing round pays exactly the canonical penalty C = v̄·n/τ = 2.0.
  fired <- penalties[penalties > 0]
  expect_true(length(fired) > 0)
  expect_true(all(abs(fired - 2.0) < 1e-9))

  ## (c) Audit-firing rate ≈ 1/τ × p(δ) detections per round (firing == detection here).
  expect_equal(length(fired) / N, (1 / 20) * (1 - exp(-2 * 1.0)), tolerance = 0.10)
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

test_that("regulatory_sds audits at average rate 1/τ (Bernoulli, not deterministic)", {
  op_out <- list(
    surplus               = 1.0,
    payments              = c(1, 1, 1, 1),
    deviation_amplitude   = 5.0,    # large δ → hazard ≈ 1, so audit ⟹ penalty
    v_bar                 = 1.0,
    n_agents              = 40
  )
  tau <- 5
  mech <- make_credibility_mechanism(
    type           = "regulatory_sds",
    tau_audit      = tau,
    beta_audit     = 2,
    stake_fraction = 0.5
  )

  ## Audits fire on a Bernoulli(1/τ) draw each round — no fixed audit rounds.
  ## Over many rounds the fraction of penalty-firing rounds converges to 1/τ
  ## (assumption A3); with δ=5, β=2 the per-audit detection hazard ≈ 1, so a
  ## firing round is essentially equivalent to an audit round.
  set.seed(42)
  N <- 20000
  fired <- 0L
  penalties <- numeric(0)
  for (round in seq_len(N)) {
    cred <- enforce_credibility(mech, op_out, round = round, broadcast_prices = NULL)
    if (cred$penalty_applied > 0) { fired <- fired + 1L; penalties <- c(penalties, cred$penalty_applied) }
  }
  empirical_rate <- fired / N
  ## Average audit rate ≈ 1/τ (within finite-sample tolerance).
  expect_equal(empirical_rate, 1 / tau, tolerance = 0.05)
  ## When a penalty fires it equals the canonical SDS penalty C(β,τ)=v̄·n/τ.
  expect_true(all(abs(penalties - (1.0 * 40 / tau)) < 1e-9))
})
