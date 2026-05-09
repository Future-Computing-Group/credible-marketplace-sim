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
