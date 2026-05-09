## TDD test suite for Experiment 9b (expL2): SDS forward calibration.
## Covers Task 4 of EXP9-FORWARD-CALIBRATION-PLAN.md.
##
## The expL2 family extends Exp 9 with an audit-frequency dimension (tau_audit)
## and per-round logs of sigma_p, delta_amplitude, epsilon_star.

library(testthat)

.SIM_DIR <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), ".."),
                          mustWork = FALSE)
if (!dir.exists(file.path(.SIM_DIR, "R"))) {
  .SIM_DIR <- "[REPO_ROOT]"
}

## Source the simulator stack used by expL2.
source(file.path(.SIM_DIR, "R", "sim_credibility.R"))
source(file.path(.SIM_DIR, "R", "sim_helpers.R"))
source(file.path(.SIM_DIR, "R", "sim_operator.R"))
source(file.path(.SIM_DIR, "R", "sim_market.R"))
source(file.path(.SIM_DIR, "R", "sim_integrator.R"))
source(file.path(.SIM_DIR, "R", "sim_expL2.R"))

## ── expL2_design ────────────────────────────────────────────────────

test_that("expL2_design produces the right factorial (default)", {
  d <- expL2_design()
  required_cols <- c("stake_fraction", "tau_audit", "operator", "dag_type",
                     "condition_id")
  expect_true(all(required_cols %in% names(d)),
              info = paste("missing:",
                           paste(setdiff(required_cols, names(d)), collapse = ", ")))
  # 5 stakes × 6 tau × 1 operator (ghost_bidder only) × 3 topologies = 90
  expect_equal(nrow(d), 5 * 6 * 1 * 3)
  # Operator restricted to ghost_bidder
  expect_setequal(unique(d$operator), "ghost_bidder")
  # Tau frequencies span the plan's spec
  expect_setequal(unique(d$tau_audit), c(1, 5, 10, 20, 40, 80))
  # Stake fractions exclude 0 (formula undefined) and 1 (degenerate)
  expect_setequal(unique(d$stake_fraction), c(0.01, 0.05, 0.1, 0.25, 0.5))
  # Topologies match the rest of the suite
  expect_setequal(unique(d$dag_type), c("tree", "sp", "entangled"))
  # condition_id is a 1..N row index
  expect_equal(d$condition_id, seq_len(nrow(d)))
})

test_that("expL2_design accepts custom dimensions", {
  d <- expL2_design(stake_fractions = c(0.01),
                    tau_values      = c(10, 20),
                    topologies      = c("tree"))
  expect_equal(nrow(d), 1 * 2 * 1 * 1)
})

## ── expL2_run_single ────────────────────────────────────────────────

test_that("expL2_run_single returns per-round logs with sigma_p / delta / epsilon_star", {
  cond <- list(stake_fraction = 0.01, tau_audit = 20,
               operator = "ghost_bidder", dag_type = "tree")
  result <- expL2_run_single(cond, n_rounds = 5, seed = 1)
  required <- c("sigma_p", "delta_amplitude", "epsilon_star",
                "stake_fraction", "tau_audit")
  expect_true(all(required %in% names(result)),
              info = paste("missing:",
                           paste(setdiff(required, names(result)), collapse = ", ")))
  expect_equal(nrow(result), 5)
  # epsilon_star is finite for stake > 0 (audit channel active)
  expect_true(all(is.finite(result$epsilon_star)),
              info = "epsilon_star must be finite when audit channel is active")
})

## ── expL2_aggregate ─────────────────────────────────────────────────

test_that("expL2_aggregate computes predicted and empirical tau_star per condition", {
  d <- expL2_design(stake_fractions = c(0.01),
                    tau_values      = c(1, 5, 10, 20, 40, 80),
                    topologies      = c("tree"))
  results <- expL2_run_all(d, n_rounds = 5, n_seeds = 1)
  agg <- expL2_aggregate(results)
  expect_true("predicted_tau_star" %in% names(agg))
  expect_true("empirical_zero_crossing_tau" %in% names(agg))
  # predicted_tau_star = beta * v_bar * n / lambda
  # With default beta=2, v_bar=1, n=40, lambda=0.01 → 8000
  expect_equal(unique(agg$predicted_tau_star), 2 * 1 * 40 / 0.01,
               tolerance = 1e-6)
})

test_that("expL2_aggregate preserves grouping by stake_fraction × dag_type", {
  d <- expL2_design(stake_fractions = c(0.01, 0.5),
                    tau_values      = c(10, 20),
                    topologies      = c("tree", "sp"))
  results <- expL2_run_all(d, n_rounds = 3, n_seeds = 1)
  agg <- expL2_aggregate(results)
  # 2 stakes × 2 topologies = 4 groups (one row per (stake, dag) pair)
  expect_equal(nrow(agg), 2 * 2)
})
