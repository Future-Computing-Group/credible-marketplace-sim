## Tests for Experiment 9 (sim_expL4): 3D credibility-deployable (λ, η, τ) surface.

library(testthat)

.SIM_DIR <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), ".."),
                          mustWork = FALSE)
if (!dir.exists(file.path(.SIM_DIR, "R"))) {
  .SIM_DIR <- "/Users/lloven/Dropbox/Research/workspace/2 Papers and Manuscripts/2026 Trustworthy Marketplace Architecture for Agents/src/simulation"
}

source(file.path(.SIM_DIR, "R", "sim_credibility.R"))
source(file.path(.SIM_DIR, "R", "sim_helpers.R"))
source(file.path(.SIM_DIR, "R", "sim_operator.R"))
source(file.path(.SIM_DIR, "R", "sim_market.R"))
source(file.path(.SIM_DIR, "R", "sim_integrator.R"))
source(file.path(.SIM_DIR, "R", "sim_expL4.R"))

test_that("expL4_design factorial size matches 6 λ × 6 η × 6 τ × 3 dag (dense η grid)", {
  d <- expL4_design()
  expect_equal(nrow(d), 6 * 6 * 6 * 3)
  expect_true(all(c("stake_fraction", "escrow_fraction", "tau_audit",
                    "operator", "dag_type", "condition_id") %in% names(d)))
})

test_that("expL4_design default operator is ghost_bidder_bb", {
  d <- expL4_design()
  expect_true(all(d$operator == "ghost_bidder_bb"),
              info = "expL4 must default to bang-bang adversary so that the 3D credibility-deployable surface is a genuine empirical test")
})

test_that("expL4_run_single propagates (λ, η, τ) into per-round log", {
  cond <- list(stake_fraction = 0.1, escrow_fraction = 0.5,
               tau_audit = 20, operator = "ghost_bidder_bb", dag_type = "tree")
  res <- expL4_run_single(cond, n_rounds = 3, seed = 1)
  expect_true(all(c("stake_fraction", "escrow_fraction", "tau_audit",
                    "net_op_surplus") %in% names(res)))
  expect_true(all(res$tau_audit == 20))
})

test_that("expL4_aggregate predicts credibility-deployable region using bang-bang Π(ε_max=1/β)", {
  d <- expL4_design(stake_fractions = c(0.01, 0.5),
                    escrow_fractions = c(0, 0.5),
                    tau_values = c(2, 1e6),
                    topologies = c("tree"))
  results <- expL4_run_all(d, n_rounds = 5, n_seeds = 1)
  agg <- expL4_aggregate(results)
  expect_true(all(c("predicted_profit_per_round", "predicted_credible",
                    "empirical_credible") %in% names(agg)))
  ## At τ = 2 (frequent audits) and small λ, predicted credible should hold
  ## (audit deterrence active).
  small_lambda_freq_audit <- agg %>%
    dplyr::filter(stake_fraction == 0.01, tau_audit == 2)
  expect_true(all(small_lambda_freq_audit$predicted_credible),
              info = "small stake + frequent audit should be credibility-deployable")
})

test_that("expL4 bang-bang adversary: predicted/empirical credibility agreement >= 90%", {
  ## Under bang-bang, the empirical surplus is either 0 (inside credibility
  ## region) or matches Π(ε_max) within seed noise. Agreement should be
  ## essentially perfect modulo finite-sample noise at the boundary.
  d <- expL4_design(stake_fractions = c(0.05, 0.2, 0.5),
                    escrow_fractions = c(0, 0.4, 0.8),
                    tau_values = c(2, 12, 80, 1e6),
                    topologies = c("tree"))
  results <- expL4_run_all(d, n_rounds = 10, n_seeds = 2)
  agg <- expL4_aggregate(results)
  agreement <- mean(agg$predicted_credible == agg$empirical_credible)
  expect_true(agreement >= 0.90,
              info = paste("Bang-bang adversary should give >= 90% agreement; got",
                           round(agreement * 100, 1), "%"))
})
