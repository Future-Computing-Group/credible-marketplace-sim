## Tests for Experiment 8 (sim_expL3): bilinear (λ, η) surface.

library(testthat)

.SIM_DIR <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), ".."),
                          mustWork = FALSE)
if (!dir.exists(file.path(.SIM_DIR, "R"))) {
  .SIM_DIR <- "[REPO_ROOT]"
}

source(file.path(.SIM_DIR, "R", "sim_credibility.R"))
source(file.path(.SIM_DIR, "R", "sim_helpers.R"))
source(file.path(.SIM_DIR, "R", "sim_operator.R"))
source(file.path(.SIM_DIR, "R", "sim_market.R"))
source(file.path(.SIM_DIR, "R", "sim_integrator.R"))
source(file.path(.SIM_DIR, "R", "sim_expL3.R"))

test_that("expL3_design factorial size matches 10 λ × 10 η × 3 dag", {
  d <- expL3_design()
  expect_equal(nrow(d), 10 * 10 * 3)
  expect_true(all(c("stake_fraction", "escrow_fraction", "operator",
                    "dag_type", "condition_id") %in% names(d)))
  expect_equal(min(d$escrow_fraction), 0)
  expect_equal(max(d$escrow_fraction), 0.9,
               info = "η=1 dropped from L3 grid (degenerate: surplus=0 by (1-η) factor); use the bang-bang test grid 0..0.9 step 0.10")
})

test_that("expL3_design uses regular grid spacing (step 0.05 in λ, step 0.10 in η)", {
  d <- expL3_design()
  lams <- sort(unique(d$stake_fraction))
  etas <- sort(unique(d$escrow_fraction))
  expect_equal(diff(lams), rep(0.05, length(lams) - 1), tolerance = 1e-9)
  expect_equal(diff(etas), rep(0.10, length(etas) - 1), tolerance = 1e-9)
})

test_that("expL3_run_single returns per-round logs with stake_fraction and escrow_fraction", {
  cond <- list(stake_fraction = 0.1, escrow_fraction = 0.3,
               operator = "ghost_bidder", dag_type = "tree")
  res <- expL3_run_single(cond, n_rounds = 3, seed = 1)
  expect_true(all(c("stake_fraction", "escrow_fraction", "net_op_surplus")
                  %in% names(res)))
  expect_equal(nrow(res), 3)
  expect_true(all(res$stake_fraction == 0.1))
  expect_true(all(res$escrow_fraction == 0.3))
})

test_that("expL3_aggregate produces one row per (dag, λ, η) cell", {
  d <- expL3_design(stake_fractions = c(0.1, 0.5),
                    escrow_fractions = c(0, 0.5),
                    topologies = c("tree"))
  results <- expL3_run_all(d, n_rounds = 3, n_seeds = 1)
  agg <- expL3_aggregate(results)
  expect_equal(nrow(agg), 2 * 2 * 1)
  expect_true(all(c("mean_op_surplus", "predicted_surplus_proportional")
                  %in% names(agg)))
})

test_that("ghost_bidder_bb produces zero deviation when Pi(eps_max) < 0", {
  ## (lambda=0.1, eta=0.5, tau=12, beta=2, v_bar=1, n=40, eps_max=1/beta=0.5):
  ## Pi(0.5) = (1-0.5)*0.1*0.5 - (1-e^{-1})*40/144
  ##         = 0.025 - 0.1756 < 0 -> adversary plays 0.
  res <- run_simulation(
    n_rounds         = 5,
    n_agents         = 40,
    dag_type         = "tree",
    load_level       = 1.0,
    operator_type    = "ghost_bidder_bb",
    operator_params  = list(escrow_fraction = 0.5, v_max = 1.0),
    credibility_type = "regulatory_sds",
    credibility_params = list(
      stake_fraction = 0.1,
      tau_audit      = 12,
      beta_audit     = 2
    ),
    value_support    = c(0, 1.0),
    force_all_active = TRUE,
    seed             = 1
  )
  expect_true("deviation_amplitude" %in% names(res))
  amps <- res$deviation_amplitude[!is.na(res$deviation_amplitude)]
  expect_true(length(amps) > 0)
  expect_equal(unique(round(amps, 6)), 0,
               info = "Pi(eps_max) < 0 region: bang-bang adversary plays 0")
  expect_true(all(res$net_op_surplus == 0 | res$net_op_surplus < 1e-6),
              info = "When adversary abstains, net operator surplus = 0")
})

test_that("ghost_bidder_bb produces eps_max deviation when Pi(eps_max) > 0", {
  ## (lambda=0.5, eta=0, tau=100, beta=2, v_bar=1, n=40, eps_max=1/beta=0.5):
  ## gain = 0.25; loss = 0.6321*40/10000 ~= 0.00253; Pi > 0 -> play 0.5.
  res <- run_simulation(
    n_rounds         = 5,
    n_agents         = 40,
    dag_type         = "tree",
    load_level       = 1.0,
    operator_type    = "ghost_bidder_bb",
    operator_params  = list(escrow_fraction = 0, v_max = 1.0),
    credibility_type = "regulatory_sds",
    credibility_params = list(
      stake_fraction = 0.5,
      tau_audit      = 100,
      beta_audit     = 2
    ),
    value_support    = c(0, 1.0),
    force_all_active = TRUE,
    seed             = 1
  )
  amps <- res$deviation_amplitude[!is.na(res$deviation_amplitude)]
  expect_true(length(amps) > 0)
  expect_equal(unique(round(amps, 6)), 0.5,
               info = "Pi(eps_max) > 0 region: bang-bang adversary plays eps_max = 1/beta = 0.5")
})

test_that("expL3_design with operator='ghost_bidder_bb' propagates to per-round logs", {
  cond <- list(stake_fraction = 0.1, escrow_fraction = 0.3,
               operator = "ghost_bidder_bb", dag_type = "tree")
  res <- expL3_run_single(cond, n_rounds = 3, seed = 1)
  expect_true(all(c("stake_fraction", "escrow_fraction", "deviation_amplitude")
                  %in% names(res)))
  expect_true(all(res$operator_type == "ghost_bidder_bb"))
})

test_that("expL3 default operator is ghost_bidder_bb at a finite tau_audit", {
  d <- expL3_design()
  expect_true(all(d$operator == "ghost_bidder_bb"),
              info = "expL3_design must default to the bang-bang adversary so Exp 8 is a genuine empirical test, not a bookkeeping identity")
  expect_true(is.finite(EXPL3_TAU_AUDIT) && EXPL3_TAU_AUDIT > 1 &&
              EXPL3_TAU_AUDIT < 1e3,
              info = paste("EXPL3_TAU_AUDIT must be finite mid-range (tau = 12); got:",
                           EXPL3_TAU_AUDIT))
})

test_that("expL3 bang-bang adversary produces BOTH zero-surplus and positive-surplus cells", {
  ## With the default tau=12 grid, the bang-bang adversary plays 0 in cells
  ## inside the credibility-deployable region (Pi(eps_max) <= 0) and eps_max
  ## outside. The heatmap should have BOTH zero-surplus and positive-surplus
  ## cells, with the boundary tracing Pi(eps_max) = 0.
  d <- expL3_design(stake_fractions  = c(0.05, 0.50),
                    escrow_fractions = c(0.0, 0.5),
                    topologies       = c("tree"))
  results <- expL3_run_all(d, n_rounds = 20, n_seeds = 2)
  agg <- expL3_aggregate(results)
  expect_true(min(agg$mean_op_surplus, na.rm = TRUE) <= 1e-6,
              info = paste("Expected at least one zero-surplus cell; min =",
                           min(agg$mean_op_surplus, na.rm = TRUE)))
  expect_true(max(agg$mean_op_surplus, na.rm = TRUE) > 0,
              info = paste("Expected at least one positive-surplus cell; max =",
                           max(agg$mean_op_surplus, na.rm = TRUE)))
})

