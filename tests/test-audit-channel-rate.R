## TDD: the regulatory_sds audit channel must fire at average rate 1/tau
## (assumption A3 of the SDS theorem), so the realized mean per-round penalty
## converges to the closed-form loss  hazard * v_bar * n / tau^2.
##
## The deterministic "audit every tau rounds" implementation gives
## floor(N/tau)/N * hazard * (v_bar n / tau) instead, which differs from the
## closed form at the experiment's finite horizon (N=100) whenever tau does
## not divide N. This test pins the average-rate (stochastic) convention.

library(testthat)

SIM_DIR <- "/Users/lloven/Dropbox/Research/workspace/2 Papers and Manuscripts/2026 Trustworthy Marketplace Architecture for Agents/src/simulation"
source(file.path(SIM_DIR, "R/sim_helpers.R"))
source(file.path(SIM_DIR, "R/sim_credibility.R"))

test_that("regulatory_sds realized mean penalty matches closed-form v_bar*n/tau^2 at N=100", {
  tau <- 28; beta <- 2; v_bar <- 1; n <- 40
  N <- 100; n_seeds <- 300
  delta <- 10            # large amplitude => detection hazard ~ 1, isolates the audit RATE
  closed_form_loss <- (1 - exp(-beta * delta)) * v_bar * n / (tau^2)  # ~ v_bar n / tau^2

  mech <- make_credibility_mechanism("regulatory_sds",
                           stake_fraction = 1.0,   # isolate the penalty term (gain scales by 1)
                           tau_audit = tau, beta_audit = beta)
  op <- list(surplus = 0, deviation_amplitude = delta,
             v_bar = v_bar, n_agents = n, payments = c(1, 2))

  total_penalty <- 0
  for (s in seq_len(n_seeds)) {
    set.seed(s)
    for (r in seq_len(N)) {
      res <- enforce_credibility(mech, op, round = r)
      total_penalty <- total_penalty + res$penalty_applied
    }
  }
  mean_penalty_per_round <- total_penalty / (N * n_seeds)

  ## Under the average-rate (1/tau) convention the realized mean penalty
  ## converges to the closed form. Deterministic every-tau audits give
  ## floor(100/28)/100 * (40/28) = 0.0429 (16% low); average-rate gives ~0.0510.
  rel_err <- abs(mean_penalty_per_round - closed_form_loss) / closed_form_loss
  expect_lt(rel_err, 0.05)
})
