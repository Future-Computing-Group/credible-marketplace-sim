#!/usr/bin/env Rscript
## Dense-grid credibility-deployable surface at the 4 audit periods NOT
## already computed: tau in {2, 5, 80, 1e6}. (tau=12 and tau=28 already
## exist as expL3_summary_t12 / _t28 on the same dense grid.)
## Saves expL3_summary_multitau (the 4 new tau, dense grid, with per-tau
## bang-bang prediction + agreement columns).

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble)
})

## Locate the repo root portably (see comment in run_expL2_stochastic.R).
SIM_DIR <- Sys.getenv("SIM_DIR", "")
if (!nzchar(SIM_DIR)) {
  SIM_DIR <- if (requireNamespace("here", quietly = TRUE)) {
    here::here()
  } else {
    normalizePath(".", mustWork = TRUE)
  }
}
setwd(SIM_DIR)
source("R/sim_credibility.R"); source("R/sim_helpers.R"); source("R/sim_operator.R")
source("R/sim_market.R"); source("R/sim_integrator.R"); source("R/sim_expL3.R")

cat("=== expL3 multi-tau START:", format(Sys.time()), "===\n")

## Dense grid (matches expL3_design defaults) crossed with the 4 new tau.
base <- expL3_design()                    # dense 10x10x3, operator ghost_bidder_bb
new_taus <- c(2, 5, 80, 1e6)
base_no_tau <- base %>% select(-any_of(c("condition_id", "tau_audit")))
conditions <- tidyr::crossing(base_no_tau, tau_audit = new_taus) %>%
  mutate(condition_id = row_number())
cat("conditions:", nrow(conditions), " taus:", paste(new_taus, collapse=","), "\n")

n_cores <- max(1, min(8, parallel::detectCores() - 2))
cat("parallel cores:", n_cores, "\n")
results_raw <- expL3_run_all(conditions, n_rounds = 100, n_seeds = 5, cores = n_cores)
cat("results rows:", nrow(results_raw), "\n")

## Per-(dag, lambda, eta, tau) aggregation with the bang-bang prediction.
beta <- EXPL3_BETA_AUDIT; v_bar <- EXPL3_V_BAR; n_ag <- EXPL3_N_AGENTS
eps_max <- 1/beta; hazard <- 1 - exp(-beta*eps_max)
summary <- results_raw %>%
  group_by(dag_type, stake_fraction, escrow_fraction, tau_audit) %>%
  summarise(mean_op_surplus = mean(net_op_surplus, na.rm = TRUE),
            mean_dev_amplitude = mean(deviation_amplitude, na.rm = TRUE),
            n_obs = n(), .groups = "drop") %>%
  mutate(predicted_profit_per_round =
           (1 - escrow_fraction) * stake_fraction * eps_max -
             hazard * v_bar * n_ag / (tau_audit^2),
         predicted_credible = predicted_profit_per_round <= 0,
         empirical_credible = mean_op_surplus <= 1e-6)
cat("summary rows:", nrow(summary), "\n")
cat("agreement:", round(100*mean(summary$predicted_credible == summary$empirical_credible),2), "%\n")

saveRDS(summary, "_targets/objects/expL3_summary_multitau")
cat("=== expL3 multi-tau END:", format(Sys.time()), "===\n")
