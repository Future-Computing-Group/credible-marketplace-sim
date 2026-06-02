#!/usr/bin/env Rscript
## Re-run Exp 8 (expL3 credibility-deployable surface) at all six audit
## periods on the dense regular grid, under the corrected average-rate
## (Bernoulli 1/tau) audit channel. Replaces the earlier t12/t28/multitau
## split with a single combined summary. Parallelized.

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

cat("=== expL3 (Exp 8) all-6-tau stochastic-audit re-run START:", format(Sys.time()), "===\n")
base <- expL3_design()                      # dense 10x10x3, ghost_bidder_bb
taus <- c(2, 5, 12, 28, 80, 1e6)
base_no_tau <- base %>% select(-any_of(c("condition_id", "tau_audit")))
conditions <- tidyr::crossing(base_no_tau, tau_audit = taus) %>%
  mutate(condition_id = row_number())
cat("conditions:", nrow(conditions), " taus:", paste(taus, collapse=","), "\n")

n_cores <- max(1, min(8, parallel::detectCores() - 2)); cat("cores:", n_cores, "\n")
results_raw <- expL3_run_all(conditions, n_rounds = 100, n_seeds = 5, cores = n_cores)
cat("result rows:", nrow(results_raw), "\n")

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
saveRDS(summary, "_targets/objects/expL3_summary_all6")
cat("summary rows:", nrow(summary), "\n")
cat("membership agreement:", round(100*mean(summary$predicted_credible==summary$empirical_credible),2), "%\n")

## Magnitude check: in active cells, does empirical surplus match Pi(eps_max)?
act <- summary %>% filter(predicted_profit_per_round > 1e-9) %>%
  mutate(abs_err = abs(mean_op_surplus - predicted_profit_per_round))
if (nrow(act) > 0) {
  cat("active cells:", nrow(act),
      " mean |emp - Pi(eps_max)|:", round(mean(act$abs_err),4),
      " max:", round(max(act$abs_err),4),
      " mean rel err:", round(mean(act$abs_err/pmax(act$predicted_profit_per_round,1e-6)),3), "\n")
}
cat("=== expL3 all-6-tau re-run END:", format(Sys.time()), "===\n")
