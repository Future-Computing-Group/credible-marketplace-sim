#!/usr/bin/env Rscript
## Re-run Exp 7 (expL2, SDS forward calibration) under the corrected
## average-rate (Bernoulli 1/tau) audit channel. Parallelized.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble)
})
SIM_DIR <- "[REPO_ROOT]"
setwd(SIM_DIR)
source("R/sim_credibility.R"); source("R/sim_helpers.R"); source("R/sim_operator.R")
source("R/sim_market.R"); source("R/sim_integrator.R"); source("R/sim_expL2.R")

cat("=== expL2 (Exp 7) stochastic-audit re-run START:", format(Sys.time()), "===\n")
conditions <- expL2_design()
cat("conditions:", nrow(conditions), "\n")
n_cores <- max(1, min(8, parallel::detectCores() - 2)); cat("cores:", n_cores, "\n")
res <- expL2_run_all(conditions, n_rounds = 100, n_seeds = 5, cores = n_cores)
cat("result rows:", nrow(res), "\n")
summ <- expL2_aggregate(res)
saveRDS(res,  "_targets/objects/expL2_results_raw")
saveRDS(summ, "_targets/objects/expL2_summary")

## Geo-mean of empirical/predicted zero-crossing ratio (the Exp 7 headline).
if ("empirical_zero_crossing_tau" %in% names(summ) && "predicted_tau_zero_fix" %in% names(summ)) {
  r <- summ$empirical_zero_crossing_tau / summ$predicted_tau_zero_fix
  r <- r[is.finite(r) & r > 0]
  cat("geo-mean empirical/predicted zero-crossing ratio:", round(exp(mean(log(r))), 3),
      " range [", round(min(r),3), ",", round(max(r),3), "] over", length(r), "cells\n")
} else {
  cat("summary cols:", paste(names(summ), collapse=", "), "\n")
}
cat("=== expL2 stochastic-audit re-run END:", format(Sys.time()), "===\n")
