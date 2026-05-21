#!/usr/bin/env Rscript
## One-off driver: Exp 8 second panel at tau = 28 (parallel).
## Saves to expL3_summary_t28 / expL3_results_raw_t28 so the existing
## tau=12 objects (expL3_summary_t12) are left untouched. The combined
## two-tau Fig 2 is assembled by plots_expL3.R::save_expL3_two_tau_plot.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble)
  library(ggplot2); library(patchwork); library(scales)
})

SIM_DIR <- "/Users/lloven/Dropbox/Research/workspace/2 Papers and Manuscripts/2026 Trustworthy Marketplace Architecture for Agents/src/simulation"
setwd(SIM_DIR)

source("R/sim_credibility.R")
source("R/sim_helpers.R")
source("R/sim_operator.R")
source("R/sim_market.R")
source("R/sim_integrator.R")
source("R/sim_expL3.R")

## Override the audit period to 28 for this run (run_single and aggregate
## both read the global EXPL3_TAU_AUDIT).
EXPL3_TAU_AUDIT <- 28

cat("=== expL3 tau=28 START:", format(Sys.time()), "===\n")
conditions <- expL3_design()
cat("conditions:", nrow(conditions), " tau:", EXPL3_TAU_AUDIT, "\n")

n_cores <- max(1, min(8, parallel::detectCores() - 2))
cat("parallel cores:", n_cores, "\n")
results_raw <- expL3_run_all(conditions, n_rounds = 100, n_seeds = 5, cores = n_cores)
cat("results rows:", nrow(results_raw), "\n")

summary <- expL3_aggregate(results_raw)   # uses EXPL3_TAU_AUDIT = 28
cat("summary rows:", nrow(summary), "\n")
cat("agreement:", round(100 * mean(summary$predicted_credible == summary$empirical_credible), 2), "%\n")

saveRDS(results_raw, "_targets/objects/expL3_results_raw_t28")
saveRDS(summary,     "_targets/objects/expL3_summary_t28")
cat("=== expL3 tau=28 END:", format(Sys.time()), "===\n")
