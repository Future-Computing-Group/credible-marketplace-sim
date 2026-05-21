#!/usr/bin/env Rscript
## Standalone driver for Exp R5 (TEAC R-5 panels).
## Bypasses targets to avoid metadata race with concurrent runs.

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
source("R/sim_expR5.R")
source("R/plot_helpers.R")
source("R/plots_expR5.R")

cat("=== expR5 standalone START:", format(Sys.time()), "===\n")

conditions <- expR5_design()
cat("conditions:", nrow(conditions), "\n")

results_raw <- expR5_run_all(conditions, n_rounds = 100, n_seeds = 5)
cat("results rows:", nrow(results_raw), "\n")

summary_obj <- expR5_aggregate(results_raw)
cat("summary rows:", nrow(summary_obj$summary), "\n")

dir.create("_targets/objects", showWarnings = FALSE, recursive = TRUE)
saveRDS(conditions,  "_targets/objects/expR5_conditions")
saveRDS(results_raw, "_targets/objects/expR5_results_raw")
saveRDS(summary_obj, "_targets/objects/expR5_summary")

save_expR5_plots(summary_obj, out_dir = "figs", prefix = "expR5")

cat("=== expR5 standalone END:", format(Sys.time()), "===\n")
