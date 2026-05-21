#!/usr/bin/env Rscript
## Standalone driver for Exp 8 (sim_expL3, bilinear (λ, η) surface).
## Bypasses targets to avoid metadata race with concurrent tar_make(expL2_*).

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble)
  library(ggplot2); library(patchwork); library(scales)
})

SIM_DIR <- "[REPO_ROOT]"
setwd(SIM_DIR)

source("R/sim_credibility.R")
source("R/sim_helpers.R")
source("R/sim_operator.R")
source("R/sim_market.R")
source("R/sim_integrator.R")
source("R/sim_expL3.R")
source("R/plots_expL3.R")
source("R/plot_helpers.R")

cat("=== expL3 standalone START:", format(Sys.time()), "===\n")

conditions <- expL3_design()
cat("conditions:", nrow(conditions), "\n")

n_cores <- max(1, min(8, parallel::detectCores() - 2))
cat("parallel cores:", n_cores, "\n")
results_raw <- expL3_run_all(conditions, n_rounds = 100, n_seeds = 5, cores = n_cores)
cat("results rows:", nrow(results_raw), "\n")

summary <- expL3_aggregate(results_raw)
cat("summary rows:", nrow(summary), "\n")

dir.create("_targets/objects", showWarnings = FALSE, recursive = TRUE)
saveRDS(conditions, "_targets/objects/expL3_conditions")
saveRDS(results_raw, "_targets/objects/expL3_results_raw")
saveRDS(summary, "_targets/objects/expL3_summary")

dir.create("figs", showWarnings = FALSE)
save_expL3_plot(summary, path = "figs/expL3_bilinear_surface.pdf")

cat("=== expL3 standalone END:", format(Sys.time()), "===\n")
