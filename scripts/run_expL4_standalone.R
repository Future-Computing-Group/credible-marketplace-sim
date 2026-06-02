#!/usr/bin/env Rscript
## Standalone driver for Exp 9 (sim_expL4, 3D (λ, η, τ) surface).
## Bypasses targets to avoid metadata race with concurrent tar_make(expL2_*).

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble)
  library(ggplot2); library(patchwork); library(scales)
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

source("R/sim_credibility.R")
source("R/sim_helpers.R")
source("R/sim_operator.R")
source("R/sim_market.R")
source("R/sim_integrator.R")
source("R/sim_expL4.R")
source("R/plots_expL4.R")
source("R/plot_helpers.R")

cat("=== expL4 standalone START:", format(Sys.time()), "===\n")

conditions <- expL4_design()
cat("conditions:", nrow(conditions), "\n")

results_raw <- expL4_run_all(conditions, n_rounds = 100, n_seeds = 5)
cat("results rows:", nrow(results_raw), "\n")

summary <- expL4_aggregate(results_raw)
cat("summary rows:", nrow(summary), "\n")

dir.create("_targets/objects", showWarnings = FALSE, recursive = TRUE)
saveRDS(conditions, "_targets/objects/expL4_conditions")
saveRDS(results_raw, "_targets/objects/expL4_results_raw")
saveRDS(summary, "_targets/objects/expL4_summary")

dir.create("figs", showWarnings = FALSE)
save_expL4_plot(summary, path = "figs/expL4_3d_surface.pdf")

cat("=== expL4 standalone END:", format(Sys.time()), "===\n")
