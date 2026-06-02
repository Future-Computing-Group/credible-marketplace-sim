#!/usr/bin/env Rscript
## Post-run helper: load expL3_summary and expL4_summary, compute the
## predicted-vs-empirical credibility-region agreement rates needed for
## the 2B manuscript placeholders \agreementLthree and \agreementLfour.

suppressPackageStartupMessages({
  library(dplyr); library(tibble)
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

source("R/sim_operator.R")

## ── Exp 8 (expL3): bilinear surface at tau=12, bang-bang adversary ──
expL3 <- readRDS("_targets/objects/expL3_summary")
beta <- 2; v_bar <- 1; n_agents <- 40; tau <- 12; eps_max <- 1.0 / beta
hazard_em <- 1 - exp(-beta * eps_max)
expL3 <- expL3 %>%
  mutate(
    predicted_profit =
      (1 - escrow_fraction) * stake_fraction * eps_max -
        hazard_em * v_bar * n_agents / (tau ^ 2),
    predicted_credible = predicted_profit <= 0,
    empirical_credible = mean_op_surplus <= 1e-6
  )
agreement_L3 <- mean(expL3$predicted_credible == expL3$empirical_credible)
cat("expL3 n_cells:", nrow(expL3), "\n")
cat("expL3 agreement:", round(agreement_L3 * 100, 1), "%\n")
cat("expL3 mean_op_surplus min/max:",
    round(min(expL3$mean_op_surplus, na.rm = TRUE), 4), "/",
    round(max(expL3$mean_op_surplus, na.rm = TRUE), 4), "\n")
cat("expL3 zero-surplus cells:", sum(expL3$mean_op_surplus <= 1e-6, na.rm = TRUE), "\n")
cat("expL3 positive-surplus cells:", sum(expL3$mean_op_surplus > 1e-6, na.rm = TRUE), "\n")

## ── Exp 9 (expL4): 3D surface (expL4_aggregate already adds predicted/empirical) ──
expL4 <- readRDS("_targets/objects/expL4_summary")
agreement_L4 <- mean(expL4$predicted_credible == expL4$empirical_credible)
cat("\nexpL4 n_cells:", nrow(expL4), "\n")
cat("expL4 agreement:", round(agreement_L4 * 100, 1), "%\n")
cat("expL4 mean_op_surplus min/max:",
    round(min(expL4$mean_op_surplus, na.rm = TRUE), 4), "/",
    round(max(expL4$mean_op_surplus, na.rm = TRUE), 4), "\n")
