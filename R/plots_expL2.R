## ── Plot for Experiment L2: SDS forward-calibration ───────────────────
##
## Two-panel figure addressing the TEAC RF2/Issue 1 reviewer concern:
##   (a) Empirical operator surplus vs audit frequency τ, one curve per
##       stake fraction λ; vertical reference line at the SDS-predicted
##       τ*(λ) = β v̄ n / λ for each λ. Curves should cross zero near
##       the predicted τ* if the SDS theorem holds.
##   (b) Predicted τ* vs empirical zero-crossing τ on log-log axes.
##       Ideal: points sit on the y = x diagonal.
##
## The figure replaces the post-hoc amplification-factor calibration
## previously flagged as AUTHOR-REQUIRED in the manuscript with an
## ex-ante prediction.
##
## Inputs:
##   results_raw — output of expL2_run_all (per-round log; needed for panel a)
##   summary     — output of expL2_aggregate (per (dag × stake): predicted vs
##                 empirical τ*; needed for panel b)
##   beta        — detection sensitivity (default 2; set in expL2_run_single)
##   v_bar       — bid scale upper bound (default 1)
##   n_agents    — agent population (default 40, matching EXPL2_N_AGENTS)
##
## Output:
##   ggplot2 object combining both panels via patchwork.

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(dplyr)
})


plot_expL2_forward_calibration <- function(results_raw,
                                           summary,
                                           beta     = 2,
                                           v_bar    = 1,
                                           n_agents = 40) {
  ## The surplus is topology-invariant here (the ghost-bid VCG perturbation
  ## carries no topology term — cross-topology spread is 0 to machine
  ## precision), so we POOL across DAG topologies rather than plotting three
  ## superimposed series. Both panels therefore key on stake fraction lambda.

  ## Panel (a) data: per-(stake × tau) surplus mean, pooled over topology.
  per_tau <- results_raw %>%
    group_by(stake_fraction, tau_audit) %>%
    summarise(surplus_mean = mean(net_op_surplus, na.rm = TRUE), .groups = "drop")

  ## Panel (b) data: pool the summary across topology -> one point per lambda.
  s <- summary %>%
    group_by(stake_fraction) %>%
    summarise(predicted_tau_star = mean(predicted_tau_star, na.rm = TRUE),
              empirical_zero_crossing_tau = mean(empirical_zero_crossing_tau, na.rm = TRUE),
              .groups = "drop")

  ## ─── Panel (a): surplus vs τ, by stake λ (topology-invariant) ─────
  ## Zoomed to the zero-crossing band: at frequent audits (small τ) the
  ## penalty dominates and all λ collapse to a deep-negative value (off the
  ## bottom of this view); the informative region is where each λ curve
  ## crosses zero, which happens at the λ-dependent τ that panel (b) predicts.
  p_a <- ggplot(per_tau, aes(x = tau_audit, y = surplus_mean,
                              colour = factor(stake_fraction),
                              group  = stake_fraction)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey40") +
    scale_x_log10() +
    coord_cartesian(ylim = c(-3, 0.6)) +
    scale_colour_viridis_d(end = 0.92) +
    labs(x = expression(tau ~ "(audit period, log scale)"),
         y = "Mean operator surplus / round",
         colour = expression(lambda ~ "(stake)"),
         title  = "(a) Surplus near the zero-crossing, by stake (topology-invariant)") +
    guides(colour = guide_legend(nrow = 1)) +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom")

  ## ─── Panel (b): predicted vs empirical τ* (one point per λ) ───────
  p_b <- ggplot(s, aes(x = predicted_tau_star,
                       y = empirical_zero_crossing_tau,
                       colour = factor(stake_fraction))) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                colour = "grey50") +
    geom_point(size = 3, alpha = 0.9, na.rm = TRUE) +
    scale_x_log10() +
    scale_y_log10() +
    scale_colour_viridis_d(end = 0.92) +
    labs(x = expression(predicted ~ tau^"*" == beta * bar(v) * n / lambda),
         y = expression(empirical ~ tau ~ "zero-crossing"),
         colour = expression(lambda ~ "(stake)"),
         title  = "(b) SDS prediction vs empirical zero-crossing (topology-invariant)") +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom")

  combined <- p_a + p_b + plot_layout(ncol = 1, heights = c(1.2, 1))
  combined
}


write_expL2_plot <- function(results_raw,
                             summary,
                             path     = "figs/expL2_forward_calibration.pdf",
                             width    = 7.16,
                             height   = 7.0) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  p <- plot_expL2_forward_calibration(results_raw, summary)
  ggsave(path, p, width = width, height = height, units = "in")
  path
}
