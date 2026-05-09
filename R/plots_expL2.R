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
  ## Panel (a) data: per-(stake × dag × tau) surplus mean + bootstrap CI
  per_tau <- results_raw %>%
    group_by(dag_type, stake_fraction, tau_audit) %>%
    summarise(
      surplus_mean = mean(net_op_surplus, na.rm = TRUE),
      surplus_se   = sd(net_op_surplus, na.rm = TRUE) /
                       sqrt(sum(!is.na(net_op_surplus))),
      .groups = "drop"
    ) %>%
    mutate(
      surplus_ci_lo = surplus_mean - 1.96 * surplus_se,
      surplus_ci_hi = surplus_mean + 1.96 * surplus_se
    )

  ## Panel (b) data: from expL2_aggregate
  s <- summary

  ## ─── Panel (a): surplus vs τ_audit, by stake fraction ────────────
  p_a <- ggplot(per_tau, aes(x = tau_audit, y = surplus_mean,
                              colour = factor(stake_fraction),
                              group  = stake_fraction)) +
    geom_ribbon(aes(ymin = surplus_ci_lo, ymax = surplus_ci_hi,
                    fill = factor(stake_fraction)),
                alpha = 0.15, colour = NA) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey40") +
    scale_x_log10() +
    facet_wrap(~ dag_type) +
    labs(x = expression(tau ~ "(audit frequency, log scale)"),
         y = "Mean operator surplus per round",
         colour = expression(lambda ~ "(stake)"),
         fill   = expression(lambda ~ "(stake)"),
         title  = "(a) Surplus vs audit frequency, by stake fraction") +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom")

  ## ─── Panel (b): predicted vs empirical τ* ─────────────────────────
  p_b <- ggplot(s,
                aes(x = predicted_tau_star,
                    y = empirical_zero_crossing_tau,
                    colour = dag_type,
                    shape  = dag_type)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                colour = "grey50") +
    geom_point(size = 3, alpha = 0.85, na.rm = TRUE) +
    scale_x_log10() +
    scale_y_log10() +
    labs(x = expression(predicted ~ tau^"*" == beta * bar(v) * n / lambda),
         y = expression(empirical ~ tau ~ "zero-crossing"),
         colour = "DAG topology",
         shape  = "DAG topology",
         title  = "(b) SDS prediction vs empirical zero-crossing (log-log)") +
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
