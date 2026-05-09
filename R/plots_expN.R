suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
})

## ── Experiment N Plots: Markov Broadcast Channel ──────────────────

plot_expN_combined <- function(summary_list) {

  summary   <- summary_list$summary
  threshold <- summary_list$threshold

  # Panel (a): Net surplus vs p_stationary, coloured by channel model
  p1 <- ggplot(summary,
               aes(x = p_stationary, y = net_surplus_mean,
                   colour = channel_model, group = channel_model)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2) +
    geom_errorbar(
      aes(ymin = net_surplus_ci_lo, ymax = net_surplus_ci_hi),
      width = 0.02, linewidth = 0.4
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    facet_wrap(~ dag_type, nrow = 1) +
    scale_colour_manual(
      values = c(
        "iid"         = "#3498DB",
        "markov_low"  = "#27AE60",
        "markov_med"  = "#E67E22",
        "markov_high" = "#E74C3C"
      ),
      labels = c(
        "iid"         = "i.i.d.",
        "markov_low"  = expression("Markov (" * rho * " = 0.3)"),
        "markov_med"  = expression("Markov (" * rho * " = 0.7)"),
        "markov_high" = expression("Markov (" * rho * " = 0.9)")
      )
    ) +
    labs(
      x      = "Stationary detection probability",
      y      = "Net operator surplus",
      colour = "Channel model",
      title  = "(a) Net surplus vs detection probability by channel model"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 9)
    )

  # Panel (b): Profitability threshold by channel model ────────────
  # For channel_model/dag_type combos where deviation is always profitable
  # (no threshold found), show bar at max tested p_stationary with alpha.
  p_max <- max(summary$p_stationary)

  all_combos <- summary %>%
    distinct(dag_type, channel_model)

  threshold_full <- all_combos %>%
    left_join(threshold, by = c("dag_type", "channel_model")) %>%
    mutate(
      always_profitable       = is.na(profitability_threshold),
      profitability_threshold = ifelse(always_profitable, p_max,
                                       profitability_threshold),
      bar_alpha               = ifelse(always_profitable, 0.3, 1.0)
    )

  p2 <- ggplot(threshold_full,
               aes(x = channel_model, y = profitability_threshold,
                   fill = channel_model, alpha = bar_alpha)) +
    geom_col(position = "dodge", width = 0.6) +
    scale_alpha_identity() +
    facet_wrap(~ dag_type, nrow = 1) +
    scale_fill_manual(
      values = c(
        "iid"         = "#3498DB",
        "markov_low"  = "#27AE60",
        "markov_med"  = "#E67E22",
        "markov_high" = "#E74C3C"
      ),
      labels = c(
        "iid"         = "i.i.d.",
        "markov_low"  = expression("Markov (" * rho * " = 0.3)"),
        "markov_med"  = expression("Markov (" * rho * " = 0.7)"),
        "markov_high" = expression("Markov (" * rho * " = 0.9)")
      )
    ) +
    scale_x_discrete(
      labels = c(
        "iid"         = "i.i.d.",
        "markov_low"  = expression(rho * "=0.3"),
        "markov_med"  = expression(rho * "=0.7"),
        "markov_high" = expression(rho * "=0.9")
      )
    ) +
    geom_text(
      data = threshold_full %>% filter(always_profitable),
      aes(label = "always\nprofitable"),
      vjust = -0.3, size = 2.5, colour = "grey30"
    ) +
    labs(
      x     = "Channel model",
      y     = "Profitability threshold",
      fill  = "Channel model",
      title = "(b) Profitability threshold by channel model"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 9)
    )

  combined <- p1 / p2 + plot_layout(heights = c(1, 1))

  save_fig(combined, "expN_combined.pdf", width = 10, height = 8)
}
