suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
})

## ── Experiment L Plots: Domain Separation Knife-Edge ────────────────

plot_expL_combined <- function(summary) {

  # Panel (a): Net surplus vs stake fraction, by operator and topology
  p1 <- ggplot(summary,
               aes(x = stake_fraction, y = surplus_mean,
                   colour = operator_type, shape = operator_type)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2) +
    geom_errorbar(
      aes(ymin = surplus_ci_lo, ymax = surplus_ci_hi),
      width = 0.015, linewidth = 0.4
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    facet_wrap(~ dag_type, nrow = 1) +
    scale_colour_manual(
      values = c(
        "ghost_bidder" = "#E74C3C",
        "inflator"     = "#E67E22",
        "misreporter"  = "#9B59B6"
      ),
      labels = c(
        "ghost_bidder" = "Ghost bidder",
        "inflator"     = "Price inflator",
        "misreporter"  = "Capacity misreporter"
      )
    ) +
    scale_shape_manual(
      values = c(
        "ghost_bidder" = 16,
        "inflator"     = 17,
        "misreporter"  = 15
      ),
      labels = c(
        "ghost_bidder" = "Ghost bidder",
        "inflator"     = "Price inflator",
        "misreporter"  = "Capacity misreporter"
      )
    ) +
    scale_x_continuous(breaks = c(0, 0.1, 0.25, 0.5, 1.0)) +
    labs(
      x = expression("Stake fraction " * phi),
      y = "Net operator surplus",
      colour = "Operator strategy",
      shape  = "Operator strategy",
      title = expression("(a) Surplus vs. stake fraction " * phi)
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 9)
    )

  # Panel (b): Welfare ratio W(phi)/W(0) vs stake fraction
  # At phi=0, all operator types behave truthfully (exchange bypass),
  # so welfare at phi=0 is the credible baseline.
  baseline_welfare <- summary %>%
    filter(stake_fraction == 0) %>%
    group_by(dag_type) %>%
    summarise(welfare_baseline = mean(welfare_mean), .groups = "drop")

  welfare_ratio <- summary %>%
    left_join(baseline_welfare, by = "dag_type") %>%
    mutate(
      w_ratio    = welfare_mean / welfare_baseline,
      w_ratio_lo = welfare_ci_lo / welfare_baseline,
      w_ratio_hi = welfare_ci_hi / welfare_baseline
    )

  dodge <- position_dodge(width = 0.02)

  p2 <- ggplot(welfare_ratio,
               aes(x = stake_fraction, y = w_ratio,
                   colour = operator_type, shape = operator_type,
                   group = operator_type)) +
    geom_line(linewidth = 0.7, position = dodge) +
    geom_point(size = 2, position = dodge) +
    geom_errorbar(
      aes(ymin = w_ratio_lo, ymax = w_ratio_hi),
      width = 0.015, linewidth = 0.4, position = dodge
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    facet_wrap(~ dag_type, nrow = 1) +
    scale_colour_manual(
      values = c(
        "ghost_bidder" = "#E74C3C",
        "inflator"     = "#E67E22",
        "misreporter"  = "#9B59B6"
      ),
      labels = c(
        "ghost_bidder" = "Ghost bidder",
        "inflator"     = "Price inflator",
        "misreporter"  = "Capacity misreporter"
      )
    ) +
    scale_shape_manual(
      values = c(
        "ghost_bidder" = 16,
        "inflator"     = 17,
        "misreporter"  = 15
      ),
      labels = c(
        "ghost_bidder" = "Ghost bidder",
        "inflator"     = "Price inflator",
        "misreporter"  = "Capacity misreporter"
      )
    ) +
    scale_x_continuous(breaks = c(0, 0.1, 0.25, 0.5, 1.0)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = expression("Stake fraction " * phi),
      y = expression("Welfare ratio  " * W(phi) / W(0)),
      colour = "Operator strategy",
      shape  = "Operator strategy",
      title = expression("(b) Welfare loss relative to credible baseline (" * phi * " = 0)")
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 9)
    )

  combined <- p1 / p2 + plot_layout(heights = c(1, 1))

  save_fig(combined, "expL_combined.pdf", width = 10, height = 8)
}
