suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
})

## ── Experiment O Plots: Non-Stationary Service Supply ─────────────

plot_expO_combined <- function(summary) {

  dodge <- position_dodge(width = 0.4)

  # Panel (a): Welfare ratio by capacity model and credibility ─────
  p1 <- ggplot(summary,
               aes(x = capacity_model, y = welfare_ratio_mean,
                   colour = credibility)) +
    geom_point(size = 2, position = dodge) +
    geom_errorbar(
      aes(ymin = welfare_ratio_ci_lo, ymax = welfare_ratio_ci_hi),
      width = 0.2, linewidth = 0.4, position = dodge
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    facet_wrap(~ dag_type, nrow = 1) +
    scale_colour_manual(
      values = c(
        "none"      = "#E74C3C",
        "broadcast" = "#27AE60",
        "exchange"  = "#3498DB"
      ),
      labels = c(
        "none"      = "No mechanism",
        "broadcast" = "Broadcast",
        "exchange"  = "Exchange"
      )
    ) +
    scale_x_discrete(
      labels = c(
        "static" = "Static",
        "cyclic" = "Cyclic",
        "shock"  = "Shock"
      )
    ) +
    labs(
      x      = "Capacity model",
      y      = "Welfare ratio",
      colour = "Credibility",
      title  = "(a) Welfare ratio by capacity model"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 9)
    )

  # Panel (b): Operator surplus by capacity model and credibility ──
  p2 <- ggplot(summary,
               aes(x = capacity_model, y = net_surplus_mean,
                   colour = credibility)) +
    geom_point(size = 2, position = dodge) +
    geom_errorbar(
      aes(ymin = net_surplus_ci_lo, ymax = net_surplus_ci_hi),
      width = 0.2, linewidth = 0.4, position = dodge
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    facet_wrap(~ dag_type, nrow = 1) +
    scale_colour_manual(
      values = c(
        "none"      = "#E74C3C",
        "broadcast" = "#27AE60",
        "exchange"  = "#3498DB"
      ),
      labels = c(
        "none"      = "No mechanism",
        "broadcast" = "Broadcast",
        "exchange"  = "Exchange"
      )
    ) +
    scale_x_discrete(
      labels = c(
        "static" = "Static",
        "cyclic" = "Cyclic",
        "shock"  = "Shock"
      )
    ) +
    labs(
      x      = "Capacity model",
      y      = "Net operator surplus",
      colour = "Credibility",
      title  = "(b) Operator surplus by capacity model"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 9)
    )

  combined <- p1 / p2 + plot_layout(heights = c(1, 1))

  save_fig(combined, "expO_combined.pdf", width = 10, height = 8)
}
