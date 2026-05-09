suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
})

## ── Experiment M Plots: Strategic Agent Adaptation ─────────────────

plot_expM_combined <- function(summary_list) {

  trajectory <- summary_list$trajectory
  summary    <- summary_list$summary

  # Panel (a): Agent retention trajectory ──────────────────────────
  p1 <- ggplot(trajectory,
               aes(x = round, y = fraction_active_mean,
                   colour = credibility, linetype = agent_mode,
                   group = interaction(credibility, agent_mode))) +
    geom_line(linewidth = 0.7) +
    geom_errorbar(
      aes(ymin = fraction_active_mean - 1.96 * fraction_active_se,
          ymax = fraction_active_mean + 1.96 * fraction_active_se),
      width = 2, linewidth = 0.3, alpha = 0.4
    ) +
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
    scale_linetype_manual(
      values = c(
        "passive"        = "solid",
        "exit_sensitive" = "dashed"
      ),
      labels = c(
        "passive"        = "Passive",
        "exit_sensitive" = "Exit-sensitive"
      )
    ) +
    labs(
      x        = "Round",
      y        = "Fraction of agents active",
      colour   = "Credibility",
      linetype = "Agent mode",
      title    = "(a) Agent retention over time"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 9)
    )

  # Panel (b): Welfare ratio by credibility and agent mode ─────────
  dodge <- position_dodge(width = 0.4)

  p2 <- ggplot(summary,
               aes(x = credibility, y = welfare_ratio_mean,
                   colour = agent_mode)) +
    geom_point(size = 2, position = dodge) +
    geom_errorbar(
      aes(ymin = welfare_ratio_ci_lo, ymax = welfare_ratio_ci_hi),
      width = 0.2, linewidth = 0.4, position = dodge
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    facet_wrap(~ dag_type, nrow = 1) +
    scale_colour_manual(
      values = c(
        "passive"        = "#7F8C8D",
        "exit_sensitive" = "#E74C3C"
      ),
      labels = c(
        "passive"        = "Passive",
        "exit_sensitive" = "Exit-sensitive"
      )
    ) +
    scale_x_discrete(
      labels = c(
        "none"      = "No mechanism",
        "broadcast" = "Broadcast",
        "exchange"  = "Exchange"
      )
    ) +
    labs(
      x      = "Credibility mechanism",
      y      = "Welfare ratio",
      colour = "Agent mode",
      title  = "(b) Welfare ratio by credibility and agent behaviour"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 9)
    )

  combined <- p1 / p2 + plot_layout(heights = c(1, 1))

  save_fig(combined, "expM_combined.pdf", width = 10, height = 8)
}
