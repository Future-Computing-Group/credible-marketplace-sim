suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
})

## ── Experiment K Plots: Revenue-Optimal (Myerson) Mechanism ──────────

plot_expK_combined <- function(summary) {

  ghost_data <- summary %>%
    filter(operator_type == "ghost_bidder")

  # Panel (a): Ghost-bid net surplus by mechanism × credibility × value dist
  p1 <- ggplot(ghost_data,
               aes(x = mechanism_cond, y = surplus_mean,
                   fill = credibility)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbar(
      aes(ymin = surplus_ci_lo, ymax = surplus_ci_hi),
      position = position_dodge(width = 0.7), width = 0.2
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    facet_grid(vdist_label ~ dag_type) +
    scale_fill_manual(
      values = c("none" = "#E74C3C", "broadcast" = "#2ECC71"),
      labels = c("none" = "No commitment", "broadcast" = "Broadcast")
    ) +
    labs(
      x = "Mechanism", y = "Net operator surplus",
      fill = "Credibility",
      title = "(a) Ghost-bid profitability: VCG vs Myerson"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 8)
    )

  # Panel (b): Mean payment per task (revenue comparison)
  p2 <- ggplot(
    summary %>% filter(operator_type == "truthful", credibility == "none"),
    aes(x = mechanism_cond, y = payment_mean, fill = vdist_label)
  ) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbar(
      aes(ymin = payment_ci_lo, ymax = payment_ci_hi),
      position = position_dodge(width = 0.7), width = 0.2
    ) +
    facet_wrap(~ dag_type, nrow = 1) +
    scale_fill_manual(
      values = c("U(1,2)" = "#3498DB", "U(0.5,2)" = "#E67E22")
    ) +
    labs(
      x = "Mechanism", y = "Mean payment per task",
      fill = "Value dist.",
      title = "(b) Revenue: Myerson payments >= VCG payments"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 8)
    )

  combined <- p1 / p2 + plot_layout(heights = c(2, 1))

  save_fig(combined, "expK_combined.pdf", width = 10, height = 10)
}
