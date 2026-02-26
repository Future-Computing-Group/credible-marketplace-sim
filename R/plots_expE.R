suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

## ── Experiment E figures ──────────────────────────────────────────
## Credibility Trilemma Demonstration

## Build E1: Ghost-bid surplus by mechanism x topology (reusable ggplot)
build_expE_trilemma <- function(summary) {
  max_n <- max(summary$n_agents_cond)
  df <- summary %>%
    filter(n_agents_cond == max_n) %>%
    mutate(
      dag_label  = label_topology[dag_type],
      cred_label = label_credibility[credibility]
    )

  ggplot(df, aes(x = cred_label, y = ghost_surplus_mean,
                      fill = cred_label)) +
    geom_col(width = 0.6) +
    geom_errorbar(
      aes(ymin = ghost_surplus_ci_lo,
          ymax = ghost_surplus_ci_hi),
      width = 0.25, linewidth = 0.4
    ) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    facet_wrap(~dag_label) +
    scale_fill_manual(values = palette_credibility, guide = "none") +
    labs(
      x = "Credibility Mechanism",
      y = "Ghost-Bid Net Surplus"
    ) +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

## Fig E1: Trilemma surplus (saved individually)
plot_expE_trilemma <- function(summary) {
  p <- build_expE_trilemma(summary)
  save_fig(p, "expE_trilemma_surplus.pdf", width = 7, height = 2.5)
}


## Build E2: Detection rate x profitability scatter (reusable ggplot)
build_expE_detection_vs_profit <- function(summary) {
  max_n <- max(summary$n_agents_cond)
  df <- summary %>%
    filter(n_agents_cond == max_n) %>%
    mutate(
      dag_label  = label_topology[dag_type],
      cred_label = label_credibility[credibility]
    )

  ggplot(df, aes(x = detection_rate_mean, y = ghost_surplus_mean,
                      colour = cred_label, shape = dag_label)) +
    geom_point(size = 3.5) +
    geom_vline(xintercept = 0.5, linetype = "dotted", colour = "grey60") +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
    scale_colour_manual(values = palette_credibility, name = "Mechanism") +
    scale_shape_manual(values = c(16, 17, 15), name = "Topology") +
    guides(
      colour = guide_legend(nrow = 2, order = 1),
      shape  = guide_legend(nrow = 1, order = 2)
    ) +
    labs(
      x = "Detection Rate",
      y = "Ghost-Bid Net Surplus"
    ) +
    theme_ieee() +
    theme(legend.box = "vertical")
}

## Fig E2: Detection vs profit (saved individually)
plot_expE_detection_vs_profit <- function(summary) {
  p <- build_expE_detection_vs_profit(summary)
  save_fig(p, "expE_detection_vs_profit.pdf", width = 3.5, height = 2.8)
}


## Fig E3: Welfare impact by agent count (supplementary)
plot_expE_scaling <- function(summary) {
  df <- summary %>%
    mutate(
      dag_label  = label_topology[dag_type],
      cred_label = label_credibility[credibility]
    )

  p <- ggplot(df, aes(x = n_agents_cond, y = welfare_ratio_mean,
                       colour = cred_label)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    geom_ribbon(aes(ymin = welfare_ratio_ci_lo, ymax = welfare_ratio_ci_hi,
                    fill = cred_label), alpha = 0.1, colour = NA) +
    geom_hline(yintercept = 1.0, linetype = "dashed",
               colour = "grey40", linewidth = 0.4) +
    facet_wrap(~dag_label) +
    scale_colour_manual(values = palette_credibility, name = "Mechanism") +
    scale_fill_manual(values = palette_credibility, guide = "none") +
    labs(x = "Number of Agents", y = "Welfare Ratio") +
    theme_ieee()

  save_fig(p, "expE_welfare_scaling.pdf", width = 7, height = 2.5)
}
