suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

## ── Experiment D figures ──────────────────────────────────────────
## Two-tier heterogeneous trust

## Build D1: Welfare ratio by L1 x L2 trust regime heatmap (reusable ggplot)
build_expD_heatmap <- function(summary) {
  max_n <- max(summary$n_agents_cond)
  df <- summary %>%
    filter(n_agents_cond == max_n) %>%
    mutate(
      dag_label  = label_topology[dag_type],
      l1_label   = label_credibility[l1_trust],
      l2_label   = label_credibility[l2_trust]
    )

  ggplot(df, aes(x = l1_label, y = l2_label,
                      fill = welfare_ratio_mean)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", welfare_ratio_mean * 100)),
              size = 5) +
    facet_wrap(~dag_label) +
    scale_fill_gradient2(
      low = "#D7191C", mid = "#FFFFBF", high = "#1A9641",
      midpoint = 0.9, name = "Welfare\nRatio",
      labels = percent_format(accuracy = 1)
    ) +
    labs(
      x = "Level 1 (Slice Marketplace) Trust",
      y = "Level 2 (Local Marketplace) Trust"
    ) +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

## Fig D1: Heatmap (saved individually)
plot_expD_heatmap <- function(summary) {
  p <- build_expD_heatmap(summary)
  save_fig(p, "expD_trust_heatmap.pdf", width = 7, height = 3)
}


## Build D2: L1 mechanism cost-benefit (reusable ggplot)
## Compliance and latency depend only on L1 mechanism (topology-invariant),
## so we collapse across L2 and topology for a clean summary.
build_expD_frontier <- function(summary) {
  max_n <- max(summary$n_agents_cond)
  df <- summary %>%
    filter(n_agents_cond == max_n) %>%
    group_by(l1_trust) %>%
    summarise(
      compliance  = mean(compliance_mean, na.rm = TRUE),
      latency_oh  = mean(latency_oh_mean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      l1_label = label_credibility[l1_trust],
      lat_text = ifelse(latency_oh > 0,
                        paste0("+", latency_oh, " rounds"),
                        "0 overhead")
    )

  ggplot(df, aes(x = l1_label, y = compliance, fill = l1_label)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = lat_text), vjust = -0.5, size = 5) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
    scale_fill_manual(values = palette_credibility, guide = "none") +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
      x = "Level-1 (Slice Marketplace) Mechanism",
      y = "Compliance Rate"
    ) +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

## Fig D2: Frontier (saved individually)
plot_expD_frontier <- function(summary) {
  p <- build_expD_frontier(summary)
  save_fig(p, "expD_cost_benefit.pdf", width = 7, height = 3)
}


## Fig D3: Welfare ratio by agent count (supplementary)
plot_expD_scaling <- function(summary) {
  regimes <- summary %>%
    filter(
      (l1_trust == "exchange" & l2_trust == "none") |
      (l1_trust == "blockchain" & l2_trust == "none") |
      (l1_trust == "none" & l2_trust == "none") |
      (l1_trust == "exchange" & l2_trust == "exchange")
    ) %>%
    mutate(
      trust_regime = paste0(l1_trust, " / ", l2_trust),
      dag_label    = label_topology[dag_type]
    )

  p <- ggplot(regimes, aes(x = n_agents_cond, y = welfare_ratio_mean,
                            colour = trust_regime)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    facet_wrap(~dag_label) +
    labs(x = "Number of Agents", y = "Welfare Ratio",
         colour = "Trust Regime\n(L1 / L2)") +
    theme_ieee()

  save_fig(p, "expD_scaling.pdf", width = 7, height = 2.5)
}
