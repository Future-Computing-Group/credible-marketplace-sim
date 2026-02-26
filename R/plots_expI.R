suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

## ── Experiment I figures ──────────────────────────────────────────
## Imperfect broadcast: welfare recovery and operator profitability
## as a function of broadcast detection probability.

## Build I1: Welfare recovery vs broadcast reliability
## Line plot with CI ribbon, one line per topology.
build_expI_welfare_vs_broadcast <- function(agg) {
  summary <- agg$summary

  df <- summary %>%
    mutate(dag_label = label_topology[dag_type])

  ggplot(df, aes(x = p_broadcast, y = welfare_ratio_mean,
                 colour = dag_label, fill = dag_label)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    geom_ribbon(aes(ymin = welfare_ratio_ci_lo,
                    ymax = welfare_ratio_ci_hi),
                alpha = 0.15, colour = NA) +
    geom_hline(yintercept = 1.0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    scale_colour_manual(values = palette_topology, name = "Topology") +
    scale_fill_manual(values = palette_topology, name = "Topology") +
    scale_x_continuous(breaks = c(0, 0.3, 0.5, 0.7, 0.9, 0.95, 1.0),
                       labels = c("0", "0.3", "0.5", "0.7", "0.9", "0.95", "1.0")) +
    labs(x = expression("Broadcast Detection Probability (" * p[broadcast] * ")"),
         y = "Welfare Ratio") +
    theme_ieee()
}


## Build I2: Operator net surplus vs broadcast reliability
## Shows the profitability threshold: where does net surplus cross zero?
build_expI_surplus_vs_broadcast <- function(agg) {
  summary <- agg$summary

  df <- summary %>%
    mutate(dag_label = label_topology[dag_type])

  ggplot(df, aes(x = p_broadcast, y = net_surplus_mean,
                 colour = dag_label, fill = dag_label)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    geom_ribbon(aes(ymin = net_surplus_ci_lo,
                    ymax = net_surplus_ci_hi),
                alpha = 0.15, colour = NA) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    scale_colour_manual(values = palette_topology, name = "Topology") +
    scale_fill_manual(values = palette_topology, name = "Topology") +
    scale_x_continuous(breaks = c(0, 0.3, 0.5, 0.7, 0.9, 0.95, 1.0),
                       labels = c("0", "0.3", "0.5", "0.7", "0.9", "0.95", "1.0")) +
    labs(x = expression("Broadcast Detection Probability (" * p[broadcast] * ")"),
         y = "Net Operator Surplus") +
    theme_ieee()
}


## Standalone figure saves
plot_expI_welfare <- function(agg) {
  p <- build_expI_welfare_vs_broadcast(agg)
  save_fig(p, "expI_welfare_vs_broadcast.pdf", width = 7, height = 3)
}

plot_expI_surplus <- function(agg) {
  p <- build_expI_surplus_vs_broadcast(agg)
  save_fig(p, "expI_surplus_vs_broadcast.pdf", width = 7, height = 3)
}
