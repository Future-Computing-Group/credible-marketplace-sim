suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

## ── Experiment H figures ──────────────────────────────────────────
## Adaptive (learning) operator: convergence behaviour under
## different credibility mechanisms.

## Build H1: Deviation magnitude trajectory (convergence)
## Shows how the operator's learned deviation changes over rounds
## for each credibility mechanism. Averaged across topologies and seeds.
build_expH_deviation_trajectory <- function(agg) {
  trajectory <- agg$trajectory

  df <- trajectory %>%
    filter(n_agents_cond == 50) %>%
    mutate(
      cred_label = label_credibility[credibility]
    ) %>%
    group_by(credibility, cred_label, round) %>%
    summarise(
      deviation_mean = mean(deviation_magnitude_mean, na.rm = TRUE),
      deviation_se   = mean(deviation_magnitude_se, na.rm = TRUE),
      .groups = "drop"
    )

  ggplot(df, aes(x = round, y = deviation_mean,
                 colour = cred_label, fill = cred_label)) +
    geom_line(linewidth = 0.8) +
    geom_ribbon(aes(ymin = pmax(0, deviation_mean - deviation_se),
                    ymax = deviation_mean + deviation_se),
                alpha = 0.15, colour = NA) +
    scale_colour_manual(values = palette_credibility, name = "Mechanism") +
    scale_fill_manual(values = palette_credibility, name = "Mechanism") +
    labs(x = "Round", y = "Deviation Magnitude") +
    guides(colour = guide_legend(nrow = 2),
           fill = guide_legend(nrow = 2)) +
    theme_ieee()
}


## Build H2: Cumulative operator surplus trajectory
## Shows the running total of net operator surplus over rounds.
## This separates the mechanisms far more than welfare (which is nearly
## identical because the ghost-bid deviation displaces only the marginal
## task). Cumulative surplus captures the economic story: under broadcast
## and blockchain the operator accumulates losses, under none the operator
## accumulates gains, and exchange stays at zero.
build_expH_cumulative_surplus <- function(agg) {
  trajectory <- agg$trajectory

  df <- trajectory %>%
    filter(n_agents_cond == 50) %>%
    group_by(credibility, round) %>%
    summarise(
      net_surplus_mean = mean(net_surplus_mean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(credibility) %>%
    arrange(round) %>%
    mutate(
      cumulative_surplus = cumsum(net_surplus_mean),
      cred_label = label_credibility[credibility]
    ) %>%
    ungroup()

  ggplot(df, aes(x = round, y = cumulative_surplus,
                 colour = cred_label)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    scale_colour_manual(values = palette_credibility, name = "Mechanism") +
    labs(x = "Round", y = "Cumulative Operator Surplus") +
    guides(colour = guide_legend(nrow = 2)) +
    theme_ieee()
}


## Build H3: Summary bar chart — converged deviation by mechanism
build_expH_converged_bar <- function(agg) {
  summary <- agg$summary

  df <- summary %>%
    filter(n_agents_cond == 50) %>%
    mutate(
      cred_label = label_credibility[credibility],
      dag_label  = label_topology[dag_type]
    )

  ggplot(df, aes(x = cred_label, y = mean_deviation_magnitude,
                 fill = dag_label)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbar(
      aes(ymin = mean_deviation_ci_lo,
          ymax = mean_deviation_ci_hi),
      position = position_dodge(width = 0.7),
      width = 0.25, linewidth = 0.4
    ) +
    scale_fill_manual(values = palette_topology, name = "Topology") +
    labs(x = "Credibility Mechanism",
         y = "Converged Deviation Magnitude") +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}


## Standalone figure saves
plot_expH_deviation <- function(agg) {
  p <- build_expH_deviation_trajectory(agg)
  save_fig(p, "expH_deviation_trajectory.pdf", width = 7, height = 3)
}

plot_expH_surplus <- function(agg) {
  p <- build_expH_cumulative_surplus(agg)
  save_fig(p, "expH_cumulative_surplus.pdf", width = 7, height = 3)
}

plot_expH_converged <- function(agg) {
  p <- build_expH_converged_bar(agg)
  save_fig(p, "expH_converged_bar.pdf", width = 7, height = 3)
}
