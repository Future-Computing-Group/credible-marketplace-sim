suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

## ── Experiment B figures ──────────────────────────────────────────
## Credibility mechanisms comparison (now with CI error bars)

## Build B1: Welfare recovery by mechanism (reusable ggplot)
build_expB_welfare_recovery <- function(summary) {
  df <- summary %>%
    filter(load_name == "medium") %>%
    mutate(
      dag_label  = label_topology[dag_type],
      cred_label = label_credibility[credibility]
    )

  ggplot(df, aes(x = cred_label, y = welfare_recovery_mean,
                      fill = cred_label)) +
    geom_col(width = 0.6) +
    geom_errorbar(
      aes(ymin = welfare_recovery_ci_lo,
          ymax = welfare_recovery_ci_hi),
      width = 0.25, linewidth = 0.4
    ) +
    geom_hline(yintercept = 1.0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    facet_grid(operator_type ~ dag_label,
               labeller = labeller(
                 operator_type = label_operator,
                 dag_label = identity
               )) +
    scale_fill_manual(values = palette_credibility, guide = "none") +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    coord_cartesian(ylim = c(0.95, 1.005)) +
    labs(x = "Credibility Mechanism", y = "Welfare Recovery (%)") +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

## Fig B1: Welfare recovery (saved individually)
plot_expB_welfare_recovery <- function(summary) {
  p <- build_expB_welfare_recovery(summary)
  save_fig(p, "expB_welfare_recovery.pdf", width = 7, height = 5)
}


## Build B2: Detection rate vs welfare recovery scatter (reusable ggplot)
build_expB_tradeoff <- function(summary) {
  df <- summary %>%
    filter(load_name == "medium") %>%
    mutate(cred_label = label_credibility[credibility])

  ggplot(df, aes(x = detection_rate_mean, y = welfare_recovery_mean,
                      colour = cred_label, shape = cred_label)) +
    geom_point(size = 3.5) +
    geom_hline(yintercept = 1.0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    geom_vline(xintercept = 0.5, linetype = "dotted",
               colour = "grey60", linewidth = 0.5) +
    facet_wrap(~dag_type, labeller = labeller(dag_type = label_topology)) +
    scale_colour_manual(values = palette_credibility, name = "Mechanism") +
    scale_shape_manual(values = c(16, 17, 15, 18, 8), name = "Mechanism") +
    guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    coord_cartesian(ylim = c(0.95, 1.005)) +
    labs(x = "Detection Rate", y = "Welfare Recovery (%)") +
    theme_ieee()
}

## Fig B2: Trade-off (saved individually)
plot_expB_tradeoff <- function(summary) {
  p <- build_expB_tradeoff(summary)
  save_fig(p, "expB_tradeoff.pdf", width = 7, height = 2.5)
}


## Fig B3: Detection rate by mechanism (supplementary)
plot_expB_detection <- function(summary) {
  df <- summary %>%
    filter(credibility != "none") %>%
    mutate(cred_label = label_credibility[credibility])

  p <- ggplot(df, aes(x = cred_label, y = detection_rate_mean,
                       fill = cred_label)) +
    geom_col(width = 0.6) +
    facet_grid(operator_type ~ load_name,
               labeller = labeller(operator_type = label_operator)) +
    scale_fill_manual(values = palette_credibility, guide = "none") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "Credibility Mechanism", y = "Detection Rate (%)") +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  save_fig(p, "expB_detection_rate.pdf", width = 7, height = 5)
}
