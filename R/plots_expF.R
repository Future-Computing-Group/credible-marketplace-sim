suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

## ── Experiment F figures ──────────────────────────────────────────
## Domain Separation Validation (Proposition 1)

short_op_F <- c(misreporter = "Misreporter", inflator = "Inflator",
                ghost_bidder = "Ghost Bid")

## Build F1: Surplus extraction: stake vs separated (reusable ggplot)
build_expF_surplus <- function(summary) {
  df <- summary %>%
    filter(load_name == "medium") %>%
    mutate(
      dag_label = label_topology[dag_type],
      op_label  = short_op_F[operator_type]
    )

  ggplot(df, aes(x = op_label, y = surplus_mean,
                      fill = fee_mode)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbar(
      aes(ymin = surplus_ci_lo, ymax = surplus_ci_hi),
      position = position_dodge(width = 0.7),
      width = 0.25, linewidth = 0.4
    ) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.4) +
    facet_wrap(~dag_label) +
    scale_fill_manual(
      values = palette_fee,
      labels = c(stake = "Stake (conflict)", separated = "Separated (Prop. 1)"),
      name = "Operator Mode"
    ) +
    labs(x = "Operator Strategy", y = "Net Operator Surplus") +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

## Fig F1: Surplus (saved individually)
plot_expF_surplus <- function(summary) {
  p <- build_expF_surplus(summary)
  save_fig(p, "expF_domain_separation.pdf", width = 7, height = 2.8)
}


## Build F2: Welfare ratio comparison (reusable ggplot)
build_expF_welfare <- function(summary) {
  df <- summary %>%
    filter(load_name == "medium") %>%
    mutate(
      dag_label = label_topology[dag_type],
      op_label  = short_op_F[operator_type]
    )

  ggplot(df, aes(x = op_label, y = welfare_ratio_mean,
                      fill = fee_mode)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbar(
      aes(ymin = welfare_ratio_ci_lo, ymax = welfare_ratio_ci_hi),
      position = position_dodge(width = 0.7),
      width = 0.25, linewidth = 0.4
    ) +
    geom_hline(yintercept = 1.0, linetype = "dashed",
               colour = "grey40", linewidth = 0.4) +
    facet_wrap(~dag_label) +
    scale_fill_manual(
      values = palette_fee,
      labels = c(stake = "Stake", separated = "Separated"),
      name = "Mode"
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    coord_cartesian(ylim = c(0.95, 1.005)) +
    labs(x = "Operator Strategy", y = "Welfare Ratio") +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

## Fig F2: Welfare ratio (saved individually)
plot_expF_welfare <- function(summary) {
  p <- build_expF_welfare(summary)
  save_fig(p, "expF_welfare_ratio.pdf", width = 7, height = 2.8)
}
