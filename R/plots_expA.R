suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

## ── Experiment A figures ──────────────────────────────────────────
## Welfare loss from strategic operators (includes ghost_bidder)

## Build A1: Welfare loss by operator type x topology (reusable ggplot)
build_expA_welfare <- function(summary) {
  df <- summary %>%
    filter(load_name == "medium") %>%
    mutate(
      dag_label = label_topology[dag_type],
      op_label  = label_operator[operator_type]
    )

  ggplot(df, aes(x = dag_label, y = welfare_loss_mean,
                      fill = op_label)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbar(
      aes(ymin = welfare_loss_ci_lo,
          ymax = welfare_loss_ci_hi),
      position = position_dodge(width = 0.7),
      width = 0.25, linewidth = 0.4
    ) +
    scale_fill_manual(values = palette_operator, name = "Operator") +
    guides(fill = guide_legend(nrow = 2)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "DAG Topology", y = "Welfare Loss (% of truthful)") +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

## Fig A1: Welfare loss (saved individually)
plot_expA_welfare <- function(summary) {
  p <- build_expA_welfare(summary)
  save_fig(p, "expA_welfare_loss.pdf", width = 3.5, height = 2.5)
}


## Build A3: Operator surplus extraction (reusable ggplot, medium load only)
build_expA_surplus <- function(summary) {
  df <- summary %>%
    filter(operator_type != "truthful", load_name == "medium") %>%
    mutate(
      dag_label = label_topology[dag_type],
      op_label  = label_operator[operator_type]
    )

  ggplot(df, aes(x = dag_label, y = op_surplus_mean,
                      fill = op_label)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    scale_fill_manual(values = palette_operator, name = "Operator") +
    guides(fill = guide_legend(nrow = 2)) +
    labs(x = "DAG Topology", y = "Operator Surplus") +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}


## Fig A2: Welfare loss by load level (supplementary)
plot_expA_welfare_by_load <- function(summary) {
  df <- summary %>%
    filter(operator_type != "truthful") %>%
    mutate(
      dag_label = label_topology[dag_type],
      op_label  = label_operator[operator_type]
    )

  p <- ggplot(df, aes(x = load_name, y = welfare_loss_mean,
                       colour = op_label, group = op_label)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    geom_ribbon(aes(ymin = welfare_loss_ci_lo, ymax = welfare_loss_ci_hi,
                    fill = op_label), alpha = 0.15, colour = NA) +
    facet_wrap(~dag_label) +
    scale_colour_manual(values = palette_operator, name = "Operator") +
    scale_fill_manual(values = palette_operator, guide = "none") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "Load Level", y = "Welfare Loss (%)") +
    theme_ieee()

  save_fig(p, "expA_welfare_by_load.pdf", width = 7, height = 2.5)
}


## Fig A3: Operator surplus extraction (supplementary, all loads)
plot_expA_surplus <- function(summary) {
  df <- summary %>%
    filter(operator_type != "truthful") %>%
    mutate(
      dag_label = label_topology[dag_type],
      op_label  = label_operator[operator_type]
    )

  p <- ggplot(df, aes(x = dag_label, y = op_surplus_mean,
                       fill = op_label)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    facet_wrap(~load_name) +
    scale_fill_manual(values = palette_operator, name = "Operator") +
    labs(x = "DAG Topology", y = "Mean Operator Surplus") +
    theme_ieee()

  save_fig(p, "expA_operator_surplus.pdf", width = 7, height = 2.5)
}
