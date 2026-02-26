suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

## ── Experiment G figures ──────────────────────────────────────────
## Sensitivity Analysis (supplementary)

## Fig G1: Sensitivity panel (welfare vs parameter value)
plot_expG_sensitivity <- function(summary) {
  df <- summary %>%
    mutate(dag_label = label_topology[dag_type])

  p <- ggplot(df, aes(x = param_value, y = welfare_mean,
                       colour = dag_label)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    geom_ribbon(aes(ymin = welfare_ci_lo, ymax = welfare_ci_hi,
                    fill = dag_label), alpha = 0.1, colour = NA) +
    facet_wrap(~param_name, scales = "free_x") +
    scale_colour_manual(values = palette_topology, name = "Topology") +
    scale_fill_manual(values = palette_topology, guide = "none") +
    labs(x = "Parameter Value", y = "Mean Welfare") +
    theme_ieee()

  save_fig(p, "expG_sensitivity.pdf", width = 7, height = 4)
}


## Fig G2: Detection rate sensitivity (regulatory and blockchain)
plot_expG_detection <- function(summary) {
  df <- summary %>%
    filter(param_name %in% c("p_detect", "tau_audit")) %>%
    mutate(dag_label = label_topology[dag_type])

  p <- ggplot(df, aes(x = param_value, y = detection_mean,
                       colour = dag_label)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    facet_wrap(~param_name, scales = "free_x") +
    scale_colour_manual(values = palette_topology, name = "Topology") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Parameter Value", y = "Detection Rate") +
    theme_ieee()

  save_fig(p, "expG_detection_sensitivity.pdf", width = 7, height = 2.5)
}


## Fig G3: Net surplus sensitivity
plot_expG_surplus <- function(summary) {
  df <- summary %>%
    mutate(dag_label = label_topology[dag_type])

  p <- ggplot(df, aes(x = param_value, y = surplus_mean,
                       colour = dag_label)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.4) +
    facet_wrap(~param_name, scales = "free_x") +
    scale_colour_manual(values = palette_topology, name = "Topology") +
    labs(x = "Parameter Value", y = "Net Operator Surplus") +
    theme_ieee()

  save_fig(p, "expG_surplus_sensitivity.pdf", width = 7, height = 4)
}
