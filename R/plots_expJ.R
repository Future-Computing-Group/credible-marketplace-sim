suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(patchwork)
})

## ── Experiment J plots: Credibility x Competition Interaction ─────


#' Combined figure for Experiment 9 (main paper).
#'
#' Panel (a): Heatmap of net operator surplus by credibility x k,
#'            faceted by topology.
#' Panel (b): Welfare ratio by k, coloured by credibility mechanism.
#'
#' @param summary  Aggregated summary from expJ_aggregate().
#' @return ggplot object (also saved to fig/exp9_combined.pdf).
expJ_plot_combined <- function(summary) {

  plot_data <- summary %>%
    mutate(
      credibility_label = label_credibility[credibility],
      topology_label    = label_topology[dag_type],
      k_label           = factor(k_integrators)
    )

  # Panel (a): Heatmap — net surplus by credibility x k
  p_heat <- ggplot(plot_data,
                   aes(x = k_label,
                       y = credibility_label,
                       fill = net_surplus_mean)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.2f", net_surplus_mean)),
              size = 3, family = "serif") +
    facet_wrap(~topology_label, nrow = 1) +
    scale_fill_gradient2(
      low = "#2C7BB6", mid = "white", high = "#D7191C",
      midpoint = 0, name = "Net surplus"
    ) +
    labs(x = "Integrators (k)", y = "Credibility mechanism",
         title = "(a) Net operator surplus") +
    theme_ieee(base_size = 12) +
    theme(legend.position = "right",
          axis.text.x = element_text(size = 10))

  # Panel (b): Integrator pricing markup by k, coloured by credibility.
  # Shows the competition effect (O(1/k) convergence) and its independence
  # from credibility: all credibility lines overlap exactly.
  p_pricing <- ggplot(plot_data,
                      aes(x = k_integrators, y = slice_adj_mean,
                          colour = credibility_label)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    facet_wrap(~topology_label, nrow = 1) +
    scale_colour_manual(values = palette_credibility, name = "Credibility") +
    scale_x_continuous(breaks = c(1, 2, 3, 5)) +
    labs(x = "Number of integrators (k)",
         y = "Integrator pricing markup",
         title = "(b) Pricing markup by competition level") +
    theme_ieee(base_size = 12) +
    theme(legend.position = "bottom")

  combined <- p_heat / p_pricing +
    plot_layout(heights = c(1, 1))

  save_fig(combined, "exp9_combined.pdf", width = 7, height = 6)
}


#' Supplementary figure: synergy decomposition.
#'
#' Shows marginal effects of credibility and competition separately,
#' plus the joint effect and synergy (super-additive component).
#'
#' @param summary  Aggregated summary from expJ_aggregate().
#' @return ggplot object (also saved to fig/exp9_synergy.pdf).
expJ_plot_synergy <- function(summary) {

  # Compute decomposition: for each topology
  # none/k=1 → none/k=5 (marginal competition)
  # none/k=1 → broadcast/k=1 (marginal credibility)
  # none/k=1 → broadcast/k=5 (joint)
  decomp <- summary %>%
    select(dag_type, credibility, k_integrators, net_surplus_mean) %>%
    pivot_wider(names_from = c(credibility, k_integrators),
                values_from = net_surplus_mean,
                names_sep = "_k") %>%
    mutate(
      baseline      = none_k1,
      marginal_k    = none_k5 - none_k1,
      marginal_c    = broadcast_k1 - none_k1,
      joint         = broadcast_k5 - none_k1,
      synergy       = joint - (marginal_k + marginal_c),
      topology_label = label_topology[dag_type]
    ) %>%
    select(dag_type, topology_label,
           marginal_k, marginal_c, joint, synergy) %>%
    pivot_longer(cols = c(marginal_k, marginal_c, joint, synergy),
                 names_to = "component", values_to = "value") %>%
    mutate(
      component = factor(component,
                         levels = c("marginal_k", "marginal_c", "joint", "synergy"),
                         labels = c("Competition\n(k: 1\u21925)",
                                    "Credibility\n(none\u2192broadcast)",
                                    "Joint effect",
                                    "Synergy\n(super-additive)"))
    )

  p <- ggplot(decomp, aes(x = component, y = value, fill = component)) +
    geom_col(width = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    facet_wrap(~topology_label, nrow = 1) +
    scale_fill_manual(
      values = c("#ABD9E9", "#2C7BB6", "#7B3294", "#D7191C"),
      guide = "none"
    ) +
    labs(x = NULL, y = "Change in net operator surplus",
         title = "Decomposition: Credibility \u00d7 Competition (orthogonal)") +
    theme_ieee(base_size = 12) +
    theme(axis.text.x = element_text(size = 9, angle = 15, hjust = 1))

  save_fig(p, "exp9_synergy.pdf", width = 7, height = 3.5)
}


#' Supplementary figure: profitability heatmap.
#'
#' Shows fraction of seeds where ghost-bid deviation is profitable.
#'
#' @param summary  Aggregated summary from expJ_aggregate().
#' @return ggplot object (also saved to fig/exp9_profitability.pdf).
expJ_plot_profitability <- function(summary) {

  plot_data <- summary %>%
    mutate(
      credibility_label = label_credibility[credibility],
      topology_label    = label_topology[dag_type],
      k_label           = factor(k_integrators)
    )

  p <- ggplot(plot_data,
              aes(x = k_label, y = credibility_label,
                  fill = pct_profitable)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", pct_profitable * 100)),
              size = 3, family = "serif") +
    facet_wrap(~topology_label, nrow = 1) +
    scale_fill_gradient(
      low = "#1A9641", high = "#D7191C",
      limits = c(0, 1), name = "% profitable"
    ) +
    labs(x = "Integrators (k)", y = "Credibility mechanism",
         title = "Profitability of ghost-bid deviation") +
    theme_ieee(base_size = 12) +
    theme(legend.position = "right")

  save_fig(p, "exp9_profitability.pdf", width = 7, height = 3)
}
