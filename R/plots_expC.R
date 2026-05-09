suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

## ── Experiment C figures ──────────────────────────────────────────
## Integrator competition and market power (now with CI)

## Build C1: Welfare ratio by k integrators (reusable ggplot)
build_expC_welfare <- function(summary) {
  df <- summary %>%
    filter(load_name == "medium") %>%
    mutate(
      dag_label    = label_topology[dag_type],
      strat_label  = label_strategy[integrator_strategy],
      k_label      = factor(k_integrators)
    )

  ggplot(df, aes(x = k_label, y = welfare_ratio_mean,
                      fill = strat_label)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbar(
      aes(ymin = welfare_ratio_ci_lo,
          ymax = welfare_ratio_ci_hi),
      position = position_dodge(width = 0.7),
      width = 0.25, linewidth = 0.4
    ) +
    geom_hline(yintercept = 1.0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    facet_wrap(~dag_label) +
    scale_fill_manual(
      values = c(Competitive = "#2C7BB6", Collusive = "#D7191C"),
      name = "Strategy"
    ) +
    labs(x = "Number of Integrators (k)",
         y = "Welfare Ratio") +
    theme_ieee()
}

## Fig C1: Welfare ratio (saved individually)
plot_expC_welfare <- function(summary) {
  p <- build_expC_welfare(summary)
  save_fig(p, "expC_welfare_by_k.pdf", width = 7, height = 2.5)
}


## Build C1b: Mean agent payment by k (for combined fig)
## Shows how integrator competition reduces agent costs. Topology-averaged
## (payment is topology-invariant, like markup).
build_expC_agent_payment <- function(summary) {
  df <- summary %>%
    filter(load_name == "medium") %>%
    mutate(strat_label = label_strategy[integrator_strategy]) %>%
    group_by(k_integrators, integrator_strategy, strat_label) %>%
    summarise(
      agent_payment_mean = mean(agent_payment_mean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(k_label = factor(k_integrators))

  ggplot(df, aes(x = k_label, y = agent_payment_mean,
                      colour = strat_label, group = strat_label)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 3.5) +
    scale_colour_manual(
      values = c(Competitive = "#2C7BB6", Collusive = "#D7191C"),
      name = "Strategy"
    ) +
    labs(x = "Number of Integrators (k)",
         y = "Mean Agent Payment") +
    theme_ieee()
}


## Build C2: Price markup by k (reusable ggplot)
## Markup is topology-invariant, so we average across topologies for
## a single clean panel. Collusive is undefined at k=1 (monopoly).
build_expC_price_markup <- function(summary) {
  df <- summary %>%
    filter(load_name == "medium") %>%
    mutate(strat_label = label_strategy[integrator_strategy]) %>%
    group_by(k_integrators, integrator_strategy, strat_label) %>%
    summarise(
      price_markup_mean = mean(price_markup_mean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(k_label = factor(k_integrators))

  ggplot(df, aes(x = k_label, y = price_markup_mean,
                      colour = strat_label, group = strat_label)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 3.5) +
    geom_hline(yintercept = 1.0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    scale_colour_manual(
      values = c(Competitive = "#2C7BB6", Collusive = "#D7191C"),
      name = "Strategy"
    ) +
    labs(x = "Number of Integrators (k)",
         y = expression("Price Markup (" * times * ")")) +
    theme_ieee()
}

## Fig C2: Price markup (saved individually)
plot_expC_price_markup <- function(summary) {
  p <- build_expC_price_markup(summary)
  save_fig(p, "expC_price_markup.pdf", width = 7, height = 2.5)
}


## Build C2b: Welfare convergence by k (line plot for combined figure)
## Topology-averaged (transport cost is topology-invariant).
## Shows O(1/k) welfare convergence under Salop differentiation.
build_expC_welfare_convergence <- function(summary) {
  df <- summary %>%
    filter(load_name == "medium") %>%
    mutate(strat_label = label_strategy[integrator_strategy]) %>%
    group_by(k_integrators, integrator_strategy, strat_label) %>%
    summarise(
      welfare_ratio_mean = mean(welfare_ratio_mean, na.rm = TRUE),
      welfare_ratio_ci_lo = mean(welfare_ratio_ci_lo, na.rm = TRUE),
      welfare_ratio_ci_hi = mean(welfare_ratio_ci_hi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(k_label = factor(k_integrators))

  ggplot(df, aes(x = k_label, y = welfare_ratio_mean,
                      colour = strat_label, group = strat_label)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 3.5) +
    geom_errorbar(
      aes(ymin = welfare_ratio_ci_lo, ymax = welfare_ratio_ci_hi),
      width = 0.15, linewidth = 0.4
    ) +
    geom_hline(yintercept = 1.0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    scale_colour_manual(
      values = c(Competitive = "#2C7BB6", Collusive = "#D7191C"),
      name = "Strategy"
    ) +
    labs(x = "Number of Integrators (k)",
         y = "Welfare Ratio") +
    theme_ieee()
}

## Fig C2b: Welfare convergence (saved individually)
plot_expC_welfare_convergence <- function(summary) {
  p <- build_expC_welfare_convergence(summary)
  save_fig(p, "expC_welfare_convergence.pdf", width = 7, height = 2.5)
}


## Fig C3: Welfare by k x load level (supplementary)
plot_expC_welfare_by_load <- function(summary) {
  df <- summary %>%
    filter(integrator_strategy == "competitive") %>%
    mutate(
      dag_label = label_topology[dag_type],
      k_label   = factor(k_integrators)
    )

  p <- ggplot(df, aes(x = load_name, y = welfare_ratio_mean,
                       colour = k_label, group = k_label)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    facet_wrap(~dag_label) +
    scale_colour_manual(values = palette_k, name = "k") +
    labs(x = "Load Level", y = "Welfare Ratio") +
    theme_ieee()

  save_fig(p, "expC_welfare_by_load.pdf", width = 7, height = 2.5)
}
