suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
})

## ── Plots for Experiment R5 (TEAC R-5 panels) ────────────────────────
## (a) CoNC^op + CoNC^ag grouped bars by mechanism × dag
## (b) γ_ij histogram per topology (one facet per topology)
## (c) Realisation-wise vs in-expectation reconciliation

plot_expR5_conc_by_mechanism <- function(summary_obj, bar_width = 0.6,
                                         panel_tag = NULL) {
  ## The CoNC variants are ADDITIVE: CoNC^ag = CoNC^op + CoNC^W (operator
  ## transfer + welfare destruction). So we STACK op (bottom) + W (top); the
  ## total bar height is the agent-side cost CoNC^ag. Topology barely matters
  ## here (CoNC^op spread across tree/sp/entangled is < 0.013), so we pool
  ## across DAG topologies and show the small spread as an error bar on the
  ## total — freeing the figure to make the mechanism-class comparison and the
  ## op/W decomposition the visual message (contrast Fig 4, where topology IS
  ## the message and gets a shared-axis treatment).
  per_dag <- summary_obj$summary %>%
    filter(operator_lbl != "truthful") %>%
    group_by(mechanism_lbl, operator_lbl, dag_type_lbl) %>%
    summarise(CoNC_op = mean(CoNC_op, na.rm = TRUE),
              CoNC_W  = mean(CoNC_W,  na.rm = TRUE),
              CoNC_ag = mean(CoNC_ag, na.rm = TRUE), .groups = "drop")

  pooled <- per_dag %>%
    group_by(mechanism_lbl, operator_lbl) %>%
    summarise(op = mean(CoNC_op), W = mean(CoNC_W),
              ag = mean(CoNC_ag), ag_sd = sd(CoNC_ag), .groups = "drop") %>%
    mutate(mech_op = paste(mechanism_lbl, operator_lbl, sep = " / ")) %>%
    arrange(ag) %>%
    mutate(mech_op = factor(mech_op, levels = mech_op))

  stack_df <- pooled %>%
    select(mech_op, op, W) %>%
    pivot_longer(c(op, W), names_to = "component", values_to = "value") %>%
    mutate(component = factor(component, levels = c("W", "op")))

  ggplot(stack_df, aes(x = mech_op, y = value, fill = component)) +
    geom_col(width = bar_width, colour = "white", linewidth = 0.3) +
    geom_errorbar(data = pooled, inherit.aes = FALSE,
                  aes(x = mech_op, ymin = ag - ag_sd, ymax = ag + ag_sd),
                  width = 0.18, linewidth = 0.4) +
    scale_fill_manual(
      values = c(W = "#FDAE61", op = "#D7191C"),
      breaks = c("W", "op"),
      labels = c(expression(CoNC^{W} ~ "(welfare destruction)"),
                 expression(CoNC^{op} ~ "(operator transfer)")),
      name = NULL) +
    labs(x = NULL, y = expression(CoNC ~ "(dimensionless)"),
         title = panel_tag %||% "Cost of Non-Credibility by mechanism / operator",
         subtitle = "Stacked: operator transfer + welfare loss = agent cost (pooled over topologies; error bar = spread)") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1),
          legend.position = "bottom",
          plot.margin = margin(t = 5, r = 8, b = 5, l = 14))
}


## Cliff's delta effect size: P(X>Y) - P(X<Y) for two samples. Honest
## overlap-aware measure of how separated two distributions are
## (+1 = X strictly above Y, 0 = full overlap). Used here to quantify the
## tree -> sp -> entangled shift without a parametric model or a p-value.
cliffs_delta <- function(x, y) {
  gt <- sum(outer(x, y, ">"))
  lt <- sum(outer(x, y, "<"))
  (gt - lt) / (length(x) * length(y))
}

plot_expR5_gamma_distribution <- function(summary_obj) {
  ## All three topologies on ONE shared axis so the rightward shift
  ## (tree < sp < entangled) and the honest overlap are visible at a glance.
  ## Kernel densities (not a parametric fit: gamma_ij is a deterministic
  ## pushforward of the value/deadline draws, so we describe shape without
  ## claiming a distributional family). Mean markers + adjacent-pair Cliff's
  ## delta quantify the shift.
  topos <- c("tree", "sp", "entangled")
  pal   <- c(tree = "#1b9e77", sp = "#7570b3", entangled = "#d95f02")
  all_df <- purrr::map_dfr(topos, function(top) {
    g <- compute_gamma_distribution(make_dag(top), make_env(),
                                    n_samples = 500, seed = 42)
    tibble(dag_type_lbl = factor(top, levels = topos), gamma = g)
  })
  means <- all_df %>% group_by(dag_type_lbl) %>%
    summarise(mu = mean(gamma), .groups = "drop")

  ## Adjacent-pair Cliff's delta (tree->sp, sp->entangled). plotmath label
  ## so the Greek delta and arrows render on the default pdf device.
  gv <- function(t) all_df$gamma[all_df$dag_type_lbl == t]
  d_ts <- cliffs_delta(gv("sp"),        gv("tree"))
  d_se <- cliffs_delta(gv("entangled"), gv("sp"))
  ann <- sprintf(
    "\"Cliff's \" * delta * \": tree\" %%->%% \"sp =\" ~ %.2f * \", sp\" %%->%% \"entangled =\" ~ %.2f",
    d_ts, d_se)

  ggplot(all_df, aes(x = gamma, colour = dag_type_lbl, fill = dag_type_lbl)) +
    geom_density(alpha = 0.25, linewidth = 0.7) +
    geom_vline(data = means, aes(xintercept = mu, colour = dag_type_lbl),
               linetype = "dashed", linewidth = 0.5, show.legend = FALSE) +
    scale_colour_manual(values = pal, name = "Topology") +
    scale_fill_manual(values = pal, name = "Topology") +
    annotate("text", x = Inf, y = Inf, label = ann, parse = TRUE,
             hjust = 1.02, vjust = 1.6, size = 3.1) +
    labs(x = expression(gamma[ij] ~ "(realised submodularity gap)"),
         y = "Density",
         title = expression("Submodularity gap " * gamma[ij] *
                            " shifts with topology: tree < sp < entangled"),
         subtitle = "Dashed lines: per-topology means. Distributions overlap; the ordering is in the central tendency.") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right",
          plot.margin = margin(t = 5, r = 10, b = 5, l = 10))
}


## Helper: ag-ordered mechanism/operator levels (deviations only), so the
## bar panel and the violin panel share an identical x-axis.
.expR5_mechop_levels <- function(summary_obj) {
  summary_obj$summary %>%
    filter(operator_lbl != "truthful") %>%
    group_by(mechanism_lbl, operator_lbl) %>%
    summarise(ag = mean(CoNC_ag, na.rm = TRUE), .groups = "drop") %>%
    arrange(ag) %>%
    mutate(mech_op = paste(mechanism_lbl, operator_lbl, sep = " / ")) %>%
    pull(mech_op)
}

## Panel (b): the REALISATION distribution. Per-round net operator surplus
## across all rounds/seeds/topologies per adversarial condition, as violins
## against the zero line. Shows the trilemma is realisation-wise — the bulk
## of per-round realisations sits above 0, not merely the mean. (A different
## glyph from the bars because it is a different quantity: a distribution of
## realisations, not a decomposed aggregate.)
plot_expR5_surplus_realisation <- function(results_raw, lvl) {
  df <- results_raw %>%
    filter(operator_lbl != "truthful") %>%
    mutate(mech_op = factor(paste(mechanism_lbl, operator_lbl, sep = " / "),
                            levels = lvl))
  ggplot(df, aes(x = mech_op, y = net_op_surplus)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3,
               colour = "grey50") +
    geom_violin(fill = "#2C7BB6", alpha = 0.35, colour = "#2C7BB6",
                linewidth = 0.4, scale = "width") +
    stat_summary(fun = median, geom = "point", size = 1.4, colour = "#08519c") +
    labs(x = NULL, y = "Per-round operator surplus",
         title = "(b) Realisation distribution",
         subtitle = "Per-round surplus across rounds/seeds/topologies; bulk above 0 = realisation-wise extraction") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

## Combined two-panel figure: (a) decomposed aggregate cost (stacked bars),
## (b) realisation distribution (violins), on a shared mechanism/operator
## x-axis. Replaces the former separate conc-bars and realisation figures.
plot_expR5_conc_combined <- function(summary_obj, results_raw) {
  lvl <- .expR5_mechop_levels(summary_obj)
  p_a <- plot_expR5_conc_by_mechanism(summary_obj, bar_width = 0.5,
                                      panel_tag = "(a) Aggregate cost decomposition")
  p_b <- plot_expR5_surplus_realisation(results_raw, lvl)
  p_a + p_b + patchwork::plot_layout(widths = c(1, 1))
}


save_expR5_plots <- function(summary_obj,
                              out_dir = "figs",
                              prefix  = "expR5") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  p1 <- plot_expR5_conc_by_mechanism(summary_obj)   # Fig: CoNC stacked bars
  p2 <- plot_expR5_gamma_distribution(summary_obj)  # Fig: gamma_ij densities
  ggsave(file.path(out_dir, paste0(prefix, "_conc_by_mechanism.pdf")),
         p1, width = 8, height = 4.4)
  ggsave(file.path(out_dir, paste0(prefix, "_gamma_distribution.pdf")),
         p2, width = 9, height = 3.8)
  invisible(list(p1 = p1, p2 = p2))
}
