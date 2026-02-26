suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(patchwork)
})

## ── Combined figures for main paper ─────────────────────────────
## Each experiment gets a multi-panel figure combining its key plots.
## Panel labels use (a), (b) via patchwork::plot_annotation(tag_levels).

## Exp 1 (sim code A): Welfare loss + Operator surplus
plot_exp1_combined <- function(summary) {
  p1 <- build_expA_welfare(summary) +
    labs(title = NULL) +
    theme(legend.position = "none")
  p2 <- build_expA_surplus(summary) +
    labs(title = NULL)

  combined <- (p1 | p2) +
    plot_layout(widths = c(1, 1)) +
    plot_annotation(tag_levels = "a", theme = theme(plot.tag = element_text(size = 16, face = "bold")))

  save_fig(combined, "exp1_combined.pdf", width = 7, height = 3.8)
}


## Exp 2 (sim code E): Trilemma surplus + Detection vs profit
plot_exp2_combined <- function(summary) {
  p1 <- build_expE_trilemma(summary) +
    labs(title = NULL)
  p2 <- build_expE_detection_vs_profit(summary) +
    labs(title = NULL)

  combined <- (p1 | p2) +
    plot_layout(widths = c(2, 1)) +
    plot_annotation(tag_levels = "a", theme = theme(plot.tag = element_text(size = 16, face = "bold")))

  save_fig(combined, "exp2_combined.pdf", width = 7, height = 4.5)
}


## Exp 3 (sim code B): Welfare recovery + Tradeoff
plot_exp3_combined <- function(summary) {
  p1 <- build_expB_welfare_recovery(summary) +
    labs(title = NULL)
  p2 <- build_expB_tradeoff(summary) +
    labs(title = NULL)

  combined <- (p1 / p2) +
    plot_layout(heights = c(2, 1)) +
    plot_annotation(tag_levels = "a", theme = theme(plot.tag = element_text(size = 16, face = "bold")))

  save_fig(combined, "exp3_combined.pdf", width = 7, height = 8)
}


## Exp 4 (sim code F): Surplus + Welfare ratio (domain separation)
plot_exp4_combined <- function(summary) {
  p1 <- build_expF_surplus(summary) +
    labs(title = NULL)
  p2 <- build_expF_welfare(summary) +
    labs(title = NULL)

  combined <- (p1 / p2) +
    plot_annotation(tag_levels = "a", theme = theme(plot.tag = element_text(size = 16, face = "bold")))

  save_fig(combined, "exp4_combined.pdf", width = 7, height = 6)
}


## Exp 5 (sim code C): Price markup by k (single panel)
## Markup is the key differentiating metric; welfare = 1.0 for all conditions
## (noted in caption) and agent payment is proportional to markup.
## Full-width single panel for readability at font size 14.
plot_exp5_combined <- function(summary) {
  p <- build_expC_price_markup(summary) +
    labs(title = NULL)

  save_fig(p, "exp5_combined.pdf", width = 7, height = 2.5)
}


## Exp 6 (sim code D): Trust heatmap + Cost-benefit frontier
plot_exp6_combined <- function(summary) {
  p1 <- build_expD_heatmap(summary) +
    labs(title = NULL)
  p2 <- build_expD_frontier(summary) +
    labs(title = NULL)

  combined <- (p1 / p2) +
    plot_annotation(tag_levels = "a", theme = theme(plot.tag = element_text(size = 16, face = "bold")))

  save_fig(combined, "exp6_combined.pdf", width = 7, height = 6.5)
}


## Exp 7 (sim code H): Adaptive operator — deviation + cumulative surplus
## Panel (a): deviation magnitude trajectory shows the operator learns to
##            stop deviating under broadcast/blockchain.
## Panel (b): cumulative operator surplus shows the economic consequence —
##            broadcast/blockchain operators accumulate losses, none accumulates
##            gains, exchange stays at zero.
plot_exp7_combined <- function(agg) {
  p1 <- build_expH_deviation_trajectory(agg) +
    labs(title = NULL) +
    theme(legend.position = "none")
  p2 <- build_expH_cumulative_surplus(agg) +
    labs(title = NULL)

  combined <- (p1 / p2) +
    plot_annotation(tag_levels = "a", theme = theme(plot.tag = element_text(size = 16, face = "bold")))

  save_fig(combined, "exp7_combined.pdf", width = 7, height = 6)
}


## Exp 8 (sim code I): Imperfect broadcast — welfare + surplus vs p_broadcast
plot_exp8_combined <- function(agg) {
  p1 <- build_expI_welfare_vs_broadcast(agg) +
    labs(title = NULL) +
    theme(legend.position = "none")
  p2 <- build_expI_surplus_vs_broadcast(agg) +
    labs(title = NULL)

  combined <- (p1 / p2) +
    plot_annotation(tag_levels = "a", theme = theme(plot.tag = element_text(size = 16, face = "bold")))

  save_fig(combined, "exp8_combined.pdf", width = 7, height = 5.5)
}
