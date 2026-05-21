suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(patchwork)
})

## ── Experiment L3 plots: bilinear (λ, η) surface ─────────────────────
##
## Under the BANG-BANG rational adversary at ε_max = 1/β, the per-cell
## mean operator surplus is either 0 (cells inside the credibility-
## deployable region, Π(ε_max) ≤ 0) or Π(ε_max) = (1−η)λε_max
##  − (1 − exp(−β·ε_max))·v̄·n/τ² (cells outside).
##
## Panel (a): empirical mean surplus on (λ, η) as a heatmap with discrete
##            factor axes (so tiles are uniform regardless of grid step).
## Panel (b): empirical surplus vs the closed-form Π(ε_max) prediction;
##            a 45° dashed line is the perfect-agreement reference.

EXPL3_PLOT_BETA   <- 2
EXPL3_PLOT_V_BAR  <- 1.0
EXPL3_PLOT_N      <- 40
EXPL3_PLOT_TAU    <- 20

plot_expL3_surface <- function(summary, surplus_clip = 0.5) {
  ## Combine topologies for the heatmap (theory predicts topology-independence
  ## in the interior under bang-bang).
  combined <- summary %>%
    group_by(stake_fraction, escrow_fraction) %>%
    summarise(
      mean_op_surplus = mean(mean_op_surplus, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      lambda_f = factor(stake_fraction, levels = sort(unique(stake_fraction))),
      eta_f    = factor(escrow_fraction, levels = sort(unique(escrow_fraction)))
    )

  ## Predicted-credibility overlay: where Π(ε_max) <= 0 the adversary abstains.
  eps_max <- 1.0 / EXPL3_PLOT_BETA
  hazard_e <- 1 - exp(-EXPL3_PLOT_BETA * eps_max)
  combined <- combined %>%
    mutate(
      predicted_profit = (1 - escrow_fraction) * stake_fraction * eps_max -
                         hazard_e * EXPL3_PLOT_V_BAR * EXPL3_PLOT_N /
                           (EXPL3_PLOT_TAU ^ 2),
      predicted_credible = predicted_profit <= 0
    )

  ## ── Panel (a): heatmap of empirical surplus on (λ, η) (discrete axes) ──
  ## Surplus is non-negative under bang-bang (0 inside the credibility region,
  ## Pi(eps_max) outside). Sequential scale 0 -> max; a diverging scale would
  ## waste the unused negative half.
  fill_hi <- max(combined$mean_op_surplus, na.rm = TRUE)
  if (!is.finite(fill_hi) || fill_hi <= 0) fill_hi <- surplus_clip
  p_a <- ggplot(combined, aes(x = lambda_f, y = eta_f,
                              fill = mean_op_surplus)) +
    geom_tile(colour = "grey80", linewidth = 0.2) +
    scale_fill_viridis_c(
      limits = c(0, fill_hi),
      oob = scales::squish,
      name = "Mean op.\nsurplus / rd."
    ) +
    geom_point(data = combined %>% filter(predicted_credible),
               aes(x = lambda_f, y = eta_f),
               colour = "white", size = 0.9, alpha = 0.85, shape = 4) +
    labs(
      x = expression(lambda ~ "(stake)"),
      y = expression(eta ~ "(escrow)"),
      title = expression(bold("(a) Empirical operator surplus on ") *
                        bold("(") * bold(lambda) * bold(", ") * bold(eta) * bold(") plane")),
      subtitle = bquote("Bang-bang adversary at " * epsilon[max] ~ "= 1/" * beta ~ "=" ~ .(eps_max) * "," ~
                        tau ~ "=" ~ .(EXPL3_PLOT_TAU) * "; black x = predicted credibility-deployable")
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 0, size = 8),
      axis.text.y = element_text(size = 8)
    )

  ## ── Panel (b): bang-bang threshold in the bilinear product (1−η)λ ─────
  ## A scatter of empirical-vs-Pi collapses (92% of cells abstain → pile at
  ## the origin; 3 DAG topologies are bit-identical because the ghost-bid VCG
  ## perturbation carries no topology term). Instead show the bang-bang
  ## STRUCTURE: empirical surplus as a function of the bilinear product
  ## (1−η)λ, with the predicted step-then-linear curve and the threshold.
  loss <- hazard_e * EXPL3_PLOT_V_BAR * EXPL3_PLOT_N / (EXPL3_PLOT_TAU ^ 2)
  thresh_product <- loss / eps_max   # (1−η)λ at which Π(ε_max) = 0

  bb <- summary %>%
    mutate(bilinear = (1 - escrow_fraction) * stake_fraction) %>%
    group_by(bilinear) %>%               # topologies identical → collapse
    summarise(emp = mean(mean_op_surplus), .groups = "drop")

  ## Predicted bang-bang curve over a dense product grid.
  prod_grid <- seq(0, max(bb$bilinear), length.out = 200)
  pred_curve <- data.frame(
    bilinear = prod_grid,
    pred = pmax(0, prod_grid * eps_max - loss)
  )

  p_b <- ggplot() +
    geom_vline(xintercept = thresh_product, linetype = "dotted",
               colour = "#1b7837", linewidth = 0.5) +
    annotate("text", x = thresh_product, y = max(bb$emp) * 0.9,
             label = "threshold", colour = "#1b7837", angle = 90,
             vjust = -0.4, size = 3) +
    geom_line(data = pred_curve, aes(x = bilinear, y = pred),
              colour = "#b2182b", linewidth = 0.7) +
    geom_point(data = bb, aes(x = bilinear, y = emp),
               size = 1.8, alpha = 0.8, colour = "black") +
    labs(
      x = expression("Bilinear product " * (1 - eta) * lambda),
      y = "Empirical mean op. surplus / round",
      title = "(b) Bang-bang threshold in the bilinear product",
      subtitle = "Points: empirical (topologies identical). Red: predicted bang-bang. Dotted: predicted threshold"
    ) +
    theme_minimal(base_size = 10)

  p_a / p_b +
    plot_annotation(
      title = "Experiment 8 (bilinear surface under bang-bang adversary)",
      subtitle = bquote("Per-round operator surplus across (" * lambda * ", " * eta * ") at " *
                        tau ~ "=" ~ .(EXPL3_PLOT_TAU) * ", " *
                        epsilon[max] ~ "= 1/" * beta ~ "=" ~ .(eps_max))
    )
}


save_expL3_plot <- function(summary, path = "figs/expL3_bilinear_surface.pdf",
                            width = 7.5, height = 9) {
  p <- plot_expL3_surface(summary)
  ## All Greek letters in titles/labels are bquote()/expression()-based, so the
  ## default pdf device's symbol font renders them correctly. Try cairo_pdf
  ## (better Unicode coverage) but fall back to the default device if the cairo
  ## DLL is unavailable at runtime (capabilities("cairo") can report TRUE while
  ## the shared object still fails to load, e.g. mac without X11/XQuartz).
  ok <- tryCatch({
    ggsave(path, plot = p, width = width, height = height, device = cairo_pdf)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) ggsave(path, plot = p, width = width, height = height)
  path
}


## ── Two-tau Fig 2: side-by-side (lambda, eta) heatmaps at two audit periods ──
## Panel (a): two heatmaps faceted by tau, showing the bang-bang boundary
##            (1-eta)lambda = (1-e^-1) v_bar n / (tau^2 eps_max) sweeping with
##            audit frequency. Common viridis fill so the two panels are
##            directly comparable.
## Panel (b): empirical surplus vs the bilinear product (1-eta)lambda, both tau
##            overlaid with their predicted bang-bang curves and thresholds.

plot_expL3_two_tau <- function(summary_by_tau,
                               beta = EXPL3_PLOT_BETA,
                               v_bar = EXPL3_PLOT_V_BAR,
                               n = EXPL3_PLOT_N,
                               facet_ncol = NULL) {
  eps_max <- 1.0 / beta
  hazard_e <- 1 - exp(-beta * eps_max)
  loss_of <- function(tau) hazard_e * v_bar * n / (tau ^ 2)

  ## Tau ordering + nice labels (1e6 -> "inf"). Levels ordered by numeric tau.
  taus <- sort(unique(summary_by_tau$tau_audit))
  tau_lab <- function(t) ifelse(t >= 1e5, "tau -> inf",
                                paste0("tau = ", format(t, trim = TRUE)))
  lab_levels <- tau_lab(taus)
  if (is.null(facet_ncol)) facet_ncol <- length(taus)

  ## Pool across DAG topologies (identical) per (tau, lambda, eta).
  pooled <- summary_by_tau %>%
    group_by(tau_audit, stake_fraction, escrow_fraction) %>%
    summarise(mean_op_surplus = mean(mean_op_surplus, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(
      lambda_f = factor(stake_fraction, levels = sort(unique(stake_fraction))),
      eta_f    = factor(escrow_fraction, levels = sort(unique(escrow_fraction))),
      predicted_profit = (1 - escrow_fraction) * stake_fraction * eps_max -
                         loss_of(tau_audit),
      predicted_credible = predicted_profit <= 0,
      tau_label = factor(tau_lab(tau_audit), levels = lab_levels)
    )

  fill_hi <- max(pooled$mean_op_surplus, na.rm = TRUE)
  if (!is.finite(fill_hi) || fill_hi <= 0) fill_hi <- 1

  ## ── Panel (a): one (lambda,eta) heatmap per audit period ──
  p_a <- ggplot(pooled, aes(x = lambda_f, y = eta_f, fill = mean_op_surplus)) +
    geom_tile(colour = "grey80", linewidth = 0.2) +
    scale_fill_viridis_c(limits = c(0, fill_hi), oob = scales::squish,
                         name = "Mean op.\nsurplus / rd.") +
    geom_point(data = pooled %>% filter(predicted_credible),
               aes(x = lambda_f, y = eta_f),
               colour = "white", size = 0.8, alpha = 0.85, shape = 4) +
    facet_wrap(~ tau_label, ncol = facet_ncol) +
    labs(
      x = expression(lambda ~ "(stake)"),
      y = expression(eta ~ "(escrow)"),
      title = expression(bold("(a) Empirical operator surplus on (") *
                        bold(lambda) * bold(", ") * bold(eta) * bold(") plane, per audit period")),
      subtitle = bquote("Bang-bang adversary at " * epsilon[max] ~ "= 1/" * beta ~
                        "= 0.5; " * times ~ "= predicted credibility-deployable")
    ) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          axis.text = element_text(size = 7))

  ## ── Panel (b): bang-bang threshold in (1-eta)lambda, all tau overlaid ──
  bb <- summary_by_tau %>%
    mutate(bilinear = (1 - escrow_fraction) * stake_fraction) %>%
    group_by(tau_audit, bilinear) %>%
    summarise(emp = mean(mean_op_surplus), .groups = "drop") %>%
    mutate(tau_f = factor(tau_lab(tau_audit), levels = lab_levels))

  prod_grid <- seq(0, max(bb$bilinear), length.out = 200)
  pred_curve <- do.call(rbind, lapply(taus, function(tt) {
    data.frame(tau_f = factor(tau_lab(tt), levels = lab_levels),
               bilinear = prod_grid,
               pred = pmax(0, prod_grid * eps_max - loss_of(tt)))
  }))
  thresh <- data.frame(tau_f = factor(tau_lab(taus), levels = lab_levels),
                       x = loss_of(taus) / eps_max)
  thresh <- thresh[is.finite(thresh$x) & thresh$x <= max(prod_grid), ]

  p_b <- ggplot() +
    geom_vline(data = thresh, aes(xintercept = x, colour = tau_f),
               linetype = "dotted", linewidth = 0.5, show.legend = FALSE) +
    geom_line(data = pred_curve, aes(x = bilinear, y = pred, colour = tau_f),
              linewidth = 0.7) +
    geom_point(data = bb, aes(x = bilinear, y = emp, colour = tau_f),
               size = 1.4, alpha = 0.8) +
    scale_colour_viridis_d(end = 0.9, name = expression(tau),
                           limits = lab_levels, breaks = lab_levels) +
    labs(
      x = expression("Bilinear product " * (1 - eta) * lambda),
      y = "Empirical mean op. surplus / round",
      title = "(b) Bang-bang threshold in the bilinear product",
      subtitle = "Points: empirical (topologies identical). Lines: predicted bang-bang. Dotted: thresholds"
    ) +
    theme_minimal(base_size = 10)

  p_a / p_b + patchwork::plot_layout(heights = c(1.4, 0.8)) +
    plot_annotation(
      title = "Credibility-deployable surface under the rational bang-bang adversary"
    )
}


save_expL3_two_tau_plot <- function(summary_by_tau,
                                    path = "figs/expL3_bilinear_surface.pdf",
                                    width = 8.5, height = 9,
                                    facet_ncol = NULL) {
  p <- plot_expL3_two_tau(summary_by_tau, facet_ncol = facet_ncol)
  ok <- tryCatch({
    ggsave(path, plot = p, width = width, height = height, device = cairo_pdf)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) ggsave(path, plot = p, width = width, height = height)
  path
}
