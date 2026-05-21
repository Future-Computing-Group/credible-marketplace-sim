suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(patchwork)
})

## ── Experiment L4 plots: 3D credibility-deployable surface (λ, η, τ) ──
##
## Small-multiples on τ: one (λ, η) heatmap per audit period. Axes are
## treated as discrete factors so tile widths are uniform regardless of
## the underlying λ/η grid spacing. The fill scale is clipped to a
## modest range centred at 0 so the credibility boundary (surplus near
## zero) is the visual signal, rather than the saturated deep-green of
## the heavily-audited slices where surplus reaches ~−9 per round.

plot_expL4_3d_surface <- function(summary, surplus_clip = 1) {
  pred_col <- if ("predicted_credible" %in% names(summary))
                "predicted_credible"
              else "predicted_credible_corrected"

  combined <- summary %>%
    group_by(stake_fraction, escrow_fraction, tau_audit) %>%
    summarise(
      mean_op_surplus    = mean(mean_op_surplus, na.rm = TRUE),
      predicted_credible = all(.data[[pred_col]]),
      empirical_credible = mean(mean_op_surplus, na.rm = TRUE) <= 0,
      .groups = "drop"
    )

  combined <- combined %>%
    mutate(
      tau_label = ifelse(tau_audit >= 1e5,
                         "tau -> infty (audit-free)",
                         paste0("tau = ", tau_audit)),
      lambda_f  = factor(stake_fraction,
                         levels = sort(unique(stake_fraction))),
      eta_f     = factor(escrow_fraction,
                         levels = sort(unique(escrow_fraction)))
    )
  combined$tau_label <- factor(combined$tau_label,
                               levels = unique(combined$tau_label[
                                 order(combined$tau_audit)
                               ]))

  ## Surplus is non-negative under the bang-bang adversary: a credibility-
  ## deployable cell has the adversary abstaining (surplus = 0) and an active
  ## cell has positive surplus Pi(eps_max). Use a SEQUENTIAL scale 0 -> max
  ## (white = credibility-deployable, red = operator extraction); a diverging
  ## scale would waste the unused negative half.
  fill_hi <- max(combined$mean_op_surplus, na.rm = TRUE)
  if (!is.finite(fill_hi) || fill_hi <= 0) fill_hi <- surplus_clip
  ggplot(combined, aes(x = lambda_f, y = eta_f,
                       fill = mean_op_surplus)) +
    geom_tile(colour = "grey80", linewidth = 0.2) +
    scale_fill_viridis_c(
      limits = c(0, fill_hi),
      oob = scales::squish,
      name = "Mean op.\nsurplus / rd."
    ) +
    geom_point(data = combined %>% filter(empirical_credible),
               aes(x = lambda_f, y = eta_f),
               colour = "white", size = 0.9, alpha = 0.85, shape = 4) +
    facet_wrap(~ tau_label, ncol = 3) +
    labs(
      x = expression(lambda ~ "(stake)"),
      y = expression(eta ~ "(escrow)"),
      title = "Experiment 9 (3D credibility-deployable surface)",
      subtitle = sprintf(
        "Per-tau slices of the (lambda, eta, tau) surface; fill clipped to +/-%g; black x = empirically credibility-deployable (surplus <= 0)",
        surplus_clip)
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 0, size = 8),
      axis.text.y = element_text(size = 8)
    )
}


save_expL4_plot <- function(summary, path = "figs/expL4_3d_surface.pdf",
                            width = 9, height = 6, surplus_clip = 1) {
  p <- plot_expL4_3d_surface(summary, surplus_clip = surplus_clip)
  ## Try cairo_pdf (Unicode); fall back to default pdf if the cairo DLL fails
  ## to load at runtime (capabilities() can report TRUE while dlopen fails).
  ok <- tryCatch({
    ggsave(path, plot = p, width = width, height = height, device = cairo_pdf)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) ggsave(path, plot = p, width = width, height = height)
  path
}
