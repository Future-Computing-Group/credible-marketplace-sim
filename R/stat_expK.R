suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

## ── Experiment K Statistical Analysis ────────────────────────────────
## Tests whether ghost-bid profitability differs across mechanism types
## (VCG vs Myerson) and credibility conditions.

stat_expK <- function(results) {

  # Aggregate per-seed summaries for statistical testing
  per_seed <- results %>%
    filter(operator_type == "ghost_bidder") %>%
    group_by(dag_type, mechanism_cond, credibility, vdist_label, seed) %>%
    summarise(
      mean_net_surplus = mean(net_op_surplus, na.rm = TRUE),
      mean_welfare     = mean(welfare, na.rm = TRUE),
      .groups = "drop"
    )

  cat("\n═══ Experiment K: Myerson Mechanism Statistical Analysis ═══\n\n")

  # --- Test 1: Mechanism effect on net surplus (within each credibility level) ---
  cat("── Test 1: Mechanism effect on net surplus ──\n")
  for (cred in c("none", "broadcast")) {
    for (vd in unique(per_seed$vdist_label)) {
      subset <- per_seed %>%
        filter(credibility == cred, vdist_label == vd)

      if (length(unique(subset$mechanism_cond)) < 2) next

      kw <- tryCatch(
        kruskal.test(mean_net_surplus ~ mechanism_cond, data = subset),
        error = function(e) NULL
      )
      if (!is.null(kw)) {
        cat(sprintf("  %s / %s: KW H=%.2f, p=%.4g\n",
                    cred, vd, kw$statistic, kw$p.value))
      }
    }
  }

  # --- Test 2: Credibility effect on net surplus (within each mechanism) ---
  cat("\n── Test 2: Credibility effect on net surplus ──\n")
  for (mech in c("vcg", "myerson")) {
    for (vd in unique(per_seed$vdist_label)) {
      subset <- per_seed %>%
        filter(mechanism_cond == mech, vdist_label == vd)

      if (length(unique(subset$credibility)) < 2) next

      kw <- tryCatch(
        kruskal.test(mean_net_surplus ~ credibility, data = subset),
        error = function(e) NULL
      )
      if (!is.null(kw)) {
        cat(sprintf("  %s / %s: KW H=%.2f, p=%.4g\n",
                    mech, vd, kw$statistic, kw$p.value))
      }
    }
  }

  # --- Test 3: Cliff's delta for mechanism effect ---
  cat("\n── Test 3: Cliff's delta (VCG vs Myerson, no credibility) ──\n")
  for (vd in unique(per_seed$vdist_label)) {
    vcg_vals <- per_seed %>%
      filter(mechanism_cond == "vcg", credibility == "none",
             vdist_label == vd) %>%
      pull(mean_net_surplus)
    myr_vals <- per_seed %>%
      filter(mechanism_cond == "myerson", credibility == "none",
             vdist_label == vd) %>%
      pull(mean_net_surplus)

    if (length(vcg_vals) > 0 && length(myr_vals) > 0) {
      # Cliff's delta: proportion of pairs where x > y minus proportion where x < y
      pairs <- outer(vcg_vals, myr_vals, `-`)
      delta <- (sum(pairs > 0) - sum(pairs < 0)) / length(pairs)
      cat(sprintf("  %s: Cliff's delta = %.3f (%s)\n",
                  vd, delta,
                  ifelse(abs(delta) < 0.147, "negligible",
                  ifelse(abs(delta) < 0.33, "small",
                  ifelse(abs(delta) < 0.474, "medium", "large")))))
    }
  }

  # --- Test 4: Profitability summary ---
  cat("\n── Test 4: Profitability summary ──\n")
  prof_summary <- per_seed %>%
    group_by(mechanism_cond, credibility, vdist_label) %>%
    summarise(
      mean_surplus = mean(mean_net_surplus),
      pct_profitable = mean(mean_net_surplus > 0) * 100,
      .groups = "drop"
    )
  print(as.data.frame(prof_summary), row.names = FALSE)

  invisible(per_seed)
}
