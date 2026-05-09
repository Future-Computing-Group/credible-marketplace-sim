suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

## ── Experiment L Statistical Analysis ────────────────────────────────
## Tests whether operator surplus scales with stake fraction (knife-edge
## of Proposition 1: any positive stake breaks credibility).

stat_expL <- function(results) {

  # Aggregate per-seed summaries for statistical testing
  per_seed <- results %>%
    group_by(dag_type, operator_type, stake_fraction, seed) %>%
    summarise(
      mean_net_surplus = mean(net_op_surplus, na.rm = TRUE),
      mean_welfare     = mean(welfare, na.rm = TRUE),
      .groups = "drop"
    )

  cat("\n═══ Experiment L: Domain Separation Knife-Edge Statistical Analysis ═══\n\n")

  # --- Test 1: Stake fraction = 0 produces zero surplus ---
  cat("── Test 1: Zero surplus at stake_fraction = 0 ──\n")
  zero_stake <- per_seed %>% filter(stake_fraction == 0)
  for (op in unique(zero_stake$operator_type)) {
    for (dag in unique(zero_stake$dag_type)) {
      vals <- zero_stake %>%
        filter(operator_type == op, dag_type == dag) %>%
        pull(mean_net_surplus)
      cat(sprintf("  %s / %s: mean = %.6f, max = %.6f, all_zero = %s\n",
                  op, dag, mean(vals), max(abs(vals)),
                  ifelse(all(abs(vals) < 1e-10), "YES", "NO")))
    }
  }

  # --- Test 2: Spearman correlation between stake_fraction and surplus ---
  cat("\n── Test 2: Spearman correlation (stake_fraction vs surplus) ──\n")
  for (op in unique(per_seed$operator_type)) {
    for (dag in unique(per_seed$dag_type)) {
      subset <- per_seed %>%
        filter(operator_type == op, dag_type == dag)

      sp <- tryCatch(
        cor.test(subset$stake_fraction, subset$mean_net_surplus,
                 method = "spearman", exact = FALSE),
        error = function(e) NULL
      )
      if (!is.null(sp)) {
        cat(sprintf("  %s / %s: rho=%.3f, p=%.4g (%s)\n",
                    op, dag, sp$estimate, sp$p.value,
                    ifelse(sp$p.value < 0.001, "***",
                    ifelse(sp$p.value < 0.01, "**",
                    ifelse(sp$p.value < 0.05, "*", "ns")))))
      }
    }
  }

  # --- Test 3: Kruskal-Wallis on stake_fraction effect ---
  cat("\n── Test 3: Kruskal-Wallis (stake_fraction effect on surplus) ──\n")
  for (op in unique(per_seed$operator_type)) {
    for (dag in unique(per_seed$dag_type)) {
      subset <- per_seed %>%
        filter(operator_type == op, dag_type == dag) %>%
        mutate(stake_group = factor(stake_fraction))

      kw <- tryCatch(
        kruskal.test(mean_net_surplus ~ stake_group, data = subset),
        error = function(e) NULL
      )
      if (!is.null(kw)) {
        cat(sprintf("  %s / %s: KW H=%.2f, p=%.4g\n",
                    op, dag, kw$statistic, kw$p.value))
      }
    }
  }

  # --- Test 4: Cliff's delta (stake = 0 vs stake = 0.01) ---
  cat("\n── Test 4: Cliff's delta (phi=0 vs phi=0.01) ──\n")
  for (op in unique(per_seed$operator_type)) {
    for (dag in unique(per_seed$dag_type)) {
      vals_0 <- per_seed %>%
        filter(operator_type == op, dag_type == dag,
               stake_fraction == 0) %>%
        pull(mean_net_surplus)
      vals_1 <- per_seed %>%
        filter(operator_type == op, dag_type == dag,
               stake_fraction == 0.01) %>%
        pull(mean_net_surplus)

      if (length(vals_0) > 0 && length(vals_1) > 0) {
        pairs <- outer(vals_1, vals_0, `-`)
        delta <- (sum(pairs > 0) - sum(pairs < 0)) / length(pairs)
        cat(sprintf("  %s / %s: Cliff's delta = %.3f (%s)\n",
                    op, dag, delta,
                    ifelse(abs(delta) < 0.147, "negligible",
                    ifelse(abs(delta) < 0.33, "small",
                    ifelse(abs(delta) < 0.474, "medium", "large")))))
      }
    }
  }

  # --- Test 5: Profitability summary ---
  cat("\n── Test 5: Profitability summary ──\n")
  prof_summary <- per_seed %>%
    group_by(operator_type, dag_type, stake_fraction) %>%
    summarise(
      mean_surplus    = mean(mean_net_surplus),
      pct_profitable  = mean(mean_net_surplus > 0) * 100,
      .groups = "drop"
    )
  print(as.data.frame(prof_summary), row.names = FALSE)

  invisible(per_seed)
}
