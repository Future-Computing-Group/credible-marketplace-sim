suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

## -- Experiment N Statistical Analysis ----------------------------------------
## Tests how correlated (Markov) broadcast channel failures affect the ghost-bid
## profitability threshold relative to the i.i.d. baseline (Experiment I).

stat_expN <- function(results) {

  # Cliff's delta: proportion of pairs where x > y minus proportion where x < y
  cliffs_delta <- function(x, y) {
    n <- length(x); m <- length(y)
    sum(sign(outer(x, y, "-"))) / (n * m)
  }

  interpret_delta <- function(d) {
    ifelse(abs(d) < 0.147, "negligible",
    ifelse(abs(d) < 0.33, "small",
    ifelse(abs(d) < 0.474, "medium", "large")))
  }

  # Aggregate per-seed summaries for statistical testing
  per_seed <- results %>%
    group_by(dag_type, channel_model, p_stationary, rho, seed) %>%
    summarise(
      mean_net_surplus = mean(net_op_surplus, na.rm = TRUE),
      detection_rate   = mean(detected, na.rm = TRUE),
      .groups = "drop"
    )

  cat("\n=== Experiment N: Markov Broadcast Channel Statistical Analysis ===\n\n")

  # --- Test 1: Channel model effect on net surplus (within each p_stationary) -
  cat("-- Test 1: Channel model effect on net surplus --\n")
  for (p_s in sort(unique(per_seed$p_stationary))) {
    for (dag in unique(per_seed$dag_type)) {
      subset <- per_seed %>%
        filter(p_stationary == p_s, dag_type == dag)

      if (length(unique(subset$channel_model)) < 2) next

      kw <- tryCatch(
        kruskal.test(mean_net_surplus ~ channel_model, data = subset),
        error = function(e) NULL
      )
      if (!is.null(kw)) {
        cat(sprintf("  p_stat=%.1f / %s: KW H=%.2f, p=%.4g\n",
                    p_s, dag, kw$statistic, kw$p.value))
      }
    }
  }

  # --- Test 2: Pairwise Wilcoxon (i.i.d. vs each Markov level) ---------------
  cat("\n-- Test 2: Pairwise Wilcoxon (i.i.d. vs each Markov level) --\n")
  markov_levels <- c("markov_low", "markov_med", "markov_high")
  for (p_s in sort(unique(per_seed$p_stationary))) {
    for (dag in unique(per_seed$dag_type)) {
      iid_vals <- per_seed %>%
        filter(channel_model == "iid", p_stationary == p_s,
               dag_type == dag) %>%
        pull(mean_net_surplus)

      for (mk in markov_levels) {
        mk_vals <- per_seed %>%
          filter(channel_model == mk, p_stationary == p_s,
                 dag_type == dag) %>%
          pull(mean_net_surplus)

        if (length(iid_vals) > 0 && length(mk_vals) > 0) {
          wt <- tryCatch(
            wilcox.test(iid_vals, mk_vals, exact = FALSE),
            error = function(e) NULL
          )
          if (!is.null(wt)) {
            cat(sprintf("  p_stat=%.1f / %s / iid vs %s: W=%.1f, p=%.4g\n",
                        p_s, dag, mk, wt$statistic, wt$p.value))
          }
        }
      }
    }
  }

  # --- Test 3: Cliff's delta (i.i.d. vs markov_high at each p_stationary) ----
  cat("\n-- Test 3: Cliff's delta (i.i.d. vs markov_high) --\n")
  for (p_s in sort(unique(per_seed$p_stationary))) {
    for (dag in unique(per_seed$dag_type)) {
      iid_vals <- per_seed %>%
        filter(channel_model == "iid", p_stationary == p_s,
               dag_type == dag) %>%
        pull(mean_net_surplus)
      mh_vals <- per_seed %>%
        filter(channel_model == "markov_high", p_stationary == p_s,
               dag_type == dag) %>%
        pull(mean_net_surplus)

      if (length(iid_vals) > 0 && length(mh_vals) > 0) {
        delta <- cliffs_delta(mh_vals, iid_vals)
        cat(sprintf("  p_stat=%.1f / %s: Cliff's delta = %.3f (%s)\n",
                    p_s, dag, delta, interpret_delta(delta)))
      }
    }
  }

  # --- Test 4: Spearman correlation (rho vs mean_net_surplus) -----------------
  cat("\n-- Test 4: Spearman correlation (rho vs net surplus, pooled) --\n")
  for (dag in unique(per_seed$dag_type)) {
    subset <- per_seed %>%
      filter(dag_type == dag)

    sp <- tryCatch(
      cor.test(subset$rho, subset$mean_net_surplus,
               method = "spearman", exact = FALSE),
      error = function(e) NULL
    )
    if (!is.null(sp)) {
      cat(sprintf("  %s: rho_s=%.3f, p=%.4g (%s)\n",
                  dag, sp$estimate, sp$p.value,
                  ifelse(sp$p.value < 0.001, "***",
                  ifelse(sp$p.value < 0.01, "**",
                  ifelse(sp$p.value < 0.05, "*", "ns")))))
    }
  }

  invisible(per_seed)
}
