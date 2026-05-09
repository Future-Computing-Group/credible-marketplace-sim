suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

## -- Experiment M Statistical Analysis ----------------------------------------
## Tests whether exit-sensitive agents amplify welfare loss under operator
## deviation and whether credibility mechanisms prevent agent attrition.

stat_expM <- function(results) {

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
    group_by(dag_type, credibility, agent_mode, seed) %>%
    summarise(
      mean_welfare          = mean(welfare, na.rm = TRUE),
      mean_net_surplus      = mean(net_op_surplus, na.rm = TRUE),
      final_fraction_active = last(fraction_active),
      .groups = "drop"
    )

  cat("\n=== Experiment M: Strategic Agent Adaptation Statistical Analysis ===\n\n")

  # --- Test 1: Agent mode effect on welfare (within credibility = "none") -----
  cat("-- Test 1: Agent mode effect on welfare (credibility = none) --\n")
  for (dag in unique(per_seed$dag_type)) {
    subset <- per_seed %>%
      filter(credibility == "none", dag_type == dag)

    if (length(unique(subset$agent_mode)) < 2) next

    kw <- tryCatch(
      kruskal.test(mean_welfare ~ agent_mode, data = subset),
      error = function(e) NULL
    )
    if (!is.null(kw)) {
      cat(sprintf("  %s: KW H=%.2f, p=%.4g\n",
                  dag, kw$statistic, kw$p.value))
    }
  }

  # --- Test 2: Cliff's delta passive vs exit_sensitive welfare (none) ---------
  cat("\n-- Test 2: Cliff's delta (passive vs exit_sensitive welfare, none) --\n")
  for (dag in unique(per_seed$dag_type)) {
    passive_vals <- per_seed %>%
      filter(credibility == "none", agent_mode == "passive",
             dag_type == dag) %>%
      pull(mean_welfare)
    exit_vals <- per_seed %>%
      filter(credibility == "none", agent_mode == "exit_sensitive",
             dag_type == dag) %>%
      pull(mean_welfare)

    if (length(passive_vals) > 0 && length(exit_vals) > 0) {
      delta <- cliffs_delta(passive_vals, exit_vals)
      cat(sprintf("  %s: Cliff's delta = %.3f (%s)\n",
                  dag, delta, interpret_delta(delta)))
    }
  }

  # --- Test 3: Cliff's delta passive vs exit_sensitive welfare (broadcast) ----
  cat("\n-- Test 3: Cliff's delta (passive vs exit_sensitive welfare, broadcast) --\n")
  for (dag in unique(per_seed$dag_type)) {
    passive_vals <- per_seed %>%
      filter(credibility == "broadcast", agent_mode == "passive",
             dag_type == dag) %>%
      pull(mean_welfare)
    exit_vals <- per_seed %>%
      filter(credibility == "broadcast", agent_mode == "exit_sensitive",
             dag_type == dag) %>%
      pull(mean_welfare)

    if (length(passive_vals) > 0 && length(exit_vals) > 0) {
      delta <- cliffs_delta(passive_vals, exit_vals)
      cat(sprintf("  %s: Cliff's delta = %.3f (%s)\n",
                  dag, delta, interpret_delta(delta)))
    }
  }

  # --- Test 4: Agent retention under "none" vs "broadcast" (exit_sensitive) ---
  cat("\n-- Test 4: Agent retention (none vs broadcast, exit_sensitive only) --\n")
  for (dag in unique(per_seed$dag_type)) {
    subset <- per_seed %>%
      filter(agent_mode == "exit_sensitive", dag_type == dag,
             credibility %in% c("none", "broadcast"))

    if (length(unique(subset$credibility)) < 2) next

    kw <- tryCatch(
      kruskal.test(final_fraction_active ~ credibility, data = subset),
      error = function(e) NULL
    )
    if (!is.null(kw)) {
      cat(sprintf("  %s: KW H=%.2f, p=%.4g\n",
                  dag, kw$statistic, kw$p.value))

      # Also report medians for interpretability
      medians <- subset %>%
        group_by(credibility) %>%
        summarise(med = median(final_fraction_active), .groups = "drop")
      for (i in seq_len(nrow(medians))) {
        cat(sprintf("    %s median retention = %.3f\n",
                    medians$credibility[i], medians$med[i]))
      }
    }
  }

  invisible(per_seed)
}
