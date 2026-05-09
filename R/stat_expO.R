suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

## -- Experiment O Statistical Analysis ----------------------------------------
## Tests whether credibility mechanisms remain effective when infrastructure
## capacity varies over time (cyclic, shock) and whether non-stationary supply
## amplifies operator exploitation.

stat_expO <- function(results) {

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
    group_by(dag_type, capacity_model, credibility, seed) %>%
    summarise(
      mean_welfare     = mean(welfare, na.rm = TRUE),
      mean_net_surplus = mean(net_op_surplus, na.rm = TRUE),
      mean_drop_rate   = mean(drop_rate, na.rm = TRUE),
      .groups = "drop"
    )

  cat("\n=== Experiment O: Non-Stationary Service Supply Statistical Analysis ===\n\n")

  # --- Test 1: Capacity model effect on welfare (credibility = "none") --------
  cat("-- Test 1: Capacity model effect on welfare (credibility = none) --\n")
  for (dag in unique(per_seed$dag_type)) {
    subset <- per_seed %>%
      filter(credibility == "none", dag_type == dag)

    if (length(unique(subset$capacity_model)) < 2) next

    kw <- tryCatch(
      kruskal.test(mean_welfare ~ capacity_model, data = subset),
      error = function(e) NULL
    )
    if (!is.null(kw)) {
      cat(sprintf("  %s: KW H=%.2f, p=%.4g\n",
                  dag, kw$statistic, kw$p.value))
    }
  }

  # --- Test 2: Capacity model effect on operator surplus (none) ---------------
  cat("\n-- Test 2: Capacity model effect on net surplus (credibility = none) --\n")
  for (dag in unique(per_seed$dag_type)) {
    subset <- per_seed %>%
      filter(credibility == "none", dag_type == dag)

    if (length(unique(subset$capacity_model)) < 2) next

    kw <- tryCatch(
      kruskal.test(mean_net_surplus ~ capacity_model, data = subset),
      error = function(e) NULL
    )
    if (!is.null(kw)) {
      cat(sprintf("  %s: KW H=%.2f, p=%.4g\n",
                  dag, kw$statistic, kw$p.value))
    }
  }

  # --- Test 3: Cliff's delta static vs cyclic (net surplus, none) -------------
  cat("\n-- Test 3: Cliff's delta (static vs cyclic, net surplus, none) --\n")
  for (dag in unique(per_seed$dag_type)) {
    static_vals <- per_seed %>%
      filter(capacity_model == "static", credibility == "none",
             dag_type == dag) %>%
      pull(mean_net_surplus)
    cyclic_vals <- per_seed %>%
      filter(capacity_model == "cyclic", credibility == "none",
             dag_type == dag) %>%
      pull(mean_net_surplus)

    if (length(static_vals) > 0 && length(cyclic_vals) > 0) {
      delta <- cliffs_delta(cyclic_vals, static_vals)
      cat(sprintf("  %s: Cliff's delta = %.3f (%s)\n",
                  dag, delta, interpret_delta(delta)))
    }
  }

  # --- Test 4: Cliff's delta static vs shock (net surplus, none) --------------
  cat("\n-- Test 4: Cliff's delta (static vs shock, net surplus, none) --\n")
  for (dag in unique(per_seed$dag_type)) {
    static_vals <- per_seed %>%
      filter(capacity_model == "static", credibility == "none",
             dag_type == dag) %>%
      pull(mean_net_surplus)
    shock_vals <- per_seed %>%
      filter(capacity_model == "shock", credibility == "none",
             dag_type == dag) %>%
      pull(mean_net_surplus)

    if (length(static_vals) > 0 && length(shock_vals) > 0) {
      delta <- cliffs_delta(shock_vals, static_vals)
      cat(sprintf("  %s: Cliff's delta = %.3f (%s)\n",
                  dag, delta, interpret_delta(delta)))
    }
  }

  # --- Test 5: Credibility effectiveness across capacity models ---------------
  cat("\n-- Test 5: Credibility effect on welfare (per capacity model) --\n")
  for (cap in c("static", "cyclic", "shock")) {
    for (dag in unique(per_seed$dag_type)) {
      subset <- per_seed %>%
        filter(capacity_model == cap, dag_type == dag)

      if (length(unique(subset$credibility)) < 2) next

      kw <- tryCatch(
        kruskal.test(mean_welfare ~ credibility, data = subset),
        error = function(e) NULL
      )
      if (!is.null(kw)) {
        cat(sprintf("  %s / %s: KW H=%.2f, p=%.4g\n",
                    cap, dag, kw$statistic, kw$p.value))
      }
    }
  }

  # --- Test 6: Interaction test (capacity_model x credibility on welfare) -----
  cat("\n-- Test 6: ART ANOVA (capacity_model x credibility on welfare) --\n")
  for (dag in unique(per_seed$dag_type)) {
    subset <- per_seed %>%
      filter(dag_type == dag) %>%
      mutate(
        capacity_model = factor(capacity_model),
        credibility    = factor(credibility)
      )

    art_result <- tryCatch({
      if (!requireNamespace("ARTool", quietly = TRUE)) {
        stop("ARTool package not available")
      }
      m <- ARTool::art(mean_welfare ~ capacity_model * credibility,
                       data = subset)
      a <- anova(m)
      cat(sprintf("  %s:\n", dag))
      for (i in seq_len(nrow(a))) {
        cat(sprintf("    %s: F=%.2f, p=%.4g\n",
                    rownames(a)[i], a$`F`[i], a$`Pr(>F)`[i]))
      }
    }, error = function(e) {
      cat(sprintf("  %s: ART ANOVA not available (%s)\n", dag, e$message))
    })
  }

  invisible(per_seed)
}
