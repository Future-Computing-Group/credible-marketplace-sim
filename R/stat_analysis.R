# stat_analysis.R
# ---------------------------------------------------------------------------
# Statistical analysis functions for the credibility ablation study.
#
# Provides:
#   - Bootstrap 95% CIs (BCa with fallback)
#   - Kruskal-Wallis and pairwise Wilcoxon tests (Holm-corrected)
#   - Cliff's delta effect size
#   - Aligned Rank Transform ANOVA for interaction analysis
#   - Complementarity / synergy tests
#   - Per-experiment statistical summaries (Experiments A--J)
#   - LaTeX table formatting for supplementary material
#
# All functions operate on per-seed (raw) result tibbles.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(boot)
})


# ===========================================================================
# Bootstrap confidence intervals
# ===========================================================================

#' Compute BCa bootstrap 95% CI for a metric across seeds.
#'
#' @param x       Numeric vector (one value per seed).
#' @param n_boot  Number of bootstrap resamples (default: 2000).
#' @param conf    Confidence level (default: 0.95).
#' @return A single-row tibble: mean, lo, hi.
bootstrap_ci <- function(x, n_boot = 2000L, conf = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0L) return(tibble(mean = NA_real_, lo = NA_real_, hi = NA_real_))
  if (n == 1L) return(tibble(mean = x, lo = x, hi = x))
  if (n == 2L) {
    # BCa fails with n=2; use range
    return(tibble(mean = mean(x), lo = min(x), hi = max(x)))
  }

  stat_fn <- function(d, i) mean(d[i])
  b <- tryCatch(
    boot::boot(x, stat_fn, R = n_boot),
    error = function(e) NULL
  )
  if (is.null(b)) {
    se <- sd(x) / sqrt(n)
    m  <- mean(x)
    return(tibble(mean = m, lo = m - 1.96 * se, hi = m + 1.96 * se))
  }

  ci <- tryCatch(
    boot::boot.ci(b, conf = conf, type = "bca"),
    error = function(e) NULL
  )

  m <- mean(x)
  if (!is.null(ci) && !is.null(ci$bca)) {
    tibble(mean = m, lo = ci$bca[4], hi = ci$bca[5])
  } else {
    # Fallback to percentile
    ci_p <- tryCatch(
      boot::boot.ci(b, conf = conf, type = "perc"),
      error = function(e) NULL
    )
    if (!is.null(ci_p) && !is.null(ci_p$percent)) {
      tibble(mean = m, lo = ci_p$percent[4], hi = ci_p$percent[5])
    } else {
      se <- sd(x) / sqrt(n)
      tibble(mean = m, lo = m - 1.96 * se, hi = m + 1.96 * se)
    }
  }
}


#' Compute bootstrap CIs for all numeric metrics in a grouped data frame.
#'
#' @param raw_df    Per-seed results (one row per seed per condition).
#' @param group_vars Character vector of grouping column names.
#' @param metrics   Character vector of metric column names.
#' @param n_boot    Number of bootstrap resamples.
#' @return A tibble with group columns, and for each metric: mean, lo, hi.
bootstrap_ci_grouped <- function(raw_df, group_vars, metrics, n_boot = 2000L) {
  raw_df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      across(
        all_of(metrics),
        list(
          mean = \(x) bootstrap_ci(x, n_boot = n_boot)$mean,
          lo   = \(x) bootstrap_ci(x, n_boot = n_boot)$lo,
          hi   = \(x) bootstrap_ci(x, n_boot = n_boot)$hi
        ),
        .names = "{.col}_{.fn}"
      ),
      n_seeds = n(),
      .groups = "drop"
    )
}


# ===========================================================================
# Hypothesis tests
# ===========================================================================

#' Kruskal-Wallis test for a metric across groups.
#'
#' @param raw_df    Per-seed results.
#' @param group_var Name of the grouping column (string).
#' @param metric    Name of the metric column (string).
#' @return A single-row tibble: metric, group_var, H, df, p_value.
kruskal_test <- function(raw_df, group_var, metric) {
  vals   <- raw_df[[metric]]
  groups <- raw_df[[group_var]]
  valid  <- is.finite(vals) & !is.na(groups)
  vals   <- vals[valid]
  groups <- factor(groups[valid])

  if (nlevels(groups) < 2 || length(vals) < 3) {
    return(tibble(metric = metric, group_var = group_var,
                  H = NA_real_, df = NA_integer_, p_value = NA_real_))
  }

  kt <- kruskal.test(vals ~ groups)
  tibble(
    metric    = metric,
    group_var = group_var,
    H         = kt$statistic,
    df        = kt$parameter,
    p_value   = kt$p.value
  )
}


#' Pairwise Wilcoxon rank-sum tests with Holm correction.
#'
#' @param raw_df    Per-seed results.
#' @param group_var Name of the grouping column.
#' @param metric    Name of the metric column.
#' @return A tibble: metric, group1, group2, W, p_raw, p_adj, significant.
pairwise_wilcox <- function(raw_df, group_var, metric) {
  vals   <- raw_df[[metric]]
  groups <- raw_df[[group_var]]
  valid  <- is.finite(vals) & !is.na(groups)
  vals   <- vals[valid]
  groups <- factor(groups[valid])
  lvls   <- levels(groups)

  if (length(lvls) < 2) {
    return(tibble(metric = character(), group1 = character(),
                  group2 = character(), W = numeric(),
                  p_raw = numeric(), p_adj = numeric(),
                  significant = logical()))
  }

  pairs <- combn(lvls, 2, simplify = FALSE)
  results <- lapply(pairs, function(pair) {
    x <- vals[groups == pair[1]]
    y <- vals[groups == pair[2]]
    if (length(x) < 2 || length(y) < 2) {
      return(tibble(metric = metric, group1 = pair[1], group2 = pair[2],
                    W = NA_real_, p_raw = NA_real_))
    }
    wt <- wilcox.test(x, y, exact = FALSE)
    tibble(metric = metric, group1 = pair[1], group2 = pair[2],
           W = wt$statistic, p_raw = wt$p.value)
  })

  out <- bind_rows(results)
  out$p_adj <- p.adjust(out$p_raw, method = "holm")
  out$significant <- out$p_adj < 0.05
  out
}


# ===========================================================================
# Effect sizes
# ===========================================================================

#' Cliff's delta (non-parametric effect size).
#'
#' @param x Numeric vector (group 1).
#' @param y Numeric vector (group 2).
#' @return A single-row tibble: delta, magnitude.
cliff_delta <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (length(x) == 0 || length(y) == 0) {
    return(tibble(delta = NA_real_, magnitude = NA_character_))
  }

  n_x <- length(x)
  n_y <- length(y)
  # Count dominance pairs
  count <- 0
  for (xi in x) {
    count <- count + sum(xi > y) - sum(xi < y)
  }
  d <- count / (n_x * n_y)

  mag <- case_when(
    abs(d) < 0.147 ~ "negligible",
    abs(d) < 0.33  ~ "small",
    abs(d) < 0.474 ~ "medium",
    TRUE            ~ "large"
  )

  tibble(delta = d, magnitude = mag)
}


#' Pairwise Cliff's delta for all group pairs.
#'
#' @param raw_df    Per-seed results.
#' @param group_var Name of the grouping column.
#' @param metric    Name of the metric column.
#' @return A tibble: metric, group1, group2, delta, magnitude.
pairwise_cliff_delta <- function(raw_df, group_var, metric) {
  vals   <- raw_df[[metric]]
  groups <- raw_df[[group_var]]
  valid  <- is.finite(vals) & !is.na(groups)
  vals   <- vals[valid]
  groups <- factor(groups[valid])
  lvls   <- levels(groups)

  if (length(lvls) < 2) {
    return(tibble(metric = character(), group1 = character(),
                  group2 = character(), delta = numeric(),
                  magnitude = character()))
  }

  pairs <- combn(lvls, 2, simplify = FALSE)
  results <- lapply(pairs, function(pair) {
    x <- vals[groups == pair[1]]
    y <- vals[groups == pair[2]]
    cd <- cliff_delta(x, y)
    tibble(metric = metric, group1 = pair[1], group2 = pair[2],
           delta = cd$delta, magnitude = cd$magnitude)
  })

  bind_rows(results)
}


# ===========================================================================
# Full statistical summary for a single factor
# ===========================================================================

#' Run the complete statistical analysis suite for a single factor.
#'
#' @param raw_df     Per-seed results.
#' @param group_var  Primary factor column name.
#' @param metrics    Character vector of metric names to analyse.
#' @param n_boot     Bootstrap resamples.
#' @return A list with components: ci, kruskal, pairwise, effect_size.
stat_summary_single_factor <- function(raw_df, group_var, metrics,
                                       n_boot = 2000L) {
  ci_df <- bootstrap_ci_grouped(raw_df, group_var, metrics, n_boot = n_boot)

  kw_list <- lapply(metrics, function(m) kruskal_test(raw_df, group_var, m))
  kw_df   <- bind_rows(kw_list)

  pw_list <- lapply(metrics, function(m) pairwise_wilcox(raw_df, group_var, m))
  pw_df   <- bind_rows(pw_list)

  cd_list <- lapply(metrics, function(m) pairwise_cliff_delta(raw_df, group_var, m))
  cd_df   <- bind_rows(cd_list)

  list(ci = ci_df, kruskal = kw_df, pairwise = pw_df, effect_size = cd_df)
}


# ===========================================================================
# Interaction analysis (ART ANOVA)
# ===========================================================================

#' Run Aligned Rank Transform ANOVA for interaction effects.
#'
#' Requires the ARTool package. Falls back gracefully if not installed.
#'
#' @param raw_df   Per-seed results.
#' @param formula  Formula for the ART model (e.g., welfare ~ topology * load).
#' @return A tibble of ANOVA results, or NULL if ARTool is unavailable.
art_anova <- function(raw_df, formula) {
  if (!requireNamespace("ARTool", quietly = TRUE)) {
    message("ARTool package not installed; skipping ART ANOVA.")
    return(NULL)
  }

  m <- tryCatch(
    ARTool::art(formula, data = raw_df),
    error = function(e) {
      message("ART model failed: ", e$message)
      NULL
    }
  )
  if (is.null(m)) return(NULL)

  a <- tryCatch(
    anova(m),
    error = function(e) {
      message("ART anova failed: ", e$message)
      NULL
    }
  )
  if (is.null(a)) return(NULL)

  as_tibble(a, rownames = "term")
}


# ===========================================================================
# Complementarity / synergy analysis
# ===========================================================================

#' Test whether two factors have super-additive effects on a metric.
#'
#' Compares: V(a_high, b_high) - V(a_low, b_low) vs
#'           [V(a_high, b_low) - V(a_low, b_low)] +
#'           [V(a_low, b_high) - V(a_low, b_low)]
#'
#' @param raw_df    Per-seed results.
#' @param factor_a  Column name for the first factor.
#' @param factor_b  Column name for the second factor.
#' @param metric    Name of the metric column.
#' @param level_a   Named vector c(low = "...", high = "...") for factor_a.
#' @param level_b   Named vector c(low = "...", high = "...") for factor_b.
#' @param group_vars Optional character vector of additional grouping variables.
#' @return A tibble with synergy estimates per group.
compute_synergy <- function(raw_df, factor_a, factor_b, metric,
                            level_a, level_b, group_vars = NULL) {
  all_groups <- c(group_vars, "seed")

  raw_df %>%
    group_by(across(all_of(all_groups))) %>%
    summarise(
      val_ll = mean(.data[[metric]][.data[[factor_a]] == level_a["low"] &
                                      .data[[factor_b]] == level_b["low"]],
                     na.rm = TRUE),
      val_hl = mean(.data[[metric]][.data[[factor_a]] == level_a["high"] &
                                      .data[[factor_b]] == level_b["low"]],
                     na.rm = TRUE),
      val_lh = mean(.data[[metric]][.data[[factor_a]] == level_a["low"] &
                                      .data[[factor_b]] == level_b["high"]],
                     na.rm = TRUE),
      val_hh = mean(.data[[metric]][.data[[factor_a]] == level_a["high"] &
                                      .data[[factor_b]] == level_b["high"]],
                     na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      marginal_a = val_hl - val_ll,
      marginal_b = val_lh - val_ll,
      joint_gain = val_hh - val_ll,
      synergy    = joint_gain - (marginal_a + marginal_b)
    ) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      synergy_mean    = mean(synergy, na.rm = TRUE),
      synergy_lo      = bootstrap_ci(synergy)$lo,
      synergy_hi      = bootstrap_ci(synergy)$hi,
      super_additive  = synergy_mean > 0,
      .groups = "drop"
    )
}


# ===========================================================================
# Experiment-specific statistical summaries
# ===========================================================================

# ── Helper: aggregate raw (per-round) data to per-seed summaries ──────────
# Each stat_expX function first aggregates to per-seed, then runs tests.


#' Statistical summary for Experiment A (strategic operator welfare loss).
#'
#' @param raw_df Per-round results from expA_results_raw.
#' @return A list with stat summaries per load level and interaction ART.
stat_expA <- function(raw_df) {
  # Aggregate to per-seed
  baselines <- raw_df %>%
    filter(operator_type == "truthful") %>%
    group_by(dag_type, load_level, seed) %>%
    summarise(baseline_welfare = mean(welfare, na.rm = TRUE), .groups = "drop")

  per_seed <- raw_df %>%
    group_by(dag_type, load_level, operator_type, seed) %>%
    summarise(
      welfare       = mean(welfare, na.rm = TRUE),
      drop_rate     = mean(drop_rate, na.rm = TRUE),
      op_surplus    = mean(operator_surplus, na.rm = TRUE),
      net_surplus   = mean(net_op_surplus, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(baselines, by = c("dag_type", "load_level", "seed")) %>%
    mutate(
      welfare_ratio = welfare / baseline_welfare,
      welfare_loss  = 1 - welfare_ratio
    )

  metrics <- c("welfare_loss", "op_surplus", "net_surplus", "drop_rate")

  # Per load level: operator effect
  by_load <- per_seed %>%
    group_by(load_level) %>%
    group_split() %>%
    setNames(., sapply(., function(d) d$load_level[1]))

  per_load <- lapply(by_load, function(d) {
    stat_summary_single_factor(d, "operator_type", metrics)
  })

  # Interaction: operator_type x dag_type
  interaction <- art_anova(
    per_seed %>% mutate(operator_type = factor(operator_type),
                        dag_type = factor(dag_type)),
    welfare_loss ~ operator_type * dag_type
  )

  list(per_load = per_load, interaction = interaction)
}


#' Statistical summary for Experiment E (credibility trilemma).
#'
#' @param raw_df Per-round results from expE_results_raw.
#' @return A list with stat summaries and interaction ART.
stat_expE <- function(raw_df) {
  baselines <- raw_df %>%
    filter(credibility == "exchange") %>%
    group_by(dag_type, n_agents_cond, seed) %>%
    summarise(baseline_welfare = mean(welfare, na.rm = TRUE), .groups = "drop")

  per_seed <- raw_df %>%
    group_by(dag_type, n_agents_cond, credibility, seed) %>%
    summarise(
      welfare       = mean(welfare, na.rm = TRUE),
      op_surplus    = mean(operator_surplus, na.rm = TRUE),
      net_surplus   = mean(net_op_surplus, na.rm = TRUE),
      detection     = mean(penalty_applied > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(baselines, by = c("dag_type", "n_agents_cond", "seed")) %>%
    mutate(
      welfare_ratio = welfare / baseline_welfare,
      profitable    = as.numeric(net_surplus > 0)
    )

  metrics <- c("net_surplus", "welfare_ratio", "detection", "profitable")

  # Per n_agents: credibility effect
  by_n <- per_seed %>%
    group_by(n_agents_cond) %>%
    group_split() %>%
    setNames(., sapply(., function(d) d$n_agents_cond[1]))

  per_n <- lapply(by_n, function(d) {
    stat_summary_single_factor(d, "credibility", metrics)
  })

  # Interaction: credibility x n_agents
  interaction <- art_anova(
    per_seed %>% mutate(credibility = factor(credibility),
                        n_agents_cond = factor(n_agents_cond)),
    net_surplus ~ credibility * n_agents_cond
  )

  list(per_n = per_n, interaction = interaction)
}


#' Statistical summary for Experiment B (credibility mechanisms comparison).
#'
#' @param raw_df Per-round results from expB_results_raw.
#' @return A list with stat summaries and interaction ART.
stat_expB <- function(raw_df) {
  # Baseline: exchange credibility (truthful execution)
  baselines <- raw_df %>%
    filter(credibility == "exchange") %>%
    group_by(dag_type, load_level, operator_type, seed) %>%
    summarise(baseline_welfare = mean(welfare, na.rm = TRUE), .groups = "drop")

  per_seed <- raw_df %>%
    group_by(dag_type, load_level, operator_type, credibility, seed) %>%
    summarise(
      welfare       = mean(welfare, na.rm = TRUE),
      net_surplus   = mean(net_op_surplus, na.rm = TRUE),
      detection     = mean(penalty_applied > 0, na.rm = TRUE),
      latency_oh    = mean(latency_overhead, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(baselines,
              by = c("dag_type", "load_level", "operator_type", "seed")) %>%
    mutate(welfare_recovery = welfare / baseline_welfare)

  metrics <- c("welfare_recovery", "detection", "net_surplus", "latency_oh")

  # Per operator type (medium load): credibility effect
  by_op <- per_seed %>%
    filter(load_level == 1.0) %>%
    group_by(operator_type) %>%
    group_split() %>%
    setNames(., sapply(., function(d) d$operator_type[1]))

  per_op <- lapply(by_op, function(d) {
    stat_summary_single_factor(d, "credibility", metrics)
  })

  # Interaction: credibility x operator_type
  interaction <- art_anova(
    per_seed %>% mutate(credibility = factor(credibility),
                        operator_type = factor(operator_type),
                        dag_type = factor(dag_type)),
    welfare_recovery ~ credibility * operator_type * dag_type
  )

  list(per_operator = per_op, interaction = interaction)
}


#' Statistical summary for Experiment F (domain separation).
#'
#' @param raw_df Per-round results from expF_results_raw.
#' @return A list with stat summaries and interaction ART.
stat_expF <- function(raw_df) {
  # Baseline: separated fee mode (truthful by construction)
  baselines <- raw_df %>%
    filter(fee_mode == "separated") %>%
    group_by(dag_type, load_level, operator_type, seed) %>%
    summarise(baseline_welfare = mean(welfare, na.rm = TRUE), .groups = "drop")

  per_seed <- raw_df %>%
    group_by(dag_type, load_level, operator_type, fee_mode, seed) %>%
    summarise(
      welfare       = mean(welfare, na.rm = TRUE),
      net_surplus   = mean(net_op_surplus, na.rm = TRUE),
      op_surplus    = mean(operator_surplus, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(baselines,
              by = c("dag_type", "load_level", "operator_type", "seed")) %>%
    mutate(welfare_ratio = welfare / baseline_welfare)

  metrics <- c("welfare_ratio", "net_surplus", "op_surplus")

  # Main effect: fee_mode
  main <- stat_summary_single_factor(per_seed, "fee_mode", metrics)

  # Interaction: fee_mode x operator_type
  interaction <- art_anova(
    per_seed %>% mutate(fee_mode = factor(fee_mode),
                        operator_type = factor(operator_type),
                        dag_type = factor(dag_type)),
    net_surplus ~ fee_mode * operator_type * dag_type
  )

  list(main = main, interaction = interaction)
}


#' Statistical summary for Experiment C (integrator competition).
#'
#' @param raw_df Per-round results from expC_results_raw.
#' @return A list with stat summaries and interaction ART.
stat_expC <- function(raw_df) {
  # Baseline: k=5, competitive
  baselines <- raw_df %>%
    filter(k_integrators == 5, integrator_strategy == "competitive") %>%
    group_by(dag_type, load_level, seed) %>%
    summarise(
      bench_welfare = mean(welfare, na.rm = TRUE),
      bench_price   = mean(slice_price_adj, na.rm = TRUE),
      .groups = "drop"
    )

  per_seed <- raw_df %>%
    group_by(dag_type, load_level, k_integrators, integrator_strategy, seed) %>%
    summarise(
      welfare       = mean(welfare, na.rm = TRUE),
      slice_adj     = mean(slice_price_adj, na.rm = TRUE),
      agent_pay     = mean(mean_agent_payment, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(baselines, by = c("dag_type", "load_level", "seed")) %>%
    mutate(
      welfare_ratio = welfare / bench_welfare,
      price_markup  = slice_adj / pmax(bench_price, 0.01)
    )

  metrics <- c("welfare_ratio", "price_markup", "agent_pay")

  # Per strategy: k effect (medium load)
  by_strat <- per_seed %>%
    filter(load_level == 1.0) %>%
    group_by(integrator_strategy) %>%
    group_split() %>%
    setNames(., sapply(., function(d) d$integrator_strategy[1]))

  per_strategy <- lapply(by_strat, function(d) {
    # k_integrators as factor for group comparisons
    d$k_factor <- factor(d$k_integrators)
    stat_summary_single_factor(d, "k_factor", metrics)
  })

  # Spearman correlation: k vs price_markup (competitive only)
  comp_data <- per_seed %>%
    filter(integrator_strategy == "competitive", load_level == 1.0)

  spearman <- if (nrow(comp_data) >= 3) {
    ct <- cor.test(comp_data$k_integrators, comp_data$price_markup,
                   method = "spearman", exact = FALSE)
    tibble(rho = ct$estimate, p_value = ct$p.value)
  } else {
    tibble(rho = NA_real_, p_value = NA_real_)
  }

  # Interaction: k x strategy
  interaction <- art_anova(
    per_seed %>%
      filter(k_integrators >= 2) %>%
      mutate(k_factor = factor(k_integrators),
             integrator_strategy = factor(integrator_strategy)),
    price_markup ~ k_factor * integrator_strategy
  )

  list(per_strategy = per_strategy, spearman = spearman,
       interaction = interaction)
}


#' Statistical summary for Experiment D (two-tier trust).
#'
#' @param raw_df Per-round results from expD_results_raw.
#' @return A list with stat summaries and interaction ART.
stat_expD <- function(raw_df) {
  # Baseline: both tiers = exchange
  baselines <- raw_df %>%
    filter(l1_trust == "exchange", l2_trust == "exchange") %>%
    group_by(dag_type, n_agents_cond, seed) %>%
    summarise(baseline_welfare = mean(welfare, na.rm = TRUE), .groups = "drop")

  per_seed <- raw_df %>%
    group_by(dag_type, n_agents_cond, l1_trust, l2_trust, seed) %>%
    summarise(
      welfare       = mean(welfare, na.rm = TRUE),
      compliance    = 1 - mean(net_op_surplus > 0, na.rm = TRUE),
      latency_oh    = mean(latency_overhead, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(baselines, by = c("dag_type", "n_agents_cond", "seed")) %>%
    mutate(welfare_ratio = welfare / baseline_welfare)

  metrics <- c("welfare_ratio", "compliance", "latency_oh")

  # Main effects: L1 and L2 separately
  l1_stats <- stat_summary_single_factor(per_seed, "l1_trust", metrics)
  l2_stats <- stat_summary_single_factor(per_seed, "l2_trust", metrics)

  # Interaction: L1 x L2
  interaction <- art_anova(
    per_seed %>% mutate(l1_trust = factor(l1_trust),
                        l2_trust = factor(l2_trust)),
    welfare_ratio ~ l1_trust * l2_trust
  )

  list(l1 = l1_stats, l2 = l2_stats, interaction = interaction)
}


#' Statistical summary for Experiment H (adaptive operator).
#'
#' @param raw_df Per-round results from expH_results_raw.
#' @return A list with stat summaries and interaction ART.
stat_expH <- function(raw_df) {
  # Baseline: exchange credibility
  baselines <- raw_df %>%
    filter(credibility == "exchange") %>%
    group_by(dag_type, n_agents_cond, seed) %>%
    summarise(baseline_welfare = mean(welfare, na.rm = TRUE), .groups = "drop")

  # Tail = last 20 rounds
  max_round <- max(raw_df$round, na.rm = TRUE)
  tail_start <- max_round - 19

  per_seed <- raw_df %>%
    group_by(dag_type, n_agents_cond, credibility, seed) %>%
    summarise(
      cum_surplus    = sum(net_op_surplus, na.rm = TRUE),
      welfare_all    = mean(welfare, na.rm = TRUE),
      welfare_tail   = mean(welfare[round >= tail_start], na.rm = TRUE),
      dev_tail       = mean(deviation_magnitude[round >= tail_start], na.rm = TRUE),
      surplus_tail   = mean(net_op_surplus[round >= tail_start], na.rm = TRUE),
      pct_truthful   = mean(deviation_magnitude[round >= tail_start] == 0,
                            na.rm = TRUE),
      detection      = mean(penalty_applied > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(baselines, by = c("dag_type", "n_agents_cond", "seed")) %>%
    mutate(welfare_ratio = welfare_tail / baseline_welfare)

  metrics <- c("cum_surplus", "dev_tail", "pct_truthful",
               "welfare_ratio", "detection")

  # Main effect: credibility
  main <- stat_summary_single_factor(per_seed, "credibility", metrics)

  # Interaction: credibility x dag_type
  interaction <- art_anova(
    per_seed %>% mutate(credibility = factor(credibility),
                        dag_type = factor(dag_type)),
    cum_surplus ~ credibility * dag_type
  )

  list(main = main, interaction = interaction)
}


#' Statistical summary for Experiment I (imperfect broadcast).
#'
#' @param raw_df Per-round results from expI_results_raw.
#' @return A list with Spearman correlations and per-p statistics.
stat_expI <- function(raw_df) {
  # Baseline: p_broadcast = 1.0
  baselines <- raw_df %>%
    filter(p_broadcast == 1.0) %>%
    group_by(dag_type, seed) %>%
    summarise(baseline_welfare = mean(welfare, na.rm = TRUE), .groups = "drop")

  per_seed <- raw_df %>%
    group_by(dag_type, p_broadcast, seed) %>%
    summarise(
      welfare       = mean(welfare, na.rm = TRUE),
      net_surplus   = mean(net_op_surplus, na.rm = TRUE),
      detection     = mean(penalty_applied > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(baselines, by = c("dag_type", "seed")) %>%
    mutate(
      welfare_ratio = welfare / baseline_welfare,
      profitable    = as.numeric(net_surplus > 0)
    )

  metrics <- c("welfare_ratio", "net_surplus", "detection")

  # Spearman correlation: p_broadcast vs net_surplus (per topology)
  by_topo <- per_seed %>%
    group_by(dag_type) %>%
    group_split() %>%
    setNames(., sapply(., function(d) d$dag_type[1]))

  correlations <- lapply(by_topo, function(d) {
    lapply(metrics, function(m) {
      ct <- cor.test(d$p_broadcast, d[[m]], method = "spearman", exact = FALSE)
      tibble(metric = m, rho = ct$estimate, p_value = ct$p.value)
    }) %>% bind_rows()
  })

  # KW across p_broadcast levels (treated as factor)
  per_seed$p_factor <- factor(per_seed$p_broadcast)
  kw <- stat_summary_single_factor(per_seed, "p_factor", metrics)

  list(correlations = correlations, kw = kw)
}


#' Statistical summary for Experiment J (credibility x competition interaction).
#'
#' @param raw_df Per-round results from expJ_results_raw.
#' @return A list with stat summaries, interaction ART, and synergy.
stat_expJ <- function(raw_df) {
  # Baseline: broadcast + k=5
  baselines <- raw_df %>%
    filter(credibility == "broadcast", k_integrators == 5) %>%
    group_by(dag_type, seed) %>%
    summarise(baseline_welfare = mean(welfare, na.rm = TRUE), .groups = "drop")

  per_seed <- raw_df %>%
    group_by(dag_type, credibility, k_integrators, seed) %>%
    summarise(
      welfare       = mean(welfare, na.rm = TRUE),
      net_surplus   = mean(net_op_surplus, na.rm = TRUE),
      op_surplus    = mean(operator_surplus, na.rm = TRUE),
      slice_adj     = mean(slice_price_adj, na.rm = TRUE),
      detection     = mean(penalty_applied > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(baselines, by = c("dag_type", "seed")) %>%
    mutate(
      welfare_ratio = welfare / baseline_welfare,
      profitable    = as.numeric(net_surplus > 0)
    )

  metrics <- c("welfare_ratio", "net_surplus", "op_surplus",
               "slice_adj", "detection", "profitable")

  # Main effects
  cred_stats <- stat_summary_single_factor(per_seed, "credibility", metrics)
  k_stats <- stat_summary_single_factor(
    per_seed %>% mutate(k_factor = factor(k_integrators)),
    "k_factor", metrics
  )

  # Two-way interaction: credibility x k_integrators (ART ANOVA)
  interaction <- art_anova(
    per_seed %>% mutate(credibility = factor(credibility),
                        k_factor = factor(k_integrators)),
    net_surplus ~ credibility * k_factor
  )

  # Synergy: credibility (none → broadcast) x competition (k=1 → k=5)
  # on net_surplus (sign-reversed for surplus *reduction*)
  synergy_df <- per_seed %>% mutate(neg_surplus = -net_surplus)
  synergy <- compute_synergy(
    synergy_df,
    factor_a   = "credibility",
    factor_b   = "k_integrators",
    metric     = "neg_surplus",
    level_a    = c(low = "none", high = "broadcast"),
    level_b    = c(low = "1", high = "5"),
    group_vars = "dag_type"
  )

  list(credibility = cred_stats, competition = k_stats,
       interaction = interaction, synergy = synergy)
}


# ===========================================================================
# LaTeX table generation
# ===========================================================================

#' Format a statistical comparison table as LaTeX (for supplementary).
#'
#' @param ci_df       Bootstrap CI tibble from bootstrap_ci_grouped().
#' @param pw_df       Pairwise Wilcoxon tibble.
#' @param cd_df       Cliff's delta tibble.
#' @param metric      Which metric to format.
#' @param group_var   Grouping variable name.
#' @param caption     Table caption.
#' @param label       LaTeX label.
#' @return A character string of LaTeX code.
format_stat_table_latex <- function(ci_df, pw_df, cd_df, metric,
                                    group_var, caption = "", label = "") {
  # CI portion
  ci_cols <- c(group_var,
               paste0(metric, "_mean"),
               paste0(metric, "_lo"),
               paste0(metric, "_hi"))
  ci_sub <- ci_df[, intersect(ci_cols, names(ci_df)), drop = FALSE]

  # Pairwise portion
  pw_sub <- pw_df %>% filter(.data$metric == !!metric) %>%
    select(group1, group2, W, p_adj)

  # Effect size portion
  cd_sub <- cd_df %>% filter(.data$metric == !!metric) %>%
    select(group1, group2, delta, magnitude)

  # Combine pairwise + effect size
  pw_cd <- pw_sub %>%
    left_join(cd_sub, by = c("group1", "group2"))

  # Build LaTeX
  header <- paste0(
    "\\begin{table}[ht]\n",
    "\\centering\n",
    "\\caption{", caption, "}\n",
    "\\label{", label, "}\n"
  )

  # CI table
  ci_header <- "\\begin{tabular}{lrrr}\n\\toprule\n"
  ci_header <- paste0(ci_header, group_var,
                      " & Mean & 95\\% CI Lo & 95\\% CI Hi \\\\\n\\midrule\n")
  ci_body <- paste0(
    apply(ci_sub, 1, function(row) {
      paste0(row[1], " & ",
             sprintf("%.3f", as.numeric(row[2])), " & ",
             sprintf("%.3f", as.numeric(row[3])), " & ",
             sprintf("%.3f", as.numeric(row[4])), " \\\\")
    }),
    collapse = "\n"
  )
  ci_footer <- "\n\\bottomrule\n\\end{tabular}\n\n"

  # Pairwise table
  pw_header <- paste0(
    "\\vspace{1em}\n\\begin{tabular}{llrrrr}\n\\toprule\n",
    "Group 1 & Group 2 & $W$ & $p_{\\mathrm{adj}}$ & ",
    "$\\delta$ & Magnitude \\\\\n\\midrule\n"
  )
  pw_body <- if (nrow(pw_cd) > 0) {
    paste0(
      apply(pw_cd, 1, function(row) {
        paste0(row[1], " & ", row[2], " & ",
               sprintf("%.1f", as.numeric(row[3])), " & ",
               sprintf("%.4f", as.numeric(row[4])), " & ",
               sprintf("%.3f", as.numeric(row[5])), " & ",
               row[6], " \\\\")
      }),
      collapse = "\n"
    )
  } else {
    "\\multicolumn{6}{c}{(fewer than 2 groups)} \\\\"
  }
  pw_footer <- "\n\\bottomrule\n\\end{tabular}\n"

  footer <- "\\end{table}\n"

  paste0(header, ci_header, ci_body, ci_footer,
         pw_header, pw_body, pw_footer, footer)
}
