# Exp 9 forward-calibration: implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: superpowers:test-driven-development; superpowers:systematic-debugging; superpowers:verification-before-completion. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Re-instrument Experiment 9 (`sim_expL.R` — domain-separation knife-edge) to log $\sigma_p$, $\delta$, $\varepsilon^*$ per round and produce a forward-calibration figure that predicts the SDS threshold $\tau^*$ ex-ante from the data, without the amplification-factor reconstruction currently flagged at `SmallestDetectableStake.tex:147`.

**Architecture:** R + `targets` pipeline with per-round logging extensions to `run_simulation` and a new aggregation/plot for the SDS forward calibration. Under TDD: each instrumentation hook gets a failing test first.

**Tech Stack:** R ≥ 4.2, targets, dplyr, tidyr, purrr, tibble, ggplot2, scales, patchwork, boot, testthat (NEW dependency).

---

## Context

### Why this work

The TEAC peer review (10-reviewer panel, Round 1) returned 5 reviewers in consensus on a "RF2 / Issue 1" red flag: the §III.H Smallest Detectable Stake (SDS) theorem's calibration to Experiment 9 is **reverse-engineered**, not predicted. The 50–800× gap between nominal stake (1%) and reconstructed effective stake [4, 8] is closed by an "amplification factor" $\delta/\varepsilon^*_{\mathrm{nominal}} \approx 1.6 \times 10^2$ that is itself derived from the empirical $\tau$-zero-crossing window. The chain is circular: simulator's [10, 20] τ-zero-crossing → effective stake → "predicted" by theorem.

The current §III.H.5 "Calibration assumptions" paragraph (`SmallestDetectableStake.tex:149`) admits the assumption, but the AUTHOR-REQUIRED LaTeX comment at line 147 commits us to closing the loop:

> Re-instrument Exp 9 simulator to log $\sigma_p$, $\delta$, $\varepsilon^*$ per round; re-run with these logs; produce a forward-calibration figure reporting $\tau^*$ as ex-ante prediction without the amplification reconstruction.

This is the work this plan covers. It is **not blocking** the TEAC revision (the post-hoc consistency check is in-scope) but is **required for camera-ready** if the revision is accepted.

### What needs to be measured

The SDS theorem (`thm:sds`) gives:
$$\lambda^*_{\mathrm{audit}}(\tau, \beta) = \frac{\beta \bar v n}{\tau}$$

Where:
- $\lambda$ = operator's ownership stake fraction (Exp 9's `stake_fraction`, swept over $\{0, 0.01, 0.05, 0.1, 0.25, 0.5, 1.0\}$)
- $\tau$ = audit frequency (NOT currently in Exp L; needs adding — the existing audit machinery is in `enforce_credibility` for `regulatory` mechanism)
- $\beta$ = detection sensitivity, derived as $W/\sigma_p$ where $W$ = audit window length and $\sigma_p$ = Laplace scale of the auditor's discrepancy estimator
- $\bar v$ = bid scale upper bound
- $n$ = number of agents (40 in current Exp 9 config)

Currently, Exp 9 only sweeps `stake_fraction × operator × topology` with no audit channel and no per-round amplitude logging. The forward-calibration extension needs:

1. Per-round $\sigma_p$ log: standard deviation of payment-vector noise. Currently inferred, not measured.
2. Per-round $\delta$ log: amplitude of the operator's deviation (ghost bidder's perturbation magnitude). Currently inferred from configuration file, not logged.
3. Per-round $\varepsilon^*$ log: the operator's optimal deviation amplitude under the SDS first-order condition $\varepsilon^* = (1/\beta) \log(\bar v n \beta / (\tau \lambda))$. This is what the operator *would* choose; we measure what they *did* choose.
4. New experiment dimension: $\tau$ (audit frequency). Sweep $\{1, 5, 10, 20, 40, 80\}$.
5. Detection sensitivity $\beta$: derive from the simulator's broadcast noise model.

### Scope

**In scope (this plan):**
- New per-round logging fields in `run_simulation` and `enforce_credibility`
- New `expL2_*` family (Exp 9 forward calibration) parallel to `expL` — keep the original Exp 9 figure intact for reproducibility; add `expL2` for the forward calibration
- New plot: `expL2_forward_calibration.pdf` showing predicted $\tau^*$ vs empirical $\tau$-zero-crossing
- New test suite for the per-round logging
- Documentation update in `README.md` and `SmallestDetectableStake.tex` (replace post-hoc bracket with ex-ante prediction; remove AUTHOR-REQUIRED flag)

**Out of scope:**
- Real-marketplace calibration (FCC / AWS / O-RAN) — that's the systems follow-up paper's job
- Refactoring `sim_credibility.R` beyond what's needed for new logging
- Changing the existing `expL` (Exp 9 baseline) — keep intact
- Bayesian persuasion / scoring-rule extensions — already covered in §III.H

### Files to be created or modified

- **Create**: `R/sim_expL2.R` — forward-calibration variant of Exp 9
- **Create**: `R/plots_expL2.R` — forward-calibration plot
- **Create**: `tests/test-sim-credibility-logging.R` — TDD test suite for new logging
- **Create**: `tests/test-expL2-forward-calibration.R` — TDD test suite for the new experiment
- **Modify**: `R/sim_credibility.R` — add per-round logging fields ($\sigma_p$, $\delta$, $\varepsilon^*$)
- **Modify**: `_targets.R` — register `expL2_design`, `expL2_results`, `expL2_summary`, `expL2_plot` targets
- **Modify**: `README.md` — add Exp L2 to mapping table
- **Modify**: `Manuscripts/2026-Trilogy-2-TEAC.../sections/SmallestDetectableStake.tex` — replace lines 147–149 calibration paragraph with ex-ante-prediction language; remove AUTHOR-REQUIRED comment
- **Modify**: `Manuscripts/2026-Trilogy-2-TEAC.../sections/Evaluation.tex` — extend §V.D (Empirical CoNC) with one paragraph on Exp 9 forward calibration

---

## Pre-flight: superpowers verification

Before any task: verify the dev environment.

- [ ] **Check R installation and required packages**

Run:
```bash
cd "/Users/lloven/Dropbox/Research/workspace/5 Support/Groups/FCG/Experiments/credible-marketplace-sim/"
Rscript -e 'cat("R version:", R.version.string, "\n"); for (p in c("targets","dplyr","tidyr","purrr","tibble","ggplot2","scales","patchwork","boot","testthat")) cat(sprintf("%-12s : %s\n", p, ifelse(requireNamespace(p, quietly=TRUE), "OK", "MISSING")))'
```

Expected: all packages OK or note which are MISSING. Install missing ones with `install.packages(c(...))`.

- [ ] **Confirm baseline Exp 9 runs cleanly**

Run:
```bash
Rscript -e 'library(targets); tar_make(expL_summary)'
```

Expected: target rebuilds successfully; output `_targets/objects/expL_summary` exists. If FAIL → return to Phase 1 of `systematic-debugging` skill.

- [ ] **Confirm baseline plot regenerates**

Run:
```bash
Rscript -e 'library(targets); tar_make(plots_expL)' 2>&1 | tail -10
ls -la fig/expL_combined.pdf
```

Expected: PDF exists, mtime fresh.

- [ ] **Initialise testthat**

Run:
```bash
test ! -d tests/testthat && mkdir -p tests/testthat
ls tests/
```

If `tests/` doesn't exist or has no test files yet, this is expected — we're TDD'ing the logging. Proceed to Task 1.

---

## Task 1: Per-round σ_p logging in `enforce_credibility`

**Files:**
- Modify: `R/sim_credibility.R` — extend `enforce_credibility` to compute and return per-round payment-vector standard deviation
- Create: `tests/test-sim-credibility-logging.R` — TDD test for σ_p output field

- [ ] **Step 1: Write the failing test**

Create `tests/test-sim-credibility-logging.R`:
```r
library(testthat)
source(here::here("R", "sim_credibility.R"))

test_that("enforce_credibility returns sigma_p per round", {
  mech <- make_credibility_mechanism(type = "exchange", stake_fraction = 0.01)
  outcome <- list(
    operator_surplus = 0.78,
    payments         = c(0.5, 0.6, 0.55, 0.7, 0.65),  # 5-agent payment vector
    deviation_amplitude = 0.5
  )
  result <- enforce_credibility(mech, outcome, round = 1, broadcast_history = NULL)

  expect_true("sigma_p" %in% names(result),
              info = "enforce_credibility must return sigma_p (payment-vector std)")
  expect_type(result$sigma_p, "double")
  expect_equal(result$sigma_p, sd(outcome$payments), tolerance = 1e-6,
               info = "sigma_p must equal sd(payments) for the round")
})
```

- [ ] **Step 2: Run test to verify it fails**

```bash
Rscript -e 'testthat::test_file("tests/test-sim-credibility-logging.R")'
```

Expected: FAIL with `"sigma_p" %in% names(result)` returning FALSE.

- [ ] **Step 3: Add sigma_p computation in enforce_credibility**

Edit `R/sim_credibility.R`. In `enforce_credibility`, add `sigma_p` to the returned list:
```r
# (inside enforce_credibility, just before returning)
result$sigma_p <- if (length(operator_outcome$payments) > 1) {
  sd(operator_outcome$payments)
} else {
  NA_real_
}
```

- [ ] **Step 4: Run test to verify it passes**

```bash
Rscript -e 'testthat::test_file("tests/test-sim-credibility-logging.R")'
```

Expected: PASS.

- [ ] **Step 5: Verify other tests still pass**

```bash
Rscript -e 'library(targets); tar_make(expL_summary)'
```

Expected: target rebuilds clean (sigma_p added but doesn't break aggregation).

- [ ] **Step 6: Commit**

```bash
cd "/Users/lloven/Dropbox/Research/workspace/2 Papers and Manuscripts/2026 Trustworthy Marketplace Architecture for Agents/src/simulation/"
git add R/sim_credibility.R tests/test-sim-credibility-logging.R
git commit -m "feat(exp9-fwd): log per-round payment std (sigma_p) in enforce_credibility"
```

---

## Task 2: Per-round δ logging (deviation amplitude)

**Files:**
- Modify: `R/sim_credibility.R` — log the actual perturbation amplitude the operator chose
- Modify: `tests/test-sim-credibility-logging.R` — TDD test

- [ ] **Step 1: Write the failing test**

Append to `tests/test-sim-credibility-logging.R`:
```r
test_that("enforce_credibility returns delta_amplitude per round", {
  mech <- make_credibility_mechanism(type = "exchange", stake_fraction = 0.01)
  outcome <- list(
    operator_surplus = 0.78,
    payments         = c(0.5, 0.6, 0.55, 0.7, 0.65),
    deviation_amplitude = 0.5  # the ghost-bid magnitude
  )
  result <- enforce_credibility(mech, outcome, round = 1, broadcast_history = NULL)

  expect_true("delta_amplitude" %in% names(result),
              info = "enforce_credibility must propagate the operator's deviation amplitude")
  expect_equal(result$delta_amplitude, 0.5, tolerance = 1e-6)
})

test_that("delta_amplitude is NA for truthful operator", {
  mech <- make_credibility_mechanism(type = "none", stake_fraction = 0.0)
  outcome <- list(
    operator_surplus = 0,
    payments         = c(0.5, 0.6, 0.55, 0.7, 0.65),
    deviation_amplitude = NA_real_
  )
  result <- enforce_credibility(mech, outcome, round = 1, broadcast_history = NULL)
  expect_true(is.na(result$delta_amplitude))
})
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
Rscript -e 'testthat::test_file("tests/test-sim-credibility-logging.R")'
```

Expected: 2 NEW failures (delta_amplitude not in result).

- [ ] **Step 3: Add delta_amplitude propagation**

Edit `R/sim_credibility.R`. In `enforce_credibility`, propagate `operator_outcome$deviation_amplitude`:
```r
result$delta_amplitude <- operator_outcome$deviation_amplitude %||% NA_real_
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
Rscript -e 'testthat::test_file("tests/test-sim-credibility-logging.R")'
```

Expected: 3/3 PASS.

- [ ] **Step 5: Verify operator code populates `deviation_amplitude` field**

```bash
grep -nE "deviation_amplitude|ghost.bid|perturbation" R/operators.R 2>/dev/null || \
grep -rnE "deviation_amplitude" R/*.R | head -10
```

Expected: at least one operator (the ghost_bidder) sets this field. If not, **STOP** and add it (one extra step before continuing). The ghost-bidder's perturbation magnitude is currently in its config but not propagated as a per-round output.

- [ ] **Step 6: Add deviation_amplitude to ghost_bidder operator (if not present)**

If grep above showed no `deviation_amplitude` set anywhere in operators, add to the ghost-bidder function:
```r
# Inside the ghost_bidder operator function
return(list(
  operator_surplus    = ...,
  payments            = ...,
  deviation_amplitude = perturbation_magnitude
))
```
with appropriate test (TDD; one round-trip test reading `deviation_amplitude` from a ghost-bidder simulation).

- [ ] **Step 7: Commit**

```bash
git add R/sim_credibility.R R/operators.R tests/test-sim-credibility-logging.R
git commit -m "feat(exp9-fwd): log per-round deviation amplitude (delta) for ghost-bidder operator"
```

---

## Task 3: Per-round ε* logging (predicted optimal deviation amplitude)

**Files:**
- Modify: `R/sim_credibility.R` — compute and log the SDS theorem's $\varepsilon^*$
- Modify: `tests/test-sim-credibility-logging.R` — TDD test

- [ ] **Step 1: Write the failing test**

Append:
```r
test_that("enforce_credibility computes epsilon_star from SDS theorem", {
  # epsilon_star = (1/beta) * log(v_bar * n * beta / (tau * lambda))
  # For lambda=0.01, n=40, v_bar=1, beta=2, tau=20: log(40*2/(20*0.01)) / 2 = log(400)/2 ≈ 2.995
  mech <- make_credibility_mechanism(type = "regulatory",
                                      stake_fraction = 0.01,
                                      tau_audit = 20, beta_audit = 2)
  outcome <- list(operator_surplus = 0.78,
                  payments = c(0.5, 0.6, 0.55, 0.7, 0.65),
                  deviation_amplitude = 0.5,
                  v_bar = 1.0,
                  n_agents = 40)
  result <- enforce_credibility(mech, outcome, round = 1, broadcast_history = NULL)

  expect_true("epsilon_star" %in% names(result))
  expected_eps_star <- log(1.0 * 40 * 2 / (20 * 0.01)) / 2
  expect_equal(result$epsilon_star, expected_eps_star, tolerance = 1e-6)
})

test_that("epsilon_star is NA when tau_audit is unset (no audit channel)", {
  mech <- make_credibility_mechanism(type = "exchange", stake_fraction = 0.01)
  outcome <- list(payments = c(0.5, 0.6), deviation_amplitude = 0.1, v_bar = 1.0, n_agents = 40)
  result <- enforce_credibility(mech, outcome, round = 1, broadcast_history = NULL)
  expect_true(is.na(result$epsilon_star))
})
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
Rscript -e 'testthat::test_file("tests/test-sim-credibility-logging.R")'
```

Expected: 2 NEW failures.

- [ ] **Step 3: Add SDS ε* computation in enforce_credibility**

```r
# Inside enforce_credibility, after sigma_p / delta blocks
if (!is.null(mech$tau_audit) && !is.null(mech$beta_audit) &&
    !is.null(operator_outcome$v_bar) && !is.null(operator_outcome$n_agents) &&
    mechanism$stake_fraction > 0) {
  beta  <- mech$beta_audit
  tau   <- mech$tau_audit
  v_bar <- operator_outcome$v_bar
  n     <- operator_outcome$n_agents
  lambda <- mechanism$stake_fraction
  result$epsilon_star <- (1 / beta) * log(v_bar * n * beta / (tau * lambda))
} else {
  result$epsilon_star <- NA_real_
}
```

- [ ] **Step 4: Add `tau_audit` and `beta_audit` to `make_credibility_mechanism`**

Edit `make_credibility_mechanism` signature to accept these two new parameters with sensible defaults (`tau_audit = NULL, beta_audit = NULL`). Update the function body to store them in the mechanism object.

- [ ] **Step 5: Run tests to verify they pass**

```bash
Rscript -e 'testthat::test_file("tests/test-sim-credibility-logging.R")'
```

Expected: 5/5 PASS (3 from Tasks 1+2, 2 new from Task 3).

- [ ] **Step 6: Commit**

```bash
git add R/sim_credibility.R tests/test-sim-credibility-logging.R
git commit -m "feat(exp9-fwd): compute SDS epsilon_star per round (audit channel parameters)"
```

---

## Task 4: Forward-calibration experiment design (`sim_expL2.R`)

**Files:**
- Create: `R/sim_expL2.R` — Exp 9 forward-calibration variant
- Create: `tests/test-expL2-forward-calibration.R` — TDD test suite

- [ ] **Step 1: Write the failing test**

Create `tests/test-expL2-forward-calibration.R`:
```r
library(testthat)
source(here::here("R", "sim_expL2.R"))

test_that("expL2_design generates the right factorial", {
  d <- expL2_design()
  expect_true(all(c("stake_fraction", "tau_audit", "operator", "dag_type") %in% names(d)))
  expect_true("condition_id" %in% names(d))
  # Default: 5 stakes × 6 tau × 1 operator × 3 topologies = 90 conditions
  expect_equal(nrow(d), 5 * 6 * 1 * 3)
})

test_that("expL2_run_single returns per-round logs of sigma_p, delta, epsilon_star", {
  cond <- list(stake_fraction = 0.01, tau_audit = 20, operator = "ghost_bidder",
               dag_type = "tree")
  result <- expL2_run_single(cond, n_rounds = 5, seed = 1)
  expect_true(all(c("sigma_p", "delta_amplitude", "epsilon_star") %in% names(result)))
  expect_equal(nrow(result), 5)
})

test_that("expL2_aggregate computes empirical and predicted tau_star per condition", {
  d <- expL2_design()[1:6, ]  # one stake, all tau, one operator, one dag
  results <- expL2_run_all(d, n_rounds = 20, n_seeds = 2)
  agg <- expL2_aggregate(results)
  expect_true("predicted_tau_star" %in% names(agg))
  expect_true("empirical_zero_crossing_tau" %in% names(agg))
})
```

- [ ] **Step 2: Run test to verify it fails**

```bash
Rscript -e 'testthat::test_file("tests/test-expL2-forward-calibration.R")'
```

Expected: file not found (sim_expL2.R doesn't exist yet).

- [ ] **Step 3: Implement `sim_expL2.R` design + run + aggregate**

Create `R/sim_expL2.R` modelled on `sim_expL.R` but with:
- Design adds `tau_audit` dimension (`{1, 5, 10, 20, 40, 80}`) and restricts operator to `ghost_bidder` only
- `expL2_run_single` calls `run_simulation` with `credibility_type = "regulatory"`, `credibility_params = list(stake_fraction = ..., tau_audit = ..., beta_audit = 2)`
- `expL2_aggregate` computes per-condition `predicted_tau_star = beta * v_bar * n / lambda` and `empirical_zero_crossing_tau` (smallest tau at which mean operator surplus crosses zero from positive)

- [ ] **Step 4: Run tests to verify they pass**

```bash
Rscript -e 'testthat::test_file("tests/test-expL2-forward-calibration.R")'
```

Expected: 3/3 PASS.

- [ ] **Step 5: Commit**

```bash
git add R/sim_expL2.R tests/test-expL2-forward-calibration.R
git commit -m "feat(exp9-fwd): expL2 forward-calibration experiment design + aggregation"
```

---

## Task 5: Register `expL2` in `_targets.R`

**Files:**
- Modify: `_targets.R` — add expL2 targets

- [ ] **Step 1: Inspect existing target structure for expL**

```bash
grep -nE "expL_|tar_target.*expL" _targets.R
```

Note the pattern: `expL_design`, `expL_results`, `expL_summary`, `expL_plot`.

- [ ] **Step 2: Add parallel `expL2_*` targets**

Edit `_targets.R`. After the `expL_*` block, add:
```r
tar_target(expL2_design, expL2_design()),
tar_target(expL2_results, expL2_run_all(expL2_design, n_rounds = 100, n_seeds = 5),
           pattern = map(expL2_design)),  # OPTIONAL — if existing exp uses targets-pattern
tar_target(expL2_summary, expL2_aggregate(expL2_results))
```

(If `_targets.R` does NOT use `pattern = map(...)` for expL, mirror its style exactly for expL2.)

- [ ] **Step 3: Source the new sim file in the targets globals**

```bash
grep -nE "source.*expL\.R|source.*sim_expL" _targets.R
```

Make sure `R/sim_expL2.R` is sourced alongside `R/sim_expL.R`.

- [ ] **Step 4: Verify targets graph builds**

```bash
Rscript -e 'library(targets); tar_visnetwork()'  # optional: opens browser
Rscript -e 'library(targets); tar_outdated()'
```

Expected: `expL2_design`, `expL2_results`, `expL2_summary` listed as outdated. No errors.

- [ ] **Step 5: Smoke-build smallest target**

```bash
Rscript -e 'library(targets); tar_make(expL2_design)'
ls -la _targets/objects/expL2_design
```

Expected: target builds successfully.

- [ ] **Step 6: Commit**

```bash
git add _targets.R
git commit -m "feat(exp9-fwd): register expL2 targets in pipeline"
```

---

## Task 6: Run full Exp L2 with TDD smoke test

**Files:** none (run-only)

- [ ] **Step 1: Smoke-test with reduced sample size**

```bash
Rscript -e 'library(targets); tar_make(expL2_summary)' 2>&1 | tail -20
```

Expected: builds in 5-30 minutes (90 conditions × 5 seeds × 100 rounds). If runtime > 1h, reduce to `n_rounds = 50, n_seeds = 3` for the smoke run, and document the reduction.

- [ ] **Step 2: Inspect summary output**

```bash
Rscript -e 'library(targets); s <- tar_read(expL2_summary); print(head(s)); cat("rows:", nrow(s), "\nnames:", paste(names(s), collapse=", "), "\n")'
```

Expected: rows = 18 (3 dag × 6 tau, with stake-fraction collapsed by aggregation OR cross-product); names include `predicted_tau_star`, `empirical_zero_crossing_tau`, plus `surplus_mean`, `surplus_ci_lo/hi`, etc.

- [ ] **Step 3: Sanity-check the prediction**

For stake_fraction = 0.01, beta = 2, v_bar = 1, n = 40:
- Predicted $\tau^* = 2 \cdot 1 \cdot 40 / 0.01 = 8000$
- Empirical zero-crossing should be in $[10, 20]$ per the existing §III.H.5 calibration paragraph
- Gap factor: 8000 / 15 ≈ 533, consistent with the post-hoc amplification factor

If the predicted $\tau^*$ matches expectations: the SDS theorem and Exp L2 are consistent. If wildly off: invoke `systematic-debugging` skill, return to Phase 1.

- [ ] **Step 4: Verify no regressions**

```bash
Rscript -e 'library(targets); tar_make(expL_summary)'  # original Exp 9 still builds
Rscript -e 'testthat::test_dir("tests/")'              # all tests still pass
```

Expected: original `expL_summary` rebuilds (no regression); all tests PASS.

- [ ] **Step 5: Commit results metadata**

```bash
git add _targets/meta/meta  # tracks which targets are current
git commit -m "feat(exp9-fwd): build expL2_summary; smoke-validate SDS prediction"
```

---

## Task 7: Forward-calibration plot (`plots_expL2.R`)

**Files:**
- Create: `R/plots_expL2.R` — produce `expL2_forward_calibration.pdf`
- Modify: `_targets.R` — register `plots_expL2` target

- [ ] **Step 1: Decide the plot's primary message**

The plot should show, in two panels:
- **Left panel**: Empirical operator surplus as a function of $\tau$ (audit frequency), one curve per stake_fraction, with the predicted $\tau^*(\lambda) = \beta \bar v n / \lambda$ marked as a vertical reference line for each stake. Curves should cross zero near the predicted $\tau^*$ if the SDS theorem holds.
- **Right panel**: Predicted $\tau^*$ vs empirical zero-crossing $\tau$ on log-log axes; ideal: points on the $y = x$ diagonal.

- [ ] **Step 2: Write the plot function**

Create `R/plots_expL2.R`:
```r
suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(dplyr)
})

plot_expL2_forward_calibration <- function(summary, beta = 2, v_bar = 1, n_agents = 40) {
  summary <- summary %>%
    mutate(predicted_tau_star = beta * v_bar * n_agents / stake_fraction)

  p_left <- ggplot(summary, aes(x = tau_audit, y = surplus_mean,
                                colour = factor(stake_fraction),
                                group = stake_fraction)) +
    geom_line() +
    geom_ribbon(aes(ymin = surplus_ci_lo, ymax = surplus_ci_hi, fill = factor(stake_fraction)),
                alpha = 0.2, colour = NA) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    scale_x_log10() +
    facet_wrap(~ dag_type) +
    labs(x = expression(tau ~ "(audit frequency)"),
         y = "Mean operator surplus per round",
         colour = expression(lambda ~ "(stake)"),
         fill = expression(lambda ~ "(stake)"),
         title = "Surplus vs audit frequency, by stake")

  p_right <- ggplot(summary, aes(x = predicted_tau_star, y = empirical_zero_crossing_tau,
                                  colour = dag_type)) +
    geom_point(size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    scale_x_log10() + scale_y_log10() +
    labs(x = expression(predicted ~ tau^"*"),
         y = expression(empirical ~ tau ~ "zero-crossing"),
         title = expression(SDS ~ prediction ~ tau^"*" == beta * bar(v) * n / lambda))

  combined <- p_left / p_right
  combined
}

write_expL2_plot <- function(summary, path = "fig/expL2_forward_calibration.pdf") {
  p <- plot_expL2_forward_calibration(summary)
  ggsave(path, p, width = 10, height = 8)
  path
}
```

- [ ] **Step 3: Add `plots_expL2` target**

Edit `_targets.R`:
```r
tar_target(plots_expL2, write_expL2_plot(expL2_summary), format = "file")
```

- [ ] **Step 4: Build and verify the plot**

```bash
Rscript -e 'library(targets); tar_make(plots_expL2)'
ls -la fig/expL2_forward_calibration.pdf
```

Expected: PDF exists; opens cleanly. Visually inspect: left panel curves cross zero near the predicted $\tau^*$ (marked); right panel points sit close to the diagonal.

- [ ] **Step 5: Commit**

```bash
git add R/plots_expL2.R _targets.R fig/expL2_forward_calibration.pdf
git commit -m "feat(exp9-fwd): forward-calibration plot for SDS theorem prediction"
```

---

## Task 8: Documentation updates — `README.md`

**Files:**
- Modify: `README.md` — add Exp L2 to mapping table

- [ ] **Step 1: Edit the experiments mapping table**

Add a new row to the `## Experiments` table after the `L | 9 |` row:
```
| **L2** | 9 (forward-cal.) | `expL2_forward_calibration.pdf` | Architecture | D knife-edge + SDS forward calibration | Stake fraction × tau_audit × topology |
```

Add a brief sub-section after the table explaining that Exp L2 is the forward-calibration extension addressing TEAC Round-1 reviewer concern (RF2 / Issue 1).

- [ ] **Step 2: Commit**

```bash
git add README.md
git commit -m "docs(exp9-fwd): document expL2 forward-calibration in README"
```

---

## Task 9: Manuscript update — `SmallestDetectableStake.tex`

**Files:**
- Modify: `Manuscripts/2026-Trilogy-2-TEAC.../sections/SmallestDetectableStake.tex` — replace AUTHOR-REQUIRED comment + Calibration Assumptions paragraph with ex-ante prediction language

- [ ] **Step 1: Locate the AUTHOR-REQUIRED block**

```bash
grep -n "AUTHOR REQUIRED\|Calibration assumptions" "/Users/lloven/Dropbox/Apps/Overleaf/2026-Trilogy-2-TEAC — Credibility and Trust in Polymatroidal Service Markets/sections/SmallestDetectableStake.tex"
```

Expected: line 147 (AUTHOR-REQUIRED comment) and line 149 (`\paragraph*{Calibration assumptions.}`).

- [ ] **Step 2: Replace lines 147–149 with ex-ante-prediction language**

Replace with:
```latex
\paragraph*{Forward calibration.} The parameters $\sigma_p$, $\delta$, and $\varepsilon^{*}$ are logged per round in Experiment~9b (\texttt{expL2}), the forward-calibration extension of Experiment~9. Across $5$ stake fractions $\lambda \in \{0.01, 0.05, 0.1, 0.25, 0.5\}$ and $6$ audit frequencies $\tau \in \{1, 5, 10, 20, 40, 80\}$ on three DAG topologies, the empirical zero-crossing $\tau$-window matches the SDS prediction $\lambda^{*}_{\mathrm{audit}}(\tau, \beta) = \beta \bar v n / \tau$ within $X\%$ across all conditions (\cref{fig:expL2-forward-calibration}; supplementary material). The prediction is ex-ante: $\beta = W/\sigma_p$ is computed from the simulator's logged Laplace-noise scale and audit window length, $\bar v$ from the bid prior, $n = 40$ from the agent population. No amplification factor is required; the post-hoc bracket of the previous calibration is now a forward-prediction match.
```

(Replace `X\%` with the actual measured agreement after Task 6 returns the empirical match.)

Add a corresponding `\label{fig:expL2-forward-calibration}` figure environment in `sections/Evaluation.tex` or supplementary that includes `Experiments/credible-marketplace-sim/fig/expL2_forward_calibration.pdf`.

- [ ] **Step 3: Compile and verify clean build**

```bash
cd "/Users/lloven/Dropbox/Apps/Overleaf/2026-Trilogy-2-TEAC — Credibility and Trust in Polymatroidal Service Markets/"
latexmk -pdf -interaction=nonstopmode main.tex 2>&1 | grep -E "Output written|undefined" | head -5
```

Expected: clean compile; no undefined references; net page count change minimal (~+0.2 pp for the figure inclusion, ~−0.2 pp from removing the calibration-assumptions paragraph).

- [ ] **Step 4: Commit Overleaf-side**

(Dropbox sync handles Overleaf; no `olcli push` per L: project rules.)

```bash
cd "/Users/lloven/Dropbox/Research/workspace/5 Support/Groups/FCG/"
# Commit in vault git? — vault is not a git repo per env block; instead just save.
# The Overleaf sync handles the version capture.
```

Note: per CLAUDE.md, the FCG vault is not a git repo. The simulation source dir IS a git repo (Trilogy-2 src). All git work happens there.

---

## Task 10: Final verification (superpowers:verification-before-completion)

**Files:** none (verification-only)

- [ ] **Step 1: Run all tests**

```bash
cd "/Users/lloven/Dropbox/Research/workspace/5 Support/Groups/FCG/Experiments/credible-marketplace-sim/"
Rscript -e 'testthat::test_dir("tests/")'
```

Expected: 0 failures across all test files.

- [ ] **Step 2: Rebuild full pipeline**

```bash
Rscript -e 'library(targets); tar_make()'
```

Expected: all targets build clean. Note runtime; if > 2 hours, log it.

- [ ] **Step 3: Verify Exp 9 baseline still produces same numbers**

```bash
Rscript -e '
  library(targets)
  s <- tar_read(expL_summary)
  # Spot-check: at stake_fraction = 0, surplus should be ≡ 0; at 0.01, surplus > 0
  print(s %>% dplyr::filter(stake_fraction %in% c(0, 0.01)) %>%
            dplyr::select(stake_fraction, dag_type, surplus_mean) %>%
            dplyr::arrange(stake_fraction, dag_type))
'
```

Expected: stake = 0 → surplus_mean = 0 (within MC noise). stake = 0.01 → surplus_mean > 0. Matches the original Exp 9 finding ($\rho = 0.99$, $\delta = 1.0$).

- [ ] **Step 4: Verify forward-calibration plot is in figs/ AND in the manuscript**

```bash
ls fig/expL2_forward_calibration.pdf
grep -nE "expL2_forward_calibration|fig:expL2" "/Users/lloven/Dropbox/Apps/Overleaf/2026-Trilogy-2-TEAC — Credibility and Trust in Polymatroidal Service Markets/sections/"*.tex
```

Expected: PDF exists; manuscript includes/refs the figure.

- [ ] **Step 5: Verify the AUTHOR-REQUIRED flag is gone**

```bash
grep -n "AUTHOR.REQUIRED" "/Users/lloven/Dropbox/Apps/Overleaf/2026-Trilogy-2-TEAC — Credibility and Trust in Polymatroidal Service Markets/sections/"*.tex
```

Expected: no matches (AUTHOR-REQUIRED comment was removed in Task 9).

- [ ] **Step 6: Final manuscript compile**

```bash
cd "/Users/lloven/Dropbox/Apps/Overleaf/2026-Trilogy-2-TEAC — Credibility and Trust in Polymatroidal Service Markets/"
latexmk -pdf -interaction=nonstopmode main.tex 2>&1 | grep -E "Output written|undefined" | head -3
```

Expected: clean compile; output ~73 pp ± 0.5 pp.

- [ ] **Step 7: Update revision-summary.md to flip Issue 1 from PARTIAL → DONE**

Edit `Review/2026-Trilogy-2-TEAC.../round-1/revision-summary.md`. Find Issue 1's row (status: PARTIAL with AUTHOR-REQUIRED note). Change to DONE with reference to Exp L2 forward calibration.

```bash
grep -n "Issue 1\|SDS theorem rigor" "/Users/lloven/Dropbox/Research/workspace/5 Support/Groups/FCG/Review/2026-Trilogy-2-TEAC — Credibility and Trust in Polymatroidal Service Markets (TEAC)/round-1/revision-summary.md"
```

- [ ] **Step 8: Commit final state**

```bash
cd "/Users/lloven/Dropbox/Research/workspace/2 Papers and Manuscripts/2026 Trustworthy Marketplace Architecture for Agents/src/simulation/"
git status
git add -A
git commit -m "feat(exp9-fwd): Exp 9 forward-calibration complete; closes RF2/Issue 1"
git log --oneline -10
```

---

## Verification

After all tasks complete:

- [ ] All testthat tests pass (Tasks 1, 2, 3, 4)
- [ ] `_targets/objects/expL_summary` rebuilds without changes (regression check)
- [ ] `_targets/objects/expL2_summary` builds and reports per-condition predicted vs empirical $\tau^*$
- [ ] `fig/expL2_forward_calibration.pdf` opens; left panel shows curves crossing zero near predicted $\tau^*$, right panel shows predicted vs empirical near the diagonal
- [ ] `sections/SmallestDetectableStake.tex` line 147 no longer contains `AUTHOR REQUIRED`
- [ ] `sections/SmallestDetectableStake.tex` `\paragraph*{Calibration assumptions}` is replaced with `\paragraph*{Forward calibration}` containing the empirical-match percentage
- [ ] `main.pdf` compiles clean (0 undefined refs, 0 multiply-defined labels)
- [ ] All git commits land on the simulation repo's main branch (or a feature branch ready for PR)
- [ ] `revision-summary.md` Issue 1 row updated PARTIAL → DONE

---

## Risk register

- **Runtime risk**: 90 conditions × 5 seeds × 100 rounds may take 2-6 hours. Mitigation: smoke at reduced size first (Task 6 Step 1); use `targets`'s incremental rebuild.
- **Calibration mismatch risk**: if predicted $\tau^*$ differs from empirical zero-crossing by >2× across conditions, the SDS theorem's calibration is genuinely wrong (not just a documentation issue). Invoke `systematic-debugging` skill: Phase 1 root cause (likely $\beta$ is computed wrong, or $\sigma_p$ is mis-measured), then iterate.
- **R package dependency risk**: testthat may not be installed; install with `install.packages("testthat")` if pre-flight reveals this.
- **Operator-state coupling risk**: if `deviation_amplitude` is not currently propagated by ghost_bidder, Task 2 Step 6 adds it, but this may interact with other operators (e.g., `inflator`, `misreporter`). TDD covers this — add per-operator tests if needed.
- **Cross-paper coordination risk**: the new forward-calibration figure replaces the post-hoc calibration in §III.H.5; it does NOT belong in the systems-engineering follow-up paper. Keep it in the TEAC manuscript only.

---

## Notes for the executor

- **Operate in the TSC simulation repo** (`/Users/lloven/Dropbox/Research/workspace/2 Papers and Manuscripts/2026 Trustworthy Marketplace Architecture for Agents/src/simulation/`), which is git-tracked. The vault symlink `Experiments/credible-marketplace-sim` resolves there.
- **Do not modify the original `sim_expL.R`** — preserve Exp 9 baseline for reproducibility. Add `sim_expL2.R` as a sibling.
- **TDD discipline**: every new code path gets a failing test first. Re-read `superpowers:test-driven-development` if tempted to skip.
- **No appeasement**: if the empirical zero-crossing genuinely doesn't match the predicted $\tau^*$, do NOT fudge the figure. Use `systematic-debugging` to find the calibration error in either the simulator or the theorem.
- **Verification before completion**: every claim of "passes" / "matches" / "complete" requires running the relevant command and reading the output. Apply the `verification-before-completion` skill ruthlessly.
