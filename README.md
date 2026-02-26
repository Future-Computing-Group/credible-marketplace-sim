# credible-marketplace-sim

Simulation study for the paper:
**"From Credibility to Trust in Agentic Service Markets: Mechanism Design for Real-Time AI Economies"**
Submitted to IEEE Transactions on Services Computing.

## Overview

This simulation studies the behaviour of strategic marketplace operators in
real-time AI service economies and evaluates mechanisms for ensuring truthful
mechanism execution. It complements the theoretical contributions of the paper
(credibility trilemma, commitment-based credible mechanisms, domain separation,
integrator competition) with a systematic ablation study across five credibility
components: commitment devices (C), domain separation (D), integrator
competition (K), two-tier architecture (T), and structural topology (S).

## Experiments

| Code | Paper | Component(s) | What varies | Key metrics |
|------|-------|--------------|-------------|-------------|
| **A** | Exp 1 | Baseline | Operator strategy x topology x load | Welfare loss, surplus extraction |
| **E** | Exp 2 | +C | Credibility x topology x N (ghost bidder) | Ghost surplus, detection rate |
| **B** | Exp 3 | C x operator | Credibility x operator x topology x load | Welfare recovery, detection rate |
| **F** | Exp 4 | +D | Fee mode x operator x topology x load | Surplus, welfare (domain separation) |
| **C** | Exp 5 | +K | k integrators x strategy x topology x load | Price markup, welfare |
| **D** | Exp 6 | T (L1 x L2) | L1 trust x L2 trust x topology x N | End-to-end welfare, compliance |
| **H** | Exp 7 | C (adaptive) | Credibility x topology x N (bandit operator) | Converged arm, cumulative surplus |
| **I** | Exp 8 | C (degradation) | p_broadcast x topology (ghost bidder) | Profitability threshold, welfare |
| **J** | Exp 9 | C x K | Credibility x k_integrators x topology | Orthogonality, surplus, pricing |
| **G** | Suppl. | --- | Parameter sweeps x topology | Sensitivity analysis |

## Running

```r
# Full pipeline (experiments + statistical analysis)
targets::tar_make()

# Visualise target DAG
targets::tar_visnetwork()

# Check what needs rebuilding
targets::tar_outdated()

# Load a result
targets::tar_read(expA_summary)
targets::tar_read(stats_expA)
```

## Dependencies

- R (>= 4.2)
- targets
- dplyr, tidyr, purrr, tibble
- ggplot2, scales, patchwork
- boot
- ARTool (optional; for interaction analysis)

Install with:

```r
install.packages(c(
  "targets", "dplyr", "tidyr", "purrr", "tibble",
  "ggplot2", "scales", "patchwork", "boot"
))
# Optional:
install.packages("ARTool")
```

## File structure

```
_targets.R              # pipeline definition (10 experiments + stats)
R/
  sim_helpers.R          # shared: DAG topologies, environment, agents, tasks
  sim_market.R           # two-tier marketplace engine: VCG + tatonnement
  sim_operator.R         # operator strategies: truthful, misreporter,
                         #   inflator, discriminator, ghost_bidder
  sim_credibility.R      # credibility: broadcast, blockchain, exchange,
                         #   regulatory
  sim_integrator.R       # integrator competition: monopoly, competitive,
                         #   collusive
  stat_analysis.R        # statistical analysis: BCa bootstrap, KW,
                         #   Wilcoxon+Holm, Cliff's delta, ART ANOVA
  sim_expA.R             # Exp A (Paper 1): welfare loss
  sim_expB.R             # Exp B (Paper 3): credibility comparison
  sim_expC.R             # Exp C (Paper 5): integrator competition
  sim_expD.R             # Exp D (Paper 6): two-tier trust
  sim_expE.R             # Exp E (Paper 2): credibility trilemma
  sim_expF.R             # Exp F (Paper 4): domain separation
  sim_expG.R             # Exp G (Suppl.): sensitivity analysis
  sim_expH.R             # Exp H (Paper 7): adaptive operator
  sim_expI.R             # Exp I (Paper 8): imperfect broadcast
  sim_expJ.R             # Exp J (Paper 9): credibility x competition
  plot_helpers.R         # shared: theme_ieee, palettes, save_fig
  plots_combined.R       # combined main-paper figures (Exps 1-9)
  plots_expA.R           # Exp A figures
  plots_expB.R           # Exp B figures
  plots_expC.R           # Exp C figures
  plots_expD.R           # Exp D figures
  plots_expE.R           # Exp E figures
  plots_expF.R           # Exp F figures
  plots_expG.R           # Exp G figures
  plots_expH.R           # Exp H figures
  plots_expI.R           # Exp I figures
  plots_expJ.R           # Exp J figures
fig/                     # generated figures (PDF, gitignored)
_targets/                # pipeline cache (gitignored)
```

## Statistical analysis

Each experiment produces a `stats_expX` target containing:

- **BCa bootstrap CIs** (2000 resamples) for all point estimates
- **Kruskal-Wallis H** omnibus test for the primary factor
- **Pairwise Wilcoxon** rank-sum tests with Holm correction
- **Cliff's delta** effect sizes (negligible < 0.147, small < 0.33,
  medium < 0.474, large >= 0.474)
- **ART ANOVA** for interaction effects (requires ARTool)
