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

The codebase uses letter codes (A-O). The manuscript uses sequential numbers
(1-14) grouped by purpose. Combined figure files use letter codes
(`expA_combined.pdf`, etc.).

| Code | Manu. | Figure file | Group | Component(s) | What varies |
|------|-------|-------------|-------|--------------|-------------|
| **A** | 1 | `expA_combined.pdf` | Trilemma | Baseline | Operator strategy x topology x load |
| **E** | 2 | `expE_combined.pdf` | Trilemma | +C | Credibility x topology x N (ghost bidder) |
| **K** | 3 | `expK_combined.pdf` | Trilemma | +M (Myerson) | Mechanism x credibility x value dist |
| **B** | 4 | `expB_combined.pdf` | Resolution | C comparison | Credibility x operator x topology x load |
| **F** | 5 | `expF_combined.pdf` | Resolution | +D | Fee mode x operator x topology x load |
| **C** | 6 | `expC_combined.pdf` | Resolution | +K | k integrators x strategy x topology x load |
| **D** | 7 | `expD_combined.pdf` | Architecture | T (L1 x L2) | L1 trust x L2 trust x topology x N |
| **J** | 8 | `expJ_combined.pdf` | Architecture | C x K | Credibility x k_integrators x topology |
| **L** | 9 | `expL_combined.pdf` | Architecture | D knife-edge | Stake fraction x operator x topology |
| **H** | 10 | `expH_combined.pdf` | Robustness | C (adaptive) | Credibility x topology x N (bandit operator) |
| **I** | 11 | `expI_combined.pdf` | Robustness | C (degradation) | p_broadcast x topology (ghost bidder) |
| **M** | 12 | `expM_combined.pdf` | Robustness | Agent exit | Credibility x agent_mode x topology |
| **N** | 13 | `expN_combined.pdf` | Robustness | Markov channel | Channel model x p_stationary x topology |
| **O** | 14 | `expO_combined.pdf` | Robustness | Non-stat. supply | Capacity model x credibility x topology |
| **G** | Suppl. | --- | Supplement | Sensitivity | Parameter sweeps x topology |

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
_targets.R              # pipeline definition (14 experiments + stats)
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
  sim_expA.R             # Exp A (Manu. 1): welfare loss
  sim_expB.R             # Exp B (Manu. 4): credibility comparison
  sim_expC.R             # Exp C (Manu. 6): integrator competition
  sim_expD.R             # Exp D (Manu. 7): two-tier trust
  sim_expE.R             # Exp E (Manu. 2): credibility trilemma
  sim_expF.R             # Exp F (Manu. 5): domain separation
  sim_expG.R             # Exp G (Suppl.): sensitivity analysis
  sim_expH.R             # Exp H (Manu. 10): adaptive operator
  sim_expI.R             # Exp I (Manu. 11): imperfect broadcast
  sim_expJ.R             # Exp J (Manu. 8): credibility x competition
  sim_expK.R             # Exp K (Manu. 3): revenue-optimal (Myerson)
  sim_expL.R             # Exp L (Manu. 9): domain sep. knife-edge
  sim_expM.R             # Exp M (Manu. 12): strategic agent adaptation
  sim_expN.R             # Exp N (Manu. 13): Markov broadcast channel
  sim_expO.R             # Exp O (Manu. 14): non-stationary supply
  plot_helpers.R         # shared: theme_ieee, palettes, save_fig
  plots_combined.R       # combined figures for Exps A-I (Manu. 1-2, 4-6, 10-11)
  plots_expA.R ... plots_expO.R   # per-experiment figure functions
figs/                    # generated figures (PDF, gitignored)
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
