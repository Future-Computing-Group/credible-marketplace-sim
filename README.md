# credible-marketplace-sim

Simulation study underlying two companion papers on credibility in
polymatroidal service markets (Trilogy Paper 2, split into 2A + 2B):

- **2A — "Credibility Trilemma in Polymatroidal Service Markets"**
- **2B — "A Deployable Credibility Surface for Polymatroidal Service Markets"**

Both are in preparation for ACM Transactions on Economics and Computation
(TEAC). Released under the MIT licence; see `LICENSE` and `CITATION.cff`,
and the Zenodo archive at https://doi.org/10.5281/zenodo.20394159.

## Overview

This simulation studies the behaviour of strategic marketplace operators in
real-time AI service economies and evaluates mechanisms for ensuring truthful
mechanism execution. It complements the theoretical contributions of the two
companion papers (the credibility trilemma and its three resolutions ---
commitment-based credible mechanisms, domain separation, integrator competition
--- in 2A; the deployable credibility surface, Smallest Detectable Stake
theorem, and two-tier architecture in 2B) with a systematic ablation study
across five credibility components: commitment devices (C), domain separation
(D), integrator competition (K), two-tier architecture (T), and structural
topology (S).

## Experiments

The codebase uses letter codes (A-O, plus R5 / L2 / L3). The **Paper**
column maps each experiment to its reporting paper after the 2026-05-23
2A/2B split:

- **2A** (Credibility Trilemma) reports the trilemma-illustration experiments
  (Exps.~1--3 in 2A's prose, code letters A / E / K) plus the mechanism-class
  robustness ablation (code R5).
- **2B** (Deployable Credibility Surface) reports the eight-experiment
  deployable-surface programme: code letters B / F / C / D / J / L / L2 / L3.
- **follow-up** marks experiments that were part of the original combined
  Paper 2 plan but are not described in either 2A or 2B; they remain in the
  repo for future work and are not cited from any current manuscript.
- **suppl.** is the in-repo sensitivity supplement (Exp G), not paper-bound.

The **Manu.** numbers (1-14) reflect the original combined experiment
programme and are kept for codebase continuity; the **Paper** column is
the authoritative manuscript-attribution. Combined figure files use
letter codes (`expA_combined.pdf`, etc.).

| Code | Paper | Manu. | Figure file | Group | Component(s) | What varies |
|------|-------|-------|-------------|-------|--------------|-------------|
| **A**  | 2A | 1 | `expA_combined.pdf` | Trilemma | Baseline | Operator strategy x topology x load |
| **E**  | 2A | 2 | `expE_combined.pdf`, `fig2_conc_summary.pdf` | Trilemma | +C | Credibility x topology x N (ghost bidder) |
| **K**  | 2A | 3 | `expK_combined.pdf` | Trilemma | +M (Myerson) | Mechanism x credibility x value dist |
| **R5** | 2A | R-5 | `expR5_conc_by_mechanism.pdf`, `expR5_gamma_distribution.pdf` | Trilemma | Mechanism-class + CoNC^ag + gamma_ij | Mechanism x operator x topology x p_post |
| **B**  | 2B | 4 | `expB_combined.pdf` | Resolution | C comparison | Credibility x operator x topology x load |
| **F**  | 2B | 5 | `expF_combined.pdf` | Resolution | +D | Fee mode x operator x topology x load |
| **C**  | 2B | 6 | `expC_combined.pdf` | Resolution | +K | k integrators x strategy x topology x load |
| **D**  | 2B | 7 | `expD_combined.pdf` | Architecture | T (L1 x L2) | L1 trust x L2 trust x topology x N |
| **J**  | 2B | 8 | `expJ_combined.pdf` | Architecture | C x K | Credibility x k_integrators x topology |
| **L**  | 2B | 9 | `expL_combined.pdf` | Architecture | D knife-edge | Stake fraction x operator x topology |
| **L2** | 2B | 9 (fwd-cal) | `expL2_forward_calibration.pdf` | Architecture | D knife-edge + SDS forward calibration | Stake fraction x tau_audit x topology (regulatory_sds) |
| **L3** | 2B | (bilinear) | `expL3_bilinear_surface.pdf` | Architecture | Bilinear (1-eta)*lambda surface | eta x lambda x topology (regulatory_sds) |
| **H**  | follow-up | 10 | `expH_combined.pdf` | Robustness | C (adaptive) | Credibility x topology x N (bandit operator) |
| **I**  | follow-up | 11 | `expI_combined.pdf` | Robustness | C (degradation) | p_broadcast x topology (ghost bidder) |
| **M**  | follow-up | 12 | `expM_combined.pdf` | Robustness | Agent exit | Credibility x agent_mode x topology |
| **N**  | follow-up | 13 | `expN_combined.pdf` | Robustness | Markov channel | Channel model x p_stationary x topology |
| **O**  | follow-up | 14 | `expO_combined.pdf` | Robustness | Non-stat. supply | Capacity model x credibility x topology |
| **G**  | suppl. | Suppl. | --- | Supplement | Sensitivity | Parameter sweeps x topology |

**Exp L2** is the forward-calibration extension of Exp L (manuscript Exp 9),
addressing the TEAC Round-1 reviewer concern (RF2 / Issue 1) that the SDS
theorem's calibration was reverse-engineered. L2 sweeps τ_audit and uses a
new `regulatory_sds` credibility branch (`R/sim_credibility.R`) that
implements the SDS hazard `1 - exp(-β·δ)` literally, with canonical penalty
`C(β,τ) = v̄·n/τ` independent of surplus extracted. This lets the predicted
threshold τ* = β v̄ n / λ be checked ex-ante against the empirical zero-crossing,
without the post-hoc amplification factor of the previous calibration.

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
