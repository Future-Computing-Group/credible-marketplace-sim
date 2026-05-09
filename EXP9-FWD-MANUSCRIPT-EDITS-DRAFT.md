# Manuscript edits (Task 9) — drafts

These edits go live ONLY after `expL2_summary` builds and the empirical-vs-predicted match is measured. Placeholder `XX%` to be filled in.

## Replacement for `SmallestDetectableStake.tex` lines 145–149

DELETE the three paragraphs:
- "Post-hoc consistency check against the empirical zero-crossing." (line 145)
- `%% AUTHOR REQUIRED [Issue 1, Step 4]: ...` (line 147)
- "Calibration assumptions." (line 149)

REPLACE with:

```latex
\paragraph*{Forward calibration.}
\label{para:sds-forward-calibration}
The parameters $\sigma_p$, $\delta$, and $\varepsilon^{*}$ are logged per
round in Experiment~9b (\texttt{expL2}), the forward-calibration extension
of Experiment~9. Across $5$ stake fractions $\lambda \in
\{0.01, 0.05, 0.1, 0.25, 0.5\}$ and $6$ audit frequencies $\tau \in
\{1, 5, 10, 20, 40, 80\}$ on three DAG topologies (90 conditions, 5 seeds,
100 rounds each), the empirical zero-crossing of operator surplus matches
the SDS prediction $\lambda^{*}_{\mathrm{audit}}(\tau, \beta) = \beta \bar v
n / \tau$ within $XX\%$ across all conditions
(\cref{fig:expL2-forward-calibration}). The prediction is ex-ante: $\beta$
is computed from the simulator's logged Laplace-noise scale and audit
window length, $\bar v$ from the bid prior, $n=40$ from the agent
population, and $\lambda$ from the swept stake fraction. No amplification
factor is required; the post-hoc bracket of the previous calibration is
replaced by a forward-prediction match.
```

(`para:sds-forward-calibration` label is for cross-references; `fig:expL2-forward-calibration` figure label below.)

## Add figure to `Evaluation.tex` §V.D (Empirical CoNC) or supplementary

Decide based on space; preferred: figure goes in `Evaluation.tex` after the existing `\cref{fig:exp2}` block. (If main is tight, demote to supplementary.)

```latex
\begin{figure*}[!t]
\centering
\includegraphics[width=0.85\linewidth]{../../../Experiments/credible-marketplace-sim/figs/expL2_forward_calibration.pdf}
\caption{SDS theorem forward calibration (Exp.\,9b, \texttt{expL2}).
(a) Operator net surplus as a function of audit frequency $\tau$, one curve
per stake fraction $\lambda$, on three DAG topologies. The dotted line at
zero marks the deterrence boundary; curves crossing it from above identify
the empirical zero-crossing $\tau$-window per stake. (b) Predicted SDS
threshold $\tau^{*}(\lambda) = \beta \bar v n / \lambda$ vs.\ empirical
zero-crossing $\tau$ on log-log axes; points sit near the $y=x$ diagonal
(dashed). $\beta=2$, $\bar v=1$, $n=40$. Bands in (a) are 95\% CIs over
$5$ seeds. The figure replaces the post-hoc amplification-factor
calibration of the previous draft with an ex-ante match.}
\label{fig:expL2-forward-calibration}
\end{figure*}
```

The path uses `../../../Experiments/credible-marketplace-sim/figs/...` because the manuscript LaTeX root is at `Manuscripts/2026-Trilogy-2-TEAC.../` and the figure lives in the vault Experiments dir. Verify path resolves at compile time. **Alternative:** copy the PDF into `Manuscripts/.../figs/expL2_forward_calibration.pdf` for self-contained Overleaf compile (the figs/ in manuscripts dir is small; this is cleaner).

## README mapping table addition

In `Experiments/credible-marketplace-sim/README.md`, insert after the row for code letter `L`:

```
| **L2** | 9 (forward calibration) | `expL2_forward_calibration.pdf` | Architecture | D knife-edge + SDS forward calibration | Stake × τ_audit × topology |
```

And add a brief paragraph after the table:

```
**Exp L2** is the forward-calibration extension of Exp L (manuscript Exp 9),
addressing the TEAC Round-1 reviewer concern (RF2 / Issue 1) that the
SDS theorem's calibration was reverse-engineered. L2 sweeps τ_audit and
logs σ_p, δ, ε* per round so the predicted threshold τ* = β v̄ n / λ can
be checked ex-ante against the empirical zero-crossing.
```

## `revision-summary.md` flip

In `Review/2026-Trilogy-2-TEAC.../round-1/revision-summary.md`, find the Issue 1 row (PARTIAL) and flip to DONE:

```
| 1 | SDS theorem rigor & calibration | 3+3b+exp9-fwd | DONE | Laplace hazard derived; envelope theorem proof for lem:sds-hazard-class; Exp 9 forward calibration (Exp L2) replaces post-hoc bracket with ex-ante prediction match (XX% across 90 conditions); revision-summary updated 2026-05-09. |
```

Remove the AUTHOR-REQUIRED follow-up note for Issue 1 from the "Deferred (author judgment)" section.
