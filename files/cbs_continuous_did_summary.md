# CBS Overall Summary
### Callaway, Goodman-Bacon, Sant'Anna — "Difference-in-Differences with a Continuous Treatment" (v4, Dec 31, 2025)

## 1. What the paper is about

Most DiD work with a continuous treatment runs a TWFE regression of the form
```
Y_{i,t} = θ_t + η_i + β^{twfe} · D_i · Post_t + v_{i,t}        (1.1)
```
and interprets `β^{twfe}` as some kind of "effect of the dose." CBS argue that this coefficient is a black box: it admits **four different decompositions** into underlying causal parameters, none of which supports a clean causal interpretation when effects are heterogeneous.

Three contributions:

1. **Identification.** Define dose-specific building-block parameters (`ATT(d|d)`, `ATT(d)`, `ACRT(d|d)`, `ACRT(d)`) and show which parallel-trends assumption identifies each.
2. **Reverse-engineering TWFE.** Decompose `β^{twfe}` into weighted integrals of these building blocks. All decompositions are problematic.
3. **Forward-engineering estimators.** Target well-defined causal objects (`ATT^loc`, `ACRT^glob`) with binarized DiD and data-adaptive nonparametric (CCK 2025) estimators. Revisit Acemoglu & Finkelstein (2008) and show the new estimators reach similar headline conclusions but tell a much more heterogeneous story.

## 2. Parameters of interest

Two distinct causal concepts (they coincide when treatment is binary but not otherwise):

- **Level treatment effect:** `Y_t(d) − Y_t(0)` — the total effect of switching from 0 to dose `d`.
- **Causal response:** `Y'_t(d)` — marginal effect of a dose increment.

Building blocks for the two-period baseline:
- `ATT(d|d') = E[Y_{t=2}(d) − Y_{t=2}(0) | D = d']` — effect of dose `d` on units who got dose `d'`.
- `ATT(d)   = E[Y_{t=2}(d) − Y_{t=2}(0) | D > 0]` — effect of dose `d` on all treated units.
- `ACRT(d|d') = ∂ATT(l|d')/∂l |_{l=d}` — marginal response at `d` for dose group `d'`.
- `ACRT(d)   = ∂ATT(d)/∂d`.

Aggregated summaries (using the dose density among treated):
- `ATT^loc = E[ATT(D|D)|D>0]`, `ATT^glob = E[ATT(D)|D>0]`
- `ACRT^loc = E[ACRT(D|D)|D>0]`, `ACRT^glob = E[ACRT(D)|D>0]`

## 3. Identification

Assumptions: 1 (iid panel), 2 (P(D=0)>0), 3 (no anticipation + observed outcomes), 4 (continuous or discrete dose).

### Parallel Trends (PT)
```
E[Y_{t=2}(0) − Y_{t=1}(0) | D = d] = E[Y_{t=2}(0) − Y_{t=1}(0) | D = 0]       for all d ∈ D_+
```
Under PT:
- **Theorem 3.1:** `ATT(d|d) = E[ΔY|D=d] − E[ΔY|D=0]`, and `ATT^loc = E[ΔY|D>0] − E[ΔY|D=0]` — a plain binary DiD on a positive-dose dummy.
- **Theorem 3.2:** Between-dose comparisons contaminate causal responses with selection bias:
  ```
  E[ΔY|D=h] − E[ΔY|D=l] = E[Y_2(h) − Y_2(l)|D=h]  +  (ATT(l|h) − ATT(l|l))
                                causal effect                selection bias
  ```
  PT does not discipline how the level effect of dose `l` varies across dose groups, so the selection bias is unidentified.

### Strong Parallel Trends (SPT)
```
E[Y_{t=2}(d) − Y_{t=1}(0) | D > 0] = E[Y_{t=2}(d) − Y_{t=1}(0) | D = d]       for all d ∈ D
```
Equivalently (under PT), `ATT(d|d) = ATT(d)`: no selection into dose on the basis of treatment effects.
- **Theorem 3.3:** Under SPT, `ATT(d) = E[ΔY|D=d] − E[ΔY|D=0]` and dose-group comparisons identify `ATT(h) − ATT(l) = E[Y_2(h) − Y_2(l)|D>0]`. Continuous: `ACRT(d) = ∂E[ΔY|D=d]/∂d`. Discrete: analogous scaled difference.
- Pre-period tests cannot distinguish PT from SPT — they only see untreated potentials.

## 4. ★ Table 1 and how to compute the TWFE weights ★ 

This is the centerpiece for the `baconplus` package. Theorem 3.4 decomposes `β^{twfe}` four different ways, and Table 1 gives the weights. Every weight shares the denominator `Var(D)` — variance of the dose taken over the **full sample including zeros**. Let `E[D]` denote the unconditional mean, `f_D` the density of D (with point mass at 0), and `P(·)` probabilities under the unconditional distribution.

### (a) Causal-response decomposition
```
β^{twfe} = ∫_{d_L}^{d_U} w^{acrt}_1(l) · [ACRT(l|l) + ∂ATT(l|h)/∂h|_{h=l}] dl
         + w^{acrt}_0 · ATT(d_L|d_L)/d_L
```
with
```
w^{acrt}_1(l) = (E[D | D ≥ l] − E[D]) · P(D ≥ l) / Var(D)
w^{acrt}_0   = (E[D | D > 0] − E[D]) · P(D > 0) · d_L / Var(D)
```
- Weights are **non-negative and integrate to 1**.
- Under PT this is a weighted `ACRT(l|l)` plus a selection-bias integrand. Under SPT the selection bias term drops out and `β^{twfe}` becomes "weakly causal" (Blandhol, Bonney, Mogstad, Torgovitsky 2025) — a non-negative weighted average of `ACRT(l)`s — but the weights `w^{acrt}_1` are **not** the dose density `f_{D|D>0}`, so this isn't `ACRT^glob`. In the AF application, dropping the untreated group shrinks `β^{twfe}` by 78% even though the underlying `ACRT(l)` curve is unchanged.

Mechanically this is Yitzhaki (1996) — the OLS slope of Y on D as a weighted average of the local slopes of `E[Y|D=d]`. The `w^{acrt}_0` term handles the discrete jump from 0 to the minimum positive dose `d_L`.

### (b) Levels decomposition
```
β^{twfe} = ∫_{d_L}^{d_U} w^{lev}_1(l) · ATT(l|l) dl      (+ boundary term w^{lev}_0)
```
with
```
w^{lev}_1(l) = (l − E[D]) · f_D(l) / Var(D)
w^{lev}_0    = − E[D] · P(D = 0) / Var(D)
```
- **Sign flips at `l = E[D]`** — negative on below-mean doses, positive on above-mean.
- Total weight integrates to zero: `∫ w^{lev}_1(l) dl + w^{lev}_0 = 0`. So `β^{twfe}` is not weakly causal when the building block is the level effect. Above-mean doses act as an effective "treated group"; below-mean doses (and D=0) act as an effective "comparison group" that can contain actually-treated units.
- Equivalent Wald representation (eq. 3.1):
  ```
  β^{twfe} = ( E[w^bin_1(D) ΔY | D > E[D]] − E[w^bin_0(D) ΔY | D < E[D]] )
             / ( E[w^bin_1(D) D | D > E[D]] − E[w^bin_0(D) D | D < E[D]] )
  ```
  where `w^bin_k(d) ∝ |d − E[D]|`. Numerator is a weighted level-effect; denominator is a weighted dose distance.

### (c) Scaled-levels decomposition
```
β^{twfe} = ∫_{d_L}^{d_U} w^s(l) · ATT(l|l)/l dl
```
with
```
w^s(l) = l · (l − E[D]) · f_D(l) / Var(D)
```
- Same sign flip at `E[D]` — still negative weights below the mean.
- But `∫ w^s(l) dl = 1` rather than 0. Doesn't cure the negative-weights problem. In the discrete case this matches Theorem S3 of dCdH (2020) supplementary appendix.

### (d) Scaled 2×2 decomposition
```
β^{twfe} = ∫_{d_L}^{d_U} ∫_{h > l} w^{2×2}_1(l, h) ·
             [ (E[Y_2(h) − Y_2(l) | D=h])/(h−l)   +   (ATT(l|h) − ATT(l|l))/(h−l) ] dh dl
         + ∫ w^{2×2}_0(l) · ATT(l|l)/l dl
```
with
```
w^{2×2}_1(l, h) = (h − l)^2 · f_D(h) · f_D(l) / Var(D)             for h > l, both in D_+
w^{2×2}_0(h)    = h^2 · f_D(h) · P(D = 0) / Var(D)
```
- Weights non-negative, integrate to 1.
- Under PT each 2×2 comparison mixes a causal effect with a selection-bias term; SPT kills the bias. Even so, the weighting pattern (squared dose gaps) doesn't correspond to any natural policy-relevant object.

### SPT upgrades across decompositions
If SPT holds instead of PT, replace: `ACRT(l|l) → ACRT(l)`, `ATT(l|l) → ATT(l)`, and `E[Y_2(h) − Y_2(l)|D=h] → E[Y_2(h) − Y_2(l)|D>0]`; the selection-bias integrands in (a) and (d) drop to zero. The weights are unchanged.

### A concrete algorithm for `baconplus`

Given cross-sectional data `{(Y_{i,1}, Y_{i,2}, D_i)}_{i=1}^n`:

1. **Dose distribution.**
   - `p0 = mean(D == 0)`; `p1 = 1 − p0`.
   - `μ = mean(D)`; `σ² = var(D)` (both computed on the full sample including zeros).
   - `d_L, d_U = min(D[D > 0]), max(D[D > 0])`.
   - Estimate `f_D(·)` on the treated sub-sample via kernel density (bandwidth by Silverman or a cross-validation rule).
2. **Evaluation grid** on `[d_L, d_U]`, say 200 points.
3. **Levels weights** (per grid point `l`):
   ```
   w_lev(l)  = (l − μ) · f̂(l) / σ²                       (can be negative)
   w_lev0    = − μ · p0 / σ²                              (scalar, for D=0)
   w_slev(l) = l · (l − μ) · f̂(l) / σ²                    (scaled levels)
   ```
4. **Causal-response weights** (per grid point `l`):
   ```
   S(l) = P̂(D ≥ l)                                        # survival function of D at l, full sample
   m(l) = Ê[D | D ≥ l]                                    # conditional mean of D above l, full sample
   w_acrt(l) = (m(l) − μ) · S(l) / σ²
   w_acrt0   = (Ê[D | D > 0] − μ) · p1 · d_L / σ²         (scalar, attached to ATT(d_L|d_L)/d_L)
   ```
5. **Scaled 2×2 weights** (per pair `(l, h)` with `h > l`):
   ```
   w_22(l, h) = (h − l)^2 · f̂(h) · f̂(l) / σ²
   w_220(h)   = h^2 · f̂(h) · p0 / σ²                      (paired with (0, h))
   ```
6. **Sanity checks** (all computed numerically on the grid):
   - `∫ w_acrt(l) dl + w_acrt0 ≈ 1` and both non-negative.
   - `∫ w_lev(l) dl + w_lev0 ≈ 0`.
   - `∫ w_slev(l) dl ≈ 1`.
   - `∬ w_22(l,h) dh dl + ∫ w_220(h) dh ≈ 1`.
7. **Plot** each weight function against `l` alongside `f̂_{D|D>0}` — this is Figure 7. The gap between `w_acrt(l)` and `f̂_{D|D>0}(l)` is the visual statement of "TWFE is not `ACRT^glob`."

## 5. Forward-engineered estimators (§4)

Under SPT:
- **`ATT^glob`** = plain binary DiD on `D^{>0} = 1{D > 0}`:
  ```
  ΔY_i = β^bin_0 + D^{>0}_i · β^bin + ε_i     ⇒     β^bin = ATT^glob
  ```
- **Discrete ACRT**: saturated regression (4.1) in treatment dummies; pairwise differences give `ACRT(d_j)`.
- **Parametric continuous ACRT**: quadratic (4.2) in D on the treated sub-sample.
- **Nonparametric ACRT**: CCK 2025 cubic B-spline sieve with Lepski-type `K̂` and honest uniform confidence bands (Appendix B; Algorithms 1 and 2).
- **`ACRT^glob` (plug-in)**: average `ACRT̂(D_i)` over treated units; `√n_{D>0}`-consistent, asymptotically normal. Neyman-orthogonal form (eq. 4.8) opens the door to double machine learning.

## 6. Empirical application — Acemoglu & Finkelstein (2008) Medicare PPS

### Setup
- 5,881 hospitals (balanced panel from AF), `t=1` = 1980–83 average, `t=2` = 1984–86 average.
- Dose `M` = 1983 Medicare inpatient share. Mean 0.45 (sd 0.15) among treated; ~15% of hospitals have `M = 0`.
- Outcome: depreciation share of operating expenses (K/L proxy).
- Homothetic two-factor production model with subsidy ratio `1 + S_t(M) = (1 − s_L M)/(1 − s_K M)`; 1983 PPS sets `s_L = 0`.

### Numerical headline

| Quantity | Estimate | SE |
|---|---|---|
| `β̂^{twfe}` (static) | 1.14 | 0.099 |
| `ATT̂^loc` (nonparametric CCK, K̂=4) | 0.80 | 0.05 |
| Implied % change in K/L (vs 1983 mean of 4.5) | ~18% | |
| `ACRT̂^glob` (dose-weighted) | −0.08 | 0.19 |
| Post-treatment `ATT^{es}_loc` average | 0.755 | 0.048 |
| Post-treatment `ACRT^{es}_glob` average | 0.220 | 0.170 |

### Key takeaways
- **Headline level effect survives.** PPS raised K/L by ~18% on average; CBS's `ATT^loc` estimate is actually **50% larger** than the comparable comparable TWFE number (authors frame this as a direct contrast to AF's interpretation in the intro).
- **β^{twfe} ≈ ATT^loc in this dataset is a coincidence.** Eq. (3.1) numerator = 0.60 (biased down because effective comparison group contains many treated hospitals); denominator = 0.53; they nearly cancel. Rescaling `M` from 0–1 to 0–100 shrinks `β^{twfe}` by a factor of 100 while `ATT^loc` is unchanged.
- **ACRT story is the bombshell.** `ACRT̂(m)` is positive at low doses, crosses zero around `m = 0.41`, and is negative for the 71% of treated hospitals with higher doses. Uniform bands don't reject zero, but pointwise CIs do. So `ACRT̂^glob ≈ 0`, versus `β̂^{twfe} = 1.14`. The **entire** gap is the weighting difference from Figure 7.
- Negative ACRTs at high doses **contradict the two-factor neoclassical model** (elasticity of substitution should be positive). Three candidate fixes: three-factor production, non-homothetic production, or SPT violation.
- **Evidence against SPT.** Pre-treatment `ACRT^{es}` is significantly nonzero in 1981 — under PT/SPT it should be zero pre-treatment.
- **Big-picture lesson.** Even when TWFE weights are all positive and integrate to 1 (as in the causal-response decomposition under SPT), `β^{twfe}` can be dramatically different from the policy-relevant target. The profession has used "negative weights" as the dividing line between acceptable and unacceptable aggregation; CBS argue that's the wrong dividing line. The right question is whether you're targeting a well-defined causal parameter in the first place.

## 7. Staggered / multi-period extension (Appendix C)
- Timing group `G_i`, dose `D_i`; potential outcomes `Y_{i,t}(g, d)`. Staggered adoption + no anticipation.
- **Theorem C.1:** under PT-MP, `ATT(g,t,d|g,d) = E[Y_t − Y_{g−1}|G=g, D=d] − E[Y_t − Y_{g−1}|W_t=0]` — long difference from `g−1` (last pre-treatment period for group g) to `t`, with a not-yet-treated or never-treated comparison. Under SPT-MP, derivative gives `ACRT(g,t,d|g,d)`.
- Aggregations: `ATT^{dose}(d|d)`, `ACRT^{dose}(d|d)`, `ATT^{loc}`, `ACRT^{loc}`, event-study versions `ATT^{es}_loc(e)`, `ACRT^{es}_loc(e)`.
- **Shortcut:** when the target is `ATT^{es}_loc(e)`, binarize the treatment (any positive dose → treated) and run the standard Callaway–Sant'Anna (2021) event study.
- Multi-period TWFE inherits all the Theorem 3.4 issues plus the usual staggered TWFE negative-weights issues (dCdH 2020, Goodman-Bacon 2021) plus sensitivity to pre-treatment PT violations.

## 8. Plan for `baconplus`
- Implement Table 1 weight calculations exactly as in §4 above — kernel density on treated units, survival function and conditional mean from the empirical CDF, numerical integration on a dose grid.
- Reproduce Figure 7 on AF's data as the first validation check.
- Reproduce `β̂^{twfe} = 1.14`, `ATT̂^loc = 0.80`, `ACRT̂^glob = −0.08` as the second validation check.
- Tie it back to the binary Bacon decomposition (Goodman-Bacon 2021) — same underlying logic of "regression coefficient = weighted average of 2×2 comparisons", extended to the dose dimension.
