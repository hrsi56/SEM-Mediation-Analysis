# SEM Mediation Analysis — Pre-Trauma Substance Use & PTSD Outcomes

> Structural Equation Modeling with bootstrap-based mediation inference, applied to a clinical dataset of 133 survivors of a mass-casualty terror attack. Core analysis published in *World Psychiatry* (IF 60+).

**Status:** Analysis complete. Published manuscript: [Nacasch, Dejorno et al., *World Psychiatry* 23(3), 2024](https://doi.org/10.1002/wps.21254).

---

## Problem

Multiple linear regression failed to capture the data structure. Substance-use patterns, peritraumatic dissociation, and PTSD symptoms are not independent predictors — they form a **hierarchical, mediated system** in which the effect of pre-trauma substances on post-trauma outcomes may be partially routed through dissociative responses during the event itself.

A flat regression model misses this: it reports the *combined* direct+indirect effect as a single coefficient, with no way to separate the pathway. The modeling choice here was to move from multiple regression to **Structural Equation Modeling with explicit mediation paths**, estimated with **bias-corrected bootstrap confidence intervals** (5,000 draws) to respect the small clinical sample.

## Dataset

- **N = 133** observations (survivors assessed via clinical intake questionnaires).
- **Exposure variables:** Pre-trauma use of alcohol, cannabis, MDMA, LSD, cocaine, DOSA, "doctor" (prescribed), MMC, and polydrug combinations.
- **Outcome variables:** VAS-Anxiety, GAD-7, PHQ-9, PDS-5 total + four DSM-5 subscales (Intrusive, Avoidance, Negative Mood, Hyperarousal).
- **Hypothesized mediator:** Peritraumatic Dissociative Experiences (PDEQ).
- **Covariates:** Sex, age.

## Modeling Approach

1. **Step 1 — Baseline regressions.** Multiple linear regression per outcome against all substances + covariates. Result: only alcohol showed consistent significance across outcomes; other substances were null. Conclusion: a simpler model than expected, but the *mechanism* was still unclear.

2. **Step 2 — Mediation hypothesis.** Testing whether the alcohol → PTSD-symptoms effect is mediated by peritraumatic dissociation, following the Baron & Kenny framework and estimated via bootstrap.

3. **Step 3 — SEM with simultaneous paths.** Full structural model with all substances as predictors, dissociation as mediator, and the full symptom panel as outcomes — allowing for correlated errors among related symptoms.

### Model (lavaan syntax excerpt)

```r
model <- '
  # Direct paths from exposure to outcomes
  Intrusive     ~ Alcohol.vs..all + dosa.vs..all + Canabis.vd.all + MDMA.vs.all + ...
  Avoidence     ~ Alcohol.vs..all + ...
  Negative.mood ~ Alcohol.vs..all + ...
  Hyperarousal  ~ Alcohol.vs..all + ...

  # Mediator regression
  Dissociation ~ Alcohol.vs..all + dosa.vs..all + ...

  # Outcome regressions include mediator
  PDS ~ Intrusive + Avoidence + Negative.mood + Hyperarousal + Dissociation

  # Covariates on exposures
  Alcohol.vs..all ~ SEX.cod + Age
  ...
'

fit <- sem(model, data = df, se = "bootstrap", bootstrap = 5000)
```

## Results

### Model Fit

| Metric | Value | Threshold |
|---|---|---|
| χ² | 59.67 (df=31) | — |
| CFI | **0.973** | > 0.95 = good |
| GFI | **0.946** | > 0.90 = acceptable |
| RMSEA | 0.083 | 0.05–0.10 = acceptable |
| Multicollinearity | tolerance 0.78–0.95 | > 0.20 = safe |

### Key Finding — Partial Mediation

Peritraumatic dissociation **partially mediates** the effect of pre-trauma alcohol use on multiple PTSD symptom clusters:

| Outcome | Direct β (pre-mediator) | Direct β (post-mediator) | Indirect β | % mediated |
|---|---|---|---|---|
| PDS-5 Mood       | 0.146 (p=0.03) | 0.109 (p=0.04) | 0.111 | **42.8%** |
| PDS-5 Arousal    | 0.159 (p=0.01) | 0.103 (p=0.01) | 0.129 | **39.5%** |
| PDS-5 Intrusive  | 0.230 (p<0.001) | 0.091 (p=0.01) | 0.184 | **28.4%** |
| PDS-5 Avoidance  | 0.129 (p=0.08) | 0.079 (p=0.02) | 0.076 | **38.0%** |
| GAD-7            | 0.204 (p=0.001) | 0.094 (p=0.01) | 0.166 | **31.5%** |
| PHQ-9            | 0.243 (p<0.001) | 0.078 (p=0.02) | 0.149 | **24.3%** |

Zero was excluded from all bias-corrected bootstrap CIs, confirming significance of the indirect effects.

**Crucially:** no other substance (cannabis, MDMA, LSD, cocaine, DOSA, MMC) showed a significant mediation path — the effect is specific to alcohol.

### Path Diagrams

Full SEM path diagrams with standardized coefficients (`***` p<0.001, `**` p<0.01, `*` p<0.05):

- `figures/sem_path_pds.jpg` — Alcohol → Dissociation → PDS-5 subscales → PDS total
- `figures/sem_path_anxdep.jpg` — Alcohol → Dissociation → VAS / GAD-7 / PHQ-9

## Repository Contents

```
.
├── scripts/
│   ├── STATS_R_YARDEN.R       # primary SEM fitting and exploration
│   └── boot_R_YARDEN.R        # bootstrap resampling + diagram generation
├── results/
│   ├── fitest.txt             # global fit indices
│   ├── summary.txt            # full lavaan summary (all parameters)
│   ├── modificationindices.txt
│   └── standardizedSolution_boot_ci.csv  # standardized estimates + 95% bootstrap CIs
├── figures/
│   ├── sem_path_pds.jpg
│   └── sem_path_anxdep.jpg
└── publication/
    └── Supplementary_Information.pdf
```

## Methodological Notes

- **Bootstrap choice.** With N=133 and a complex structural model, parametric SEs lean on multivariate-normality assumptions that are hard to justify for Likert-scale clinical data. Bias-corrected bootstrap with 5,000 draws trades compute time for inference that survives the distributional assumptions.
- **Interpretation of partial mediation.** The remaining direct path is significant but attenuated once the mediator is included — meaning dissociation accounts for part, not all, of the alcohol → symptom effect. Other unmodeled mechanisms remain.
- **Specificity.** The model was estimated on *all* substances jointly. Alcohol's mediation path is the only one that survives — so the result is not an artifact of reduced statistical power in a single-substance model.
- **Limitations.** Cross-sectional design (cannot establish temporal causality); self-report of substance use; single population.

## Citation

> Nacasch N., Malka T., Zohar J., **Dejorno Y.V.**, Levi G., Gross R., Weiser M., Cohen H. (2024). Impact of Pre-Trauma Recreational Drug Use on Mental Health Outcomes among Survivors of the Israeli Nova Festival Terrorist Attack. *World Psychiatry* 23(3): 452–454. [DOI: 10.1002/wps.21254](https://doi.org/10.1002/wps.21254)

## My Role

Primary statistical analyst. Designed the modeling progression (from multiple regression through SEM); implemented the lavaan models and bootstrap pipeline in R; produced all diagnostic tests (fit indices, modification indices, multicollinearity); generated the final path diagrams; co-authored the published manuscript.

## Contact

Yarden Viktor Dejorno — [hrsi56@gmail.com](mailto:hrsi56@gmail.com) · [LinkedIn](https://www.linkedin.com/in/yvdejorno) · Beer Sheva, Israel
