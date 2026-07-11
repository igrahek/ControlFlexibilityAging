# Recovery / transition-dynamics analyses (switch-repeat & intervals-since-switch)

Trial/interval-level analyses probing whether the varying-block undershoot is a genuine
**slow transition** through drift–threshold space (older adults moving slower) vs. a static
**in-between strategic setpoint**, and whether the block-level age effect holds at finer timescales.
See the companion GitHub issue for the full narrative summary and the list of final analyses.

## Models

| Model | Spec (v & a) | Data | Fitted output |
|---|---|---|---|
| Switch/repeat (binary) | `intervalType * Switch * Age` | varying blocks | `analyses/hssm/output/Model3` (this repo's numbering) |
| **Model 4 (intervals-since-switch)** | `intervalType * SinceSwitch * Age` | varying blocks | Oscar: `~/data/igrahek/aging_switch-repeat/analyses/hssm/output/Model4` |

`SinceSwitch` = intervals since the last goal switch (0 = switch interval, 1–4 capped, "4+").
Model 4 is a strict generalization of the switch/repeat model: `SinceSwitch == 0` ⟺ `Switch == "Switch"`.
It was fit on Oscar (partition `batch`, account `carney-frankmj-condo2`, env `pyHSSM_New_Nov24`).

## Scripts (run with the `pyHSSM_New_Nov24` python; they open only the `posterior` group with `engine="h5netcdf"`)

- `model4_fixed_effects.py` — population fixed-effects table for v and a (estimate, 95% CI, tail-p).
- `model4_convergence.py` — R-hat / ESS via `az.summary` (population and whole-model).
- `model4_recovery_slopes.py` — model-implied recovery slope (d[Speed↔Accuracy gap]/d[SinceSwitch]) at young (25) vs old (75), per parameter.
- `model4_distance_vs_sinceswitch.py` — Euclidean **distance in (v,a) space** vs SinceSwitch, young vs old; the transition-vs-setpoint test.
- `switchrepeat_model_fixed_effects.csv` — fixed-effects table for the binary switch/repeat model (reference).

Paths inside the scripts point at the Oscar Model 4 output; adjust `path=` for other machines/models.

## figures/
- `switchrepeat_trial_undershoot_vs_age.png` — trial-level (binary Switch) Euclidean undershoot vs age (left) and Repeat/Switch distance (right).
- `behavioral_recovery_curve.png` — model-free behavioral recovery: Speed↔Accuracy RT gap vs intervals-since-switch, with the fixed-block (settled) asymptote.

## Data provenance
`SinceSwitch` was attached to the canonical varying DDM file by `analyses/hssm/data/add_sinceswitch.R`
(binary switch boundary exact; ~0.14% of trials carry graded ambiguity). A full modern-tidyverse
re-run of `Preprocessing.R` does **not** reproduce the published `Switch` labels (~6% drift), so the
canonical data was preserved rather than regenerated. Run `Preprocessing.R` in the original tidyverse
for a fully exact regeneration.
