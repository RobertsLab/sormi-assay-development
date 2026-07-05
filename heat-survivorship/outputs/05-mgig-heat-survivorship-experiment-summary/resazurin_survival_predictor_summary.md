# Resazurin feature most associated with heat survival

Inputs:

- `Resazurin/outputs/03.5-resazurin-curve-features-all-experiments/curve_feature_family_summary.csv`
- `heat-survivorship/outputs/05-mgig-heat-survivorship-experiment-summary/family_survival_summary.csv`
- `heat-survivorship/outputs/05-mgig-heat-survivorship-experiment-summary/standardized_survival_data.csv`

Comparable experiment pairs used:

- Resazurin `01.00-resazurin-20260511-mgig-freshwater-RT` with heat survival `20260511 - 33 C`
- Resazurin `01.00-resazurin-20260604-mgig-36C` with heat survival `20260604 - 36 C`

Ambiguous 2/9 groups (`9` and `9B` where noted in the survivorship summary) were excluded.

## Main result

The strongest family-level predictor of poorer heat survival was the late-phase resazurin signal, especially `corrected_fc` `delta_auc_late_minus_early`.

Interpretation: families with a larger late increase in corrected resazurin fluorescence tended to have higher mortality and shorter median survival. In the directly date-matched 20260604 36 C comparison, `delta_auc_late_minus_early` had Spearman rho = 0.791 with death percent and rho = -0.756 with median survival time.

Across the two matched experiments after within-experiment z-scoring, the top features were:

| Rank | Value metric | Feature | Spearman rho with death percent |
|---:|---|---|---:|
| 1 | corrected_fc | delta_auc_late_minus_early | 0.594 |
| 2 | metabolism_per_area_mm2_measurement | stability_cv | 0.479 |
| 3 | corrected_fc | stability_cv | 0.479 |
| 4 | corrected_fc | auc_late | 0.453 |
| 5 | metabolism_per_area_mm2_measurement | vmax | 0.453 |

Closely related late-response measures in the 20260604 direct comparison also ranked highly: `auc_late`, `peak_value`, `metabolic_scope`, `final_value`, `final_delta`, and `auc_total`.

## Secondary model result

An individual-level Cox model with family-level corrected-fold-change features, stratified by heat experiment, ranked `initial_slope` highest by likelihood-ratio p-value:

- `initial_slope`: HR = 0.733 per within-experiment SD, LRT p = 0.00295, concordance = 0.657
- `delta_auc_late_minus_early`: HR = 1.31 per within-experiment SD, LRT p = 0.00336, concordance = 0.531

This suggests early conversion rate may contain survival-time information, but the more consistent family-summary signal is late resazurin accumulation.

## Caveats

This is a descriptive family-level comparison, not a validated predictive model. Only two resazurin/heat-survival experiment dates match cleanly, and the 20260511 survivorship experiment has little death-percent variation because almost all families reached 100% mortality. The clearest single-experiment evidence comes from the 20260604 36 C match.
