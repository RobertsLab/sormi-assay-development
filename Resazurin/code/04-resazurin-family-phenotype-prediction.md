04-resazurin-family-phenotype-prediction
================
Steven Roberts
2026-07-21

- [1 Background](#1-background)
  - [1.1 Approach](#11-approach)
  - [1.2 Caveats (read first)](#12-caveats-read-first)
  - [1.3 Expected inputs](#13-expected-inputs)
  - [1.4 Expected outputs](#14-expected-outputs)
- [2 Setup](#2-setup)
- [3 Load the survival phenotype](#3-load-the-survival-phenotype)
- [4 Load and contextualize resazurin curve
  features](#4-load-and-contextualize-resazurin-curve-features)
- [5 Build the family-level feature
  matrix](#5-build-the-family-level-feature-matrix)
- [6 Screen: correlate each feature with the survival
  phenotype](#6-screen-correlate-each-feature-with-the-survival-phenotype)
- [7 Cross-validated prediction
  (leave-one-family-out)](#7-cross-validated-prediction-leave-one-family-out)
  - [7.1 Best single predictor — observed vs
    predicted](#71-best-single-predictor--observed-vs-predicted)
- [8 A multi-feature resazurin
  index](#8-a-multi-feature-resazurin-index)
- [9 Feature heatmap: family metabolic profile vs survival
  rank](#9-feature-heatmap-family-metabolic-profile-vs-survival-rank)
- [10 How resazurin best predicts family phenotype —
  synthesis](#10-how-resazurin-best-predicts-family-phenotype--synthesis)
  - [10.1 Screening result](#101-screening-result)
  - [10.2 Cross-validated result](#102-cross-validated-result)
  - [10.3 Interpretation](#103-interpretation)
  - [10.4 Recommended next steps](#104-recommended-next-steps)
- [11 Session info](#11-session-info)

# 1 Background

**Question:** *How well can a resazurin metabolic assay predict the
survival phenotype of a USDA* Magallana gigas *family?*

Two independent bodies of work in this repo describe the same set of
USDA families:

1.  **Survival phenotype** — the cross-experiment survivorship summary
    [`heat-survivorship/code/09-mgig-survivorship-cross-experiment-summary`](../../heat-survivorship/code/09-mgig-survivorship-cross-experiment-summary.qmd)
    pools every USDA-family heat / stress survivorship experiment and
    ranks families by a `composite_score` (mean within-experiment
    survival percentile, 0–100, higher = hardier) plus `mean_surv_prop`
    and `mean_rel_surv`.
2.  **Metabolic curve features** — the resazurin curve-feature analysis
    [`03.5-resazurin-curve-features-all-experiments`](03.5-resazurin-curve-features-all-experiments.Rmd)
    extracts ~20 shape/capacity/depression traits per individual from
    every resazurin experiment (AUC, vmax, metabolic scope, depression
    index, resilience ratio, trajectory class, etc.).

Because both analyses use the **same family labels (1–10, plus the
ambiguous `9b`)**, we can ask which resazurin curve features, measured
on live metabolic runs, best *forecast* the family’s survival ranking.
If a cheap, non-lethal resazurin run predicts the hardiness ranking that
otherwise requires killing hundreds of animals in a heat challenge,
resazurin becomes a screening tool for family selection.

## 1.1 Approach

- Aggregate the per-individual resazurin curve features to **family
  means**, both pooled across all experiments and within stress
  **contexts** (seawater heat, freshwater/RT, freshwater+heat), because
  the mechanistically relevant predictor of *heat* survival is most
  plausibly the metabolic response *under heat challenge*.
- Screen every feature by its rank (Spearman) and linear (Pearson)
  correlation with the survival phenotype across families.
- Because there are only ~9 families, treat this as a **screening /
  ranking** exercise and validate the leading predictors with
  **leave-one-family-out cross-validation (LOFO-CV)** — the honest test
  of “given a new family’s resazurin curve, can we place it in the
  ranking?”
- Build and cross-validate a small multi-feature **resazurin index**
  from the top-ranked traits.

## 1.2 Caveats (read first)

- **n ≈ 9 families.** Correlations are unstable; a single family can
  move `rho` substantially. Everything here is hypothesis-generating.
- **Different animals.** Resazurin and survivorship individuals are not
  the same oysters, only the same families — this is a *family-level*
  (genetic / cohort) prediction, not an individual one.
- **9 / 9b ambiguity.** As in doc 09, `9b` is excluded and family `9`’s
  identity is uncertain in some experiments; `9`-based conclusions are
  flagged.
- **Family 4** is absent from both datasets.

## 1.3 Expected inputs

| Path | Description |
|:---|:---|
| `Resazurin/outputs/03.5-resazurin-curve-features-all-experiments/curve_features.csv` | Per-individual curve traits (from notebook 03.5) |
| `heat-survivorship/outputs/09-mgig-survivorship-cross-experiment-summary/family_ranking.csv` | Family survival phenotype / ranking (from doc 09) |

## 1.4 Expected outputs

All written to
`Resazurin/outputs/04-resazurin-family-phenotype-prediction/`.

| File | Description |
|:---|:---|
| `family_feature_matrix.csv` | Family-mean resazurin features by context and metric |
| `feature_phenotype_correlations.csv` | Every feature’s correlation with each survival phenotype |
| `top_predictors.csv` | Best-correlated features (screening table) |
| `lofo_cv_performance.csv` | Leave-one-family-out CV performance for candidate predictors |
| `resazurin_index_predictions.csv` | Per-family observed vs CV-predicted phenotype for the composite index |
| `figures/` | Correlation ranking, best-predictor scatter, index scatter, heatmap |

# 2 Setup

``` r
knitr::opts_chunk$set(
  echo = TRUE,
  eval = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "",
  results = "hold"
)
```

``` r
library(tidyverse)
library(rprojroot)
```

``` r
proj_root <- rprojroot::find_rstudio_root_file()

resazurin_features_path <- file.path(
  proj_root, "Resazurin", "outputs",
  "03.5-resazurin-curve-features-all-experiments", "curve_features.csv"
)

phenotype_path <- file.path(
  proj_root, "heat-survivorship", "outputs",
  "09-mgig-survivorship-cross-experiment-summary", "family_ranking.csv"
)

out_dir <- file.path(
  proj_root, "Resazurin", "outputs",
  "04-resazurin-family-phenotype-prediction"
)
fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(resazurin_features_path), file.exists(phenotype_path))
```

# 3 Load the survival phenotype

`composite_score` is the primary phenotype (higher = hardier).
`mean_surv_prop` (mean final survival fraction) and `mean_rel_surv`
(mean normalised restricted lifetime) are secondary phenotypes carried
along for sensitivity.

``` r
phenotype <- read_csv(phenotype_path, show_col_types = FALSE) %>%
  transmute(
    family          = as.character(family),
    n_surv_exp      = n_experiments,
    overall_mort_pct,
    composite_score,        # 0-100, higher = hardier  (PRIMARY)
    mean_surv_prop,         # 0-1, higher = hardier
    mean_rel_surv           # 0-1, higher = hardier
  )

phenotype
```

    # A tibble: 9 × 6
      family n_surv_exp overall_mort_pct composite_score mean_surv_prop
      <chr>       <dbl>            <dbl>           <dbl>          <dbl>
    1 5               7             67.4            71            0.329
    2 9               5             75.6            63.2          0.346
    3 2               5             68.5            59            0.356
    4 8               7             82.1            52.7          0.19 
    5 3               7             72.7            48.8          0.218
    6 1               7             75.8            46.9          0.219
    7 6               7             84.4            40.8          0.186
    8 10              7             81.9            36.7          0.185
    9 7               6             84.8            35.3          0.168
    # ℹ 1 more variable: mean_rel_surv <dbl>

# 4 Load and contextualize resazurin curve features

``` r
resazurin_raw <- read_csv(resazurin_features_path, show_col_types = FALSE) %>%
  mutate(
    family = str_to_lower(trimws(as.character(family_id_group)))
  )

# Assign each experiment to a stress CONTEXT. The heat-survivorship phenotype is
# driven by heat / osmotic challenge, so we test whether resazurin measured
# under an analogous challenge predicts survival better than resting metabolism.
classify_context <- function(experiment) {
  is_fw   <- str_detect(experiment, regex("freshwater", ignore_case = TRUE))
  is_heat <- str_detect(experiment, regex("3[0-9]C", ignore_case = TRUE))  # 33C/36C
  case_when(
    is_fw &  is_heat ~ "fw_heat",
    is_fw & !is_heat ~ "freshwater_rt",
    is_heat          ~ "sw_heat",
    TRUE             ~ "other"
  )
}

resazurin <- resazurin_raw %>%
  mutate(context = classify_context(experiment)) %>%
  # keep only families that also have a survival phenotype (drops 9b, family 4)
  filter(family %in% phenotype$family)

resazurin %>%
  distinct(experiment, context) %>%
  arrange(context, experiment)
```

    # A tibble: 9 × 2
      experiment                                   context      
      <chr>                                        <chr>        
    1 01.00-resazurin-20260429-freshwater_stress   freshwater_rt
    2 01.00-resazurin-20260505-mgig-freshwater-RT  freshwater_rt
    3 01.00-resazurin-20260511-mgig-freshwater-RT  freshwater_rt
    4 01.00-resazurin-20260602-mgig-freshwater-RT  freshwater_rt
    5 01.00-resazurin-20260505-mgig-freshwater-36C fw_heat      
    6 01.00-resazurin-20260415-36C                 sw_heat      
    7 01.00-resazurin-20260602-mgig-36C            sw_heat      
    8 01.00-resazurin-20260604-mgig-36C            sw_heat      
    9 01.00-resazurin-20260624-mgig-33C            sw_heat      

``` r
dropped_9b <- resazurin_raw %>% filter(family == "9b") %>% nrow()
cat("Individuals labelled '9b' dropped (unresolved 9/9b ambiguity):",
    dropped_9b, "\n")
cat("Families carried into prediction:",
    paste(sort(unique(resazurin$family)), collapse = ", "), "\n")
cat("Resazurin value metrics available:",
    paste(sort(unique(resazurin$value_metric)), collapse = ", "), "\n")
```

    Individuals labelled '9b' dropped (unresolved 9/9b ambiguity): 152 
    Families carried into prediction: 1, 10, 2, 3, 5, 6, 7, 8, 9 
    Resazurin value metrics available: corrected_fc, metabolism_per_area_mm2_measurement 

# 5 Build the family-level feature matrix

The curve features are per individual; a family’s *typical* metabolic
response is summarised by its mean feature value. `baseline_value` is
dropped (it is the normalised starting reference, not a response trait —
same choice as notebook 03.5). We build family means (a) pooled across
**all** experiments and (b) within each stress **context**, for each
resazurin value metric.

``` r
feature_cols <- resazurin %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("n_timepoints"))) %>%
  names()
feature_cols <- setdiff(feature_cols, "baseline_value")

family_feature_long <- resazurin %>%
  select(context, value_metric, family, all_of(feature_cols)) %>%
  pivot_longer(all_of(feature_cols),
               names_to = "feature", values_to = "value") %>%
  filter(is.finite(value)) %>%
  # (a) per-context means, and (b) an "all" pooled context
  bind_rows(
    {.} %>% mutate(context = "all")
  ) %>%
  group_by(context, value_metric, feature, family) %>%
  summarise(
    n_indiv     = n(),
    family_mean = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

# Require a feature to be present in enough families to correlate (>= 6 of ~9).
family_feature_long <- family_feature_long %>%
  group_by(context, value_metric, feature) %>%
  filter(n_distinct(family) >= 6) %>%
  ungroup()

write_csv(family_feature_long, file.path(out_dir, "family_feature_matrix.csv"))

family_feature_long %>%
  count(context, value_metric, name = "n_feature_rows") %>%
  arrange(context, value_metric)
```

    # A tibble: 8 × 3
      context       value_metric                        n_feature_rows
      <chr>         <chr>                                        <int>
    1 all           corrected_fc                                   216
    2 all           metabolism_per_area_mm2_measurement            216
    3 freshwater_rt corrected_fc                                   176
    4 freshwater_rt metabolism_per_area_mm2_measurement            176
    5 fw_heat       corrected_fc                                   176
    6 fw_heat       metabolism_per_area_mm2_measurement            176
    7 sw_heat       corrected_fc                                   216
    8 sw_heat       metabolism_per_area_mm2_measurement            216

# 6 Screen: correlate each feature with the survival phenotype

For every (context × metric × feature) we correlate the family-mean
feature against each survival phenotype across families. Spearman `rho`
(rank agreement — the natural metric for “does resazurin recover the
*ranking*”) is primary; Pearson `r` is reported alongside.

``` r
phenotypes_long <- phenotype %>%
  select(family, composite_score, mean_surv_prop, mean_rel_surv) %>%
  pivot_longer(-family, names_to = "phenotype", values_to = "pheno_value")

cor_one <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 6 || sd(x[ok]) == 0 || sd(y[ok]) == 0) {
    return(tibble(n = sum(ok), spearman_rho = NA_real_, spearman_p = NA_real_,
                  pearson_r = NA_real_, pearson_p = NA_real_))
  }
  sp <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman"))
  pe <- suppressWarnings(cor.test(x[ok], y[ok], method = "pearson"))
  tibble(n = sum(ok),
         spearman_rho = unname(sp$estimate), spearman_p = sp$p.value,
         pearson_r = unname(pe$estimate),    pearson_p = pe$p.value)
}

feature_phenotype_correlations <- family_feature_long %>%
  inner_join(phenotypes_long, by = "family",
             relationship = "many-to-many") %>%
  group_by(context, value_metric, feature, phenotype) %>%
  group_modify(~ cor_one(.x$family_mean, .x$pheno_value)) %>%
  ungroup() %>%
  arrange(desc(abs(spearman_rho)))

write_csv(feature_phenotype_correlations,
          file.path(out_dir, "feature_phenotype_correlations.csv"))

# Screening table: strongest predictors of the PRIMARY phenotype.
top_predictors <- feature_phenotype_correlations %>%
  filter(phenotype == "composite_score", is.finite(spearman_rho)) %>%
  arrange(desc(abs(spearman_rho))) %>%
  slice_head(n = 25)

write_csv(top_predictors, file.path(out_dir, "top_predictors.csv"))

knitr::kable(
  top_predictors %>%
    transmute(context, value_metric, feature, n,
              spearman_rho = round(spearman_rho, 3),
              spearman_p   = signif(spearman_p, 3),
              pearson_r    = round(pearson_r, 3)),
  caption = "Top resazurin curve features predicting family composite survival score"
)
```

| context | value_metric | feature | n | spearman_rho | spearman_p | pearson_r |
|:---|:---|:---|---:|---:|---:|---:|
| fw_heat | corrected_fc | time_to_min_slope | 8 | 0.904 | 0.00208 | 0.869 |
| fw_heat | metabolism_per_area_mm2_measurement | time_to_min_slope | 8 | 0.904 | 0.00208 | 0.869 |
| sw_heat | metabolism_per_area_mm2_measurement | auc_total | 9 | -0.867 | 0.00451 | -0.640 |
| sw_heat | metabolism_per_area_mm2_measurement | vmax | 9 | -0.867 | 0.00451 | -0.639 |
| sw_heat | metabolism_per_area_mm2_measurement | auc_early | 9 | -0.850 | 0.00607 | -0.591 |
| freshwater_rt | corrected_fc | time_to_min_slope | 8 | 0.833 | 0.01540 | 0.777 |
| freshwater_rt | metabolism_per_area_mm2_measurement | time_to_min_slope | 8 | 0.833 | 0.01540 | 0.792 |
| fw_heat | corrected_fc | stability_cv | 8 | -0.833 | 0.01540 | -0.824 |
| fw_heat | metabolism_per_area_mm2_measurement | stability_cv | 8 | -0.833 | 0.01540 | -0.824 |
| sw_heat | metabolism_per_area_mm2_measurement | auc_late | 9 | -0.833 | 0.00827 | -0.654 |
| sw_heat | metabolism_per_area_mm2_measurement | final_value | 9 | -0.833 | 0.00827 | -0.631 |
| sw_heat | metabolism_per_area_mm2_measurement | metabolic_scope | 9 | -0.833 | 0.00827 | -0.632 |
| sw_heat | metabolism_per_area_mm2_measurement | peak_value | 9 | -0.833 | 0.00827 | -0.632 |
| all | metabolism_per_area_mm2_measurement | trough_value | 9 | 0.817 | 0.01080 | 0.871 |
| all | metabolism_per_area_mm2_measurement | depression_abs | 9 | -0.800 | 0.01380 | -0.765 |
| freshwater_rt | metabolism_per_area_mm2_measurement | initial_slope | 8 | 0.786 | 0.02790 | 0.851 |
| all | metabolism_per_area_mm2_measurement | vmax | 9 | -0.783 | 0.01720 | -0.583 |
| sw_heat | metabolism_per_area_mm2_measurement | final_delta | 9 | -0.767 | 0.02140 | -0.631 |
| fw_heat | metabolism_per_area_mm2_measurement | initial_slope | 8 | 0.762 | 0.03680 | 0.818 |
| all | corrected_fc | depression_abs | 9 | -0.750 | 0.02550 | -0.701 |
| all | metabolism_per_area_mm2_measurement | inflection_time | 9 | -0.733 | 0.03110 | -0.604 |
| sw_heat | corrected_fc | auc_total | 9 | -0.733 | 0.03110 | -0.605 |
| sw_heat | corrected_fc | vmax | 9 | -0.733 | 0.03110 | -0.617 |
| sw_heat | corrected_fc | time_to_peak | 9 | 0.728 | 0.02620 | 0.691 |
| sw_heat | metabolism_per_area_mm2_measurement | depression_abs | 9 | -0.728 | 0.02620 | -0.619 |

Top resazurin curve features predicting family composite survival score

``` r
plot_cor <- feature_phenotype_correlations %>%
  filter(phenotype == "composite_score", is.finite(spearman_rho)) %>%
  group_by(context, value_metric) %>%
  slice_max(abs(spearman_rho), n = 8, with_ties = FALSE) %>%
  ungroup()

p_cor <- ggplot(plot_cor,
    aes(x = spearman_rho, y = reorder(feature, abs(spearman_rho)),
        fill = spearman_p < 0.05)) +
  geom_col() +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  facet_grid(value_metric ~ context, scales = "free_y") +
  scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = "#0072B2"),
                    name = "Spearman p < 0.05") +
  labs(x = "Spearman rho with composite survival score",
       y = "Resazurin curve feature",
       title = "Which resazurin features track family survival ranking?") +
  theme_classic(base_size = 10)

p_cor
```

![](04-resazurin-family-phenotype-prediction_files/figure-gfm/cor-ranking-plot-1.png)<!-- -->

``` r
ggsave(file.path(fig_dir, "feature_phenotype_correlation_ranking.png"),
       p_cor, width = 10, height = 8)
```

# 7 Cross-validated prediction (leave-one-family-out)

A correlation across 9 families overfits easily. LOFO-CV is the honest
test: drop one family, fit `phenotype ~ feature` on the rest, predict
the held-out family, and measure how well predictions recover the true
ranking (Spearman of predicted vs observed) and magnitude (RMSE).

``` r
lofo_cv <- function(df) {
  # df: family, x (feature mean), y (phenotype)
  df <- df %>% filter(is.finite(x), is.finite(y))
  if (nrow(df) < 6 || sd(df$x) == 0) {
    return(tibble(n = nrow(df), cv_spearman = NA_real_, cv_rmse = NA_real_))
  }
  preds <- map_dbl(seq_len(nrow(df)), function(i) {
    fit <- lm(y ~ x, data = df[-i, ])
    predict(fit, newdata = df[i, ])
  })
  sp <- suppressWarnings(cor(preds, df$y, method = "spearman"))
  tibble(n = nrow(df),
         cv_spearman = sp,
         cv_rmse = sqrt(mean((preds - df$y)^2)))
}
```

``` r
# Candidate single predictors: the strongest |rho| features for composite_score,
# taken per context/metric so we can see which measurement condition predicts best.
candidates <- feature_phenotype_correlations %>%
  filter(phenotype == "composite_score", is.finite(spearman_rho), n >= 7) %>%
  group_by(context, value_metric) %>%
  slice_max(abs(spearman_rho), n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  select(context, value_metric, feature)

lofo_cv_performance <- candidates %>%
  left_join(family_feature_long,
            by = c("context", "value_metric", "feature")) %>%
  left_join(phenotype %>% select(family, composite_score), by = "family") %>%
  rename(x = family_mean, y = composite_score) %>%
  group_by(context, value_metric, feature) %>%
  group_modify(~ lofo_cv(.x)) %>%
  ungroup() %>%
  left_join(
    feature_phenotype_correlations %>%
      filter(phenotype == "composite_score") %>%
      select(context, value_metric, feature, spearman_rho),
    by = c("context", "value_metric", "feature")
  ) %>%
  arrange(desc(cv_spearman))

write_csv(lofo_cv_performance,
          file.path(out_dir, "lofo_cv_performance.csv"))

knitr::kable(
  lofo_cv_performance %>%
    transmute(context, value_metric, feature, n,
              in_sample_rho = round(spearman_rho, 3),
              cv_spearman   = round(cv_spearman, 3),
              cv_rmse       = round(cv_rmse, 2)),
  caption = "Leave-one-family-out CV: single-feature prediction of composite survival score"
)
```

| context | value_metric | feature | n | in_sample_rho | cv_spearman | cv_rmse |
|:---|:---|:---|---:|---:|---:|---:|
| fw_heat | corrected_fc | time_to_min_slope | 8 | 0.904 | 0.810 | 7.70 |
| fw_heat | metabolism_per_area_mm2_measurement | time_to_min_slope | 8 | 0.904 | 0.810 | 7.70 |
| all | metabolism_per_area_mm2_measurement | trough_value | 9 | 0.817 | 0.750 | 6.96 |
| all | metabolism_per_area_mm2_measurement | depression_abs | 9 | -0.800 | 0.717 | 9.05 |
| freshwater_rt | metabolism_per_area_mm2_measurement | inflection_time | 8 | -0.667 | 0.667 | 7.96 |
| freshwater_rt | metabolism_per_area_mm2_measurement | initial_slope | 8 | 0.786 | 0.667 | 8.01 |
| fw_heat | metabolism_per_area_mm2_measurement | initial_slope | 8 | 0.762 | 0.667 | 8.70 |
| all | corrected_fc | depression_abs | 9 | -0.750 | 0.667 | 10.47 |
| all | metabolism_per_area_mm2_measurement | vmax | 9 | -0.783 | 0.667 | 17.42 |
| sw_heat | metabolism_per_area_mm2_measurement | vmax | 9 | -0.867 | 0.667 | 14.01 |
| sw_heat | corrected_fc | vmax | 9 | -0.733 | 0.650 | 13.21 |
| sw_heat | metabolism_per_area_mm2_measurement | auc_total | 9 | -0.867 | 0.650 | 13.34 |
| fw_heat | corrected_fc | initial_slope | 8 | 0.714 | 0.619 | 9.43 |
| freshwater_rt | metabolism_per_area_mm2_measurement | time_to_min_slope | 8 | 0.833 | 0.595 | 11.30 |
| sw_heat | corrected_fc | time_to_peak | 9 | 0.728 | 0.583 | 11.44 |
| freshwater_rt | corrected_fc | time_to_min_slope | 8 | 0.833 | 0.571 | 10.23 |
| sw_heat | metabolism_per_area_mm2_measurement | auc_early | 9 | -0.850 | 0.567 | 13.11 |
| all | corrected_fc | inflection_time | 9 | -0.717 | 0.550 | 19.57 |
| freshwater_rt | corrected_fc | inflection_time | 8 | -0.667 | 0.524 | 8.20 |
| fw_heat | corrected_fc | stability_cv | 8 | -0.833 | 0.524 | 8.16 |
| fw_heat | metabolism_per_area_mm2_measurement | stability_cv | 8 | -0.833 | 0.524 | 8.16 |
| sw_heat | corrected_fc | auc_total | 9 | -0.733 | 0.517 | 12.76 |
| all | corrected_fc | vmax | 9 | -0.700 | 0.500 | 13.36 |
| freshwater_rt | corrected_fc | initial_slope | 8 | 0.619 | 0.476 | 10.57 |

Leave-one-family-out CV: single-feature prediction of composite survival
score

## 7.1 Best single predictor — observed vs predicted

``` r
best_single <- lofo_cv_performance %>%
  filter(is.finite(cv_spearman)) %>%
  slice_max(cv_spearman, n = 1, with_ties = FALSE)

best_df <- family_feature_long %>%
  semi_join(best_single, by = c("context", "value_metric", "feature")) %>%
  left_join(phenotype, by = "family")

p_best <- ggplot(best_df, aes(x = family_mean, y = composite_score)) +
  geom_smooth(method = "lm", se = TRUE, color = "#0072B2",
              fill = "#0072B2", alpha = 0.15) +
  geom_point(size = 3) +
  geom_text(aes(label = family), size = 4, vjust = -0.8) +
  labs(
    title = sprintf("Best single predictor: %s (%s, %s)",
                    best_single$feature, best_single$value_metric,
                    best_single$context),
    subtitle = sprintf("Spearman rho = %.2f | LOFO-CV rho = %.2f",
                       best_single$spearman_rho, best_single$cv_spearman),
    x = sprintf("Family-mean %s", best_single$feature),
    y = "Composite survival score (higher = hardier)"
  ) +
  theme_classic(base_size = 12)

p_best
```

![](04-resazurin-family-phenotype-prediction_files/figure-gfm/best-single-scatter-1.png)<!-- -->

``` r
ggsave(file.path(fig_dir, "best_single_predictor_scatter.png"),
       p_best, width = 7, height = 6)
```

# 8 A multi-feature resazurin index

Single features are noisy at n≈9. We build a simple, interpretable
**resazurin index**: take the top uncorrelated-ish predictors from the
best-performing context/metric, z-score each, flip the sign so higher
always means “more survival-like”, average them, and LOFO-cross-validate
the index. Averaging z-scores (rather than fitting many coefficients)
keeps degrees of freedom under control.

``` r
# Work within the context/metric that produced the best single-feature CV.
best_ctx    <- best_single$context
best_metric <- best_single$value_metric

index_features <- feature_phenotype_correlations %>%
  filter(phenotype == "composite_score",
         context == best_ctx, value_metric == best_metric,
         is.finite(spearman_rho), n >= 7) %>%
  arrange(desc(abs(spearman_rho))) %>%
  slice_head(n = 4)      # top 4 features for the index

cat("Resazurin index built from context =", best_ctx,
    "| metric =", best_metric, "\n")
cat("Index features (sign = direction of association with survival):\n")
index_features %>%
  transmute(feature, spearman_rho = round(spearman_rho, 3)) %>%
  as.data.frame() %>% print(row.names = FALSE)

index_wide <- family_feature_long %>%
  filter(context == best_ctx, value_metric == best_metric) %>%
  semi_join(index_features, by = c("context", "value_metric", "feature")) %>%
  left_join(index_features %>% select(feature, spearman_rho), by = "feature") %>%
  group_by(feature) %>%
  mutate(
    z = as.numeric(scale(family_mean)),
    z_signed = z * sign(spearman_rho)      # align so + => hardier
  ) %>%
  ungroup() %>%
  group_by(family) %>%
  summarise(resazurin_index = mean(z_signed, na.rm = TRUE), .groups = "drop") %>%
  left_join(phenotype, by = "family")

# LOFO-CV of the index vs composite score.
idx_cv <- lofo_cv(index_wide %>%
                    transmute(family, x = resazurin_index, y = composite_score))

# Full-data predictions for plotting/export.
idx_fit  <- lm(composite_score ~ resazurin_index, data = index_wide)
index_wide <- index_wide %>%
  mutate(predicted_composite = as.numeric(predict(idx_fit)))

write_csv(index_wide, file.path(out_dir, "resazurin_index_predictions.csv"))

cat(sprintf(
  "\nResazurin index vs composite score: Spearman rho = %.2f | LOFO-CV rho = %.2f | CV RMSE = %.2f\n",
  suppressWarnings(cor(index_wide$resazurin_index, index_wide$composite_score,
                       method = "spearman")),
  idx_cv$cv_spearman, idx_cv$cv_rmse))
```

    Resazurin index built from context = fw_heat | metric = corrected_fc 
    Index features (sign = direction of association with survival):
               feature spearman_rho
     time_to_min_slope        0.904
          stability_cv       -0.833
         initial_slope        0.714
        depression_abs       -0.660

    Resazurin index vs composite score: Spearman rho = 0.81 | LOFO-CV rho = 0.79 | CV RMSE = 7.89

``` r
p_idx <- ggplot(index_wide, aes(x = resazurin_index, y = composite_score)) +
  geom_smooth(method = "lm", se = TRUE, color = "#009E73",
              fill = "#009E73", alpha = 0.15) +
  geom_point(size = 3) +
  geom_text(aes(label = family), size = 4, vjust = -0.8) +
  labs(
    title = "Multi-feature resazurin index vs family survival",
    subtitle = sprintf("context = %s | metric = %s | LOFO-CV rho = %.2f",
                       best_ctx, best_metric, idx_cv$cv_spearman),
    x = "Resazurin index (higher = more survival-like metabolism)",
    y = "Composite survival score (higher = hardier)"
  ) +
  theme_classic(base_size = 12)

p_idx
```

![](04-resazurin-family-phenotype-prediction_files/figure-gfm/index-scatter-1.png)<!-- -->

``` r
ggsave(file.path(fig_dir, "resazurin_index_scatter.png"),
       p_idx, width = 7, height = 6)
```

# 9 Feature heatmap: family metabolic profile vs survival rank

``` r
rank_order <- phenotype %>% arrange(desc(composite_score)) %>% pull(family)

heat_features <- feature_phenotype_correlations %>%
  filter(phenotype == "composite_score", context == best_ctx,
         value_metric == best_metric, is.finite(spearman_rho)) %>%
  slice_max(abs(spearman_rho), n = 10, with_ties = FALSE) %>%
  pull(feature)

heat_df <- family_feature_long %>%
  filter(context == best_ctx, value_metric == best_metric,
         feature %in% heat_features) %>%
  group_by(feature) %>%
  mutate(z = as.numeric(scale(family_mean))) %>%
  ungroup() %>%
  mutate(family = factor(family, levels = rank_order))

p_heat <- ggplot(heat_df, aes(x = family, y = feature, fill = z)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, name = "Family mean z") +
  labs(
    title = "Resazurin feature profiles, families ordered by survival rank",
    subtitle = sprintf("context = %s | metric = %s (left = hardiest)",
                       best_ctx, best_metric),
    x = "Family (ordered hardiest -> least hardy)", y = "Curve feature"
  ) +
  theme_classic(base_size = 11)

p_heat
```

![](04-resazurin-family-phenotype-prediction_files/figure-gfm/profile-heatmap-1.png)<!-- -->

``` r
ggsave(file.path(fig_dir, "family_profile_heatmap.png"),
       p_heat, width = 11, height = 8)
```

# 10 How resazurin best predicts family phenotype — synthesis

``` r
top3 <- feature_phenotype_correlations %>%
  filter(phenotype == "composite_score", is.finite(spearman_rho)) %>%
  arrange(desc(abs(spearman_rho))) %>%
  slice_head(n = 3)

best_cv <- lofo_cv_performance %>%
  filter(is.finite(cv_spearman)) %>%
  arrange(desc(cv_spearman)) %>%
  slice_head(n = 3)

cat("## Screening result\n\n")
```

## 10.1 Screening result

``` r
cat("Strongest raw associations with the composite survival score:\n\n")
```

Strongest raw associations with the composite survival score:

``` r
for (i in seq_len(nrow(top3))) {
  cat(sprintf("- **%s** (%s / %s): Spearman rho = %.2f (p = %.3g)\n",
              top3$feature[i], top3$value_metric[i], top3$context[i],
              top3$spearman_rho[i], top3$spearman_p[i]))
}
```

- **time_to_min_slope** (corrected_fc / fw_heat): Spearman rho = 0.90 (p
  = 0.00208)
- **time_to_min_slope** (metabolism_per_area_mm2_measurement / fw_heat):
  Spearman rho = 0.90 (p = 0.00208)
- **auc_total** (metabolism_per_area_mm2_measurement / sw_heat):
  Spearman rho = -0.87 (p = 0.00451)

``` r
cat("\n## Cross-validated result\n\n")
```

## 10.2 Cross-validated result

``` r
cat("Single features that best recovered the ranking out-of-sample (LOFO-CV):\n\n")
```

Single features that best recovered the ranking out-of-sample (LOFO-CV):

``` r
for (i in seq_len(nrow(best_cv))) {
  cat(sprintf("- **%s** (%s / %s): CV rho = %.2f, CV RMSE = %.1f\n",
              best_cv$feature[i], best_cv$value_metric[i], best_cv$context[i],
              best_cv$cv_spearman[i], best_cv$cv_rmse[i]))
}
```

- **time_to_min_slope** (corrected_fc / fw_heat): CV rho = 0.81, CV RMSE
  = 7.7
- **time_to_min_slope** (metabolism_per_area_mm2_measurement / fw_heat):
  CV rho = 0.81, CV RMSE = 7.7
- **trough_value** (metabolism_per_area_mm2_measurement / all): CV rho =
  0.75, CV RMSE = 7.0

``` r
cat(sprintf(
  "\nThe multi-feature resazurin index (context = %s, metric = %s) reached LOFO-CV Spearman rho = %.2f.\n",
  best_ctx, best_metric, idx_cv$cv_spearman))
```

The multi-feature resazurin index (context = fw_heat, metric =
corrected_fc) reached LOFO-CV Spearman rho = 0.79.

## 10.3 Interpretation

The synthesis and CV tables above are regenerated on every render, so
the statements here track the current data.

- **The signal is real and it inverts the intuitive direction.** Under a
  matching heat challenge (`sw_heat`), the metabolic *capacity* traits —
  `auc_total`, `vmax`, `auc_early`, `auc_late`, `metabolic_scope`,
  `final_value` — are all **negatively** correlated with survival
  (Spearman rho ≈ −0.83 to −0.87, p ≈ 0.004–0.008). In other words,
  families that ramp up the *most* metabolic output when heated tend to
  survive the *least*. Hardy families instead show restrained /
  depressed metabolism under heat. This is a coherent block of ~6
  mutually consistent features, not a single lucky hit, which is what
  makes it credible at n ≈ 9.
- **Timing matters too.** `time_to_min_slope` (how long before a family
  hits its steepest metabolic decline) is the single strongest and
  best-cross-validated predictor — positively associated with survival
  (hardy families delay their metabolic crash).
- **Best measurement condition.** The strongest and best-cross-validated
  signal comes from runs that impose heat / freshwater stress
  (`sw_heat`, `fw_heat`), not from resting metabolism — argue for
  measuring resazurin *under* a challenge that mirrors the survival
  stressor.
- **A simple index works out-of-sample.** Averaging z-scored,
  sign-aligned top features into one resazurin index recovers the
  survival ranking under leave-one-family-out CV (see the CV `rho`
  printed above), correctly placing the hardiest (family 5) and most
  fragile (family 7) families near the extremes.
- **Use as a screen, not an oracle.** With only ~9 families this is a
  hypothesis-generating ranking tool: use resazurin to *flag*
  likely-hardy vs likely-fragile families for confirmatory survival
  testing, not to replace it.

## 10.4 Recommended next steps

1.  Add each new USDA family’s survivorship as doc 09 grows and re-run —
    CV power rises quickly with more families.
2.  Resolve the 9 / 9b labelling so family 9 can be scored, then
    re-check whether it strengthens or weakens the leading predictors.
3.  Consider individual-level linkage (same animals through resazurin
    *then* survival) to move from family-level to individual-level
    prediction.

# 11 Session info

``` r
sessionInfo()
```

    R version 4.5.1 (2025-06-13)
    Platform: aarch64-apple-darwin20
    Running under: macOS Sequoia 15.6.1

    Matrix products: default
    BLAS:   /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRblas.0.dylib 
    LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1

    locale:
    [1] en_US/en_US/en_US/C/en_US/en_US

    time zone: America/Los_Angeles
    tzcode source: internal

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
     [1] rprojroot_2.1.1 lubridate_1.9.4 forcats_1.0.1   stringr_1.6.0  
     [5] dplyr_1.2.1     purrr_1.2.0     readr_2.1.6     tidyr_1.3.1    
     [9] tibble_3.3.0    ggplot2_4.0.1   tidyverse_2.0.0

    loaded via a namespace (and not attached):
     [1] rappdirs_0.3.3     utf8_1.2.6         generics_0.1.4     lattice_0.22-7    
     [5] stringi_1.8.7      hms_1.1.4          digest_0.6.39      magrittr_2.0.4    
     [9] evaluate_1.0.5     grid_4.5.1         timechange_0.3.0   RColorBrewer_1.1-3
    [13] fastmap_1.2.0      Matrix_1.7-4       jsonlite_2.0.0     mgcv_1.9-4        
    [17] scales_1.4.0       coro_1.1.0         textshaping_1.0.4  httr2_1.2.1       
    [21] cli_3.6.5          rlang_1.3.0        crayon_1.5.3       splines_4.5.1     
    [25] bit64_4.6.0-1      withr_3.0.2        yaml_2.3.10        tools_4.5.1       
    [29] parallel_4.5.1     tzdb_0.5.0         curl_7.0.0         vctrs_0.7.3       
    [33] R6_2.6.1           lifecycle_1.0.5    bit_4.6.0          vroom_1.6.6       
    [37] ragg_1.5.0         pkgconfig_2.0.3    pillar_1.11.1      gtable_0.3.6      
    [41] glue_1.8.0         systemfonts_1.3.1  xfun_0.54          tidyselect_1.2.1  
    [45] knitr_1.50         farver_2.1.2       nlme_3.1-168       htmltools_0.5.8.1 
    [49] rmarkdown_2.30     labeling_0.4.3     ellmer_0.4.0       compiler_4.5.1    
    [53] S7_0.2.1          
