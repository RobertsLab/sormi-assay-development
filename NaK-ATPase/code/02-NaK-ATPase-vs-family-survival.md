02-NaK-ATPase-vs-family-survival
================
Steven Roberts
2026-10-02

-   [Overview](#overview)
-   [Data](#data)
    -   [Na/K-ATPase](#nak-atpase)
    -   [Survival](#survival)
-   [ATPase vs overall survival
    ranking](#atpase-vs-overall-survival-ranking)
-   [ATPase vs survival in each
    experiment](#atpase-vs-survival-in-each-experiment)

# Overview

Compare family-level gill Na<sup>+</sup>/K<sup>+</sup>-ATPase activity
(June 2026 sampling; see `01-NaK-ATPase-202606-sampling.Rmd`) with
family-level survival from the heat-survivorship cross-experiment
summary
(`heat-survivorship/code/09-mgig-survivorship-cross-experiment-summary.qmd`).

Only 9 families are shared, so correlations are descriptive.

# Data

## Na/K-ATPase

``` r
atpase <- read_tsv("../data/202606-sampling.tsv") %>%
  rename(ATPase = `ATPase (umol ADP/mg protein/hr)`)

sampling_log <- read_csv("../../sampling-event-metadata/june-2026/sampling-log.csv") %>%
  select(`Tube ID`, Condition, `Oyster Family ID`)

nak <- atpase %>%
  left_join(sampling_log, by = "Tube ID") %>%
  mutate(family = str_remove(`Oyster Family ID`, "Family "))

stopifnot(!any(is.na(nak$Condition)))
```

Family summaries: median activity under each condition (median is robust
to the failed M21 assay), and the heat response as the 36C − Ambient
difference.

``` r
fam_atpase <- nak %>%
  group_by(family, Condition) %>%
  summarise(median_ATPase = median(ATPase), .groups = "drop") %>%
  pivot_wider(names_from = Condition, values_from = median_ATPase) %>%
  mutate(delta_36C_minus_Ambient = `36C` - Ambient)

knitr::kable(fam_atpase, digits = 2)
```

| family |  36C | Ambient | delta\_36C\_minus\_Ambient |
|:-------|-----:|--------:|---------------------------:|
| 1      | 3.83 |    5.14 |                      -1.31 |
| 10     | 3.74 |    5.48 |                      -1.74 |
| 2      | 4.33 |    3.97 |                       0.36 |
| 3      | 4.64 |    5.44 |                      -0.81 |
| 5      | 3.56 |    3.38 |                       0.17 |
| 6      | 4.45 |    3.66 |                       0.79 |
| 7      | 4.14 |    4.27 |                      -0.13 |
| 8      | 3.42 |    3.84 |                      -0.42 |
| 9      | 3.19 |    4.01 |                      -0.82 |

## Survival

``` r
surv_dir <- "../../heat-survivorship/outputs/09-mgig-survivorship-cross-experiment-summary"

fam_rank <- read_csv(file.path(surv_dir, "family_ranking.csv"),
                     col_types = cols(family = col_character()))
fam_exp  <- read_csv(file.path(surv_dir, "family_by_experiment.csv"),
                     col_types = cols(family = col_character()))

fam <- fam_atpase %>% inner_join(fam_rank, by = "family")
nrow(fam)
```

    ## [1] 9

# ATPase vs overall survival ranking

`composite_score` is the mean within-experiment survival percentile
(0-100, higher = hardier) across all seven survival experiments.

``` r
fam_long <- fam %>%
  pivot_longer(c(Ambient, `36C`, delta_36C_minus_Ambient),
               names_to = "atpase_metric", values_to = "atpase_value") %>%
  mutate(atpase_metric = factor(atpase_metric,
                                levels = c("Ambient", "36C", "delta_36C_minus_Ambient"),
                                labels = c("Ambient", "36C", "36C - Ambient")))

overall_cor <- fam_long %>%
  group_by(atpase_metric) %>%
  summarise(rho = cor(atpase_value, composite_score, method = "spearman"),
            p   = cor.test(atpase_value, composite_score, method = "spearman", exact = TRUE)$p.value,
            .groups = "drop")

knitr::kable(overall_cor, digits = 3)
```

| atpase\_metric |    rho |     p |
|:---------------|-------:|------:|
| Ambient        | -0.533 | 0.148 |
| 36C            | -0.433 | 0.250 |
| 36C - Ambient  |  0.183 | 0.644 |

``` r
ggplot(fam_long, aes(x = atpase_value, y = composite_score)) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey60", linewidth = 0.5) +
  geom_point(size = 2) +
  geom_text(aes(label = family), nudge_y = 3, size = 3) +
  geom_text(data = overall_cor,
            aes(label = sprintf("rho = %.2f, p = %.2f", rho, p)),
            x = Inf, y = -Inf, hjust = 1.1, vjust = -0.8, size = 3, inherit.aes = FALSE) +
  facet_wrap(~ atpase_metric, scales = "free_x") +
  labs(x = "Median Na+/K+-ATPase activity (umol ADP / mg protein / hr)",
       y = "Composite survival score\n(higher = hardier)") +
  theme_bw()
```

![](02-NaK-ATPase-vs-family-survival_files/figure-gfm/overall-plot-1.png)<!-- -->

# ATPase vs survival in each experiment

Spearman correlation between family ATPase metrics and within-experiment
survival proportion. Experiments with few families (or near-total
mortality) carry little information.

``` r
exp_cor <- fam_exp %>%
  inner_join(fam_atpase, by = "family") %>%
  pivot_longer(c(Ambient, `36C`, delta_36C_minus_Ambient),
               names_to = "atpase_metric", values_to = "atpase_value") %>%
  group_by(exp_id, stressor, atpase_metric) %>%
  summarise(n_fam = n(),
            rho   = suppressWarnings(cor(atpase_value, surv_prop, method = "spearman")),
            .groups = "drop") %>%
  mutate(atpase_metric = factor(atpase_metric,
                                levels = c("Ambient", "36C", "delta_36C_minus_Ambient"),
                                labels = c("Ambient", "36C", "36C - Ambient")),
         exp_label = paste0(exp_id, " (n=", n_fam, ")"))

ggplot(exp_cor, aes(x = atpase_metric, y = exp_label, fill = rho)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = ifelse(is.na(rho), "NA", sprintf("%.2f", rho))), size = 3) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                       limits = c(-1, 1), na.value = "grey85") +
  labs(x = "Family ATPase metric", y = NULL, fill = "Spearman rho",
       title = "ATPase vs family survival proportion, by experiment") +
  theme_minimal()
```

![](02-NaK-ATPase-vs-family-survival_files/figure-gfm/per-experiment-1.png)<!-- -->
