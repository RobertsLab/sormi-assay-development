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
-   [Within-family heat response vs
    survival](#within-family-heat-response-vs-survival)
-   [Family rankings: ATPase vs
    survival](#family-rankings-atpase-vs-survival)

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
to the failed M21 assay), and the heat response as the 36C - Ambient
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

# Within-family heat response vs survival

Does the change in ATPase between 36C and Ambient *within* a family
predict survival? The heat response is expressed three ways:

-   `diff`: difference in medians (36C - Ambient), as above
-   `log2ratio`: log2(median 36C / median Ambient), a relative change
-   `cohens_d`: standardized mean difference, scaling the change by
    within-family variability

`wilcox_p` tests whether 36C and Ambient differ within each family.

``` r
heat_resp <- nak %>%
  group_by(family) %>%
  summarise(
    diff      = median(ATPase[Condition == "36C"]) - median(ATPase[Condition == "Ambient"]),
    log2ratio = log2(median(ATPase[Condition == "36C"]) / median(ATPase[Condition == "Ambient"])),
    cohens_d  = (mean(ATPase[Condition == "36C"]) - mean(ATPase[Condition == "Ambient"])) /
      sqrt((var(ATPase[Condition == "36C"]) + var(ATPase[Condition == "Ambient"])) / 2),
    wilcox_p  = suppressWarnings(
      wilcox.test(ATPase[Condition == "36C"], ATPase[Condition == "Ambient"])$p.value),
    .groups = "drop"
  ) %>%
  inner_join(fam_rank, by = "family") %>%
  arrange(desc(composite_score))

heat_resp %>%
  select(family, composite_score, mean_surv_prop, diff, log2ratio, cohens_d, wilcox_p) %>%
  knitr::kable(digits = 3)
```

| family | composite\_score | mean\_surv\_prop |   diff | log2ratio | cohens\_d | wilcox\_p |
|:-------|-----------------:|-----------------:|-------:|----------:|----------:|----------:|
| 5      |             71.0 |            0.329 |  0.172 |     0.072 |     0.557 |     0.267 |
| 9      |             63.2 |            0.346 | -0.818 |    -0.329 |    -1.304 |     0.002 |
| 2      |             59.0 |            0.356 |  0.359 |     0.125 |     0.292 |     0.267 |
| 8      |             52.7 |            0.190 | -0.422 |    -0.168 |    -0.784 |     0.098 |
| 3      |             48.8 |            0.218 | -0.806 |    -0.231 |    -0.463 |     0.116 |
| 1      |             46.9 |            0.219 | -1.311 |    -0.425 |    -0.488 |     0.245 |
| 6      |             40.8 |            0.186 |  0.789 |     0.282 |     0.118 |     0.412 |
| 10     |             36.7 |            0.185 | -1.737 |    -0.550 |    -0.859 |     0.037 |
| 7      |             35.3 |            0.168 | -0.128 |    -0.044 |    -0.454 |     0.202 |

``` r
heat_long <- heat_resp %>%
  pivot_longer(c(diff, log2ratio, cohens_d),
               names_to = "response_metric", values_to = "response_value") %>%
  mutate(response_metric = factor(response_metric, levels = c("diff", "log2ratio", "cohens_d")))

heat_cor <- heat_long %>%
  pivot_longer(c(composite_score, mean_surv_prop),
               names_to = "survival_metric", values_to = "survival_value") %>%
  group_by(response_metric, survival_metric) %>%
  summarise(rho = cor(response_value, survival_value, method = "spearman"),
            p   = cor.test(response_value, survival_value, method = "spearman", exact = TRUE)$p.value,
            .groups = "drop")

knitr::kable(heat_cor, digits = 3)
```

| response\_metric | survival\_metric |   rho |     p |
|:-----------------|:-----------------|------:|------:|
| diff             | composite\_score | 0.183 | 0.644 |
| diff             | mean\_surv\_prop | 0.117 | 0.776 |
| log2ratio        | composite\_score | 0.183 | 0.644 |
| log2ratio        | mean\_surv\_prop | 0.117 | 0.776 |
| cohens\_d        | composite\_score | 0.167 | 0.678 |
| cohens\_d        | mean\_surv\_prop | 0.167 | 0.678 |

``` r
heat_lab <- filter(heat_cor, survival_metric == "composite_score")

ggplot(heat_long, aes(x = response_value, y = composite_score)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
  geom_smooth(method = "lm", se = FALSE, colour = "grey60", linewidth = 0.5) +
  geom_point(aes(shape = wilcox_p < 0.05), size = 2.5) +
  geom_text(aes(label = family), nudge_y = 3, size = 3) +
  geom_text(data = heat_lab,
            aes(label = sprintf("rho = %.2f, p = %.2f", rho, p)),
            x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5, size = 3, inherit.aes = FALSE) +
  scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 16),
                     labels = c(`FALSE` = "no", `TRUE` = "yes"),
                     name = "36C vs Ambient\np < 0.05") +
  facet_wrap(~ response_metric, scales = "free_x",
             labeller = as_labeller(c(diff = "36C - Ambient (median)",
                                      log2ratio = "log2(36C / Ambient)",
                                      cohens_d = "Cohen's d"))) +
  labs(x = "Within-family ATPase heat response",
       y = "Composite survival score\n(higher = hardier)") +
  theme_bw()
```

![](02-NaK-ATPase-vs-family-survival_files/figure-gfm/heat-response-plot-1.png)<!-- -->

The heat response does not track survival under any of the three metrics
(Spearman rho 0.12-0.18, all p &gt; 0.6). The two hardiest families (5,
2) held or slightly raised ATPase at 36C, but family 9 (second hardiest)
showed the clearest decline and family 6 (near the bottom) the largest
increase. Only families 9 and 10 changed significantly within family,
and they sit at opposite ends of the survival ranking.

# Family rankings: ATPase vs survival

Rank families (1 = highest) by survival `composite_score` and by three
ATPase measures: baseline (Ambient median), 36C median, and the relative
change at 36C (`log2ratio`, where rank 1 = largest gain). Spearman rho
is the correlation of these ranks; Kendall tau is a more conservative
rank-agreement measure for small samples.

``` r
fam_ranks <- heat_resp %>%
  select(family, composite_score, log2ratio) %>%
  left_join(fam_atpase %>% select(family, Ambient, `36C`), by = "family") %>%
  mutate(
    survival_rank = rank(-composite_score),
    ambient_rank  = rank(-Ambient),
    heat_rank     = rank(-`36C`),
    relative_rank = rank(-log2ratio)
  ) %>%
  arrange(survival_rank)

fam_ranks %>%
  select(family, survival_rank, composite_score, ambient_rank, Ambient,
         heat_rank, `36C`, relative_rank, log2ratio) %>%
  knitr::kable(digits = 2)
```

| family | survival\_rank | composite\_score | ambient\_rank | Ambient | heat\_rank |  36C | relative\_rank | log2ratio |
|:-------|---------------:|-----------------:|--------------:|--------:|-----------:|-----:|---------------:|----------:|
| 5      |              1 |             71.0 |             9 |    3.38 |          7 | 3.56 |              3 |      0.07 |
| 9      |              2 |             63.2 |             5 |    4.01 |          9 | 3.19 |              7 |     -0.33 |
| 2      |              3 |             59.0 |             6 |    3.97 |          3 | 4.33 |              2 |      0.12 |
| 8      |              4 |             52.7 |             7 |    3.84 |          8 | 3.42 |              5 |     -0.17 |
| 3      |              5 |             48.8 |             2 |    5.44 |          1 | 4.64 |              6 |     -0.23 |
| 1      |              6 |             46.9 |             3 |    5.14 |          5 | 3.83 |              8 |     -0.43 |
| 6      |              7 |             40.8 |             8 |    3.66 |          2 | 4.45 |              1 |      0.28 |
| 10     |              8 |             36.7 |             1 |    5.48 |          6 | 3.74 |              9 |     -0.55 |
| 7      |              9 |             35.3 |             4 |    4.27 |          4 | 4.14 |              4 |     -0.04 |

``` r
rank_long <- fam_ranks %>%
  pivot_longer(c(ambient_rank, heat_rank, relative_rank),
               names_to = "atpase_rank", values_to = "atpase_rank_value") %>%
  mutate(atpase_rank = factor(atpase_rank,
                              levels = c("ambient_rank", "heat_rank", "relative_rank"),
                              labels = c("Ambient (baseline)", "36C", "Relative change at 36C")))

rank_cor <- rank_long %>%
  group_by(atpase_rank) %>%
  summarise(
    spearman_rho = cor(atpase_rank_value, survival_rank, method = "spearman"),
    spearman_p   = cor.test(atpase_rank_value, survival_rank, method = "spearman", exact = TRUE)$p.value,
    kendall_tau  = cor(atpase_rank_value, survival_rank, method = "kendall"),
    kendall_p    = cor.test(atpase_rank_value, survival_rank, method = "kendall", exact = TRUE)$p.value,
    .groups = "drop"
  )

knitr::kable(rank_cor, digits = 3)
```

| atpase\_rank           | spearman\_rho | spearman\_p | kendall\_tau | kendall\_p |
|:-----------------------|--------------:|------------:|-------------:|-----------:|
| Ambient (baseline)     |        -0.533 |       0.148 |       -0.333 |      0.260 |
| 36C                    |        -0.433 |       0.250 |       -0.278 |      0.358 |
| Relative change at 36C |         0.183 |       0.644 |        0.167 |      0.612 |

``` r
rank_labels <- rank_cor %>%
  mutate(label = sprintf("%s\nrho = %.2f, tau = %.2f", atpase_rank, spearman_rho, kendall_tau)) %>%
  select(atpase_rank, label) %>%
  deframe()

ggplot(rank_long, aes(x = atpase_rank_value, y = survival_rank)) +
  geom_line(data = tibble(atpase_rank_value = 1:9, survival_rank = 1:9),
            linetype = "dashed", colour = "grey70") +
  geom_point(size = 2.5) +
  geom_text(aes(label = family), nudge_x = 0.3, nudge_y = -0.3, size = 3) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_reverse(breaks = 1:9) +
  facet_wrap(~ atpase_rank, labeller = as_labeller(rank_labels)) +
  labs(x = "ATPase rank (1 = highest)",
       y = "Survival rank (1 = hardiest)",
       caption = "Dashed line: identical ranks") +
  theme_bw()
```

![](02-NaK-ATPase-vs-family-survival_files/figure-gfm/rank-plot-1.png)<!-- -->

Baseline (Ambient) ATPase gives the strongest, and inverse, ranking
agreement: the three families with the highest baseline activity (10, 3,
1) rank 8th, 5th and 6th for survival, and the hardiest family (5) has
the lowest baseline. With 9 families this is not significant (Spearman p
= 0.15; Kendall p = 0.26). The 36C ranking is weaker in the same
direction, and the relative change at 36C does not align with survival.
