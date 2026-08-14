Gen5-20260813-mgig-sormi-BSA-F05-F07-reassay-protein
================
Sam White
2026-08-13

- [1 BACKGROUND](#1-background)
- [2 SETUP](#2-setup)
  - [2.1 Libraries](#21-libraries)
  - [2.2 Set output directory](#22-set-output-directory)
- [3 DATA IMPORT](#3-data-import)
- [4 RESHAPING](#4-reshaping)
- [5 MEAN VALUES](#5-mean-values)
  - [5.1 Mean 595 nm absorbance](#51-mean-595-nm-absorbance)
  - [5.2 Mean concentration](#52-mean-concentration)
- [6 STANDARD DEVIATION AND COEFFICIENT OF
  VARIATION](#6-standard-deviation-and-coefficient-of-variation)
  - [6.1 SD and CV of 595 nm
    absorbance](#61-sd-and-cv-of-595-nm-absorbance)
  - [6.2 SD and CV of concentration](#62-sd-and-cv-of-concentration)
  - [6.3 Combined per-group summary](#63-combined-per-group-summary)
- [7 CV-BASED EXCLUSION](#7-cv-based-exclusion)
- [8 STANDARD CURVE](#8-standard-curve)
  - [8.1 Plot standard curve](#81-plot-standard-curve)
- [9 APPLYING THE STANDARD CURVE TO
  SAMPLES](#9-applying-the-standard-curve-to-samples)
  - [9.1 Plot samples on standard
    curve](#91-plot-samples-on-standard-curve)
- [10 RESULTS TABLE](#10-results-table)
- [11 SUMMARY](#11-summary)
  - [11.1 Samples passing QC](#111-samples-passing-qc)
  - [11.2 Samples recommended for
    re-assay](#112-samples-recommended-for-re-assay)

# 1 BACKGROUND

Protein quantification (BCA/Bradford-style, 595 nm) of *Magallana gigas*
(Pacific oyster) SoRMI re-assay plate, 9 samples plus an 8-point BSA
standard curve, each measured in triplicate wells. All 9 samples are
re-assays of samples previously flagged for re-assay in the `F05` and
`F07` analyses (5 from `F05` – CV or standard-curve-range failures – and
4 from `F07` – CV failures), now run together on a single plate against
a fresh standard curve.

The raw Gen5 export
(`Gen5-20260813-mgig-BSA-F05-F07-reassay-absorbance.csv`) has the same
per-well layout as the earlier `F05`/`F07` exports (see those documents
for detail), but with one new wrinkle: sample names carry a
dilution-factor suffix, `<sample>-df.<N>`, e.g. `F05_01_ambient-df.0` or
`F05_03_ambient-df.2`. `df.0` means the well was read on **undiluted**
material; `df.N` (N \> 0) means the material was diluted **N-fold**
before reading, so the well’s back-calculated concentration must be
multiplied by `N` to recover the sample’s true (pre-dilution)
concentration – `df.0` is therefore treated as a multiplier of 1, not 0.
This multiplication is kept as a distinct, explicit step (see “Applying
the standard curve to samples” below) so the well-level (as-measured)
and final (dilution-corrected) concentrations both remain visible.

As before, this document ignores the instrument’s pre-computed summary
columns and recomputes mean, SD, and CV independently in R via `dplyr`,
for both the raw absorbance (595 nm) and the concentration.

**CV QC metric used for exclusion:** the \>15% technical-replicate CV
exclusion rule (including for standard-curve points) is applied to **CV
of concentration** (matching what the instrument’s own `CV (%)` column
already represents for this export – verified against the raw file:
e.g. `SPL1`’s given Mean/SD/CV of 883.966/50.075/5.665 are exactly the
mean/SD/CV of its three `[Concentration]` values, not of its `595`
values, and this holds regardless of that sample’s dilution factor since
the raw `[Concentration]` column reflects the as-measured,
pre-correction well concentration).

Two edge cases in the raw `[Concentration]` column need explicit
handling:

- **Censored reads.** Wells reading outside the instrument’s own
  internal standard-curve range are reported as `<0.000` or `>2100.000`
  rather than a number. These are parsed to a `_censored` flag plus the
  boundary value, and excluded from concentration mean/SD/CV
  calculations (their true value is unknown, not equal to the boundary).
- **The zero (blank) BSA standard** (`STD8`) is *always* censored
  (`<0.000` on all three wells), by construction – a true-zero
  standard’s back-calculated concentration is not a meaningful quantity
  to compute a CV on, and this is not a data-quality problem. `STD8` is
  therefore exempt from the concentration-CV exclusion rule and is
  retained as the curve’s origin point on the strength of its
  (excellent) 595 CV instead.

Any other group left with fewer than 2 valid (non-censored)
concentration replicates has an undefined (`NA`) concentration CV and is
treated as failing QC, consistent with the source file itself being
unable to compute a CV there either (`?????`). In this plate’s data,
every sample well yields a numeric (non-censored) concentration, so this
case only arises for the zero BSA standard (which is exempted, above).

# 2 SETUP

## 2.1 Libraries

``` r
library(knitr)
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)

knitr::opts_chunk$set(
  echo = TRUE,         # Display code chunks
  eval = TRUE,         # Evaluate code chunks
  warning = FALSE,     # Hide warnings
  message = FALSE,     # Hide messages
  comment = "",        # Prevents appending '##' to beginning of lines in code output
  results = 'hold'     # Holds output so it's all printed together after code chunk
)
```

## 2.2 Set output directory

``` r
output_dir <- "../outputs/Gen5-20260813-mgig-BSA-F05-F07-reassay"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
```

# 3 DATA IMPORT

The raw file is a Gen5 per-well export with non-syntactic column names
(`[Concentration]`, `CV (%)`, etc.) and a UTF-8 byte-order mark on the
first header cell, so columns are renamed by position immediately after
import rather than referenced by their original names.

``` r
data_path <- "../data/BSA/raw_absorbance/Gen5-20260813-mgig-BSA-F05-F07-reassay-absorbance.csv"

protein_raw <- read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE,
                         na.strings = c("", "NA"))
colnames(protein_raw) <- c("well_id", "name", "well", "std_conc_known", "abs_595",
                           "gen5_concentration", "gen5_count", "gen5_mean",
                           "gen5_sd", "gen5_cv")

cat("--- protein_raw: as imported from the Gen5 export ---\n")
str(protein_raw)
```

    --- protein_raw: as imported from the Gen5 export ---
    'data.frame':   51 obs. of  10 variables:
     $ well_id           : chr  "SPL1" NA NA "SPL2" ...
     $ name              : chr  "F05_01_ambient-df.0" NA NA "F05_02_ambient-df.0" ...
     $ well              : chr  "B1" "B2" "B3" "B4" ...
     $ std_conc_known    : int  NA NA NA NA NA NA NA NA NA NA ...
     $ abs_595           : num  1.14 1.18 1.19 1.22 1.28 ...
     $ gen5_concentration: chr  "827.572" "901.105" "923.221" "989.193" ...
     $ gen5_count        : int  3 NA NA 3 NA NA 3 NA NA 3 ...
     $ gen5_mean         : chr  "883.966" NA NA "1044.327" ...
     $ gen5_sd           : chr  "50.075" NA NA "56.98" ...
     $ gen5_cv           : chr  "5.665" NA NA "5.456" ...

# 4 RESHAPING

`well_id` and `name` are only populated on the first row of each
triplicate group in the raw export, so they are filled downward. Each
row is also classified as a `standard` (`well_id` starts with `STD`) or
a `sample` (`SPL`), and the instrument’s censored concentration strings
(`<0.000`, `>2100.000`) are parsed into a numeric value plus a
`gen5_concentration_censored` flag. For sample rows, the `-df.<N>`
suffix is parsed off `name` into a clean `base_sample_name`, plus
`dilution_code` (the raw `N`) and `dilution_multiplier` (1 when
`N == 0`, otherwise `N`).

``` r
protein_long <- protein_raw %>%
  select(well_id, name, well, std_conc_known, abs_595, gen5_concentration) %>%
  fill(well_id, name, .direction = "down") %>%
  mutate(
    type = if_else(grepl("^STD", well_id), "standard", "sample"),
    gen5_concentration_censored = grepl("^[<>]", gen5_concentration),
    gen5_concentration_numeric  = as.numeric(gsub("[<>]", "", gen5_concentration)),
    base_sample_name = if_else(type == "sample",
                                sub("-df\\.[0-9]+$", "", name),
                                NA_character_),
    dilution_code = if_else(type == "sample",
                             as.numeric(sub("^.*-df\\.([0-9]+)$", "\\1", name)),
                             NA_real_),
    dilution_multiplier = if_else(type == "sample",
                                   if_else(dilution_code == 0, 1, dilution_code),
                                   NA_real_)
  )

cat("--- protein_long: one row per well, group identifiers filled down, dilution parsed ---\n")
str(protein_long)
```

    --- protein_long: one row per well, group identifiers filled down, dilution parsed ---
    'data.frame':   51 obs. of  12 variables:
     $ well_id                    : chr  "SPL1" "SPL1" "SPL1" "SPL2" ...
     $ name                       : chr  "F05_01_ambient-df.0" "F05_01_ambient-df.0" "F05_01_ambient-df.0" "F05_02_ambient-df.0" ...
     $ well                       : chr  "B1" "B2" "B3" "B4" ...
     $ std_conc_known             : int  NA NA NA NA NA NA NA NA NA NA ...
     $ abs_595                    : num  1.14 1.18 1.19 1.22 1.28 ...
     $ gen5_concentration         : chr  "827.572" "901.105" "923.221" "989.193" ...
     $ type                       : chr  "sample" "sample" "sample" "sample" ...
     $ gen5_concentration_censored: logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
     $ gen5_concentration_numeric : num  828 901 923 989 1103 ...
     $ base_sample_name           : chr  "F05_01_ambient" "F05_01_ambient" "F05_01_ambient" "F05_02_ambient" ...
     $ dilution_code              : num  0 0 0 0 0 0 2 2 2 2 ...
     $ dilution_multiplier        : num  1 1 1 1 1 1 2 2 2 2 ...

# 5 MEAN VALUES

## 5.1 Mean 595 nm absorbance

``` r
mean_595_by_group <- protein_long %>%
  group_by(well_id, name, type, std_conc_known,
           base_sample_name, dilution_code, dilution_multiplier) %>%
  summarise(n_wells  = n(),
            mean_595 = mean(abs_595),
            .groups  = "drop")

cat("--- mean_595_by_group ---\n")
str(mean_595_by_group)
```

    --- mean_595_by_group ---
    tibble [17 × 9] (S3: tbl_df/tbl/data.frame)
     $ well_id            : chr [1:17] "SPL1" "SPL2" "SPL3" "SPL4" ...
     $ name               : chr [1:17] "F05_01_ambient-df.0" "F05_02_ambient-df.0" "F05_03_ambient-df.2" "F05_06_ambient-df.2" ...
     $ type               : chr [1:17] "sample" "sample" "sample" "sample" ...
     $ std_conc_known     : int [1:17] NA NA NA NA NA NA NA NA NA 2000 ...
     $ base_sample_name   : chr [1:17] "F05_01_ambient" "F05_02_ambient" "F05_03_ambient" "F05_06_ambient" ...
     $ dilution_code      : num [1:17] 0 0 2 2 2 0 0 0 0 NA ...
     $ dilution_multiplier: num [1:17] 1 1 2 2 2 1 1 1 1 NA ...
     $ n_wells            : int [1:17] 3 3 3 3 3 3 3 3 3 3 ...
     $ mean_595           : num [1:17] 1.17 1.25 1.2 1.29 1.41 ...

## 5.2 Mean concentration

Censored wells (reading outside the instrument’s own standard-curve
range) are dropped before averaging, since their true concentration is
unknown.

``` r
mean_conc_by_group <- protein_long %>%
  filter(!gen5_concentration_censored) %>%
  group_by(well_id, name, type, std_conc_known,
           base_sample_name, dilution_code, dilution_multiplier) %>%
  summarise(n_wells_valid_conc  = n(),
            mean_concentration  = mean(gen5_concentration_numeric),
            .groups = "drop")

cat("--- mean_conc_by_group ---\n")
str(mean_conc_by_group)
```

    --- mean_conc_by_group ---
    tibble [16 × 9] (S3: tbl_df/tbl/data.frame)
     $ well_id            : chr [1:16] "SPL1" "SPL2" "SPL3" "SPL4" ...
     $ name               : chr [1:16] "F05_01_ambient-df.0" "F05_02_ambient-df.0" "F05_03_ambient-df.2" "F05_06_ambient-df.2" ...
     $ type               : chr [1:16] "sample" "sample" "sample" "sample" ...
     $ std_conc_known     : int [1:16] NA NA NA NA NA NA NA NA NA 2000 ...
     $ base_sample_name   : chr [1:16] "F05_01_ambient" "F05_02_ambient" "F05_03_ambient" "F05_06_ambient" ...
     $ dilution_code      : num [1:16] 0 0 2 2 2 0 0 0 0 NA ...
     $ dilution_multiplier: num [1:16] 1 1 2 2 2 1 1 1 1 NA ...
     $ n_wells_valid_conc : int [1:16] 3 3 3 3 3 3 3 3 3 3 ...
     $ mean_concentration : num [1:16] 884 1044 956 1120 1353 ...

# 6 STANDARD DEVIATION AND COEFFICIENT OF VARIATION

## 6.1 SD and CV of 595 nm absorbance

``` r
sd_cv_595_by_group <- protein_long %>%
  group_by(well_id, name, type, std_conc_known,
           base_sample_name, dilution_code, dilution_multiplier) %>%
  summarise(
    n_wells  = n(),
    mean_595 = mean(abs_595),
    sd_595   = sd(abs_595),
    cv_595   = 100 * sd_595 / mean_595,
    .groups  = "drop"
  )

cat("--- sd_cv_595_by_group ---\n")
str(sd_cv_595_by_group)
```

    --- sd_cv_595_by_group ---
    tibble [17 × 11] (S3: tbl_df/tbl/data.frame)
     $ well_id            : chr [1:17] "SPL1" "SPL2" "SPL3" "SPL4" ...
     $ name               : chr [1:17] "F05_01_ambient-df.0" "F05_02_ambient-df.0" "F05_03_ambient-df.2" "F05_06_ambient-df.2" ...
     $ type               : chr [1:17] "sample" "sample" "sample" "sample" ...
     $ std_conc_known     : int [1:17] NA NA NA NA NA NA NA NA NA 2000 ...
     $ base_sample_name   : chr [1:17] "F05_01_ambient" "F05_02_ambient" "F05_03_ambient" "F05_06_ambient" ...
     $ dilution_code      : num [1:17] 0 0 2 2 2 0 0 0 0 NA ...
     $ dilution_multiplier: num [1:17] 1 1 2 2 2 1 1 1 1 NA ...
     $ n_wells            : int [1:17] 3 3 3 3 3 3 3 3 3 3 ...
     $ mean_595           : num [1:17] 1.17 1.25 1.2 1.29 1.41 ...
     $ sd_595             : num [1:17] 0.0267 0.03 0.0259 0.0375 0.0229 ...
     $ cv_595             : num [1:17] 2.29 2.4 2.15 2.91 1.62 ...

## 6.2 SD and CV of concentration

``` r
sd_cv_conc_by_group <- protein_long %>%
  filter(!gen5_concentration_censored) %>%
  group_by(well_id, name, type, std_conc_known,
           base_sample_name, dilution_code, dilution_multiplier) %>%
  summarise(
    n_wells_valid_conc  = n(),
    mean_concentration  = mean(gen5_concentration_numeric),
    sd_concentration    = sd(gen5_concentration_numeric),
    cv_concentration    = 100 * sd_concentration / mean_concentration,
    .groups = "drop"
  )

cat("--- sd_cv_conc_by_group ---\n")
str(sd_cv_conc_by_group)
```

    --- sd_cv_conc_by_group ---
    tibble [16 × 11] (S3: tbl_df/tbl/data.frame)
     $ well_id            : chr [1:16] "SPL1" "SPL2" "SPL3" "SPL4" ...
     $ name               : chr [1:16] "F05_01_ambient-df.0" "F05_02_ambient-df.0" "F05_03_ambient-df.2" "F05_06_ambient-df.2" ...
     $ type               : chr [1:16] "sample" "sample" "sample" "sample" ...
     $ std_conc_known     : int [1:16] NA NA NA NA NA NA NA NA NA 2000 ...
     $ base_sample_name   : chr [1:16] "F05_01_ambient" "F05_02_ambient" "F05_03_ambient" "F05_06_ambient" ...
     $ dilution_code      : num [1:16] 0 0 2 2 2 0 0 0 0 NA ...
     $ dilution_multiplier: num [1:16] 1 1 2 2 2 1 1 1 1 NA ...
     $ n_wells_valid_conc : int [1:16] 3 3 3 3 3 3 3 3 3 3 ...
     $ mean_concentration : num [1:16] 884 1044 956 1120 1353 ...
     $ sd_concentration   : num [1:16] 50.1 57 48.4 71.7 43.2 ...
     $ cv_concentration   : num [1:16] 5.66 5.46 5.06 6.4 3.19 ...

## 6.3 Combined per-group summary

``` r
group_stats <- sd_cv_595_by_group %>%
  left_join(
    sd_cv_conc_by_group %>%
      select(well_id, mean_concentration, sd_concentration, cv_concentration, n_wells_valid_conc),
    by = "well_id"
  ) %>%
  arrange(type, well_id)

cat("--- group_stats: one row per sample/standard, 595- and concentration-level stats ---\n")
str(group_stats)
kable(group_stats, digits = 3)
```

    --- group_stats: one row per sample/standard, 595- and concentration-level stats ---
    tibble [17 × 15] (S3: tbl_df/tbl/data.frame)
     $ well_id            : chr [1:17] "SPL1" "SPL2" "SPL3" "SPL4" ...
     $ name               : chr [1:17] "F05_01_ambient-df.0" "F05_02_ambient-df.0" "F05_03_ambient-df.2" "F05_06_ambient-df.2" ...
     $ type               : chr [1:17] "sample" "sample" "sample" "sample" ...
     $ std_conc_known     : int [1:17] NA NA NA NA NA NA NA NA NA 2000 ...
     $ base_sample_name   : chr [1:17] "F05_01_ambient" "F05_02_ambient" "F05_03_ambient" "F05_06_ambient" ...
     $ dilution_code      : num [1:17] 0 0 2 2 2 0 0 0 0 NA ...
     $ dilution_multiplier: num [1:17] 1 1 2 2 2 1 1 1 1 NA ...
     $ n_wells            : int [1:17] 3 3 3 3 3 3 3 3 3 3 ...
     $ mean_595           : num [1:17] 1.17 1.25 1.2 1.29 1.41 ...
     $ sd_595             : num [1:17] 0.0267 0.03 0.0259 0.0375 0.0229 ...
     $ cv_595             : num [1:17] 2.29 2.4 2.15 2.91 1.62 ...
     $ mean_concentration : num [1:17] 884 1044 956 1120 1353 ...
     $ sd_concentration   : num [1:17] 50.1 57 48.4 71.7 43.2 ...
     $ cv_concentration   : num [1:17] 5.66 5.46 5.06 6.4 3.19 ...
     $ n_wells_valid_conc : int [1:17] 3 3 3 3 3 3 3 3 3 3 ...

| well_id | name | type | std_conc_known | base_sample_name | dilution_code | dilution_multiplier | n_wells | mean_595 | sd_595 | cv_595 | mean_concentration | sd_concentration | cv_concentration | n_wells_valid_conc |
|:---|:---|:---|---:|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| SPL1 | F05_01_ambient-df.0 | sample | NA | F05_01_ambient | 0 | 1 | 3 | 1.166 | 0.027 | 2.287 | 883.966 | 50.075 | 5.665 | 3 |
| SPL2 | F05_02_ambient-df.0 | sample | NA | F05_02_ambient | 0 | 1 | 3 | 1.251 | 0.030 | 2.402 | 1044.327 | 56.980 | 5.456 | 3 |
| SPL3 | F05_03_ambient-df.2 | sample | NA | F05_03_ambient | 2 | 2 | 3 | 1.205 | 0.026 | 2.149 | 956.302 | 48.370 | 5.058 | 3 |
| SPL4 | F05_06_ambient-df.2 | sample | NA | F05_06_ambient | 2 | 2 | 3 | 1.291 | 0.038 | 2.907 | 1120.380 | 71.728 | 6.402 | 3 |
| SPL5 | F05_08_ambient-df.2 | sample | NA | F05_08_ambient | 2 | 2 | 3 | 1.414 | 0.023 | 1.619 | 1353.329 | 43.195 | 3.192 | 3 |
| SPL6 | F07_05_36C-df.0 | sample | NA | F07_05_36C | 0 | 1 | 3 | 0.932 | 0.008 | 0.810 | 441.760 | 13.784 | 3.120 | 3 |
| SPL7 | F07_01_ambient-df.0 | sample | NA | F07_01_ambient | 0 | 1 | 3 | 0.846 | 0.019 | 2.280 | 278.438 | 35.812 | 12.862 | 3 |
| SPL8 | F07_04_ambient-df.0 | sample | NA | F07_04_ambient | 0 | 1 | 3 | 1.001 | 0.026 | 2.551 | 571.561 | 48.935 | 8.562 | 3 |
| SPL9 | F07_06_ambient-df.0 | sample | NA | F07_06_ambient | 0 | 1 | 3 | 1.293 | 0.020 | 1.545 | 1123.972 | 37.008 | 3.293 | 3 |
| STD1 | BSA | standard | 2000 | NA | NA | NA | 3 | 1.620 | 0.017 | 1.059 | 1742.543 | 32.899 | 1.888 | 3 |
| STD2 | BSA | standard | 1500 | NA | NA | NA | 3 | 1.543 | 0.026 | 1.654 | 1596.044 | 48.516 | 3.040 | 3 |
| STD3 | BSA | standard | 1000 | NA | NA | NA | 3 | 1.374 | 0.011 | 0.772 | 1276.142 | 19.343 | 1.516 | 3 |
| STD4 | BSA | standard | 750 | NA | NA | NA | 3 | 1.146 | 0.029 | 2.553 | 845.908 | 55.390 | 6.548 | 3 |
| STD5 | BSA | standard | 500 | NA | NA | NA | 3 | 1.004 | 0.024 | 2.342 | 577.674 | 44.343 | 7.676 | 3 |
| STD6 | BSA | standard | 250 | NA | NA | NA | 3 | 0.811 | 0.009 | 1.054 | 212.656 | 16.562 | 7.788 | 3 |
| STD7 | BSA | standard | 125 | NA | NA | NA | 3 | 0.736 | 0.031 | 4.209 | 71.197 | 58.999 | 82.867 | 3 |
| STD8 | BSA | standard | 0 | NA | NA | NA | 3 | 0.594 | 0.014 | 2.297 | NA | NA | NA | NA |

# 7 CV-BASED EXCLUSION

Groups with CV(concentration) \> 15% – or an undefined concentration CV
due to insufficient valid (non-censored) replicates – are flagged and
excluded from all downstream analysis, including standard-curve fitting.
The zero BSA standard (`STD8`) is exempt from this rule (see Background)
and is always retained.

``` r
group_stats <- group_stats %>%
  mutate(
    is_zero_standard = type == "standard" & !is.na(std_conc_known) & std_conc_known == 0,
    high_cv = !is_zero_standard & (is.na(cv_concentration) | cv_concentration > 15)
  )

excluded_groups <- group_stats %>% filter(high_cv)

cat("--- excluded_groups: CV(concentration) > 15% (or undefined), dropped from further analysis ---\n")
str(excluded_groups)
kable(excluded_groups %>%
        select(well_id, name, type, std_conc_known, n_wells_valid_conc,
               mean_concentration, sd_concentration, cv_concentration),
      digits = 3)

clean_group_stats <- group_stats %>% filter(!high_cv)

cat("\nRetained after CV exclusion:", nrow(clean_group_stats), "of", nrow(group_stats), "groups\n")
str(clean_group_stats)
```

    --- excluded_groups: CV(concentration) > 15% (or undefined), dropped from further analysis ---
    tibble [1 × 17] (S3: tbl_df/tbl/data.frame)
     $ well_id            : chr "STD7"
     $ name               : chr "BSA"
     $ type               : chr "standard"
     $ std_conc_known     : int 125
     $ base_sample_name   : chr NA
     $ dilution_code      : num NA
     $ dilution_multiplier: num NA
     $ n_wells            : int 3
     $ mean_595           : num 0.736
     $ sd_595             : num 0.031
     $ cv_595             : num 4.21
     $ mean_concentration : num 71.2
     $ sd_concentration   : num 59
     $ cv_concentration   : num 82.9
     $ n_wells_valid_conc : int 3
     $ is_zero_standard   : logi FALSE
     $ high_cv            : logi TRUE

| well_id | name | type | std_conc_known | n_wells_valid_conc | mean_concentration | sd_concentration | cv_concentration |
|:---|:---|:---|---:|---:|---:|---:|---:|
| STD7 | BSA | standard | 125 | 3 | 71.197 | 58.999 | 82.867 |

    Retained after CV exclusion: 16 of 17 groups
    tibble [16 × 17] (S3: tbl_df/tbl/data.frame)
     $ well_id            : chr [1:16] "SPL1" "SPL2" "SPL3" "SPL4" ...
     $ name               : chr [1:16] "F05_01_ambient-df.0" "F05_02_ambient-df.0" "F05_03_ambient-df.2" "F05_06_ambient-df.2" ...
     $ type               : chr [1:16] "sample" "sample" "sample" "sample" ...
     $ std_conc_known     : int [1:16] NA NA NA NA NA NA NA NA NA 2000 ...
     $ base_sample_name   : chr [1:16] "F05_01_ambient" "F05_02_ambient" "F05_03_ambient" "F05_06_ambient" ...
     $ dilution_code      : num [1:16] 0 0 2 2 2 0 0 0 0 NA ...
     $ dilution_multiplier: num [1:16] 1 1 2 2 2 1 1 1 1 NA ...
     $ n_wells            : int [1:16] 3 3 3 3 3 3 3 3 3 3 ...
     $ mean_595           : num [1:16] 1.17 1.25 1.2 1.29 1.41 ...
     $ sd_595             : num [1:16] 0.0267 0.03 0.0259 0.0375 0.0229 ...
     $ cv_595             : num [1:16] 2.29 2.4 2.15 2.91 1.62 ...
     $ mean_concentration : num [1:16] 884 1044 956 1120 1353 ...
     $ sd_concentration   : num [1:16] 50.1 57 48.4 71.7 43.2 ...
     $ cv_concentration   : num [1:16] 5.66 5.46 5.06 6.4 3.19 ...
     $ n_wells_valid_conc : int [1:16] 3 3 3 3 3 3 3 3 3 3 ...
     $ is_zero_standard   : logi [1:16] FALSE FALSE FALSE FALSE FALSE FALSE ...
     $ high_cv            : logi [1:16] FALSE FALSE FALSE FALSE FALSE FALSE ...

# 8 STANDARD CURVE

Fit using only the retained (CV ≤ 15%) standard points: known BSA
concentration (`Conc/Dil`) on the x-axis, mean 595 nm absorbance on the
y-axis. Dilution correction (below) has no bearing on the curve fit
itself, since standards are not diluted.

``` r
standards_clean <- clean_group_stats %>% filter(type == "standard")

cat("--- standards_clean: standard points retained for curve fitting ---\n")
str(standards_clean)

standard_curve_model <- lm(mean_595 ~ std_conc_known, data = standards_clean)

curve_slope     <- unname(coef(standard_curve_model)[2])
curve_intercept <- unname(coef(standard_curve_model)[1])
curve_r2        <- summary(standard_curve_model)$r.squared

cat("Standard curve fit: 595 =", format(curve_slope, digits = 6), "* concentration +",
    format(curve_intercept, digits = 4), "\n")
cat("R2:", round(curve_r2, 5), "\n")
```

    --- standards_clean: standard points retained for curve fitting ---
    tibble [7 × 17] (S3: tbl_df/tbl/data.frame)
     $ well_id            : chr [1:7] "STD1" "STD2" "STD3" "STD4" ...
     $ name               : chr [1:7] "BSA" "BSA" "BSA" "BSA" ...
     $ type               : chr [1:7] "standard" "standard" "standard" "standard" ...
     $ std_conc_known     : int [1:7] 2000 1500 1000 750 500 250 0
     $ base_sample_name   : chr [1:7] NA NA NA NA ...
     $ dilution_code      : num [1:7] NA NA NA NA NA NA NA
     $ dilution_multiplier: num [1:7] NA NA NA NA NA NA NA
     $ n_wells            : int [1:7] 3 3 3 3 3 3 3
     $ mean_595           : num [1:7] 1.62 1.54 1.37 1.15 1 ...
     $ sd_595             : num [1:7] 0.0172 0.0255 0.0106 0.0293 0.0235 ...
     $ cv_595             : num [1:7] 1.059 1.654 0.772 2.553 2.342 ...
     $ mean_concentration : num [1:7] 1743 1596 1276 846 578 ...
     $ sd_concentration   : num [1:7] 32.9 48.5 19.3 55.4 44.3 ...
     $ cv_concentration   : num [1:7] 1.89 3.04 1.52 6.55 7.68 ...
     $ n_wells_valid_conc : int [1:7] 3 3 3 3 3 3 NA
     $ is_zero_standard   : logi [1:7] FALSE FALSE FALSE FALSE FALSE FALSE ...
     $ high_cv            : logi [1:7] FALSE FALSE FALSE FALSE FALSE FALSE ...
    Standard curve fit: 595 = 0.000522028 * concentration + 0.7086 
    R2: 0.93363 

## 8.1 Plot standard curve

``` r
eq_label <- paste0("y = ", format(curve_slope, digits = 4, scientific = TRUE), "x + ",
                    sprintf("%.4f", curve_intercept),
                    "\nR² = ", sprintf("%.4f", curve_r2))

standard_curve_plot <- ggplot(standards_clean, aes(x = std_conc_known, y = mean_595)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "steelblue", linewidth = 0.6) +
  geom_errorbar(aes(ymin = mean_595 - sd_595, ymax = mean_595 + sd_595),
                width = 30, color = "grey40") +
  geom_point(size = 2.6, color = "steelblue") +
  annotate("text", x = -Inf, y = Inf, label = eq_label,
           hjust = -0.08, vjust = 1.3, size = 3.6, fontface = "bold") +
  labs(x = "BSA standard concentration (µg/mL)",
       y = "Mean absorbance (595 nm)",
       title = "BSA standard curve",
       subtitle = "Points with technical-replicate CV(concentration) > 15% excluded (see below)") +
  theme_bw(base_size = 12)

print(standard_curve_plot)
```

![](Gen5-20260813-mgig-sormi-BSA-F05-F07-reassay-protein_files/figure-gfm/plot-standard-curve-1.png)<!-- -->

``` r
ggsave(file.path(output_dir, "standard_curve_595.png"), standard_curve_plot,
       width = 7, height = 5.5, dpi = 300)
```

Standard points excluded from the curve:

``` r
excluded_standards <- group_stats %>% filter(type == "standard", high_cv)

cat("--- excluded_standards ---\n")
str(excluded_standards)
kable(excluded_standards %>%
        select(well_id, std_conc_known, mean_concentration, sd_concentration, cv_concentration),
      digits = 3)
```

    --- excluded_standards ---
    tibble [1 × 17] (S3: tbl_df/tbl/data.frame)
     $ well_id            : chr "STD7"
     $ name               : chr "BSA"
     $ type               : chr "standard"
     $ std_conc_known     : int 125
     $ base_sample_name   : chr NA
     $ dilution_code      : num NA
     $ dilution_multiplier: num NA
     $ n_wells            : int 3
     $ mean_595           : num 0.736
     $ sd_595             : num 0.031
     $ cv_595             : num 4.21
     $ mean_concentration : num 71.2
     $ sd_concentration   : num 59
     $ cv_concentration   : num 82.9
     $ n_wells_valid_conc : int 3
     $ is_zero_standard   : logi FALSE
     $ high_cv            : logi TRUE

| well_id | std_conc_known | mean_concentration | sd_concentration | cv_concentration |
|:--------|---------------:|-------------------:|-----------------:|-----------------:|
| STD7    |            125 |             71.197 |           58.999 |           82.867 |

# 9 APPLYING THE STANDARD CURVE TO SAMPLES

Samples that passed CV QC are back-calculated against the fitted curve
to give a **well-level** concentration (`calculated_concentration_well`)
– the concentration of whatever material was actually in the well,
diluted or not. Multiplying by each sample’s `dilution_multiplier` then
gives the **final** concentration (`calculated_concentration`) – the
true concentration of the original, pre-dilution sample, which is the
value reported in the results below. Samples whose mean 595 falls
outside the retained standards’ absorbance range are flagged as
extrapolated; this range check is done on the as-measured 595 value and
is unaffected by dilution.

``` r
samples_clean <- clean_group_stats %>%
  filter(type == "sample") %>%
  mutate(
    se_595                         = sd_595 / sqrt(n_wells),
    calculated_concentration_well  = (mean_595 - curve_intercept) / curve_slope,
    calculated_concentration       = calculated_concentration_well * dilution_multiplier,
    in_curve_range                 = mean_595 >= min(standards_clean$mean_595) &
                                       mean_595 <= max(standards_clean$mean_595)
  ) %>%
  arrange(base_sample_name)

cat("--- samples_clean: CV-QC-passing samples, quantified against the fitted standard curve ---\n")
cat("    (calculated_concentration_well = as-measured; calculated_concentration = dilution-corrected)\n")
str(samples_clean)
```

    --- samples_clean: CV-QC-passing samples, quantified against the fitted standard curve ---
        (calculated_concentration_well = as-measured; calculated_concentration = dilution-corrected)
    tibble [9 × 21] (S3: tbl_df/tbl/data.frame)
     $ well_id                      : chr [1:9] "SPL1" "SPL2" "SPL3" "SPL4" ...
     $ name                         : chr [1:9] "F05_01_ambient-df.0" "F05_02_ambient-df.0" "F05_03_ambient-df.2" "F05_06_ambient-df.2" ...
     $ type                         : chr [1:9] "sample" "sample" "sample" "sample" ...
     $ std_conc_known               : int [1:9] NA NA NA NA NA NA NA NA NA
     $ base_sample_name             : chr [1:9] "F05_01_ambient" "F05_02_ambient" "F05_03_ambient" "F05_06_ambient" ...
     $ dilution_code                : num [1:9] 0 0 2 2 2 0 0 0 0
     $ dilution_multiplier          : num [1:9] 1 1 2 2 2 1 1 1 1
     $ n_wells                      : int [1:9] 3 3 3 3 3 3 3 3 3
     $ mean_595                     : num [1:9] 1.17 1.25 1.2 1.29 1.41 ...
     $ sd_595                       : num [1:9] 0.0267 0.03 0.0259 0.0375 0.0229 ...
     $ cv_595                       : num [1:9] 2.29 2.4 2.15 2.91 1.62 ...
     $ mean_concentration           : num [1:9] 884 1044 956 1120 1353 ...
     $ sd_concentration             : num [1:9] 50.1 57 48.4 71.7 43.2 ...
     $ cv_concentration             : num [1:9] 5.66 5.46 5.06 6.4 3.19 ...
     $ n_wells_valid_conc           : int [1:9] 3 3 3 3 3 3 3 3 3
     $ is_zero_standard             : logi [1:9] FALSE FALSE FALSE FALSE FALSE FALSE ...
     $ high_cv                      : logi [1:9] FALSE FALSE FALSE FALSE FALSE FALSE ...
     $ se_595                       : num [1:9] 0.0154 0.0173 0.0149 0.0217 0.0132 ...
     $ calculated_concentration_well: num [1:9] 876 1039 950 1116 1352 ...
     $ calculated_concentration     : num [1:9] 876 1039 1900 2232 2704 ...
     $ in_curve_range               : logi [1:9] TRUE TRUE TRUE TRUE TRUE TRUE ...

``` r
cat("Samples with absorbance outside the retained standard curve's range (extrapolated):\n")
kable(samples_clean %>% filter(!in_curve_range) %>%
        select(well_id, base_sample_name, mean_595, calculated_concentration),
      digits = 3)
```

    Samples with absorbance outside the retained standard curve's range (extrapolated):

well_id base_sample_name mean_595 calculated_concentration ——— ——————
———- ————————–

## 9.1 Plot samples on standard curve

Every QC-passing sample is plotted at its own mean 595 and
**well-level** back-calculated concentration (i.e. before dilution
correction, since that is what is directly comparable to the curve’s own
x/y relationship), with vertical error bars of ±1 standard error (of
mean 595, across its 2-3 retained technical replicates), overlaid on the
standard curve fit line and retained standard points.

``` r
samples_on_curve_plot <- ggplot() +
  geom_smooth(data = standards_clean, aes(x = std_conc_known, y = mean_595),
              method = "lm", formula = y ~ x, se = FALSE,
              color = "steelblue", linewidth = 0.6) +
  geom_point(data = standards_clean, aes(x = std_conc_known, y = mean_595),
             size = 2.6, color = "steelblue") +
  geom_errorbar(data = samples_clean,
                aes(x = calculated_concentration_well,
                    ymin = mean_595 - se_595, ymax = mean_595 + se_595),
                width = 30, color = "grey40") +
  geom_point(data = samples_clean,
             aes(x = calculated_concentration_well, y = mean_595, shape = in_curve_range),
             size = 2.4, color = "firebrick", alpha = 0.85) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 4),
                      labels = c(`TRUE` = "in range", `FALSE` = "extrapolated"),
                      name = "Sample status") +
  labs(x = "Well-level concentration (µg/mL, pre-dilution-correction)",
       y = "Mean absorbance (595 nm)",
       title = "BSA standard curve with QC-passing samples overlaid",
       subtitle = "Samples plotted at their well-level back-calculated concentration; error bars = ±1 SE of mean 595") +
  theme_bw(base_size = 12)

print(samples_on_curve_plot)
```

![](Gen5-20260813-mgig-sormi-BSA-F05-F07-reassay-protein_files/figure-gfm/plot-samples-on-curve-1.png)<!-- -->

``` r
ggsave(file.path(output_dir, "standard_curve_with_samples.png"), samples_on_curve_plot,
       width = 8, height = 5.5, dpi = 300)
```

# 10 RESULTS TABLE

``` r
results_table <- samples_clean %>%
  transmute(
    Sample                                   = base_sample_name,
    `Well ID`                                = well_id,
    `Dilution factor`                        = dilution_multiplier,
    `Mean 595`                               = round(mean_595, 3),
    `CV 595 (%)`                             = round(cv_595, 2),
    `CV concentration (%)`                   = round(cv_concentration, 2),
    `Well concentration (ug/mL)`             = round(calculated_concentration_well, 1),
    `Calculated concentration (ug/mL)`       = round(calculated_concentration, 1),
    `In standard curve range`                = ifelse(in_curve_range, "yes", "NO - extrapolated")
  ) %>%
  arrange(gsub("[0-9]+$", "", `Well ID`), as.numeric(gsub("^[A-Za-z]+", "", `Well ID`)))

kable(results_table)
```

| Sample | Well ID | Dilution factor | Mean 595 | CV 595 (%) | CV concentration (%) | Well concentration (ug/mL) | Calculated concentration (ug/mL) | In standard curve range |
|:---|:---|---:|---:|----|---:|---:|---:|:---|
| F05_01_ambient | SPL1 | 1 | 1.166 | 2.29 | 5.66 | 876.1 | 876.1 | yes |
| F05_02_ambient | SPL2 | 1 | 1.251 | 2.40 | 5.46 | 1038.9 | 1038.9 | yes |
| F05_03_ambient | SPL3 | 2 | 1.205 | 2.15 | 5.06 | 950.2 | 1900.4 | yes |
| F05_06_ambient | SPL4 | 2 | 1.291 | 2.91 | 6.40 | 1116.2 | 2232.4 | yes |
| F05_08_ambient | SPL5 | 2 | 1.414 | 1.62 | 3.19 | 1351.8 | 2703.7 | yes |
| F07_05_36C | SPL6 | 1 | 0.932 | 0.81 | 3.12 | 427.9 | 427.9 | yes |
| F07_01_ambient | SPL7 | 1 | 0.846 | 2.28 | 12.86 | 263.1 | 263.1 | yes |
| F07_04_ambient | SPL8 | 1 | 1.001 | 2.55 | 8.56 | 560.0 | 560.0 | yes |
| F07_06_ambient | SPL9 | 1 | 1.293 | 1.54 | 3.29 | 1119.4 | 1119.4 | yes |

Only samples passing *all* QC – CV ≤ 15% and within the retained
standard curve’s range (i.e. not extrapolated) – are written to the CSV
output.

``` r
results_table_export <- results_table %>% filter(`In standard curve range` == "yes")

cat("--- results_table_export: samples passing all QC (CV and in-range), written to CSV ---\n")
str(results_table_export)

write.csv(results_table_export, file.path(output_dir, "sample_protein_concentrations.csv"), row.names = FALSE)
```

    --- results_table_export: samples passing all QC (CV and in-range), written to CSV ---
    tibble [9 × 9] (S3: tbl_df/tbl/data.frame)
     $ Sample                          : chr [1:9] "F05_01_ambient" "F05_02_ambient" "F05_03_ambient" "F05_06_ambient" ...
     $ Well ID                         : chr [1:9] "SPL1" "SPL2" "SPL3" "SPL4" ...
     $ Dilution factor                 : num [1:9] 1 1 2 2 2 1 1 1 1
     $ Mean 595                        : num [1:9] 1.17 1.25 1.21 1.29 1.41 ...
     $ CV 595 (%)                      : num [1:9] 2.29 2.4 2.15 2.91 1.62 0.81 2.28 2.55 1.54
     $ CV concentration (%)            : num [1:9] 5.66 5.46 5.06 6.4 3.19 ...
     $ Well concentration (ug/mL)      : num [1:9] 876 1039 950 1116 1352 ...
     $ Calculated concentration (ug/mL): num [1:9] 876 1039 1900 2232 2704 ...
     $ In standard curve range         : chr [1:9] "yes" "yes" "yes" "yes" ...

# 11 SUMMARY

- 17 sample/standard triplicate groups were imported (8 BSA standards, 9
  samples).
- 1 group(s) exceeded 15% technical-replicate CV(concentration), or had
  an undefined CV from insufficient valid replicates, and were excluded
  from all downstream analysis: STD7 (BSA).
- The BSA standard curve was fit on the 7 remaining standard points:
  slope = 0.000522, intercept = 0.7086, R² = 0.9336.
- 0 CV-QC-passing sample(s) fell outside the retained standard curve’s
  absorbance range and are flagged as extrapolated in the results table
  above.

## 11.1 Samples passing QC

Sample names are sorted naturally (ignoring stray punctuation) rather
than by plain lexicographic string order. Concentrations shown are the
final, dilution-corrected values.

``` r
natural_sort_key <- function(x) gsub("[^A-Za-z0-9]", "", x)
```

``` r
passing_samples_table <- samples_clean %>%
  transmute(sample_name = base_sample_name,
            `concentration(ug/mL)` = round(calculated_concentration, 1)) %>%
  arrange(natural_sort_key(sample_name))

cat("--- passing_samples_table: samples passing CV QC, name + calculated concentration ---\n")
str(passing_samples_table)

kable(passing_samples_table)
```

    --- passing_samples_table: samples passing CV QC, name + calculated concentration ---
    tibble [9 × 2] (S3: tbl_df/tbl/data.frame)
     $ sample_name         : chr [1:9] "F05_01_ambient" "F05_02_ambient" "F05_03_ambient" "F05_06_ambient" ...
     $ concentration(ug/mL): num [1:9] 876 1039 1900 2232 2704 ...

| sample_name    | concentration(ug/mL) |
|:---------------|---------------------:|
| F05_01_ambient |                876.1 |
| F05_02_ambient |               1038.9 |
| F05_03_ambient |               1900.4 |
| F05_06_ambient |               2232.4 |
| F05_08_ambient |               2703.7 |
| F07_01_ambient |                263.1 |
| F07_04_ambient |                560.0 |
| F07_05_36C     |                427.9 |
| F07_06_ambient |               1119.4 |

## 11.2 Samples recommended for re-assay

``` r
failed_qc_samples    <- excluded_groups %>% filter(type == "sample") %>% arrange(base_sample_name)
extrapolated_samples <- samples_clean %>% filter(!in_curve_range) %>% arrange(base_sample_name)

cat("--- failed_qc_samples: samples excluded from analysis on CV grounds ---\n")
str(failed_qc_samples)
cat("\n--- extrapolated_samples: QC-passing samples outside the retained standard curve's range ---\n")
str(extrapolated_samples)
```

    --- failed_qc_samples: samples excluded from analysis on CV grounds ---
    tibble [0 × 17] (S3: tbl_df/tbl/data.frame)
     $ well_id            : chr(0) 
     $ name               : chr(0) 
     $ type               : chr(0) 
     $ std_conc_known     : int(0) 
     $ base_sample_name   : chr(0) 
     $ dilution_code      : num(0) 
     $ dilution_multiplier: num(0) 
     $ n_wells            : int(0) 
     $ mean_595           : num(0) 
     $ sd_595             : num(0) 
     $ cv_595             : num(0) 
     $ mean_concentration : num(0) 
     $ sd_concentration   : num(0) 
     $ cv_concentration   : num(0) 
     $ n_wells_valid_conc : int(0) 
     $ is_zero_standard   : logi(0) 
     $ high_cv            : logi(0) 

    --- extrapolated_samples: QC-passing samples outside the retained standard curve's range ---
    tibble [0 × 21] (S3: tbl_df/tbl/data.frame)
     $ well_id                      : chr(0) 
     $ name                         : chr(0) 
     $ type                         : chr(0) 
     $ std_conc_known               : int(0) 
     $ base_sample_name             : chr(0) 
     $ dilution_code                : num(0) 
     $ dilution_multiplier          : num(0) 
     $ n_wells                      : int(0) 
     $ mean_595                     : num(0) 
     $ sd_595                       : num(0) 
     $ cv_595                       : num(0) 
     $ mean_concentration           : num(0) 
     $ sd_concentration             : num(0) 
     $ cv_concentration             : num(0) 
     $ n_wells_valid_conc           : int(0) 
     $ is_zero_standard             : logi(0) 
     $ high_cv                      : logi(0) 
     $ se_595                       : num(0) 
     $ calculated_concentration_well: num(0) 
     $ calculated_concentration     : num(0) 
     $ in_curve_range               : logi(0) 

``` r
reassay_candidates <- bind_rows(
  failed_qc_samples %>%
    transmute(`Sample name` = base_sample_name,
              `Rationale for re-assay` = as.character(ifelse(
                is.na(cv_concentration),
                "CV undefined (< 2 valid replicates)",
                "CV > 15%"
              ))),
  extrapolated_samples %>%
    transmute(`Sample name` = base_sample_name,
              `Rationale for re-assay` = "outside standard curve range")
) %>%
  arrange(natural_sort_key(`Sample name`))

cat("--- reassay_candidates: all samples flagged for re-assay, with rationale ---\n")
str(reassay_candidates)

if (nrow(reassay_candidates) == 0) {
  cat("None -- every sample on this plate passed all QC.\n")
} else {
  kable(reassay_candidates)
}
```

    --- reassay_candidates: all samples flagged for re-assay, with rationale ---
    tibble [0 × 2] (S3: tbl_df/tbl/data.frame)
     $ Sample name           : chr(0) 
     $ Rationale for re-assay: chr(0) 
    None -- every sample on this plate passed all QC.
