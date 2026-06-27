Gen5-20260417-mgig-glycogenglo
================
Hazel Abrahamson-Amerine
2026-06-25

- [1 STANDARD CURVES](#1-standard-curves)
  - [1.1 Glycogen Standard Curve](#11-glycogen-standard-curve)
    - [1.1.1 Extract luminescence data](#111-extract-luminescence-data)
    - [1.1.2 Glycogen standard curve summary statistics and linear
      regression](#112-glycogen-standard-curve-summary-statistics-and-linear-regression)
    - [1.1.3 Calculate sample glycogen
      levels](#113-calculate-sample-glycogen-levels)
    - [1.1.4 Plot glycogen standard curve, sample
      points](#114-plot-glycogen-standard-curve-sample-points)
    - [1.1.5 Sample glycogen table](#115-sample-glycogen-table)
  - [1.2 glucose Standard Curve](#12-glucose-standard-curve)
    - [1.2.1 Extract luminescence data](#121-extract-luminescence-data)
    - [1.2.2 Glucose standard curve summary statistics and linear
      regression](#122-glucose-standard-curve-summary-statistics-and-linear-regression)
    - [1.2.3 Calculate sample glucose
      levels](#123-calculate-sample-glucose-levels)
    - [1.2.4 Plot glucose standard curve, sample
      points](#124-plot-glucose-standard-curve-sample-points)
    - [1.2.5 Sample glucose table](#125-sample-glucose-table)
- [2 Overall summary table](#2-overall-summary-table)

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
knitr::opts_chunk$set(
  echo = TRUE,         # Display code chunks
  eval = TRUE,        # Evaluate code chunks
  warning = FALSE,     # Hide warnings
  message = FALSE,     # Hide messages
  comment = "",         # Prevents appending '##' to beginning of lines in code output
  results = 'hold'     # Holds output so it's all printed together after code chunk
)
```

``` r
#plate 1 - samples 0.0A-0.0J
plate_layout1 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-Gen5-20260417A-mgig-glycogenglo.csv", header = FALSE)
raw_luminescence1 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-Gen5-20260417A-mgig-glycogenglo.csv", header = FALSE)

cat("Plate layout1:\n")
str(plate_layout1)

cat("\n\n")

cat("Raw luminescence1:\n")
str(raw_luminescence1)

#plate 2 - samples 48.0A-48.2F
plate_layout2 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-Gen5-20260417B-mgig-glycogenglo.csv", header = FALSE)
raw_luminescence2 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-Gen5-20260417B-mgig-glycogenglo.csv", header = FALSE)

cat("Plate layout2:\n")
str(plate_layout2)

cat("\n\n")

cat("Raw luminescence2:\n")
str(raw_luminescence2)

#plate 3 - samples 48.2G-48.7J
plate_layout3 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-Gen5-20260417C-mgig-glycogenglo.csv", header = FALSE)
raw_luminescence3 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-Gen5-20260417C-mgig-glycogenglo.csv", header = FALSE)

cat("Plate layout3:\n")
str(plate_layout3)

cat("\n\n")

cat("Raw luminescence3:\n")
str(raw_luminescence3)

#sample weights
sample_weights <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/20260625A_mgig_wholetissue_weights_GlycogenGlo.csv", header = FALSE)

cat("Sample Weights:\n")
str(sample_weights)
```

    Plate layout1:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : chr  "0.0A-glyc-17.5-df.20" "0.0B-glyc-18.0-df.20" "0.0C-glyc-17.0-df.20" "0.0D-glyc-19.4-df.20" ...
     $ V2 : chr  "0.0A-glyc-17.5-df.20" "0.0B-glyc-18.0-df.20" "0.0C-glyc-17.0-df.20" "0.0D-glyc-19.4-df.20" ...
     $ V3 : chr  "0.0A-glyc-17.5-df.20" "0.0B-glyc-18.0-df.20" "0.0C-glyc-17.0-df.20" "0.0D-glyc-19.4-df.20" ...
     $ V4 : chr  "0.0F-glyc-23.1-df.20" "0.0G-glyc-23.2-df.20" "0.0H-glyc-13.1-df.20" "0.0I-glyc-14.2-df.20" ...
     $ V5 : chr  "0.0F-glyc-23.1-df.20" "0.0G-glyc-23.2-df.20" "0.0H-glyc-13.1-df.20" "0.0I-glyc-14.2-df.20" ...
     $ V6 : chr  "0.0F-glyc-23.1-df.20" "0.0G-glyc-23.2-df.20" "0.0H-glyc-13.1-df.20" "0.0I-glyc-14.2-df.20" ...
     $ V7 : chr  "0.0A-glu-17.5-df.20" "0.0B-glu-18.0-df.20" "0.0C-glu-17.0-df.20" "0.0D-glu-19.4-df.20" ...
     $ V8 : chr  "0.0A-glu-17.5-df.20" "0.0B-glu-18.0-df.20" "0.0C-glu-17.0-df.20" "0.0D-glu-19.4-df.20" ...
     $ V9 : chr  "0.0A-glu-17.5-df.20" "0.0B-glu-18.0-df.20" "0.0C-glu-17.0-df.20" "0.0D-glu-19.4-df.20" ...
     $ V10: chr  "0.0F-glu-23.1-df.20" "0.0G-glu-23.2-df.20" "0.0H-glu-13.1-df.20" "0.0I-glu-14.2-df.20" ...
     $ V11: chr  "0.0F-glu-23.1-df.20" "0.0G-glu-23.2-df.20" "0.0H-glu-13.1-df.20" "0.0I-glu-14.2-df.20" ...
     $ V12: chr  "0.0F-glu-23.1-df.20" "0.0G-glu-23.2-df.20" "0.0H-glu-13.1-df.20" "0.0I-glu-14.2-df.20" ...


    Raw luminescence1:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : int  11298 12052 21267 10356 22711 96818 91415 89797
     $ V2 : int  11136 11886 22778 10103 22752 5225 5737 4843
     $ V3 : int  11458 11641 21249 10226 22710 999 1115 1358
     $ V4 : int  20326 12616 2258 10576 2736 656 735 671
     $ V5 : int  19828 14450 2383 10563 2715 4813 701 735
     $ V6 : int  20451 12667 2219 10637 2993 NA NA NA
     $ V7 : int  528 258 553 397 361 110918 110118 110539
     $ V8 : int  487 250 633 338 349 9528 9323 9671
     $ V9 : int  498 252 557 349 294 1015 988 978
     $ V10: int  304 246 228 250 346 311 290 334
     $ V11: int  329 252 256 247 295 226 232 252
     $ V12: int  384 292 272 252 300 NA NA NA
    Plate layout2:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : chr  "48.0A-glyc-11.9-df.20" "48.0B-glyc-18.6-df.20" "48.0C-glyc-21.6-df.20" "48.0D-glyc-8.4-df.20" ...
     $ V2 : chr  "48.0A-glyc-11.9-df.20" "48.0B-glyc-18.6-df.20" "48.0C-glyc-21.6-df.20" "48.0D-glyc-8.4-df.20" ...
     $ V3 : chr  "48.0A-glyc-11.9-df.20" "48.0B-glyc-18.6-df.20" "48.0C-glyc-21.6-df.20" "48.0D-glyc-8.4-df.20" ...
     $ V4 : chr  "48.0I-glyc-21.2-df.20" "48.0J-glyc-14.5-df.20" "48.2A-glyc-12.2-df.20" "48.2B-glyc-10.5-df.20" ...
     $ V5 : chr  "48.0I-glyc-21.2-df.20" "48.0J-glyc-14.5-df.20" "48.2A-glyc-12.2-df.20" "48.2B-glyc-10.5-df.20" ...
     $ V6 : chr  "48.0I-glyc-21.2-df.20" "48.0J-glyc-14.5-df.20" "48.2A-glyc-12.2-df.20" "48.2B-glyc-10.5-df.20" ...
     $ V7 : chr  "48.0A-glu-11.9-df.20" "48.0B-glu-18.6-df.20" "48.0C-glu-21.6-df.20" "48.0D-glu-8.4-df.20" ...
     $ V8 : chr  "48.0A-glu-11.9-df.20" "48.0B-glu-18.6-df.20" "48.0C-glu-21.6-df.20" "48.0D-glu-8.4-df.20" ...
     $ V9 : chr  "48.0A-glu-11.9-df.20" "48.0B-glu-18.6-df.20" "48.0C-glu-21.6-df.20" "48.0D-glu-8.4-df.20" ...
     $ V10: chr  "48.0I-glu-21.2-df.20" "48.0J-glu-14.5-df.20" "48.2A-glu-12.2-df.20" "48.2B-glu-10.5-df.20" ...
     $ V11: chr  "48.0I-glu-21.2-df.20" "48.0J-glu-14.5-df.20" "48.2A-glu-12.2-df.20" "48.2B-glu-10.5-df.20" ...
     $ V12: chr  "48.0I-glu-21.2-df.20" "48.0J-glu-14.5-df.20" "48.2A-glu-12.2-df.20" "48.2B-glu-10.5-df.20" ...


    Raw luminescence2:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : int  1350 788 2394 2012 2521 1267 1471 1350
     $ V2 : int  1355 658 2395 1939 2415 1273 1456 6069
     $ V3 : int  1185 868 2328 1918 2454 1237 1592 1463
     $ V4 : int  3093 963 3752 1200 1637 8617 8921 11139
     $ V5 : int  3161 1006 3793 1190 1703 8211 8862 11037
     $ V6 : int  3161 1159 3699 1260 1648 8181 9042 12035
     $ V7 : int  301 2201 369 363 941 346 266 616
     $ V8 : int  346 266 305 349 291 275 252 321
     $ V9 : int  349 314 272 407 276 299 281 290
     $ V10: int  295 321 394 272 328 281 332 413
     $ V11: int  318 269 421 309 325 308 361 464
     $ V12: int  329 336 423 323 317 269 311 421
    Plate layout3:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : chr  "48.2G-glyc-16.4-df.20" "48.2H-glyc-20.1-df.20" "48.2I-glyc-9.9-df.20" "48.2J-glyc-9.1-df.20" ...
     $ V2 : chr  "48.2G-glyc-16.4-df.20" "48.2H-glyc-20.1-df.20" "48.2I-glyc-9.9-df.20" "48.2J-glyc-9.1-df.20" ...
     $ V3 : chr  "48.2G-glyc-16.4-df.20" "48.2H-glyc-20.1-df.20" "48.2I-glyc-9.9-df.20" "48.2J-glyc-9.1-df.20" ...
     $ V4 : chr  "48.7E-glyc-11.7-df.20" "48.7F-glyc-6.6-df.20" "48.7G-glyc-22.4-df.20" "48.7H-glyc-17.3-df.20" ...
     $ V5 : chr  "48.7E-glyc-11.7-df.20" "48.7F-glyc-6.6-df.20" "48.7G-glyc-22.4-df.20" "48.7H-glyc-17.3-df.20" ...
     $ V6 : chr  "48.7E-glyc-11.7-df.20" "48.7F-glyc-6.6-df.20" "48.7G-glyc-22.4-df.20" "48.7H-glyc-17.3-df.20" ...
     $ V7 : chr  "48.2G-glu-16.4-df.20" "48.2H-glu-20.1-df.20" "48.2I-glu-9.9-df.20" "48.2J-glu-9.1-df.20" ...
     $ V8 : chr  "48.2G-glu-16.4-df.20" "48.2H-glu-20.1-df.20" "48.2I-glu-9.9-df.20" "48.2J-glu-9.1-df.20" ...
     $ V9 : chr  "48.2G-glu-16.4-df.20" "48.2H-glu-20.1-df.20" "48.2I-glu-9.9-df.20" "48.2J-glu-9.1-df.20" ...
     $ V10: chr  "48.7E-glu-11.7-df.20" "48.7F-glu-6.6-df.20" "48.7G-glu-22.4-df.20" "48.7H-glu-17.3-df.20" ...
     $ V11: chr  "48.7E-glu-11.7-df.20" "48.7F-glu-6.6-df.20" "48.7G-glu-22.4-df.20" "48.7H-glu-17.3-df.20" ...
     $ V12: chr  "48.7E-glu-11.7-df.20" "48.7F-glu-6.6-df.20" "48.7G-glu-22.4-df.20" "48.7H-glu-17.3-df.20" ...


    Raw luminescence3:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : int  13396 3083 2486 3246 6904 4881 10539 4906
     $ V2 : int  12876 3036 2503 3403 6719 5108 10803 5021
     $ V3 : int  12982 3345 2290 3792 8185 4801 10783 4804
     $ V4 : int  9889 1435 11949 16157 5380 13822 NA NA
     $ V5 : int  10206 1573 10631 15758 4984 12949 NA NA
     $ V6 : int  9655 1690 10789 15989 5165 12751 NA NA
     $ V7 : int  365 366 367 374 388 383 377 410
     $ V8 : int  457 372 338 375 370 386 384 393
     $ V9 : int  470 367 300 340 345 367 324 379
     $ V10: int  370 340 466 410 390 310 NA NA
     $ V11: int  442 353 434 382 435 359 NA NA
     $ V12: int  397 296 474 420 402 332 NA NA
    Sample Weights:
    'data.frame':   41 obs. of  2 variables:
     $ V1: chr  "SampleID" "0.0A" "0.0B" "0.0C" ...
     $ V2: chr  "Weight (mg)" "17.5" "18.0" "17.0" ...

# 1 STANDARD CURVES

## 1.1 Glycogen Standard Curve

### 1.1.1 Extract luminescence data

``` r
# Extract glycogen standard curve data from plate layout and raw luminescence.
# Rows F, G, H (rows 6, 7, 8) correspond to glycogen standards
# Columns 1-5 contain the standard curve concentrations

# Extract concentration values from plate layout (row 6, columns 1-5)
# Parse concentration from labels like "STD-glyc-20" -> 20
glyc_concentrations <- as.numeric(gsub("STD-glyc-", "", plate_layout1[6, 1:5]))
glyc_concentrations

# Extract standard curve luminescence values for each replicate from raw_luminescence
glyc_row_F <- as.numeric(raw_luminescence1[6, 1:5])  # Row 6 (F)
glyc_row_G <- as.numeric(raw_luminescence1[7, 1:5])  # Row 7 (G)
glyc_row_H <- as.numeric(raw_luminescence1[8, 1:5])  # Row 8 (H)
```

    [1] 20.00  2.00  0.20  0.02  0.00

``` r
#Extract glycogen sample data - wells A1-E6
glyc_sample_cols1 <- c(1,2,3)
glyc_0.0A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[1, 1]))
glyc_0.0A_luminescence <- as.numeric(raw_luminescence1[1, glyc_sample_cols1])

glyc_0.0B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[2, 1]))
glyc_0.0B_luminescence <- as.numeric(raw_luminescence1[2, glyc_sample_cols1])

glyc_0.0C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[3, 1]))
glyc_0.0C_luminescence <- as.numeric(raw_luminescence1[3, glyc_sample_cols1])

glyc_0.0D_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[4, 1]))
glyc_0.0D_luminescence <- as.numeric(raw_luminescence1[4, glyc_sample_cols1])

glyc_0.0E_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[5, 1]))
glyc_0.0E_luminescence <- as.numeric(raw_luminescence1[5, glyc_sample_cols1])

glyc_sample_cols2 <- c(4,5,6)
glyc_0.0F_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[1, 4]))
glyc_0.0F_luminescence <- as.numeric(raw_luminescence1[1, glyc_sample_cols2])

glyc_0.0G_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[2, 4]))
glyc_0.0G_luminescence <- as.numeric(raw_luminescence1[2, glyc_sample_cols2])

glyc_0.0H_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[3, 4]))
glyc_0.0H_luminescence <- as.numeric(raw_luminescence1[3, glyc_sample_cols2])

glyc_0.0I_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[4, 4]))
glyc_0.0I_luminescence <- as.numeric(raw_luminescence1[4, glyc_sample_cols2])

glyc_0.0J_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[5, 4]))
glyc_0.0J_luminescence <- as.numeric(raw_luminescence1[5, glyc_sample_cols2])


glyc_sample_cols1 <- c(1,2,3)
glyc_48.0A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[1, 1]))
glyc_48.0A_luminescence <- as.numeric(raw_luminescence2[1, glyc_sample_cols1])

glyc_48.0B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[2, 1]))
glyc_48.0B_luminescence <- as.numeric(raw_luminescence2[2, glyc_sample_cols1])

glyc_48.0C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[3, 1]))
glyc_48.0C_luminescence <- as.numeric(raw_luminescence2[3, glyc_sample_cols1])

glyc_48.0D_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[4, 1]))
glyc_48.0D_luminescence <- as.numeric(raw_luminescence2[4, glyc_sample_cols1])

glyc_48.0E_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[5, 1]))
glyc_48.0E_luminescence <- as.numeric(raw_luminescence2[5, glyc_sample_cols1])

glyc_48.0F_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[6, 1]))
glyc_48.0F_luminescence <- as.numeric(raw_luminescence2[6, glyc_sample_cols1])

glyc_48.0G_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[7, 1]))
glyc_48.0G_luminescence <- as.numeric(raw_luminescence2[7, glyc_sample_cols1])

glyc_48.0H_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[8, 1]))
glyc_48.0H_luminescence <- as.numeric(raw_luminescence2[8, glyc_sample_cols1])

glyc_sample_cols2 <- c(4,5,6)
glyc_48.0I_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[1, 4]))
glyc_48.0I_luminescence <- as.numeric(raw_luminescence2[1, glyc_sample_cols2])

glyc_48.0J_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[2, 4]))
glyc_48.0J_luminescence <- as.numeric(raw_luminescence2[2, glyc_sample_cols2])

glyc_48.2A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[3, 4]))
glyc_48.2A_luminescence <- as.numeric(raw_luminescence2[3, glyc_sample_cols2])

glyc_48.2B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[4, 4]))
glyc_48.2B_luminescence <- as.numeric(raw_luminescence2[4, glyc_sample_cols2])

glyc_48.2C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[5, 4]))
glyc_48.2C_luminescence <- as.numeric(raw_luminescence2[5, glyc_sample_cols2])

glyc_48.2D_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[6, 4]))
glyc_48.2D_luminescence <- as.numeric(raw_luminescence2[6, glyc_sample_cols2])

glyc_48.2E_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[7, 4]))
glyc_48.2E_luminescence <- as.numeric(raw_luminescence2[7, glyc_sample_cols2])

glyc_48.2F_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[8, 4]))
glyc_48.2F_luminescence <- as.numeric(raw_luminescence2[8, glyc_sample_cols2])


glyc_sample_cols1 <- c(1,2,3)
glyc_48.2G_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[1, 1]))
glyc_48.2G_luminescence <- as.numeric(raw_luminescence3[1, glyc_sample_cols1])

glyc_48.2H_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[2, 1]))
glyc_48.2H_luminescence <- as.numeric(raw_luminescence3[2, glyc_sample_cols1])

glyc_48.2I_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[3, 1]))
glyc_48.2I_luminescence <- as.numeric(raw_luminescence3[3, glyc_sample_cols1])

glyc_48.2J_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[4, 1]))
glyc_48.2J_luminescence <- as.numeric(raw_luminescence3[4, glyc_sample_cols1])

glyc_48.7A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[5, 1]))
glyc_48.7A_luminescence <- as.numeric(raw_luminescence3[5, glyc_sample_cols1])

glyc_48.7B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[6, 1]))
glyc_48.7B_luminescence <- as.numeric(raw_luminescence3[6, glyc_sample_cols1])

glyc_48.7C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[7, 1]))
glyc_48.7C_luminescence <- as.numeric(raw_luminescence3[7, glyc_sample_cols1])

glyc_48.7D_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[8, 1]))
glyc_48.7D_luminescence <- as.numeric(raw_luminescence3[8, glyc_sample_cols1])

glyc_sample_cols2 <- c(4,5,6)
glyc_48.7E_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[1, 4]))
glyc_48.7E_luminescence <- as.numeric(raw_luminescence3[1, glyc_sample_cols2])

glyc_48.7F_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[2, 4]))
glyc_48.7F_luminescence <- as.numeric(raw_luminescence3[2, glyc_sample_cols2])

glyc_48.7G_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[3, 4]))
glyc_48.7G_luminescence <- as.numeric(raw_luminescence3[3, glyc_sample_cols2])

glyc_48.7H_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[4, 4]))
glyc_48.7H_luminescence <- as.numeric(raw_luminescence3[4, glyc_sample_cols2])

glyc_48.7I_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[5, 4]))
glyc_48.7I_luminescence <- as.numeric(raw_luminescence3[5, glyc_sample_cols2])

glyc_48.7J_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[6, 4]))
glyc_48.7J_luminescence <- as.numeric(raw_luminescence3[6, glyc_sample_cols2])
```

### 1.1.2 Glycogen standard curve summary statistics and linear regression

``` r
# Calculate mean and standard error for each concentration
glyc_means <- numeric(length(glyc_concentrations))
glyc_std_errors <- numeric(length(glyc_concentrations))
glyc_std_devs <- numeric(length(glyc_concentrations))

for (i in 1:length(glyc_concentrations)) {
  glyc_values <- c(glyc_row_F[i], glyc_row_G[i], glyc_row_H[i])
  glyc_means[i] <- mean(glyc_values)
  glyc_std_devs[i] <- sd(glyc_values)
  glyc_std_errors[i] <- sd(glyc_values) / sqrt(length(glyc_values))
}

# Create data frame for plotting
glycogen_summary_data <- data.frame(
  glyc_concentration = glyc_concentrations,
  glyc_mean_luminescence = glyc_means,
  glyc_se = glyc_std_errors,
  glyc_sd = glyc_std_devs,
  glyc_cv = (glyc_std_devs / glyc_means) * 100
)

# Calculate linear regression and R-squared
lm_model <- lm(glyc_mean_luminescence ~ glyc_concentration, data = glycogen_summary_data)
glyc_r_squared <- summary(lm_model)$r.squared
glyc_slope <- coef(lm_model)[2]
glyc_intercept <- coef(lm_model)[1]
```

### 1.1.3 Calculate sample glycogen levels

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_0.0A_mean_lum <- numeric(length(glyc_0.0A_dilution))
glyc_0.0A_se_lum <- numeric(length(glyc_0.0A_dilution))
glyc_0.0A_mean_conc <- numeric(length(glyc_0.0A_dilution))

for (i in 1:length(glyc_0.0A_dilution)) {
  df_val <- glyc_0.0A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_0.0A_lum_values <- c(glyc_0.0A_luminescence[glyc_0.0A_dilution == df_val])
  glyc_0.0A_mean_lum[i] <- mean(glyc_0.0A_lum_values)
  glyc_0.0A_se_lum[i] <- sd(glyc_0.0A_lum_values) / sqrt(length(glyc_0.0A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_0.0A_mean_conc[i] <- (glyc_0.0A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_0.0A_data <- data.frame(
  glyc_0.0A_dilution_factor = glyc_0.0A_dilution,
  glyc_0.0A_mean_luminescence = glyc_0.0A_mean_lum,
  glyc_0.0A_se = glyc_0.0A_se_lum,
  glyc_0.0A_conc =  glyc_0.0A_mean_conc,
  label = paste0("0.0Adf.", glyc_0.0A_dilution)
)
glyc_0.0A_mean_lum
glyc_0.0A_mean_conc
```

    [1] 11297.33
    [1] 2.480813

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_0.0B_mean_lum <- numeric(length(glyc_0.0B_dilution))
glyc_0.0B_se_lum <- numeric(length(glyc_0.0B_dilution))
glyc_0.0B_mean_conc <- numeric(length(glyc_0.0B_dilution))

for (i in 1:length(glyc_0.0B_dilution)) {
  df_val <- glyc_0.0B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_0.0B_lum_values <- c(glyc_0.0B_luminescence[glyc_0.0B_dilution == df_val])
  glyc_0.0B_mean_lum[i] <- mean(glyc_0.0B_lum_values)
  glyc_0.0B_se_lum[i] <- sd(glyc_0.0B_lum_values) / sqrt(length(glyc_0.0B_lum_values))
  # Calculate concentration from mean luminescence
  glyc_0.0B_mean_conc[i] <- (glyc_0.0B_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_0.0B_data <- data.frame(
  glyc_0.0B_dilution_factor = glyc_0.0B_dilution,
  glyc_0.0B_mean_luminescence = glyc_0.0B_mean_lum,
  glyc_0.0B_se = glyc_0.0B_se_lum,
  glyc_0.0B_conc =  glyc_0.0B_mean_conc,
  label = paste0("0.0Bdf.", glyc_0.0B_dilution)
)
glyc_0.0B_mean_lum
glyc_0.0B_mean_conc
```

    [1] 11859.67
    [1] 2.602433

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_0.0C_mean_lum <- numeric(length(glyc_0.0C_dilution))
glyc_0.0C_se_lum <- numeric(length(glyc_0.0C_dilution))
glyc_0.0C_mean_conc <- numeric(length(glyc_0.0C_dilution))

for (i in 1:length(glyc_0.0C_dilution)) {
  df_val <- glyc_0.0C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_0.0C_lum_values <- c(glyc_0.0C_luminescence[glyc_0.0C_dilution == df_val])
  glyc_0.0C_mean_lum[i] <- mean(glyc_0.0C_lum_values)
  glyc_0.0C_se_lum[i] <- sd(glyc_0.0C_lum_values) / sqrt(length(glyc_0.0C_lum_values))
  # Calculate concentration from mean luminescence
  glyc_0.0C_mean_conc[i] <- (glyc_0.0C_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_0.0C_data <- data.frame(
  glyc_0.0C_dilution_factor = glyc_0.0C_dilution,
  glyc_0.0C_mean_luminescence = glyc_0.0C_mean_lum,
  glyc_0.0C_se = glyc_0.0C_se_lum,
  glyc_0.0C_conc =  glyc_0.0C_mean_conc,
  label = paste0("0.0Cdf.", glyc_0.0C_dilution)
)
glyc_0.0C_mean_lum
glyc_0.0C_mean_conc
```

    [1] 21764.67
    [1] 4.744653

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_0.0D_mean_lum <- numeric(length(glyc_0.0D_dilution))
glyc_0.0D_se_lum <- numeric(length(glyc_0.0D_dilution))
glyc_0.0D_mean_conc <- numeric(length(glyc_0.0D_dilution))

for (i in 1:length(glyc_0.0D_dilution)) {
  df_val <- glyc_0.0D_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_0.0D_lum_values <- c(glyc_0.0D_luminescence[glyc_0.0D_dilution == df_val])
  glyc_0.0D_mean_lum[i] <- mean(glyc_0.0D_lum_values)
  glyc_0.0D_se_lum[i] <- sd(glyc_0.0D_lum_values) / sqrt(length(glyc_0.0D_lum_values))
  # Calculate concentration from mean luminescence
  glyc_0.0D_mean_conc[i] <- (glyc_0.0D_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_0.0D_data <- data.frame(
  glyc_0.0D_dilution_factor = glyc_0.0D_dilution,
  glyc_0.0D_mean_luminescence = glyc_0.0D_mean_lum,
  glyc_0.0D_se = glyc_0.0D_se_lum,
  glyc_0.0D_conc =  glyc_0.0D_mean_conc,
  label = paste0("0.0Ddf.", glyc_0.0D_dilution)
)
glyc_0.0D_mean_lum
glyc_0.0D_mean_conc
```

    [1] 10228.33
    [1] 2.249613

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_0.0E_mean_lum <- numeric(length(glyc_0.0E_dilution))
glyc_0.0E_se_lum <- numeric(length(glyc_0.0E_dilution))
glyc_0.0E_mean_conc <- numeric(length(glyc_0.0E_dilution))

for (i in 1:length(glyc_0.0E_dilution)) {
  df_val <- glyc_0.0E_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_0.0E_lum_values <- c(glyc_0.0E_luminescence[glyc_0.0E_dilution == df_val])
  glyc_0.0E_mean_lum[i] <- mean(glyc_0.0E_lum_values)
  glyc_0.0E_se_lum[i] <- sd(glyc_0.0E_lum_values) / sqrt(length(glyc_0.0E_lum_values))
  # Calculate concentration from mean luminescence
  glyc_0.0E_mean_conc[i] <- (glyc_0.0E_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_0.0E_data <- data.frame(
  glyc_0.0E_dilution_factor = glyc_0.0E_dilution,
  glyc_0.0E_mean_luminescence = glyc_0.0E_mean_lum,
  glyc_0.0E_se = glyc_0.0E_se_lum,
  glyc_0.0E_conc =  glyc_0.0E_mean_conc,
  label = paste0("0.0Edf.", glyc_0.0E_dilution)
)
glyc_0.0E_mean_lum
glyc_0.0E_mean_conc
```

    [1] 22724.33
    [1] 4.952207

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_0.0F_mean_lum <- numeric(length(glyc_0.0F_dilution))
glyc_0.0F_se_lum <- numeric(length(glyc_0.0F_dilution))
glyc_0.0F_mean_conc <- numeric(length(glyc_0.0F_dilution))

for (i in 1:length(glyc_0.0F_dilution)) {
  df_val <- glyc_0.0F_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_0.0F_lum_values <- c(glyc_0.0F_luminescence[glyc_0.0F_dilution == df_val])
  glyc_0.0F_mean_lum[i] <- mean(glyc_0.0F_lum_values)
  glyc_0.0F_se_lum[i] <- sd(glyc_0.0F_lum_values) / sqrt(length(glyc_0.0F_lum_values))
  # Calculate concentration from mean luminescence
  glyc_0.0F_mean_conc[i] <- (glyc_0.0F_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_0.0F_data <- data.frame(
  glyc_0.0F_dilution_factor = glyc_0.0F_dilution,
  glyc_0.0F_mean_luminescence = glyc_0.0F_mean_lum,
  glyc_0.0F_se = glyc_0.0F_se_lum,
  glyc_0.0F_conc =  glyc_0.0F_mean_conc,
  label = paste0("0.0Fdf.", glyc_0.0F_dilution)
)
glyc_0.0F_mean_lum
glyc_0.0F_mean_conc
```

    [1] 20201.67
    [1] 4.406613

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_0.0G_mean_lum <- numeric(length(glyc_0.0G_dilution))
glyc_0.0G_se_lum <- numeric(length(glyc_0.0G_dilution))
glyc_0.0G_mean_conc <- numeric(length(glyc_0.0G_dilution))

for (i in 1:length(glyc_0.0G_dilution)) {
  df_val <- glyc_0.0G_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_0.0G_lum_values <- c(glyc_0.0G_luminescence[glyc_0.0G_dilution == df_val])
  glyc_0.0G_mean_lum[i] <- mean(glyc_0.0G_lum_values)
  glyc_0.0G_se_lum[i] <- sd(glyc_0.0G_lum_values) / sqrt(length(glyc_0.0G_lum_values))
  # Calculate concentration from mean luminescence
  glyc_0.0G_mean_conc[i] <- (glyc_0.0G_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_0.0G_data <- data.frame(
  glyc_0.0G_dilution_factor = glyc_0.0G_dilution,
  glyc_0.0G_mean_luminescence = glyc_0.0G_mean_lum,
  glyc_0.0G_se = glyc_0.0G_se_lum,
  glyc_0.0G_conc =  glyc_0.0G_mean_conc,
  label = paste0("0.0Gdf.", glyc_0.0G_dilution)
)
glyc_0.0G_mean_lum
glyc_0.0G_mean_conc
```

    [1] 13244.33
    [1] 2.901904

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_0.0H_mean_lum <- numeric(length(glyc_0.0H_dilution))
glyc_0.0H_se_lum <- numeric(length(glyc_0.0H_dilution))
glyc_0.0H_mean_conc <- numeric(length(glyc_0.0H_dilution))

for (i in 1:length(glyc_0.0H_dilution)) {
  df_val <- glyc_0.0H_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_0.0H_lum_values <- c(glyc_0.0H_luminescence[glyc_0.0H_dilution == df_val])
  glyc_0.0H_mean_lum[i] <- mean(glyc_0.0H_lum_values)
  glyc_0.0H_se_lum[i] <- sd(glyc_0.0H_lum_values) / sqrt(length(glyc_0.0H_lum_values))
  # Calculate concentration from mean luminescence
  glyc_0.0H_mean_conc[i] <- (glyc_0.0H_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_0.0H_data <- data.frame(
  glyc_0.0H_dilution_factor = glyc_0.0H_dilution,
  glyc_0.0H_mean_luminescence = glyc_0.0H_mean_lum,
  glyc_0.0H_se = glyc_0.0H_se_lum,
  glyc_0.0H_conc =  glyc_0.0H_mean_conc,
  label = paste0("0.0Hdf.", glyc_0.0H_dilution)
)
glyc_0.0H_mean_lum
glyc_0.0H_mean_conc
```

    [1] 2286.667
    [1] 0.5320155

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_0.0I_mean_lum <- numeric(length(glyc_0.0I_dilution))
glyc_0.0I_se_lum <- numeric(length(glyc_0.0I_dilution))
glyc_0.0I_mean_conc <- numeric(length(glyc_0.0I_dilution))

for (i in 1:length(glyc_0.0I_dilution)) {
  df_val <- glyc_0.0I_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_0.0I_lum_values <- c(glyc_0.0I_luminescence[glyc_0.0I_dilution == df_val])
  glyc_0.0I_mean_lum[i] <- mean(glyc_0.0I_lum_values)
  glyc_0.0I_se_lum[i] <- sd(glyc_0.0I_lum_values) / sqrt(length(glyc_0.0I_lum_values))
  # Calculate concentration from mean luminescence
  glyc_0.0I_mean_conc[i] <- (glyc_0.0I_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_0.0I_data <- data.frame(
  glyc_0.0I_dilution_factor = glyc_0.0I_dilution,
  glyc_0.0I_mean_luminescence = glyc_0.0I_mean_lum,
  glyc_0.0I_se = glyc_0.0I_se_lum,
  glyc_0.0I_conc =  glyc_0.0I_mean_conc,
  label = paste0("0.0Idf.", glyc_0.0I_dilution)
)
glyc_0.0I_mean_lum
glyc_0.0I_mean_conc
```

    [1] 10592
    [1] 2.328266

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_0.0J_mean_lum <- numeric(length(glyc_0.0J_dilution))
glyc_0.0J_se_lum <- numeric(length(glyc_0.0J_dilution))
glyc_0.0J_mean_conc <- numeric(length(glyc_0.0J_dilution))

for (i in 1:length(glyc_0.0J_dilution)) {
  df_val <- glyc_0.0J_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_0.0J_lum_values <- c(glyc_0.0J_luminescence[glyc_0.0J_dilution == df_val])
  glyc_0.0J_mean_lum[i] <- mean(glyc_0.0J_lum_values)
  glyc_0.0J_se_lum[i] <- sd(glyc_0.0J_lum_values) / sqrt(length(glyc_0.0J_lum_values))
  # Calculate concentration from mean luminescence
  glyc_0.0J_mean_conc[i] <- (glyc_0.0J_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_0.0J_data <- data.frame(
  glyc_0.0J_dilution_factor = glyc_0.0J_dilution,
  glyc_0.0J_mean_luminescence = glyc_0.0J_mean_lum,
  glyc_0.0J_se = glyc_0.0J_se_lum,
  glyc_0.0J_conc =  glyc_0.0J_mean_conc,
  label = paste0("0.0Jdf.", glyc_0.0J_dilution)
)
glyc_0.0J_mean_lum
glyc_0.0J_mean_conc
```

    [1] 2814.667
    [1] 0.6462097

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.0A_mean_lum <- numeric(length(glyc_48.0A_dilution))
glyc_48.0A_se_lum <- numeric(length(glyc_48.0A_dilution))
glyc_48.0A_mean_conc <- numeric(length(glyc_48.0A_dilution))

for (i in 1:length(glyc_48.0A_dilution)) {
  df_val <- glyc_48.0A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.0A_lum_values <- c(glyc_48.0A_luminescence[glyc_48.0A_dilution == df_val])
  glyc_48.0A_mean_lum[i] <- mean(glyc_48.0A_lum_values)
  glyc_48.0A_se_lum[i] <- sd(glyc_48.0A_lum_values) / sqrt(length(glyc_48.0A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.0A_mean_conc[i] <- (glyc_48.0A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.0A_data <- data.frame(
  glyc_48.0A_dilution_factor = glyc_48.0A_dilution,
  glyc_48.0A_mean_luminescence = glyc_48.0A_mean_lum,
  glyc_48.0A_se = glyc_48.0A_se_lum,
  glyc_48.0A_conc =  glyc_48.0A_mean_conc,
  label = paste0("48.0Adf.", glyc_48.0A_dilution)
)
glyc_48.0A_mean_lum
glyc_48.0A_mean_conc
```

    [1] 1296.667
    [1] 0.3179016

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.0B_mean_lum <- numeric(length(glyc_48.0B_dilution))
glyc_48.0B_se_lum <- numeric(length(glyc_48.0B_dilution))
glyc_48.0B_mean_conc <- numeric(length(glyc_48.0B_dilution))

for (i in 1:length(glyc_48.0B_dilution)) {
  df_val <- glyc_48.0B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.0B_lum_values <- c(glyc_48.0B_luminescence[glyc_48.0B_dilution == df_val])
  glyc_48.0B_mean_lum[i] <- mean(glyc_48.0B_lum_values)
  glyc_48.0B_se_lum[i] <- sd(glyc_48.0B_lum_values) / sqrt(length(glyc_48.0B_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.0B_mean_conc[i] <- (glyc_48.0B_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.0B_data <- data.frame(
  glyc_48.0B_dilution_factor = glyc_48.0B_dilution,
  glyc_48.0B_mean_luminescence = glyc_48.0B_mean_lum,
  glyc_48.0B_se = glyc_48.0B_se_lum,
  glyc_48.0B_conc =  glyc_48.0B_mean_conc,
  label = paste0("48.0Bdf.", glyc_48.0B_dilution)
)
glyc_48.0B_mean_lum
glyc_48.0B_mean_conc
```

    [1] 771.3333
    [1] 0.2042842

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.0C_mean_lum <- numeric(length(glyc_48.0C_dilution))
glyc_48.0C_se_lum <- numeric(length(glyc_48.0C_dilution))
glyc_48.0C_mean_conc <- numeric(length(glyc_48.0C_dilution))

for (i in 1:length(glyc_48.0C_dilution)) {
  df_val <- glyc_48.0C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.0C_lum_values <- c(glyc_48.0C_luminescence[glyc_48.0C_dilution == df_val])
  glyc_48.0C_mean_lum[i] <- mean(glyc_48.0C_lum_values)
  glyc_48.0C_se_lum[i] <- sd(glyc_48.0C_lum_values) / sqrt(length(glyc_48.0C_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.0C_mean_conc[i] <- (glyc_48.0C_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.0C_data <- data.frame(
  glyc_48.0C_dilution_factor = glyc_48.0C_dilution,
  glyc_48.0C_mean_luminescence = glyc_48.0C_mean_lum,
  glyc_48.0C_se = glyc_48.0C_se_lum,
  glyc_48.0C_conc =  glyc_48.0C_mean_conc,
  label = paste0("48.0Cdf.", glyc_48.0C_dilution)
)
glyc_48.0C_mean_lum
glyc_48.0C_mean_conc
```

    [1] 2372.333
    [1] 0.5505433

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.0D_mean_lum <- numeric(length(glyc_48.0D_dilution))
glyc_48.0D_se_lum <- numeric(length(glyc_48.0D_dilution))
glyc_48.0D_mean_conc <- numeric(length(glyc_48.0D_dilution))

for (i in 1:length(glyc_48.0D_dilution)) {
  df_val <- glyc_48.0D_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.0D_lum_values <- c(glyc_48.0D_luminescence[glyc_48.0D_dilution == df_val])
  glyc_48.0D_mean_lum[i] <- mean(glyc_48.0D_lum_values)
  glyc_48.0D_se_lum[i] <- sd(glyc_48.0D_lum_values) / sqrt(length(glyc_48.0D_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.0D_mean_conc[i] <- (glyc_48.0D_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.0D_data <- data.frame(
  glyc_48.0D_dilution_factor = glyc_48.0D_dilution,
  glyc_48.0D_mean_luminescence = glyc_48.0D_mean_lum,
  glyc_48.0D_se = glyc_48.0D_se_lum,
  glyc_48.0D_conc =  glyc_48.0D_mean_conc,
  label = paste0("48.0Ddf.", glyc_48.0D_dilution)
)
glyc_48.0D_mean_lum
glyc_48.0D_mean_conc
```

    [1] 1956.333
    [1] 0.4605721

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.0E_mean_lum <- numeric(length(glyc_48.0E_dilution))
glyc_48.0E_se_lum <- numeric(length(glyc_48.0E_dilution))
glyc_48.0E_mean_conc <- numeric(length(glyc_48.0E_dilution))

for (i in 1:length(glyc_48.0E_dilution)) {
  df_val <- glyc_48.0E_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.0E_lum_values <- c(glyc_48.0E_luminescence[glyc_48.0E_dilution == df_val])
  glyc_48.0E_mean_lum[i] <- mean(glyc_48.0E_lum_values)
  glyc_48.0E_se_lum[i] <- sd(glyc_48.0E_lum_values) / sqrt(length(glyc_48.0E_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.0E_mean_conc[i] <- (glyc_48.0E_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.0E_data <- data.frame(
  glyc_48.0E_dilution_factor = glyc_48.0E_dilution,
  glyc_48.0E_mean_luminescence = glyc_48.0E_mean_lum,
  glyc_48.0E_se = glyc_48.0E_se_lum,
  glyc_48.0E_conc =  glyc_48.0E_mean_conc,
  label = paste0("48.0Edf.", glyc_48.0E_dilution)
)
glyc_48.0E_mean_lum
glyc_48.0E_mean_conc
```

    [1] 2463.333
    [1] 0.5702244

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.0F_mean_lum <- numeric(length(glyc_48.0F_dilution))
glyc_48.0F_se_lum <- numeric(length(glyc_48.0F_dilution))
glyc_48.0F_mean_conc <- numeric(length(glyc_48.0F_dilution))

for (i in 1:length(glyc_48.0F_dilution)) {
  df_val <- glyc_48.0F_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.0F_lum_values <- c(glyc_48.0F_luminescence[glyc_48.0F_dilution == df_val])
  glyc_48.0F_mean_lum[i] <- mean(glyc_48.0F_lum_values)
  glyc_48.0F_se_lum[i] <- sd(glyc_48.0F_lum_values) / sqrt(length(glyc_48.0F_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.0F_mean_conc[i] <- (glyc_48.0F_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.0F_data <- data.frame(
  glyc_48.0F_dilution_factor = glyc_48.0F_dilution,
  glyc_48.0F_mean_luminescence = glyc_48.0F_mean_lum,
  glyc_48.0F_se = glyc_48.0F_se_lum,
  glyc_48.0F_conc =  glyc_48.0F_mean_conc,
  label = paste0("48.0Fdf.", glyc_48.0F_dilution)
)
glyc_48.0F_mean_lum
glyc_48.0F_mean_conc
```

    [1] 1259
    [1] 0.3097552

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.0G_mean_lum <- numeric(length(glyc_48.0G_dilution))
glyc_48.0G_se_lum <- numeric(length(glyc_48.0G_dilution))
glyc_48.0G_mean_conc <- numeric(length(glyc_48.0G_dilution))

for (i in 1:length(glyc_48.0G_dilution)) {
  df_val <- glyc_48.0G_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.0G_lum_values <- c(glyc_48.0G_luminescence[glyc_48.0G_dilution == df_val])
  glyc_48.0G_mean_lum[i] <- mean(glyc_48.0G_lum_values)
  glyc_48.0G_se_lum[i] <- sd(glyc_48.0G_lum_values) / sqrt(length(glyc_48.0G_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.0G_mean_conc[i] <- (glyc_48.0G_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.0G_data <- data.frame(
  glyc_48.0G_dilution_factor = glyc_48.0G_dilution,
  glyc_48.0G_mean_luminescence = glyc_48.0G_mean_lum,
  glyc_48.0G_se = glyc_48.0G_se_lum,
  glyc_48.0G_conc =  glyc_48.0G_mean_conc,
  label = paste0("48.0Gdf.", glyc_48.0G_dilution)
)
glyc_48.0G_mean_lum
glyc_48.0G_mean_conc
```

    [1] 1506.333
    [1] 0.3632476

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.0H_mean_lum <- numeric(length(glyc_48.0H_dilution))
glyc_48.0H_se_lum <- numeric(length(glyc_48.0H_dilution))
glyc_48.0H_mean_conc <- numeric(length(glyc_48.0H_dilution))

for (i in 1:length(glyc_48.0H_dilution)) {
  df_val <- glyc_48.0H_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.0H_lum_values <- c(glyc_48.0H_luminescence[glyc_48.0H_dilution == df_val])
  glyc_48.0H_mean_lum[i] <- mean(glyc_48.0H_lum_values)
  glyc_48.0H_se_lum[i] <- sd(glyc_48.0H_lum_values) / sqrt(length(glyc_48.0H_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.0H_mean_conc[i] <- (glyc_48.0H_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.0H_data <- data.frame(
  glyc_48.0H_dilution_factor = glyc_48.0H_dilution,
  glyc_48.0H_mean_luminescence = glyc_48.0H_mean_lum,
  glyc_48.0H_se = glyc_48.0H_se_lum,
  glyc_48.0H_conc =  glyc_48.0H_mean_conc,
  label = paste0("48.0Hdf.", glyc_48.0H_dilution)
)
glyc_48.0H_mean_lum
glyc_48.0H_mean_conc
```

    [1] 2960.667
    [1] 0.6777861

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.0I_mean_lum <- numeric(length(glyc_48.0I_dilution))
glyc_48.0I_se_lum <- numeric(length(glyc_48.0I_dilution))
glyc_48.0I_mean_conc <- numeric(length(glyc_48.0I_dilution))

for (i in 1:length(glyc_48.0I_dilution)) {
  df_val <- glyc_48.0I_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.0I_lum_values <- c(glyc_48.0I_luminescence[glyc_48.0I_dilution == df_val])
  glyc_48.0I_mean_lum[i] <- mean(glyc_48.0I_lum_values)
  glyc_48.0I_se_lum[i] <- sd(glyc_48.0I_lum_values) / sqrt(length(glyc_48.0I_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.0I_mean_conc[i] <- (glyc_48.0I_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.0I_data <- data.frame(
  glyc_48.0I_dilution_factor = glyc_48.0I_dilution,
  glyc_48.0I_mean_luminescence = glyc_48.0I_mean_lum,
  glyc_48.0I_se = glyc_48.0I_se_lum,
  glyc_48.0I_conc =  glyc_48.0I_mean_conc,
  label = paste0("48.0Idf.", glyc_48.0I_dilution)
)
glyc_48.0I_mean_lum
glyc_48.0I_mean_conc
```

    [1] 3138.333
    [1] 0.7162112

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.0J_mean_lum <- numeric(length(glyc_48.0J_dilution))
glyc_48.0J_se_lum <- numeric(length(glyc_48.0J_dilution))
glyc_48.0J_mean_conc <- numeric(length(glyc_48.0J_dilution))

for (i in 1:length(glyc_48.0J_dilution)) {
  df_val <- glyc_48.0J_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.0J_lum_values <- c(glyc_48.0J_luminescence[glyc_48.0J_dilution == df_val])
  glyc_48.0J_mean_lum[i] <- mean(glyc_48.0J_lum_values)
  glyc_48.0J_se_lum[i] <- sd(glyc_48.0J_lum_values) / sqrt(length(glyc_48.0J_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.0J_mean_conc[i] <- (glyc_48.0J_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.0J_data <- data.frame(
  glyc_48.0J_dilution_factor = glyc_48.0J_dilution,
  glyc_48.0J_mean_luminescence = glyc_48.0J_mean_lum,
  glyc_48.0J_se = glyc_48.0J_se_lum,
  glyc_48.0J_conc =  glyc_48.0J_mean_conc,
  label = paste0("48.0Jdf.", glyc_48.0J_dilution)
)
glyc_48.0J_mean_lum
glyc_48.0J_mean_conc
```

    [1] 1042.667
    [1] 0.2629673

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.2A_mean_lum <- numeric(length(glyc_48.2A_dilution))
glyc_48.2A_se_lum <- numeric(length(glyc_48.2A_dilution))
glyc_48.2A_mean_conc <- numeric(length(glyc_48.2A_dilution))

for (i in 1:length(glyc_48.2A_dilution)) {
  df_val <- glyc_48.2A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.2A_lum_values <- c(glyc_48.2A_luminescence[glyc_48.2A_dilution == df_val])
  glyc_48.2A_mean_lum[i] <- mean(glyc_48.2A_lum_values)
  glyc_48.2A_se_lum[i] <- sd(glyc_48.2A_lum_values) / sqrt(length(glyc_48.2A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.2A_mean_conc[i] <- (glyc_48.2A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.2A_data <- data.frame(
  glyc_48.2A_dilution_factor = glyc_48.2A_dilution,
  glyc_48.2A_mean_luminescence = glyc_48.2A_mean_lum,
  glyc_48.2A_se = glyc_48.2A_se_lum,
  glyc_48.2A_conc =  glyc_48.2A_mean_conc,
  label = paste0("48.2Adf.", glyc_48.2A_dilution)
)
glyc_48.2A_mean_lum
glyc_48.2A_mean_conc
```

    [1] 3748
    [1] 0.8480679

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.2B_mean_lum <- numeric(length(glyc_48.2B_dilution))
glyc_48.2B_se_lum <- numeric(length(glyc_48.2B_dilution))
glyc_48.2B_mean_conc <- numeric(length(glyc_48.2B_dilution))

for (i in 1:length(glyc_48.2B_dilution)) {
  df_val <- glyc_48.2B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.2B_lum_values <- c(glyc_48.2B_luminescence[glyc_48.2B_dilution == df_val])
  glyc_48.2B_mean_lum[i] <- mean(glyc_48.2B_lum_values)
  glyc_48.2B_se_lum[i] <- sd(glyc_48.2B_lum_values) / sqrt(length(glyc_48.2B_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.2B_mean_conc[i] <- (glyc_48.2B_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.2B_data <- data.frame(
  glyc_48.2B_dilution_factor = glyc_48.2B_dilution,
  glyc_48.2B_mean_luminescence = glyc_48.2B_mean_lum,
  glyc_48.2B_se = glyc_48.2B_se_lum,
  glyc_48.2B_conc =  glyc_48.2B_mean_conc,
  label = paste0("48.2Bdf.", glyc_48.2B_dilution)
)
glyc_48.2B_mean_lum
glyc_48.2B_mean_conc
```

    [1] 1216.667
    [1] 0.3005995

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.2C_mean_lum <- numeric(length(glyc_48.2C_dilution))
glyc_48.2C_se_lum <- numeric(length(glyc_48.2C_dilution))
glyc_48.2C_mean_conc <- numeric(length(glyc_48.2C_dilution))

for (i in 1:length(glyc_48.2C_dilution)) {
  df_val <- glyc_48.2C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.2C_lum_values <- c(glyc_48.2C_luminescence[glyc_48.2C_dilution == df_val])
  glyc_48.2C_mean_lum[i] <- mean(glyc_48.2C_lum_values)
  glyc_48.2C_se_lum[i] <- sd(glyc_48.2C_lum_values) / sqrt(length(glyc_48.2C_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.2C_mean_conc[i] <- (glyc_48.2C_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.2C_data <- data.frame(
  glyc_48.2C_dilution_factor = glyc_48.2C_dilution,
  glyc_48.2C_mean_luminescence = glyc_48.2C_mean_lum,
  glyc_48.2C_se = glyc_48.2C_se_lum,
  glyc_48.2C_conc =  glyc_48.2C_mean_conc,
  label = paste0("48.2Cdf.", glyc_48.2C_dilution)
)
glyc_48.2C_mean_lum
glyc_48.2C_mean_conc
```

    [1] 1662.667
    [1] 0.3970589

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.2D_mean_lum <- numeric(length(glyc_48.2D_dilution))
glyc_48.2D_se_lum <- numeric(length(glyc_48.2D_dilution))
glyc_48.2D_mean_conc <- numeric(length(glyc_48.2D_dilution))

for (i in 1:length(glyc_48.2D_dilution)) {
  df_val <- glyc_48.2D_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.2D_lum_values <- c(glyc_48.2D_luminescence[glyc_48.2D_dilution == df_val])
  glyc_48.2D_mean_lum[i] <- mean(glyc_48.2D_lum_values)
  glyc_48.2D_se_lum[i] <- sd(glyc_48.2D_lum_values) / sqrt(length(glyc_48.2D_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.2D_mean_conc[i] <- (glyc_48.2D_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.2D_data <- data.frame(
  glyc_48.2D_dilution_factor = glyc_48.2D_dilution,
  glyc_48.2D_mean_luminescence = glyc_48.2D_mean_lum,
  glyc_48.2D_se = glyc_48.2D_se_lum,
  glyc_48.2D_conc =  glyc_48.2D_mean_conc,
  label = paste0("48.2Ddf.", glyc_48.2D_dilution)
)
glyc_48.2D_mean_lum
glyc_48.2D_mean_conc
```

    [1] 8336.333
    [1] 1.840418

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.2E_mean_lum <- numeric(length(glyc_48.2E_dilution))
glyc_48.2E_se_lum <- numeric(length(glyc_48.2E_dilution))
glyc_48.2E_mean_conc <- numeric(length(glyc_48.2E_dilution))

for (i in 1:length(glyc_48.2E_dilution)) {
  df_val <- glyc_48.2E_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.2E_lum_values <- c(glyc_48.2E_luminescence[glyc_48.2E_dilution == df_val])
  glyc_48.2E_mean_lum[i] <- mean(glyc_48.2E_lum_values)
  glyc_48.2E_se_lum[i] <- sd(glyc_48.2E_lum_values) / sqrt(length(glyc_48.2E_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.2E_mean_conc[i] <- (glyc_48.2E_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.2E_data <- data.frame(
  glyc_48.2E_dilution_factor = glyc_48.2E_dilution,
  glyc_48.2E_mean_luminescence = glyc_48.2E_mean_lum,
  glyc_48.2E_se = glyc_48.2E_se_lum,
  glyc_48.2E_conc =  glyc_48.2E_mean_conc,
  label = paste0("48.2Edf.", glyc_48.2E_dilution)
)
glyc_48.2E_mean_lum
glyc_48.2E_mean_conc
```

    [1] 8941.667
    [1] 1.971337

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.2F_mean_lum <- numeric(length(glyc_48.2F_dilution))
glyc_48.2F_se_lum <- numeric(length(glyc_48.2F_dilution))
glyc_48.2F_mean_conc <- numeric(length(glyc_48.2F_dilution))

for (i in 1:length(glyc_48.2F_dilution)) {
  df_val <- glyc_48.2F_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.2F_lum_values <- c(glyc_48.2F_luminescence[glyc_48.2F_dilution == df_val])
  glyc_48.2F_mean_lum[i] <- mean(glyc_48.2F_lum_values)
  glyc_48.2F_se_lum[i] <- sd(glyc_48.2F_lum_values) / sqrt(length(glyc_48.2F_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.2F_mean_conc[i] <- (glyc_48.2F_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.2F_data <- data.frame(
  glyc_48.2F_dilution_factor = glyc_48.2F_dilution,
  glyc_48.2F_mean_luminescence = glyc_48.2F_mean_lum,
  glyc_48.2F_se = glyc_48.2F_se_lum,
  glyc_48.2F_conc =  glyc_48.2F_mean_conc,
  label = paste0("48.2Fdf.", glyc_48.2F_dilution)
)
glyc_48.2F_mean_lum
glyc_48.2F_mean_conc
```

    [1] 11403.67
    [1] 2.50381

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.2G_mean_lum <- numeric(length(glyc_48.2G_dilution))
glyc_48.2G_se_lum <- numeric(length(glyc_48.2G_dilution))
glyc_48.2G_mean_conc <- numeric(length(glyc_48.2G_dilution))

for (i in 1:length(glyc_48.2G_dilution)) {
  df_val <- glyc_48.2G_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.2G_lum_values <- c(glyc_48.2G_luminescence[glyc_48.2G_dilution == df_val])
  glyc_48.2G_mean_lum[i] <- mean(glyc_48.2G_lum_values)
  glyc_48.2G_se_lum[i] <- sd(glyc_48.2G_lum_values) / sqrt(length(glyc_48.2G_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.2G_mean_conc[i] <- (glyc_48.2G_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.2G_data <- data.frame(
  glyc_48.2G_dilution_factor = glyc_48.2G_dilution,
  glyc_48.2G_mean_luminescence = glyc_48.2G_mean_lum,
  glyc_48.2G_se = glyc_48.2G_se_lum,
  glyc_48.2G_conc =  glyc_48.2G_mean_conc,
  label = paste0("48.2Gdf.", glyc_48.2G_dilution)
)
glyc_48.2G_mean_lum
glyc_48.2G_mean_conc
```

    [1] 13084.67
    [1] 2.867372

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.2H_mean_lum <- numeric(length(glyc_48.2H_dilution))
glyc_48.2H_se_lum <- numeric(length(glyc_48.2H_dilution))
glyc_48.2H_mean_conc <- numeric(length(glyc_48.2H_dilution))

for (i in 1:length(glyc_48.2H_dilution)) {
  df_val <- glyc_48.2H_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.2H_lum_values <- c(glyc_48.2H_luminescence[glyc_48.2H_dilution == df_val])
  glyc_48.2H_mean_lum[i] <- mean(glyc_48.2H_lum_values)
  glyc_48.2H_se_lum[i] <- sd(glyc_48.2H_lum_values) / sqrt(length(glyc_48.2H_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.2H_mean_conc[i] <- (glyc_48.2H_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.2H_data <- data.frame(
  glyc_48.2H_dilution_factor = glyc_48.2H_dilution,
  glyc_48.2H_mean_luminescence = glyc_48.2H_mean_lum,
  glyc_48.2H_se = glyc_48.2H_se_lum,
  glyc_48.2H_conc =  glyc_48.2H_mean_conc,
  label = paste0("48.2Hdf.", glyc_48.2H_dilution)
)
glyc_48.2H_mean_lum
glyc_48.2H_mean_conc
```

    [1] 3154.667
    [1] 0.7197437

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.2I_mean_lum <- numeric(length(glyc_48.2I_dilution))
glyc_48.2I_se_lum <- numeric(length(glyc_48.2I_dilution))
glyc_48.2I_mean_conc <- numeric(length(glyc_48.2I_dilution))

for (i in 1:length(glyc_48.2I_dilution)) {
  df_val <- glyc_48.2I_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.2I_lum_values <- c(glyc_48.2I_luminescence[glyc_48.2I_dilution == df_val])
  glyc_48.2I_mean_lum[i] <- mean(glyc_48.2I_lum_values)
  glyc_48.2I_se_lum[i] <- sd(glyc_48.2I_lum_values) / sqrt(length(glyc_48.2I_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.2I_mean_conc[i] <- (glyc_48.2I_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.2I_data <- data.frame(
  glyc_48.2I_dilution_factor = glyc_48.2I_dilution,
  glyc_48.2I_mean_luminescence = glyc_48.2I_mean_lum,
  glyc_48.2I_se = glyc_48.2I_se_lum,
  glyc_48.2I_conc =  glyc_48.2I_mean_conc,
  label = paste0("48.2Idf.", glyc_48.2I_dilution)
)
glyc_48.2I_mean_lum
glyc_48.2I_mean_conc
```

    [1] 2426.333
    [1] 0.5622222

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.2J_mean_lum <- numeric(length(glyc_48.2J_dilution))
glyc_48.2J_se_lum <- numeric(length(glyc_48.2J_dilution))
glyc_48.2J_mean_conc <- numeric(length(glyc_48.2J_dilution))

for (i in 1:length(glyc_48.2J_dilution)) {
  df_val <- glyc_48.2J_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.2J_lum_values <- c(glyc_48.2J_luminescence[glyc_48.2J_dilution == df_val])
  glyc_48.2J_mean_lum[i] <- mean(glyc_48.2J_lum_values)
  glyc_48.2J_se_lum[i] <- sd(glyc_48.2J_lum_values) / sqrt(length(glyc_48.2J_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.2J_mean_conc[i] <- (glyc_48.2J_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.2J_data <- data.frame(
  glyc_48.2J_dilution_factor = glyc_48.2J_dilution,
  glyc_48.2J_mean_luminescence = glyc_48.2J_mean_lum,
  glyc_48.2J_se = glyc_48.2J_se_lum,
  glyc_48.2J_conc =  glyc_48.2J_mean_conc,
  label = paste0("48.2Jdf.", glyc_48.2J_dilution)
)
glyc_48.2J_mean_lum
glyc_48.2J_mean_conc
```

    [1] 3480.333
    [1] 0.7901779

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.7A_mean_lum <- numeric(length(glyc_48.7A_dilution))
glyc_48.7A_se_lum <- numeric(length(glyc_48.7A_dilution))
glyc_48.7A_mean_conc <- numeric(length(glyc_48.7A_dilution))

for (i in 1:length(glyc_48.7A_dilution)) {
  df_val <- glyc_48.7A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.7A_lum_values <- c(glyc_48.7A_luminescence[glyc_48.7A_dilution == df_val])
  glyc_48.7A_mean_lum[i] <- mean(glyc_48.7A_lum_values)
  glyc_48.7A_se_lum[i] <- sd(glyc_48.7A_lum_values) / sqrt(length(glyc_48.7A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.7A_mean_conc[i] <- (glyc_48.7A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.7A_data <- data.frame(
  glyc_48.7A_dilution_factor = glyc_48.7A_dilution,
  glyc_48.7A_mean_luminescence = glyc_48.7A_mean_lum,
  glyc_48.7A_se = glyc_48.7A_se_lum,
  glyc_48.7A_conc =  glyc_48.7A_mean_conc,
  label = paste0("48.7Adf.", glyc_48.7A_dilution)
)
glyc_48.7A_mean_lum
glyc_48.7A_mean_conc
```

    [1] 7269.333
    [1] 1.60965

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.7B_mean_lum <- numeric(length(glyc_48.7B_dilution))
glyc_48.7B_se_lum <- numeric(length(glyc_48.7B_dilution))
glyc_48.7B_mean_conc <- numeric(length(glyc_48.7B_dilution))

for (i in 1:length(glyc_48.7B_dilution)) {
  df_val <- glyc_48.7B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.7B_lum_values <- c(glyc_48.7B_luminescence[glyc_48.7B_dilution == df_val])
  glyc_48.7B_mean_lum[i] <- mean(glyc_48.7B_lum_values)
  glyc_48.7B_se_lum[i] <- sd(glyc_48.7B_lum_values) / sqrt(length(glyc_48.7B_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.7B_mean_conc[i] <- (glyc_48.7B_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.7B_data <- data.frame(
  glyc_48.7B_dilution_factor = glyc_48.7B_dilution,
  glyc_48.7B_mean_luminescence = glyc_48.7B_mean_lum,
  glyc_48.7B_se = glyc_48.7B_se_lum,
  glyc_48.7B_conc =  glyc_48.7B_mean_conc,
  label = paste0("48.7Bdf.", glyc_48.7B_dilution)
)
glyc_48.7B_mean_lum
glyc_48.7B_mean_conc
```

    [1] 4930
    [1] 1.103707

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.7C_mean_lum <- numeric(length(glyc_48.7C_dilution))
glyc_48.7C_se_lum <- numeric(length(glyc_48.7C_dilution))
glyc_48.7C_mean_conc <- numeric(length(glyc_48.7C_dilution))

for (i in 1:length(glyc_48.7C_dilution)) {
  df_val <- glyc_48.7C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.7C_lum_values <- c(glyc_48.7C_luminescence[glyc_48.7C_dilution == df_val])
  glyc_48.7C_mean_lum[i] <- mean(glyc_48.7C_lum_values)
  glyc_48.7C_se_lum[i] <- sd(glyc_48.7C_lum_values) / sqrt(length(glyc_48.7C_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.7C_mean_conc[i] <- (glyc_48.7C_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.7C_data <- data.frame(
  glyc_48.7C_dilution_factor = glyc_48.7C_dilution,
  glyc_48.7C_mean_luminescence = glyc_48.7C_mean_lum,
  glyc_48.7C_se = glyc_48.7C_se_lum,
  glyc_48.7C_conc =  glyc_48.7C_mean_conc,
  label = paste0("48.7Cdf.", glyc_48.7C_dilution)
)
glyc_48.7C_mean_lum
glyc_48.7C_mean_conc
```

    [1] 10708.33
    [1] 2.353426

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.7D_mean_lum <- numeric(length(glyc_48.7D_dilution))
glyc_48.7D_se_lum <- numeric(length(glyc_48.7D_dilution))
glyc_48.7D_mean_conc <- numeric(length(glyc_48.7D_dilution))

for (i in 1:length(glyc_48.7D_dilution)) {
  df_val <- glyc_48.7D_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.7D_lum_values <- c(glyc_48.7D_luminescence[glyc_48.7D_dilution == df_val])
  glyc_48.7D_mean_lum[i] <- mean(glyc_48.7D_lum_values)
  glyc_48.7D_se_lum[i] <- sd(glyc_48.7D_lum_values) / sqrt(length(glyc_48.7D_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.7D_mean_conc[i] <- (glyc_48.7D_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.7D_data <- data.frame(
  glyc_48.7D_dilution_factor = glyc_48.7D_dilution,
  glyc_48.7D_mean_luminescence = glyc_48.7D_mean_lum,
  glyc_48.7D_se = glyc_48.7D_se_lum,
  glyc_48.7D_conc =  glyc_48.7D_mean_conc,
  label = paste0("48.7Ddf.", glyc_48.7D_dilution)
)
glyc_48.7D_mean_lum
glyc_48.7D_mean_conc
```

    [1] 4910.333
    [1] 1.099454

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.7E_mean_lum <- numeric(length(glyc_48.7E_dilution))
glyc_48.7E_se_lum <- numeric(length(glyc_48.7E_dilution))
glyc_48.7E_mean_conc <- numeric(length(glyc_48.7E_dilution))

for (i in 1:length(glyc_48.7E_dilution)) {
  df_val <- glyc_48.7E_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.7E_lum_values <- c(glyc_48.7E_luminescence[glyc_48.7E_dilution == df_val])
  glyc_48.7E_mean_lum[i] <- mean(glyc_48.7E_lum_values)
  glyc_48.7E_se_lum[i] <- sd(glyc_48.7E_lum_values) / sqrt(length(glyc_48.7E_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.7E_mean_conc[i] <- (glyc_48.7E_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.7E_data <- data.frame(
  glyc_48.7E_dilution_factor = glyc_48.7E_dilution,
  glyc_48.7E_mean_luminescence = glyc_48.7E_mean_lum,
  glyc_48.7E_se = glyc_48.7E_se_lum,
  glyc_48.7E_conc =  glyc_48.7E_mean_conc,
  label = paste0("48.7Edf.", glyc_48.7E_dilution)
)
glyc_48.7E_mean_lum
glyc_48.7E_mean_conc
```

    [1] 9916.667
    [1] 2.182207

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.7F_mean_lum <- numeric(length(glyc_48.7F_dilution))
glyc_48.7F_se_lum <- numeric(length(glyc_48.7F_dilution))
glyc_48.7F_mean_conc <- numeric(length(glyc_48.7F_dilution))

for (i in 1:length(glyc_48.7F_dilution)) {
  df_val <- glyc_48.7F_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.7F_lum_values <- c(glyc_48.7F_luminescence[glyc_48.7F_dilution == df_val])
  glyc_48.7F_mean_lum[i] <- mean(glyc_48.7F_lum_values)
  glyc_48.7F_se_lum[i] <- sd(glyc_48.7F_lum_values) / sqrt(length(glyc_48.7F_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.7F_mean_conc[i] <- (glyc_48.7F_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.7F_data <- data.frame(
  glyc_48.7F_dilution_factor = glyc_48.7F_dilution,
  glyc_48.7F_mean_luminescence = glyc_48.7F_mean_lum,
  glyc_48.7F_se = glyc_48.7F_se_lum,
  glyc_48.7F_conc =  glyc_48.7F_mean_conc,
  label = paste0("48.7Fdf.", glyc_48.7F_dilution)
)
glyc_48.7F_mean_lum
glyc_48.7F_mean_conc
```

    [1] 1566
    [1] 0.3761521

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.7G_mean_lum <- numeric(length(glyc_48.7G_dilution))
glyc_48.7G_se_lum <- numeric(length(glyc_48.7G_dilution))
glyc_48.7G_mean_conc <- numeric(length(glyc_48.7G_dilution))

for (i in 1:length(glyc_48.7G_dilution)) {
  df_val <- glyc_48.7G_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.7G_lum_values <- c(glyc_48.7G_luminescence[glyc_48.7G_dilution == df_val])
  glyc_48.7G_mean_lum[i] <- mean(glyc_48.7G_lum_values)
  glyc_48.7G_se_lum[i] <- sd(glyc_48.7G_lum_values) / sqrt(length(glyc_48.7G_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.7G_mean_conc[i] <- (glyc_48.7G_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.7G_data <- data.frame(
  glyc_48.7G_dilution_factor = glyc_48.7G_dilution,
  glyc_48.7G_mean_luminescence = glyc_48.7G_mean_lum,
  glyc_48.7G_se = glyc_48.7G_se_lum,
  glyc_48.7G_conc =  glyc_48.7G_mean_conc,
  label = paste0("48.7Gdf.", glyc_48.7G_dilution)
)
glyc_48.7G_mean_lum
glyc_48.7G_mean_conc
```

    [1] 11123
    [1] 2.443109

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.7H_mean_lum <- numeric(length(glyc_48.7H_dilution))
glyc_48.7H_se_lum <- numeric(length(glyc_48.7H_dilution))
glyc_48.7H_mean_conc <- numeric(length(glyc_48.7H_dilution))

for (i in 1:length(glyc_48.7H_dilution)) {
  df_val <- glyc_48.7H_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.7H_lum_values <- c(glyc_48.7H_luminescence[glyc_48.7H_dilution == df_val])
  glyc_48.7H_mean_lum[i] <- mean(glyc_48.7H_lum_values)
  glyc_48.7H_se_lum[i] <- sd(glyc_48.7H_lum_values) / sqrt(length(glyc_48.7H_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.7H_mean_conc[i] <- (glyc_48.7H_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.7H_data <- data.frame(
  glyc_48.7H_dilution_factor = glyc_48.7H_dilution,
  glyc_48.7H_mean_luminescence = glyc_48.7H_mean_lum,
  glyc_48.7H_se = glyc_48.7H_se_lum,
  glyc_48.7H_conc =  glyc_48.7H_mean_conc,
  label = paste0("48.7Hdf.", glyc_48.7H_dilution)
)
glyc_48.7H_mean_lum
glyc_48.7H_mean_conc
```

    [1] 15968
    [1] 3.490969

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.7I_mean_lum <- numeric(length(glyc_48.7I_dilution))
glyc_48.7I_se_lum <- numeric(length(glyc_48.7I_dilution))
glyc_48.7I_mean_conc <- numeric(length(glyc_48.7I_dilution))

for (i in 1:length(glyc_48.7I_dilution)) {
  df_val <- glyc_48.7I_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.7I_lum_values <- c(glyc_48.7I_luminescence[glyc_48.7I_dilution == df_val])
  glyc_48.7I_mean_lum[i] <- mean(glyc_48.7I_lum_values)
  glyc_48.7I_se_lum[i] <- sd(glyc_48.7I_lum_values) / sqrt(length(glyc_48.7I_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.7I_mean_conc[i] <- (glyc_48.7I_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.7I_data <- data.frame(
  glyc_48.7I_dilution_factor = glyc_48.7I_dilution,
  glyc_48.7I_mean_luminescence = glyc_48.7I_mean_lum,
  glyc_48.7I_se = glyc_48.7I_se_lum,
  glyc_48.7I_conc =  glyc_48.7I_mean_conc,
  label = paste0("48.7Idf.", glyc_48.7I_dilution)
)
glyc_48.7I_mean_lum
glyc_48.7I_mean_conc
```

    [1] 5176.333
    [1] 1.156983

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_48.7J_mean_lum <- numeric(length(glyc_48.7J_dilution))
glyc_48.7J_se_lum <- numeric(length(glyc_48.7J_dilution))
glyc_48.7J_mean_conc <- numeric(length(glyc_48.7J_dilution))

for (i in 1:length(glyc_48.7J_dilution)) {
  df_val <- glyc_48.7J_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_48.7J_lum_values <- c(glyc_48.7J_luminescence[glyc_48.7J_dilution == df_val])
  glyc_48.7J_mean_lum[i] <- mean(glyc_48.7J_lum_values)
  glyc_48.7J_se_lum[i] <- sd(glyc_48.7J_lum_values) / sqrt(length(glyc_48.7J_lum_values))
  # Calculate concentration from mean luminescence
  glyc_48.7J_mean_conc[i] <- (glyc_48.7J_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_48.7J_data <- data.frame(
  glyc_48.7J_dilution_factor = glyc_48.7J_dilution,
  glyc_48.7J_mean_luminescence = glyc_48.7J_mean_lum,
  glyc_48.7J_se = glyc_48.7J_se_lum,
  glyc_48.7J_conc =  glyc_48.7J_mean_conc,
  label = paste0("48.7Jdf.", glyc_48.7J_dilution)
)
glyc_48.7J_mean_lum
glyc_48.7J_mean_conc
```

    [1] 13174
    [1] 2.886692

### 1.1.4 Plot glycogen standard curve, sample points

``` r
# Create the plot
glyc_plot <- ggplot(glycogen_summary_data, aes(x = glyc_concentration, y = glyc_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glyc_mean_luminescence - glyc_se, ymax = glyc_mean_luminescence + glyc_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +
  
   geom_errorbar(data = glyc_0.0A_data, aes(x = glyc_0.0A_conc, y = glyc_0.0A_mean_luminescence,
                ymin = glyc_0.0A_mean_luminescence - glyc_0.0A_se, ymax = glyc_0.0A_mean_luminescence + glyc_0.0A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_0.0A_data, aes(x = glyc_0.0A_conc, y = glyc_0.0A_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_0.0B_data, aes(x = glyc_0.0B_conc, y = glyc_0.0B_mean_luminescence,
                ymin = glyc_0.0B_mean_luminescence - glyc_0.0B_se, ymax = glyc_0.0B_mean_luminescence + glyc_0.0B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_0.0B_data, aes(x = glyc_0.0B_conc, y = glyc_0.0B_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_0.0C_data, aes(x = glyc_0.0C_conc, y = glyc_0.0C_mean_luminescence,
                ymin = glyc_0.0C_mean_luminescence - glyc_0.0C_se, ymax = glyc_0.0C_mean_luminescence + glyc_0.0C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_0.0C_data, aes(x = glyc_0.0C_conc, y = glyc_0.0C_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_0.0D_data, aes(x = glyc_0.0D_conc, y = glyc_0.0D_mean_luminescence,
                ymin = glyc_0.0D_mean_luminescence - glyc_0.0D_se, ymax = glyc_0.0D_mean_luminescence + glyc_0.0D_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_0.0D_data, aes(x = glyc_0.0D_conc, y = glyc_0.0D_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_0.0E_data, aes(x = glyc_0.0E_conc, y = glyc_0.0E_mean_luminescence,
                ymin = glyc_0.0E_mean_luminescence - glyc_0.0E_se, ymax = glyc_0.0E_mean_luminescence + glyc_0.0E_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_0.0E_data, aes(x = glyc_0.0E_conc, y = glyc_0.0E_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_0.0F_data, aes(x = glyc_0.0F_conc, y = glyc_0.0F_mean_luminescence,
                ymin = glyc_0.0F_mean_luminescence - glyc_0.0F_se, ymax = glyc_0.0F_mean_luminescence + glyc_0.0F_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_0.0F_data, aes(x = glyc_0.0F_conc, y = glyc_0.0F_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_0.0G_data, aes(x = glyc_0.0G_conc, y = glyc_0.0G_mean_luminescence,
                ymin = glyc_0.0G_mean_luminescence - glyc_0.0G_se, ymax = glyc_0.0G_mean_luminescence + glyc_0.0G_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_0.0G_data, aes(x = glyc_0.0G_conc, y = glyc_0.0G_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_0.0H_data, aes(x = glyc_0.0H_conc, y = glyc_0.0H_mean_luminescence,
                ymin = glyc_0.0H_mean_luminescence - glyc_0.0H_se, ymax = glyc_0.0H_mean_luminescence + glyc_0.0H_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_0.0H_data, aes(x = glyc_0.0H_conc, y = glyc_0.0H_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_0.0I_data, aes(x = glyc_0.0I_conc, y = glyc_0.0I_mean_luminescence,
                ymin = glyc_0.0I_mean_luminescence - glyc_0.0I_se, ymax = glyc_0.0I_mean_luminescence + glyc_0.0I_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_0.0I_data, aes(x = glyc_0.0I_conc, y = glyc_0.0I_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_0.0J_data, aes(x = glyc_0.0J_conc, y = glyc_0.0J_mean_luminescence,
                ymin = glyc_0.0J_mean_luminescence - glyc_0.0J_se, ymax = glyc_0.0J_mean_luminescence + glyc_0.0J_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_0.0J_data, aes(x = glyc_0.0J_conc, y = glyc_0.0J_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_48.0A_data, aes(x = glyc_48.0A_conc, y = glyc_48.0A_mean_luminescence,
                ymin = glyc_48.0A_mean_luminescence - glyc_48.0A_se, ymax = glyc_48.0A_mean_luminescence + glyc_48.0A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.0A_data, aes(x = glyc_48.0A_conc, y = glyc_48.0A_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_48.0B_data, aes(x = glyc_48.0B_conc, y = glyc_48.0B_mean_luminescence,
                ymin = glyc_48.0B_mean_luminescence - glyc_48.0B_se, ymax = glyc_48.0B_mean_luminescence + glyc_48.0B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.0B_data, aes(x = glyc_48.0B_conc, y = glyc_48.0B_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_48.0C_data, aes(x = glyc_48.0C_conc, y = glyc_48.0C_mean_luminescence,
                ymin = glyc_48.0C_mean_luminescence - glyc_48.0C_se, ymax = glyc_48.0C_mean_luminescence + glyc_48.0C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.0C_data, aes(x = glyc_48.0C_conc, y = glyc_48.0C_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_48.0D_data, aes(x = glyc_48.0D_conc, y = glyc_48.0D_mean_luminescence,
                ymin = glyc_48.0D_mean_luminescence - glyc_48.0D_se, ymax = glyc_48.0D_mean_luminescence + glyc_48.0D_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.0D_data, aes(x = glyc_48.0D_conc, y = glyc_48.0D_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_48.0E_data, aes(x = glyc_48.0E_conc, y = glyc_48.0E_mean_luminescence,
                ymin = glyc_48.0E_mean_luminescence - glyc_48.0E_se, ymax = glyc_48.0E_mean_luminescence + glyc_48.0E_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.0E_data, aes(x = glyc_48.0E_conc, y = glyc_48.0E_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_48.0F_data, aes(x = glyc_48.0F_conc, y = glyc_48.0F_mean_luminescence,
                ymin = glyc_48.0F_mean_luminescence - glyc_48.0F_se, ymax = glyc_48.0F_mean_luminescence + glyc_48.0F_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.0F_data, aes(x = glyc_48.0F_conc, y = glyc_48.0F_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_48.0G_data, aes(x = glyc_48.0G_conc, y = glyc_48.0G_mean_luminescence,
                ymin = glyc_48.0G_mean_luminescence - glyc_48.0G_se, ymax = glyc_48.0G_mean_luminescence + glyc_48.0G_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.0G_data, aes(x = glyc_48.0G_conc, y = glyc_48.0G_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_48.0H_data, aes(x = glyc_48.0H_conc, y = glyc_48.0H_mean_luminescence,
                ymin = glyc_48.0H_mean_luminescence - glyc_48.0H_se, ymax = glyc_48.0H_mean_luminescence + glyc_48.0H_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.0H_data, aes(x = glyc_48.0H_conc, y = glyc_48.0H_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_48.0I_data, aes(x = glyc_48.0I_conc, y = glyc_48.0I_mean_luminescence,
                ymin = glyc_48.0I_mean_luminescence - glyc_48.0I_se, ymax = glyc_48.0I_mean_luminescence + glyc_48.0I_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.0I_data, aes(x = glyc_48.0I_conc, y = glyc_48.0I_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_48.0J_data, aes(x = glyc_48.0J_conc, y = glyc_48.0J_mean_luminescence,
                ymin = glyc_48.0J_mean_luminescence - glyc_48.0J_se, ymax = glyc_48.0J_mean_luminescence + glyc_48.0J_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.0J_data, aes(x = glyc_48.0J_conc, y = glyc_48.0J_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_48.2A_data, aes(x = glyc_48.2A_conc, y = glyc_48.2A_mean_luminescence,
                ymin = glyc_48.2A_mean_luminescence - glyc_48.2A_se, ymax = glyc_48.2A_mean_luminescence + glyc_48.2A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.2A_data, aes(x = glyc_48.2A_conc, y = glyc_48.2A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
      geom_errorbar(data = glyc_48.2B_data, aes(x = glyc_48.2B_conc, y = glyc_48.2B_mean_luminescence,
                ymin = glyc_48.2B_mean_luminescence - glyc_48.2B_se, ymax = glyc_48.2B_mean_luminescence + glyc_48.2B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.2B_data, aes(x = glyc_48.2B_conc, y = glyc_48.2B_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_48.2C_data, aes(x = glyc_48.2C_conc, y = glyc_48.2C_mean_luminescence,
                ymin = glyc_48.2C_mean_luminescence - glyc_48.2C_se, ymax = glyc_48.2C_mean_luminescence + glyc_48.2C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.2C_data, aes(x = glyc_48.2C_conc, y = glyc_48.2C_mean_luminescence,
             color = label, shape = label), size = 4) +

   geom_errorbar(data = glyc_48.2D_data, aes(x = glyc_48.2D_conc, y = glyc_48.2D_mean_luminescence,
                ymin = glyc_48.2D_mean_luminescence - glyc_48.2D_se, ymax = glyc_48.2D_mean_luminescence + glyc_48.2D_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.2D_data, aes(x = glyc_48.2D_conc, y = glyc_48.2D_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_48.2E_data, aes(x = glyc_48.2E_conc, y = glyc_48.2E_mean_luminescence,
                ymin = glyc_48.2E_mean_luminescence - glyc_48.2E_se, ymax = glyc_48.2E_mean_luminescence + glyc_48.2E_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.2E_data, aes(x = glyc_48.2E_conc, y = glyc_48.2E_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_48.2F_data, aes(x = glyc_48.2F_conc, y = glyc_48.2F_mean_luminescence,
                ymin = glyc_48.2F_mean_luminescence - glyc_48.2F_se, ymax = glyc_48.2F_mean_luminescence + glyc_48.2F_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.2F_data, aes(x = glyc_48.2F_conc, y = glyc_48.2F_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_48.2G_data, aes(x = glyc_48.2G_conc, y = glyc_48.2G_mean_luminescence,
                ymin = glyc_48.2G_mean_luminescence - glyc_48.2G_se, ymax = glyc_48.2G_mean_luminescence + glyc_48.2G_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.2G_data, aes(x = glyc_48.2G_conc, y = glyc_48.2G_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_48.2H_data, aes(x = glyc_48.2H_conc, y = glyc_48.2H_mean_luminescence,
                ymin = glyc_48.2H_mean_luminescence - glyc_48.2H_se, ymax = glyc_48.2H_mean_luminescence + glyc_48.2H_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.2H_data, aes(x = glyc_48.2H_conc, y = glyc_48.2H_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_48.2I_data, aes(x = glyc_48.2I_conc, y = glyc_48.2I_mean_luminescence,
                ymin = glyc_48.2I_mean_luminescence - glyc_48.2I_se, ymax = glyc_48.2I_mean_luminescence + glyc_48.2I_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.2I_data, aes(x = glyc_48.2I_conc, y = glyc_48.2I_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_48.2J_data, aes(x = glyc_48.2J_conc, y = glyc_48.2J_mean_luminescence,
                ymin = glyc_48.2J_mean_luminescence - glyc_48.2J_se, ymax = glyc_48.2J_mean_luminescence + glyc_48.2J_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.2J_data, aes(x = glyc_48.2J_conc, y = glyc_48.2J_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_48.7A_data, aes(x = glyc_48.7A_conc, y = glyc_48.7A_mean_luminescence,
                ymin = glyc_48.7A_mean_luminescence - glyc_48.7A_se, ymax = glyc_48.7A_mean_luminescence + glyc_48.7A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.7A_data, aes(x = glyc_48.7A_conc, y = glyc_48.7A_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_48.7B_data, aes(x = glyc_48.7B_conc, y = glyc_48.7B_mean_luminescence,
                ymin = glyc_48.7B_mean_luminescence - glyc_48.7B_se, ymax = glyc_48.7B_mean_luminescence + glyc_48.7B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.7B_data, aes(x = glyc_48.7B_conc, y = glyc_48.7B_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_48.7C_data, aes(x = glyc_48.7C_conc, y = glyc_48.7C_mean_luminescence,
                ymin = glyc_48.7C_mean_luminescence - glyc_48.7C_se, ymax = glyc_48.7C_mean_luminescence + glyc_48.7C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.7C_data, aes(x = glyc_48.7C_conc, y = glyc_48.7C_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_48.7D_data, aes(x = glyc_48.7D_conc, y = glyc_48.7D_mean_luminescence,
                ymin = glyc_48.7D_mean_luminescence - glyc_48.7D_se, ymax = glyc_48.7D_mean_luminescence + glyc_48.7D_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.7D_data, aes(x = glyc_48.7D_conc, y = glyc_48.7D_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_48.7E_data, aes(x = glyc_48.7E_conc, y = glyc_48.7E_mean_luminescence,
                ymin = glyc_48.7E_mean_luminescence - glyc_48.7E_se, ymax = glyc_48.7E_mean_luminescence + glyc_48.7E_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.7E_data, aes(x = glyc_48.7E_conc, y = glyc_48.7E_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  
    geom_errorbar(data = glyc_48.7F_data, aes(x = glyc_48.7F_conc, y = glyc_48.7F_mean_luminescence,
                ymin = glyc_48.7F_mean_luminescence - glyc_48.7F_se, ymax = glyc_48.7F_mean_luminescence + glyc_48.7F_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.7F_data, aes(x = glyc_48.7F_conc, y = glyc_48.7F_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_48.7G_data, aes(x = glyc_48.7G_conc, y = glyc_48.7G_mean_luminescence,
                ymin = glyc_48.7G_mean_luminescence - glyc_48.7G_se, ymax = glyc_48.7G_mean_luminescence + glyc_48.7G_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.7G_data, aes(x = glyc_48.7G_conc, y = glyc_48.7G_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_48.7H_data, aes(x = glyc_48.7H_conc, y = glyc_48.7H_mean_luminescence,
                ymin = glyc_48.7H_mean_luminescence - glyc_48.7H_se, ymax = glyc_48.7H_mean_luminescence + glyc_48.7H_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.7H_data, aes(x = glyc_48.7H_conc, y = glyc_48.7H_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_48.7I_data, aes(x = glyc_48.7I_conc, y = glyc_48.7I_mean_luminescence,
                ymin = glyc_48.7I_mean_luminescence - glyc_48.7I_se, ymax = glyc_48.7I_mean_luminescence + glyc_48.7I_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.7I_data, aes(x = glyc_48.7I_conc, y = glyc_48.7I_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_48.7J_data, aes(x = glyc_48.7J_conc, y = glyc_48.7J_mean_luminescence,
                ymin = glyc_48.7J_mean_luminescence - glyc_48.7J_se, ymax = glyc_48.7J_mean_luminescence + glyc_48.7J_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_48.7J_data, aes(x = glyc_48.7J_conc, y = glyc_48.7J_mean_luminescence,
             color = label, shape = label), size = 4) +

  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                               "0.0Adf.20" = "darkorange",
                                "0.0Bdf.20" = "darkgreen",
                                "0.0Cdf.20" = "purple",
                                "0.0Ddf.20" = "brown",
                                "0.0Edf.20" = "green3",
                                "0.0Fdf.20" = "firebrick1",
                                "0.0Gdf.20" = "cyan",
                                "0.0Hdf.20" = "yellow3",
                                "0.0Idf.20" = "thistle3",
                                "0.0Jdf.20" = "azure4",
                                "48.0Adf.20" = "bisque4",
                                "48.0Bdf.20" = "chartreuse",
                                "48.0Cdf.20" = "chocolate4",
                                "48.0Ddf.20" = "darkslategray",
                                "48.0Edf.20" = "deeppink2",
                                "48.0Fdf.20" = "goldenrod3",
                                "48.0Gdf.20" = "hotpink3",
                                "48.0Hdf.20" = "mediumpurple2",
                                "48.0Idf.20" = "yellow",
                                "48.0Jdf.20" = "gray1",
                                "48.2Adf.20" = "darkcyan",
                                "48.2Bdf.20" = "darkorange",
                                "48.2Cdf.20" = "darkgreen",
                                "48.2Ddf.20" = "purple",
                                "48.2Edf.20" = "brown",
                                "48.2Fdf.20" = "green3",
                                "48.2Gdf.20" = "firebrick1",
                                "48.2Hdf.20" = "cyan",
                                "48.2Idf.20" = "yellow3",
                                "48.2Jdf.20" = "thistle3",
                                "48.7Adf.20" = "azure4",
                                "48.7Bdf.20" = "bisque4",
                                "48.7Cdf.20" = "chartreuse",
                                "48.7Ddf.20" = "chocolate4",
                                "48.7Edf.20" = "darkslategray",
                                "48.7Fdf.20" = "deeppink2",
                                "48.7Gdf.20" = "goldenrod3",
                                "48.7Hdf.20" = "hotpink3",
                                "48.7Idf.20" = "mediumpurple2",
                                "48.7Jdf.20" = "yellow"
                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                "0.0Adf.20" = 16,
                                "0.0Bdf.20" = 17,
                                "0.0Cdf.20" = 15,
                                "0.0Ddf.20" = 18,
                                "0.0Edf.20" = 8,
                                "0.0Fdf.20" = 4,
                                "0.0Gdf.20" = 5,
                                "0.0Hdf.20" = 0,
                                "0.0Idf.20" = 2,
                                "0.0Jdf.20" = 19,
                                "48.0Adf.20" = 20,
                                "48.0Bdf.20" = 17,
                                "48.0Cdf.20" = 15,
                                "48.0Ddf.20" = 18,
                                "48.0Edf.20" = 8,
                                "48.0Fdf.20" = 4,
                                "48.0Gdf.20" = 0,
                                "48.0Hdf.20" = 2,
                                "48.0Idf.20" = 19,
                                "48.0Jdf.20" = 20,
                                "48.2Adf.20" = 5, 
                                "48.2Bdf.20" = 0, 
                                "48.2Cdf.20" = 2, 
                                "48.2Ddf.20" = 19, 
                                "48.2Edf.20" = 20, 
                                "48.2Fdf.20" = 8, 
                                "48.2Gdf.20" = 4,
                                "48.2Hdf.20" = 5, 
                                "48.2Idf.20" = 17, 
                                "48.2Jdf.20" = 16, 
                                "48.7Adf.20" = 18, 
                                "48.7Bdf.20" = 5, 
                                "48.7Cdf.20" = 0, 
                                "48.7Ddf.20" = 19, 
                                "48.7Edf.20" = 20, 
                                "48.7Fdf.20" = 8, 
                                "48.7Gdf.20" = 4,
                                "48.7Hdf.20" = 2, 
                                "48.7Idf.20" = 16, 
                                "48.7Jdf.20" = 17
                                )) +
  scale_linetype_manual(name = "",
                        values = c("Std Curve Best Fit Line" = "dashed")) +
  annotate("label", x = max(glycogen_summary_data$glyc_concentration) * 0.75, 
           y = max(glycogen_summary_data$glyc_mean_luminescence) * 0.15,
           label = sprintf("y = %.2fx + %.2f\nR² = %.4f", glyc_slope, glyc_intercept, glyc_r_squared),
           size = 3.5, fontface = "bold", fill = "white", 
           color = "coral", label.padding = unit(0.3, "lines")) +
  labs(
    title = "glycogen Standard Curve",
    x = "glycogen Concentration (µg/µL)",
    y = "Luminescence",
    caption = "Error bars represent standard error of the mean (SEM)"
  ) +
  scale_x_continuous(breaks = glyc_concentrations) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_line(linetype = "dashed", color = "grey70")
  )

# Display the plot
glyc_plot
```

![](Gen5-20260417-mgig-glycogenglo_files/figure-gfm/plot%20glycogen%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glyc_0.0A_data
glyc_0.0B_data
glyc_0.0C_data
glyc_0.0D_data
glyc_0.0E_data
glyc_0.0F_data
glyc_0.0G_data
glyc_0.0H_data
glyc_0.0I_data
glyc_0.0J_data

glyc_48.0A_data
glyc_48.0B_data
glyc_48.0C_data
glyc_48.0D_data
glyc_48.0E_data
glyc_48.0F_data
glyc_48.0G_data
glyc_48.0H_data
glyc_48.0I_data
glyc_48.0J_data

glyc_48.2A_data
glyc_48.2B_data
glyc_48.2C_data
glyc_48.2D_data
glyc_48.2E_data
glyc_48.2F_data
glyc_48.2G_data
glyc_48.2H_data
glyc_48.2I_data
glyc_48.2J_data

glyc_48.7A_data
glyc_48.7B_data
glyc_48.7C_data
glyc_48.7D_data
glyc_48.7E_data
glyc_48.7F_data
glyc_48.7G_data
glyc_48.7H_data
glyc_48.7I_data
glyc_48.7J_data

# Print summary statistics
cat("glycogen Standard Curve Summary:\n")
cat(rep("=", 50), "\n", sep = "")
for (i in 1:nrow(glycogen_summary_data)) {
  cat(sprintf("Concentration: %g µg/µL\n", glycogen_summary_data$glyc_concentration[i]))
  cat(sprintf("  Mean Luminescence: %.2f\n",glycogen_summary_data$glyc_mean_luminescence[i]))
  cat(sprintf("  Standard Error: %.2f\n", glycogen_summary_data$glyc_se[i]))
  cat(sprintf("  CV%%: %.2f%%\n\n", glycogen_summary_data$glyc_cv[i]))
}
```

      glyc_0.0A_dilution_factor glyc_0.0A_mean_luminescence glyc_0.0A_se
    1                        20                    11297.33     92.95399
      glyc_0.0A_conc     label
    1       2.480813 0.0Adf.20
      glyc_0.0B_dilution_factor glyc_0.0B_mean_luminescence glyc_0.0B_se
    1                        20                    11859.67     119.3738
      glyc_0.0B_conc     label
    1       2.602433 0.0Bdf.20
      glyc_0.0C_dilution_factor glyc_0.0C_mean_luminescence glyc_0.0C_se
    1                        20                    21764.67     506.6933
      glyc_0.0C_conc     label
    1       4.744653 0.0Cdf.20
      glyc_0.0D_dilution_factor glyc_0.0D_mean_luminescence glyc_0.0D_se
    1                        20                    10228.33     73.04413
      glyc_0.0D_conc     label
    1       2.249613 0.0Ddf.20
      glyc_0.0E_dilution_factor glyc_0.0E_mean_luminescence glyc_0.0E_se
    1                        20                    22724.33     13.83635
      glyc_0.0E_conc     label
    1       4.952207 0.0Edf.20
      glyc_0.0F_dilution_factor glyc_0.0F_mean_luminescence glyc_0.0F_se
    1                        20                    20201.67      190.286
      glyc_0.0F_conc     label
    1       4.406613 0.0Fdf.20
      glyc_0.0G_dilution_factor glyc_0.0G_mean_luminescence glyc_0.0G_se
    1                        20                    13244.33     603.0131
      glyc_0.0G_conc     label
    1       2.901904 0.0Gdf.20
      glyc_0.0H_dilution_factor glyc_0.0H_mean_luminescence glyc_0.0H_se
    1                        20                    2286.667     49.46491
      glyc_0.0H_conc     label
    1      0.5320155 0.0Hdf.20
      glyc_0.0I_dilution_factor glyc_0.0I_mean_luminescence glyc_0.0I_se
    1                        20                       10592     22.81082
      glyc_0.0I_conc     label
    1       2.328266 0.0Idf.20
      glyc_0.0J_dilution_factor glyc_0.0J_mean_luminescence glyc_0.0J_se
    1                        20                    2814.667      89.3725
      glyc_0.0J_conc     label
    1      0.6462097 0.0Jdf.20
      glyc_48.0A_dilution_factor glyc_48.0A_mean_luminescence glyc_48.0A_se
    1                         20                     1296.667      55.85199
      glyc_48.0A_conc      label
    1       0.3179016 48.0Adf.20
      glyc_48.0B_dilution_factor glyc_48.0B_mean_luminescence glyc_48.0B_se
    1                         20                     771.3333      61.19187
      glyc_48.0B_conc      label
    1       0.2042842 48.0Bdf.20
      glyc_48.0C_dilution_factor glyc_48.0C_mean_luminescence glyc_48.0C_se
    1                         20                     2372.333      22.16855
      glyc_48.0C_conc      label
    1       0.5505433 48.0Cdf.20
      glyc_48.0D_dilution_factor glyc_48.0D_mean_luminescence glyc_48.0D_se
    1                         20                     1956.333      28.48586
      glyc_48.0D_conc      label
    1       0.4605721 48.0Ddf.20
      glyc_48.0E_dilution_factor glyc_48.0E_mean_luminescence glyc_48.0E_se
    1                         20                     2463.333      30.95337
      glyc_48.0E_conc      label
    1       0.5702244 48.0Edf.20
      glyc_48.0F_dilution_factor glyc_48.0F_mean_luminescence glyc_48.0F_se
    1                         20                         1259      11.13553
      glyc_48.0F_conc      label
    1       0.3097552 48.0Fdf.20
      glyc_48.0G_dilution_factor glyc_48.0G_mean_luminescence glyc_48.0G_se
    1                         20                     1506.333      43.05165
      glyc_48.0G_conc      label
    1       0.3632476 48.0Gdf.20
      glyc_48.0H_dilution_factor glyc_48.0H_mean_luminescence glyc_48.0H_se
    1                         20                     2960.667      1554.509
      glyc_48.0H_conc      label
    1       0.6777861 48.0Hdf.20
      glyc_48.0I_dilution_factor glyc_48.0I_mean_luminescence glyc_48.0I_se
    1                         20                     3138.333      22.66667
      glyc_48.0I_conc      label
    1       0.7162112 48.0Idf.20
      glyc_48.0J_dilution_factor glyc_48.0J_mean_luminescence glyc_48.0J_se
    1                         20                     1042.667      59.47642
      glyc_48.0J_conc      label
    1       0.2629673 48.0Jdf.20
      glyc_48.2A_dilution_factor glyc_48.2A_mean_luminescence glyc_48.2A_se
    1                         20                         3748      27.20907
      glyc_48.2A_conc      label
    1       0.8480679 48.2Adf.20
      glyc_48.2B_dilution_factor glyc_48.2B_mean_luminescence glyc_48.2B_se
    1                         20                     1216.667      21.85813
      glyc_48.2B_conc      label
    1       0.3005995 48.2Bdf.20
      glyc_48.2C_dilution_factor glyc_48.2C_mean_luminescence glyc_48.2C_se
    1                         20                     1662.667      20.41514
      glyc_48.2C_conc      label
    1       0.3970589 48.2Cdf.20
      glyc_48.2D_dilution_factor glyc_48.2D_mean_luminescence glyc_48.2D_se
    1                         20                     8336.333      140.6003
      glyc_48.2D_conc      label
    1        1.840418 48.2Ddf.20
      glyc_48.2E_dilution_factor glyc_48.2E_mean_luminescence glyc_48.2E_se
    1                         20                     8941.667      52.97903
      glyc_48.2E_conc      label
    1        1.971337 48.2Edf.20
      glyc_48.2F_dilution_factor glyc_48.2F_mean_luminescence glyc_48.2F_se
    1                         20                     11403.67       317.037
      glyc_48.2F_conc      label
    1         2.50381 48.2Fdf.20
      glyc_48.2G_dilution_factor glyc_48.2G_mean_luminescence glyc_48.2G_se
    1                         20                     13084.67      158.6457
      glyc_48.2G_conc      label
    1        2.867372 48.2Gdf.20
      glyc_48.2H_dilution_factor glyc_48.2H_mean_luminescence glyc_48.2H_se
    1                         20                     3154.667      96.12896
      glyc_48.2H_conc      label
    1       0.7197437 48.2Hdf.20
      glyc_48.2I_dilution_factor glyc_48.2I_mean_luminescence glyc_48.2I_se
    1                         20                     2426.333      68.34309
      glyc_48.2I_conc      label
    1       0.5622222 48.2Idf.20
      glyc_48.2J_dilution_factor glyc_48.2J_mean_luminescence glyc_48.2J_se
    1                         20                     3480.333      162.2902
      glyc_48.2J_conc      label
    1       0.7901779 48.2Jdf.20
      glyc_48.7A_dilution_factor glyc_48.7A_mean_luminescence glyc_48.7A_se
    1                         20                     7269.333      460.9376
      glyc_48.7A_conc      label
    1         1.60965 48.7Adf.20
      glyc_48.7B_dilution_factor glyc_48.7B_mean_luminescence glyc_48.7B_se
    1                         20                         4930      91.94745
      glyc_48.7B_conc      label
    1        1.103707 48.7Bdf.20
      glyc_48.7C_dilution_factor glyc_48.7C_mean_luminescence glyc_48.7C_se
    1                         20                     10708.33      84.86329
      glyc_48.7C_conc      label
    1        2.353426 48.7Cdf.20
      glyc_48.7D_dilution_factor glyc_48.7D_mean_luminescence glyc_48.7D_se
    1                         20                     4910.333      62.67996
      glyc_48.7D_conc      label
    1        1.099454 48.7Ddf.20
      glyc_48.7E_dilution_factor glyc_48.7E_mean_luminescence glyc_48.7E_se
    1                         20                     9916.667      159.6604
      glyc_48.7E_conc      label
    1        2.182207 48.7Edf.20
      glyc_48.7F_dilution_factor glyc_48.7F_mean_luminescence glyc_48.7F_se
    1                         20                         1566      73.69532
      glyc_48.7F_conc      label
    1       0.3761521 48.7Fdf.20
      glyc_48.7G_dilution_factor glyc_48.7G_mean_luminescence glyc_48.7G_se
    1                         20                        11123      415.5109
      glyc_48.7G_conc      label
    1        2.443109 48.7Gdf.20
      glyc_48.7H_dilution_factor glyc_48.7H_mean_luminescence glyc_48.7H_se
    1                         20                        15968       115.659
      glyc_48.7H_conc      label
    1        3.490969 48.7Hdf.20
      glyc_48.7I_dilution_factor glyc_48.7I_mean_luminescence glyc_48.7I_se
    1                         20                     5176.333      114.4557
      glyc_48.7I_conc      label
    1        1.156983 48.7Idf.20
      glyc_48.7J_dilution_factor glyc_48.7J_mean_luminescence glyc_48.7J_se
    1                         20                        13174       329.003
      glyc_48.7J_conc      label
    1        2.886692 48.7Jdf.20
    glycogen Standard Curve Summary:
    ==================================================
    Concentration: 20 µg/µL
      Mean Luminescence: 92676.67
      Standard Error: 2122.69
      CV%: 3.97%

    Concentration: 2 µg/µL
      Mean Luminescence: 5268.33
      Standard Error: 258.98
      CV%: 8.51%

    Concentration: 0.2 µg/µL
      Mean Luminescence: 1157.33
      Standard Error: 105.77
      CV%: 15.83%

    Concentration: 0.02 µg/µL
      Mean Luminescence: 687.33
      Standard Error: 24.22
      CV%: 6.10%

    Concentration: 0 µg/µL
      Mean Luminescence: 2083.00
      Standard Error: 1365.04
      CV%: 113.51%

### 1.1.5 Sample glycogen table

``` r
#plate1
glyc_0.0A_mean_conc_normalized <- glyc_0.0A_mean_conc/as.numeric(sample_weights[2,2])
glyc_0.0B_mean_conc_normalized <- glyc_0.0B_mean_conc/as.numeric(sample_weights[3,2])
glyc_0.0C_mean_conc_normalized <- glyc_0.0C_mean_conc/as.numeric(sample_weights[4,2])
glyc_0.0D_mean_conc_normalized <- glyc_0.0D_mean_conc/as.numeric(sample_weights[5,2])
glyc_0.0E_mean_conc_normalized <- glyc_0.0E_mean_conc/as.numeric(sample_weights[6,2])
glyc_0.0F_mean_conc_normalized <- glyc_0.0F_mean_conc/as.numeric(sample_weights[7,2])
glyc_0.0G_mean_conc_normalized <- glyc_0.0G_mean_conc/as.numeric(sample_weights[8,2])
glyc_0.0H_mean_conc_normalized <- glyc_0.0H_mean_conc/as.numeric(sample_weights[9,2])
glyc_0.0I_mean_conc_normalized <- glyc_0.0I_mean_conc/as.numeric(sample_weights[10,2])
glyc_0.0J_mean_conc_normalized <- glyc_0.0J_mean_conc/as.numeric(sample_weights[11,2])
#plate2
glyc_48.0A_mean_conc_normalized <- glyc_48.0A_mean_conc/as.numeric(sample_weights[12,2])
glyc_48.0B_mean_conc_normalized <- glyc_48.0B_mean_conc/as.numeric(sample_weights[13,2])
glyc_48.0C_mean_conc_normalized <- glyc_48.0C_mean_conc/as.numeric(sample_weights[14,2])
glyc_48.0D_mean_conc_normalized <- glyc_48.0D_mean_conc/as.numeric(sample_weights[15,2])
glyc_48.0E_mean_conc_normalized <- glyc_48.0E_mean_conc/as.numeric(sample_weights[16,2])
glyc_48.0F_mean_conc_normalized <- glyc_48.0F_mean_conc/as.numeric(sample_weights[17,2])
glyc_48.0G_mean_conc_normalized <- glyc_48.0G_mean_conc/as.numeric(sample_weights[18,2])
glyc_48.0H_mean_conc_normalized <- glyc_48.0H_mean_conc/as.numeric(sample_weights[19,2])
glyc_48.0I_mean_conc_normalized <- glyc_48.0I_mean_conc/as.numeric(sample_weights[20,2])
glyc_48.0J_mean_conc_normalized <- glyc_48.0J_mean_conc/as.numeric(sample_weights[21,2])
glyc_48.2A_mean_conc_normalized <- glyc_48.2A_mean_conc/as.numeric(sample_weights[22,2])
glyc_48.2B_mean_conc_normalized <- glyc_48.2B_mean_conc/as.numeric(sample_weights[23,2])
glyc_48.2C_mean_conc_normalized <- glyc_48.2C_mean_conc/as.numeric(sample_weights[24,2])
glyc_48.2D_mean_conc_normalized <- glyc_48.2D_mean_conc/as.numeric(sample_weights[25,2])
glyc_48.2E_mean_conc_normalized <- glyc_48.2E_mean_conc/as.numeric(sample_weights[26,2])
glyc_48.2F_mean_conc_normalized <- glyc_48.2F_mean_conc/as.numeric(sample_weights[27,2])
#plate3
glyc_48.2G_mean_conc_normalized <- glyc_48.2G_mean_conc/as.numeric(sample_weights[28,2])
glyc_48.2H_mean_conc_normalized <- glyc_48.2H_mean_conc/as.numeric(sample_weights[29,2])
glyc_48.2I_mean_conc_normalized <- glyc_48.2I_mean_conc/as.numeric(sample_weights[30,2])
glyc_48.2J_mean_conc_normalized <- glyc_48.2J_mean_conc/as.numeric(sample_weights[31,2])
glyc_48.7A_mean_conc_normalized <- glyc_48.7A_mean_conc/as.numeric(sample_weights[32,2])
glyc_48.7B_mean_conc_normalized <- glyc_48.7B_mean_conc/as.numeric(sample_weights[33,2])
glyc_48.7C_mean_conc_normalized <- glyc_48.7C_mean_conc/as.numeric(sample_weights[34,2])
glyc_48.7D_mean_conc_normalized <- glyc_48.7D_mean_conc/as.numeric(sample_weights[35,2])
glyc_48.7E_mean_conc_normalized <- glyc_48.7E_mean_conc/as.numeric(sample_weights[36,2])
glyc_48.7F_mean_conc_normalized <- glyc_48.7F_mean_conc/as.numeric(sample_weights[37,2])
glyc_48.7G_mean_conc_normalized <- glyc_48.7G_mean_conc/as.numeric(sample_weights[38,2])
glyc_48.7H_mean_conc_normalized <- glyc_48.7H_mean_conc/as.numeric(sample_weights[39,2])
glyc_48.7I_mean_conc_normalized <- glyc_48.7I_mean_conc/as.numeric(sample_weights[40,2])
glyc_48.7J_mean_conc_normalized <- glyc_48.7J_mean_conc/as.numeric(sample_weights[41,2])
```

``` r
tab_glyc <- matrix(c(glyc_0.0A_dilution, glyc_0.0A_mean_lum, glyc_0.0A_mean_conc,(glyc_0.0A_dilution*glyc_0.0A_mean_conc), (glyc_0.0A_dilution*glyc_0.0A_mean_conc_normalized), 
                glyc_0.0B_dilution, glyc_0.0B_mean_lum, glyc_0.0B_mean_conc,(glyc_0.0B_dilution*glyc_0.0B_mean_conc), (glyc_0.0B_dilution*glyc_0.0B_mean_conc_normalized),
                glyc_0.0C_dilution, glyc_0.0C_mean_lum, glyc_0.0C_mean_conc,(glyc_0.0C_dilution*glyc_0.0C_mean_conc), (glyc_0.0C_dilution*glyc_0.0C_mean_conc_normalized), 
                glyc_0.0D_dilution, glyc_0.0D_mean_lum, glyc_0.0D_mean_conc,(glyc_0.0D_dilution*glyc_0.0D_mean_conc), (glyc_0.0D_dilution*glyc_0.0D_mean_conc_normalized),
                glyc_0.0E_dilution, glyc_0.0E_mean_lum, glyc_0.0E_mean_conc,(glyc_0.0E_dilution*glyc_0.0E_mean_conc), (glyc_0.0E_dilution*glyc_0.0E_mean_conc_normalized), 
                glyc_0.0F_dilution, glyc_0.0F_mean_lum, glyc_0.0F_mean_conc,(glyc_0.0F_dilution*glyc_0.0F_mean_conc), (glyc_0.0F_dilution*glyc_0.0F_mean_conc_normalized),
                glyc_0.0G_dilution, glyc_0.0G_mean_lum, glyc_0.0G_mean_conc,(glyc_0.0G_dilution*glyc_0.0G_mean_conc), (glyc_0.0G_dilution*glyc_0.0G_mean_conc_normalized), 
                glyc_0.0H_dilution, glyc_0.0H_mean_lum, glyc_0.0H_mean_conc,(glyc_0.0H_dilution*glyc_0.0H_mean_conc), (glyc_0.0H_dilution*glyc_0.0H_mean_conc_normalized),
                glyc_0.0I_dilution, glyc_0.0I_mean_lum, glyc_0.0I_mean_conc,(glyc_0.0I_dilution*glyc_0.0I_mean_conc), (glyc_0.0I_dilution*glyc_0.0I_mean_conc_normalized), 
                glyc_0.0J_dilution, glyc_0.0J_mean_lum, glyc_0.0J_mean_conc,(glyc_0.0J_dilution*glyc_0.0J_mean_conc), (glyc_0.0J_dilution*glyc_0.0J_mean_conc_normalized),
                
                glyc_48.0A_dilution, glyc_48.0A_mean_lum, glyc_48.0A_mean_conc,(glyc_48.0A_dilution*glyc_48.0A_mean_conc), (glyc_48.0A_dilution*glyc_48.0A_mean_conc_normalized), 
                glyc_48.0B_dilution, glyc_48.0B_mean_lum, glyc_48.0B_mean_conc,(glyc_48.0B_dilution*glyc_48.0B_mean_conc), (glyc_48.0B_dilution*glyc_48.0B_mean_conc_normalized),
                glyc_48.0C_dilution, glyc_48.0C_mean_lum, glyc_48.0C_mean_conc,(glyc_48.0C_dilution*glyc_48.0C_mean_conc), (glyc_48.0C_dilution*glyc_48.0C_mean_conc_normalized), 
                glyc_48.0D_dilution, glyc_48.0D_mean_lum, glyc_48.0D_mean_conc,(glyc_48.0D_dilution*glyc_48.0D_mean_conc), (glyc_48.0D_dilution*glyc_48.0D_mean_conc_normalized),
                glyc_48.0E_dilution, glyc_48.0E_mean_lum, glyc_48.0E_mean_conc,(glyc_48.0E_dilution*glyc_48.0E_mean_conc), (glyc_48.0E_dilution*glyc_48.0E_mean_conc_normalized), 
                glyc_48.0F_dilution, glyc_48.0F_mean_lum, glyc_48.0F_mean_conc,(glyc_48.0F_dilution*glyc_48.0F_mean_conc), (glyc_48.0F_dilution*glyc_48.0F_mean_conc_normalized),
                glyc_48.0G_dilution, glyc_48.0G_mean_lum, glyc_48.0G_mean_conc,(glyc_48.0G_dilution*glyc_48.0G_mean_conc), (glyc_48.0G_dilution*glyc_48.0G_mean_conc_normalized), 
                glyc_48.0H_dilution, glyc_48.0H_mean_lum, glyc_48.0H_mean_conc,(glyc_48.0H_dilution*glyc_48.0H_mean_conc), (glyc_48.0H_dilution*glyc_48.0H_mean_conc_normalized),
                glyc_48.0I_dilution, glyc_48.0I_mean_lum, glyc_48.0I_mean_conc,(glyc_48.0I_dilution*glyc_48.0I_mean_conc), (glyc_48.0I_dilution*glyc_48.0I_mean_conc_normalized), 
                glyc_48.0J_dilution, glyc_48.0J_mean_lum, glyc_48.0J_mean_conc,(glyc_48.0J_dilution*glyc_48.0J_mean_conc), (glyc_48.0J_dilution*glyc_48.0J_mean_conc_normalized),
                
                glyc_48.2A_dilution, glyc_48.2A_mean_lum, glyc_48.2A_mean_conc,(glyc_48.2A_dilution*glyc_48.2A_mean_conc), (glyc_48.2A_dilution*glyc_48.2A_mean_conc_normalized), 
                glyc_48.2B_dilution, glyc_48.2B_mean_lum, glyc_48.2B_mean_conc,(glyc_48.2B_dilution*glyc_48.2B_mean_conc), (glyc_48.2B_dilution*glyc_48.2B_mean_conc_normalized),
                glyc_48.2C_dilution, glyc_48.2C_mean_lum, glyc_48.2C_mean_conc,(glyc_48.2C_dilution*glyc_48.2C_mean_conc), (glyc_48.2C_dilution*glyc_48.2C_mean_conc_normalized), 
                glyc_48.2D_dilution, glyc_48.2D_mean_lum, glyc_48.2D_mean_conc,(glyc_48.2D_dilution*glyc_48.2D_mean_conc), (glyc_48.2D_dilution*glyc_48.2D_mean_conc_normalized),
                glyc_48.2E_dilution, glyc_48.2E_mean_lum, glyc_48.2E_mean_conc,(glyc_48.2E_dilution*glyc_48.2E_mean_conc), (glyc_48.2E_dilution*glyc_48.2E_mean_conc_normalized), 
                glyc_48.2F_dilution, glyc_48.2F_mean_lum, glyc_48.2F_mean_conc,(glyc_48.2F_dilution*glyc_48.2F_mean_conc), (glyc_48.2F_dilution*glyc_48.2F_mean_conc_normalized),
                glyc_48.2G_dilution, glyc_48.2G_mean_lum, glyc_48.2G_mean_conc,(glyc_48.2G_dilution*glyc_48.2G_mean_conc), (glyc_48.2G_dilution*glyc_48.2G_mean_conc_normalized), 
                glyc_48.2H_dilution, glyc_48.2H_mean_lum, glyc_48.2H_mean_conc,(glyc_48.2H_dilution*glyc_48.2H_mean_conc), (glyc_48.2H_dilution*glyc_48.2H_mean_conc_normalized),
                glyc_48.2I_dilution, glyc_48.2I_mean_lum, glyc_48.2I_mean_conc,(glyc_48.2I_dilution*glyc_48.2I_mean_conc), (glyc_48.2I_dilution*glyc_48.2I_mean_conc_normalized), 
                glyc_48.2J_dilution, glyc_48.2J_mean_lum, glyc_48.2J_mean_conc,(glyc_48.2J_dilution*glyc_48.2J_mean_conc), (glyc_48.2J_dilution*glyc_48.2J_mean_conc_normalized),
                
                glyc_48.7A_dilution, glyc_48.7A_mean_lum, glyc_48.7A_mean_conc,(glyc_48.7A_dilution*glyc_48.7A_mean_conc), (glyc_48.7A_dilution*glyc_48.7A_mean_conc_normalized), 
                glyc_48.7B_dilution, glyc_48.7B_mean_lum, glyc_48.7B_mean_conc,(glyc_48.7B_dilution*glyc_48.7B_mean_conc), (glyc_48.7B_dilution*glyc_48.7B_mean_conc_normalized),
                glyc_48.7C_dilution, glyc_48.7C_mean_lum, glyc_48.7C_mean_conc,(glyc_48.7C_dilution*glyc_48.7C_mean_conc), (glyc_48.7C_dilution*glyc_48.7C_mean_conc_normalized), 
                glyc_48.7D_dilution, glyc_48.7D_mean_lum, glyc_48.7D_mean_conc,(glyc_48.7D_dilution*glyc_48.7D_mean_conc), (glyc_48.7D_dilution*glyc_48.7D_mean_conc_normalized),
                glyc_48.7E_dilution, glyc_48.7E_mean_lum, glyc_48.7E_mean_conc,(glyc_48.7E_dilution*glyc_48.7E_mean_conc), (glyc_48.7E_dilution*glyc_48.7E_mean_conc_normalized), 
                glyc_48.7F_dilution, glyc_48.7F_mean_lum, glyc_48.7F_mean_conc,(glyc_48.7F_dilution*glyc_48.7F_mean_conc), (glyc_48.7F_dilution*glyc_48.7F_mean_conc_normalized),
                glyc_48.7G_dilution, glyc_48.7G_mean_lum, glyc_48.7G_mean_conc,(glyc_48.7G_dilution*glyc_48.7G_mean_conc), (glyc_48.7G_dilution*glyc_48.7G_mean_conc_normalized), 
                glyc_48.7H_dilution, glyc_48.7H_mean_lum, glyc_48.7H_mean_conc,(glyc_48.7H_dilution*glyc_48.7H_mean_conc), (glyc_48.7H_dilution*glyc_48.7H_mean_conc_normalized),
                glyc_48.7I_dilution, glyc_48.7I_mean_lum, glyc_48.7I_mean_conc,(glyc_48.7I_dilution*glyc_48.7I_mean_conc), (glyc_48.7I_dilution*glyc_48.7I_mean_conc_normalized), 
                glyc_48.7J_dilution, glyc_48.7J_mean_lum, glyc_48.7J_mean_conc,(glyc_48.7J_dilution*glyc_48.7J_mean_conc), (glyc_48.7J_dilution*glyc_48.7J_mean_conc_normalized)), ncol=5, byrow=TRUE)
              
colnames(tab_glyc) <- c('Dilution factor','Luminescence','Calculated Glycogen (ug/uL)','Total glycogen (ug/uL)','Normalized glycogen (ug/uL/mg)')
rownames(tab_glyc) <- c('0.0A','0.0B','0.0C','0.0D','0.0E','0.0F','0.0G','0.0H','0.0I','0.0J','48.0A','48.0B','48.0C','48.0D','48.0E','48.0F','48.0G','48.0H','48.0I','48.0J','48.2A','48.2B','48.2C','48.2D','48.2E','48.2F','48.2G','48.2H','48.2I','48.2J','48.7A','48.7B','48.7C','48.7D','48.7E','48.7F','48.7G','48.7H','48.7I','48.7J')
tab_glyc <- as.table(tab_glyc)
tab_glyc
```

          Dilution factor Luminescence Calculated Glycogen (ug/uL)
    0.0A     2.000000e+01 1.129733e+04                2.480813e+00
    0.0B     2.000000e+01 1.185967e+04                2.602433e+00
    0.0C     2.000000e+01 2.176467e+04                4.744653e+00
    0.0D     2.000000e+01 1.022833e+04                2.249613e+00
    0.0E     2.000000e+01 2.272433e+04                4.952207e+00
    0.0F     2.000000e+01 2.020167e+04                4.406613e+00
    0.0G     2.000000e+01 1.324433e+04                2.901904e+00
    0.0H     2.000000e+01 2.286667e+03                5.320155e-01
    0.0I     2.000000e+01 1.059200e+04                2.328266e+00
    0.0J     2.000000e+01 2.814667e+03                6.462097e-01
    48.0A    2.000000e+01 1.296667e+03                3.179016e-01
    48.0B    2.000000e+01 7.713333e+02                2.042842e-01
    48.0C    2.000000e+01 2.372333e+03                5.505433e-01
    48.0D    2.000000e+01 1.956333e+03                4.605721e-01
    48.0E    2.000000e+01 2.463333e+03                5.702244e-01
    48.0F    2.000000e+01 1.259000e+03                3.097552e-01
    48.0G    2.000000e+01 1.506333e+03                3.632476e-01
    48.0H    2.000000e+01 2.960667e+03                6.777861e-01
    48.0I    2.000000e+01 3.138333e+03                7.162112e-01
    48.0J    2.000000e+01 1.042667e+03                2.629673e-01
    48.2A    2.000000e+01 3.748000e+03                8.480679e-01
    48.2B    2.000000e+01 1.216667e+03                3.005995e-01
    48.2C    2.000000e+01 1.662667e+03                3.970589e-01
    48.2D    2.000000e+01 8.336333e+03                1.840418e+00
    48.2E    2.000000e+01 8.941667e+03                1.971337e+00
    48.2F    2.000000e+01 1.140367e+04                2.503810e+00
    48.2G    2.000000e+01 1.308467e+04                2.867372e+00
    48.2H    2.000000e+01 3.154667e+03                7.197437e-01
    48.2I    2.000000e+01 2.426333e+03                5.622222e-01
    48.2J    2.000000e+01 3.480333e+03                7.901779e-01
    48.7A    2.000000e+01 7.269333e+03                1.609650e+00
    48.7B    2.000000e+01 4.930000e+03                1.103707e+00
    48.7C    2.000000e+01 1.070833e+04                2.353426e+00
    48.7D    2.000000e+01 4.910333e+03                1.099454e+00
    48.7E    2.000000e+01 9.916667e+03                2.182207e+00
    48.7F    2.000000e+01 1.566000e+03                3.761521e-01
    48.7G    2.000000e+01 1.112300e+04                2.443109e+00
    48.7H    2.000000e+01 1.596800e+04                3.490969e+00
    48.7I    2.000000e+01 5.176333e+03                1.156983e+00
    48.7J    2.000000e+01 1.317400e+04                2.886692e+00
          Total glycogen (ug/uL) Normalized glycogen (ug/uL/mg)
    0.0A            4.961626e+01                   2.835215e+00
    0.0B            5.204865e+01                   2.891592e+00
    0.0C            9.489307e+01                   5.581945e+00
    0.0D            4.499226e+01                   2.319189e+00
    0.0E            9.904414e+01                   5.860600e+00
    0.0F            8.813226e+01                   3.815249e+00
    0.0G            5.803807e+01                   2.501641e+00
    0.0H            1.064031e+01                   8.122375e-01
    0.0I            4.656532e+01                   3.279248e+00
    0.0J            1.292419e+01                   1.374914e+00
    48.0A           6.358032e+00                   5.342884e-01
    48.0B           4.085685e+00                   2.196605e-01
    48.0C           1.101087e+01                   5.097623e-01
    48.0D           9.211443e+00                   1.096600e+00
    48.0E           1.140449e+01                   6.911811e-01
    48.0F           6.195104e+00                   3.871940e-01
    48.0G           7.264952e+00                   5.545765e-01
    48.0H           1.355572e+01                   1.120308e+00
    48.0I           1.432422e+01                   6.756710e-01
    48.0J           5.259346e+00                   3.627135e-01
    48.2A           1.696136e+01                   1.390275e+00
    48.2B           6.011989e+00                   5.725704e-01
    48.2C           7.941178e+00                   6.155952e-01
    48.2D           3.680835e+01                   1.426680e+00
    48.2E           3.942674e+01                   1.981243e+00
    48.2F           5.007621e+01                   3.551504e+00
    48.2G           5.734743e+01                   3.496795e+00
    48.2H           1.439487e+01                   7.161629e-01
    48.2I           1.124444e+01                   1.135802e+00
    48.2J           1.580356e+01                   1.736655e+00
    48.7A           3.219301e+01                   2.104118e+00
    48.7B           2.207414e+01                   1.635121e+00
    48.7C           4.706852e+01                   2.736542e+00
    48.7D           2.198907e+01                   2.931876e+00
    48.7E           4.364414e+01                   3.730268e+00
    48.7F           7.523043e+00                   1.139855e+00
    48.7G           4.886217e+01                   2.181347e+00
    48.7H           6.981939e+01                   4.035803e+00
    48.7I           2.313966e+01                   2.488136e+00
    48.7J           5.773385e+01                   3.823433e+00

## 1.2 glucose Standard Curve

### 1.2.1 Extract luminescence data

``` r
# Extract glucose standard curve data from plate layout and raw luminescence.
# Rows F, G, H (rows 6, 7, 8) correspond to glucose standards
# Columns 1-5 contain the standard curve concentrations

# Extract concentration values from plate layout (row 6, columns 1-5)
# Parse concentration from labels like "STD-glu-20" -> 20
glu_concentrations <- as.numeric(gsub("STD-glu-", "", plate_layout1[6, 7:11]))
glu_concentrations

# Extract standard curve luminescence values for each replicate from raw_luminescence
glu_row_F <- as.numeric(raw_luminescence1[6, 7:11])  # Row 6 (F)
glu_row_G <- as.numeric(raw_luminescence1[7, 7:11])  # Row 7 (G)
glu_row_H <- as.numeric(raw_luminescence1[8, 7:11])  # Row 8 (H)
```

    [1] 100.0  10.0   1.0   0.1   0.0

``` r
#Extract glucose sample data - wells A1-E6
glu_sample_cols1 <- c(7,8,9)
glu_0.0A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[1, 1]))
glu_0.0A_luminescence <- as.numeric(raw_luminescence1[1, glu_sample_cols1])

glu_0.0B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[2, 1]))
glu_0.0B_luminescence <- as.numeric(raw_luminescence1[2, glu_sample_cols1])

glu_0.0C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[3, 1]))
glu_0.0C_luminescence <- as.numeric(raw_luminescence1[3, glu_sample_cols1])

glu_0.0D_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[4, 1]))
glu_0.0D_luminescence <- as.numeric(raw_luminescence1[4, glu_sample_cols1])

glu_0.0E_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[5, 1]))
glu_0.0E_luminescence <- as.numeric(raw_luminescence1[5, glu_sample_cols1])

glu_sample_cols2 <- c(10,11,12)
glu_0.0F_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[1, 4]))
glu_0.0F_luminescence <- as.numeric(raw_luminescence1[1, glu_sample_cols2])

glu_0.0G_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[2, 4]))
glu_0.0G_luminescence <- as.numeric(raw_luminescence1[2, glu_sample_cols2])

glu_0.0H_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[3, 4]))
glu_0.0H_luminescence <- as.numeric(raw_luminescence1[3, glu_sample_cols2])

glu_0.0I_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[4, 4]))
glu_0.0I_luminescence <- as.numeric(raw_luminescence1[4, glu_sample_cols2])

glu_0.0J_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[5, 4]))
glu_0.0J_luminescence <- as.numeric(raw_luminescence1[5, glu_sample_cols2])


glu_sample_cols1 <- c(7,8,9)
glu_48.0A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[1, 1]))
glu_48.0A_luminescence <- as.numeric(raw_luminescence2[1, glu_sample_cols1])

glu_48.0B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[2, 1]))
glu_48.0B_luminescence <- as.numeric(raw_luminescence2[2, glu_sample_cols1])

glu_48.0C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[3, 1]))
glu_48.0C_luminescence <- as.numeric(raw_luminescence2[3, glu_sample_cols1])

glu_48.0D_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[4, 1]))
glu_48.0D_luminescence <- as.numeric(raw_luminescence2[4, glu_sample_cols1])

glu_48.0E_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[5, 1]))
glu_48.0E_luminescence <- as.numeric(raw_luminescence2[5, glu_sample_cols1])

glu_48.0F_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[6, 1]))
glu_48.0F_luminescence <- as.numeric(raw_luminescence2[6, glu_sample_cols1])

glu_48.0G_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[7, 1]))
glu_48.0G_luminescence <- as.numeric(raw_luminescence2[7, glu_sample_cols1])

glu_48.0H_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[8, 1]))
glu_48.0H_luminescence <- as.numeric(raw_luminescence2[8, glu_sample_cols1])

glu_sample_cols2 <- c(10,11,12)
glu_48.0I_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[1, 4]))
glu_48.0I_luminescence <- as.numeric(raw_luminescence2[1, glu_sample_cols2])

glu_48.0J_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[2, 4]))
glu_48.0J_luminescence <- as.numeric(raw_luminescence2[2, glu_sample_cols2])

glu_48.2A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[3, 4]))
glu_48.2A_luminescence <- as.numeric(raw_luminescence2[3, glu_sample_cols2])

glu_48.2B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[4, 4]))
glu_48.2B_luminescence <- as.numeric(raw_luminescence2[4, glu_sample_cols2])

glu_48.2C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[5, 4]))
glu_48.2C_luminescence <- as.numeric(raw_luminescence2[5, glu_sample_cols2])

glu_48.2D_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[6, 4]))
glu_48.2D_luminescence <- as.numeric(raw_luminescence2[6, glu_sample_cols2])

glu_48.2E_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[7, 4]))
glu_48.2E_luminescence <- as.numeric(raw_luminescence2[7, glu_sample_cols2])

glu_48.2F_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[8, 4]))
glu_48.2F_luminescence <- as.numeric(raw_luminescence2[8, glu_sample_cols2])


glu_sample_cols1 <- c(7,8,9)
glu_48.2G_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[1, 1]))
glu_48.2G_luminescence <- as.numeric(raw_luminescence3[1, glu_sample_cols1])

glu_48.2H_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[2, 1]))
glu_48.2H_luminescence <- as.numeric(raw_luminescence3[2, glu_sample_cols1])

glu_48.2I_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[3, 1]))
glu_48.2I_luminescence <- as.numeric(raw_luminescence3[3, glu_sample_cols1])

glu_48.2J_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[4, 1]))
glu_48.2J_luminescence <- as.numeric(raw_luminescence3[4, glu_sample_cols1])

glu_48.7A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[5, 1]))
glu_48.7A_luminescence <- as.numeric(raw_luminescence3[5, glu_sample_cols1])

glu_48.7B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[6, 1]))
glu_48.7B_luminescence <- as.numeric(raw_luminescence3[6, glu_sample_cols1])

glu_48.7C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[7, 1]))
glu_48.7C_luminescence <- as.numeric(raw_luminescence3[7, glu_sample_cols1])

glu_48.7D_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[8, 1]))
glu_48.7D_luminescence <- as.numeric(raw_luminescence3[8, glu_sample_cols1])

glu_sample_cols2 <- c(10,11,12)
glu_48.7E_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[1, 4]))
glu_48.7E_luminescence <- as.numeric(raw_luminescence3[1, glu_sample_cols2])

glu_48.7F_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[2, 4]))
glu_48.7F_luminescence <- as.numeric(raw_luminescence3[2, glu_sample_cols2])

glu_48.7G_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[3, 4]))
glu_48.7G_luminescence <- as.numeric(raw_luminescence3[3, glu_sample_cols2])

glu_48.7H_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[4, 4]))
glu_48.7H_luminescence <- as.numeric(raw_luminescence3[4, glu_sample_cols2])

glu_48.7I_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[5, 4]))
glu_48.7I_luminescence <- as.numeric(raw_luminescence3[5, glu_sample_cols2])

glu_48.7J_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout3[6, 4]))
glu_48.7J_luminescence <- as.numeric(raw_luminescence3[6, glu_sample_cols2])
```

### 1.2.2 Glucose standard curve summary statistics and linear regression

``` r
# Calculate mean and standard error for each concentration
glu_means <- numeric(length(glu_concentrations))
glu_std_errors <- numeric(length(glu_concentrations))
glu_std_devs <- numeric(length(glu_concentrations))

for (i in 1:length(glu_concentrations)) {
  glu_values <- c(glu_row_F[i], glu_row_G[i], glu_row_H[i])
  glu_means[i] <- mean(glu_values)
  glu_std_devs[i] <- sd(glu_values)
  glu_std_errors[i] <- sd(glu_values) / sqrt(length(glu_values))
}

# Create data frame for plotting
glucose_summary_data <- data.frame(
  glu_concentration = glu_concentrations,
  glu_mean_luminescence = glu_means,
  glu_se = glu_std_errors,
  glu_sd = glu_std_devs,
  glu_cv = (glu_std_devs / glu_means) * 100
)

# Calculate linear regression and R-squared
lm_model <- lm(glu_mean_luminescence ~ glu_concentration, data = glucose_summary_data)
glu_r_squared <- summary(lm_model)$r.squared
glu_slope <- coef(lm_model)[2]
glu_intercept <- coef(lm_model)[1]

glu_r_squared
glu_slope
glu_intercept
```

    [1] 0.9997667
    glu_concentration 
             1106.762 
    (Intercept) 
      -277.3897 

### 1.2.3 Calculate sample glucose levels

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_0.0A_mean_lum <- numeric(length(glu_0.0A_dilution))
glu_0.0A_se_lum <- numeric(length(glu_0.0A_dilution))
glu_0.0A_mean_conc <- numeric(length(glu_0.0A_dilution))

for (i in 1:length(glu_0.0A_dilution)) {
  df_val <- glu_0.0A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_0.0A_lum_values <- c(glu_0.0A_luminescence[glu_0.0A_dilution == df_val])
  glu_0.0A_mean_lum[i] <- mean(glu_0.0A_lum_values)
  glu_0.0A_se_lum[i] <- sd(glu_0.0A_lum_values) / sqrt(length(glu_0.0A_lum_values))
  # Calculate concentration from mean luminescence
  glu_0.0A_mean_conc[i] <- (glu_0.0A_mean_lum[i] - glu_intercept) / glu_slope
}

glu_0.0A_data <- data.frame(
  glu_0.0A_dilution_factor = glu_0.0A_dilution,
  glu_0.0A_mean_luminescence = glu_0.0A_mean_lum,
  glu_0.0A_se = glu_0.0A_se_lum,
  glu_0.0A_conc =  glu_0.0A_mean_conc,
  label = paste0("0.0Adf.", glu_0.0A_dilution)
)
glu_0.0A_mean_lum
glu_0.0A_mean_conc
```

    [1] 504.3333
    [1] 0.7063152

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_0.0B_mean_lum <- numeric(length(glu_0.0B_dilution))
glu_0.0B_se_lum <- numeric(length(glu_0.0B_dilution))
glu_0.0B_mean_conc <- numeric(length(glu_0.0B_dilution))

for (i in 1:length(glu_0.0B_dilution)) {
  df_val <- glu_0.0B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_0.0B_lum_values <- c(glu_0.0B_luminescence[glu_0.0B_dilution == df_val])
  glu_0.0B_mean_lum[i] <- mean(glu_0.0B_lum_values)
  glu_0.0B_se_lum[i] <- sd(glu_0.0B_lum_values) / sqrt(length(glu_0.0B_lum_values))
  # Calculate concentration from mean luminescence
  glu_0.0B_mean_conc[i] <- (glu_0.0B_mean_lum[i] - glu_intercept) / glu_slope
}

glu_0.0B_data <- data.frame(
  glu_0.0B_dilution_factor = glu_0.0B_dilution,
  glu_0.0B_mean_luminescence = glu_0.0B_mean_lum,
  glu_0.0B_se = glu_0.0B_se_lum,
  glu_0.0B_conc =  glu_0.0B_mean_conc,
  label = paste0("0.0Bdf.", glu_0.0B_dilution)
)
glu_0.0B_mean_lum
glu_0.0B_mean_conc
```

    [1] 253.3333
    [1] 0.4795276

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_0.0C_mean_lum <- numeric(length(glu_0.0C_dilution))
glu_0.0C_se_lum <- numeric(length(glu_0.0C_dilution))
glu_0.0C_mean_conc <- numeric(length(glu_0.0C_dilution))

for (i in 1:length(glu_0.0C_dilution)) {
  df_val <- glu_0.0C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_0.0C_lum_values <- c(glu_0.0C_luminescence[glu_0.0C_dilution == df_val])
  glu_0.0C_mean_lum[i] <- mean(glu_0.0C_lum_values)
  glu_0.0C_se_lum[i] <- sd(glu_0.0C_lum_values) / sqrt(length(glu_0.0C_lum_values))
  # Calculate concentration from mean luminescence
  glu_0.0C_mean_conc[i] <- (glu_0.0C_mean_lum[i] - glu_intercept) / glu_slope
}

glu_0.0C_data <- data.frame(
  glu_0.0C_dilution_factor = glu_0.0C_dilution,
  glu_0.0C_mean_luminescence = glu_0.0C_mean_lum,
  glu_0.0C_se = glu_0.0C_se_lum,
  glu_0.0C_conc =  glu_0.0C_mean_conc,
  label = paste0("0.0Cdf.", glu_0.0C_dilution)
)
glu_0.0C_mean_lum
glu_0.0C_mean_conc
```

    [1] 581
    [1] 0.7755863

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_0.0D_mean_lum <- numeric(length(glu_0.0D_dilution))
glu_0.0D_se_lum <- numeric(length(glu_0.0D_dilution))
glu_0.0D_mean_conc <- numeric(length(glu_0.0D_dilution))

for (i in 1:length(glu_0.0D_dilution)) {
  df_val <- glu_0.0D_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_0.0D_lum_values <- c(glu_0.0D_luminescence[glu_0.0D_dilution == df_val])
  glu_0.0D_mean_lum[i] <- mean(glu_0.0D_lum_values)
  glu_0.0D_se_lum[i] <- sd(glu_0.0D_lum_values) / sqrt(length(glu_0.0D_lum_values))
  # Calculate concentration from mean luminescence
  glu_0.0D_mean_conc[i] <- (glu_0.0D_mean_lum[i] - glu_intercept) / glu_slope
}

glu_0.0D_data <- data.frame(
  glu_0.0D_dilution_factor = glu_0.0D_dilution,
  glu_0.0D_mean_luminescence = glu_0.0D_mean_lum,
  glu_0.0D_se = glu_0.0D_se_lum,
  glu_0.0D_conc =  glu_0.0D_mean_conc,
  label = paste0("0.0Ddf.", glu_0.0D_dilution)
)
glu_0.0D_mean_lum
glu_0.0D_mean_conc
```

    [1] 361.3333
    [1] 0.5771095

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_0.0E_mean_lum <- numeric(length(glu_0.0E_dilution))
glu_0.0E_se_lum <- numeric(length(glu_0.0E_dilution))
glu_0.0E_mean_conc <- numeric(length(glu_0.0E_dilution))

for (i in 1:length(glu_0.0E_dilution)) {
  df_val <- glu_0.0E_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_0.0E_lum_values <- c(glu_0.0E_luminescence[glu_0.0E_dilution == df_val])
  glu_0.0E_mean_lum[i] <- mean(glu_0.0E_lum_values)
  glu_0.0E_se_lum[i] <- sd(glu_0.0E_lum_values) / sqrt(length(glu_0.0E_lum_values))
  # Calculate concentration from mean luminescence
  glu_0.0E_mean_conc[i] <- (glu_0.0E_mean_lum[i] - glu_intercept) / glu_slope
}

glu_0.0E_data <- data.frame(
  glu_0.0E_dilution_factor = glu_0.0E_dilution,
  glu_0.0E_mean_luminescence = glu_0.0E_mean_lum,
  glu_0.0E_se = glu_0.0E_se_lum,
  glu_0.0E_conc =  glu_0.0E_mean_conc,
  label = paste0("0.0Edf.", glu_0.0E_dilution)
)
glu_0.0E_mean_lum
glu_0.0E_mean_conc
```

    [1] 334.6667
    [1] 0.5530152

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_0.0F_mean_lum <- numeric(length(glu_0.0F_dilution))
glu_0.0F_se_lum <- numeric(length(glu_0.0F_dilution))
glu_0.0F_mean_conc <- numeric(length(glu_0.0F_dilution))

for (i in 1:length(glu_0.0F_dilution)) {
  df_val <- glu_0.0F_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_0.0F_lum_values <- c(glu_0.0F_luminescence[glu_0.0F_dilution == df_val])
  glu_0.0F_mean_lum[i] <- mean(glu_0.0F_lum_values)
  glu_0.0F_se_lum[i] <- sd(glu_0.0F_lum_values) / sqrt(length(glu_0.0F_lum_values))
  # Calculate concentration from mean luminescence
  glu_0.0F_mean_conc[i] <- (glu_0.0F_mean_lum[i] - glu_intercept) / glu_slope
}

glu_0.0F_data <- data.frame(
  glu_0.0F_dilution_factor = glu_0.0F_dilution,
  glu_0.0F_mean_luminescence = glu_0.0F_mean_lum,
  glu_0.0F_se = glu_0.0F_se_lum,
  glu_0.0F_conc =  glu_0.0F_mean_conc,
  label = paste0("0.0Fdf.", glu_0.0F_dilution)
)
glu_0.0F_mean_lum
glu_0.0F_mean_conc
```

    [1] 339
    [1] 0.5569305

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_0.0G_mean_lum <- numeric(length(glu_0.0G_dilution))
glu_0.0G_se_lum <- numeric(length(glu_0.0G_dilution))
glu_0.0G_mean_conc <- numeric(length(glu_0.0G_dilution))

for (i in 1:length(glu_0.0G_dilution)) {
  df_val <- glu_0.0G_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_0.0G_lum_values <- c(glu_0.0G_luminescence[glu_0.0G_dilution == df_val])
  glu_0.0G_mean_lum[i] <- mean(glu_0.0G_lum_values)
  glu_0.0G_se_lum[i] <- sd(glu_0.0G_lum_values) / sqrt(length(glu_0.0G_lum_values))
  # Calculate concentration from mean luminescence
  glu_0.0G_mean_conc[i] <- (glu_0.0G_mean_lum[i] - glu_intercept) / glu_slope
}

glu_0.0G_data <- data.frame(
  glu_0.0G_dilution_factor = glu_0.0G_dilution,
  glu_0.0G_mean_luminescence = glu_0.0G_mean_lum,
  glu_0.0G_se = glu_0.0G_se_lum,
  glu_0.0G_conc =  glu_0.0G_mean_conc,
  label = paste0("0.0Gdf.", glu_0.0G_dilution)
)
glu_0.0G_mean_lum
glu_0.0G_mean_conc
```

    [1] 263.3333
    [1] 0.4885629

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_0.0H_mean_lum <- numeric(length(glu_0.0H_dilution))
glu_0.0H_se_lum <- numeric(length(glu_0.0H_dilution))
glu_0.0H_mean_conc <- numeric(length(glu_0.0H_dilution))

for (i in 1:length(glu_0.0H_dilution)) {
  df_val <- glu_0.0H_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_0.0H_lum_values <- c(glu_0.0H_luminescence[glu_0.0H_dilution == df_val])
  glu_0.0H_mean_lum[i] <- mean(glu_0.0H_lum_values)
  glu_0.0H_se_lum[i] <- sd(glu_0.0H_lum_values) / sqrt(length(glu_0.0H_lum_values))
  # Calculate concentration from mean luminescence
  glu_0.0H_mean_conc[i] <- (glu_0.0H_mean_lum[i] - glu_intercept) / glu_slope
}

glu_0.0H_data <- data.frame(
  glu_0.0H_dilution_factor = glu_0.0H_dilution,
  glu_0.0H_mean_luminescence = glu_0.0H_mean_lum,
  glu_0.0H_se = glu_0.0H_se_lum,
  glu_0.0H_conc =  glu_0.0H_mean_conc,
  label = paste0("0.0Hdf.", glu_0.0H_dilution)
)
glu_0.0H_mean_lum
glu_0.0H_mean_conc
```

    [1] 252
    [1] 0.4783228

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_0.0I_mean_lum <- numeric(length(glu_0.0I_dilution))
glu_0.0I_se_lum <- numeric(length(glu_0.0I_dilution))
glu_0.0I_mean_conc <- numeric(length(glu_0.0I_dilution))

for (i in 1:length(glu_0.0I_dilution)) {
  df_val <- glu_0.0I_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_0.0I_lum_values <- c(glu_0.0I_luminescence[glu_0.0I_dilution == df_val])
  glu_0.0I_mean_lum[i] <- mean(glu_0.0I_lum_values)
  glu_0.0I_se_lum[i] <- sd(glu_0.0I_lum_values) / sqrt(length(glu_0.0I_lum_values))
  # Calculate concentration from mean luminescence
  glu_0.0I_mean_conc[i] <- (glu_0.0I_mean_lum[i] - glu_intercept) / glu_slope
}

glu_0.0I_data <- data.frame(
  glu_0.0I_dilution_factor = glu_0.0I_dilution,
  glu_0.0I_mean_luminescence = glu_0.0I_mean_lum,
  glu_0.0I_se = glu_0.0I_se_lum,
  glu_0.0I_conc =  glu_0.0I_mean_conc,
  label = paste0("0.0Idf.", glu_0.0I_dilution)
)
glu_0.0I_mean_lum
glu_0.0I_mean_conc
```

    [1] 249.6667
    [1] 0.4762146

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_0.0J_mean_lum <- numeric(length(glu_0.0J_dilution))
glu_0.0J_se_lum <- numeric(length(glu_0.0J_dilution))
glu_0.0J_mean_conc <- numeric(length(glu_0.0J_dilution))

for (i in 1:length(glu_0.0J_dilution)) {
  df_val <- glu_0.0J_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_0.0J_lum_values <- c(glu_0.0J_luminescence[glu_0.0J_dilution == df_val])
  glu_0.0J_mean_lum[i] <- mean(glu_0.0J_lum_values)
  glu_0.0J_se_lum[i] <- sd(glu_0.0J_lum_values) / sqrt(length(glu_0.0J_lum_values))
  # Calculate concentration from mean luminescence
  glu_0.0J_mean_conc[i] <- (glu_0.0J_mean_lum[i] - glu_intercept) / glu_slope
}

glu_0.0J_data <- data.frame(
  glu_0.0J_dilution_factor = glu_0.0J_dilution,
  glu_0.0J_mean_luminescence = glu_0.0J_mean_lum,
  glu_0.0J_se = glu_0.0J_se_lum,
  glu_0.0J_conc =  glu_0.0J_mean_conc,
  label = paste0("0.0Jdf.", glu_0.0J_dilution)
)
glu_0.0J_mean_lum
glu_0.0J_mean_conc
```

    [1] 313.6667
    [1] 0.5340409

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.0A_mean_lum <- numeric(length(glu_48.0A_dilution))
glu_48.0A_se_lum <- numeric(length(glu_48.0A_dilution))
glu_48.0A_mean_conc <- numeric(length(glu_48.0A_dilution))

for (i in 1:length(glu_48.0A_dilution)) {
  df_val <- glu_48.0A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.0A_lum_values <- c(glu_48.0A_luminescence[glu_48.0A_dilution == df_val])
  glu_48.0A_mean_lum[i] <- mean(glu_48.0A_lum_values)
  glu_48.0A_se_lum[i] <- sd(glu_48.0A_lum_values) / sqrt(length(glu_48.0A_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.0A_mean_conc[i] <- (glu_48.0A_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.0A_data <- data.frame(
  glu_48.0A_dilution_factor = glu_48.0A_dilution,
  glu_48.0A_mean_luminescence = glu_48.0A_mean_lum,
  glu_48.0A_se = glu_48.0A_se_lum,
  glu_48.0A_conc =  glu_48.0A_mean_conc,
  label = paste0("48.0Adf.", glu_48.0A_dilution)
)
glu_48.0A_mean_lum
glu_48.0A_mean_conc
```

    [1] 332
    [1] 0.5506058

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.0B_mean_lum <- numeric(length(glu_48.0B_dilution))
glu_48.0B_se_lum <- numeric(length(glu_48.0B_dilution))
glu_48.0B_mean_conc <- numeric(length(glu_48.0B_dilution))

for (i in 1:length(glu_48.0B_dilution)) {
  df_val <- glu_48.0B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.0B_lum_values <- c(glu_48.0B_luminescence[glu_48.0B_dilution == df_val])
  glu_48.0B_mean_lum[i] <- mean(glu_48.0B_lum_values)
  glu_48.0B_se_lum[i] <- sd(glu_48.0B_lum_values) / sqrt(length(glu_48.0B_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.0B_mean_conc[i] <- (glu_48.0B_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.0B_data <- data.frame(
  glu_48.0B_dilution_factor = glu_48.0B_dilution,
  glu_48.0B_mean_luminescence = glu_48.0B_mean_lum,
  glu_48.0B_se = glu_48.0B_se_lum,
  glu_48.0B_conc =  glu_48.0B_mean_conc,
  label = paste0("48.0Bdf.", glu_48.0B_dilution)
)
glu_48.0B_mean_lum
glu_48.0B_mean_conc
```

    [1] 927
    [1] 1.08821

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.0C_mean_lum <- numeric(length(glu_48.0C_dilution))
glu_48.0C_se_lum <- numeric(length(glu_48.0C_dilution))
glu_48.0C_mean_conc <- numeric(length(glu_48.0C_dilution))

for (i in 1:length(glu_48.0C_dilution)) {
  df_val <- glu_48.0C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.0C_lum_values <- c(glu_48.0C_luminescence[glu_48.0C_dilution == df_val])
  glu_48.0C_mean_lum[i] <- mean(glu_48.0C_lum_values)
  glu_48.0C_se_lum[i] <- sd(glu_48.0C_lum_values) / sqrt(length(glu_48.0C_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.0C_mean_conc[i] <- (glu_48.0C_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.0C_data <- data.frame(
  glu_48.0C_dilution_factor = glu_48.0C_dilution,
  glu_48.0C_mean_luminescence = glu_48.0C_mean_lum,
  glu_48.0C_se = glu_48.0C_se_lum,
  glu_48.0C_conc =  glu_48.0C_mean_conc,
  label = paste0("48.0Cdf.", glu_48.0C_dilution)
)
glu_48.0C_mean_lum
glu_48.0C_mean_conc
```

    [1] 315.3333
    [1] 0.5355468

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.0D_mean_lum <- numeric(length(glu_48.0D_dilution))
glu_48.0D_se_lum <- numeric(length(glu_48.0D_dilution))
glu_48.0D_mean_conc <- numeric(length(glu_48.0D_dilution))

for (i in 1:length(glu_48.0D_dilution)) {
  df_val <- glu_48.0D_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.0D_lum_values <- c(glu_48.0D_luminescence[glu_48.0D_dilution == df_val])
  glu_48.0D_mean_lum[i] <- mean(glu_48.0D_lum_values)
  glu_48.0D_se_lum[i] <- sd(glu_48.0D_lum_values) / sqrt(length(glu_48.0D_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.0D_mean_conc[i] <- (glu_48.0D_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.0D_data <- data.frame(
  glu_48.0D_dilution_factor = glu_48.0D_dilution,
  glu_48.0D_mean_luminescence = glu_48.0D_mean_lum,
  glu_48.0D_se = glu_48.0D_se_lum,
  glu_48.0D_conc =  glu_48.0D_mean_conc,
  label = paste0("48.0Ddf.", glu_48.0D_dilution)
)
glu_48.0D_mean_lum
glu_48.0D_mean_conc
```

    [1] 373
    [1] 0.5876508

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.0E_mean_lum <- numeric(length(glu_48.0E_dilution))
glu_48.0E_se_lum <- numeric(length(glu_48.0E_dilution))
glu_48.0E_mean_conc <- numeric(length(glu_48.0E_dilution))

for (i in 1:length(glu_48.0E_dilution)) {
  df_val <- glu_48.0E_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.0E_lum_values <- c(glu_48.0E_luminescence[glu_48.0E_dilution == df_val])
  glu_48.0E_mean_lum[i] <- mean(glu_48.0E_lum_values)
  glu_48.0E_se_lum[i] <- sd(glu_48.0E_lum_values) / sqrt(length(glu_48.0E_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.0E_mean_conc[i] <- (glu_48.0E_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.0E_data <- data.frame(
  glu_48.0E_dilution_factor = glu_48.0E_dilution,
  glu_48.0E_mean_luminescence = glu_48.0E_mean_lum,
  glu_48.0E_se = glu_48.0E_se_lum,
  glu_48.0E_conc =  glu_48.0E_mean_conc,
  label = paste0("48.0Edf.", glu_48.0E_dilution)
)
glu_48.0E_mean_lum
glu_48.0E_mean_conc
```

    [1] 502.6667
    [1] 0.7048093

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.0F_mean_lum <- numeric(length(glu_48.0F_dilution))
glu_48.0F_se_lum <- numeric(length(glu_48.0F_dilution))
glu_48.0F_mean_conc <- numeric(length(glu_48.0F_dilution))

for (i in 1:length(glu_48.0F_dilution)) {
  df_val <- glu_48.0F_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.0F_lum_values <- c(glu_48.0F_luminescence[glu_48.0F_dilution == df_val])
  glu_48.0F_mean_lum[i] <- mean(glu_48.0F_lum_values)
  glu_48.0F_se_lum[i] <- sd(glu_48.0F_lum_values) / sqrt(length(glu_48.0F_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.0F_mean_conc[i] <- (glu_48.0F_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.0F_data <- data.frame(
  glu_48.0F_dilution_factor = glu_48.0F_dilution,
  glu_48.0F_mean_luminescence = glu_48.0F_mean_lum,
  glu_48.0F_se = glu_48.0F_se_lum,
  glu_48.0F_conc =  glu_48.0F_mean_conc,
  label = paste0("48.0Fdf.", glu_48.0F_dilution)
)
glu_48.0F_mean_lum
glu_48.0F_mean_conc
```

    [1] 306.6667
    [1] 0.5277162

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.0G_mean_lum <- numeric(length(glu_48.0G_dilution))
glu_48.0G_se_lum <- numeric(length(glu_48.0G_dilution))
glu_48.0G_mean_conc <- numeric(length(glu_48.0G_dilution))

for (i in 1:length(glu_48.0G_dilution)) {
  df_val <- glu_48.0G_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.0G_lum_values <- c(glu_48.0G_luminescence[glu_48.0G_dilution == df_val])
  glu_48.0G_mean_lum[i] <- mean(glu_48.0G_lum_values)
  glu_48.0G_se_lum[i] <- sd(glu_48.0G_lum_values) / sqrt(length(glu_48.0G_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.0G_mean_conc[i] <- (glu_48.0G_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.0G_data <- data.frame(
  glu_48.0G_dilution_factor = glu_48.0G_dilution,
  glu_48.0G_mean_luminescence = glu_48.0G_mean_lum,
  glu_48.0G_se = glu_48.0G_se_lum,
  glu_48.0G_conc =  glu_48.0G_mean_conc,
  label = paste0("48.0Gdf.", glu_48.0G_dilution)
)
glu_48.0G_mean_lum
glu_48.0G_mean_conc
```

    [1] 266.3333
    [1] 0.4912735

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.0H_mean_lum <- numeric(length(glu_48.0H_dilution))
glu_48.0H_se_lum <- numeric(length(glu_48.0H_dilution))
glu_48.0H_mean_conc <- numeric(length(glu_48.0H_dilution))

for (i in 1:length(glu_48.0H_dilution)) {
  df_val <- glu_48.0H_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.0H_lum_values <- c(glu_48.0H_luminescence[glu_48.0H_dilution == df_val])
  glu_48.0H_mean_lum[i] <- mean(glu_48.0H_lum_values)
  glu_48.0H_se_lum[i] <- sd(glu_48.0H_lum_values) / sqrt(length(glu_48.0H_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.0H_mean_conc[i] <- (glu_48.0H_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.0H_data <- data.frame(
  glu_48.0H_dilution_factor = glu_48.0H_dilution,
  glu_48.0H_mean_luminescence = glu_48.0H_mean_lum,
  glu_48.0H_se = glu_48.0H_se_lum,
  glu_48.0H_conc =  glu_48.0H_mean_conc,
  label = paste0("48.0Hdf.", glu_48.0H_dilution)
)
glu_48.0H_mean_lum
glu_48.0H_mean_conc
```

    [1] 409
    [1] 0.6201781

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.0I_mean_lum <- numeric(length(glu_48.0I_dilution))
glu_48.0I_se_lum <- numeric(length(glu_48.0I_dilution))
glu_48.0I_mean_conc <- numeric(length(glu_48.0I_dilution))

for (i in 1:length(glu_48.0I_dilution)) {
  df_val <- glu_48.0I_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.0I_lum_values <- c(glu_48.0I_luminescence[glu_48.0I_dilution == df_val])
  glu_48.0I_mean_lum[i] <- mean(glu_48.0I_lum_values)
  glu_48.0I_se_lum[i] <- sd(glu_48.0I_lum_values) / sqrt(length(glu_48.0I_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.0I_mean_conc[i] <- (glu_48.0I_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.0I_data <- data.frame(
  glu_48.0I_dilution_factor = glu_48.0I_dilution,
  glu_48.0I_mean_luminescence = glu_48.0I_mean_lum,
  glu_48.0I_se = glu_48.0I_se_lum,
  glu_48.0I_conc =  glu_48.0I_mean_conc,
  label = paste0("48.0Idf.", glu_48.0I_dilution)
)
glu_48.0I_mean_lum
glu_48.0I_mean_conc
```

    [1] 314
    [1] 0.5343421

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.0J_mean_lum <- numeric(length(glu_48.0J_dilution))
glu_48.0J_se_lum <- numeric(length(glu_48.0J_dilution))
glu_48.0J_mean_conc <- numeric(length(glu_48.0J_dilution))

for (i in 1:length(glu_48.0J_dilution)) {
  df_val <- glu_48.0J_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.0J_lum_values <- c(glu_48.0J_luminescence[glu_48.0J_dilution == df_val])
  glu_48.0J_mean_lum[i] <- mean(glu_48.0J_lum_values)
  glu_48.0J_se_lum[i] <- sd(glu_48.0J_lum_values) / sqrt(length(glu_48.0J_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.0J_mean_conc[i] <- (glu_48.0J_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.0J_data <- data.frame(
  glu_48.0J_dilution_factor = glu_48.0J_dilution,
  glu_48.0J_mean_luminescence = glu_48.0J_mean_lum,
  glu_48.0J_se = glu_48.0J_se_lum,
  glu_48.0J_conc =  glu_48.0J_mean_conc,
  label = paste0("48.0Jdf.", glu_48.0J_dilution)
)
glu_48.0J_mean_lum
glu_48.0J_mean_conc
```

    [1] 308.6667
    [1] 0.5295232

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.2A_mean_lum <- numeric(length(glu_48.2A_dilution))
glu_48.2A_se_lum <- numeric(length(glu_48.2A_dilution))
glu_48.2A_mean_conc <- numeric(length(glu_48.2A_dilution))

for (i in 1:length(glu_48.2A_dilution)) {
  df_val <- glu_48.2A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.2A_lum_values <- c(glu_48.2A_luminescence[glu_48.2A_dilution == df_val])
  glu_48.2A_mean_lum[i] <- mean(glu_48.2A_lum_values)
  glu_48.2A_se_lum[i] <- sd(glu_48.2A_lum_values) / sqrt(length(glu_48.2A_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.2A_mean_conc[i] <- (glu_48.2A_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.2A_data <- data.frame(
  glu_48.2A_dilution_factor = glu_48.2A_dilution,
  glu_48.2A_mean_luminescence = glu_48.2A_mean_lum,
  glu_48.2A_se = glu_48.2A_se_lum,
  glu_48.2A_conc =  glu_48.2A_mean_conc,
  label = paste0("48.2Adf.", glu_48.2A_dilution)
)
glu_48.2A_mean_lum
glu_48.2A_mean_conc
```

    [1] 412.6667
    [1] 0.623491

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.2B_mean_lum <- numeric(length(glu_48.2B_dilution))
glu_48.2B_se_lum <- numeric(length(glu_48.2B_dilution))
glu_48.2B_mean_conc <- numeric(length(glu_48.2B_dilution))

for (i in 1:length(glu_48.2B_dilution)) {
  df_val <- glu_48.2B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.2B_lum_values <- c(glu_48.2B_luminescence[glu_48.2B_dilution == df_val])
  glu_48.2B_mean_lum[i] <- mean(glu_48.2B_lum_values)
  glu_48.2B_se_lum[i] <- sd(glu_48.2B_lum_values) / sqrt(length(glu_48.2B_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.2B_mean_conc[i] <- (glu_48.2B_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.2B_data <- data.frame(
  glu_48.2B_dilution_factor = glu_48.2B_dilution,
  glu_48.2B_mean_luminescence = glu_48.2B_mean_lum,
  glu_48.2B_se = glu_48.2B_se_lum,
  glu_48.2B_conc =  glu_48.2B_mean_conc,
  label = paste0("48.2Bdf.", glu_48.2B_dilution)
)
glu_48.2B_mean_lum
glu_48.2B_mean_conc
```

    [1] 301.3333
    [1] 0.5228973

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.2C_mean_lum <- numeric(length(glu_48.2C_dilution))
glu_48.2C_se_lum <- numeric(length(glu_48.2C_dilution))
glu_48.2C_mean_conc <- numeric(length(glu_48.2C_dilution))

for (i in 1:length(glu_48.2C_dilution)) {
  df_val <- glu_48.2C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.2C_lum_values <- c(glu_48.2C_luminescence[glu_48.2C_dilution == df_val])
  glu_48.2C_mean_lum[i] <- mean(glu_48.2C_lum_values)
  glu_48.2C_se_lum[i] <- sd(glu_48.2C_lum_values) / sqrt(length(glu_48.2C_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.2C_mean_conc[i] <- (glu_48.2C_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.2C_data <- data.frame(
  glu_48.2C_dilution_factor = glu_48.2C_dilution,
  glu_48.2C_mean_luminescence = glu_48.2C_mean_lum,
  glu_48.2C_se = glu_48.2C_se_lum,
  glu_48.2C_conc =  glu_48.2C_mean_conc,
  label = paste0("48.2Cdf.", glu_48.2C_dilution)
)
glu_48.2C_mean_lum
glu_48.2C_mean_conc
```

    [1] 323.3333
    [1] 0.5427751

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.2D_mean_lum <- numeric(length(glu_48.2D_dilution))
glu_48.2D_se_lum <- numeric(length(glu_48.2D_dilution))
glu_48.2D_mean_conc <- numeric(length(glu_48.2D_dilution))

for (i in 1:length(glu_48.2D_dilution)) {
  df_val <- glu_48.2D_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.2D_lum_values <- c(glu_48.2D_luminescence[glu_48.2D_dilution == df_val])
  glu_48.2D_mean_lum[i] <- mean(glu_48.2D_lum_values)
  glu_48.2D_se_lum[i] <- sd(glu_48.2D_lum_values) / sqrt(length(glu_48.2D_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.2D_mean_conc[i] <- (glu_48.2D_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.2D_data <- data.frame(
  glu_48.2D_dilution_factor = glu_48.2D_dilution,
  glu_48.2D_mean_luminescence = glu_48.2D_mean_lum,
  glu_48.2D_se = glu_48.2D_se_lum,
  glu_48.2D_conc =  glu_48.2D_mean_conc,
  label = paste0("48.2Ddf.", glu_48.2D_dilution)
)
glu_48.2D_mean_lum
glu_48.2D_mean_conc
```

    [1] 286
    [1] 0.5090431

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.2E_mean_lum <- numeric(length(glu_48.2E_dilution))
glu_48.2E_se_lum <- numeric(length(glu_48.2E_dilution))
glu_48.2E_mean_conc <- numeric(length(glu_48.2E_dilution))

for (i in 1:length(glu_48.2E_dilution)) {
  df_val <- glu_48.2E_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.2E_lum_values <- c(glu_48.2E_luminescence[glu_48.2E_dilution == df_val])
  glu_48.2E_mean_lum[i] <- mean(glu_48.2E_lum_values)
  glu_48.2E_se_lum[i] <- sd(glu_48.2E_lum_values) / sqrt(length(glu_48.2E_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.2E_mean_conc[i] <- (glu_48.2E_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.2E_data <- data.frame(
  glu_48.2E_dilution_factor = glu_48.2E_dilution,
  glu_48.2E_mean_luminescence = glu_48.2E_mean_lum,
  glu_48.2E_se = glu_48.2E_se_lum,
  glu_48.2E_conc =  glu_48.2E_mean_conc,
  label = paste0("48.2Edf.", glu_48.2E_dilution)
)
glu_48.2E_mean_lum
glu_48.2E_mean_conc
```

    [1] 334.6667
    [1] 0.5530152

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.2F_mean_lum <- numeric(length(glu_48.2F_dilution))
glu_48.2F_se_lum <- numeric(length(glu_48.2F_dilution))
glu_48.2F_mean_conc <- numeric(length(glu_48.2F_dilution))

for (i in 1:length(glu_48.2F_dilution)) {
  df_val <- glu_48.2F_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.2F_lum_values <- c(glu_48.2F_luminescence[glu_48.2F_dilution == df_val])
  glu_48.2F_mean_lum[i] <- mean(glu_48.2F_lum_values)
  glu_48.2F_se_lum[i] <- sd(glu_48.2F_lum_values) / sqrt(length(glu_48.2F_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.2F_mean_conc[i] <- (glu_48.2F_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.2F_data <- data.frame(
  glu_48.2F_dilution_factor = glu_48.2F_dilution,
  glu_48.2F_mean_luminescence = glu_48.2F_mean_lum,
  glu_48.2F_se = glu_48.2F_se_lum,
  glu_48.2F_conc =  glu_48.2F_mean_conc,
  label = paste0("48.2Fdf.", glu_48.2F_dilution)
)
glu_48.2F_mean_lum
glu_48.2F_mean_conc
```

    [1] 432.6667
    [1] 0.6415618

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.2G_mean_lum <- numeric(length(glu_48.2G_dilution))
glu_48.2G_se_lum <- numeric(length(glu_48.2G_dilution))
glu_48.2G_mean_conc <- numeric(length(glu_48.2G_dilution))

for (i in 1:length(glu_48.2G_dilution)) {
  df_val <- glu_48.2G_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.2G_lum_values <- c(glu_48.2G_luminescence[glu_48.2G_dilution == df_val])
  glu_48.2G_mean_lum[i] <- mean(glu_48.2G_lum_values)
  glu_48.2G_se_lum[i] <- sd(glu_48.2G_lum_values) / sqrt(length(glu_48.2G_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.2G_mean_conc[i] <- (glu_48.2G_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.2G_data <- data.frame(
  glu_48.2G_dilution_factor = glu_48.2G_dilution,
  glu_48.2G_mean_luminescence = glu_48.2G_mean_lum,
  glu_48.2G_se = glu_48.2G_se_lum,
  glu_48.2G_conc =  glu_48.2G_mean_conc,
  label = paste0("48.2Gdf.", glu_48.2G_dilution)
)
glu_48.2G_mean_lum
glu_48.2G_mean_conc
```

    [1] 430.6667
    [1] 0.6397547

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.2H_mean_lum <- numeric(length(glu_48.2H_dilution))
glu_48.2H_se_lum <- numeric(length(glu_48.2H_dilution))
glu_48.2H_mean_conc <- numeric(length(glu_48.2H_dilution))

for (i in 1:length(glu_48.2H_dilution)) {
  df_val <- glu_48.2H_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.2H_lum_values <- c(glu_48.2H_luminescence[glu_48.2H_dilution == df_val])
  glu_48.2H_mean_lum[i] <- mean(glu_48.2H_lum_values)
  glu_48.2H_se_lum[i] <- sd(glu_48.2H_lum_values) / sqrt(length(glu_48.2H_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.2H_mean_conc[i] <- (glu_48.2H_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.2H_data <- data.frame(
  glu_48.2H_dilution_factor = glu_48.2H_dilution,
  glu_48.2H_mean_luminescence = glu_48.2H_mean_lum,
  glu_48.2H_se = glu_48.2H_se_lum,
  glu_48.2H_conc =  glu_48.2H_mean_conc,
  label = paste0("48.2Hdf.", glu_48.2H_dilution)
)
glu_48.2H_mean_lum
glu_48.2H_mean_conc
```

    [1] 368.3333
    [1] 0.5834343

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.2I_mean_lum <- numeric(length(glu_48.2I_dilution))
glu_48.2I_se_lum <- numeric(length(glu_48.2I_dilution))
glu_48.2I_mean_conc <- numeric(length(glu_48.2I_dilution))

for (i in 1:length(glu_48.2I_dilution)) {
  df_val <- glu_48.2I_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.2I_lum_values <- c(glu_48.2I_luminescence[glu_48.2I_dilution == df_val])
  glu_48.2I_mean_lum[i] <- mean(glu_48.2I_lum_values)
  glu_48.2I_se_lum[i] <- sd(glu_48.2I_lum_values) / sqrt(length(glu_48.2I_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.2I_mean_conc[i] <- (glu_48.2I_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.2I_data <- data.frame(
  glu_48.2I_dilution_factor = glu_48.2I_dilution,
  glu_48.2I_mean_luminescence = glu_48.2I_mean_lum,
  glu_48.2I_se = glu_48.2I_se_lum,
  glu_48.2I_conc =  glu_48.2I_mean_conc,
  label = paste0("48.2Idf.", glu_48.2I_dilution)
)
glu_48.2I_mean_lum
glu_48.2I_mean_conc
```

    [1] 335
    [1] 0.5533164

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.2J_mean_lum <- numeric(length(glu_48.2J_dilution))
glu_48.2J_se_lum <- numeric(length(glu_48.2J_dilution))
glu_48.2J_mean_conc <- numeric(length(glu_48.2J_dilution))

for (i in 1:length(glu_48.2J_dilution)) {
  df_val <- glu_48.2J_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.2J_lum_values <- c(glu_48.2J_luminescence[glu_48.2J_dilution == df_val])
  glu_48.2J_mean_lum[i] <- mean(glu_48.2J_lum_values)
  glu_48.2J_se_lum[i] <- sd(glu_48.2J_lum_values) / sqrt(length(glu_48.2J_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.2J_mean_conc[i] <- (glu_48.2J_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.2J_data <- data.frame(
  glu_48.2J_dilution_factor = glu_48.2J_dilution,
  glu_48.2J_mean_luminescence = glu_48.2J_mean_lum,
  glu_48.2J_se = glu_48.2J_se_lum,
  glu_48.2J_conc =  glu_48.2J_mean_conc,
  label = paste0("48.2Jdf.", glu_48.2J_dilution)
)
glu_48.2J_mean_lum
glu_48.2J_mean_conc
```

    [1] 363
    [1] 0.5786154

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.7A_mean_lum <- numeric(length(glu_48.7A_dilution))
glu_48.7A_se_lum <- numeric(length(glu_48.7A_dilution))
glu_48.7A_mean_conc <- numeric(length(glu_48.7A_dilution))

for (i in 1:length(glu_48.7A_dilution)) {
  df_val <- glu_48.7A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.7A_lum_values <- c(glu_48.7A_luminescence[glu_48.7A_dilution == df_val])
  glu_48.7A_mean_lum[i] <- mean(glu_48.7A_lum_values)
  glu_48.7A_se_lum[i] <- sd(glu_48.7A_lum_values) / sqrt(length(glu_48.7A_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.7A_mean_conc[i] <- (glu_48.7A_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.7A_data <- data.frame(
  glu_48.7A_dilution_factor = glu_48.7A_dilution,
  glu_48.7A_mean_luminescence = glu_48.7A_mean_lum,
  glu_48.7A_se = glu_48.7A_se_lum,
  glu_48.7A_conc =  glu_48.7A_mean_conc,
  label = paste0("48.7Adf.", glu_48.7A_dilution)
)
glu_48.7A_mean_lum
glu_48.7A_mean_conc
```

    [1] 367.6667
    [1] 0.5828319

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.7B_mean_lum <- numeric(length(glu_48.7B_dilution))
glu_48.7B_se_lum <- numeric(length(glu_48.7B_dilution))
glu_48.7B_mean_conc <- numeric(length(glu_48.7B_dilution))

for (i in 1:length(glu_48.7B_dilution)) {
  df_val <- glu_48.7B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.7B_lum_values <- c(glu_48.7B_luminescence[glu_48.7B_dilution == df_val])
  glu_48.7B_mean_lum[i] <- mean(glu_48.7B_lum_values)
  glu_48.7B_se_lum[i] <- sd(glu_48.7B_lum_values) / sqrt(length(glu_48.7B_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.7B_mean_conc[i] <- (glu_48.7B_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.7B_data <- data.frame(
  glu_48.7B_dilution_factor = glu_48.7B_dilution,
  glu_48.7B_mean_luminescence = glu_48.7B_mean_lum,
  glu_48.7B_se = glu_48.7B_se_lum,
  glu_48.7B_conc =  glu_48.7B_mean_conc,
  label = paste0("48.7Bdf.", glu_48.7B_dilution)
)
glu_48.7B_mean_lum
glu_48.7B_mean_conc
```

    [1] 378.6667
    [1] 0.5927708

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.7C_mean_lum <- numeric(length(glu_48.7C_dilution))
glu_48.7C_se_lum <- numeric(length(glu_48.7C_dilution))
glu_48.7C_mean_conc <- numeric(length(glu_48.7C_dilution))

for (i in 1:length(glu_48.7C_dilution)) {
  df_val <- glu_48.7C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.7C_lum_values <- c(glu_48.7C_luminescence[glu_48.7C_dilution == df_val])
  glu_48.7C_mean_lum[i] <- mean(glu_48.7C_lum_values)
  glu_48.7C_se_lum[i] <- sd(glu_48.7C_lum_values) / sqrt(length(glu_48.7C_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.7C_mean_conc[i] <- (glu_48.7C_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.7C_data <- data.frame(
  glu_48.7C_dilution_factor = glu_48.7C_dilution,
  glu_48.7C_mean_luminescence = glu_48.7C_mean_lum,
  glu_48.7C_se = glu_48.7C_se_lum,
  glu_48.7C_conc =  glu_48.7C_mean_conc,
  label = paste0("48.7Cdf.", glu_48.7C_dilution)
)
glu_48.7C_mean_lum
glu_48.7C_mean_conc
```

    [1] 361.6667
    [1] 0.5774107

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.7D_mean_lum <- numeric(length(glu_48.7D_dilution))
glu_48.7D_se_lum <- numeric(length(glu_48.7D_dilution))
glu_48.7D_mean_conc <- numeric(length(glu_48.7D_dilution))

for (i in 1:length(glu_48.7D_dilution)) {
  df_val <- glu_48.7D_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.7D_lum_values <- c(glu_48.7D_luminescence[glu_48.7D_dilution == df_val])
  glu_48.7D_mean_lum[i] <- mean(glu_48.7D_lum_values)
  glu_48.7D_se_lum[i] <- sd(glu_48.7D_lum_values) / sqrt(length(glu_48.7D_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.7D_mean_conc[i] <- (glu_48.7D_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.7D_data <- data.frame(
  glu_48.7D_dilution_factor = glu_48.7D_dilution,
  glu_48.7D_mean_luminescence = glu_48.7D_mean_lum,
  glu_48.7D_se = glu_48.7D_se_lum,
  glu_48.7D_conc =  glu_48.7D_mean_conc,
  label = paste0("48.7Ddf.", glu_48.7D_dilution)
)
glu_48.7D_mean_lum
glu_48.7D_mean_conc
```

    [1] 394
    [1] 0.606625

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.7E_mean_lum <- numeric(length(glu_48.7E_dilution))
glu_48.7E_se_lum <- numeric(length(glu_48.7E_dilution))
glu_48.7E_mean_conc <- numeric(length(glu_48.7E_dilution))

for (i in 1:length(glu_48.7E_dilution)) {
  df_val <- glu_48.7E_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.7E_lum_values <- c(glu_48.7E_luminescence[glu_48.7E_dilution == df_val])
  glu_48.7E_mean_lum[i] <- mean(glu_48.7E_lum_values)
  glu_48.7E_se_lum[i] <- sd(glu_48.7E_lum_values) / sqrt(length(glu_48.7E_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.7E_mean_conc[i] <- (glu_48.7E_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.7E_data <- data.frame(
  glu_48.7E_dilution_factor = glu_48.7E_dilution,
  glu_48.7E_mean_luminescence = glu_48.7E_mean_lum,
  glu_48.7E_se = glu_48.7E_se_lum,
  glu_48.7E_conc =  glu_48.7E_mean_conc,
  label = paste0("48.7Edf.", glu_48.7E_dilution)
)
glu_48.7E_mean_lum
glu_48.7E_mean_conc
```

    [1] 403
    [1] 0.6147569

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.7F_mean_lum <- numeric(length(glu_48.7F_dilution))
glu_48.7F_se_lum <- numeric(length(glu_48.7F_dilution))
glu_48.7F_mean_conc <- numeric(length(glu_48.7F_dilution))

for (i in 1:length(glu_48.7F_dilution)) {
  df_val <- glu_48.7F_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.7F_lum_values <- c(glu_48.7F_luminescence[glu_48.7F_dilution == df_val])
  glu_48.7F_mean_lum[i] <- mean(glu_48.7F_lum_values)
  glu_48.7F_se_lum[i] <- sd(glu_48.7F_lum_values) / sqrt(length(glu_48.7F_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.7F_mean_conc[i] <- (glu_48.7F_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.7F_data <- data.frame(
  glu_48.7F_dilution_factor = glu_48.7F_dilution,
  glu_48.7F_mean_luminescence = glu_48.7F_mean_lum,
  glu_48.7F_se = glu_48.7F_se_lum,
  glu_48.7F_conc =  glu_48.7F_mean_conc,
  label = paste0("48.7Fdf.", glu_48.7F_dilution)
)
glu_48.7F_mean_lum
glu_48.7F_mean_conc
```

    [1] 329.6667
    [1] 0.5484975

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.7G_mean_lum <- numeric(length(glu_48.7G_dilution))
glu_48.7G_se_lum <- numeric(length(glu_48.7G_dilution))
glu_48.7G_mean_conc <- numeric(length(glu_48.7G_dilution))

for (i in 1:length(glu_48.7G_dilution)) {
  df_val <- glu_48.7G_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.7G_lum_values <- c(glu_48.7G_luminescence[glu_48.7G_dilution == df_val])
  glu_48.7G_mean_lum[i] <- mean(glu_48.7G_lum_values)
  glu_48.7G_se_lum[i] <- sd(glu_48.7G_lum_values) / sqrt(length(glu_48.7G_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.7G_mean_conc[i] <- (glu_48.7G_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.7G_data <- data.frame(
  glu_48.7G_dilution_factor = glu_48.7G_dilution,
  glu_48.7G_mean_luminescence = glu_48.7G_mean_lum,
  glu_48.7G_se = glu_48.7G_se_lum,
  glu_48.7G_conc =  glu_48.7G_mean_conc,
  label = paste0("48.7Gdf.", glu_48.7G_dilution)
)
glu_48.7G_mean_lum
glu_48.7G_mean_conc
```

    [1] 458
    [1] 0.6644514

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.7H_mean_lum <- numeric(length(glu_48.7H_dilution))
glu_48.7H_se_lum <- numeric(length(glu_48.7H_dilution))
glu_48.7H_mean_conc <- numeric(length(glu_48.7H_dilution))

for (i in 1:length(glu_48.7H_dilution)) {
  df_val <- glu_48.7H_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.7H_lum_values <- c(glu_48.7H_luminescence[glu_48.7H_dilution == df_val])
  glu_48.7H_mean_lum[i] <- mean(glu_48.7H_lum_values)
  glu_48.7H_se_lum[i] <- sd(glu_48.7H_lum_values) / sqrt(length(glu_48.7H_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.7H_mean_conc[i] <- (glu_48.7H_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.7H_data <- data.frame(
  glu_48.7H_dilution_factor = glu_48.7H_dilution,
  glu_48.7H_mean_luminescence = glu_48.7H_mean_lum,
  glu_48.7H_se = glu_48.7H_se_lum,
  glu_48.7H_conc =  glu_48.7H_mean_conc,
  label = paste0("48.7Hdf.", glu_48.7H_dilution)
)
glu_48.7H_mean_lum
glu_48.7H_mean_conc
```

    [1] 404
    [1] 0.6156604

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.7I_mean_lum <- numeric(length(glu_48.7I_dilution))
glu_48.7I_se_lum <- numeric(length(glu_48.7I_dilution))
glu_48.7I_mean_conc <- numeric(length(glu_48.7I_dilution))

for (i in 1:length(glu_48.7I_dilution)) {
  df_val <- glu_48.7I_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.7I_lum_values <- c(glu_48.7I_luminescence[glu_48.7I_dilution == df_val])
  glu_48.7I_mean_lum[i] <- mean(glu_48.7I_lum_values)
  glu_48.7I_se_lum[i] <- sd(glu_48.7I_lum_values) / sqrt(length(glu_48.7I_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.7I_mean_conc[i] <- (glu_48.7I_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.7I_data <- data.frame(
  glu_48.7I_dilution_factor = glu_48.7I_dilution,
  glu_48.7I_mean_luminescence = glu_48.7I_mean_lum,
  glu_48.7I_se = glu_48.7I_se_lum,
  glu_48.7I_conc =  glu_48.7I_mean_conc,
  label = paste0("48.7Idf.", glu_48.7I_dilution)
)
glu_48.7I_mean_lum
glu_48.7I_mean_conc
```

    [1] 409
    [1] 0.6201781

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_48.7J_mean_lum <- numeric(length(glu_48.7J_dilution))
glu_48.7J_se_lum <- numeric(length(glu_48.7J_dilution))
glu_48.7J_mean_conc <- numeric(length(glu_48.7J_dilution))

for (i in 1:length(glu_48.7J_dilution)) {
  df_val <- glu_48.7J_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_48.7J_lum_values <- c(glu_48.7J_luminescence[glu_48.7J_dilution == df_val])
  glu_48.7J_mean_lum[i] <- mean(glu_48.7J_lum_values)
  glu_48.7J_se_lum[i] <- sd(glu_48.7J_lum_values) / sqrt(length(glu_48.7J_lum_values))
  # Calculate concentration from mean luminescence
  glu_48.7J_mean_conc[i] <- (glu_48.7J_mean_lum[i] - glu_intercept) / glu_slope
}

glu_48.7J_data <- data.frame(
  glu_48.7J_dilution_factor = glu_48.7J_dilution,
  glu_48.7J_mean_luminescence = glu_48.7J_mean_lum,
  glu_48.7J_se = glu_48.7J_se_lum,
  glu_48.7J_conc =  glu_48.7J_mean_conc,
  label = paste0("48.7Jdf.", glu_48.7J_dilution)
)
glu_48.7J_mean_lum
glu_48.7J_mean_conc
```

    [1] 333.6667
    [1] 0.5521117

### 1.2.4 Plot glucose standard curve, sample points

``` r
# Create the plot
glu_plot <- ggplot(glucose_summary_data, aes(x = glu_concentration, y = glu_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glu_mean_luminescence - glu_se, ymax = glu_mean_luminescence + glu_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +
  
   geom_errorbar(data = glu_0.0A_data, aes(x = glu_0.0A_conc, y = glu_0.0A_mean_luminescence,
                ymin = glu_0.0A_mean_luminescence - glu_0.0A_se, ymax = glu_0.0A_mean_luminescence + glu_0.0A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_0.0A_data, aes(x = glu_0.0A_conc, y = glu_0.0A_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_0.0B_data, aes(x = glu_0.0B_conc, y = glu_0.0B_mean_luminescence,
                ymin = glu_0.0B_mean_luminescence - glu_0.0B_se, ymax = glu_0.0B_mean_luminescence + glu_0.0B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_0.0B_data, aes(x = glu_0.0B_conc, y = glu_0.0B_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_0.0C_data, aes(x = glu_0.0C_conc, y = glu_0.0C_mean_luminescence,
                ymin = glu_0.0C_mean_luminescence - glu_0.0C_se, ymax = glu_0.0C_mean_luminescence + glu_0.0C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_0.0C_data, aes(x = glu_0.0C_conc, y = glu_0.0C_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_0.0D_data, aes(x = glu_0.0D_conc, y = glu_0.0D_mean_luminescence,
                ymin = glu_0.0D_mean_luminescence - glu_0.0D_se, ymax = glu_0.0D_mean_luminescence + glu_0.0D_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_0.0D_data, aes(x = glu_0.0D_conc, y = glu_0.0D_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_0.0E_data, aes(x = glu_0.0E_conc, y = glu_0.0E_mean_luminescence,
                ymin = glu_0.0E_mean_luminescence - glu_0.0E_se, ymax = glu_0.0E_mean_luminescence + glu_0.0E_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_0.0E_data, aes(x = glu_0.0E_conc, y = glu_0.0E_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_0.0F_data, aes(x = glu_0.0F_conc, y = glu_0.0F_mean_luminescence,
                ymin = glu_0.0F_mean_luminescence - glu_0.0F_se, ymax = glu_0.0F_mean_luminescence + glu_0.0F_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_0.0F_data, aes(x = glu_0.0F_conc, y = glu_0.0F_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_0.0G_data, aes(x = glu_0.0G_conc, y = glu_0.0G_mean_luminescence,
                ymin = glu_0.0G_mean_luminescence - glu_0.0G_se, ymax = glu_0.0G_mean_luminescence + glu_0.0G_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_0.0G_data, aes(x = glu_0.0G_conc, y = glu_0.0G_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_0.0H_data, aes(x = glu_0.0H_conc, y = glu_0.0H_mean_luminescence,
                ymin = glu_0.0H_mean_luminescence - glu_0.0H_se, ymax = glu_0.0H_mean_luminescence + glu_0.0H_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_0.0H_data, aes(x = glu_0.0H_conc, y = glu_0.0H_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_0.0I_data, aes(x = glu_0.0I_conc, y = glu_0.0I_mean_luminescence,
                ymin = glu_0.0I_mean_luminescence - glu_0.0I_se, ymax = glu_0.0I_mean_luminescence + glu_0.0I_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_0.0I_data, aes(x = glu_0.0I_conc, y = glu_0.0I_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_0.0J_data, aes(x = glu_0.0J_conc, y = glu_0.0J_mean_luminescence,
                ymin = glu_0.0J_mean_luminescence - glu_0.0J_se, ymax = glu_0.0J_mean_luminescence + glu_0.0J_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_0.0J_data, aes(x = glu_0.0J_conc, y = glu_0.0J_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_48.0A_data, aes(x = glu_48.0A_conc, y = glu_48.0A_mean_luminescence,
                ymin = glu_48.0A_mean_luminescence - glu_48.0A_se, ymax = glu_48.0A_mean_luminescence + glu_48.0A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.0A_data, aes(x = glu_48.0A_conc, y = glu_48.0A_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_48.0B_data, aes(x = glu_48.0B_conc, y = glu_48.0B_mean_luminescence,
                ymin = glu_48.0B_mean_luminescence - glu_48.0B_se, ymax = glu_48.0B_mean_luminescence + glu_48.0B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.0B_data, aes(x = glu_48.0B_conc, y = glu_48.0B_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_48.0C_data, aes(x = glu_48.0C_conc, y = glu_48.0C_mean_luminescence,
                ymin = glu_48.0C_mean_luminescence - glu_48.0C_se, ymax = glu_48.0C_mean_luminescence + glu_48.0C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.0C_data, aes(x = glu_48.0C_conc, y = glu_48.0C_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_48.0D_data, aes(x = glu_48.0D_conc, y = glu_48.0D_mean_luminescence,
                ymin = glu_48.0D_mean_luminescence - glu_48.0D_se, ymax = glu_48.0D_mean_luminescence + glu_48.0D_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.0D_data, aes(x = glu_48.0D_conc, y = glu_48.0D_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_48.0E_data, aes(x = glu_48.0E_conc, y = glu_48.0E_mean_luminescence,
                ymin = glu_48.0E_mean_luminescence - glu_48.0E_se, ymax = glu_48.0E_mean_luminescence + glu_48.0E_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.0E_data, aes(x = glu_48.0E_conc, y = glu_48.0E_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_48.0F_data, aes(x = glu_48.0F_conc, y = glu_48.0F_mean_luminescence,
                ymin = glu_48.0F_mean_luminescence - glu_48.0F_se, ymax = glu_48.0F_mean_luminescence + glu_48.0F_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.0F_data, aes(x = glu_48.0F_conc, y = glu_48.0F_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_48.0G_data, aes(x = glu_48.0G_conc, y = glu_48.0G_mean_luminescence,
                ymin = glu_48.0G_mean_luminescence - glu_48.0G_se, ymax = glu_48.0G_mean_luminescence + glu_48.0G_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.0G_data, aes(x = glu_48.0G_conc, y = glu_48.0G_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_48.0H_data, aes(x = glu_48.0H_conc, y = glu_48.0H_mean_luminescence,
                ymin = glu_48.0H_mean_luminescence - glu_48.0H_se, ymax = glu_48.0H_mean_luminescence + glu_48.0H_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.0H_data, aes(x = glu_48.0H_conc, y = glu_48.0H_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_48.0I_data, aes(x = glu_48.0I_conc, y = glu_48.0I_mean_luminescence,
                ymin = glu_48.0I_mean_luminescence - glu_48.0I_se, ymax = glu_48.0I_mean_luminescence + glu_48.0I_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.0I_data, aes(x = glu_48.0I_conc, y = glu_48.0I_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_48.0J_data, aes(x = glu_48.0J_conc, y = glu_48.0J_mean_luminescence,
                ymin = glu_48.0J_mean_luminescence - glu_48.0J_se, ymax = glu_48.0J_mean_luminescence + glu_48.0J_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.0J_data, aes(x = glu_48.0J_conc, y = glu_48.0J_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_48.2A_data, aes(x = glu_48.2A_conc, y = glu_48.2A_mean_luminescence,
                ymin = glu_48.2A_mean_luminescence - glu_48.2A_se, ymax = glu_48.2A_mean_luminescence + glu_48.2A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.2A_data, aes(x = glu_48.2A_conc, y = glu_48.2A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
      geom_errorbar(data = glu_48.2B_data, aes(x = glu_48.2B_conc, y = glu_48.2B_mean_luminescence,
                ymin = glu_48.2B_mean_luminescence - glu_48.2B_se, ymax = glu_48.2B_mean_luminescence + glu_48.2B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.2B_data, aes(x = glu_48.2B_conc, y = glu_48.2B_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_48.2C_data, aes(x = glu_48.2C_conc, y = glu_48.2C_mean_luminescence,
                ymin = glu_48.2C_mean_luminescence - glu_48.2C_se, ymax = glu_48.2C_mean_luminescence + glu_48.2C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.2C_data, aes(x = glu_48.2C_conc, y = glu_48.2C_mean_luminescence,
             color = label, shape = label), size = 4) +

   geom_errorbar(data = glu_48.2D_data, aes(x = glu_48.2D_conc, y = glu_48.2D_mean_luminescence,
                ymin = glu_48.2D_mean_luminescence - glu_48.2D_se, ymax = glu_48.2D_mean_luminescence + glu_48.2D_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.2D_data, aes(x = glu_48.2D_conc, y = glu_48.2D_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_48.2E_data, aes(x = glu_48.2E_conc, y = glu_48.2E_mean_luminescence,
                ymin = glu_48.2E_mean_luminescence - glu_48.2E_se, ymax = glu_48.2E_mean_luminescence + glu_48.2E_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.2E_data, aes(x = glu_48.2E_conc, y = glu_48.2E_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_48.2F_data, aes(x = glu_48.2F_conc, y = glu_48.2F_mean_luminescence,
                ymin = glu_48.2F_mean_luminescence - glu_48.2F_se, ymax = glu_48.2F_mean_luminescence + glu_48.2F_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.2F_data, aes(x = glu_48.2F_conc, y = glu_48.2F_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_48.2G_data, aes(x = glu_48.2G_conc, y = glu_48.2G_mean_luminescence,
                ymin = glu_48.2G_mean_luminescence - glu_48.2G_se, ymax = glu_48.2G_mean_luminescence + glu_48.2G_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.2G_data, aes(x = glu_48.2G_conc, y = glu_48.2G_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_48.2H_data, aes(x = glu_48.2H_conc, y = glu_48.2H_mean_luminescence,
                ymin = glu_48.2H_mean_luminescence - glu_48.2H_se, ymax = glu_48.2H_mean_luminescence + glu_48.2H_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.2H_data, aes(x = glu_48.2H_conc, y = glu_48.2H_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_48.2I_data, aes(x = glu_48.2I_conc, y = glu_48.2I_mean_luminescence,
                ymin = glu_48.2I_mean_luminescence - glu_48.2I_se, ymax = glu_48.2I_mean_luminescence + glu_48.2I_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.2I_data, aes(x = glu_48.2I_conc, y = glu_48.2I_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_48.2J_data, aes(x = glu_48.2J_conc, y = glu_48.2J_mean_luminescence,
                ymin = glu_48.2J_mean_luminescence - glu_48.2J_se, ymax = glu_48.2J_mean_luminescence + glu_48.2J_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.2J_data, aes(x = glu_48.2J_conc, y = glu_48.2J_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_48.7A_data, aes(x = glu_48.7A_conc, y = glu_48.7A_mean_luminescence,
                ymin = glu_48.7A_mean_luminescence - glu_48.7A_se, ymax = glu_48.7A_mean_luminescence + glu_48.7A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.7A_data, aes(x = glu_48.7A_conc, y = glu_48.7A_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_48.7B_data, aes(x = glu_48.7B_conc, y = glu_48.7B_mean_luminescence,
                ymin = glu_48.7B_mean_luminescence - glu_48.7B_se, ymax = glu_48.7B_mean_luminescence + glu_48.7B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.7B_data, aes(x = glu_48.7B_conc, y = glu_48.7B_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_48.7C_data, aes(x = glu_48.7C_conc, y = glu_48.7C_mean_luminescence,
                ymin = glu_48.7C_mean_luminescence - glu_48.7C_se, ymax = glu_48.7C_mean_luminescence + glu_48.7C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.7C_data, aes(x = glu_48.7C_conc, y = glu_48.7C_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_48.7D_data, aes(x = glu_48.7D_conc, y = glu_48.7D_mean_luminescence,
                ymin = glu_48.7D_mean_luminescence - glu_48.7D_se, ymax = glu_48.7D_mean_luminescence + glu_48.7D_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.7D_data, aes(x = glu_48.7D_conc, y = glu_48.7D_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_48.7E_data, aes(x = glu_48.7E_conc, y = glu_48.7E_mean_luminescence,
                ymin = glu_48.7E_mean_luminescence - glu_48.7E_se, ymax = glu_48.7E_mean_luminescence + glu_48.7E_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.7E_data, aes(x = glu_48.7E_conc, y = glu_48.7E_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  
    geom_errorbar(data = glu_48.7F_data, aes(x = glu_48.7F_conc, y = glu_48.7F_mean_luminescence,
                ymin = glu_48.7F_mean_luminescence - glu_48.7F_se, ymax = glu_48.7F_mean_luminescence + glu_48.7F_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.7F_data, aes(x = glu_48.7F_conc, y = glu_48.7F_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_48.7G_data, aes(x = glu_48.7G_conc, y = glu_48.7G_mean_luminescence,
                ymin = glu_48.7G_mean_luminescence - glu_48.7G_se, ymax = glu_48.7G_mean_luminescence + glu_48.7G_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.7G_data, aes(x = glu_48.7G_conc, y = glu_48.7G_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_48.7H_data, aes(x = glu_48.7H_conc, y = glu_48.7H_mean_luminescence,
                ymin = glu_48.7H_mean_luminescence - glu_48.7H_se, ymax = glu_48.7H_mean_luminescence + glu_48.7H_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.7H_data, aes(x = glu_48.7H_conc, y = glu_48.7H_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_48.7I_data, aes(x = glu_48.7I_conc, y = glu_48.7I_mean_luminescence,
                ymin = glu_48.7I_mean_luminescence - glu_48.7I_se, ymax = glu_48.7I_mean_luminescence + glu_48.7I_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.7I_data, aes(x = glu_48.7I_conc, y = glu_48.7I_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_48.7J_data, aes(x = glu_48.7J_conc, y = glu_48.7J_mean_luminescence,
                ymin = glu_48.7J_mean_luminescence - glu_48.7J_se, ymax = glu_48.7J_mean_luminescence + glu_48.7J_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_48.7J_data, aes(x = glu_48.7J_conc, y = glu_48.7J_mean_luminescence,
             color = label, shape = label), size = 4) +

  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                               "0.0Adf.20" = "darkorange",
                                "0.0Bdf.20" = "darkgreen",
                                "0.0Cdf.20" = "purple",
                                "0.0Ddf.20" = "brown",
                                "0.0Edf.20" = "green3",
                                "0.0Fdf.20" = "firebrick1",
                                "0.0Gdf.20" = "cyan",
                                "0.0Hdf.20" = "yellow3",
                                "0.0Idf.20" = "thistle3",
                                "0.0Jdf.20" = "azure4",
                                "48.0Adf.20" = "bisque4",
                                "48.0Bdf.20" = "chartreuse",
                                "48.0Cdf.20" = "chocolate4",
                                "48.0Ddf.20" = "darkslategray",
                                "48.0Edf.20" = "deeppink2",
                                "48.0Fdf.20" = "goldenrod3",
                                "48.0Gdf.20" = "hotpink3",
                                "48.0Hdf.20" = "mediumpurple2",
                                "48.0Idf.20" = "yellow",
                                "48.0Jdf.20" = "gray1",
                                "48.2Adf.20" = "darkcyan",
                                "48.2Bdf.20" = "darkorange",
                                "48.2Cdf.20" = "darkgreen",
                                "48.2Ddf.20" = "purple",
                                "48.2Edf.20" = "brown",
                                "48.2Fdf.20" = "green3",
                                "48.2Gdf.20" = "firebrick1",
                                "48.2Hdf.20" = "cyan",
                                "48.2Idf.20" = "yellow3",
                                "48.2Jdf.20" = "thistle3",
                                "48.7Adf.20" = "azure4",
                                "48.7Bdf.20" = "bisque4",
                                "48.7Cdf.20" = "chartreuse",
                                "48.7Ddf.20" = "chocolate4",
                                "48.7Edf.20" = "darkslategray",
                                "48.7Fdf.20" = "deeppink2",
                                "48.7Gdf.20" = "goldenrod3",
                                "48.7Hdf.20" = "hotpink3",
                                "48.7Idf.20" = "mediumpurple2",
                                "48.7Jdf.20" = "yellow"
                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                "0.0Adf.20" = 16,
                                "0.0Bdf.20" = 17,
                                "0.0Cdf.20" = 15,
                                "0.0Ddf.20" = 18,
                                "0.0Edf.20" = 8,
                                "0.0Fdf.20" = 4,
                                "0.0Gdf.20" = 5,
                                "0.0Hdf.20" = 0,
                                "0.0Idf.20" = 2,
                                "0.0Jdf.20" = 19,
                                "48.0Adf.20" = 20,
                                "48.0Bdf.20" = 17,
                                "48.0Cdf.20" = 15,
                                "48.0Ddf.20" = 18,
                                "48.0Edf.20" = 8,
                                "48.0Fdf.20" = 4,
                                "48.0Gdf.20" = 0,
                                "48.0Hdf.20" = 2,
                                "48.0Idf.20" = 19,
                                "48.0Jdf.20" = 20,
                                "48.2Adf.20" = 5, 
                                "48.2Bdf.20" = 0, 
                                "48.2Cdf.20" = 2, 
                                "48.2Ddf.20" = 19, 
                                "48.2Edf.20" = 20, 
                                "48.2Fdf.20" = 8, 
                                "48.2Gdf.20" = 4,
                                "48.2Hdf.20" = 5, 
                                "48.2Idf.20" = 17, 
                                "48.2Jdf.20" = 16, 
                                "48.7Adf.20" = 18, 
                                "48.7Bdf.20" = 5, 
                                "48.7Cdf.20" = 0, 
                                "48.7Ddf.20" = 19, 
                                "48.7Edf.20" = 20, 
                                "48.7Fdf.20" = 8, 
                                "48.7Gdf.20" = 4,
                                "48.7Hdf.20" = 2, 
                                "48.7Idf.20" = 16, 
                                "48.7Jdf.20" = 17
                                )) +
  scale_linetype_manual(name = "",
                        values = c("Std Curve Best Fit Line" = "dashed")) +
  annotate("label", x = max(glucose_summary_data$glu_concentration) * 0.75, 
           y = max(glucose_summary_data$glu_mean_luminescence) * 0.15,
           label = sprintf("y = %.2fx + %.2f\nR² = %.4f", glu_slope, glu_intercept, glu_r_squared),
           size = 3.5, fontface = "bold", fill = "white", 
           color = "coral", label.padding = unit(0.3, "lines")) +
  labs(
    title = "glucose Standard Curve",
    x = "glucose Concentration (µg/µL)",
    y = "Luminescence",
    caption = "Error bars represent standard error of the mean (SEM)"
  ) +
  scale_x_continuous(breaks = glu_concentrations) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_line(linetype = "dashed", color = "grey70")
  )

# Display the plot
glu_plot
```

![](Gen5-20260417-mgig-glycogenglo_files/figure-gfm/plot%20glucose%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glu_0.0A_data
glu_0.0B_data
glu_0.0C_data
glu_0.0D_data
glu_0.0E_data
glu_0.0F_data
glu_0.0G_data
glu_0.0H_data
glu_0.0I_data
glu_0.0J_data

glu_48.0A_data
glu_48.0B_data
glu_48.0C_data
glu_48.0D_data
glu_48.0E_data
glu_48.0F_data
glu_48.0G_data
glu_48.0H_data
glu_48.0I_data
glu_48.0J_data

glu_48.2A_data
glu_48.2B_data
glu_48.2C_data
glu_48.2D_data
glu_48.2E_data
glu_48.2F_data
glu_48.2G_data
glu_48.2H_data
glu_48.2I_data
glu_48.2J_data

glu_48.7A_data
glu_48.7B_data
glu_48.7C_data
glu_48.7D_data
glu_48.7E_data
glu_48.7F_data
glu_48.7G_data
glu_48.7H_data
glu_48.7I_data
glu_48.7J_data

# Print summary statistics
cat("glucose Standard Curve Summary:\n")
cat(rep("=", 50), "\n", sep = "")
for (i in 1:nrow(glucose_summary_data)) {
  cat(sprintf("Concentration: %g µg/µL\n", glucose_summary_data$glu_concentration[i]))
  cat(sprintf("  Mean Luminescence: %.2f\n",glucose_summary_data$glu_mean_luminescence[i]))
  cat(sprintf("  Standard Error: %.2f\n", glucose_summary_data$glu_se[i]))
  cat(sprintf("  CV%%: %.2f%%\n\n", glucose_summary_data$glu_cv[i]))
}
```

      glu_0.0A_dilution_factor glu_0.0A_mean_luminescence glu_0.0A_se glu_0.0A_conc
    1                       20                   504.3333    12.25198     0.7063152
          label
    1 0.0Adf.20
      glu_0.0B_dilution_factor glu_0.0B_mean_luminescence glu_0.0B_se glu_0.0B_conc
    1                       20                   253.3333    2.403701     0.4795276
          label
    1 0.0Bdf.20
      glu_0.0C_dilution_factor glu_0.0C_mean_luminescence glu_0.0C_se glu_0.0C_conc
    1                       20                        581    26.02563     0.7755863
          label
    1 0.0Cdf.20
      glu_0.0D_dilution_factor glu_0.0D_mean_luminescence glu_0.0D_se glu_0.0D_conc
    1                       20                   361.3333    18.11384     0.5771095
          label
    1 0.0Ddf.20
      glu_0.0E_dilution_factor glu_0.0E_mean_luminescence glu_0.0E_se glu_0.0E_conc
    1                       20                   334.6667     20.6263     0.5530152
          label
    1 0.0Edf.20
      glu_0.0F_dilution_factor glu_0.0F_mean_luminescence glu_0.0F_se glu_0.0F_conc
    1                       20                        339    23.62908     0.5569305
          label
    1 0.0Fdf.20
      glu_0.0G_dilution_factor glu_0.0G_mean_luminescence glu_0.0G_se glu_0.0G_conc
    1                       20                   263.3333    14.43761     0.4885629
          label
    1 0.0Gdf.20
      glu_0.0H_dilution_factor glu_0.0H_mean_luminescence glu_0.0H_se glu_0.0H_conc
    1                       20                        252     12.8582     0.4783228
          label
    1 0.0Hdf.20
      glu_0.0I_dilution_factor glu_0.0I_mean_luminescence glu_0.0I_se glu_0.0I_conc
    1                       20                   249.6667    1.452966     0.4762146
          label
    1 0.0Idf.20
      glu_0.0J_dilution_factor glu_0.0J_mean_luminescence glu_0.0J_se glu_0.0J_conc
    1                       20                   313.6667    16.23097     0.5340409
          label
    1 0.0Jdf.20
      glu_48.0A_dilution_factor glu_48.0A_mean_luminescence glu_48.0A_se
    1                        20                         332     15.52417
      glu_48.0A_conc      label
    1      0.5506058 48.0Adf.20
      glu_48.0B_dilution_factor glu_48.0B_mean_luminescence glu_48.0B_se
    1                        20                         927     637.1507
      glu_48.0B_conc      label
    1        1.08821 48.0Bdf.20
      glu_48.0C_dilution_factor glu_48.0C_mean_luminescence glu_48.0C_se
    1                        20                    315.3333     28.47416
      glu_48.0C_conc      label
    1      0.5355468 48.0Cdf.20
      glu_48.0D_dilution_factor glu_48.0D_mean_luminescence glu_48.0D_se
    1                        20                         373     17.47379
      glu_48.0D_conc      label
    1      0.5876508 48.0Ddf.20
      glu_48.0E_dilution_factor glu_48.0E_mean_luminescence glu_48.0E_se
    1                        20                    502.6667     219.2094
      glu_48.0E_conc      label
    1      0.7048093 48.0Edf.20
      glu_48.0F_dilution_factor glu_48.0F_mean_luminescence glu_48.0F_se
    1                        20                    306.6667     20.85133
      glu_48.0F_conc      label
    1      0.5277162 48.0Fdf.20
      glu_48.0G_dilution_factor glu_48.0G_mean_luminescence glu_48.0G_se
    1                        20                    266.3333     8.373238
      glu_48.0G_conc      label
    1      0.4912735 48.0Gdf.20
      glu_48.0H_dilution_factor glu_48.0H_mean_luminescence glu_48.0H_se
    1                        20                         409     103.8862
      glu_48.0H_conc      label
    1      0.6201781 48.0Hdf.20
      glu_48.0I_dilution_factor glu_48.0I_mean_luminescence glu_48.0I_se
    1                        20                         314     10.01665
      glu_48.0I_conc      label
    1      0.5343421 48.0Idf.20
      glu_48.0J_dilution_factor glu_48.0J_mean_luminescence glu_48.0J_se
    1                        20                    308.6667     20.30052
      glu_48.0J_conc      label
    1      0.5295232 48.0Jdf.20
      glu_48.2A_dilution_factor glu_48.2A_mean_luminescence glu_48.2A_se
    1                        20                    412.6667     9.351173
      glu_48.2A_conc      label
    1       0.623491 48.2Adf.20
      glu_48.2B_dilution_factor glu_48.2B_mean_luminescence glu_48.2B_se
    1                        20                    301.3333      15.2133
      glu_48.2B_conc      label
    1      0.5228973 48.2Bdf.20
      glu_48.2C_dilution_factor glu_48.2C_mean_luminescence glu_48.2C_se
    1                        20                    323.3333     3.282953
      glu_48.2C_conc      label
    1      0.5427751 48.2Cdf.20
      glu_48.2D_dilution_factor glu_48.2D_mean_luminescence glu_48.2D_se
    1                        20                         286     11.53256
      glu_48.2D_conc      label
    1      0.5090431 48.2Ddf.20
      glu_48.2E_dilution_factor glu_48.2E_mean_luminescence glu_48.2E_se
    1                        20                    334.6667     14.49521
      glu_48.2E_conc      label
    1      0.5530152 48.2Edf.20
      glu_48.2F_dilution_factor glu_48.2F_mean_luminescence glu_48.2F_se
    1                        20                    432.6667     15.83596
      glu_48.2F_conc      label
    1      0.6415618 48.2Fdf.20
      glu_48.2G_dilution_factor glu_48.2G_mean_luminescence glu_48.2G_se
    1                        20                    430.6667      33.0471
      glu_48.2G_conc      label
    1      0.6397547 48.2Gdf.20
      glu_48.2H_dilution_factor glu_48.2H_mean_luminescence glu_48.2H_se
    1                        20                    368.3333     1.855921
      glu_48.2H_conc      label
    1      0.5834343 48.2Hdf.20
      glu_48.2I_dilution_factor glu_48.2I_mean_luminescence glu_48.2I_se
    1                        20                         335     19.39931
      glu_48.2I_conc      label
    1      0.5533164 48.2Idf.20
      glu_48.2J_dilution_factor glu_48.2J_mean_luminescence glu_48.2J_se
    1                        20                         363     11.50362
      glu_48.2J_conc      label
    1      0.5786154 48.2Jdf.20
      glu_48.7A_dilution_factor glu_48.7A_mean_luminescence glu_48.7A_se
    1                        20                    367.6667     12.46774
      glu_48.7A_conc      label
    1      0.5828319 48.7Adf.20
      glu_48.7B_dilution_factor glu_48.7B_mean_luminescence glu_48.7B_se
    1                        20                    378.6667     5.897269
      glu_48.7B_conc      label
    1      0.5927708 48.7Bdf.20
      glu_48.7C_dilution_factor glu_48.7C_mean_luminescence glu_48.7C_se
    1                        20                    361.6667     18.94143
      glu_48.7C_conc      label
    1      0.5774107 48.7Cdf.20
      glu_48.7D_dilution_factor glu_48.7D_mean_luminescence glu_48.7D_se
    1                        20                         394     8.962886
      glu_48.7D_conc      label
    1       0.606625 48.7Ddf.20
      glu_48.7E_dilution_factor glu_48.7E_mean_luminescence glu_48.7E_se
    1                        20                         403           21
      glu_48.7E_conc      label
    1      0.6147569 48.7Edf.20
      glu_48.7F_dilution_factor glu_48.7F_mean_luminescence glu_48.7F_se
    1                        20                    329.6667     17.24658
      glu_48.7F_conc      label
    1      0.5484975 48.7Fdf.20
      glu_48.7G_dilution_factor glu_48.7G_mean_luminescence glu_48.7G_se
    1                        20                         458      12.2202
      glu_48.7G_conc      label
    1      0.6644514 48.7Gdf.20
      glu_48.7H_dilution_factor glu_48.7H_mean_luminescence glu_48.7H_se
    1                        20                         404     11.37248
      glu_48.7H_conc      label
    1      0.6156604 48.7Hdf.20
      glu_48.7I_dilution_factor glu_48.7I_mean_luminescence glu_48.7I_se
    1                        20                         409     13.45362
      glu_48.7I_conc      label
    1      0.6201781 48.7Idf.20
      glu_48.7J_dilution_factor glu_48.7J_mean_luminescence glu_48.7J_se
    1                        20                    333.6667     14.16961
      glu_48.7J_conc      label
    1      0.5521117 48.7Jdf.20
    glucose Standard Curve Summary:
    ==================================================
    Concentration: 100 µg/µL
      Mean Luminescence: 110525.00
      Standard Error: 231.05
      CV%: 0.36%

    Concentration: 10 µg/µL
      Mean Luminescence: 9507.33
      Standard Error: 100.99
      CV%: 1.84%

    Concentration: 1 µg/µL
      Mean Luminescence: 993.67
      Standard Error: 11.05
      CV%: 1.93%

    Concentration: 0.1 µg/µL
      Mean Luminescence: 311.67
      Standard Error: 12.71
      CV%: 7.06%

    Concentration: 0 µg/µL
      Mean Luminescence: 236.67
      Standard Error: 7.86
      CV%: 5.75%

### 1.2.5 Sample glucose table

``` r
#plate1
glu_0.0A_mean_conc_normalized <- glu_0.0A_mean_conc/as.numeric(sample_weights[2,2])
glu_0.0B_mean_conc_normalized <- glu_0.0B_mean_conc/as.numeric(sample_weights[3,2])
glu_0.0C_mean_conc_normalized <- glu_0.0C_mean_conc/as.numeric(sample_weights[4,2])
glu_0.0D_mean_conc_normalized <- glu_0.0D_mean_conc/as.numeric(sample_weights[5,2])
glu_0.0E_mean_conc_normalized <- glu_0.0E_mean_conc/as.numeric(sample_weights[6,2])
glu_0.0F_mean_conc_normalized <- glu_0.0F_mean_conc/as.numeric(sample_weights[7,2])
glu_0.0G_mean_conc_normalized <- glu_0.0G_mean_conc/as.numeric(sample_weights[8,2])
glu_0.0H_mean_conc_normalized <- glu_0.0H_mean_conc/as.numeric(sample_weights[9,2])
glu_0.0I_mean_conc_normalized <- glu_0.0I_mean_conc/as.numeric(sample_weights[10,2])
glu_0.0J_mean_conc_normalized <- glu_0.0J_mean_conc/as.numeric(sample_weights[11,2])
#plate2
glu_48.0A_mean_conc_normalized <- glu_48.0A_mean_conc/as.numeric(sample_weights[12,2])
glu_48.0B_mean_conc_normalized <- glu_48.0B_mean_conc/as.numeric(sample_weights[13,2])
glu_48.0C_mean_conc_normalized <- glu_48.0C_mean_conc/as.numeric(sample_weights[14,2])
glu_48.0D_mean_conc_normalized <- glu_48.0D_mean_conc/as.numeric(sample_weights[15,2])
glu_48.0E_mean_conc_normalized <- glu_48.0E_mean_conc/as.numeric(sample_weights[16,2])
glu_48.0F_mean_conc_normalized <- glu_48.0F_mean_conc/as.numeric(sample_weights[17,2])
glu_48.0G_mean_conc_normalized <- glu_48.0G_mean_conc/as.numeric(sample_weights[18,2])
glu_48.0H_mean_conc_normalized <- glu_48.0H_mean_conc/as.numeric(sample_weights[19,2])
glu_48.0I_mean_conc_normalized <- glu_48.0I_mean_conc/as.numeric(sample_weights[20,2])
glu_48.0J_mean_conc_normalized <- glu_48.0J_mean_conc/as.numeric(sample_weights[21,2])
glu_48.2A_mean_conc_normalized <- glu_48.2A_mean_conc/as.numeric(sample_weights[22,2])
glu_48.2B_mean_conc_normalized <- glu_48.2B_mean_conc/as.numeric(sample_weights[23,2])
glu_48.2C_mean_conc_normalized <- glu_48.2C_mean_conc/as.numeric(sample_weights[24,2])
glu_48.2D_mean_conc_normalized <- glu_48.2D_mean_conc/as.numeric(sample_weights[25,2])
glu_48.2E_mean_conc_normalized <- glu_48.2E_mean_conc/as.numeric(sample_weights[26,2])
glu_48.2F_mean_conc_normalized <- glu_48.2F_mean_conc/as.numeric(sample_weights[27,2])
#plate3
glu_48.2G_mean_conc_normalized <- glu_48.2G_mean_conc/as.numeric(sample_weights[28,2])
glu_48.2H_mean_conc_normalized <- glu_48.2H_mean_conc/as.numeric(sample_weights[29,2])
glu_48.2I_mean_conc_normalized <- glu_48.2I_mean_conc/as.numeric(sample_weights[30,2])
glu_48.2J_mean_conc_normalized <- glu_48.2J_mean_conc/as.numeric(sample_weights[31,2])
glu_48.7A_mean_conc_normalized <- glu_48.7A_mean_conc/as.numeric(sample_weights[32,2])
glu_48.7B_mean_conc_normalized <- glu_48.7B_mean_conc/as.numeric(sample_weights[33,2])
glu_48.7C_mean_conc_normalized <- glu_48.7C_mean_conc/as.numeric(sample_weights[34,2])
glu_48.7D_mean_conc_normalized <- glu_48.7D_mean_conc/as.numeric(sample_weights[35,2])
glu_48.7E_mean_conc_normalized <- glu_48.7E_mean_conc/as.numeric(sample_weights[36,2])
glu_48.7F_mean_conc_normalized <- glu_48.7F_mean_conc/as.numeric(sample_weights[37,2])
glu_48.7G_mean_conc_normalized <- glu_48.7G_mean_conc/as.numeric(sample_weights[38,2])
glu_48.7H_mean_conc_normalized <- glu_48.7H_mean_conc/as.numeric(sample_weights[39,2])
glu_48.7I_mean_conc_normalized <- glu_48.7I_mean_conc/as.numeric(sample_weights[40,2])
glu_48.7J_mean_conc_normalized <- glu_48.7J_mean_conc/as.numeric(sample_weights[41,2])
```

``` r
tab_glu <- matrix(c(glu_0.0A_dilution, glu_0.0A_mean_lum, glu_0.0A_mean_conc,(glu_0.0A_dilution*glu_0.0A_mean_conc), (glu_0.0A_dilution*glu_0.0A_mean_conc_normalized), 
                glu_0.0B_dilution, glu_0.0B_mean_lum, glu_0.0B_mean_conc,(glu_0.0B_dilution*glu_0.0B_mean_conc), (glu_0.0B_dilution*glu_0.0B_mean_conc_normalized),
                glu_0.0C_dilution, glu_0.0C_mean_lum, glu_0.0C_mean_conc,(glu_0.0C_dilution*glu_0.0C_mean_conc), (glu_0.0C_dilution*glu_0.0C_mean_conc_normalized), 
                glu_0.0D_dilution, glu_0.0D_mean_lum, glu_0.0D_mean_conc,(glu_0.0D_dilution*glu_0.0D_mean_conc), (glu_0.0D_dilution*glu_0.0D_mean_conc_normalized),
                glu_0.0E_dilution, glu_0.0E_mean_lum, glu_0.0E_mean_conc,(glu_0.0E_dilution*glu_0.0E_mean_conc), (glu_0.0E_dilution*glu_0.0E_mean_conc_normalized), 
                glu_0.0F_dilution, glu_0.0F_mean_lum, glu_0.0F_mean_conc,(glu_0.0F_dilution*glu_0.0F_mean_conc), (glu_0.0F_dilution*glu_0.0F_mean_conc_normalized),
                glu_0.0G_dilution, glu_0.0G_mean_lum, glu_0.0G_mean_conc,(glu_0.0G_dilution*glu_0.0G_mean_conc), (glu_0.0G_dilution*glu_0.0G_mean_conc_normalized), 
                glu_0.0H_dilution, glu_0.0H_mean_lum, glu_0.0H_mean_conc,(glu_0.0H_dilution*glu_0.0H_mean_conc), (glu_0.0H_dilution*glu_0.0H_mean_conc_normalized),
                glu_0.0I_dilution, glu_0.0I_mean_lum, glu_0.0I_mean_conc,(glu_0.0I_dilution*glu_0.0I_mean_conc), (glu_0.0I_dilution*glu_0.0I_mean_conc_normalized), 
                glu_0.0J_dilution, glu_0.0J_mean_lum, glu_0.0J_mean_conc,(glu_0.0J_dilution*glu_0.0J_mean_conc), (glu_0.0J_dilution*glu_0.0J_mean_conc_normalized),
                
                glu_48.0A_dilution, glu_48.0A_mean_lum, glu_48.0A_mean_conc,(glu_48.0A_dilution*glu_48.0A_mean_conc), (glu_48.0A_dilution*glu_48.0A_mean_conc_normalized), 
                glu_48.0B_dilution, glu_48.0B_mean_lum, glu_48.0B_mean_conc,(glu_48.0B_dilution*glu_48.0B_mean_conc), (glu_48.0B_dilution*glu_48.0B_mean_conc_normalized),
                glu_48.0C_dilution, glu_48.0C_mean_lum, glu_48.0C_mean_conc,(glu_48.0C_dilution*glu_48.0C_mean_conc), (glu_48.0C_dilution*glu_48.0C_mean_conc_normalized), 
                glu_48.0D_dilution, glu_48.0D_mean_lum, glu_48.0D_mean_conc,(glu_48.0D_dilution*glu_48.0D_mean_conc), (glu_48.0D_dilution*glu_48.0D_mean_conc_normalized),
                glu_48.0E_dilution, glu_48.0E_mean_lum, glu_48.0E_mean_conc,(glu_48.0E_dilution*glu_48.0E_mean_conc), (glu_48.0E_dilution*glu_48.0E_mean_conc_normalized), 
                glu_48.0F_dilution, glu_48.0F_mean_lum, glu_48.0F_mean_conc,(glu_48.0F_dilution*glu_48.0F_mean_conc), (glu_48.0F_dilution*glu_48.0F_mean_conc_normalized),
                glu_48.0G_dilution, glu_48.0G_mean_lum, glu_48.0G_mean_conc,(glu_48.0G_dilution*glu_48.0G_mean_conc), (glu_48.0G_dilution*glu_48.0G_mean_conc_normalized), 
                glu_48.0H_dilution, glu_48.0H_mean_lum, glu_48.0H_mean_conc,(glu_48.0H_dilution*glu_48.0H_mean_conc), (glu_48.0H_dilution*glu_48.0H_mean_conc_normalized),
                glu_48.0I_dilution, glu_48.0I_mean_lum, glu_48.0I_mean_conc,(glu_48.0I_dilution*glu_48.0I_mean_conc), (glu_48.0I_dilution*glu_48.0I_mean_conc_normalized), 
                glu_48.0J_dilution, glu_48.0J_mean_lum, glu_48.0J_mean_conc,(glu_48.0J_dilution*glu_48.0J_mean_conc), (glu_48.0J_dilution*glu_48.0J_mean_conc_normalized),
                
                glu_48.2A_dilution, glu_48.2A_mean_lum, glu_48.2A_mean_conc,(glu_48.2A_dilution*glu_48.2A_mean_conc), (glu_48.2A_dilution*glu_48.2A_mean_conc_normalized), 
                glu_48.2B_dilution, glu_48.2B_mean_lum, glu_48.2B_mean_conc,(glu_48.2B_dilution*glu_48.2B_mean_conc), (glu_48.2B_dilution*glu_48.2B_mean_conc_normalized),
                glu_48.2C_dilution, glu_48.2C_mean_lum, glu_48.2C_mean_conc,(glu_48.2C_dilution*glu_48.2C_mean_conc), (glu_48.2C_dilution*glu_48.2C_mean_conc_normalized), 
                glu_48.2D_dilution, glu_48.2D_mean_lum, glu_48.2D_mean_conc,(glu_48.2D_dilution*glu_48.2D_mean_conc), (glu_48.2D_dilution*glu_48.2D_mean_conc_normalized),
                glu_48.2E_dilution, glu_48.2E_mean_lum, glu_48.2E_mean_conc,(glu_48.2E_dilution*glu_48.2E_mean_conc), (glu_48.2E_dilution*glu_48.2E_mean_conc_normalized), 
                glu_48.2F_dilution, glu_48.2F_mean_lum, glu_48.2F_mean_conc,(glu_48.2F_dilution*glu_48.2F_mean_conc), (glu_48.2F_dilution*glu_48.2F_mean_conc_normalized),
                glu_48.2G_dilution, glu_48.2G_mean_lum, glu_48.2G_mean_conc,(glu_48.2G_dilution*glu_48.2G_mean_conc), (glu_48.2G_dilution*glu_48.2G_mean_conc_normalized), 
                glu_48.2H_dilution, glu_48.2H_mean_lum, glu_48.2H_mean_conc,(glu_48.2H_dilution*glu_48.2H_mean_conc), (glu_48.2H_dilution*glu_48.2H_mean_conc_normalized),
                glu_48.2I_dilution, glu_48.2I_mean_lum, glu_48.2I_mean_conc,(glu_48.2I_dilution*glu_48.2I_mean_conc), (glu_48.2I_dilution*glu_48.2I_mean_conc_normalized), 
                glu_48.2J_dilution, glu_48.2J_mean_lum, glu_48.2J_mean_conc,(glu_48.2J_dilution*glu_48.2J_mean_conc), (glu_48.2J_dilution*glu_48.2J_mean_conc_normalized),
                
                glu_48.7A_dilution, glu_48.7A_mean_lum, glu_48.7A_mean_conc,(glu_48.7A_dilution*glu_48.7A_mean_conc), (glu_48.7A_dilution*glu_48.7A_mean_conc_normalized), 
                glu_48.7B_dilution, glu_48.7B_mean_lum, glu_48.7B_mean_conc,(glu_48.7B_dilution*glu_48.7B_mean_conc), (glu_48.7B_dilution*glu_48.7B_mean_conc_normalized),
                glu_48.7C_dilution, glu_48.7C_mean_lum, glu_48.7C_mean_conc,(glu_48.7C_dilution*glu_48.7C_mean_conc), (glu_48.7C_dilution*glu_48.7C_mean_conc_normalized), 
                glu_48.7D_dilution, glu_48.7D_mean_lum, glu_48.7D_mean_conc,(glu_48.7D_dilution*glu_48.7D_mean_conc), (glu_48.7D_dilution*glu_48.7D_mean_conc_normalized),
                glu_48.7E_dilution, glu_48.7E_mean_lum, glu_48.7E_mean_conc,(glu_48.7E_dilution*glu_48.7E_mean_conc), (glu_48.7E_dilution*glu_48.7E_mean_conc_normalized), 
                glu_48.7F_dilution, glu_48.7F_mean_lum, glu_48.7F_mean_conc,(glu_48.7F_dilution*glu_48.7F_mean_conc), (glu_48.7F_dilution*glu_48.7F_mean_conc_normalized),
                glu_48.7G_dilution, glu_48.7G_mean_lum, glu_48.7G_mean_conc,(glu_48.7G_dilution*glu_48.7G_mean_conc), (glu_48.7G_dilution*glu_48.7G_mean_conc_normalized), 
                glu_48.7H_dilution, glu_48.7H_mean_lum, glu_48.7H_mean_conc,(glu_48.7H_dilution*glu_48.7H_mean_conc), (glu_48.7H_dilution*glu_48.7H_mean_conc_normalized),
                glu_48.7I_dilution, glu_48.7I_mean_lum, glu_48.7I_mean_conc,(glu_48.7I_dilution*glu_48.7I_mean_conc), (glu_48.7I_dilution*glu_48.7I_mean_conc_normalized), 
                glu_48.7J_dilution, glu_48.7J_mean_lum, glu_48.7J_mean_conc,(glu_48.7J_dilution*glu_48.7J_mean_conc), (glu_48.7J_dilution*glu_48.7J_mean_conc_normalized)), ncol=5, byrow=TRUE)
              
colnames(tab_glu) <- c('Dilution factor','Luminescence','Calculated glucose (ug/uL)','Total glucose (ug/uL)','Normalized glucose (ug/uL/mg)')
rownames(tab_glu) <- c('0.0A','0.0B','0.0C','0.0D','0.0E','0.0F','0.0G','0.0H','0.0I','0.0J','48.0A','48.0B','48.0C','48.0D','48.0E','48.0F','48.0G','48.0H','48.0I','48.0J','48.2A','48.2B','48.2C','48.2D','48.2E','48.2F','48.2G','48.2H','48.2I','48.2J','48.7A','48.7B','48.7C','48.7D','48.7E','48.7F','48.7G','48.7H','48.7I','48.7J')
tab_glu <- as.table(tab_glu)
tab_glu
```

          Dilution factor Luminescence Calculated glucose (ug/uL)
    0.0A       20.0000000  504.3333333                  0.7063152
    0.0B       20.0000000  253.3333333                  0.4795276
    0.0C       20.0000000  581.0000000                  0.7755863
    0.0D       20.0000000  361.3333333                  0.5771095
    0.0E       20.0000000  334.6666667                  0.5530152
    0.0F       20.0000000  339.0000000                  0.5569305
    0.0G       20.0000000  263.3333333                  0.4885629
    0.0H       20.0000000  252.0000000                  0.4783228
    0.0I       20.0000000  249.6666667                  0.4762146
    0.0J       20.0000000  313.6666667                  0.5340409
    48.0A      20.0000000  332.0000000                  0.5506058
    48.0B      20.0000000  927.0000000                  1.0882100
    48.0C      20.0000000  315.3333333                  0.5355468
    48.0D      20.0000000  373.0000000                  0.5876508
    48.0E      20.0000000  502.6666667                  0.7048093
    48.0F      20.0000000  306.6666667                  0.5277162
    48.0G      20.0000000  266.3333333                  0.4912735
    48.0H      20.0000000  409.0000000                  0.6201781
    48.0I      20.0000000  314.0000000                  0.5343421
    48.0J      20.0000000  308.6666667                  0.5295232
    48.2A      20.0000000  412.6666667                  0.6234910
    48.2B      20.0000000  301.3333333                  0.5228973
    48.2C      20.0000000  323.3333333                  0.5427751
    48.2D      20.0000000  286.0000000                  0.5090431
    48.2E      20.0000000  334.6666667                  0.5530152
    48.2F      20.0000000  432.6666667                  0.6415618
    48.2G      20.0000000  430.6666667                  0.6397547
    48.2H      20.0000000  368.3333333                  0.5834343
    48.2I      20.0000000  335.0000000                  0.5533164
    48.2J      20.0000000  363.0000000                  0.5786154
    48.7A      20.0000000  367.6666667                  0.5828319
    48.7B      20.0000000  378.6666667                  0.5927708
    48.7C      20.0000000  361.6666667                  0.5774107
    48.7D      20.0000000  394.0000000                  0.6066250
    48.7E      20.0000000  403.0000000                  0.6147569
    48.7F      20.0000000  329.6666667                  0.5484975
    48.7G      20.0000000  458.0000000                  0.6644514
    48.7H      20.0000000  404.0000000                  0.6156604
    48.7I      20.0000000  409.0000000                  0.6201781
    48.7J      20.0000000  333.6666667                  0.5521117
          Total glucose (ug/uL) Normalized glucose (ug/uL/mg)
    0.0A             14.1263043                     0.8072174
    0.0B              9.5905513                     0.5328084
    0.0C             15.5117268                     0.9124545
    0.0D             11.5421900                     0.5949582
    0.0E             11.0603039                     0.6544559
    0.0F             11.1386104                     0.4821909
    0.0G              9.7712586                     0.4211749
    0.0H              9.5664570                     0.7302639
    0.0I              9.5242919                     0.6707248
    0.0J             10.6808186                     1.1362573
    48.0A            11.0121153                     0.9253878
    48.0B            21.7641991                     1.1701182
    48.0C            10.7109365                     0.4958767
    48.0D            11.7530152                     1.3991685
    48.0E            14.0961864                     0.8543143
    48.0F            10.5543235                     0.6596452
    48.0G             9.8254708                     0.7500359
    48.0H            12.4035614                     1.0250877
    48.0I            10.6868422                     0.5040963
    48.0J            10.5904650                     0.7303769
    48.2A            12.4698208                     1.0221165
    48.2B            10.4579463                     0.9959949
    48.2C            10.8555023                     0.8415118
    48.2D            10.1808618                     0.3946070
    48.2E            11.0603039                     0.5557942
    48.2F            12.8312354                     0.9100167
    48.2G            12.7950939                     0.7801887
    48.2H            11.6686851                     0.5805316
    48.2I            11.0663275                     1.1178109
    48.2J            11.5723079                     1.2716822
    48.7A            11.6566380                     0.7618718
    48.7B            11.8554160                     0.8781790
    48.7C            11.5482136                     0.6714078
    48.7D            12.1325005                     1.6176667
    48.7E            12.2951371                     1.0508664
    48.7F            10.9699503                     1.6621137
    48.7G            13.2890272                     0.5932601
    48.7H            12.3132078                     0.7117461
    48.7I            12.4035614                     1.3337163
    48.7J            11.0422332                     0.7312737

# 2 Overall summary table

Note: the ‘glycogen’ readout includes background glucose - the
‘Glycogen-Glucose’ number is a more accurate measurement of glycogen
levels, as this removes the contribution of background glucose

``` r
tab_summary <- matrix(c(as.numeric(sample_weights[2,2]), glyc_0.0A_dilution, glyc_0.0A_mean_conc, glu_0.0A_mean_conc, (glyc_0.0A_mean_conc-glu_0.0A_mean_conc), ((glyc_0.0A_mean_conc-glu_0.0A_mean_conc)*(glyc_0.0A_dilution/as.numeric(sample_weights[2,2]))), 
                as.numeric(sample_weights[3,2]), glyc_0.0B_dilution, glyc_0.0B_mean_conc, glu_0.0B_mean_conc, (glyc_0.0B_mean_conc-glu_0.0B_mean_conc), ((glyc_0.0B_mean_conc-glu_0.0B_mean_conc)*(glyc_0.0B_dilution/as.numeric(sample_weights[3,2]))),
                as.numeric(sample_weights[4,2]), glyc_0.0C_dilution, glyc_0.0C_mean_conc, glu_0.0C_mean_conc, (glyc_0.0C_mean_conc-glu_0.0C_mean_conc), ((glyc_0.0C_mean_conc-glu_0.0C_mean_conc)*(glyc_0.0C_dilution/as.numeric(sample_weights[4,2]))), 
                as.numeric(sample_weights[5,2]), glyc_0.0D_dilution, glyc_0.0D_mean_conc, glu_0.0D_mean_conc, (glyc_0.0D_mean_conc-glu_0.0D_mean_conc), ((glyc_0.0D_mean_conc-glu_0.0D_mean_conc)*(glyc_0.0D_dilution/as.numeric(sample_weights[5,2]))),
                as.numeric(sample_weights[6,2]), glyc_0.0E_dilution, glyc_0.0E_mean_conc, glu_0.0E_mean_conc, (glyc_0.0E_mean_conc-glu_0.0E_mean_conc), ((glyc_0.0E_mean_conc-glu_0.0E_mean_conc)*(glyc_0.0E_dilution/as.numeric(sample_weights[6,2]))),
                as.numeric(sample_weights[7,2]), glyc_0.0F_dilution, glyc_0.0F_mean_conc, glu_0.0F_mean_conc, (glyc_0.0F_mean_conc-glu_0.0F_mean_conc), ((glyc_0.0F_mean_conc-glu_0.0F_mean_conc)*(glyc_0.0F_dilution/as.numeric(sample_weights[7,2]))), 
                as.numeric(sample_weights[8,2]), glyc_0.0G_dilution, glyc_0.0G_mean_conc, glu_0.0G_mean_conc, (glyc_0.0G_mean_conc-glu_0.0G_mean_conc), ((glyc_0.0G_mean_conc-glu_0.0G_mean_conc)*(glyc_0.0G_dilution/as.numeric(sample_weights[8,2]))),
                as.numeric(sample_weights[9,2]), glyc_0.0H_dilution, glyc_0.0H_mean_conc, glu_0.0H_mean_conc, (glyc_0.0H_mean_conc-glu_0.0H_mean_conc), ((glyc_0.0H_mean_conc-glu_0.0H_mean_conc)*(glyc_0.0H_dilution/as.numeric(sample_weights[9,2]))), 
                as.numeric(sample_weights[10,2]), glyc_0.0I_dilution, glyc_0.0I_mean_conc, glu_0.0I_mean_conc, (glyc_0.0I_mean_conc-glu_0.0I_mean_conc), ((glyc_0.0I_mean_conc-glu_0.0I_mean_conc)*(glyc_0.0I_dilution/as.numeric(sample_weights[10,2]))),
                as.numeric(sample_weights[11,2]), glyc_0.0J_dilution, glyc_0.0J_mean_conc, glu_0.0J_mean_conc, (glyc_0.0J_mean_conc-glu_0.0J_mean_conc), ((glyc_0.0J_mean_conc-glu_0.0J_mean_conc)*(glyc_0.0J_dilution/as.numeric(sample_weights[11,2]))),
                as.numeric(sample_weights[12,2]), glyc_48.0A_dilution, glyc_48.0A_mean_conc, glu_48.0A_mean_conc, (glyc_48.0A_mean_conc-glu_48.0A_mean_conc), ((glyc_48.0A_mean_conc-glu_48.0A_mean_conc)*(glyc_48.0A_dilution/as.numeric(sample_weights[12,2]))), 
                as.numeric(sample_weights[13,2]), glyc_48.0B_dilution, glyc_48.0B_mean_conc, glu_48.0B_mean_conc, (glyc_48.0B_mean_conc-glu_48.0B_mean_conc), ((glyc_48.0B_mean_conc-glu_48.0B_mean_conc)*(glyc_48.0B_dilution/as.numeric(sample_weights[13,2]))),
                as.numeric(sample_weights[14,2]), glyc_48.0C_dilution, glyc_48.0C_mean_conc, glu_48.0C_mean_conc, (glyc_48.0C_mean_conc-glu_48.0C_mean_conc), ((glyc_48.0C_mean_conc-glu_48.0C_mean_conc)*(glyc_48.0C_dilution/as.numeric(sample_weights[14,2]))), 
                as.numeric(sample_weights[15,2]), glyc_48.0D_dilution, glyc_48.0D_mean_conc, glu_48.0D_mean_conc, (glyc_48.0D_mean_conc-glu_48.0D_mean_conc), ((glyc_48.0D_mean_conc-glu_48.0D_mean_conc)*(glyc_48.0D_dilution/as.numeric(sample_weights[15,2]))),
                as.numeric(sample_weights[16,2]), glyc_48.0E_dilution, glyc_48.0E_mean_conc, glu_48.0E_mean_conc, (glyc_48.0E_mean_conc-glu_48.0E_mean_conc), ((glyc_48.0E_mean_conc-glu_48.0E_mean_conc)*(glyc_48.0E_dilution/as.numeric(sample_weights[16,2]))),
                as.numeric(sample_weights[17,2]), glyc_48.0F_dilution, glyc_48.0F_mean_conc, glu_48.0F_mean_conc, (glyc_48.0F_mean_conc-glu_48.0F_mean_conc), ((glyc_48.0F_mean_conc-glu_48.0F_mean_conc)*(glyc_48.0F_dilution/as.numeric(sample_weights[17,2]))), 
                as.numeric(sample_weights[18,2]), glyc_48.0G_dilution, glyc_48.0G_mean_conc, glu_48.0G_mean_conc, (glyc_48.0G_mean_conc-glu_48.0G_mean_conc), ((glyc_48.0G_mean_conc-glu_48.0G_mean_conc)*(glyc_48.0G_dilution/as.numeric(sample_weights[18,2]))),
                as.numeric(sample_weights[19,2]), glyc_48.0H_dilution, glyc_48.0H_mean_conc, glu_48.0H_mean_conc, (glyc_48.0H_mean_conc-glu_48.0H_mean_conc), ((glyc_48.0H_mean_conc-glu_48.0H_mean_conc)*(glyc_48.0H_dilution/as.numeric(sample_weights[19,2]))), 
                as.numeric(sample_weights[20,2]), glyc_48.0I_dilution, glyc_48.0I_mean_conc, glu_48.0I_mean_conc, (glyc_48.0I_mean_conc-glu_48.0I_mean_conc), ((glyc_48.0I_mean_conc-glu_48.0I_mean_conc)*(glyc_48.0I_dilution/as.numeric(sample_weights[20,2]))),
                as.numeric(sample_weights[21,2]), glyc_48.0J_dilution, glyc_48.0J_mean_conc, glu_48.0J_mean_conc, (glyc_48.0J_mean_conc-glu_48.0J_mean_conc), ((glyc_48.0J_mean_conc-glu_48.0J_mean_conc)*(glyc_48.0J_dilution/as.numeric(sample_weights[21,2]))),
                as.numeric(sample_weights[22,2]), glyc_48.2A_dilution, glyc_48.2A_mean_conc, glu_48.2A_mean_conc, (glyc_48.2A_mean_conc-glu_48.2A_mean_conc), ((glyc_48.2A_mean_conc-glu_48.2A_mean_conc)*(glyc_48.2A_dilution/as.numeric(sample_weights[22,2]))), 
                as.numeric(sample_weights[23,2]), glyc_48.2B_dilution, glyc_48.2B_mean_conc, glu_48.2B_mean_conc, (glyc_48.2B_mean_conc-glu_48.2B_mean_conc), ((glyc_48.2B_mean_conc-glu_48.2B_mean_conc)*(glyc_48.2B_dilution/as.numeric(sample_weights[23,2]))),
                as.numeric(sample_weights[24,2]), glyc_48.2C_dilution, glyc_48.2C_mean_conc, glu_48.2C_mean_conc, (glyc_48.2C_mean_conc-glu_48.2C_mean_conc), ((glyc_48.2C_mean_conc-glu_48.2C_mean_conc)*(glyc_48.2C_dilution/as.numeric(sample_weights[24,2]))), 
                as.numeric(sample_weights[25,2]), glyc_48.2D_dilution, glyc_48.2D_mean_conc, glu_48.2D_mean_conc, (glyc_48.2D_mean_conc-glu_48.2D_mean_conc), ((glyc_48.2D_mean_conc-glu_48.2D_mean_conc)*(glyc_48.2D_dilution/as.numeric(sample_weights[25,2]))),
                as.numeric(sample_weights[26,2]), glyc_48.2E_dilution, glyc_48.2E_mean_conc, glu_48.2E_mean_conc, (glyc_48.2E_mean_conc-glu_48.2E_mean_conc), ((glyc_48.2E_mean_conc-glu_48.2E_mean_conc)*(glyc_48.2E_dilution/as.numeric(sample_weights[26,2]))),
                as.numeric(sample_weights[27,2]), glyc_48.2F_dilution, glyc_48.2F_mean_conc, glu_48.2F_mean_conc, (glyc_48.2F_mean_conc-glu_48.2F_mean_conc), ((glyc_48.2F_mean_conc-glu_48.2F_mean_conc)*(glyc_48.2F_dilution/as.numeric(sample_weights[27,2]))), 
                as.numeric(sample_weights[28,2]), glyc_48.2G_dilution, glyc_48.2G_mean_conc, glu_48.2G_mean_conc, (glyc_48.2G_mean_conc-glu_48.2G_mean_conc), ((glyc_48.2G_mean_conc-glu_48.2G_mean_conc)*(glyc_48.2G_dilution/as.numeric(sample_weights[28,2]))),
                as.numeric(sample_weights[29,2]), glyc_48.2H_dilution, glyc_48.2H_mean_conc, glu_48.2H_mean_conc, (glyc_48.2H_mean_conc-glu_48.2H_mean_conc), ((glyc_48.2H_mean_conc-glu_48.2H_mean_conc)*(glyc_48.2H_dilution/as.numeric(sample_weights[29,2]))), 
                as.numeric(sample_weights[30,2]), glyc_48.2I_dilution, glyc_48.2I_mean_conc, glu_48.2I_mean_conc, (glyc_48.2I_mean_conc-glu_48.2I_mean_conc), ((glyc_48.2I_mean_conc-glu_48.2I_mean_conc)*(glyc_48.2I_dilution/as.numeric(sample_weights[30,2]))),
                as.numeric(sample_weights[31,2]), glyc_48.2J_dilution, glyc_48.2J_mean_conc, glu_48.2J_mean_conc, (glyc_48.2J_mean_conc-glu_48.2J_mean_conc), ((glyc_48.2J_mean_conc-glu_48.2J_mean_conc)*(glyc_48.2J_dilution/as.numeric(sample_weights[31,2]))),
                as.numeric(sample_weights[32,2]), glyc_48.7A_dilution, glyc_48.7A_mean_conc, glu_48.7A_mean_conc, (glyc_48.7A_mean_conc-glu_48.7A_mean_conc), ((glyc_48.7A_mean_conc-glu_48.7A_mean_conc)*(glyc_48.7A_dilution/as.numeric(sample_weights[32,2]))), 
                as.numeric(sample_weights[33,2]), glyc_48.7B_dilution, glyc_48.7B_mean_conc, glu_48.7B_mean_conc, (glyc_48.7B_mean_conc-glu_48.7B_mean_conc), ((glyc_48.7B_mean_conc-glu_48.7B_mean_conc)*(glyc_48.7B_dilution/as.numeric(sample_weights[33,2]))),
                as.numeric(sample_weights[34,2]), glyc_48.7C_dilution, glyc_48.7C_mean_conc, glu_48.7C_mean_conc, (glyc_48.7C_mean_conc-glu_48.7C_mean_conc), ((glyc_48.7C_mean_conc-glu_48.7C_mean_conc)*(glyc_48.7C_dilution/as.numeric(sample_weights[34,2]))), 
                as.numeric(sample_weights[35,2]), glyc_48.7D_dilution, glyc_48.7D_mean_conc, glu_48.7D_mean_conc, (glyc_48.7D_mean_conc-glu_48.7D_mean_conc), ((glyc_48.7D_mean_conc-glu_48.7D_mean_conc)*(glyc_48.7D_dilution/as.numeric(sample_weights[35,2]))),
                as.numeric(sample_weights[36,2]), glyc_48.7E_dilution, glyc_48.7E_mean_conc, glu_48.7E_mean_conc, (glyc_48.7E_mean_conc-glu_48.7E_mean_conc), ((glyc_48.7E_mean_conc-glu_48.7E_mean_conc)*(glyc_48.7E_dilution/as.numeric(sample_weights[36,2]))),
                as.numeric(sample_weights[37,2]), glyc_48.7F_dilution, glyc_48.7F_mean_conc, glu_48.7F_mean_conc, (glyc_48.7F_mean_conc-glu_48.7F_mean_conc), ((glyc_48.7F_mean_conc-glu_48.7F_mean_conc)*(glyc_48.7F_dilution/as.numeric(sample_weights[37,2]))), 
                as.numeric(sample_weights[38,2]), glyc_48.7G_dilution, glyc_48.7G_mean_conc, glu_48.7G_mean_conc, (glyc_48.7G_mean_conc-glu_48.7G_mean_conc), ((glyc_48.7G_mean_conc-glu_48.7G_mean_conc)*(glyc_48.7G_dilution/as.numeric(sample_weights[38,2]))),
                as.numeric(sample_weights[39,2]), glyc_48.7H_dilution, glyc_48.7H_mean_conc, glu_48.7H_mean_conc, (glyc_48.7H_mean_conc-glu_48.7H_mean_conc), ((glyc_48.7H_mean_conc-glu_48.7H_mean_conc)*(glyc_48.7H_dilution/as.numeric(sample_weights[39,2]))), 
                as.numeric(sample_weights[40,2]), glyc_48.7I_dilution, glyc_48.7I_mean_conc, glu_48.7I_mean_conc, (glyc_48.7I_mean_conc-glu_48.7I_mean_conc), ((glyc_48.7I_mean_conc-glu_48.7I_mean_conc)*(glyc_48.7I_dilution/as.numeric(sample_weights[40,2]))),
                as.numeric(sample_weights[41,2]), glyc_48.7J_dilution, glyc_48.7J_mean_conc, glu_48.7J_mean_conc, (glyc_48.7J_mean_conc-glu_48.7J_mean_conc), ((glyc_48.7J_mean_conc-glu_48.7J_mean_conc)*(glyc_48.7J_dilution/as.numeric(sample_weights[41,2])))
                ), ncol=6, byrow=TRUE)

colnames(tab_summary) <- c('Weight (mg)', 'df', 'Glycogen (ug/uL)', 'Glucose (uM)', 'Glycogen-Glucose (ug/uL)', '(Glycogen-Glucose)*df/weight (ug/uL/mg)')
rownames(tab_summary) <- c('0.0A','0.0B','0.0C','0.0D','0.0E','0.0F','0.0G','0.0H','0.0I','0.0J','48.0A','48.0B','48.0C','48.0D','48.0E','48.0F','48.0G','48.0H','48.0I', '48.0J','48.2A','48.2B','48.2C','48.2D','48.2E','48.2F','48.2G','48.2H','48.2I','48.2J','48.7A','48.7B','48.7C','48.7D','48.7E','48.7F','48.7G','48.7H','48.7I','48.7J')
tab_summary <- as.table(tab_summary)
tab_summary
```

           Weight (mg)           df Glycogen (ug/uL) Glucose (uM)
    0.0A  17.500000000 20.000000000      2.480812969  0.706315213
    0.0B  18.000000000 20.000000000      2.602432578  0.479527564
    0.0C  17.000000000 20.000000000      4.744653476  0.775586341
    0.0D  19.400000000 20.000000000      2.249613157  0.577109501
    0.0E  16.900000000 20.000000000      4.952207034  0.553015195
    0.0F  23.100000000 20.000000000      4.406612965  0.556930520
    0.0G  23.200000000 20.000000000      2.901903741  0.488562929
    0.0H  13.100000000 20.000000000      0.532015550  0.478322849
    0.0I  14.200000000 20.000000000      2.328265791  0.476214597
    0.0J   9.400000000 20.000000000      0.646209657  0.534040930
    48.0A 11.900000000 20.000000000      0.317901598  0.550605765
    48.0B 18.600000000 20.000000000      0.204284229  1.088209953
    48.0C 21.600000000 20.000000000      0.550543255  0.535546824
    48.0D  8.400000000 20.000000000      0.460572140  0.587650759
    48.0E 16.500000000 20.000000000      0.570224437  0.704809319
    48.0F 16.000000000 20.000000000      0.309755175  0.527716175
    48.0G 13.100000000 20.000000000      0.363247617  0.491273538
    48.0H 12.100000000 20.000000000      0.677786058  0.620178072
    48.0I 21.200000000 20.000000000      0.716211222  0.534342109
    48.0J 14.500000000 20.000000000      0.262967312  0.529523248
    48.2A 12.200000000 20.000000000      0.848067928  0.623491039
    48.2B 10.500000000 20.000000000      0.300599461  0.522897314
    48.2C 12.900000000 20.000000000      0.397058877  0.542775116
    48.2D 25.800000000 20.000000000      1.840417605  0.509043088
    48.2E 19.900000000 20.000000000      1.971337112  0.553015195
    48.2F 14.100000000 20.000000000      2.503810394  0.641561768
    48.2G 16.400000000 20.000000000      2.867371558  0.639754695
    48.2H 20.100000000 20.000000000      0.719743742  0.583434256
    48.2I  9.900000000 20.000000000      0.562222198  0.553316374
    48.2J  9.100000000 20.000000000      0.790177860  0.578615395
    48.7A 15.300000000 20.000000000      1.609650347  0.582831898
    48.7B 13.500000000 20.000000000      1.103707009  0.592770799
    48.7C 17.200000000 20.000000000      2.353425982  0.577410680
    48.7D  7.500000000 20.000000000      1.099453567  0.606625025
    48.7E 11.700000000 20.000000000      2.182206913  0.614756853
    48.7F  6.600000000 20.000000000      0.376152128  0.548497513
    48.7G 22.400000000 20.000000000      2.443108728  0.664451358
    48.7H 17.300000000 20.000000000      3.490969430  0.615660389
    48.7I  9.300000000 20.000000000      1.156983174  0.620178072
    48.7J 15.100000000 20.000000000      2.886692278  0.552111659
          Glycogen-Glucose (ug/uL) (Glycogen-Glucose)*df/weight (ug/uL/mg)
    0.0A               1.774497756                             2.027997436
    0.0B               2.122905014                             2.358783348
    0.0C               3.969067135                             4.669490747
    0.0D               1.672503656                             1.724230574
    0.0E               4.399191838                             5.206144187
    0.0F               3.849682445                             3.333058394
    0.0G               2.413340812                             2.080466217
    0.0H               0.053692701                             0.081973589
    0.0I               1.852051193                             2.608522808
    0.0J               0.112168727                             0.238656867
    48.0A             -0.232704166                            -0.391099439
    48.0B             -0.883925724                            -0.950457768
    48.0C              0.014996431                             0.013885585
    48.0D             -0.127078619                            -0.302568140
    48.0E             -0.134584882                            -0.163133191
    48.0F             -0.217960999                            -0.272451249
    48.0G             -0.128025921                            -0.195459421
    48.0H              0.057607987                             0.095219813
    48.0I              0.181869113                             0.171574635
    48.0J             -0.266555936                            -0.367663360
    48.2A              0.224576889                             0.368158835
    48.2B             -0.222297853                            -0.423424482
    48.2C             -0.145716238                            -0.225916648
    48.2D              1.331374517                             1.032073269
    48.2E              1.418321917                             1.425449163
    48.2F              1.862248626                             2.641487413
    48.2G              2.227616863                             2.716605931
    48.2H              0.136309486                             0.135631329
    48.2I              0.008905824                             0.017991564
    48.2J              0.211562465                             0.464972450
    48.7A              1.026818448                             1.342246338
    48.7B              0.510936210                             0.756942534
    48.7C              1.776015303                             2.065134073
    48.7D              0.492828543                             1.314209447
    48.7E              1.567450060                             2.679401812
    48.7F             -0.172345385                            -0.522258743
    48.7G              1.778657370                             1.588086938
    48.7H              2.875309041                             3.324056694
    48.7I              0.536805103                             1.154419576
    48.7J              2.334580619                             3.092159760
