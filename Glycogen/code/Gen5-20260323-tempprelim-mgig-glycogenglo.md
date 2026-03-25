Gen5-20260323-tempprelim-mgig-glycogenglo
================
Hazel Abrahamson-Amerine
2026-03-24

- [1 STANDARD CURVES](#1-standard-curves)
  - [1.1 Glycogen Standard Curve](#11-glycogen-standard-curve)
    - [1.1.1 Extract luminescence data](#111-extract-luminescence-data)
- [2 STANDARD CURVES](#2-standard-curves)
  - [2.1 Glucose Standard Curve](#21-glucose-standard-curve)

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
#plate 1 - samples 0.0A, 6.0A-6.7C
plate_layout1 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-Gen5-20260323A-tempprelim-mgig-glycogenglo.csv", header = FALSE)
raw_luminescence1 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-Gen5-20260323A-tempprelim-mgig-glycogenglo.csv", header = FALSE)

cat("Plate layout1:\n")
str(plate_layout1)

cat("\n\n")

cat("Raw luminescence1:\n")
str(raw_luminescence1)

#plate 2 - samples 18.0A-24.0C, 0.0A, 0.0B
plate_layout2 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-Gen5-20260323B-tempprelim-mgig-glycogenglo.csv", header = FALSE)
raw_luminescence2 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-Gen5-20260323B-tempprelim-mgig-glycogenglo.csv", header = FALSE)

cat("Plate layout2:\n")
str(plate_layout2)

cat("\n\n")

cat("Raw luminescence2:\n")
str(raw_luminescence2)

#sample weights
sample_weights <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/20260320_mgig_wholetissue_weights_GlycogenGlo.csv", header = FALSE)

cat("Sample Weights:\n")
str(sample_weights)
```

    Plate layout1:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : chr  "0.0A-glyc-16.3-df.20" "6.0A-glyc-11.5-df.20" "6.0B-glyc-14.0-df.20" "6.0C-glyc-26.9-df.20" ...
     $ V2 : chr  "0.0A-glyc-16.3-df.20" "6.0A-glyc-11.5-df.20" "6.0B-glyc-14.0-df.20" "6.0C-glyc-26.9-df.20" ...
     $ V3 : chr  "0.0A-glyc-16.3-df.20" "6.0A-glyc-11.5-df.20" "6.0B-glyc-14.0-df.20" "6.0C-glyc-26.9-df.20" ...
     $ V4 : chr  "6.2B-glyc-20.7-df.20" "6.2C-glyc-17.0-df.20" "6.7A-glyc-9.2-df.20" "6.7B-glyc-41.1-df.20" ...
     $ V5 : chr  "6.2B-glyc-20.7-df.20" "6.2C-glyc-17.0-df.20" "6.7A-glyc-9.2-df.20" "6.7B-glyc-41.1-df.20" ...
     $ V6 : chr  "6.2B-glyc-20.7-df.20" "6.2C-glyc-17.0-df.20" "6.7A-glyc-9.2-df.20" "6.7B-glyc-41.1-df.20" ...
     $ V7 : chr  "0.0A-glu-16.3-df.20" "6.0A-glu-11.5-df.20" "6.0B-glu-14.0-df.20" "6.0C-glu-26.9-df.20" ...
     $ V8 : chr  "0.0A-glu-16.3-df.20" "6.0A-glu-11.5-df.20" "6.0B-glu-14.0-df.20" "6.0C-glu-26.9-df.20" ...
     $ V9 : chr  "0.0A-glu-16.3-df.20" "6.0A-glu-11.5-df.20" "6.0B-glu-14.0-df.20" "6.0C-glu-26.9-df.20" ...
     $ V10: chr  "6.2B-glu-20.7-df.20" "6.2C-glu-17.0-df.20" "6.7A-glu-9.2-df.20" "6.7B-glu-41.1-df.20" ...
     $ V11: chr  "6.2B-glu-20.7-df.20" "6.2C-glu-17.0-df.20" "6.7A-glu-9.2-df.20" "6.7B-glu-41.1-df.20" ...
     $ V12: chr  "6.2B-glu-20.7-df.20" "6.2C-glu-17.0-df.20" "6.7A-glu-9.2-df.20" "6.7B-glu-41.1-df.20" ...


    Raw luminescence1:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : int  6085 6311 7202 38795 17019 84299 92663 87071
     $ V2 : int  5647 6060 7765 38022 16795 7086 7740 6673
     $ V3 : int  5774 6126 7751 38256 17127 1269 1242 1250
     $ V4 : int  18833 10305 7794 31312 2700 842 1256 933
     $ V5 : int  18546 10409 7985 32681 2907 750 753 841
     $ V6 : int  20088 10259 8115 32536 2899 NA NA NA
     $ V7 : int  607 580 538 1299 933 92377 93908 98652
     $ V8 : int  597 560 588 1292 941 8964 9190 9100
     $ V9 : int  592 602 543 1176 944 1040 1064 1078
     $ V10: int  862 693 687 1362 437 326 341 342
     $ V11: int  767 701 646 1439 461 250 268 265
     $ V12: int  832 743 650 1265 480 NA NA NA
    Plate layout2:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : chr  "18.0A-glyc-19.3-df.20" "18.0B-glyc-12.0-df.20" "18.0C-glyc-21.0-df.20" "18.2A-glyc-21.9-df.20" ...
     $ V2 : chr  "18.0A-glyc-19.3-df.20" "18.0B-glyc-12.0-df.20" "18.0C-glyc-21.0-df.20" "18.2A-glyc-21.9-df.20" ...
     $ V3 : chr  "18.0A-glyc-19.3-df.20" "18.0B-glyc-12.0-df.20" "18.0C-glyc-21.0-df.20" "18.2A-glyc-21.9-df.20" ...
     $ V4 : chr  "18.7C-glyc-14.5-df.20" "24.0A-glyc-21.6-df.20" "24.0B-glyc-17.4-df.20" "24.0C-glyc-15.6-df.20" ...
     $ V5 : chr  "18.7C-glyc-14.5-df.20" "24.0A-glyc-21.6-df.20" "24.0B-glyc-17.4-df.20" "24.0C-glyc-15.6-df.20" ...
     $ V6 : chr  "18.7C-glyc-14.5-df.20" "24.0A-glyc-21.6-df.20" "24.0B-glyc-17.4-df.20" "24.0C-glyc-15.6-df.20" ...
     $ V7 : chr  "18.0A-glu-19.3-df.20" "18.0B-glu-12.0-df.20" "18.0C-glu-21.0-df.20" "18.2A-glu-21.9-df.20" ...
     $ V8 : chr  "18.0A-glu-19.3-df.20" "18.0B-glu-12.0-df.20" "18.0C-glu-21.0-df.20" "18.2A-glu-21.9-df.20" ...
     $ V9 : chr  "18.0A-glu-19.3-df.20" "18.0B-glu-12.0-df.20" "18.0C-glu-21.0-df.20" "18.2A-glu-21.9-df.20" ...
     $ V10: chr  "18.7C-glu-14.5-df.20" "24.0A-glu-21.6-df.20" "24.0B-glu-17.4-df.20" "24.0C-glu-15.6-df.20" ...
     $ V11: chr  "18.7C-glu-14.5-df.20" "24.0A-glu-21.6-df.20" "24.0B-glu-17.4-df.20" "24.0C-glu-15.6-df.20" ...
     $ V12: chr  "18.7C-glu-14.5-df.20" "24.0A-glu-21.6-df.20" "24.0B-glu-17.4-df.20" "24.0C-glu-15.6-df.20" ...


    Raw luminescence2:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : int  11721 14303 15525 15167 17078 11867 10862 12722
     $ V2 : int  9050 13241 15250 15072 17013 11501 10691 12591
     $ V3 : int  9384 13684 15517 15024 16920 11834 11117 13220
     $ V4 : int  21314 8377 13322 5750 3740 34908 NA NA
     $ V5 : int  21007 6962 13087 5756 3615 35260 NA NA
     $ V6 : int  21374 6962 12919 6104 3671 37121 NA NA
     $ V7 : int  812 732 930 1544 1264 755 696 1278
     $ V8 : int  754 868 806 991 1434 948 641 1279
     $ V9 : int  790 802 938 1045 4671 1019 605 1389
     $ V10: int  930 643 1418 515 476 19207 NA NA
     $ V11: int  1026 587 774 492 494 954 NA NA
     $ V12: int  958 672 941 559 600 998 NA NA
    Sample Weights:
    'data.frame':   40 obs. of  2 variables:
     $ V1: chr  "SampleID" "0.0A" "0.0B" "0.0C" ...
     $ V2: chr  "Weight (mg)" "16.3" "21.4" "21.9" ...

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

glyc_6.0A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[2, 1]))
glyc_6.0A_luminescence <- as.numeric(raw_luminescence1[2, glyc_sample_cols1])

glyc_6.0B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[3, 1]))
glyc_6.0B_luminescence <- as.numeric(raw_luminescence1[3, glyc_sample_cols1])

glyc_6.0C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[4, 1]))
glyc_6.0C_luminescence <- as.numeric(raw_luminescence1[4, glyc_sample_cols1])

glyc_6.2A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[5, 1]))
glyc_6.2A_luminescence <- as.numeric(raw_luminescence1[5, glyc_sample_cols1])


glyc_sample_cols2 <- c(4,5,6)
glyc_6.2B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[1, 4]))
glyc_6.2B_luminescence <- as.numeric(raw_luminescence1[1, glyc_sample_cols2])

glyc_6.2C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[2, 4]))
glyc_6.2C_luminescence <- as.numeric(raw_luminescence1[2, glyc_sample_cols2])

glyc_6.7A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[3, 4]))
glyc_6.7A_luminescence <- as.numeric(raw_luminescence1[3, glyc_sample_cols2])

glyc_6.7B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[4, 4]))
glyc_6.7B_luminescence <- as.numeric(raw_luminescence1[4, glyc_sample_cols2])

glyc_6.7C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[5, 4]))
glyc_6.7C_luminescence <- as.numeric(raw_luminescence1[5, glyc_sample_cols2])


glyc_sample_cols1 <- c(1,2,3)
glyc_18.0A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[1, 1]))
glyc_18.0A_luminescence <- as.numeric(raw_luminescence2[1, glyc_sample_cols1])

glyc_18.0B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[2, 1]))
glyc_18.0B_luminescence <- as.numeric(raw_luminescence2[2, glyc_sample_cols1])

glyc_18.0C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[3, 1]))
glyc_18.0C_luminescence <- as.numeric(raw_luminescence2[3, glyc_sample_cols1])

glyc_18.2A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[4, 1]))
glyc_18.2A_luminescence <- as.numeric(raw_luminescence2[4, glyc_sample_cols1])

glyc_18.2B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[5, 1]))
glyc_18.2B_luminescence <- as.numeric(raw_luminescence2[5, glyc_sample_cols1])

glyc_18.2C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[6, 1]))
glyc_18.2C_luminescence <- as.numeric(raw_luminescence2[6, glyc_sample_cols1])

glyc_18.7A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[7, 1]))
glyc_18.7A_luminescence <- as.numeric(raw_luminescence2[7, glyc_sample_cols1])

glyc_18.7B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[8, 1]))
glyc_18.7B_luminescence <- as.numeric(raw_luminescence2[8, glyc_sample_cols1])

glyc_sample_cols2 <- c(4,5,6)
glyc_18.7C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[1, 4]))
glyc_18.7C_luminescence <- as.numeric(raw_luminescence2[1, glyc_sample_cols2])

glyc_24.0A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[2, 4]))
glyc_24.0A_luminescence <- as.numeric(raw_luminescence2[2, glyc_sample_cols2])

glyc_24.0B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[3, 4]))
glyc_24.0B_luminescence <- as.numeric(raw_luminescence2[3, glyc_sample_cols2])

glyc_24.0C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[4, 4]))
glyc_24.0C_luminescence <- as.numeric(raw_luminescence2[4, glyc_sample_cols2])

glyc_0.0B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[5, 4]))
glyc_0.0B_luminescence <- as.numeric(raw_luminescence2[5, glyc_sample_cols2])

glyc_0.0C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[6, 4]))
glyc_0.0C_luminescence <- as.numeric(raw_luminescence2[6, glyc_sample_cols2])
```

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

    [1] 5835.333
    [1] 1.293879

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_6.0A_mean_lum <- numeric(length(glyc_6.0A_dilution))
glyc_6.0A_se_lum <- numeric(length(glyc_6.0A_dilution))
glyc_6.0A_mean_conc <- numeric(length(glyc_6.0A_dilution))

for (i in 1:length(glyc_6.0A_dilution)) {
  df_val <- glyc_6.0A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_6.0A_lum_values <- c(glyc_6.0A_luminescence[glyc_6.0A_dilution == df_val])
  glyc_6.0A_mean_lum[i] <- mean(glyc_6.0A_lum_values)
  glyc_6.0A_se_lum[i] <- sd(glyc_6.0A_lum_values) / sqrt(length(glyc_6.0A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_6.0A_mean_conc[i] <- (glyc_6.0A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_6.0A_data <- data.frame(
  glyc_6.0A_dilution_factor = glyc_6.0A_dilution,
  glyc_6.0A_mean_luminescence = glyc_6.0A_mean_lum,
  glyc_6.0A_se = glyc_6.0A_se_lum,
  glyc_6.0A_conc =  glyc_6.0A_mean_conc,
  label = paste0("6.0Adf.", glyc_6.0A_dilution)
)
glyc_6.0A_mean_lum
glyc_6.0A_mean_conc
```

    [1] 6165.667
    [1] 1.369234

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_6.0B_mean_lum <- numeric(length(glyc_6.0B_dilution))
glyc_6.0B_se_lum <- numeric(length(glyc_6.0B_dilution))
glyc_6.0B_mean_conc <- numeric(length(glyc_6.0B_dilution))

for (i in 1:length(glyc_6.0B_dilution)) {
  df_val <- glyc_6.0B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_6.0B_lum_values <- c(glyc_6.0B_luminescence[glyc_6.0B_dilution == df_val])
  glyc_6.0B_mean_lum[i] <- mean(glyc_6.0B_lum_values)
  glyc_6.0B_se_lum[i] <- sd(glyc_6.0B_lum_values) / sqrt(length(glyc_6.0B_lum_values))
  # Calculate concentration from mean luminescence
  glyc_6.0B_mean_conc[i] <- (glyc_6.0B_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_6.0B_data <- data.frame(
  glyc_6.0B_dilution_factor = glyc_6.0B_dilution,
  glyc_6.0B_mean_luminescence = glyc_6.0B_mean_lum,
  glyc_6.0B_se = glyc_6.0B_se_lum,
  glyc_6.0B_conc =  glyc_6.0B_mean_conc,
  label = paste0("6.0Bdf.", glyc_6.0B_dilution)
)
glyc_6.0B_mean_lum
glyc_6.0B_mean_conc
```

    [1] 7572.667
    [1] 1.690196

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_6.0C_mean_lum <- numeric(length(glyc_6.0C_dilution))
glyc_6.0C_se_lum <- numeric(length(glyc_6.0C_dilution))
glyc_6.0C_mean_conc <- numeric(length(glyc_6.0C_dilution))

for (i in 1:length(glyc_6.0C_dilution)) {
  df_val <- glyc_6.0C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_6.0C_lum_values <- c(glyc_6.0C_luminescence[glyc_6.0C_dilution == df_val])
  glyc_6.0C_mean_lum[i] <- mean(glyc_6.0C_lum_values)
  glyc_6.0C_se_lum[i] <- sd(glyc_6.0C_lum_values) / sqrt(length(glyc_6.0C_lum_values))
  # Calculate concentration from mean luminescence
  glyc_6.0C_mean_conc[i] <- (glyc_6.0C_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_6.0C_data <- data.frame(
  glyc_6.0C_dilution_factor = glyc_6.0C_dilution,
  glyc_6.0C_mean_luminescence = glyc_6.0C_mean_lum,
  glyc_6.0C_se = glyc_6.0C_se_lum,
  glyc_6.0C_conc =  glyc_6.0C_mean_conc,
  label = paste0("6.0Cdf.", glyc_6.0C_dilution)
)
glyc_6.0C_mean_lum
glyc_6.0C_mean_conc
```

    [1] 38357.67
    [1] 8.712794

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_6.2A_mean_lum <- numeric(length(glyc_6.2A_dilution))
glyc_6.2A_se_lum <- numeric(length(glyc_6.2A_dilution))
glyc_6.2A_mean_conc <- numeric(length(glyc_6.2A_dilution))

for (i in 1:length(glyc_6.2A_dilution)) {
  df_val <- glyc_6.2A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_6.2A_lum_values <- c(glyc_6.2A_luminescence[glyc_6.2A_dilution == df_val])
  glyc_6.2A_mean_lum[i] <- mean(glyc_6.2A_lum_values)
  glyc_6.2A_se_lum[i] <- sd(glyc_6.2A_lum_values) / sqrt(length(glyc_6.2A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_6.2A_mean_conc[i] <- (glyc_6.2A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_6.2A_data <- data.frame(
  glyc_6.2A_dilution_factor = glyc_6.2A_dilution,
  glyc_6.2A_mean_luminescence = glyc_6.2A_mean_lum,
  glyc_6.2A_se = glyc_6.2A_se_lum,
  glyc_6.2A_conc =  glyc_6.2A_mean_conc,
  label = paste0("6.2Adf.", glyc_6.2A_dilution)
)
glyc_6.2A_mean_lum
glyc_6.2A_mean_conc
```

    [1] 16980.33
    [1] 3.836249

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_6.2B_mean_lum <- numeric(length(glyc_6.2B_dilution))
glyc_6.2B_se_lum <- numeric(length(glyc_6.2B_dilution))
glyc_6.2B_mean_conc <- numeric(length(glyc_6.2B_dilution))

for (i in 1:length(glyc_6.2B_dilution)) {
  df_val <- glyc_6.2B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_6.2B_lum_values <- c(glyc_6.2B_luminescence[glyc_6.2B_dilution == df_val])
  glyc_6.2B_mean_lum[i] <- mean(glyc_6.2B_lum_values)
  glyc_6.2B_se_lum[i] <- sd(glyc_6.2B_lum_values) / sqrt(length(glyc_6.2B_lum_values))
  # Calculate concentration from mean luminescence
  glyc_6.2B_mean_conc[i] <- (glyc_6.2B_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_6.2B_data <- data.frame(
  glyc_6.2B_dilution_factor = glyc_6.2B_dilution,
  glyc_6.2B_mean_luminescence = glyc_6.2B_mean_lum,
  glyc_6.2B_se = glyc_6.2B_se_lum,
  glyc_6.2B_conc =  glyc_6.2B_mean_conc,
  label = paste0("6.2Bdf.", glyc_6.2B_dilution)
)
glyc_6.2B_mean_lum
glyc_6.2B_mean_conc
```

    [1] 19155.67
    [1] 4.332481

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_6.2C_mean_lum <- numeric(length(glyc_6.2C_dilution))
glyc_6.2C_se_lum <- numeric(length(glyc_6.2C_dilution))
glyc_6.2C_mean_conc <- numeric(length(glyc_6.2C_dilution))

for (i in 1:length(glyc_6.2C_dilution)) {
  df_val <- glyc_6.2C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_6.2C_lum_values <- c(glyc_6.2C_luminescence[glyc_6.2C_dilution == df_val])
  glyc_6.2C_mean_lum[i] <- mean(glyc_6.2C_lum_values)
  glyc_6.2C_se_lum[i] <- sd(glyc_6.2C_lum_values) / sqrt(length(glyc_6.2C_lum_values))
  # Calculate concentration from mean luminescence
  glyc_6.2C_mean_conc[i] <- (glyc_6.2C_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_6.2C_data <- data.frame(
  glyc_6.2C_dilution_factor = glyc_6.2C_dilution,
  glyc_6.2C_mean_luminescence = glyc_6.2C_mean_lum,
  glyc_6.2C_se = glyc_6.2C_se_lum,
  glyc_6.2C_conc =  glyc_6.2C_mean_conc,
  label = paste0("6.2Cdf.", glyc_6.2C_dilution)
)
glyc_6.2C_mean_lum
glyc_6.2C_mean_conc
```

    [1] 10324.33
    [1] 2.317899

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_6.7A_mean_lum <- numeric(length(glyc_6.7A_dilution))
glyc_6.7A_se_lum <- numeric(length(glyc_6.7A_dilution))
glyc_6.7A_mean_conc <- numeric(length(glyc_6.7A_dilution))

for (i in 1:length(glyc_6.7A_dilution)) {
  df_val <- glyc_6.7A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_6.7A_lum_values <- c(glyc_6.7A_luminescence[glyc_6.7A_dilution == df_val])
  glyc_6.7A_mean_lum[i] <- mean(glyc_6.7A_lum_values)
  glyc_6.7A_se_lum[i] <- sd(glyc_6.7A_lum_values) / sqrt(length(glyc_6.7A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_6.7A_mean_conc[i] <- (glyc_6.7A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_6.7A_data <- data.frame(
  glyc_6.7A_dilution_factor = glyc_6.7A_dilution,
  glyc_6.7A_mean_luminescence = glyc_6.7A_mean_lum,
  glyc_6.7A_se = glyc_6.7A_se_lum,
  glyc_6.7A_conc =  glyc_6.7A_mean_conc,
  label = paste0("6.7Adf.", glyc_6.7A_dilution)
)
glyc_6.7A_mean_lum
glyc_6.7A_mean_conc
```

    [1] 7964.667
    [1] 1.779618

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_6.7B_mean_lum <- numeric(length(glyc_6.7B_dilution))
glyc_6.7B_se_lum <- numeric(length(glyc_6.7B_dilution))
glyc_6.7B_mean_conc <- numeric(length(glyc_6.7B_dilution))

for (i in 1:length(glyc_6.7B_dilution)) {
  df_val <- glyc_6.7B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_6.7B_lum_values <- c(glyc_6.7B_luminescence[glyc_6.7B_dilution == df_val])
  glyc_6.7B_mean_lum[i] <- mean(glyc_6.7B_lum_values)
  glyc_6.7B_se_lum[i] <- sd(glyc_6.7B_lum_values) / sqrt(length(glyc_6.7B_lum_values))
  # Calculate concentration from mean luminescence
  glyc_6.7B_mean_conc[i] <- (glyc_6.7B_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_6.7B_data <- data.frame(
  glyc_6.7B_dilution_factor = glyc_6.7B_dilution,
  glyc_6.7B_mean_luminescence = glyc_6.7B_mean_lum,
  glyc_6.7B_se = glyc_6.7B_se_lum,
  glyc_6.7B_conc =  glyc_6.7B_mean_conc,
  label = paste0("6.7Bdf.", glyc_6.7B_dilution)
)
glyc_6.7B_mean_lum
glyc_6.7B_mean_conc
```

    [1] 32176.33
    [1] 7.302723

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_6.7C_mean_lum <- numeric(length(glyc_6.7C_dilution))
glyc_6.7C_se_lum <- numeric(length(glyc_6.7C_dilution))
glyc_6.7C_mean_conc <- numeric(length(glyc_6.7C_dilution))

for (i in 1:length(glyc_6.7C_dilution)) {
  df_val <- glyc_6.7C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_6.7C_lum_values <- c(glyc_6.7C_luminescence[glyc_6.7C_dilution == df_val])
  glyc_6.7C_mean_lum[i] <- mean(glyc_6.7C_lum_values)
  glyc_6.7C_se_lum[i] <- sd(glyc_6.7C_lum_values) / sqrt(length(glyc_6.7C_lum_values))
  # Calculate concentration from mean luminescence
  glyc_6.7C_mean_conc[i] <- (glyc_6.7C_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_6.7C_data <- data.frame(
  glyc_6.7C_dilution_factor = glyc_6.7C_dilution,
  glyc_6.7C_mean_luminescence = glyc_6.7C_mean_lum,
  glyc_6.7C_se = glyc_6.7C_se_lum,
  glyc_6.7C_conc =  glyc_6.7C_mean_conc,
  label = paste0("6.7Cdf.", glyc_6.7C_dilution)
)
glyc_6.7C_mean_lum
glyc_6.7C_mean_conc
```

    [1] 2835.333
    [1] 0.6095269

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_18.0A_mean_lum <- numeric(length(glyc_18.0A_dilution))
glyc_18.0A_se_lum <- numeric(length(glyc_18.0A_dilution))
glyc_18.0A_mean_conc <- numeric(length(glyc_18.0A_dilution))

for (i in 1:length(glyc_18.0A_dilution)) {
  df_val <- glyc_18.0A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_18.0A_lum_values <- c(glyc_18.0A_luminescence[glyc_18.0A_dilution == df_val])
  glyc_18.0A_mean_lum[i] <- mean(glyc_18.0A_lum_values)
  glyc_18.0A_se_lum[i] <- sd(glyc_18.0A_lum_values) / sqrt(length(glyc_18.0A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_18.0A_mean_conc[i] <- (glyc_18.0A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_18.0A_data <- data.frame(
  glyc_18.0A_dilution_factor = glyc_18.0A_dilution,
  glyc_18.0A_mean_luminescence = glyc_18.0A_mean_lum,
  glyc_18.0A_se = glyc_18.0A_se_lum,
  glyc_18.0A_conc =  glyc_18.0A_mean_conc,
  label = paste0("18.0Adf.", glyc_18.0A_dilution)
)
glyc_18.0A_mean_lum
glyc_18.0A_mean_conc
```

    [1] 10051.67
    [1] 2.255699

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_18.0B_mean_lum <- numeric(length(glyc_18.0B_dilution))
glyc_18.0B_se_lum <- numeric(length(glyc_18.0B_dilution))
glyc_18.0B_mean_conc <- numeric(length(glyc_18.0B_dilution))

for (i in 1:length(glyc_18.0B_dilution)) {
  df_val <- glyc_18.0B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_18.0B_lum_values <- c(glyc_18.0B_luminescence[glyc_18.0B_dilution == df_val])
  glyc_18.0B_mean_lum[i] <- mean(glyc_18.0B_lum_values)
  glyc_18.0B_se_lum[i] <- sd(glyc_18.0B_lum_values) / sqrt(length(glyc_18.0B_lum_values))
  # Calculate concentration from mean luminescence
  glyc_18.0B_mean_conc[i] <- (glyc_18.0B_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_18.0B_data <- data.frame(
  glyc_18.0B_dilution_factor = glyc_18.0B_dilution,
  glyc_18.0B_mean_luminescence = glyc_18.0B_mean_lum,
  glyc_18.0B_se = glyc_18.0B_se_lum,
  glyc_18.0B_conc =  glyc_18.0B_mean_conc,
  label = paste0("18.0Bdf.", glyc_18.0B_dilution)
)
glyc_18.0B_mean_lum
glyc_18.0B_mean_conc
```

    [1] 13742.67
    [1] 3.097681

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_18.0C_mean_lum <- numeric(length(glyc_18.0C_dilution))
glyc_18.0C_se_lum <- numeric(length(glyc_18.0C_dilution))
glyc_18.0C_mean_conc <- numeric(length(glyc_18.0C_dilution))

for (i in 1:length(glyc_18.0C_dilution)) {
  df_val <- glyc_18.0C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_18.0C_lum_values <- c(glyc_18.0C_luminescence[glyc_18.0C_dilution == df_val])
  glyc_18.0C_mean_lum[i] <- mean(glyc_18.0C_lum_values)
  glyc_18.0C_se_lum[i] <- sd(glyc_18.0C_lum_values) / sqrt(length(glyc_18.0C_lum_values))
  # Calculate concentration from mean luminescence
  glyc_18.0C_mean_conc[i] <- (glyc_18.0C_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_18.0C_data <- data.frame(
  glyc_18.0C_dilution_factor = glyc_18.0C_dilution,
  glyc_18.0C_mean_luminescence = glyc_18.0C_mean_lum,
  glyc_18.0C_se = glyc_18.0C_se_lum,
  glyc_18.0C_conc =  glyc_18.0C_mean_conc,
  label = paste0("18.0Cdf.", glyc_18.0C_dilution)
)
glyc_18.0C_mean_lum
glyc_18.0C_mean_conc
```

    [1] 15430.67
    [1] 3.482743

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_18.2A_mean_lum <- numeric(length(glyc_18.2A_dilution))
glyc_18.2A_se_lum <- numeric(length(glyc_18.2A_dilution))
glyc_18.2A_mean_conc <- numeric(length(glyc_18.2A_dilution))

for (i in 1:length(glyc_18.2A_dilution)) {
  df_val <- glyc_18.2A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_18.2A_lum_values <- c(glyc_18.2A_luminescence[glyc_18.2A_dilution == df_val])
  glyc_18.2A_mean_lum[i] <- mean(glyc_18.2A_lum_values)
  glyc_18.2A_se_lum[i] <- sd(glyc_18.2A_lum_values) / sqrt(length(glyc_18.2A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_18.2A_mean_conc[i] <- (glyc_18.2A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_18.2A_data <- data.frame(
  glyc_18.2A_dilution_factor = glyc_18.2A_dilution,
  glyc_18.2A_mean_luminescence = glyc_18.2A_mean_lum,
  glyc_18.2A_se = glyc_18.2A_se_lum,
  glyc_18.2A_conc =  glyc_18.2A_mean_conc,
  label = paste0("18.2Adf.", glyc_18.2A_dilution)
)
glyc_18.2A_mean_lum
glyc_18.2A_mean_conc
```

    [1] 15087.67
    [1] 3.404499

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_18.2B_mean_lum <- numeric(length(glyc_18.2B_dilution))
glyc_18.2B_se_lum <- numeric(length(glyc_18.2B_dilution))
glyc_18.2B_mean_conc <- numeric(length(glyc_18.2B_dilution))

for (i in 1:length(glyc_18.2B_dilution)) {
  df_val <- glyc_18.2B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_18.2B_lum_values <- c(glyc_18.2B_luminescence[glyc_18.2B_dilution == df_val])
  glyc_18.2B_mean_lum[i] <- mean(glyc_18.2B_lum_values)
  glyc_18.2B_se_lum[i] <- sd(glyc_18.2B_lum_values) / sqrt(length(glyc_18.2B_lum_values))
  # Calculate concentration from mean luminescence
  glyc_18.2B_mean_conc[i] <- (glyc_18.2B_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_18.2B_data <- data.frame(
  glyc_18.2B_dilution_factor = glyc_18.2B_dilution,
  glyc_18.2B_mean_luminescence = glyc_18.2B_mean_lum,
  glyc_18.2B_se = glyc_18.2B_se_lum,
  glyc_18.2B_conc =  glyc_18.2B_mean_conc,
  label = paste0("18.2Bdf.", glyc_18.2B_dilution)
)
glyc_18.2B_mean_lum
glyc_18.2B_mean_conc
```

    [1] 17003.67
    [1] 3.841572

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_18.2C_mean_lum <- numeric(length(glyc_18.2C_dilution))
glyc_18.2C_se_lum <- numeric(length(glyc_18.2C_dilution))
glyc_18.2C_mean_conc <- numeric(length(glyc_18.2C_dilution))

for (i in 1:length(glyc_18.2C_dilution)) {
  df_val <- glyc_18.2C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_18.2C_lum_values <- c(glyc_18.2C_luminescence[glyc_18.2C_dilution == df_val])
  glyc_18.2C_mean_lum[i] <- mean(glyc_18.2C_lum_values)
  glyc_18.2C_se_lum[i] <- sd(glyc_18.2C_lum_values) / sqrt(length(glyc_18.2C_lum_values))
  # Calculate concentration from mean luminescence
  glyc_18.2C_mean_conc[i] <- (glyc_18.2C_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_18.2C_data <- data.frame(
  glyc_18.2C_dilution_factor = glyc_18.2C_dilution,
  glyc_18.2C_mean_luminescence = glyc_18.2C_mean_lum,
  glyc_18.2C_se = glyc_18.2C_se_lum,
  glyc_18.2C_conc =  glyc_18.2C_mean_conc,
  label = paste0("18.2Cdf.", glyc_18.2C_dilution)
)
glyc_18.2C_mean_lum
glyc_18.2C_mean_conc
```

    [1] 11734
    [1] 2.639469

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_18.7A_mean_lum <- numeric(length(glyc_18.7A_dilution))
glyc_18.7A_se_lum <- numeric(length(glyc_18.7A_dilution))
glyc_18.7A_mean_conc <- numeric(length(glyc_18.7A_dilution))

for (i in 1:length(glyc_18.7A_dilution)) {
  df_val <- glyc_18.7A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_18.7A_lum_values <- c(glyc_18.7A_luminescence[glyc_18.7A_dilution == df_val])
  glyc_18.7A_mean_lum[i] <- mean(glyc_18.7A_lum_values)
  glyc_18.7A_se_lum[i] <- sd(glyc_18.7A_lum_values) / sqrt(length(glyc_18.7A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_18.7A_mean_conc[i] <- (glyc_18.7A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_18.7A_data <- data.frame(
  glyc_18.7A_dilution_factor = glyc_18.7A_dilution,
  glyc_18.7A_mean_luminescence = glyc_18.7A_mean_lum,
  glyc_18.7A_se = glyc_18.7A_se_lum,
  glyc_18.7A_conc =  glyc_18.7A_mean_conc,
  label = paste0("18.7Adf.", glyc_18.7A_dilution)
)
glyc_18.7A_mean_lum
glyc_18.7A_mean_conc
```

    [1] 10890
    [1] 2.446938

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_18.7B_mean_lum <- numeric(length(glyc_18.7B_dilution))
glyc_18.7B_se_lum <- numeric(length(glyc_18.7B_dilution))
glyc_18.7B_mean_conc <- numeric(length(glyc_18.7B_dilution))

for (i in 1:length(glyc_18.7B_dilution)) {
  df_val <- glyc_18.7B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_18.7B_lum_values <- c(glyc_18.7B_luminescence[glyc_18.7B_dilution == df_val])
  glyc_18.7B_mean_lum[i] <- mean(glyc_18.7B_lum_values)
  glyc_18.7B_se_lum[i] <- sd(glyc_18.7B_lum_values) / sqrt(length(glyc_18.7B_lum_values))
  # Calculate concentration from mean luminescence
  glyc_18.7B_mean_conc[i] <- (glyc_18.7B_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_18.7B_data <- data.frame(
  glyc_18.7B_dilution_factor = glyc_18.7B_dilution,
  glyc_18.7B_mean_luminescence = glyc_18.7B_mean_lum,
  glyc_18.7B_se = glyc_18.7B_se_lum,
  glyc_18.7B_conc =  glyc_18.7B_mean_conc,
  label = paste0("18.7Bdf.", glyc_18.7B_dilution)
)
glyc_18.7B_mean_lum
glyc_18.7B_mean_conc
```

    [1] 12844.33
    [1] 2.892755

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_18.7C_mean_lum <- numeric(length(glyc_18.7C_dilution))
glyc_18.7C_se_lum <- numeric(length(glyc_18.7C_dilution))
glyc_18.7C_mean_conc <- numeric(length(glyc_18.7C_dilution))

for (i in 1:length(glyc_18.7C_dilution)) {
  df_val <- glyc_18.7C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_18.7C_lum_values <- c(glyc_18.7C_luminescence[glyc_18.7C_dilution == df_val])
  glyc_18.7C_mean_lum[i] <- mean(glyc_18.7C_lum_values)
  glyc_18.7C_se_lum[i] <- sd(glyc_18.7C_lum_values) / sqrt(length(glyc_18.7C_lum_values))
  # Calculate concentration from mean luminescence
  glyc_18.7C_mean_conc[i] <- (glyc_18.7C_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_18.7C_data <- data.frame(
  glyc_18.7C_dilution_factor = glyc_18.7C_dilution,
  glyc_18.7C_mean_luminescence = glyc_18.7C_mean_lum,
  glyc_18.7C_se = glyc_18.7C_se_lum,
  glyc_18.7C_conc =  glyc_18.7C_mean_conc,
  label = paste0("18.7Cdf.", glyc_18.7C_dilution)
)
glyc_18.7C_mean_lum
glyc_18.7C_mean_conc
```

    [1] 21231.67
    [1] 4.806053

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_24.0A_mean_lum <- numeric(length(glyc_24.0A_dilution))
glyc_24.0A_se_lum <- numeric(length(glyc_24.0A_dilution))
glyc_24.0A_mean_conc <- numeric(length(glyc_24.0A_dilution))

for (i in 1:length(glyc_24.0A_dilution)) {
  df_val <- glyc_24.0A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_24.0A_lum_values <- c(glyc_24.0A_luminescence[glyc_24.0A_dilution == df_val])
  glyc_24.0A_mean_lum[i] <- mean(glyc_24.0A_lum_values)
  glyc_24.0A_se_lum[i] <- sd(glyc_24.0A_lum_values) / sqrt(length(glyc_24.0A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_24.0A_mean_conc[i] <- (glyc_24.0A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_24.0A_data <- data.frame(
  glyc_24.0A_dilution_factor = glyc_24.0A_dilution,
  glyc_24.0A_mean_luminescence = glyc_24.0A_mean_lum,
  glyc_24.0A_se = glyc_24.0A_se_lum,
  glyc_24.0A_conc =  glyc_24.0A_mean_conc,
  label = paste0("24.0Adf.", glyc_24.0A_dilution)
)
glyc_24.0A_mean_lum
glyc_24.0A_mean_conc
```

    [1] 7433.667
    [1] 1.658487

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_24.0B_mean_lum <- numeric(length(glyc_24.0B_dilution))
glyc_24.0B_se_lum <- numeric(length(glyc_24.0B_dilution))
glyc_24.0B_mean_conc <- numeric(length(glyc_24.0B_dilution))

for (i in 1:length(glyc_24.0B_dilution)) {
  df_val <- glyc_24.0B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_24.0B_lum_values <- c(glyc_24.0B_luminescence[glyc_24.0B_dilution == df_val])
  glyc_24.0B_mean_lum[i] <- mean(glyc_24.0B_lum_values)
  glyc_24.0B_se_lum[i] <- sd(glyc_24.0B_lum_values) / sqrt(length(glyc_24.0B_lum_values))
  # Calculate concentration from mean luminescence
  glyc_24.0B_mean_conc[i] <- (glyc_24.0B_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_24.0B_data <- data.frame(
  glyc_24.0B_dilution_factor = glyc_24.0B_dilution,
  glyc_24.0B_mean_luminescence = glyc_24.0B_mean_lum,
  glyc_24.0B_se = glyc_24.0B_se_lum,
  glyc_24.0B_conc =  glyc_24.0B_mean_conc,
  label = paste0("24.0Bdf.", glyc_24.0B_dilution)
)
glyc_24.0B_mean_lum
glyc_24.0B_mean_conc
```

    [1] 13109.33
    [1] 2.953206

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_24.0C_mean_lum <- numeric(length(glyc_24.0C_dilution))
glyc_24.0C_se_lum <- numeric(length(glyc_24.0C_dilution))
glyc_24.0C_mean_conc <- numeric(length(glyc_24.0C_dilution))

for (i in 1:length(glyc_24.0C_dilution)) {
  df_val <- glyc_24.0C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_24.0C_lum_values <- c(glyc_24.0C_luminescence[glyc_24.0C_dilution == df_val])
  glyc_24.0C_mean_lum[i] <- mean(glyc_24.0C_lum_values)
  glyc_24.0C_se_lum[i] <- sd(glyc_24.0C_lum_values) / sqrt(length(glyc_24.0C_lum_values))
  # Calculate concentration from mean luminescence
  glyc_24.0C_mean_conc[i] <- (glyc_24.0C_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_24.0C_data <- data.frame(
  glyc_24.0C_dilution_factor = glyc_24.0C_dilution,
  glyc_24.0C_mean_luminescence = glyc_24.0C_mean_lum,
  glyc_24.0C_se = glyc_24.0C_se_lum,
  glyc_24.0C_conc =  glyc_24.0C_mean_conc,
  label = paste0("24.0Cdf.", glyc_24.0C_dilution)
)
glyc_24.0C_mean_lum
glyc_24.0C_mean_conc
```

    [1] 5870
    [1] 1.301788

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

    [1] 3675.333
    [1] 0.8011456

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

    [1] 35763
    [1] 8.120905

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
  
  geom_errorbar(data = glyc_6.0A_data, aes(x = glyc_6.0A_conc, y = glyc_6.0A_mean_luminescence,
                ymin = glyc_6.0A_mean_luminescence - glyc_6.0A_se, ymax = glyc_6.0A_mean_luminescence + glyc_6.0A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_6.0A_data, aes(x = glyc_6.0A_conc, y = glyc_6.0A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_6.0B_data, aes(x = glyc_6.0B_conc, y = glyc_6.0B_mean_luminescence,
                ymin = glyc_6.0B_mean_luminescence - glyc_6.0B_se, ymax = glyc_6.0B_mean_luminescence + glyc_6.0B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_6.0B_data, aes(x = glyc_6.0B_conc, y = glyc_6.0B_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_6.0C_data, aes(x = glyc_6.0C_conc, y = glyc_6.0C_mean_luminescence,
                ymin = glyc_6.0C_mean_luminescence - glyc_6.0C_se, ymax = glyc_6.0C_mean_luminescence + glyc_6.0C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_6.0C_data, aes(x = glyc_6.0C_conc, y = glyc_6.0C_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_6.2A_data, aes(x = glyc_6.2A_conc, y = glyc_6.2A_mean_luminescence,
                ymin = glyc_6.2A_mean_luminescence - glyc_6.2A_se, ymax = glyc_6.2A_mean_luminescence + glyc_6.2A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_6.2A_data, aes(x = glyc_6.2A_conc, y = glyc_6.2A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_6.2B_data, aes(x = glyc_6.2B_conc, y = glyc_6.2B_mean_luminescence,
                ymin = glyc_6.2B_mean_luminescence - glyc_6.2B_se, ymax = glyc_6.2B_mean_luminescence + glyc_6.2B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_6.2B_data, aes(x = glyc_6.2B_conc, y = glyc_6.2B_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_6.2C_data, aes(x = glyc_6.2C_conc, y = glyc_6.2C_mean_luminescence,
                ymin = glyc_6.2C_mean_luminescence - glyc_6.2C_se, ymax = glyc_6.2C_mean_luminescence + glyc_6.2C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_6.2C_data, aes(x = glyc_6.2C_conc, y = glyc_6.2C_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_6.7A_data, aes(x = glyc_6.7A_conc, y = glyc_6.7A_mean_luminescence,
                ymin = glyc_6.7A_mean_luminescence - glyc_6.7A_se, ymax = glyc_6.7A_mean_luminescence + glyc_6.7A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_6.7A_data, aes(x = glyc_6.7A_conc, y = glyc_6.7A_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_6.7B_data, aes(x = glyc_6.7B_conc, y = glyc_6.7B_mean_luminescence,
                ymin = glyc_6.7B_mean_luminescence - glyc_6.7B_se, ymax = glyc_6.7B_mean_luminescence + glyc_6.7B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_6.7B_data, aes(x = glyc_6.7B_conc, y = glyc_6.7B_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_6.7C_data, aes(x = glyc_6.7C_conc, y = glyc_6.7C_mean_luminescence,
                ymin = glyc_6.7C_mean_luminescence - glyc_6.7C_se, ymax = glyc_6.7C_mean_luminescence + glyc_6.7C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_6.7C_data, aes(x = glyc_6.7C_conc, y = glyc_6.7C_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  
    geom_errorbar(data = glyc_18.0A_data, aes(x = glyc_18.0A_conc, y = glyc_18.0A_mean_luminescence,
                ymin = glyc_18.0A_mean_luminescence - glyc_18.0A_se, ymax = glyc_18.0A_mean_luminescence + glyc_18.0A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_18.0A_data, aes(x = glyc_18.0A_conc, y = glyc_18.0A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_18.0B_data, aes(x = glyc_18.0B_conc, y = glyc_18.0B_mean_luminescence,
                ymin = glyc_18.0B_mean_luminescence - glyc_18.0B_se, ymax = glyc_18.0B_mean_luminescence + glyc_18.0B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_18.0B_data, aes(x = glyc_18.0B_conc, y = glyc_18.0B_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_18.0C_data, aes(x = glyc_18.0C_conc, y = glyc_18.0C_mean_luminescence,
                ymin = glyc_18.0C_mean_luminescence - glyc_18.0C_se, ymax = glyc_18.0C_mean_luminescence + glyc_18.0C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_18.0C_data, aes(x = glyc_18.0C_conc, y = glyc_18.0C_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_18.2A_data, aes(x = glyc_18.2A_conc, y = glyc_18.2A_mean_luminescence,
                ymin = glyc_18.2A_mean_luminescence - glyc_18.2A_se, ymax = glyc_18.2A_mean_luminescence + glyc_18.2A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_18.2A_data, aes(x = glyc_18.2A_conc, y = glyc_18.2A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_18.2B_data, aes(x = glyc_18.2B_conc, y = glyc_18.2B_mean_luminescence,
                ymin = glyc_18.2B_mean_luminescence - glyc_18.2B_se, ymax = glyc_18.2B_mean_luminescence + glyc_18.2B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_18.2B_data, aes(x = glyc_18.2B_conc, y = glyc_18.2B_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_18.2C_data, aes(x = glyc_18.2C_conc, y = glyc_18.2C_mean_luminescence,
                ymin = glyc_18.2C_mean_luminescence - glyc_18.2C_se, ymax = glyc_18.2C_mean_luminescence + glyc_18.2C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_18.2C_data, aes(x = glyc_18.2C_conc, y = glyc_18.2C_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_18.7A_data, aes(x = glyc_18.7A_conc, y = glyc_18.7A_mean_luminescence,
                ymin = glyc_18.7A_mean_luminescence - glyc_18.7A_se, ymax = glyc_18.7A_mean_luminescence + glyc_18.7A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_18.7A_data, aes(x = glyc_18.7A_conc, y = glyc_18.7A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_18.7B_data, aes(x = glyc_18.7B_conc, y = glyc_18.7B_mean_luminescence,
                ymin = glyc_18.7B_mean_luminescence - glyc_18.7B_se, ymax = glyc_18.7B_mean_luminescence + glyc_18.7B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_18.7B_data, aes(x = glyc_18.7B_conc, y = glyc_18.7B_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_18.7C_data, aes(x = glyc_18.7C_conc, y = glyc_18.7C_mean_luminescence,
                ymin = glyc_18.7C_mean_luminescence - glyc_18.7C_se, ymax = glyc_18.7C_mean_luminescence + glyc_18.7C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_18.7C_data, aes(x = glyc_18.7C_conc, y = glyc_18.7C_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_24.0A_data, aes(x = glyc_24.0A_conc, y = glyc_24.0A_mean_luminescence,
                ymin = glyc_24.0A_mean_luminescence - glyc_24.0A_se, ymax = glyc_24.0A_mean_luminescence + glyc_24.0A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_24.0A_data, aes(x = glyc_24.0A_conc, y = glyc_24.0A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
      geom_errorbar(data = glyc_24.0B_data, aes(x = glyc_24.0B_conc, y = glyc_24.0B_mean_luminescence,
                ymin = glyc_24.0B_mean_luminescence - glyc_24.0B_se, ymax = glyc_24.0B_mean_luminescence + glyc_24.0B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_24.0B_data, aes(x = glyc_24.0B_conc, y = glyc_24.0B_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_24.0C_data, aes(x = glyc_24.0C_conc, y = glyc_24.0C_mean_luminescence,
                ymin = glyc_24.0C_mean_luminescence - glyc_24.0C_se, ymax = glyc_24.0C_mean_luminescence + glyc_24.0C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_24.0C_data, aes(x = glyc_24.0C_conc, y = glyc_24.0C_mean_luminescence,
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
  
  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                                "0.0Adf.20" = "darkred",
                                "6.0Adf.20" = "darkorange",
                                "6.0Bdf.20" = "darkgreen",
                                "6.0Cdf.20" = "purple",
                                "6.2Adf.20" = "brown",
                                "6.2Bdf.20" = "green3",
                                "6.2Cdf.20" = "firebrick1",
                                "6.7Adf.20" = "cyan",
                                "6.7Bdf.20" = "yellow3",
                                "6.7Cdf.20" = "thistle3",
                                "18.0Adf.20" = "azure4",
                                "18.0Bdf.20" = "bisque4",
                                "18.0Cdf.20" = "chartreuse",
                                "18.2Adf.20" = "chocolate4",
                                "18.2Bdf.20" = "darkslategray",
                                "18.2Cdf.20" = "deeppink2",
                                "18.7Adf.20" = "goldenrod3",
                                "18.7Bdf.20" = "hotpink3",
                                "18.7Cdf.20" = "mediumpurple2",
                                "24.0Adf.20" = "yellow",
                                "24.0Bdf.20" = "gray1",
                                "24.0Cdf.20" = "darkcyan",
                                "0.0Bdf.20" = "tomato3",
                                "0.0Cdf.20" = "turquoise"

                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                 "0.0Adf.20" = 17,
                                 "6.0Adf.20" = 15,
                                 "6.0Bdf.20" = 18,
                                 "6.0Cdf.20" = 8,
                                 "6.2Adf.20" = 4,
                                 "6.2Bdf.20" = 5,
                                 "6.2Cdf.20" = 0,
                                 "6.7Adf.20" = 2,
                                 "6.7Bdf.20" = 19,
                                 "6.7Cdf.20" = 20,
                                "18.0Adf.20" = 17,
                                 "18.0Bdf.20" = 15,
                                 "18.0Cdf.20" = 18,
                                 "18.2Adf.20" = 8,
                                 "18.2Bdf.20" = 4,
                                 "18.2Cdf.20" = 0,
                                 "18.7Adf.20" = 2,
                                 "18.7Bdf.20" = 19,
                                 "18.7Cdf.20" = 20,
                                  "24.0Adf.20" = 5,
                                  "24.0Bdf.20" = 0,
                                 "24.0Cdf.20" = 2,
                                 "0.0Bdf.20" = 19,
                                 "0.0Cdf.20" = 20

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

![](Gen5-20260323-tempprelim-mgig-glycogenglo_files/figure-gfm/plot%20glycogen%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glyc_0.0A_data
glyc_6.0A_data
glyc_6.0B_data
glyc_6.0C_data
glyc_6.2A_data
glyc_6.2B_data
glyc_6.2C_data
glyc_6.7A_data
glyc_6.7B_data
glyc_6.7C_data

glyc_18.0A_data
glyc_18.0B_data
glyc_18.0C_data
glyc_18.2A_data
glyc_18.2B_data
glyc_18.2C_data
glyc_18.7A_data
glyc_18.7B_data
glyc_18.7C_data
glyc_24.0A_data
glyc_24.0B_data
glyc_24.0C_data
glyc_0.0B_data
glyc_0.0C_data



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
    1                        20                    5835.333     130.1055
      glyc_0.0A_conc     label
    1       1.293879 0.0Adf.20
      glyc_6.0A_dilution_factor glyc_6.0A_mean_luminescence glyc_6.0A_se
    1                        20                    6165.667     75.12286
      glyc_6.0A_conc     label
    1       1.369234 6.0Adf.20
      glyc_6.0B_dilution_factor glyc_6.0B_mean_luminescence glyc_6.0B_se
    1                        20                    7572.667     185.3774
      glyc_6.0B_conc     label
    1       1.690196 6.0Bdf.20
      glyc_6.0C_dilution_factor glyc_6.0C_mean_luminescence glyc_6.0C_se
    1                        20                    38357.67     228.8626
      glyc_6.0C_conc     label
    1       8.712794 6.0Cdf.20
      glyc_6.2A_dilution_factor glyc_6.2A_mean_luminescence glyc_6.2A_se
    1                        20                    16980.33     97.77071
      glyc_6.2A_conc     label
    1       3.836249 6.2Adf.20
      glyc_6.2B_dilution_factor glyc_6.2B_mean_luminescence glyc_6.2B_se
    1                        20                    19155.67     473.4717
      glyc_6.2B_conc     label
    1       4.332481 6.2Bdf.20
      glyc_6.2C_dilution_factor glyc_6.2C_mean_luminescence glyc_6.2C_se
    1                        20                    10324.33     44.36716
      glyc_6.2C_conc     label
    1       2.317899 6.2Cdf.20
      glyc_6.7A_dilution_factor glyc_6.7A_mean_luminescence glyc_6.7A_se
    1                        20                    7964.667     93.22077
      glyc_6.7A_conc     label
    1       1.779618 6.7Adf.20
      glyc_6.7B_dilution_factor glyc_6.7B_mean_luminescence glyc_6.7B_se
    1                        20                    32176.33      434.189
      glyc_6.7B_conc     label
    1       7.302723 6.7Bdf.20
      glyc_6.7C_dilution_factor glyc_6.7C_mean_luminescence glyc_6.7C_se
    1                        20                    2835.333     67.70606
      glyc_6.7C_conc     label
    1      0.6095269 6.7Cdf.20
      glyc_18.0A_dilution_factor glyc_18.0A_mean_luminescence glyc_18.0A_se
    1                         20                     10051.67      840.2171
      glyc_18.0A_conc      label
    1        2.255699 18.0Adf.20
      glyc_18.0B_dilution_factor glyc_18.0B_mean_luminescence glyc_18.0B_se
    1                         20                     13742.67      307.9731
      glyc_18.0B_conc      label
    1        3.097681 18.0Bdf.20
      glyc_18.0C_dilution_factor glyc_18.0C_mean_luminescence glyc_18.0C_se
    1                         20                     15430.67      90.36285
      glyc_18.0C_conc      label
    1        3.482743 18.0Cdf.20
      glyc_18.2A_dilution_factor glyc_18.2A_mean_luminescence glyc_18.2A_se
    1                         20                     15087.67      42.01719
      glyc_18.2A_conc      label
    1        3.404499 18.2Adf.20
      glyc_18.2B_dilution_factor glyc_18.2B_mean_luminescence glyc_18.2B_se
    1                         20                     17003.67      45.84879
      glyc_18.2B_conc      label
    1        3.841572 18.2Bdf.20
      glyc_18.2C_dilution_factor glyc_18.2C_mean_luminescence glyc_18.2C_se
    1                         20                        11734      116.8888
      glyc_18.2C_conc      label
    1        2.639469 18.2Cdf.20
      glyc_18.7A_dilution_factor glyc_18.7A_mean_luminescence glyc_18.7A_se
    1                         20                        10890      123.7699
      glyc_18.7A_conc      label
    1        2.446938 18.7Adf.20
      glyc_18.7B_dilution_factor glyc_18.7B_mean_luminescence glyc_18.7B_se
    1                         20                     12844.33      191.6023
      glyc_18.7B_conc      label
    1        2.892755 18.7Bdf.20
      glyc_18.7C_dilution_factor glyc_18.7C_mean_luminescence glyc_18.7C_se
    1                         20                     21231.67      113.6608
      glyc_18.7C_conc      label
    1        4.806053 18.7Cdf.20
      glyc_24.0A_dilution_factor glyc_24.0A_mean_luminescence glyc_24.0A_se
    1                         20                     7433.667      471.6667
      glyc_24.0A_conc      label
    1        1.658487 24.0Adf.20
      glyc_24.0B_dilution_factor glyc_24.0B_mean_luminescence glyc_24.0B_se
    1                         20                     13109.33      116.8708
      glyc_24.0B_conc      label
    1        2.953206 24.0Bdf.20
      glyc_24.0C_dilution_factor glyc_24.0C_mean_luminescence glyc_24.0C_se
    1                         20                         5870      117.0128
      glyc_24.0C_conc      label
    1        1.301788 24.0Cdf.20
      glyc_0.0B_dilution_factor glyc_0.0B_mean_luminescence glyc_0.0B_se
    1                        20                    3675.333     36.14938
      glyc_0.0B_conc     label
    1      0.8011456 0.0Bdf.20
      glyc_0.0C_dilution_factor glyc_0.0C_mean_luminescence glyc_0.0C_se
    1                        20                       35763     686.5612
      glyc_0.0C_conc     label
    1       8.120905 0.0Cdf.20
    glycogen Standard Curve Summary:
    ==================================================
    Concentration: 20 µg/µL
      Mean Luminescence: 88011.00
      Standard Error: 2459.80
      CV%: 4.84%

    Concentration: 2 µg/µL
      Mean Luminescence: 7166.33
      Standard Error: 310.62
      CV%: 7.51%

    Concentration: 0.2 µg/µL
      Mean Luminescence: 1253.67
      Standard Error: 8.01
      CV%: 1.11%

    Concentration: 0.02 µg/µL
      Mean Luminescence: 1010.33
      Standard Error: 125.61
      CV%: 21.53%

    Concentration: 0 µg/µL
      Mean Luminescence: 781.33
      Standard Error: 29.85
      CV%: 6.62%

``` r
#plate1
glyc_0.0A_mean_conc_normalized <- glyc_0.0A_mean_conc/as.numeric(sample_weights[2,2])
glyc_6.0A_mean_conc_normalized <- glyc_6.0A_mean_conc/as.numeric(sample_weights[5,2])
glyc_6.0B_mean_conc_normalized <- glyc_6.0B_mean_conc/as.numeric(sample_weights[6,2])
glyc_6.0C_mean_conc_normalized <- glyc_6.0C_mean_conc/as.numeric(sample_weights[7,2])
glyc_6.2A_mean_conc_normalized <- glyc_6.2A_mean_conc/as.numeric(sample_weights[8,2])
glyc_6.2B_mean_conc_normalized <- glyc_6.2B_mean_conc/as.numeric(sample_weights[9,2])
glyc_6.2C_mean_conc_normalized <- glyc_6.2C_mean_conc/as.numeric(sample_weights[10,2])
glyc_6.7A_mean_conc_normalized <- glyc_6.7A_mean_conc/as.numeric(sample_weights[11,2])
glyc_6.7B_mean_conc_normalized <- glyc_6.7B_mean_conc/as.numeric(sample_weights[12,2])
glyc_6.7C_mean_conc_normalized <- glyc_6.7C_mean_conc/as.numeric(sample_weights[13,2])
#plate2
glyc_18.0A_mean_conc_normalized <- glyc_18.0A_mean_conc/as.numeric(sample_weights[14,2])
glyc_18.0B_mean_conc_normalized <- glyc_18.0B_mean_conc/as.numeric(sample_weights[15,2])
glyc_18.0C_mean_conc_normalized <- glyc_18.0C_mean_conc/as.numeric(sample_weights[16,2])
glyc_18.2A_mean_conc_normalized <- glyc_18.2A_mean_conc/as.numeric(sample_weights[17,2])
glyc_18.2B_mean_conc_normalized <- glyc_18.2B_mean_conc/as.numeric(sample_weights[18,2])
glyc_18.2C_mean_conc_normalized <- glyc_18.2C_mean_conc/as.numeric(sample_weights[19,2])
glyc_18.7A_mean_conc_normalized <- glyc_18.7A_mean_conc/as.numeric(sample_weights[20,2])
glyc_18.7B_mean_conc_normalized <- glyc_18.7B_mean_conc/as.numeric(sample_weights[21,2])
glyc_18.7C_mean_conc_normalized <- glyc_18.7C_mean_conc/as.numeric(sample_weights[22,2])
glyc_24.0A_mean_conc_normalized <- glyc_24.0A_mean_conc/as.numeric(sample_weights[23,2])
glyc_24.0B_mean_conc_normalized <- glyc_24.0B_mean_conc/as.numeric(sample_weights[24,2])
glyc_24.0C_mean_conc_normalized <- glyc_24.0C_mean_conc/as.numeric(sample_weights[25,2])
glyc_0.0B_mean_conc_normalized <- glyc_0.0B_mean_conc/as.numeric(sample_weights[3,2])
glyc_0.0C_mean_conc_normalized <- glyc_0.0C_mean_conc/as.numeric(sample_weights[4,2])
glyc_6.7B_mean_conc_normalized
```

    [1] 0.1776818

``` r
tab <- matrix(c(glyc_0.0A_dilution, glyc_0.0A_mean_lum, glyc_0.0A_mean_conc,(glyc_0.0A_dilution*glyc_0.0A_mean_conc), (glyc_0.0A_dilution*glyc_0.0A_mean_conc_normalized), 
                glyc_0.0B_dilution, glyc_0.0B_mean_lum, glyc_0.0B_mean_conc,(glyc_0.0B_dilution*glyc_0.0B_mean_conc), (glyc_0.0B_dilution*glyc_0.0B_mean_conc_normalized),
                glyc_0.0C_dilution, glyc_0.0C_mean_lum, glyc_0.0C_mean_conc,(glyc_0.0C_dilution*glyc_0.0C_mean_conc), (glyc_0.0C_dilution*glyc_0.0C_mean_conc_normalized), 
                glyc_6.0A_dilution, glyc_6.0A_mean_lum, glyc_6.0A_mean_conc,(glyc_6.0A_dilution*glyc_6.0A_mean_conc), (glyc_6.0A_dilution*glyc_6.0A_mean_conc_normalized),
                glyc_6.0B_dilution, glyc_6.0B_mean_lum, glyc_6.0B_mean_conc,(glyc_6.0B_dilution*glyc_6.0B_mean_conc), (glyc_6.0B_dilution*glyc_6.0B_mean_conc_normalized), 
                glyc_6.0C_dilution, glyc_6.0C_mean_lum, glyc_6.0C_mean_conc,(glyc_6.0C_dilution*glyc_6.0C_mean_conc), (glyc_6.0C_dilution*glyc_6.0C_mean_conc_normalized),
                glyc_6.2A_dilution, glyc_6.2A_mean_lum, glyc_6.2A_mean_conc,(glyc_6.2A_dilution*glyc_6.2A_mean_conc), (glyc_6.2A_dilution*glyc_6.2A_mean_conc_normalized), 
                glyc_6.2B_dilution, glyc_6.2B_mean_lum, glyc_6.2B_mean_conc,(glyc_6.2B_dilution*glyc_6.2B_mean_conc), (glyc_6.2B_dilution*glyc_6.2B_mean_conc_normalized),
                glyc_6.2C_dilution, glyc_6.2C_mean_lum, glyc_6.2C_mean_conc,(glyc_6.2C_dilution*glyc_6.2C_mean_conc), (glyc_6.2C_dilution*glyc_6.2C_mean_conc_normalized), 
                glyc_6.7A_dilution, glyc_6.7A_mean_lum, glyc_6.7A_mean_conc,(glyc_6.7A_dilution*glyc_6.7A_mean_conc), (glyc_6.7A_dilution*glyc_6.7A_mean_conc_normalized),
                glyc_6.7B_dilution, glyc_6.7B_mean_lum, glyc_6.7B_mean_conc,(glyc_6.7B_dilution*glyc_6.7B_mean_conc), (glyc_6.7B_dilution*glyc_6.7B_mean_conc_normalized), 
                glyc_6.7C_dilution, glyc_6.7C_mean_lum, glyc_6.7C_mean_conc,(glyc_6.7C_dilution*glyc_6.7C_mean_conc), (glyc_6.7C_dilution*glyc_6.7C_mean_conc_normalized),
                glyc_18.0A_dilution, glyc_18.0A_mean_lum, glyc_18.0A_mean_conc,(glyc_18.0A_dilution*glyc_18.0A_mean_conc), (glyc_18.0A_dilution*glyc_18.0A_mean_conc_normalized), 
                glyc_18.0B_dilution, glyc_18.0B_mean_lum, glyc_18.0B_mean_conc,(glyc_18.0B_dilution*glyc_18.0B_mean_conc), (glyc_18.0B_dilution*glyc_18.0B_mean_conc_normalized),
                glyc_18.0C_dilution, glyc_18.0C_mean_lum, glyc_18.0C_mean_conc,(glyc_18.0C_dilution*glyc_18.0C_mean_conc), (glyc_18.0C_dilution*glyc_18.0C_mean_conc_normalized), 
                glyc_18.2A_dilution, glyc_18.2A_mean_lum, glyc_18.2A_mean_conc,(glyc_18.2A_dilution*glyc_18.2A_mean_conc), (glyc_18.2A_dilution*glyc_18.2A_mean_conc_normalized),
                glyc_18.2B_dilution, glyc_18.2B_mean_lum, glyc_18.2B_mean_conc,(glyc_18.2B_dilution*glyc_18.2B_mean_conc), (glyc_18.2B_dilution*glyc_18.2B_mean_conc_normalized), 
                glyc_18.2C_dilution, glyc_18.2C_mean_lum, glyc_18.2C_mean_conc,(glyc_18.2C_dilution*glyc_18.2C_mean_conc), (glyc_18.2C_dilution*glyc_18.2C_mean_conc_normalized),
                glyc_18.7A_dilution, glyc_18.7A_mean_lum, glyc_18.7A_mean_conc,(glyc_18.7A_dilution*glyc_18.7A_mean_conc), (glyc_18.7A_dilution*glyc_18.7A_mean_conc_normalized), 
                glyc_18.7B_dilution, glyc_18.7B_mean_lum, glyc_18.7B_mean_conc,(glyc_18.7B_dilution*glyc_18.7B_mean_conc), (glyc_18.7B_dilution*glyc_18.7B_mean_conc_normalized),
                glyc_18.7C_dilution, glyc_18.7C_mean_lum, glyc_18.7C_mean_conc,(glyc_18.7C_dilution*glyc_18.7C_mean_conc), (glyc_18.7C_dilution*glyc_18.7C_mean_conc_normalized), 
                glyc_24.0A_dilution, glyc_24.0A_mean_lum, glyc_24.0A_mean_conc,(glyc_24.0A_dilution*glyc_24.0A_mean_conc), (glyc_24.0A_dilution*glyc_24.0A_mean_conc_normalized),
                glyc_24.0B_dilution, glyc_24.0B_mean_lum, glyc_24.0B_mean_conc,(glyc_24.0B_dilution*glyc_24.0B_mean_conc), (glyc_24.0B_dilution*glyc_24.0B_mean_conc_normalized), 
                glyc_24.0C_dilution, glyc_24.0C_mean_lum, glyc_24.0C_mean_conc,(glyc_24.0C_dilution*glyc_24.0C_mean_conc), (glyc_24.0C_dilution*glyc_24.0C_mean_conc_normalized)

), ncol=5, byrow=TRUE)
              
colnames(tab) <- c('Dilution factor','Luminescence','Calculated Glycogen (ug/uL)','Total glycogen (ug/uL)','Normalized glycogen (ug/uL/mg)')
rownames(tab) <- c('0.0A','0.0B','0.0C','6.0A','6.0B','6.0C','6.2A','6.2B','6.2C','6.7A','6.7B','6.7C','18.0A','18.0B','18.0C','18.2A','18.2B','18.2C','18.7A','18.7B','18.7C','24.0A','24.0B','24.0C')
tab <- as.table(tab)
tab
```

          Dilution factor Luminescence Calculated Glycogen (ug/uL)
    0.0A     2.000000e+01 5.835333e+03                1.293879e+00
    0.0B     2.000000e+01 3.675333e+03                8.011456e-01
    0.0C     2.000000e+01 3.576300e+04                8.120905e+00
    6.0A     2.000000e+01 6.165667e+03                1.369234e+00
    6.0B     2.000000e+01 7.572667e+03                1.690196e+00
    6.0C     2.000000e+01 3.835767e+04                8.712794e+00
    6.2A     2.000000e+01 1.698033e+04                3.836249e+00
    6.2B     2.000000e+01 1.915567e+04                4.332481e+00
    6.2C     2.000000e+01 1.032433e+04                2.317899e+00
    6.7A     2.000000e+01 7.964667e+03                1.779618e+00
    6.7B     2.000000e+01 3.217633e+04                7.302723e+00
    6.7C     2.000000e+01 2.835333e+03                6.095269e-01
    18.0A    2.000000e+01 1.005167e+04                2.255699e+00
    18.0B    2.000000e+01 1.374267e+04                3.097681e+00
    18.0C    2.000000e+01 1.543067e+04                3.482743e+00
    18.2A    2.000000e+01 1.508767e+04                3.404499e+00
    18.2B    2.000000e+01 1.700367e+04                3.841572e+00
    18.2C    2.000000e+01 1.173400e+04                2.639469e+00
    18.7A    2.000000e+01 1.089000e+04                2.446938e+00
    18.7B    2.000000e+01 1.284433e+04                2.892755e+00
    18.7C    2.000000e+01 2.123167e+04                4.806053e+00
    24.0A    2.000000e+01 7.433667e+03                1.658487e+00
    24.0B    2.000000e+01 1.310933e+04                2.953206e+00
    24.0C    2.000000e+01 5.870000e+03                1.301788e+00
          Total glycogen (ug/uL) Normalized glycogen (ug/uL/mg)
    0.0A            2.587759e+01                   1.587582e+00
    0.0B            1.602291e+01                   7.487342e-01
    0.0C            1.624181e+02                   7.416351e+00
    6.0A            2.738469e+01                   2.381277e+00
    6.0B            3.380391e+01                   2.414565e+00
    6.0C            1.742559e+02                   6.477914e+00
    6.2A            7.672499e+01                   4.918268e+00
    6.2B            8.664962e+01                   4.185972e+00
    6.2C            4.635798e+01                   2.726940e+00
    6.7A            3.559235e+01                   3.868734e+00
    6.7B            1.460545e+02                   3.553637e+00
    6.7C            1.219054e+01                   2.300101e+00
    18.0A           4.511398e+01                   2.337512e+00
    18.0B           6.195362e+01                   5.162801e+00
    18.0C           6.965486e+01                   3.316898e+00
    18.2A           6.808998e+01                   3.109131e+00
    18.2B           7.683144e+01                   3.766247e+00
    18.2C           5.278937e+01                   2.466793e+00
    18.7A           4.893875e+01                   3.978760e+00
    18.7B           5.785510e+01                   2.850005e+00
    18.7C           9.612106e+01                   6.629039e+00
    24.0A           3.316975e+01                   1.535636e+00
    24.0B           5.906413e+01                   3.394490e+00
    24.0C           2.603575e+01                   1.668958e+00

# 2 STANDARD CURVES

## 2.1 Glucose Standard Curve

``` r
# Extract glycogen standard curve data from plate layout and raw luminescence.
# Rows F, G, H (rows 6, 7, 8) correspond to glycogen standards
# Columns 1-5 contain the standard curve concentrations

# Extract concentration values from plate layout (row 6, columns 1-5)
# Parse concentration from labels like "STD-glyc-20" -> 20
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
glu_0.0A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[1, 7]))
glu_0.0A_luminescence <- as.numeric(raw_luminescence1[1, glu_sample_cols1])

glu_6.0A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[2, 7]))
glu_6.0A_luminescence <- as.numeric(raw_luminescence1[2, glu_sample_cols1])

glu_6.0B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[3, 7]))
glu_6.0B_luminescence <- as.numeric(raw_luminescence1[3, glu_sample_cols1])

glu_6.0C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[4, 7]))
glu_6.0C_luminescence <- as.numeric(raw_luminescence1[4, glu_sample_cols1])

glu_6.2A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[5, 7]))
glu_6.2A_luminescence <- as.numeric(raw_luminescence1[5, glu_sample_cols1])


glu_sample_cols2 <- c(10,11,12)
glu_6.2B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[1, 10]))
glu_6.2B_luminescence <- as.numeric(raw_luminescence1[1, glu_sample_cols2])

glu_6.2C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[2, 10]))
glu_6.2C_luminescence <- as.numeric(raw_luminescence1[2, glu_sample_cols2])

glu_6.7A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[3, 10]))
glu_6.7A_luminescence <- as.numeric(raw_luminescence1[3, glu_sample_cols2])

glu_6.7B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[4, 10]))
glu_6.7B_luminescence <- as.numeric(raw_luminescence1[4, glu_sample_cols2])

glu_6.7C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[5, 10]))
glu_6.7C_luminescence <- as.numeric(raw_luminescence1[5, glu_sample_cols2])


glu_sample_cols1 <- c(7,8,9)
glu_18.0A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[1, 7]))
glu_18.0A_luminescence <- as.numeric(raw_luminescence2[1, glu_sample_cols1])

glu_18.0B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[2, 7]))
glu_18.0B_luminescence <- as.numeric(raw_luminescence2[2, glu_sample_cols1])

glu_18.0C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[3, 7]))
glu_18.0C_luminescence <- as.numeric(raw_luminescence2[3, glu_sample_cols1])

glu_18.2A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[4, 7]))
glu_18.2A_luminescence <- as.numeric(raw_luminescence2[4, glu_sample_cols1])

glu_18.2B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[5, 7]))
glu_18.2B_luminescence <- as.numeric(raw_luminescence2[5, glu_sample_cols1])

glu_18.2C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[6, 7]))
glu_18.2C_luminescence <- as.numeric(raw_luminescence2[6, glu_sample_cols1])

glu_18.7A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[7, 7]))
glu_18.7A_luminescence <- as.numeric(raw_luminescence2[7, glu_sample_cols1])

glu_18.7B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[8, 7]))
glu_18.7B_luminescence <- as.numeric(raw_luminescence2[8, glu_sample_cols1])

glu_sample_cols2 <- c(10,11,12)
glu_18.7C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[1, 10]))
glu_18.7C_luminescence <- as.numeric(raw_luminescence2[1, glu_sample_cols2])

glu_24.0A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[2, 10]))
glu_24.0A_luminescence <- as.numeric(raw_luminescence2[2, glu_sample_cols2])

glu_24.0B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[3, 10]))
glu_24.0B_luminescence <- as.numeric(raw_luminescence2[3, glu_sample_cols2])

glu_24.0C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[4, 10]))
glu_24.0C_luminescence <- as.numeric(raw_luminescence2[4, glu_sample_cols2])

glu_0.0B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[5, 10]))
glu_0.0B_luminescence <- as.numeric(raw_luminescence2[5, glu_sample_cols2])

glu_0.0C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[6, 10]))
glu_0.0C_luminescence <- as.numeric(raw_luminescence2[6, c(11,12)])
#excluding first well of 0.0C - abnormally high - will be re-running sample
```

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
```

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

    [1] 598.6667
    [1] 0.5629822

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_6.0A_mean_lum <- numeric(length(glu_6.0A_dilution))
glu_6.0A_se_lum <- numeric(length(glu_6.0A_dilution))
glu_6.0A_mean_conc <- numeric(length(glu_6.0A_dilution))

for (i in 1:length(glu_6.0A_dilution)) {
  df_val <- glu_6.0A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_6.0A_lum_values <- c(glu_6.0A_luminescence[glu_6.0A_dilution == df_val])
  glu_6.0A_mean_lum[i] <- mean(glu_6.0A_lum_values)
  glu_6.0A_se_lum[i] <- sd(glu_6.0A_lum_values) / sqrt(length(glu_6.0A_lum_values))
  # Calculate concentration from mean luminescence
  glu_6.0A_mean_conc[i] <- (glu_6.0A_mean_lum[i] - glu_intercept) / glu_slope
}

glu_6.0A_data <- data.frame(
  glu_6.0A_dilution_factor = glu_6.0A_dilution,
  glu_6.0A_mean_luminescence = glu_6.0A_mean_lum,
  glu_6.0A_se = glu_6.0A_se_lum,
  glu_6.0A_conc =  glu_6.0A_mean_conc,
  label = paste0("6.0Adf.", glu_6.0A_dilution)
)
glu_6.0A_mean_lum
glu_6.0A_mean_conc
```

    [1] 580.6667
    [1] 0.5440085

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_6.0B_mean_lum <- numeric(length(glu_6.0B_dilution))
glu_6.0B_se_lum <- numeric(length(glu_6.0B_dilution))
glu_6.0B_mean_conc <- numeric(length(glu_6.0B_dilution))

for (i in 1:length(glu_6.0B_dilution)) {
  df_val <- glu_6.0B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_6.0B_lum_values <- c(glu_6.0B_luminescence[glu_6.0B_dilution == df_val])
  glu_6.0B_mean_lum[i] <- mean(glu_6.0B_lum_values)
  glu_6.0B_se_lum[i] <- sd(glu_6.0B_lum_values) / sqrt(length(glu_6.0B_lum_values))
  # Calculate concentration from mean luminescence
  glu_6.0B_mean_conc[i] <- (glu_6.0B_mean_lum[i] - glu_intercept) / glu_slope
}

glu_6.0B_data <- data.frame(
  glu_6.0B_dilution_factor = glu_6.0B_dilution,
  glu_6.0B_mean_luminescence = glu_6.0B_mean_lum,
  glu_6.0B_se = glu_6.0B_se_lum,
  glu_6.0B_conc =  glu_6.0B_mean_conc,
  label = paste0("6.0Bdf.", glu_6.0B_dilution)
)
glu_6.0B_mean_lum
glu_6.0B_mean_conc
```

    [1] 556.3333
    [1] 0.518359

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_6.0C_mean_lum <- numeric(length(glu_6.0C_dilution))
glu_6.0C_se_lum <- numeric(length(glu_6.0C_dilution))
glu_6.0C_mean_conc <- numeric(length(glu_6.0C_dilution))

for (i in 1:length(glu_6.0C_dilution)) {
  df_val <- glu_6.0C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_6.0C_lum_values <- c(glu_6.0C_luminescence[glu_6.0C_dilution == df_val])
  glu_6.0C_mean_lum[i] <- mean(glu_6.0C_lum_values)
  glu_6.0C_se_lum[i] <- sd(glu_6.0C_lum_values) / sqrt(length(glu_6.0C_lum_values))
  # Calculate concentration from mean luminescence
  glu_6.0C_mean_conc[i] <- (glu_6.0C_mean_lum[i] - glu_intercept) / glu_slope
}

glu_6.0C_data <- data.frame(
  glu_6.0C_dilution_factor = glu_6.0C_dilution,
  glu_6.0C_mean_luminescence = glu_6.0C_mean_lum,
  glu_6.0C_se = glu_6.0C_se_lum,
  glu_6.0C_conc =  glu_6.0C_mean_conc,
  label = paste0("6.0Cdf.", glu_6.0C_dilution)
)
glu_6.0C_mean_lum
glu_6.0C_mean_conc
```

    [1] 1255.667
    [1] 1.25552

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_6.2A_mean_lum <- numeric(length(glu_6.2A_dilution))
glu_6.2A_se_lum <- numeric(length(glu_6.2A_dilution))
glu_6.2A_mean_conc <- numeric(length(glu_6.2A_dilution))

for (i in 1:length(glu_6.2A_dilution)) {
  df_val <- glu_6.2A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_6.2A_lum_values <- c(glu_6.2A_luminescence[glu_6.2A_dilution == df_val])
  glu_6.2A_mean_lum[i] <- mean(glu_6.2A_lum_values)
  glu_6.2A_se_lum[i] <- sd(glu_6.2A_lum_values) / sqrt(length(glu_6.2A_lum_values))
  # Calculate concentration from mean luminescence
  glu_6.2A_mean_conc[i] <- (glu_6.2A_mean_lum[i] - glu_intercept) / glu_slope
}

glu_6.2A_data <- data.frame(
  glu_6.2A_dilution_factor = glu_6.2A_dilution,
  glu_6.2A_mean_luminescence = glu_6.2A_mean_lum,
  glu_6.2A_se = glu_6.2A_se_lum,
  glu_6.2A_conc =  glu_6.2A_mean_conc,
  label = paste0("6.2Adf.", glu_6.2A_dilution)
)
glu_6.2A_mean_lum
glu_6.2A_mean_conc
```

    [1] 939.3333
    [1] 0.9220761

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_6.2B_mean_lum <- numeric(length(glu_6.2B_dilution))
glu_6.2B_se_lum <- numeric(length(glu_6.2B_dilution))
glu_6.2B_mean_conc <- numeric(length(glu_6.2B_dilution))

for (i in 1:length(glu_6.2B_dilution)) {
  df_val <- glu_6.2B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_6.2B_lum_values <- c(glu_6.2B_luminescence[glu_6.2B_dilution == df_val])
  glu_6.2B_mean_lum[i] <- mean(glu_6.2B_lum_values)
  glu_6.2B_se_lum[i] <- sd(glu_6.2B_lum_values) / sqrt(length(glu_6.2B_lum_values))
  # Calculate concentration from mean luminescence
  glu_6.2B_mean_conc[i] <- (glu_6.2B_mean_lum[i] - glu_intercept) / glu_slope
}

glu_6.2B_data <- data.frame(
  glu_6.2B_dilution_factor = glu_6.2B_dilution,
  glu_6.2B_mean_luminescence = glu_6.2B_mean_lum,
  glu_6.2B_se = glu_6.2B_se_lum,
  glu_6.2B_conc =  glu_6.2B_mean_conc,
  label = paste0("6.2Bdf.", glu_6.2B_dilution)
)
glu_6.2B_mean_lum
glu_6.2B_mean_conc
```

    [1] 820.3333
    [1] 0.7966392

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_6.2C_mean_lum <- numeric(length(glu_6.2C_dilution))
glu_6.2C_se_lum <- numeric(length(glu_6.2C_dilution))
glu_6.2C_mean_conc <- numeric(length(glu_6.2C_dilution))

for (i in 1:length(glu_6.2C_dilution)) {
  df_val <- glu_6.2C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_6.2C_lum_values <- c(glu_6.2C_luminescence[glu_6.2C_dilution == df_val])
  glu_6.2C_mean_lum[i] <- mean(glu_6.2C_lum_values)
  glu_6.2C_se_lum[i] <- sd(glu_6.2C_lum_values) / sqrt(length(glu_6.2C_lum_values))
  # Calculate concentration from mean luminescence
  glu_6.2C_mean_conc[i] <- (glu_6.2C_mean_lum[i] - glu_intercept) / glu_slope
}

glu_6.2C_data <- data.frame(
  glu_6.2C_dilution_factor = glu_6.2C_dilution,
  glu_6.2C_mean_luminescence = glu_6.2C_mean_lum,
  glu_6.2C_se = glu_6.2C_se_lum,
  glu_6.2C_conc =  glu_6.2C_mean_conc,
  label = paste0("6.2Cdf.", glu_6.2C_dilution)
)
glu_6.2C_mean_lum
glu_6.2C_mean_conc
```

    [1] 712.3333
    [1] 0.6827973

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_6.7A_mean_lum <- numeric(length(glu_6.7A_dilution))
glu_6.7A_se_lum <- numeric(length(glu_6.7A_dilution))
glu_6.7A_mean_conc <- numeric(length(glu_6.7A_dilution))

for (i in 1:length(glu_6.7A_dilution)) {
  df_val <- glu_6.7A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_6.7A_lum_values <- c(glu_6.7A_luminescence[glu_6.7A_dilution == df_val])
  glu_6.7A_mean_lum[i] <- mean(glu_6.7A_lum_values)
  glu_6.7A_se_lum[i] <- sd(glu_6.7A_lum_values) / sqrt(length(glu_6.7A_lum_values))
  # Calculate concentration from mean luminescence
  glu_6.7A_mean_conc[i] <- (glu_6.7A_mean_lum[i] - glu_intercept) / glu_slope
}

glu_6.7A_data <- data.frame(
  glu_6.7A_dilution_factor = glu_6.7A_dilution,
  glu_6.7A_mean_luminescence = glu_6.7A_mean_lum,
  glu_6.7A_se = glu_6.7A_se_lum,
  glu_6.7A_conc =  glu_6.7A_mean_conc,
  label = paste0("6.7Adf.", glu_6.7A_dilution)
)
glu_6.7A_mean_lum
glu_6.7A_mean_conc
```

    [1] 661
    [1] 0.6286872

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_6.7B_mean_lum <- numeric(length(glu_6.7B_dilution))
glu_6.7B_se_lum <- numeric(length(glu_6.7B_dilution))
glu_6.7B_mean_conc <- numeric(length(glu_6.7B_dilution))

for (i in 1:length(glu_6.7B_dilution)) {
  df_val <- glu_6.7B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_6.7B_lum_values <- c(glu_6.7B_luminescence[glu_6.7B_dilution == df_val])
  glu_6.7B_mean_lum[i] <- mean(glu_6.7B_lum_values)
  glu_6.7B_se_lum[i] <- sd(glu_6.7B_lum_values) / sqrt(length(glu_6.7B_lum_values))
  # Calculate concentration from mean luminescence
  glu_6.7B_mean_conc[i] <- (glu_6.7B_mean_lum[i] - glu_intercept) / glu_slope
}

glu_6.7B_data <- data.frame(
  glu_6.7B_dilution_factor = glu_6.7B_dilution,
  glu_6.7B_mean_luminescence = glu_6.7B_mean_lum,
  glu_6.7B_se = glu_6.7B_se_lum,
  glu_6.7B_conc =  glu_6.7B_mean_conc,
  label = paste0("6.7Bdf.", glu_6.7B_dilution)
)
glu_6.7B_mean_lum
glu_6.7B_mean_conc
```

    [1] 1355.333
    [1] 1.360578

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_6.7C_mean_lum <- numeric(length(glu_6.7C_dilution))
glu_6.7C_se_lum <- numeric(length(glu_6.7C_dilution))
glu_6.7C_mean_conc <- numeric(length(glu_6.7C_dilution))

for (i in 1:length(glu_6.7C_dilution)) {
  df_val <- glu_6.7C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_6.7C_lum_values <- c(glu_6.7C_luminescence[glu_6.7C_dilution == df_val])
  glu_6.7C_mean_lum[i] <- mean(glu_6.7C_lum_values)
  glu_6.7C_se_lum[i] <- sd(glu_6.7C_lum_values) / sqrt(length(glu_6.7C_lum_values))
  # Calculate concentration from mean luminescence
  glu_6.7C_mean_conc[i] <- (glu_6.7C_mean_lum[i] - glu_intercept) / glu_slope
}

glu_6.7C_data <- data.frame(
  glu_6.7C_dilution_factor = glu_6.7C_dilution,
  glu_6.7C_mean_luminescence = glu_6.7C_mean_lum,
  glu_6.7C_se = glu_6.7C_se_lum,
  glu_6.7C_conc =  glu_6.7C_mean_conc,
  label = paste0("6.7Cdf.", glu_6.7C_dilution)
)
glu_6.7C_mean_lum
glu_6.7C_mean_conc
```

    [1] 459.3333
    [1] 0.4161121

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_18.0A_mean_lum <- numeric(length(glu_18.0A_dilution))
glu_18.0A_se_lum <- numeric(length(glu_18.0A_dilution))
glu_18.0A_mean_conc <- numeric(length(glu_18.0A_dilution))

for (i in 1:length(glu_18.0A_dilution)) {
  df_val <- glu_18.0A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_18.0A_lum_values <- c(glu_18.0A_luminescence[glu_18.0A_dilution == df_val])
  glu_18.0A_mean_lum[i] <- mean(glu_18.0A_lum_values)
  glu_18.0A_se_lum[i] <- sd(glu_18.0A_lum_values) / sqrt(length(glu_18.0A_lum_values))
  # Calculate concentration from mean luminescence
  glu_18.0A_mean_conc[i] <- (glu_18.0A_mean_lum[i] - glu_intercept) / glu_slope
}

glu_18.0A_data <- data.frame(
  glu_18.0A_dilution_factor = glu_18.0A_dilution,
  glu_18.0A_mean_luminescence = glu_18.0A_mean_lum,
  glu_18.0A_se = glu_18.0A_se_lum,
  glu_18.0A_conc =  glu_18.0A_mean_conc,
  label = paste0("18.0Adf.", glu_18.0A_dilution)
)
glu_18.0A_mean_lum
glu_18.0A_mean_conc
```

    [1] 785.3333
    [1] 0.759746

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_18.0B_mean_lum <- numeric(length(glu_18.0B_dilution))
glu_18.0B_se_lum <- numeric(length(glu_18.0B_dilution))
glu_18.0B_mean_conc <- numeric(length(glu_18.0B_dilution))

for (i in 1:length(glu_18.0B_dilution)) {
  df_val <- glu_18.0B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_18.0B_lum_values <- c(glu_18.0B_luminescence[glu_18.0B_dilution == df_val])
  glu_18.0B_mean_lum[i] <- mean(glu_18.0B_lum_values)
  glu_18.0B_se_lum[i] <- sd(glu_18.0B_lum_values) / sqrt(length(glu_18.0B_lum_values))
  # Calculate concentration from mean luminescence
  glu_18.0B_mean_conc[i] <- (glu_18.0B_mean_lum[i] - glu_intercept) / glu_slope
}

glu_18.0B_data <- data.frame(
  glu_18.0B_dilution_factor = glu_18.0B_dilution,
  glu_18.0B_mean_luminescence = glu_18.0B_mean_lum,
  glu_18.0B_se = glu_18.0B_se_lum,
  glu_18.0B_conc =  glu_18.0B_mean_conc,
  label = paste0("18.0Bdf.", glu_18.0B_dilution)
)
glu_18.0B_mean_lum
glu_18.0B_mean_conc
```

    [1] 800.6667
    [1] 0.7759087

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_18.0C_mean_lum <- numeric(length(glu_18.0C_dilution))
glu_18.0C_se_lum <- numeric(length(glu_18.0C_dilution))
glu_18.0C_mean_conc <- numeric(length(glu_18.0C_dilution))

for (i in 1:length(glu_18.0C_dilution)) {
  df_val <- glu_18.0C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_18.0C_lum_values <- c(glu_18.0C_luminescence[glu_18.0C_dilution == df_val])
  glu_18.0C_mean_lum[i] <- mean(glu_18.0C_lum_values)
  glu_18.0C_se_lum[i] <- sd(glu_18.0C_lum_values) / sqrt(length(glu_18.0C_lum_values))
  # Calculate concentration from mean luminescence
  glu_18.0C_mean_conc[i] <- (glu_18.0C_mean_lum[i] - glu_intercept) / glu_slope
}

glu_18.0C_data <- data.frame(
  glu_18.0C_dilution_factor = glu_18.0C_dilution,
  glu_18.0C_mean_luminescence = glu_18.0C_mean_lum,
  glu_18.0C_se = glu_18.0C_se_lum,
  glu_18.0C_conc =  glu_18.0C_mean_conc,
  label = paste0("18.0Cdf.", glu_18.0C_dilution)
)
glu_18.0C_mean_lum
glu_18.0C_mean_conc
```

    [1] 891.3333
    [1] 0.8714797

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_18.2A_mean_lum <- numeric(length(glu_18.2A_dilution))
glu_18.2A_se_lum <- numeric(length(glu_18.2A_dilution))
glu_18.2A_mean_conc <- numeric(length(glu_18.2A_dilution))

for (i in 1:length(glu_18.2A_dilution)) {
  df_val <- glu_18.2A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_18.2A_lum_values <- c(glu_18.2A_luminescence[glu_18.2A_dilution == df_val])
  glu_18.2A_mean_lum[i] <- mean(glu_18.2A_lum_values)
  glu_18.2A_se_lum[i] <- sd(glu_18.2A_lum_values) / sqrt(length(glu_18.2A_lum_values))
  # Calculate concentration from mean luminescence
  glu_18.2A_mean_conc[i] <- (glu_18.2A_mean_lum[i] - glu_intercept) / glu_slope
}

glu_18.2A_data <- data.frame(
  glu_18.2A_dilution_factor = glu_18.2A_dilution,
  glu_18.2A_mean_luminescence = glu_18.2A_mean_lum,
  glu_18.2A_se = glu_18.2A_se_lum,
  glu_18.2A_conc =  glu_18.2A_mean_conc,
  label = paste0("18.2Adf.", glu_18.2A_dilution)
)
glu_18.2A_mean_lum
glu_18.2A_mean_conc
```

    [1] 1193.333
    [1] 1.189815

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_18.2B_mean_lum <- numeric(length(glu_18.2B_dilution))
glu_18.2B_se_lum <- numeric(length(glu_18.2B_dilution))
glu_18.2B_mean_conc <- numeric(length(glu_18.2B_dilution))

for (i in 1:length(glu_18.2B_dilution)) {
  df_val <- glu_18.2B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_18.2B_lum_values <- c(glu_18.2B_luminescence[glu_18.2B_dilution == df_val])
  glu_18.2B_mean_lum[i] <- mean(glu_18.2B_lum_values)
  glu_18.2B_se_lum[i] <- sd(glu_18.2B_lum_values) / sqrt(length(glu_18.2B_lum_values))
  # Calculate concentration from mean luminescence
  glu_18.2B_mean_conc[i] <- (glu_18.2B_mean_lum[i] - glu_intercept) / glu_slope
}

glu_18.2B_data <- data.frame(
  glu_18.2B_dilution_factor = glu_18.2B_dilution,
  glu_18.2B_mean_luminescence = glu_18.2B_mean_lum,
  glu_18.2B_se = glu_18.2B_se_lum,
  glu_18.2B_conc =  glu_18.2B_mean_conc,
  label = paste0("18.2Bdf.", glu_18.2B_dilution)
)
glu_18.2B_mean_lum
glu_18.2B_mean_conc
```

    [1] 2456.333
    [1] 2.521133

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_18.2C_mean_lum <- numeric(length(glu_18.2C_dilution))
glu_18.2C_se_lum <- numeric(length(glu_18.2C_dilution))
glu_18.2C_mean_conc <- numeric(length(glu_18.2C_dilution))

for (i in 1:length(glu_18.2C_dilution)) {
  df_val <- glu_18.2C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_18.2C_lum_values <- c(glu_18.2C_luminescence[glu_18.2C_dilution == df_val])
  glu_18.2C_mean_lum[i] <- mean(glu_18.2C_lum_values)
  glu_18.2C_se_lum[i] <- sd(glu_18.2C_lum_values) / sqrt(length(glu_18.2C_lum_values))
  # Calculate concentration from mean luminescence
  glu_18.2C_mean_conc[i] <- (glu_18.2C_mean_lum[i] - glu_intercept) / glu_slope
}

glu_18.2C_data <- data.frame(
  glu_18.2C_dilution_factor = glu_18.2C_dilution,
  glu_18.2C_mean_luminescence = glu_18.2C_mean_lum,
  glu_18.2C_se = glu_18.2C_se_lum,
  glu_18.2C_conc =  glu_18.2C_mean_conc,
  label = paste0("18.2Cdf.", glu_18.2C_dilution)
)
glu_18.2C_mean_lum
glu_18.2C_mean_conc
```

    [1] 907.3333
    [1] 0.8883452

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_18.7A_mean_lum <- numeric(length(glu_18.7A_dilution))
glu_18.7A_se_lum <- numeric(length(glu_18.7A_dilution))
glu_18.7A_mean_conc <- numeric(length(glu_18.7A_dilution))

for (i in 1:length(glu_18.7A_dilution)) {
  df_val <- glu_18.7A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_18.7A_lum_values <- c(glu_18.7A_luminescence[glu_18.7A_dilution == df_val])
  glu_18.7A_mean_lum[i] <- mean(glu_18.7A_lum_values)
  glu_18.7A_se_lum[i] <- sd(glu_18.7A_lum_values) / sqrt(length(glu_18.7A_lum_values))
  # Calculate concentration from mean luminescence
  glu_18.7A_mean_conc[i] <- (glu_18.7A_mean_lum[i] - glu_intercept) / glu_slope
}

glu_18.7A_data <- data.frame(
  glu_18.7A_dilution_factor = glu_18.7A_dilution,
  glu_18.7A_mean_luminescence = glu_18.7A_mean_lum,
  glu_18.7A_se = glu_18.7A_se_lum,
  glu_18.7A_conc =  glu_18.7A_mean_conc,
  label = paste0("18.7Adf.", glu_18.7A_dilution)
)
glu_18.7A_mean_lum
glu_18.7A_mean_conc
```

    [1] 647.3333
    [1] 0.6142813

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_18.7B_mean_lum <- numeric(length(glu_18.7B_dilution))
glu_18.7B_se_lum <- numeric(length(glu_18.7B_dilution))
glu_18.7B_mean_conc <- numeric(length(glu_18.7B_dilution))

for (i in 1:length(glu_18.7B_dilution)) {
  df_val <- glu_18.7B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_18.7B_lum_values <- c(glu_18.7B_luminescence[glu_18.7B_dilution == df_val])
  glu_18.7B_mean_lum[i] <- mean(glu_18.7B_lum_values)
  glu_18.7B_se_lum[i] <- sd(glu_18.7B_lum_values) / sqrt(length(glu_18.7B_lum_values))
  # Calculate concentration from mean luminescence
  glu_18.7B_mean_conc[i] <- (glu_18.7B_mean_lum[i] - glu_intercept) / glu_slope
}

glu_18.7B_data <- data.frame(
  glu_18.7B_dilution_factor = glu_18.7B_dilution,
  glu_18.7B_mean_luminescence = glu_18.7B_mean_lum,
  glu_18.7B_se = glu_18.7B_se_lum,
  glu_18.7B_conc =  glu_18.7B_mean_conc,
  label = paste0("18.7Bdf.", glu_18.7B_dilution)
)
glu_18.7B_mean_lum
glu_18.7B_mean_conc
```

    [1] 1315.333
    [1] 1.318415

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_18.7C_mean_lum <- numeric(length(glu_18.7C_dilution))
glu_18.7C_se_lum <- numeric(length(glu_18.7C_dilution))
glu_18.7C_mean_conc <- numeric(length(glu_18.7C_dilution))

for (i in 1:length(glu_18.7C_dilution)) {
  df_val <- glu_18.7C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_18.7C_lum_values <- c(glu_18.7C_luminescence[glu_18.7C_dilution == df_val])
  glu_18.7C_mean_lum[i] <- mean(glu_18.7C_lum_values)
  glu_18.7C_se_lum[i] <- sd(glu_18.7C_lum_values) / sqrt(length(glu_18.7C_lum_values))
  # Calculate concentration from mean luminescence
  glu_18.7C_mean_conc[i] <- (glu_18.7C_mean_lum[i] - glu_intercept) / glu_slope
}

glu_18.7C_data <- data.frame(
  glu_18.7C_dilution_factor = glu_18.7C_dilution,
  glu_18.7C_mean_luminescence = glu_18.7C_mean_lum,
  glu_18.7C_se = glu_18.7C_se_lum,
  glu_18.7C_conc =  glu_18.7C_mean_conc,
  label = paste0("18.7Cdf.", glu_18.7C_dilution)
)
glu_18.7C_mean_lum
glu_18.7C_mean_conc
```

    [1] 971.3333
    [1] 0.955807

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_24.0A_mean_lum <- numeric(length(glu_24.0A_dilution))
glu_24.0A_se_lum <- numeric(length(glu_24.0A_dilution))
glu_24.0A_mean_conc <- numeric(length(glu_24.0A_dilution))

for (i in 1:length(glu_24.0A_dilution)) {
  df_val <- glu_24.0A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_24.0A_lum_values <- c(glu_24.0A_luminescence[glu_24.0A_dilution == df_val])
  glu_24.0A_mean_lum[i] <- mean(glu_24.0A_lum_values)
  glu_24.0A_se_lum[i] <- sd(glu_24.0A_lum_values) / sqrt(length(glu_24.0A_lum_values))
  # Calculate concentration from mean luminescence
  glu_24.0A_mean_conc[i] <- (glu_24.0A_mean_lum[i] - glu_intercept) / glu_slope
}

glu_24.0A_data <- data.frame(
  glu_24.0A_dilution_factor = glu_24.0A_dilution,
  glu_24.0A_mean_luminescence = glu_24.0A_mean_lum,
  glu_24.0A_se = glu_24.0A_se_lum,
  glu_24.0A_conc =  glu_24.0A_mean_conc,
  label = paste0("24.0Adf.", glu_24.0A_dilution)
)
glu_24.0A_mean_lum
glu_24.0A_mean_conc
```

    [1] 634
    [1] 0.6002267

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_24.0B_mean_lum <- numeric(length(glu_24.0B_dilution))
glu_24.0B_se_lum <- numeric(length(glu_24.0B_dilution))
glu_24.0B_mean_conc <- numeric(length(glu_24.0B_dilution))

for (i in 1:length(glu_24.0B_dilution)) {
  df_val <- glu_24.0B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_24.0B_lum_values <- c(glu_24.0B_luminescence[glu_24.0B_dilution == df_val])
  glu_24.0B_mean_lum[i] <- mean(glu_24.0B_lum_values)
  glu_24.0B_se_lum[i] <- sd(glu_24.0B_lum_values) / sqrt(length(glu_24.0B_lum_values))
  # Calculate concentration from mean luminescence
  glu_24.0B_mean_conc[i] <- (glu_24.0B_mean_lum[i] - glu_intercept) / glu_slope
}

glu_24.0B_data <- data.frame(
  glu_24.0B_dilution_factor = glu_24.0B_dilution,
  glu_24.0B_mean_luminescence = glu_24.0B_mean_lum,
  glu_24.0B_se = glu_24.0B_se_lum,
  glu_24.0B_conc =  glu_24.0B_mean_conc,
  label = paste0("24.0Bdf.", glu_24.0B_dilution)
)
glu_24.0B_mean_lum
glu_24.0B_mean_conc
```

    [1] 1044.333
    [1] 1.032756

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_24.0C_mean_lum <- numeric(length(glu_24.0C_dilution))
glu_24.0C_se_lum <- numeric(length(glu_24.0C_dilution))
glu_24.0C_mean_conc <- numeric(length(glu_24.0C_dilution))

for (i in 1:length(glu_24.0C_dilution)) {
  df_val <- glu_24.0C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_24.0C_lum_values <- c(glu_24.0C_luminescence[glu_24.0C_dilution == df_val])
  glu_24.0C_mean_lum[i] <- mean(glu_24.0C_lum_values)
  glu_24.0C_se_lum[i] <- sd(glu_24.0C_lum_values) / sqrt(length(glu_24.0C_lum_values))
  # Calculate concentration from mean luminescence
  glu_24.0C_mean_conc[i] <- (glu_24.0C_mean_lum[i] - glu_intercept) / glu_slope
}

glu_24.0C_data <- data.frame(
  glu_24.0C_dilution_factor = glu_24.0C_dilution,
  glu_24.0C_mean_luminescence = glu_24.0C_mean_lum,
  glu_24.0C_se = glu_24.0C_se_lum,
  glu_24.0C_conc =  glu_24.0C_mean_conc,
  label = paste0("24.0Cdf.", glu_24.0C_dilution)
)
glu_24.0C_mean_lum
glu_24.0C_mean_conc
```

    [1] 522
    [1] 0.4821685

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

    [1] 523.3333
    [1] 0.4835739

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

    [1] 976
    [1] 0.9607261

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
  
  geom_errorbar(data = glu_6.0A_data, aes(x = glu_6.0A_conc, y = glu_6.0A_mean_luminescence,
                ymin = glu_6.0A_mean_luminescence - glu_6.0A_se, ymax = glu_6.0A_mean_luminescence + glu_6.0A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_6.0A_data, aes(x = glu_6.0A_conc, y = glu_6.0A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_6.0B_data, aes(x = glu_6.0B_conc, y = glu_6.0B_mean_luminescence,
                ymin = glu_6.0B_mean_luminescence - glu_6.0B_se, ymax = glu_6.0B_mean_luminescence + glu_6.0B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_6.0B_data, aes(x = glu_6.0B_conc, y = glu_6.0B_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_6.0C_data, aes(x = glu_6.0C_conc, y = glu_6.0C_mean_luminescence,
                ymin = glu_6.0C_mean_luminescence - glu_6.0C_se, ymax = glu_6.0C_mean_luminescence + glu_6.0C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_6.0C_data, aes(x = glu_6.0C_conc, y = glu_6.0C_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_6.2A_data, aes(x = glu_6.2A_conc, y = glu_6.2A_mean_luminescence,
                ymin = glu_6.2A_mean_luminescence - glu_6.2A_se, ymax = glu_6.2A_mean_luminescence + glu_6.2A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_6.2A_data, aes(x = glu_6.2A_conc, y = glu_6.2A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_6.2B_data, aes(x = glu_6.2B_conc, y = glu_6.2B_mean_luminescence,
                ymin = glu_6.2B_mean_luminescence - glu_6.2B_se, ymax = glu_6.2B_mean_luminescence + glu_6.2B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_6.2B_data, aes(x = glu_6.2B_conc, y = glu_6.2B_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_6.2C_data, aes(x = glu_6.2C_conc, y = glu_6.2C_mean_luminescence,
                ymin = glu_6.2C_mean_luminescence - glu_6.2C_se, ymax = glu_6.2C_mean_luminescence + glu_6.2C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_6.2C_data, aes(x = glu_6.2C_conc, y = glu_6.2C_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_6.7A_data, aes(x = glu_6.7A_conc, y = glu_6.7A_mean_luminescence,
                ymin = glu_6.7A_mean_luminescence - glu_6.7A_se, ymax = glu_6.7A_mean_luminescence + glu_6.7A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_6.7A_data, aes(x = glu_6.7A_conc, y = glu_6.7A_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_6.7B_data, aes(x = glu_6.7B_conc, y = glu_6.7B_mean_luminescence,
                ymin = glu_6.7B_mean_luminescence - glu_6.7B_se, ymax = glu_6.7B_mean_luminescence + glu_6.7B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_6.7B_data, aes(x = glu_6.7B_conc, y = glu_6.7B_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_6.7C_data, aes(x = glu_6.7C_conc, y = glu_6.7C_mean_luminescence,
                ymin = glu_6.7C_mean_luminescence - glu_6.7C_se, ymax = glu_6.7C_mean_luminescence + glu_6.7C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_6.7C_data, aes(x = glu_6.7C_conc, y = glu_6.7C_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  
    geom_errorbar(data = glu_18.0A_data, aes(x = glu_18.0A_conc, y = glu_18.0A_mean_luminescence,
                ymin = glu_18.0A_mean_luminescence - glu_18.0A_se, ymax = glu_18.0A_mean_luminescence + glu_18.0A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_18.0A_data, aes(x = glu_18.0A_conc, y = glu_18.0A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_18.0B_data, aes(x = glu_18.0B_conc, y = glu_18.0B_mean_luminescence,
                ymin = glu_18.0B_mean_luminescence - glu_18.0B_se, ymax = glu_18.0B_mean_luminescence + glu_18.0B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_18.0B_data, aes(x = glu_18.0B_conc, y = glu_18.0B_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_18.0C_data, aes(x = glu_18.0C_conc, y = glu_18.0C_mean_luminescence,
                ymin = glu_18.0C_mean_luminescence - glu_18.0C_se, ymax = glu_18.0C_mean_luminescence + glu_18.0C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_18.0C_data, aes(x = glu_18.0C_conc, y = glu_18.0C_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_18.2A_data, aes(x = glu_18.2A_conc, y = glu_18.2A_mean_luminescence,
                ymin = glu_18.2A_mean_luminescence - glu_18.2A_se, ymax = glu_18.2A_mean_luminescence + glu_18.2A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_18.2A_data, aes(x = glu_18.2A_conc, y = glu_18.2A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_18.2B_data, aes(x = glu_18.2B_conc, y = glu_18.2B_mean_luminescence,
                ymin = glu_18.2B_mean_luminescence - glu_18.2B_se, ymax = glu_18.2B_mean_luminescence + glu_18.2B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_18.2B_data, aes(x = glu_18.2B_conc, y = glu_18.2B_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_18.2C_data, aes(x = glu_18.2C_conc, y = glu_18.2C_mean_luminescence,
                ymin = glu_18.2C_mean_luminescence - glu_18.2C_se, ymax = glu_18.2C_mean_luminescence + glu_18.2C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_18.2C_data, aes(x = glu_18.2C_conc, y = glu_18.2C_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_18.7A_data, aes(x = glu_18.7A_conc, y = glu_18.7A_mean_luminescence,
                ymin = glu_18.7A_mean_luminescence - glu_18.7A_se, ymax = glu_18.7A_mean_luminescence + glu_18.7A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_18.7A_data, aes(x = glu_18.7A_conc, y = glu_18.7A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_18.7B_data, aes(x = glu_18.7B_conc, y = glu_18.7B_mean_luminescence,
                ymin = glu_18.7B_mean_luminescence - glu_18.7B_se, ymax = glu_18.7B_mean_luminescence + glu_18.7B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_18.7B_data, aes(x = glu_18.7B_conc, y = glu_18.7B_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_18.7C_data, aes(x = glu_18.7C_conc, y = glu_18.7C_mean_luminescence,
                ymin = glu_18.7C_mean_luminescence - glu_18.7C_se, ymax = glu_18.7C_mean_luminescence + glu_18.7C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_18.7C_data, aes(x = glu_18.7C_conc, y = glu_18.7C_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_24.0A_data, aes(x = glu_24.0A_conc, y = glu_24.0A_mean_luminescence,
                ymin = glu_24.0A_mean_luminescence - glu_24.0A_se, ymax = glu_24.0A_mean_luminescence + glu_24.0A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_24.0A_data, aes(x = glu_24.0A_conc, y = glu_24.0A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
      geom_errorbar(data = glu_24.0B_data, aes(x = glu_24.0B_conc, y = glu_24.0B_mean_luminescence,
                ymin = glu_24.0B_mean_luminescence - glu_24.0B_se, ymax = glu_24.0B_mean_luminescence + glu_24.0B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_24.0B_data, aes(x = glu_24.0B_conc, y = glu_24.0B_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_24.0C_data, aes(x = glu_24.0C_conc, y = glu_24.0C_mean_luminescence,
                ymin = glu_24.0C_mean_luminescence - glu_24.0C_se, ymax = glu_24.0C_mean_luminescence + glu_24.0C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_24.0C_data, aes(x = glu_24.0C_conc, y = glu_24.0C_mean_luminescence,
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
  
  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                                "0.0Adf.20" = "darkred",
                                "6.0Adf.20" = "darkorange",
                                "6.0Bdf.20" = "darkgreen",
                                "6.0Cdf.20" = "purple",
                                "6.2Adf.20" = "brown",
                                "6.2Bdf.20" = "green3",
                                "6.2Cdf.20" = "firebrick1",
                                "6.7Adf.20" = "cyan",
                                "6.7Bdf.20" = "yellow3",
                                "6.7Cdf.20" = "thistle3",
                                "18.0Adf.20" = "azure4",
                                "18.0Bdf.20" = "bisque4",
                                "18.0Cdf.20" = "chartreuse",
                                "18.2Adf.20" = "chocolate4",
                                "18.2Bdf.20" = "darkslategray",
                                "18.2Cdf.20" = "deeppink2",
                                "18.7Adf.20" = "goldenrod3",
                                "18.7Bdf.20" = "hotpink3",
                                "18.7Cdf.20" = "mediumpurple2",
                                "24.0Adf.20" = "yellow",
                                "24.0Bdf.20" = "gray1",
                                "24.0Cdf.20" = "darkcyan",
                                "0.0Bdf.20" = "tomato3",
                                "0.0Cdf.20" = "turquoise"

                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                 "0.0Adf.20" = 17,
                                 "6.0Adf.20" = 15,
                                 "6.0Bdf.20" = 18,
                                 "6.0Cdf.20" = 8,
                                 "6.2Adf.20" = 4,
                                 "6.2Bdf.20" = 5,
                                 "6.2Cdf.20" = 0,
                                 "6.7Adf.20" = 2,
                                 "6.7Bdf.20" = 19,
                                 "6.7Cdf.20" = 20,
                                "18.0Adf.20" = 17,
                                 "18.0Bdf.20" = 15,
                                 "18.0Cdf.20" = 18,
                                 "18.2Adf.20" = 8,
                                 "18.2Bdf.20" = 4,
                                 "18.2Cdf.20" = 0,
                                 "18.7Adf.20" = 2,
                                 "18.7Bdf.20" = 19,
                                 "18.7Cdf.20" = 20,
                                  "24.0Adf.20" = 5,
                                  "24.0Bdf.20" = 0,
                                 "24.0Cdf.20" = 2,
                                 "0.0Bdf.20" = 19,
                                 "0.0Cdf.20" = 20

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
    x = "glucose Concentration (µM)",
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

![](Gen5-20260323-tempprelim-mgig-glycogenglo_files/figure-gfm/plot%20glucose%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glu_0.0A_data
glu_6.0A_data
glu_6.0B_data
glu_6.0C_data
glu_6.2A_data
glu_6.2B_data
glu_6.2C_data
glu_6.7A_data
glu_6.7B_data
glu_6.7C_data

glu_18.0A_data
glu_18.0B_data
glu_18.0C_data
glu_18.2A_data
glu_18.2B_data
glu_18.2C_data
glu_18.7A_data
glu_18.7B_data
glu_18.7C_data
glu_24.0A_data
glu_24.0B_data
glu_24.0C_data
glu_0.0B_data
glu_0.0C_data



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
    1                       20                   598.6667    4.409586     0.5629822
          label
    1 0.0Adf.20
      glu_6.0A_dilution_factor glu_6.0A_mean_luminescence glu_6.0A_se glu_6.0A_conc
    1                       20                   580.6667    12.12894     0.5440085
          label
    1 6.0Adf.20
      glu_6.0B_dilution_factor glu_6.0B_mean_luminescence glu_6.0B_se glu_6.0B_conc
    1                       20                   556.3333    15.89899      0.518359
          label
    1 6.0Bdf.20
      glu_6.0C_dilution_factor glu_6.0C_mean_luminescence glu_6.0C_se glu_6.0C_conc
    1                       20                   1255.667    39.88456       1.25552
          label
    1 6.0Cdf.20
      glu_6.2A_dilution_factor glu_6.2A_mean_luminescence glu_6.2A_se glu_6.2A_conc
    1                       20                   939.3333    3.282953     0.9220761
          label
    1 6.2Adf.20
      glu_6.2B_dilution_factor glu_6.2B_mean_luminescence glu_6.2B_se glu_6.2B_conc
    1                       20                   820.3333    28.03767     0.7966392
          label
    1 6.2Bdf.20
      glu_6.2C_dilution_factor glu_6.2C_mean_luminescence glu_6.2C_se glu_6.2C_conc
    1                       20                   712.3333    15.50627     0.6827973
          label
    1 6.2Cdf.20
      glu_6.7A_dilution_factor glu_6.7A_mean_luminescence glu_6.7A_se glu_6.7A_conc
    1                       20                        661    13.05118     0.6286872
          label
    1 6.7Adf.20
      glu_6.7B_dilution_factor glu_6.7B_mean_luminescence glu_6.7B_se glu_6.7B_conc
    1                       20                   1355.333    50.33996      1.360578
          label
    1 6.7Bdf.20
      glu_6.7C_dilution_factor glu_6.7C_mean_luminescence glu_6.7C_se glu_6.7C_conc
    1                       20                   459.3333    12.44097     0.4161121
          label
    1 6.7Cdf.20
      glu_18.0A_dilution_factor glu_18.0A_mean_luminescence glu_18.0A_se
    1                        20                    785.3333     16.90496
      glu_18.0A_conc      label
    1       0.759746 18.0Adf.20
      glu_18.0B_dilution_factor glu_18.0B_mean_luminescence glu_18.0B_se
    1                        20                    800.6667     39.26548
      glu_18.0B_conc      label
    1      0.7759087 18.0Bdf.20
      glu_18.0C_dilution_factor glu_18.0C_mean_luminescence glu_18.0C_se
    1                        20                    891.3333     42.72912
      glu_18.0C_conc      label
    1      0.8714797 18.0Cdf.20
      glu_18.2A_dilution_factor glu_18.2A_mean_luminescence glu_18.2A_se
    1                        20                    1193.333     176.0249
      glu_18.2A_conc      label
    1       1.189815 18.2Adf.20
      glu_18.2B_dilution_factor glu_18.2B_mean_luminescence glu_18.2B_se
    1                        20                    2456.333      1108.42
      glu_18.2B_conc      label
    1       2.521133 18.2Bdf.20
      glu_18.2C_dilution_factor glu_18.2C_mean_luminescence glu_18.2C_se
    1                        20                    907.3333     78.87613
      glu_18.2C_conc      label
    1      0.8883452 18.2Cdf.20
      glu_18.7A_dilution_factor glu_18.7A_mean_luminescence glu_18.7A_se
    1                        20                    647.3333     26.45961
      glu_18.7A_conc      label
    1      0.6142813 18.7Adf.20
      glu_18.7B_dilution_factor glu_18.7B_mean_luminescence glu_18.7B_se
    1                        20                    1315.333     36.83446
      glu_18.7B_conc      label
    1       1.318415 18.7Bdf.20
      glu_18.7C_dilution_factor glu_18.7C_mean_luminescence glu_18.7C_se
    1                        20                    971.3333     28.50341
      glu_18.7C_conc      label
    1       0.955807 18.7Cdf.20
      glu_24.0A_dilution_factor glu_24.0A_mean_luminescence glu_24.0A_se
    1                        20                         634     24.94661
      glu_24.0A_conc      label
    1      0.6002267 24.0Adf.20
      glu_24.0B_dilution_factor glu_24.0B_mean_luminescence glu_24.0B_se
    1                        20                    1044.333     192.9528
      glu_24.0B_conc      label
    1       1.032756 24.0Bdf.20
      glu_24.0C_dilution_factor glu_24.0C_mean_luminescence glu_24.0C_se
    1                        20                         522     19.65536
      glu_24.0C_conc      label
    1      0.4821685 24.0Cdf.20
      glu_0.0B_dilution_factor glu_0.0B_mean_luminescence glu_0.0B_se glu_0.0B_conc
    1                       20                   523.3333     38.6839     0.4835739
          label
    1 0.0Bdf.20
      glu_0.0C_dilution_factor glu_0.0C_mean_luminescence glu_0.0C_se glu_0.0C_conc
    1                       20                        976          22     0.9607261
          label
    1 0.0Cdf.20
    glucose Standard Curve Summary:
    ==================================================
    Concentration: 100 µg/µL
      Mean Luminescence: 94979.00
      Standard Error: 1888.93
      CV%: 3.44%

    Concentration: 10 µg/µL
      Mean Luminescence: 9084.67
      Standard Error: 65.69
      CV%: 1.25%

    Concentration: 1 µg/µL
      Mean Luminescence: 1060.67
      Standard Error: 11.10
      CV%: 1.81%

    Concentration: 0.1 µg/µL
      Mean Luminescence: 336.33
      Standard Error: 5.17
      CV%: 2.66%

    Concentration: 0 µg/µL
      Mean Luminescence: 261.00
      Standard Error: 5.57
      CV%: 3.69%

``` r
#plate1
glu_0.0A_mean_conc_normalized <- glu_0.0A_mean_conc/as.numeric(sample_weights[2,2])
glu_6.0A_mean_conc_normalized <- glu_6.0A_mean_conc/as.numeric(sample_weights[5,2])
glu_6.0B_mean_conc_normalized <- glu_6.0B_mean_conc/as.numeric(sample_weights[6,2])
glu_6.0C_mean_conc_normalized <- glu_6.0C_mean_conc/as.numeric(sample_weights[7,2])
glu_6.2A_mean_conc_normalized <- glu_6.2A_mean_conc/as.numeric(sample_weights[8,2])
glu_6.2B_mean_conc_normalized <- glu_6.2B_mean_conc/as.numeric(sample_weights[9,2])
glu_6.2C_mean_conc_normalized <- glu_6.2C_mean_conc/as.numeric(sample_weights[10,2])
glu_6.7A_mean_conc_normalized <- glu_6.7A_mean_conc/as.numeric(sample_weights[11,2])
glu_6.7B_mean_conc_normalized <- glu_6.7B_mean_conc/as.numeric(sample_weights[12,2])
glu_6.7C_mean_conc_normalized <- glu_6.7C_mean_conc/as.numeric(sample_weights[13,2])
#plate2
glu_18.0A_mean_conc_normalized <- glu_18.0A_mean_conc/as.numeric(sample_weights[14,2])
glu_18.0B_mean_conc_normalized <- glu_18.0B_mean_conc/as.numeric(sample_weights[15,2])
glu_18.0C_mean_conc_normalized <- glu_18.0C_mean_conc/as.numeric(sample_weights[16,2])
glu_18.2A_mean_conc_normalized <- glu_18.2A_mean_conc/as.numeric(sample_weights[17,2])
glu_18.2B_mean_conc_normalized <- glu_18.2B_mean_conc/as.numeric(sample_weights[18,2])
glu_18.2C_mean_conc_normalized <- glu_18.2C_mean_conc/as.numeric(sample_weights[19,2])
glu_18.7A_mean_conc_normalized <- glu_18.7A_mean_conc/as.numeric(sample_weights[20,2])
glu_18.7B_mean_conc_normalized <- glu_18.7B_mean_conc/as.numeric(sample_weights[21,2])
glu_18.7C_mean_conc_normalized <- glu_18.7C_mean_conc/as.numeric(sample_weights[22,2])
glu_24.0A_mean_conc_normalized <- glu_24.0A_mean_conc/as.numeric(sample_weights[23,2])
glu_24.0B_mean_conc_normalized <- glu_24.0B_mean_conc/as.numeric(sample_weights[24,2])
glu_24.0C_mean_conc_normalized <- glu_24.0C_mean_conc/as.numeric(sample_weights[25,2])
glu_0.0B_mean_conc_normalized <- glu_0.0B_mean_conc/as.numeric(sample_weights[3,2])
glu_0.0C_mean_conc_normalized <- glu_0.0C_mean_conc/as.numeric(sample_weights[4,2])
glu_6.7B_mean_conc_normalized
```

    [1] 0.03310409

``` r
tab <- matrix(c(glu_0.0A_dilution, glu_0.0A_mean_lum, glu_0.0A_mean_conc,(glu_0.0A_dilution*glu_0.0A_mean_conc), (glu_0.0A_dilution*glu_0.0A_mean_conc_normalized), 
                glu_0.0B_dilution, glu_0.0B_mean_lum, glu_0.0B_mean_conc,(glu_0.0B_dilution*glu_0.0B_mean_conc), (glu_0.0B_dilution*glu_0.0B_mean_conc_normalized),
                glu_0.0C_dilution, glu_0.0C_mean_lum, glu_0.0C_mean_conc,(glu_0.0C_dilution*glu_0.0C_mean_conc), (glu_0.0C_dilution*glu_0.0C_mean_conc_normalized), 
                glu_6.0A_dilution, glu_6.0A_mean_lum, glu_6.0A_mean_conc,(glu_6.0A_dilution*glu_6.0A_mean_conc), (glu_6.0A_dilution*glu_6.0A_mean_conc_normalized),
                glu_6.0B_dilution, glu_6.0B_mean_lum, glu_6.0B_mean_conc,(glu_6.0B_dilution*glu_6.0B_mean_conc), (glu_6.0B_dilution*glu_6.0B_mean_conc_normalized), 
                glu_6.0C_dilution, glu_6.0C_mean_lum, glu_6.0C_mean_conc,(glu_6.0C_dilution*glu_6.0C_mean_conc), (glu_6.0C_dilution*glu_6.0C_mean_conc_normalized),
                glu_6.2A_dilution, glu_6.2A_mean_lum, glu_6.2A_mean_conc,(glu_6.2A_dilution*glu_6.2A_mean_conc), (glu_6.2A_dilution*glu_6.2A_mean_conc_normalized), 
                glu_6.2B_dilution, glu_6.2B_mean_lum, glu_6.2B_mean_conc,(glu_6.2B_dilution*glu_6.2B_mean_conc), (glu_6.2B_dilution*glu_6.2B_mean_conc_normalized),
                glu_6.2C_dilution, glu_6.2C_mean_lum, glu_6.2C_mean_conc,(glu_6.2C_dilution*glu_6.2C_mean_conc), (glu_6.2C_dilution*glu_6.2C_mean_conc_normalized), 
                glu_6.7A_dilution, glu_6.7A_mean_lum, glu_6.7A_mean_conc,(glu_6.7A_dilution*glu_6.7A_mean_conc), (glu_6.7A_dilution*glu_6.7A_mean_conc_normalized),
                glu_6.7B_dilution, glu_6.7B_mean_lum, glu_6.7B_mean_conc,(glu_6.7B_dilution*glu_6.7B_mean_conc), (glu_6.7B_dilution*glu_6.7B_mean_conc_normalized), 
                glu_6.7C_dilution, glu_6.7C_mean_lum, glu_6.7C_mean_conc,(glu_6.7C_dilution*glu_6.7C_mean_conc), (glu_6.7C_dilution*glu_6.7C_mean_conc_normalized),
                glu_18.0A_dilution, glu_18.0A_mean_lum, glu_18.0A_mean_conc,(glu_18.0A_dilution*glu_18.0A_mean_conc), (glu_18.0A_dilution*glu_18.0A_mean_conc_normalized), 
                glu_18.0B_dilution, glu_18.0B_mean_lum, glu_18.0B_mean_conc,(glu_18.0B_dilution*glu_18.0B_mean_conc), (glu_18.0B_dilution*glu_18.0B_mean_conc_normalized),
                glu_18.0C_dilution, glu_18.0C_mean_lum, glu_18.0C_mean_conc,(glu_18.0C_dilution*glu_18.0C_mean_conc), (glu_18.0C_dilution*glu_18.0C_mean_conc_normalized), 
                glu_18.2A_dilution, glu_18.2A_mean_lum, glu_18.2A_mean_conc,(glu_18.2A_dilution*glu_18.2A_mean_conc), (glu_18.2A_dilution*glu_18.2A_mean_conc_normalized),
                glu_18.2B_dilution, glu_18.2B_mean_lum, glu_18.2B_mean_conc,(glu_18.2B_dilution*glu_18.2B_mean_conc), (glu_18.2B_dilution*glu_18.2B_mean_conc_normalized), 
                glu_18.2C_dilution, glu_18.2C_mean_lum, glu_18.2C_mean_conc,(glu_18.2C_dilution*glu_18.2C_mean_conc), (glu_18.2C_dilution*glu_18.2C_mean_conc_normalized),
                glu_18.7A_dilution, glu_18.7A_mean_lum, glu_18.7A_mean_conc,(glu_18.7A_dilution*glu_18.7A_mean_conc), (glu_18.7A_dilution*glu_18.7A_mean_conc_normalized), 
                glu_18.7B_dilution, glu_18.7B_mean_lum, glu_18.7B_mean_conc,(glu_18.7B_dilution*glu_18.7B_mean_conc), (glu_18.7B_dilution*glu_18.7B_mean_conc_normalized),
                glu_18.7C_dilution, glu_18.7C_mean_lum, glu_18.7C_mean_conc,(glu_18.7C_dilution*glu_18.7C_mean_conc), (glu_18.7C_dilution*glu_18.7C_mean_conc_normalized), 
                glu_24.0A_dilution, glu_24.0A_mean_lum, glu_24.0A_mean_conc,(glu_24.0A_dilution*glu_24.0A_mean_conc), (glu_24.0A_dilution*glu_24.0A_mean_conc_normalized),
                glu_24.0B_dilution, glu_24.0B_mean_lum, glu_24.0B_mean_conc,(glu_24.0B_dilution*glu_24.0B_mean_conc), (glu_24.0B_dilution*glu_24.0B_mean_conc_normalized), 
                glu_24.0C_dilution, glu_24.0C_mean_lum, glu_24.0C_mean_conc,(glu_24.0C_dilution*glu_24.0C_mean_conc), (glu_24.0C_dilution*glu_24.0C_mean_conc_normalized)

), ncol=5, byrow=TRUE)
              
colnames(tab) <- c('Dilution factor','Luminescence','Calculated glucose (uM)','Total glucose (uM)','Normalized glucose (uM/mg)')
rownames(tab) <- c('0.0A','0.0B','0.0C','6.0A','6.0B','6.0C','6.2A','6.2B','6.2C','6.7A','6.7B','6.7C','18.0A','18.0B','18.0C','18.2A','18.2B','18.2C','18.7A','18.7B','18.7C','24.0A','24.0B','24.0C')
tab <- as.table(tab)
tab
```

          Dilution factor Luminescence Calculated glucose (uM) Total glucose (uM)
    0.0A       20.0000000  598.6666667               0.5629822         11.2596435
    0.0B       20.0000000  523.3333333               0.4835739          9.6714786
    0.0C       20.0000000  976.0000000               0.9607261         19.2145226
    6.0A       20.0000000  580.6666667               0.5440085         10.8801705
    6.0B       20.0000000  556.3333333               0.5183590         10.3671791
    6.0C       20.0000000 1255.6666667               1.2555205         25.1104091
    6.2A       20.0000000  939.3333333               0.9220761         18.4415220
    6.2B       20.0000000  820.3333333               0.7966392         15.9327836
    6.2C       20.0000000  712.3333333               0.6827973         13.6559454
    6.7A       20.0000000  661.0000000               0.6286872         12.5737445
    6.7B       20.0000000 1355.3333333               1.3605783         27.2115654
    6.7C       20.0000000  459.3333333               0.4161121          8.3222411
    18.0A      20.0000000  785.3333333               0.7597460         15.1949194
    18.0B      20.0000000  800.6666667               0.7759087         15.5181742
    18.0C      20.0000000  891.3333333               0.8714797         17.4295939
    18.2A      20.0000000 1193.3333333               1.1898154         23.7963081
    18.2B      20.0000000 2456.3333333               2.5211333         50.4226658
    18.2C      20.0000000  907.3333333               0.8883452         17.7669033
    18.7A      20.0000000  647.3333333               0.6142813         12.2856261
    18.7B      20.0000000 1315.3333333               1.3184146         26.3682920
    18.7C      20.0000000  971.3333333               0.9558070         19.1161407
    24.0A      20.0000000  634.0000000               0.6002267         12.0045350
    24.0B      20.0000000 1044.3333333               1.0327557         20.6551147
    24.0C      20.0000000  522.0000000               0.4821685          9.6433695
          Normalized glucose (uM/mg)
    0.0A                   0.6907757
    0.0B                   0.4519383
    0.0C                   0.8773755
    6.0A                   0.9461018
    6.0B                   0.7405128
    6.0C                   0.9334725
    6.2A                   1.1821488
    6.2B                   0.7696997
    6.2C                   0.8032909
    6.7A                   1.3667114
    6.7B                   0.6620819
    6.7C                   1.5702342
    18.0A                  0.7873015
    18.0B                  1.2931812
    18.0C                  0.8299807
    18.2A                  1.0865894
    18.2B                  2.4716993
    18.2C                  0.8302291
    18.7A                  0.9988314
    18.7B                  1.2989306
    18.7C                  1.3183545
    24.0A                  0.5557655
    24.0B                  1.1870756
    24.0C                  0.6181647
