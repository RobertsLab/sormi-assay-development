Gen5-20260204-mgig-glycogenglo
================
Hazel Abrahamson-Amerine
2026-02-27

- [1 STANDARD CURVES](#1-standard-curves)
  - [1.1 Glycogen Standard Curve](#11-glycogen-standard-curve)
    - [1.1.1 Extract glycogen luminescence
      data](#111-extract-glycogen-luminescence-data)
    - [1.1.2 Glycogen standard curve summary statistics and linear
      regression](#112-glycogen-standard-curve-summary-statistics-and-linear-regression)
    - [1.1.3 Calculate sample glycogen
      levels](#113-calculate-sample-glycogen-levels)
    - [1.1.4 Plot glycogen standard curve, sample
      points](#114-plot-glycogen-standard-curve-sample-points)
    - [1.1.5 Glycogen summary table](#115-glycogen-summary-table)
  - [1.2 Glucose Standard Curve](#12-glucose-standard-curve)
    - [1.2.1 Glucose standard curve summary statistics and linear
      regression](#121-glucose-standard-curve-summary-statistics-and-linear-regression)
    - [1.2.2 Calculate sample glucose
      levels](#122-calculate-sample-glucose-levels)
    - [1.2.3 Plot glucose standard curve, sample
      points](#123-plot-glucose-standard-curve-sample-points)
    - [1.2.4 Glucose summary table](#124-glucose-summary-table)
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
plate_layout <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-Gen5-20260204-mgig-glycogenglo.csv", header = FALSE)
raw_luminescence <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-Gen5-20260204-mgig-glycogenglo.csv", header = FALSE)
weights <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/20251210-mgig-ctenidia_weights-glycogen_assay.csv")


cat("Plate layout:\n")
str(plate_layout)

cat("\n\n")

cat("Raw luminescence:\n")
str(raw_luminescence)
```

    Plate layout:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : chr  "A6-glyc-25-df.20" "A7-glyc-17-df.20" "A8-glyc-12-df.20" "B1-glyc-20-df.20" ...
     $ V2 : chr  "A6-glyc-25-df.20" "A7-glyc-17-df.20" "A8-glyc-12-df.20" "B1-glyc-20-df.20" ...
     $ V3 : chr  "A6-glyc-25-df.20" "A7-glyc-17-df.20" "A8-glyc-12-df.20" "B1-glyc-20-df.20" ...
     $ V4 : chr  "B3-glyc-15-df.20" "B4-glyc-28-df.20" "B5-glyc-27-df.20" "B6-glyc-35-df.20" ...
     $ V5 : chr  "B3-glyc-15-df.20" "B4-glyc-28-df.20" "B5-glyc-27-df.20" "B6-glyc-35-df.20" ...
     $ V6 : chr  "B3-glyc-15-df.20" "B4-glyc-28-df.20" "B5-glyc-27-df.20" "B6-glyc-35-df.20" ...
     $ V7 : chr  "A6-glu-25-df.20" "A7-glu-17-df.20" "A8-glu-12-df.20" "B1-glu-20-df.20" ...
     $ V8 : chr  "A6-glu-25-df.20" "A7-glu-17-df.20" "A8-glu-12-df.20" "B1-glu-20-df.20" ...
     $ V9 : chr  "A6-glu-25-df.20" "A7-glu-17-df.20" "A8-glu-12-df.20" "B1-glu-20-df.20" ...
     $ V10: chr  "B3-glu-15-df.20" "B4-glu-28-df.20" "B5-glu-27-df.20" "B6-glu-35-df.20" ...
     $ V11: chr  "B3-glu-15-df.20" "B4-glu-28-df.20" "B5-glu-27-df.20" "B6-glu-35-df.20" ...
     $ V12: chr  "B3-glu-15-df.20" "B4-glu-28-df.20" "B5-glu-27-df.20" "B6-glu-35-df.20" ...


    Raw luminescence:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : int  45305 13069 44163 33662 61951 114941 114336 115315
     $ V2 : int  44213 11211 43305 34529 63244 7101 6906 9660
     $ V3 : int  44763 13204 41636 33178 66142 1439 1382 1501
     $ V4 : int  20383 89772 81935 41112 5933 964 989 1028
     $ V5 : int  32232 85256 86742 45344 5986 944 1258 912
     $ V6 : int  27787 88576 86889 45379 10980 NA NA NA
     $ V7 : int  5855 5050 7153 7284 8037 123828 125644 125987
     $ V8 : int  5669 13272 3580 7116 4004 12699 15394 13826
     $ V9 : int  10306 9401 1181 2685 4433 9748 2458 1655
     $ V10: int  5014 674 1304 5322 7924 8758 5016 582
     $ V11: int  2188 1123 547 11109 3497 3299 757 382
     $ V12: int  1140 2027 4170 713 4318 NA NA NA

# 1 STANDARD CURVES

## 1.1 Glycogen Standard Curve

### 1.1.1 Extract glycogen luminescence data

``` r
# Extract glycogen standard curve data from plate layout and raw luminescence.
# Rows F, G, H (rows 6, 7, 8) correspond to glycogen standards
# Columns 1-5 contain the standard curve concentrations

# Extract concentration values from plate layout (row 6, columns 1-5)
# Parse concentration from labels like "STD-glyc-20" -> 20
glyc_concentrations <- as.numeric(gsub("STD-glyc-", "", plate_layout[6, 1:5]))
glyc_concentrations

# Extract standard curve luminescence values for each replicate from raw_luminescence
glyc_row_F <- as.numeric(raw_luminescence[6, 1:5])  # Row 6 (F)
glyc_row_G <- as.numeric(raw_luminescence[7, 1:5])  # Row 7 (G)
glyc_row_H <- as.numeric(raw_luminescence[8, 1:5])  # Row 8 (H)
```

    [1] 20.00  2.00  0.20  0.02  0.00

``` r
#Extract glycogen sample data - wells A6-E6
glyc_sample_cols1 <- c(1,2,3)
glyc_A6_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 1]))
glyc_A6_luminescence <- as.numeric(raw_luminescence[1, glyc_sample_cols1])

glyc_A7_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 1]))
glyc_A7_luminescence <- as.numeric(raw_luminescence[2, glyc_sample_cols1])

glyc_A8_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 1]))
glyc_A8_luminescence <- as.numeric(raw_luminescence[3, glyc_sample_cols1])

glyc_B1_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 1]))
glyc_B1_luminescence <- as.numeric(raw_luminescence[4, glyc_sample_cols1])

glyc_B2_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 1]))
glyc_B2_luminescence <- as.numeric(raw_luminescence[5, glyc_sample_cols1])


glyc_sample_cols2 <- c(4,5,6)
glyc_B3_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 4]))
glyc_B3_luminescence <- as.numeric(raw_luminescence[1, glyc_sample_cols2])

glyc_B4_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 4]))
glyc_B4_luminescence <- as.numeric(raw_luminescence[2, glyc_sample_cols2])

glyc_B5_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 4]))
glyc_B5_luminescence <- as.numeric(raw_luminescence[3, glyc_sample_cols2])

glyc_B6_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 4]))
glyc_B6_luminescence <- as.numeric(raw_luminescence[4, glyc_sample_cols2])

glyc_B7_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 4]))
glyc_B7_luminescence <- as.numeric(raw_luminescence[5, glyc_sample_cols2])
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
glyc_A6_mean_lum <- numeric(length(glyc_A6_dilution))
glyc_A6_se_lum <- numeric(length(glyc_A6_dilution))
glyc_A6_mean_conc <- numeric(length(glyc_A6_dilution))

for (i in 1:length(glyc_A6_dilution)) {
  df_val <- glyc_A6_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A6_lum_values <- c(glyc_A6_luminescence[glyc_A6_dilution == df_val])
  glyc_A6_mean_lum[i] <- mean(glyc_A6_lum_values)
  glyc_A6_se_lum[i] <- sd(glyc_A6_lum_values) / sqrt(length(glyc_A6_lum_values))
  # Calculate concentration from mean luminescence
  glyc_A6_mean_conc[i] <- (glyc_A6_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_A6_data <- data.frame(
  glyc_A6_dilution_factor = glyc_A6_dilution,
  glyc_A6_mean_luminescence = glyc_A6_mean_lum,
  glyc_A6_se = glyc_A6_se_lum,
  glyc_A6_conc =  glyc_A6_mean_conc,
  label = paste0("A6df.", glyc_A6_dilution)
)
glyc_A6_mean_lum
glyc_A6_mean_conc
```

    [1] 44760.33
    [1] 7.843816

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A7_mean_lum <- numeric(length(glyc_A7_dilution))
glyc_A7_se_lum <- numeric(length(glyc_A7_dilution))
glyc_A7_mean_conc <- numeric(length(glyc_A7_dilution))

for (i in 1:length(glyc_A7_dilution)) {
  df_val <- glyc_A7_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A7_lum_values <- c(glyc_A7_luminescence[glyc_A7_dilution == df_val])
  glyc_A7_mean_lum[i] <- mean(glyc_A7_lum_values)
  glyc_A7_se_lum[i] <- sd(glyc_A7_lum_values) / sqrt(length(glyc_A7_lum_values))
  # Calculate concentration from mean luminescence
  glyc_A7_mean_conc[i] <- (glyc_A7_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_A7_data <- data.frame(
  glyc_A7_dilution_factor = glyc_A7_dilution,
  glyc_A7_mean_luminescence = glyc_A7_mean_lum,
  glyc_A7_se = glyc_A7_se_lum,
  glyc_A7_conc =  glyc_A7_mean_conc,
  label = paste0("A7df.", glyc_A7_dilution)
)
glyc_A7_mean_lum
glyc_A7_mean_conc
```

    [1] 12494.67
    [1] 2.222713

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A8_mean_lum <- numeric(length(glyc_A8_dilution))
glyc_A8_se_lum <- numeric(length(glyc_A8_dilution))
glyc_A8_mean_conc <- numeric(length(glyc_A8_dilution))

for (i in 1:length(glyc_A8_dilution)) {
  df_val <- glyc_A8_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A8_lum_values <- c(glyc_A8_luminescence[glyc_A8_dilution == df_val])
  glyc_A8_mean_lum[i] <- mean(glyc_A8_lum_values)
  glyc_A8_se_lum[i] <- sd(glyc_A8_lum_values) / sqrt(length(glyc_A8_lum_values))
  # Calculate concentration from mean luminescence
  glyc_A8_mean_conc[i] <- (glyc_A8_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_A8_data <- data.frame(
  glyc_A8_dilution_factor = glyc_A8_dilution,
  glyc_A8_mean_luminescence = glyc_A8_mean_lum,
  glyc_A8_se = glyc_A8_se_lum,
  glyc_A8_conc =  glyc_A8_mean_conc,
  label = paste0("A8df.", glyc_A8_dilution)
)
glyc_A8_mean_lum
glyc_A8_mean_conc
```

    [1] 43034.67
    [1] 7.543182

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_B1_mean_lum <- numeric(length(glyc_B1_dilution))
glyc_B1_se_lum <- numeric(length(glyc_B1_dilution))
glyc_B1_mean_conc <- numeric(length(glyc_B1_dilution))

for (i in 1:length(glyc_B1_dilution)) {
  df_val <- glyc_B1_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_B1_lum_values <- c(glyc_B1_luminescence[glyc_B1_dilution == df_val])
  glyc_B1_mean_lum[i] <- mean(glyc_B1_lum_values)
  glyc_B1_se_lum[i] <- sd(glyc_B1_lum_values) / sqrt(length(glyc_B1_lum_values))
  # Calculate concentration from mean luminescence
  glyc_B1_mean_conc[i] <- (glyc_B1_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_B1_data <- data.frame(
  glyc_B1_dilution_factor = glyc_B1_dilution,
  glyc_B1_mean_luminescence = glyc_B1_mean_lum,
  glyc_B1_se = glyc_B1_se_lum,
  glyc_B1_conc =  glyc_B1_mean_conc,
  label = paste0("B1df.", glyc_B1_dilution)
)
glyc_B1_mean_lum
glyc_B1_mean_conc
```

    [1] 33789.67
    [1] 5.932582

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_B2_mean_lum <- numeric(length(glyc_B2_dilution))
glyc_B2_se_lum <- numeric(length(glyc_B2_dilution))
glyc_B2_mean_conc <- numeric(length(glyc_B2_dilution))

for (i in 1:length(glyc_B2_dilution)) {
  df_val <- glyc_B2_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_B2_lum_values <- c(glyc_B2_luminescence[glyc_B2_dilution == df_val])
  glyc_B2_mean_lum[i] <- mean(glyc_B2_lum_values)
  glyc_B2_se_lum[i] <- sd(glyc_B2_lum_values) / sqrt(length(glyc_B2_lum_values))
  # Calculate concentration from mean luminescence
  glyc_B2_mean_conc[i] <- (glyc_B2_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_B2_data <- data.frame(
  glyc_B2_dilution_factor = glyc_B2_dilution,
  glyc_B2_mean_luminescence = glyc_B2_mean_lum,
  glyc_B2_se = glyc_B2_se_lum,
  glyc_B2_conc =  glyc_B2_mean_conc,
  label = paste0("B2df.", glyc_B2_dilution)
)
glyc_B2_mean_lum
glyc_B2_mean_conc
```

    [1] 63779
    [1] 11.15712

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_B3_mean_lum <- numeric(length(glyc_B3_dilution))
glyc_B3_se_lum <- numeric(length(glyc_B3_dilution))
glyc_B3_mean_conc <- numeric(length(glyc_B3_dilution))

for (i in 1:length(glyc_B3_dilution)) {
  df_val <- glyc_B3_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_B3_lum_values <- c(glyc_B3_luminescence[glyc_B3_dilution == df_val])
  glyc_B3_mean_lum[i] <- mean(glyc_B3_lum_values)
  glyc_B3_se_lum[i] <- sd(glyc_B3_lum_values) / sqrt(length(glyc_B3_lum_values))
  # Calculate concentration from mean luminescence
  glyc_B3_mean_conc[i] <- (glyc_B3_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_B3_data <- data.frame(
  glyc_B3_dilution_factor = glyc_B3_dilution,
  glyc_B3_mean_luminescence = glyc_B3_mean_lum,
  glyc_B3_se = glyc_B3_se_lum,
  glyc_B3_conc =  glyc_B3_mean_conc,
  label = paste0("B3df.", glyc_B3_dilution)
)
glyc_B3_mean_lum
glyc_B3_mean_conc
```

    [1] 26800.67
    [1] 4.715006

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_B4_mean_lum <- numeric(length(glyc_B4_dilution))
glyc_B4_se_lum <- numeric(length(glyc_B4_dilution))
glyc_B4_mean_conc <- numeric(length(glyc_B4_dilution))

for (i in 1:length(glyc_B4_dilution)) {
  df_val <- glyc_B4_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_B4_lum_values <- c(glyc_B4_luminescence[glyc_B4_dilution == df_val])
  glyc_B4_mean_lum[i] <- mean(glyc_B4_lum_values)
  glyc_B4_se_lum[i] <- sd(glyc_B4_lum_values) / sqrt(length(glyc_B4_lum_values))
  # Calculate concentration from mean luminescence
  glyc_B4_mean_conc[i] <- (glyc_B4_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_B4_data <- data.frame(
  glyc_B4_dilution_factor = glyc_B4_dilution,
  glyc_B4_mean_luminescence = glyc_B4_mean_lum,
  glyc_B4_se = glyc_B4_se_lum,
  glyc_B4_conc =  glyc_B4_mean_conc,
  label = paste0("B4df.", glyc_B4_dilution)
)
glyc_B4_mean_lum
glyc_B4_mean_conc
```

    [1] 87868
    [1] 15.35374

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_B5_mean_lum <- numeric(length(glyc_B5_dilution))
glyc_B5_se_lum <- numeric(length(glyc_B5_dilution))
glyc_B5_mean_conc <- numeric(length(glyc_B5_dilution))

for (i in 1:length(glyc_B5_dilution)) {
  df_val <- glyc_B5_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_B5_lum_values <- c(glyc_B5_luminescence[glyc_B5_dilution == df_val])
  glyc_B5_mean_lum[i] <- mean(glyc_B5_lum_values)
  glyc_B5_se_lum[i] <- sd(glyc_B5_lum_values) / sqrt(length(glyc_B5_lum_values))
  # Calculate concentration from mean luminescence
  glyc_B5_mean_conc[i] <- (glyc_B5_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_B5_data <- data.frame(
  glyc_B5_dilution_factor = glyc_B5_dilution,
  glyc_B5_mean_luminescence = glyc_B5_mean_lum,
  glyc_B5_se = glyc_B5_se_lum,
  glyc_B5_conc =  glyc_B5_mean_conc,
  label = paste0("B5df.", glyc_B5_dilution)
)
glyc_B5_mean_lum
glyc_B5_mean_conc
```

    [1] 85188.67
    [1] 14.88696

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_B6_mean_lum <- numeric(length(glyc_B6_dilution))
glyc_B6_se_lum <- numeric(length(glyc_B6_dilution))
glyc_B6_mean_conc <- numeric(length(glyc_B6_dilution))

for (i in 1:length(glyc_B6_dilution)) {
  df_val <- glyc_B6_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_B6_lum_values <- c(glyc_B6_luminescence[glyc_B6_dilution == df_val])
  glyc_B6_mean_lum[i] <- mean(glyc_B6_lum_values)
  glyc_B6_se_lum[i] <- sd(glyc_B6_lum_values) / sqrt(length(glyc_B6_lum_values))
  # Calculate concentration from mean luminescence
  glyc_B6_mean_conc[i] <- (glyc_B6_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_B6_data <- data.frame(
  glyc_B6_dilution_factor = glyc_B6_dilution,
  glyc_B6_mean_luminescence = glyc_B6_mean_lum,
  glyc_B6_se = glyc_B6_se_lum,
  glyc_B6_conc =  glyc_B6_mean_conc,
  label = paste0("B6df.", glyc_B6_dilution)
)
glyc_B6_mean_lum
glyc_B6_mean_conc
```

    [1] 43945
    [1] 7.701774

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_B7_mean_lum <- numeric(length(glyc_B7_dilution))
glyc_B7_se_lum <- numeric(length(glyc_B7_dilution))
glyc_B7_mean_conc <- numeric(length(glyc_B7_dilution))

for (i in 1:length(glyc_B7_dilution)) {
  df_val <- glyc_B7_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_B7_lum_values <- c(glyc_B7_luminescence[glyc_B7_dilution == df_val])
  glyc_B7_mean_lum[i] <- mean(glyc_B7_lum_values)
  glyc_B7_se_lum[i] <- sd(glyc_B7_lum_values) / sqrt(length(glyc_B7_lum_values))
  # Calculate concentration from mean luminescence
  glyc_B7_mean_conc[i] <- (glyc_B7_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_B7_data <- data.frame(
  glyc_B7_dilution_factor = glyc_B7_dilution,
  glyc_B7_mean_luminescence = glyc_B7_mean_lum,
  glyc_B7_se = glyc_B7_se_lum,
  glyc_B7_conc =  glyc_B7_mean_conc,
  label = paste0("B7df.", glyc_B7_dilution)
)
glyc_B7_mean_lum
glyc_B7_mean_conc
```

    [1] 7633
    [1] 1.375746

### 1.1.4 Plot glycogen standard curve, sample points

``` r
# Create the plot
glyc_plot <- ggplot(glycogen_summary_data, aes(x = glyc_concentration, y = glyc_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glyc_mean_luminescence - glyc_se, ymax = glyc_mean_luminescence + glyc_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +
  
   geom_errorbar(data = glyc_A6_data, aes(x = glyc_A6_conc, y = glyc_A6_mean_luminescence,
                ymin = glyc_A6_mean_luminescence - glyc_A6_se, ymax = glyc_A6_mean_luminescence + glyc_A6_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A6_data, aes(x = glyc_A6_conc, y = glyc_A6_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_A7_data, aes(x = glyc_A7_conc, y = glyc_A7_mean_luminescence,
                ymin = glyc_A7_mean_luminescence - glyc_A7_se, ymax = glyc_A7_mean_luminescence + glyc_A7_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A7_data, aes(x = glyc_A7_conc, y = glyc_A7_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_A8_data, aes(x = glyc_A8_conc, y = glyc_A8_mean_luminescence,
                ymin = glyc_A8_mean_luminescence - glyc_A8_se, ymax = glyc_A8_mean_luminescence + glyc_A8_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A8_data, aes(x = glyc_A8_conc, y = glyc_A8_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_B1_data, aes(x = glyc_B1_conc, y = glyc_B1_mean_luminescence,
                ymin = glyc_B1_mean_luminescence - glyc_B1_se, ymax = glyc_B1_mean_luminescence + glyc_B1_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_B1_data, aes(x = glyc_B1_conc, y = glyc_B1_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_B2_data, aes(x = glyc_B2_conc, y = glyc_B2_mean_luminescence,
                ymin = glyc_B2_mean_luminescence - glyc_B2_se, ymax = glyc_B2_mean_luminescence + glyc_B2_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_B2_data, aes(x = glyc_B2_conc, y = glyc_B2_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_B3_data, aes(x = glyc_B3_conc, y = glyc_B3_mean_luminescence,
                ymin = glyc_B3_mean_luminescence - glyc_B3_se, ymax = glyc_B3_mean_luminescence + glyc_B3_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_B3_data, aes(x = glyc_B3_conc, y = glyc_B3_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_B4_data, aes(x = glyc_B4_conc, y = glyc_B4_mean_luminescence,
                ymin = glyc_B4_mean_luminescence - glyc_B4_se, ymax = glyc_B4_mean_luminescence + glyc_B4_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_B4_data, aes(x = glyc_B4_conc, y = glyc_B4_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_B5_data, aes(x = glyc_B5_conc, y = glyc_B5_mean_luminescence,
                ymin = glyc_B5_mean_luminescence - glyc_B5_se, ymax = glyc_B5_mean_luminescence + glyc_B5_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_B5_data, aes(x = glyc_B5_conc, y = glyc_B5_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_B6_data, aes(x = glyc_B6_conc, y = glyc_B6_mean_luminescence,
                ymin = glyc_B6_mean_luminescence - glyc_B6_se, ymax = glyc_B6_mean_luminescence + glyc_B6_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_B6_data, aes(x = glyc_B6_conc, y = glyc_B6_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_B7_data, aes(x = glyc_B7_conc, y = glyc_B7_mean_luminescence,
                ymin = glyc_B7_mean_luminescence - glyc_B7_se, ymax = glyc_B7_mean_luminescence + glyc_B7_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_B7_data, aes(x = glyc_B7_conc, y = glyc_B7_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                                "A6df.20" = "darkred",
                                "A7df.20" = "darkorange",
                                "A8df.20" = "darkgreen",
                                "B1df.20" = "purple",
                                "B2df.20" = "brown",
                                "B3df.20" = "green3",
                                "B4df.20" = "firebrick1",
                                "B5df.20" = "cyan",
                                "B6df.20" = "yellow3",
                                "B7df.20" = "thistle3"
                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                 "A6df.20" = 17,
                                 "A7df.20" = 15,
                                 "A8df.20" = 18,
                                 "B1df.20" = 8,
                                 "B2df.20" = 4,
                                 "B3df.20" = 5,
                                 "B4df.20" = 0,
                                 "B5df.20" = 2,
                                 "B6df.20" = 19,
                                 "B7df.20" = 20
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

![](Gen5-20260204-mgig-glycogenglo_files/figure-gfm/plot%20glycogen%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glyc_A6_data
glyc_A7_data
glyc_A8_data
glyc_B1_data
glyc_B2_data
glyc_B3_data
glyc_B4_data
glyc_B5_data
glyc_B6_data
glyc_B7_data

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

      glyc_A6_dilution_factor glyc_A6_mean_luminescence glyc_A6_se glyc_A6_conc
    1                      20                  44760.33   315.2361     7.843816
        label
    1 A6df.20
      glyc_A7_dilution_factor glyc_A7_mean_luminescence glyc_A7_se glyc_A7_conc
    1                      20                  12494.67   643.0154     2.222713
        label
    1 A7df.20
      glyc_A8_dilution_factor glyc_A8_mean_luminescence glyc_A8_se glyc_A8_conc
    1                      20                  43034.67    741.899     7.543182
        label
    1 A8df.20
      glyc_B1_dilution_factor glyc_B1_mean_luminescence glyc_B1_se glyc_B1_conc
    1                      20                  33789.67   395.1895     5.932582
        label
    1 B1df.20
      glyc_B2_dilution_factor glyc_B2_mean_luminescence glyc_B2_se glyc_B2_conc
    1                      20                     63779   1239.057     11.15712
        label
    1 B2df.20
      glyc_B3_dilution_factor glyc_B3_mean_luminescence glyc_B3_se glyc_B3_conc
    1                      20                  26800.67   3455.881     4.715006
        label
    1 B3df.20
      glyc_B4_dilution_factor glyc_B4_mean_luminescence glyc_B4_se glyc_B4_conc
    1                      20                     87868   1350.865     15.35374
        label
    1 B4df.20
      glyc_B5_dilution_factor glyc_B5_mean_luminescence glyc_B5_se glyc_B5_conc
    1                      20                  85188.67   1627.387     14.88696
        label
    1 B5df.20
      glyc_B6_dilution_factor glyc_B6_mean_luminescence glyc_B6_se glyc_B6_conc
    1                      20                     43945   1416.536     7.701774
        label
    1 B6df.20
      glyc_B7_dilution_factor glyc_B7_mean_luminescence glyc_B7_se glyc_B7_conc
    1                      20                      7633    1673.57     1.375746
        label
    1 B7df.20
    glycogen Standard Curve Summary:
    ==================================================
    Concentration: 20 µg/µL
      Mean Luminescence: 114864.00
      Standard Error: 285.22
      CV%: 0.43%

    Concentration: 2 µg/µL
      Mean Luminescence: 7889.00
      Standard Error: 887.29
      CV%: 19.48%

    Concentration: 0.2 µg/µL
      Mean Luminescence: 1440.67
      Standard Error: 34.36
      CV%: 4.13%

    Concentration: 0.02 µg/µL
      Mean Luminescence: 993.67
      Standard Error: 18.62
      CV%: 3.25%

    Concentration: 0 µg/µL
      Mean Luminescence: 1038.00
      Standard Error: 110.39
      CV%: 18.42%

### 1.1.5 Glycogen summary table

``` r
tab <- matrix(c(glyc_A6_dilution, glyc_A6_mean_lum, glyc_A6_mean_conc,  (glyc_A6_dilution*glyc_A6_mean_conc), 
                glyc_A7_dilution, glyc_A7_mean_lum, glyc_A7_mean_conc,  (glyc_A7_dilution*glyc_A7_mean_conc), 
                glyc_A8_dilution, glyc_A8_mean_lum, glyc_A8_mean_conc,  (glyc_A8_dilution*glyc_A8_mean_conc), 
                glyc_B1_dilution, glyc_B1_mean_lum, glyc_B1_mean_conc,  (glyc_B1_dilution*glyc_B1_mean_conc), 
                glyc_B2_dilution, glyc_B2_mean_lum, glyc_B2_mean_conc,  (glyc_B2_dilution*glyc_B2_mean_conc), 
                glyc_B3_dilution, glyc_B3_mean_lum, glyc_B3_mean_conc,  (glyc_B3_dilution*glyc_B3_mean_conc), 
                glyc_B4_dilution, glyc_B4_mean_lum, glyc_B4_mean_conc,  (glyc_B4_dilution*glyc_B4_mean_conc), 
                glyc_B5_dilution, glyc_B5_mean_lum, glyc_B5_mean_conc,  (glyc_B5_dilution*glyc_B5_mean_conc), 
                glyc_B6_dilution, glyc_B6_mean_lum, glyc_B6_mean_conc,  (glyc_B6_dilution*glyc_B6_mean_conc), 
                glyc_B7_dilution, glyc_B7_mean_lum, glyc_B7_mean_conc,  (glyc_B7_dilution*glyc_B7_mean_conc)), ncol=4, byrow=TRUE)
colnames(tab) <- c('Dilution factor','Luminescence','Calculated Glycogen (ug/uL)', 'Total glycogen (ug/uL)')
rownames(tab) <- c('A6','A7','A8','B1','B2','B3','B4','B5','B6','B7' )
tab <- as.table(tab)
tab
```

       Dilution factor Luminescence Calculated Glycogen (ug/uL)
    A6       20.000000 44760.333333                    7.843816
    A7       20.000000 12494.666667                    2.222713
    A8       20.000000 43034.666667                    7.543182
    B1       20.000000 33789.666667                    5.932582
    B2       20.000000 63779.000000                   11.157118
    B3       20.000000 26800.666667                    4.715006
    B4       20.000000 87868.000000                   15.353739
    B5       20.000000 85188.666667                   14.886963
    B6       20.000000 43945.000000                    7.701774
    B7       20.000000  7633.000000                    1.375746
       Total glycogen (ug/uL)
    A6             156.876322
    A7              44.454253
    A8             150.863646
    B1             118.651634
    B2             223.142359
    B3              94.300120
    B4             307.074772
    B5             297.739269
    B6             154.035486
    B7              27.514927

## 1.2 Glucose Standard Curve

``` r
# Extract glycogen standard curve data from plate layout and raw luminescence.
# Rows F, G, H (rows 6, 7, 8) correspond to glycogen standards
# Columns 1-5 contain the standard curve concentrations

# Extract concentration values from plate layout (row 6, columns 1-5)
# Parse concentration from labels like "STD-glyc-20" -> 20
glu_concentrations <- as.numeric(gsub("STD-glu-", "", plate_layout[6, 7:11]))
glu_concentrations

# Extract standard curve luminescence values for each replicate from raw_luminescence
glu_row_F <- as.numeric(raw_luminescence[6, 7:11])  # Row 6 (F)
glu_row_G <- as.numeric(raw_luminescence[7, 7:11])  # Row 7 (G)
glu_row_H <- as.numeric(raw_luminescence[8, 7:11])  # Row 8 (H)
```

    [1] 100.0  10.0   1.0   0.1   0.0

``` r
#Extract glucose sample data - wells A6-E6
glu_sample_cols1 <- c(7,8,9)
glu_A6_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 1]))
glu_A6_luminescence <- as.numeric(raw_luminescence[1, glu_sample_cols1])

glu_A7_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 1]))
glu_A7_luminescence <- as.numeric(raw_luminescence[2, glu_sample_cols1])

glu_A8_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 1]))
glu_A8_luminescence <- as.numeric(raw_luminescence[3, glu_sample_cols1])

glu_B1_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 1]))
glu_B1_luminescence <- as.numeric(raw_luminescence[4, glu_sample_cols1])

glu_B2_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 1]))
glu_B2_luminescence <- as.numeric(raw_luminescence[5, glu_sample_cols1])


glu_sample_cols2 <- c(10,11,12)
glu_B3_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 4]))
glu_B3_luminescence <- as.numeric(raw_luminescence[1, glu_sample_cols2])

glu_B4_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 4]))
glu_B4_luminescence <- as.numeric(raw_luminescence[2, glu_sample_cols2])

glu_B5_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 4]))
glu_B5_luminescence <- as.numeric(raw_luminescence[3, glu_sample_cols2])

glu_B6_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 4]))
glu_B6_luminescence <- as.numeric(raw_luminescence[4, glu_sample_cols2])

glu_B7_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 4]))
glu_B7_luminescence <- as.numeric(raw_luminescence[5, glu_sample_cols2])
```

### 1.2.1 Glucose standard curve summary statistics and linear regression

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

### 1.2.2 Calculate sample glucose levels

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A6_mean_lum <- numeric(length(glu_A6_dilution))
glu_A6_se_lum <- numeric(length(glu_A6_dilution))
glu_A6_mean_conc <- numeric(length(glu_A6_dilution))

for (i in 1:length(glu_A6_dilution)) {
  df_val <- glu_A6_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A6_lum_values <- c(glu_A6_luminescence[glu_A6_dilution == df_val])
  glu_A6_mean_lum[i] <- mean(glu_A6_lum_values)
  glu_A6_se_lum[i] <- sd(glu_A6_lum_values) / sqrt(length(glu_A6_lum_values))
  # Calculate concentration from mean luminescence
  glu_A6_mean_conc[i] <- (glu_A6_mean_lum[i] - glu_intercept) / glu_slope
}

glu_A6_data <- data.frame(
  glu_A6_dilution_factor = glu_A6_dilution,
  glu_A6_mean_luminescence = glu_A6_mean_lum,
  glu_A6_se = glu_A6_se_lum,
  glu_A6_conc =  glu_A6_mean_conc,
  label = paste0("A6df.", glu_A6_dilution)
)
glu_A6_mean_lum
glu_A6_mean_conc
```

    [1] 7276.667
    [1] 3.623261

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A7_mean_lum <- numeric(length(glu_A7_dilution))
glu_A7_se_lum <- numeric(length(glu_A7_dilution))
glu_A7_mean_conc <- numeric(length(glu_A7_dilution))

for (i in 1:length(glu_A7_dilution)) {
  df_val <- glu_A7_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A7_lum_values <- c(glu_A7_luminescence[glu_A7_dilution == df_val])
  glu_A7_mean_lum[i] <- mean(glu_A7_lum_values)
  glu_A7_se_lum[i] <- sd(glu_A7_lum_values) / sqrt(length(glu_A7_lum_values))
  # Calculate concentration from mean luminescence
  glu_A7_mean_conc[i] <- (glu_A7_mean_lum[i] - glu_intercept) / glu_slope
}

glu_A7_data <- data.frame(
  glu_A7_dilution_factor = glu_A7_dilution,
  glu_A7_mean_luminescence = glu_A7_mean_lum,
  glu_A7_se = glu_A7_se_lum,
  glu_A7_conc =  glu_A7_mean_conc,
  label = paste0("A7df.", glu_A7_dilution)
)
glu_A7_mean_lum
glu_A7_mean_conc
```

    [1] 9241
    [1] 5.230713

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A8_mean_lum <- numeric(length(glu_A8_dilution))
glu_A8_se_lum <- numeric(length(glu_A8_dilution))
glu_A8_mean_conc <- numeric(length(glu_A8_dilution))

for (i in 1:length(glu_A8_dilution)) {
  df_val <- glu_A8_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A8_lum_values <- c(glu_A8_luminescence[glu_A8_dilution == df_val])
  glu_A8_mean_lum[i] <- mean(glu_A8_lum_values)
  glu_A8_se_lum[i] <- sd(glu_A8_lum_values) / sqrt(length(glu_A8_lum_values))
  # Calculate concentration from mean luminescence
  glu_A8_mean_conc[i] <- (glu_A8_mean_lum[i] - glu_intercept) / glu_slope
}

glu_A8_data <- data.frame(
  glu_A8_dilution_factor = glu_A8_dilution,
  glu_A8_mean_luminescence = glu_A8_mean_lum,
  glu_A8_se = glu_A8_se_lum,
  glu_A8_conc =  glu_A8_mean_conc,
  label = paste0("A8df.", glu_A8_dilution)
)
glu_A8_mean_lum
glu_A8_mean_conc
```

    [1] 3971.333
    [1] 0.9184437

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_B1_mean_lum <- numeric(length(glu_B1_dilution))
glu_B1_se_lum <- numeric(length(glu_B1_dilution))
glu_B1_mean_conc <- numeric(length(glu_B1_dilution))

for (i in 1:length(glu_B1_dilution)) {
  df_val <- glu_B1_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_B1_lum_values <- c(glu_B1_luminescence[glu_B1_dilution == df_val])
  glu_B1_mean_lum[i] <- mean(glu_B1_lum_values)
  glu_B1_se_lum[i] <- sd(glu_B1_lum_values) / sqrt(length(glu_B1_lum_values))
  # Calculate concentration from mean luminescence
  glu_B1_mean_conc[i] <- (glu_B1_mean_lum[i] - glu_intercept) / glu_slope
}

glu_B1_data <- data.frame(
  glu_B1_dilution_factor = glu_B1_dilution,
  glu_B1_mean_luminescence = glu_B1_mean_lum,
  glu_B1_se = glu_B1_se_lum,
  glu_B1_conc =  glu_B1_mean_conc,
  label = paste0("B1df.", glu_B1_dilution)
)
glu_B1_mean_lum
glu_B1_mean_conc
```

    [1] 5695
    [1] 2.328953

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_B2_mean_lum <- numeric(length(glu_B2_dilution))
glu_B2_se_lum <- numeric(length(glu_B2_dilution))
glu_B2_mean_conc <- numeric(length(glu_B2_dilution))

for (i in 1:length(glu_B2_dilution)) {
  df_val <- glu_B2_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_B2_lum_values <- c(glu_B2_luminescence[glu_B2_dilution == df_val])
  glu_B2_mean_lum[i] <- mean(glu_B2_lum_values)
  glu_B2_se_lum[i] <- sd(glu_B2_lum_values) / sqrt(length(glu_B2_lum_values))
  # Calculate concentration from mean luminescence
  glu_B2_mean_conc[i] <- (glu_B2_mean_lum[i] - glu_intercept) / glu_slope
}

glu_B2_data <- data.frame(
  glu_B2_dilution_factor = glu_B2_dilution,
  glu_B2_mean_luminescence = glu_B2_mean_lum,
  glu_B2_se = glu_B2_se_lum,
  glu_B2_conc =  glu_B2_mean_conc,
  label = paste0("B2df.", glu_B2_dilution)
)
glu_B2_mean_lum
glu_B2_mean_conc
```

    [1] 5491.333
    [1] 2.162289

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_B3_mean_lum <- numeric(length(glu_B3_dilution))
glu_B3_se_lum <- numeric(length(glu_B3_dilution))
glu_B3_mean_conc <- numeric(length(glu_B3_dilution))

for (i in 1:length(glu_B3_dilution)) {
  df_val <- glu_B3_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_B3_lum_values <- c(glu_B3_luminescence[glu_B3_dilution == df_val])
  glu_B3_mean_lum[i] <- mean(glu_B3_lum_values)
  glu_B3_se_lum[i] <- sd(glu_B3_lum_values) / sqrt(length(glu_B3_lum_values))
  # Calculate concentration from mean luminescence
  glu_B3_mean_conc[i] <- (glu_B3_mean_lum[i] - glu_intercept) / glu_slope
}

glu_B3_data <- data.frame(
  glu_B3_dilution_factor = glu_B3_dilution,
  glu_B3_mean_luminescence = glu_B3_mean_lum,
  glu_B3_se = glu_B3_se_lum,
  glu_B3_conc =  glu_B3_mean_conc,
  label = paste0("B3df.", glu_B3_dilution)
)
glu_B3_mean_lum
glu_B3_mean_conc
```

    [1] 2780.667
    [1] -0.05590157

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_B4_mean_lum <- numeric(length(glu_B4_dilution))
glu_B4_se_lum <- numeric(length(glu_B4_dilution))
glu_B4_mean_conc <- numeric(length(glu_B4_dilution))

for (i in 1:length(glu_B4_dilution)) {
  df_val <- glu_B4_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_B4_lum_values <- c(glu_B4_luminescence[glu_B4_dilution == df_val])
  glu_B4_mean_lum[i] <- mean(glu_B4_lum_values)
  glu_B4_se_lum[i] <- sd(glu_B4_lum_values) / sqrt(length(glu_B4_lum_values))
  # Calculate concentration from mean luminescence
  glu_B4_mean_conc[i] <- (glu_B4_mean_lum[i] - glu_intercept) / glu_slope
}

glu_B4_data <- data.frame(
  glu_B4_dilution_factor = glu_B4_dilution,
  glu_B4_mean_luminescence = glu_B4_mean_lum,
  glu_B4_se = glu_B4_se_lum,
  glu_B4_conc =  glu_B4_mean_conc,
  label = paste0("B4df.", glu_B4_dilution)
)
glu_B4_mean_lum
glu_B4_mean_conc
```

    [1] 1274.667
    [1] -1.28829

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_B5_mean_lum <- numeric(length(glu_B5_dilution))
glu_B5_se_lum <- numeric(length(glu_B5_dilution))
glu_B5_mean_conc <- numeric(length(glu_B5_dilution))

for (i in 1:length(glu_B5_dilution)) {
  df_val <- glu_B5_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_B5_lum_values <- c(glu_B5_luminescence[glu_B5_dilution == df_val])
  glu_B5_mean_lum[i] <- mean(glu_B5_lum_values)
  glu_B5_se_lum[i] <- sd(glu_B5_lum_values) / sqrt(length(glu_B5_lum_values))
  # Calculate concentration from mean luminescence
  glu_B5_mean_conc[i] <- (glu_B5_mean_lum[i] - glu_intercept) / glu_slope
}

glu_B5_data <- data.frame(
  glu_B5_dilution_factor = glu_B5_dilution,
  glu_B5_mean_luminescence = glu_B5_mean_lum,
  glu_B5_se = glu_B5_se_lum,
  glu_B5_conc =  glu_B5_mean_conc,
  label = paste0("B5df.", glu_B5_dilution)
)
glu_B5_mean_lum
glu_B5_mean_conc
```

    [1] 2007
    [1] -0.6890078

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_B6_mean_lum <- numeric(length(glu_B6_dilution))
glu_B6_se_lum <- numeric(length(glu_B6_dilution))
glu_B6_mean_conc <- numeric(length(glu_B6_dilution))

for (i in 1:length(glu_B6_dilution)) {
  df_val <- glu_B6_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_B6_lum_values <- c(glu_B6_luminescence[glu_B6_dilution == df_val])
  glu_B6_mean_lum[i] <- mean(glu_B6_lum_values)
  glu_B6_se_lum[i] <- sd(glu_B6_lum_values) / sqrt(length(glu_B6_lum_values))
  # Calculate concentration from mean luminescence
  glu_B6_mean_conc[i] <- (glu_B6_mean_lum[i] - glu_intercept) / glu_slope
}

glu_B6_data <- data.frame(
  glu_B6_dilution_factor = glu_B6_dilution,
  glu_B6_mean_luminescence = glu_B6_mean_lum,
  glu_B6_se = glu_B6_se_lum,
  glu_B6_conc =  glu_B6_mean_conc,
  label = paste0("B6df.", glu_B6_dilution)
)
glu_B6_mean_lum
glu_B6_mean_conc
```

    [1] 5714.667
    [1] 2.345047

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_B7_mean_lum <- numeric(length(glu_B7_dilution))
glu_B7_se_lum <- numeric(length(glu_B7_dilution))
glu_B7_mean_conc <- numeric(length(glu_B7_dilution))

for (i in 1:length(glu_B7_dilution)) {
  df_val <- glu_B7_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_B7_lum_values <- c(glu_B7_luminescence[glu_B7_dilution == df_val])
  glu_B7_mean_lum[i] <- mean(glu_B7_lum_values)
  glu_B7_se_lum[i] <- sd(glu_B7_lum_values) / sqrt(length(glu_B7_lum_values))
  # Calculate concentration from mean luminescence
  glu_B7_mean_conc[i] <- (glu_B7_mean_lum[i] - glu_intercept) / glu_slope
}

glu_B7_data <- data.frame(
  glu_B7_dilution_factor = glu_B7_dilution,
  glu_B7_mean_luminescence = glu_B7_mean_lum,
  glu_B7_se = glu_B7_se_lum,
  glu_B7_conc =  glu_B7_mean_conc,
  label = paste0("B7df.", glu_B7_dilution)
)
glu_B7_mean_lum
glu_B7_mean_conc
```

    [1] 5246.333
    [1] 1.961801

### 1.2.3 Plot glucose standard curve, sample points

``` r
# Create the plot
glu_plot <- ggplot(glucose_summary_data, aes(x = glu_concentration, y = glu_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glu_mean_luminescence - glu_se, ymax = glu_mean_luminescence + glu_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +
  
   geom_errorbar(data = glu_A6_data, aes(x = glu_A6_conc, y = glu_A6_mean_luminescence,
                ymin = glu_A6_mean_luminescence - glu_A6_se, ymax = glu_A6_mean_luminescence + glu_A6_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A6_data, aes(x = glu_A6_conc, y = glu_A6_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_A7_data, aes(x = glu_A7_conc, y = glu_A7_mean_luminescence,
                ymin = glu_A7_mean_luminescence - glu_A7_se, ymax = glu_A7_mean_luminescence + glu_A7_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A7_data, aes(x = glu_A7_conc, y = glu_A7_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_A8_data, aes(x = glu_A8_conc, y = glu_A8_mean_luminescence,
                ymin = glu_A8_mean_luminescence - glu_A8_se, ymax = glu_A8_mean_luminescence + glu_A8_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A8_data, aes(x = glu_A8_conc, y = glu_A8_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_B1_data, aes(x = glu_B1_conc, y = glu_B1_mean_luminescence,
                ymin = glu_B1_mean_luminescence - glu_B1_se, ymax = glu_B1_mean_luminescence + glu_B1_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_B1_data, aes(x = glu_B1_conc, y = glu_B1_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_B2_data, aes(x = glu_B2_conc, y = glu_B2_mean_luminescence,
                ymin = glu_B2_mean_luminescence - glu_B2_se, ymax = glu_B2_mean_luminescence + glu_B2_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_B2_data, aes(x = glu_B2_conc, y = glu_B2_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_B3_data, aes(x = glu_B3_conc, y = glu_B3_mean_luminescence,
                ymin = glu_B3_mean_luminescence - glu_B3_se, ymax = glu_B3_mean_luminescence + glu_B3_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_B3_data, aes(x = glu_B3_conc, y = glu_B3_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_B4_data, aes(x = glu_B4_conc, y = glu_B4_mean_luminescence,
                ymin = glu_B4_mean_luminescence - glu_B4_se, ymax = glu_B4_mean_luminescence + glu_B4_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_B4_data, aes(x = glu_B4_conc, y = glu_B4_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_B5_data, aes(x = glu_B5_conc, y = glu_B5_mean_luminescence,
                ymin = glu_B5_mean_luminescence - glu_B5_se, ymax = glu_B5_mean_luminescence + glu_B5_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_B5_data, aes(x = glu_B5_conc, y = glu_B5_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_B6_data, aes(x = glu_B6_conc, y = glu_B6_mean_luminescence,
                ymin = glu_B6_mean_luminescence - glu_B6_se, ymax = glu_B6_mean_luminescence + glu_B6_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_B6_data, aes(x = glu_B6_conc, y = glu_B6_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_B7_data, aes(x = glu_B7_conc, y = glu_B7_mean_luminescence,
                ymin = glu_B7_mean_luminescence - glu_B7_se, ymax = glu_B7_mean_luminescence + glu_B7_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_B7_data, aes(x = glu_B7_conc, y = glu_B7_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                                "A6df.20" = "darkred",
                                "A7df.20" = "darkorange",
                                "A8df.20" = "darkgreen",
                                "B1df.20" = "purple",
                                "B2df.20" = "brown",
                                "B3df.20" = "green3",
                                "B4df.20" = "firebrick1",
                                "B5df.20" = "cyan",
                                "B6df.20" = "yellow3",
                                "B7df.20" = "thistle3"
                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                 "A6df.20" = 17,
                                 "A7df.20" = 15,
                                 "A8df.20" = 18,
                                 "B1df.20" = 8,
                                 "B2df.20" = 4,
                                 "B3df.20" = 5,
                                 "B4df.20" = 0,
                                 "B5df.20" = 2,
                                 "B6df.20" = 19,
                                 "B7df.20" = 20
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

![](Gen5-20260204-mgig-glycogenglo_files/figure-gfm/plot%20glucose%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glu_A6_data
glu_A7_data
glu_A8_data
glu_B1_data
glu_B2_data
glu_B3_data
glu_B4_data
glu_B5_data
glu_B6_data
glu_B7_data

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

      glu_A6_dilution_factor glu_A6_mean_luminescence glu_A6_se glu_A6_conc   label
    1                     20                 7276.667  1515.618    3.623261 A6df.20
      glu_A7_dilution_factor glu_A7_mean_luminescence glu_A7_se glu_A7_conc   label
    1                     20                     9241  2374.835    5.230713 A7df.20
      glu_A8_dilution_factor glu_A8_mean_luminescence glu_A8_se glu_A8_conc   label
    1                     20                 3971.333  1735.036   0.9184437 A8df.20
      glu_B1_dilution_factor glu_B1_mean_luminescence glu_B1_se glu_B1_conc   label
    1                     20                     5695  1505.781    2.328953 B1df.20
      glu_B2_dilution_factor glu_B2_mean_luminescence glu_B2_se glu_B2_conc   label
    1                     20                 5491.333  1278.844    2.162289 B2df.20
      glu_B3_dilution_factor glu_B3_mean_luminescence glu_B3_se glu_B3_conc   label
    1                     20                 2780.667  1156.923 -0.05590157 B3df.20
      glu_B4_dilution_factor glu_B4_mean_luminescence glu_B4_se glu_B4_conc   label
    1                     20                 1274.667  397.8711    -1.28829 B4df.20
      glu_B5_dilution_factor glu_B5_mean_luminescence glu_B5_se glu_B5_conc   label
    1                     20                     2007  1103.357  -0.6890078 B5df.20
      glu_B6_dilution_factor glu_B6_mean_luminescence glu_B6_se glu_B6_conc   label
    1                     20                 5714.667  3007.482    2.345047 B6df.20
      glu_B7_dilution_factor glu_B7_mean_luminescence glu_B7_se glu_B7_conc   label
    1                     20                 5246.333  1359.649    1.961801 B7df.20
    glucose Standard Curve Summary:
    ==================================================
    Concentration: 100 µg/µL
      Mean Luminescence: 125153.00
      Standard Error: 669.86
      CV%: 0.93%

    Concentration: 10 µg/µL
      Mean Luminescence: 13973.00
      Standard Error: 781.44
      CV%: 9.69%

    Concentration: 1 µg/µL
      Mean Luminescence: 4620.33
      Standard Error: 2574.29
      CV%: 96.50%

    Concentration: 0.1 µg/µL
      Mean Luminescence: 4785.33
      Standard Error: 2363.02
      CV%: 85.53%

    Concentration: 0 µg/µL
      Mean Luminescence: 1479.33
      Standard Error: 916.25
      CV%: 107.28%

### 1.2.4 Glucose summary table

``` r
tab <- matrix(c(glu_A6_dilution, glu_A6_mean_lum, glu_A6_mean_conc,  (glu_A6_dilution*glu_A6_mean_conc), 
                glu_A7_dilution, glu_A7_mean_lum, glu_A7_mean_conc,  (glu_A7_dilution*glu_A7_mean_conc), 
                glu_A8_dilution, glu_A8_mean_lum, glu_A8_mean_conc,  (glu_A8_dilution*glu_A8_mean_conc), 
                glu_B1_dilution, glu_B1_mean_lum, glu_B1_mean_conc,  (glu_B1_dilution*glu_B1_mean_conc), 
                glu_B2_dilution, glu_B2_mean_lum, glu_B2_mean_conc,  (glu_B2_dilution*glu_B2_mean_conc), 
                glu_B3_dilution, glu_B3_mean_lum, glu_B3_mean_conc,  (glu_B3_dilution*glu_B3_mean_conc), 
                glu_B4_dilution, glu_B4_mean_lum, glu_B4_mean_conc,  (glu_B4_dilution*glu_B4_mean_conc), 
                glu_B5_dilution, glu_B5_mean_lum, glu_B5_mean_conc,  (glu_B5_dilution*glu_B5_mean_conc), 
                glu_B6_dilution, glu_B6_mean_lum, glu_B6_mean_conc,  (glu_B6_dilution*glu_B6_mean_conc), 
                glu_B7_dilution, glu_B7_mean_lum, glu_B7_mean_conc,  (glu_B7_dilution*glu_B7_mean_conc)), ncol=4, byrow=TRUE)
colnames(tab) <- c('Dilution factor','Luminescence','Calculated glucose (ug/uL)', 'Total glucose (ug/uL)')
rownames(tab) <- c('A6','A7','A8','B1','B2','B3','B4','B5','B6','B7' )
tab <- as.table(tab)
tab
```

       Dilution factor  Luminescence Calculated glucose (ug/uL)
    A6     20.00000000 7276.66666667                 3.62326125
    A7     20.00000000 9241.00000000                 5.23071280
    A8     20.00000000 3971.33333333                 0.91844374
    B1     20.00000000 5695.00000000                 2.32895315
    B2     20.00000000 5491.33333333                 2.16228882
    B3     20.00000000 2780.66666667                -0.05590157
    B4     20.00000000 1274.66666667                -1.28829019
    B5     20.00000000 2007.00000000                -0.68900781
    B6     20.00000000 5714.66666667                 2.34504676
    B7     20.00000000 5246.33333333                 1.96180064
       Total glucose (ug/uL)
    A6           72.46522502
    A7          104.61425605
    A8           18.36887483
    B1           46.57906309
    B2           43.24577649
    B3           -1.11803148
    B4          -25.76580378
    B5          -13.78015620
    B6           46.90093529
    B7           39.23601273

# 2 Overall summary table

Note: the ‘glycogen’ readout includes background glucose - the
‘Glycogen-Glucose’ number is a more accurate measurement of glycogen
levels, as this removes the contribution of background glucose

``` r
tab <- matrix(c(as.numeric(weights[6,2]), glyc_A6_dilution, glyc_A6_mean_conc, glu_A6_mean_conc, (glyc_A6_mean_conc-glu_A6_mean_conc), ((glyc_A6_mean_conc-glu_A6_mean_conc)*(glyc_A6_dilution/as.numeric(weights[6,2]))), 
                as.numeric(weights[7,2]), glyc_A7_dilution, glyc_A7_mean_conc, glu_A7_mean_conc, (glyc_A7_mean_conc-glu_A7_mean_conc), ((glyc_A7_mean_conc-glu_A7_mean_conc)*(glyc_A7_dilution/as.numeric(weights[7,2]))),
                as.numeric(weights[8,2]), glyc_A8_dilution, glyc_A8_mean_conc, glu_A8_mean_conc, (glyc_A8_mean_conc-glu_A8_mean_conc), ((glyc_A8_mean_conc-glu_A8_mean_conc)*(glyc_A8_dilution/as.numeric(weights[8,2]))), 
                as.numeric(weights[9,2]), glyc_B1_dilution, glyc_B1_mean_conc, glu_B1_mean_conc, (glyc_B1_mean_conc-glu_B1_mean_conc), ((glyc_B1_mean_conc-glu_B1_mean_conc)*(glyc_B1_dilution/as.numeric(weights[9,2]))),
                as.numeric(weights[10,2]), glyc_B2_dilution, glyc_B2_mean_conc, glu_B2_mean_conc, (glyc_B2_mean_conc-glu_B2_mean_conc), ((glyc_B2_mean_conc-glu_B2_mean_conc)*(glyc_B2_dilution/as.numeric(weights[10,2]))),
                as.numeric(weights[11,2]), glyc_B3_dilution, glyc_B3_mean_conc, glu_B3_mean_conc, (glyc_B3_mean_conc-glu_B3_mean_conc), ((glyc_B3_mean_conc-glu_B3_mean_conc)*(glyc_B3_dilution/as.numeric(weights[11,2]))), 
                as.numeric(weights[12,2]), glyc_B4_dilution, glyc_B4_mean_conc, glu_B4_mean_conc, (glyc_B4_mean_conc-glu_B4_mean_conc), ((glyc_B4_mean_conc-glu_B4_mean_conc)*(glyc_B4_dilution/as.numeric(weights[12,2]))),
                as.numeric(weights[13,2]), glyc_B5_dilution, glyc_B5_mean_conc, glu_B5_mean_conc, (glyc_B5_mean_conc-glu_B5_mean_conc), ((glyc_B5_mean_conc-glu_B5_mean_conc)*(glyc_B5_dilution/as.numeric(weights[13,2]))), 
                as.numeric(weights[14,2]), glyc_B6_dilution, glyc_B6_mean_conc, glu_B6_mean_conc, (glyc_B6_mean_conc-glu_B6_mean_conc), ((glyc_B6_mean_conc-glu_B6_mean_conc)*(glyc_B6_dilution/as.numeric(weights[14,2]))),
                as.numeric(weights[15,2]), glyc_B7_dilution, glyc_B7_mean_conc, glu_B7_mean_conc, (glyc_B7_mean_conc-glu_B7_mean_conc), ((glyc_B7_mean_conc-glu_B7_mean_conc)*(glyc_B7_dilution/as.numeric(weights[15,2])))
                ), ncol=6, byrow=TRUE)

colnames(tab) <- c('Weight (mg)', 'df', 'Glycogen (ug/uL)', 'Glucose (uM)', 'Glycogen-Glucose (ug/uL)', '(Glycogen-Glucose)*df/weight (ug/uL/mg)')
rownames(tab) <- c('A6','A7','A8','B1','B2','B3','B4','B5','B6','B7')
tab <- as.table(tab)
tab
```

       Weight (mg)          df Glycogen (ug/uL) Glucose (uM)
    A6 25.00000000 20.00000000       7.84381611   3.62326125
    A7 17.00000000 20.00000000       2.22271263   5.23071280
    A8 12.00000000 20.00000000       7.54318228   0.91844374
    B1 20.00000000 20.00000000       5.93258169   2.32895315
    B2 31.00000000 20.00000000      11.15711797   2.16228882
    B3 15.00000000 20.00000000       4.71500598  -0.05590157
    B4 28.00000000 20.00000000      15.35373858  -1.28829019
    B5 27.00000000 20.00000000      14.88696347  -0.68900781
    B6 35.00000000 20.00000000       7.70177432   2.34504676
    B7 17.00000000 20.00000000       1.37574636   1.96180064
       Glycogen-Glucose (ug/uL) (Glycogen-Glucose)*df/weight (ug/uL/mg)
    A6               4.22055486                              3.37644389
    A7              -3.00800017                             -3.53882373
    A8               6.62473854                             11.04123090
    B1               3.60362854                              3.60362854
    B2               8.99482914                              5.80311558
    B3               4.77090755                              6.36121007
    B4              16.64202877                             11.88716340
    B5              15.57597128                             11.53775651
    B6               5.35672756                              3.06098717
    B7              -0.58605428                             -0.68947562
