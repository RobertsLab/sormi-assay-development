Gen5-20260123-mgig-glycogenglo
================
Hazel Abrahamson-Amerine
2026-02-27

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
plate_layout <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-Gen5-20260123-mgig-glycogenglo.csv", header = FALSE)
raw_luminescence <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-Gen5-20260123-mgig-glycogenglo.csv", header = FALSE)

cat("Plate layout:\n")
str(plate_layout)

cat("\n\n")

cat("Raw luminescence:\n")
str(raw_luminescence)
```

    Plate layout:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : chr  "A1-glyc-25-df.2" "A2-glyc-50-df.2" "A3-glyc-14-df.2" "A4-glyc-21-df.2" ...
     $ V2 : chr  "A1-glyc-25-df.2" "A2-glyc-50-df.2" "A3-glyc-14-df.2" "A4-glyc-21-df.2" ...
     $ V3 : chr  "A1-glyc-25-df.2" "A2-glyc-50-df.2" "A3-glyc-14-df.2" "A4-glyc-21-df.2" ...
     $ V4 : chr  "A6-glyc-25-df.2" "A7-glyc-17-df.2" "A8-glyc-12-df.2" "B1-glyc-20-df.2" ...
     $ V5 : chr  "A6-glyc-25-df.2" "A7-glyc-17-df.2" "A8-glyc-12-df.2" "B1-glyc-20-df.2" ...
     $ V6 : chr  "A6-glyc-25-df.2" "A7-glyc-17-df.2" "A8-glyc-12-df.2" "B1-glyc-20-df.2" ...
     $ V7 : chr  "A1-glu-25-df.2" "A2-glu-50-df.2" "A3-glu-14-df.2" "A4-glu-21-df.2" ...
     $ V8 : chr  "A1-glu-25-df.2" "A2-glu-50-df.2" "A3-glu-14-df.2" "A4-glu-21-df.2" ...
     $ V9 : chr  "A1-glu-25-df.2" "A2-glu-50-df.2" "A3-glu-14-df.2" "A4-glu-21-df.2" ...
     $ V10: chr  "A6-glu-25-df.2" "A7-glu-17-df.2" "A8-glu-12-df.2" "B1-glu-20-df.2" ...
     $ V11: chr  "A6-glu-25-df.2" "A7-glu-17-df.2" "A8-glu-12-df.2" "B1-glu-20-df.2" ...
     $ V12: chr  "A6-glu-25-df.2" "A7-glu-17-df.2" "A8-glu-12-df.2" "B1-glu-20-df.2" ...


    Raw luminescence:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : int  138127 140900 137665 143464 145071 116109 119520 114570
     $ V2 : int  140563 146591 147974 140186 126417 3115 2801 3570
     $ V3 : int  142305 136007 138454 152745 134643 496 454 436
     $ V4 : int  141042 134446 145883 145204 134213 310 317 270
     $ V5 : int  153140 135683 125187 145351 146408 395 337 588
     $ V6 : int  150731 134544 154292 145177 155515 NA NA NA
     $ V7 : int  1673 510 304 277 799 124306 131464 124353
     $ V8 : int  1726 588 324 182 774 5387 3813 3932
     $ V9 : int  1736 4137 314 195 580 311 364 684
     $ V10: int  2394 362 430 385 376 154 178 613
     $ V11: int  2316 294 396 386 407 145 142 1449
     $ V12: int  2475 495 427 356 425 NA NA NA

# 1 STANDARD CURVES

## 1.1 Glycogen Standard Curve

### 1.1.1 Extract luminescence data

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
#Extract glycogen sample data - wells A1-E6
glyc_sample_cols1 <- c(1,2,3)
glyc_A1_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 1]))
glyc_A1_luminescence <- as.numeric(raw_luminescence[1, glyc_sample_cols1])

glyc_A2_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 1]))
glyc_A2_luminescence <- as.numeric(raw_luminescence[2, glyc_sample_cols1])

glyc_A3_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 1]))
glyc_A3_luminescence <- as.numeric(raw_luminescence[3, glyc_sample_cols1])

glyc_A4_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 1]))
glyc_A4_luminescence <- as.numeric(raw_luminescence[4, glyc_sample_cols1])

glyc_A5_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 1]))
glyc_A5_luminescence <- as.numeric(raw_luminescence[5, glyc_sample_cols1])


glyc_sample_cols2 <- c(4,5,6)
glyc_A6_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 4]))
glyc_A6_luminescence <- as.numeric(raw_luminescence[1, glyc_sample_cols2])

glyc_A7_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 4]))
glyc_A7_luminescence <- as.numeric(raw_luminescence[2, glyc_sample_cols2])

glyc_A8_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 4]))
glyc_A8_luminescence <- as.numeric(raw_luminescence[3, glyc_sample_cols2])

glyc_B1_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 4]))
glyc_B1_luminescence <- as.numeric(raw_luminescence[4, glyc_sample_cols2])

glyc_B2_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 4]))
glyc_B2_luminescence <- as.numeric(raw_luminescence[5, glyc_sample_cols2])
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
glyc_A1_mean_lum <- numeric(length(glyc_A1_dilution))
glyc_A1_se_lum <- numeric(length(glyc_A1_dilution))
glyc_A1_mean_conc <- numeric(length(glyc_A1_dilution))

for (i in 1:length(glyc_A1_dilution)) {
  df_val <- glyc_A1_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A1_lum_values <- c(glyc_A1_luminescence[glyc_A1_dilution == df_val])
  glyc_A1_mean_lum[i] <- mean(glyc_A1_lum_values)
  glyc_A1_se_lum[i] <- sd(glyc_A1_lum_values) / sqrt(length(glyc_A1_lum_values))
  # Calculate concentration from mean luminescence
  glyc_A1_mean_conc[i] <- (glyc_A1_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_A1_data <- data.frame(
  glyc_A1_dilution_factor = glyc_A1_dilution,
  glyc_A1_mean_luminescence = glyc_A1_mean_lum,
  glyc_A1_se = glyc_A1_se_lum,
  glyc_A1_conc =  glyc_A1_mean_conc,
  label = paste0("A1df.", glyc_A1_dilution)
)
glyc_A1_mean_lum
glyc_A1_mean_conc
```

    [1] 140331.7
    [1] 24.1056

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A2_mean_lum <- numeric(length(glyc_A2_dilution))
glyc_A2_se_lum <- numeric(length(glyc_A2_dilution))
glyc_A2_mean_conc <- numeric(length(glyc_A2_dilution))

for (i in 1:length(glyc_A2_dilution)) {
  df_val <- glyc_A2_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A2_lum_values <- c(glyc_A2_luminescence[glyc_A2_dilution == df_val])
  glyc_A2_mean_lum[i] <- mean(glyc_A2_lum_values)
  glyc_A2_se_lum[i] <- sd(glyc_A2_lum_values) / sqrt(length(glyc_A2_lum_values))
  # Calculate concentration from mean luminescence
  glyc_A2_mean_conc[i] <- (glyc_A2_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_A2_data <- data.frame(
  glyc_A2_dilution_factor = glyc_A2_dilution,
  glyc_A2_mean_luminescence = glyc_A2_mean_lum,
  glyc_A2_se = glyc_A2_se_lum,
  glyc_A2_conc =  glyc_A2_mean_conc,
  label = paste0("A2df.", glyc_A2_dilution)
)
glyc_A2_mean_lum
glyc_A2_mean_conc
```

    [1] 141166
    [1] 24.24688

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A3_mean_lum <- numeric(length(glyc_A3_dilution))
glyc_A3_se_lum <- numeric(length(glyc_A3_dilution))
glyc_A3_mean_conc <- numeric(length(glyc_A3_dilution))

for (i in 1:length(glyc_A3_dilution)) {
  df_val <- glyc_A3_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A3_lum_values <- c(glyc_A3_luminescence[glyc_A3_dilution == df_val])
  glyc_A3_mean_lum[i] <- mean(glyc_A3_lum_values)
  glyc_A3_se_lum[i] <- sd(glyc_A3_lum_values) / sqrt(length(glyc_A3_lum_values))
  # Calculate concentration from mean luminescence
  glyc_A3_mean_conc[i] <- (glyc_A3_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_A3_data <- data.frame(
  glyc_A3_dilution_factor = glyc_A3_dilution,
  glyc_A3_mean_luminescence = glyc_A3_mean_lum,
  glyc_A3_se = glyc_A3_se_lum,
  glyc_A3_conc =  glyc_A3_mean_conc,
  label = paste0("A3df.", glyc_A3_dilution)
)
glyc_A3_mean_lum
glyc_A3_mean_conc
```

    [1] 141364.3
    [1] 24.28046

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A4_mean_lum <- numeric(length(glyc_A4_dilution))
glyc_A4_se_lum <- numeric(length(glyc_A4_dilution))
glyc_A4_mean_conc <- numeric(length(glyc_A4_dilution))

for (i in 1:length(glyc_A4_dilution)) {
  df_val <- glyc_A4_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A4_lum_values <- c(glyc_A4_luminescence[glyc_A4_dilution == df_val])
  glyc_A4_mean_lum[i] <- mean(glyc_A4_lum_values)
  glyc_A4_se_lum[i] <- sd(glyc_A4_lum_values) / sqrt(length(glyc_A4_lum_values))
  # Calculate concentration from mean luminescence
  glyc_A4_mean_conc[i] <- (glyc_A4_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_A4_data <- data.frame(
  glyc_A4_dilution_factor = glyc_A4_dilution,
  glyc_A4_mean_luminescence = glyc_A4_mean_lum,
  glyc_A4_se = glyc_A4_se_lum,
  glyc_A4_conc =  glyc_A4_mean_conc,
  label = paste0("A4df.", glyc_A4_dilution)
)
glyc_A4_mean_lum
glyc_A4_mean_conc
```

    [1] 145465
    [1] 24.97484

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A5_mean_lum <- numeric(length(glyc_A5_dilution))
glyc_A5_se_lum <- numeric(length(glyc_A5_dilution))
glyc_A5_mean_conc <- numeric(length(glyc_A5_dilution))

for (i in 1:length(glyc_A5_dilution)) {
  df_val <- glyc_A5_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A5_lum_values <- c(glyc_A5_luminescence[glyc_A5_dilution == df_val])
  glyc_A5_mean_lum[i] <- mean(glyc_A5_lum_values)
  glyc_A5_se_lum[i] <- sd(glyc_A5_lum_values) / sqrt(length(glyc_A5_lum_values))
  # Calculate concentration from mean luminescence
  glyc_A5_mean_conc[i] <- (glyc_A5_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_A5_data <- data.frame(
  glyc_A5_dilution_factor = glyc_A5_dilution,
  glyc_A5_mean_luminescence = glyc_A5_mean_lum,
  glyc_A5_se = glyc_A5_se_lum,
  glyc_A5_conc =  glyc_A5_mean_conc,
  label = paste0("A5df.", glyc_A5_dilution)
)
glyc_A5_mean_lum
glyc_A5_mean_conc
```

    [1] 135377
    [1] 23.26661

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

    [1] 148304.3
    [1] 25.45563

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

    [1] 134891
    [1] 23.18432

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

    [1] 141787.3
    [1] 24.35209

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

    [1] 145244
    [1] 24.93742

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

    [1] 145378.7
    [1] 24.96022

``` r
# Create the plot
glyc_plot <- ggplot(glycogen_summary_data, aes(x = glyc_concentration, y = glyc_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glyc_mean_luminescence - glyc_se, ymax = glyc_mean_luminescence + glyc_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +
  
  geom_errorbar(data = glyc_A1_data, aes(x = glyc_A1_conc, y = glyc_A1_mean_luminescence,
                ymin = glyc_A1_mean_luminescence - glyc_A1_se, ymax = glyc_A1_mean_luminescence + glyc_A1_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A1_data, aes(x = glyc_A1_conc, y = glyc_A1_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_A2_data, aes(x = glyc_A2_conc, y = glyc_A2_mean_luminescence,
                ymin = glyc_A2_mean_luminescence - glyc_A2_se, ymax = glyc_A2_mean_luminescence + glyc_A2_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A2_data, aes(x = glyc_A2_conc, y = glyc_A2_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_A3_data, aes(x = glyc_A3_conc, y = glyc_A3_mean_luminescence,
                ymin = glyc_A3_mean_luminescence - glyc_A3_se, ymax = glyc_A3_mean_luminescence + glyc_A3_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A3_data, aes(x = glyc_A3_conc, y = glyc_A3_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_A4_data, aes(x = glyc_A4_conc, y = glyc_A4_mean_luminescence,
                ymin = glyc_A4_mean_luminescence - glyc_A4_se, ymax = glyc_A4_mean_luminescence + glyc_A4_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A4_data, aes(x = glyc_A4_conc, y = glyc_A4_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_A5_data, aes(x = glyc_A5_conc, y = glyc_A5_mean_luminescence,
                ymin = glyc_A5_mean_luminescence - glyc_A5_se, ymax = glyc_A5_mean_luminescence + glyc_A5_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A5_data, aes(x = glyc_A5_conc, y = glyc_A5_mean_luminescence,
             color = label, shape = label), size = 4) +
  
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

  
  
  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                                "A1df.2" = "darkred",
                                "A2df.2" = "darkorange",
                                "A3df.2" = "darkgreen",
                                "A4df.2" = "purple",
                                "A5df.2" = "brown",
                                "A6df.2" = "green3",
                                "A7df.2" = "firebrick1",
                                "A8df.2" = "cyan",
                                "B1df.2" = "yellow3",
                                "B2df.2" = "thistle3"
                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                 "A1df.2" = 17,
                                 "A2df.2" = 15,
                                 "A3df.2" = 18,
                                 "A4df.2" = 8,
                                 "A5df.2" = 4,
                                 "A6df.2" = 5,
                                 "A7df.2" = 0,
                                 "A8df.2" = 2,
                                 "B1df.2" = 19,
                                 "B2df.2" = 20
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

![](Gen5-20260123-mgig-glycogenglo_files/figure-gfm/plot%20glycogen%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glyc_A1_data
glyc_A2_data
glyc_A3_data
glyc_A4_data
glyc_A5_data
glyc_A6_data
glyc_A7_data
glyc_A8_data
glyc_B1_data
glyc_B2_data



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

      glyc_A1_dilution_factor glyc_A1_mean_luminescence glyc_A1_se glyc_A1_conc
    1                       2                  140331.7   1211.618      24.1056
       label
    1 A1df.2
      glyc_A2_dilution_factor glyc_A2_mean_luminescence glyc_A2_se glyc_A2_conc
    1                       2                    141166   3058.231     24.24688
       label
    1 A2df.2
      glyc_A3_dilution_factor glyc_A3_mean_luminescence glyc_A3_se glyc_A3_conc
    1                       2                  141364.3   3312.673     24.28046
       label
    1 A3df.2
      glyc_A4_dilution_factor glyc_A4_mean_luminescence glyc_A4_se glyc_A4_conc
    1                       2                    145465   3760.989     24.97484
       label
    1 A4df.2
      glyc_A5_dilution_factor glyc_A5_mean_luminescence glyc_A5_se glyc_A5_conc
    1                       2                    135377   5397.438     23.26661
       label
    1 A5df.2
      glyc_A6_dilution_factor glyc_A6_mean_luminescence glyc_A6_se glyc_A6_conc
    1                       2                  148304.3   3697.158     25.45563
       label
    1 A6df.2
      glyc_A7_dilution_factor glyc_A7_mean_luminescence glyc_A7_se glyc_A7_conc
    1                       2                    134891   397.0092     23.18432
       label
    1 A7df.2
      glyc_A8_dilution_factor glyc_A8_mean_luminescence glyc_A8_se glyc_A8_conc
    1                       2                  141787.3   8647.854     24.35209
       label
    1 A8df.2
      glyc_B1_dilution_factor glyc_B1_mean_luminescence glyc_B1_se glyc_B1_conc
    1                       2                    145244   54.06478     24.93742
       label
    1 B1df.2
      glyc_B2_dilution_factor glyc_B2_mean_luminescence glyc_B2_se glyc_B2_conc
    1                       2                  145378.7   6170.857     24.96022
       label
    1 B2df.2
    glycogen Standard Curve Summary:
    ==================================================
    Concentration: 20 µg/µL
      Mean Luminescence: 116733.00
      Standard Error: 1462.61
      CV%: 2.17%

    Concentration: 2 µg/µL
      Mean Luminescence: 3162.00
      Standard Error: 223.23
      CV%: 12.23%

    Concentration: 0.2 µg/µL
      Mean Luminescence: 462.00
      Standard Error: 17.78
      CV%: 6.66%

    Concentration: 0.02 µg/µL
      Mean Luminescence: 299.00
      Standard Error: 14.64
      CV%: 8.48%

    Concentration: 0 µg/µL
      Mean Luminescence: 440.00
      Standard Error: 75.87
      CV%: 29.87%

``` r
tab <- matrix(c(glyc_A1_dilution, glyc_A1_mean_lum, glyc_A1_mean_conc,  (glyc_A1_dilution*glyc_A1_mean_conc), 
                glyc_A2_dilution, glyc_A2_mean_lum, glyc_A2_mean_conc,  (glyc_A2_dilution*glyc_A2_mean_conc), 
                glyc_A3_dilution, glyc_A3_mean_lum, glyc_A3_mean_conc,  (glyc_A3_dilution*glyc_A3_mean_conc), 
                glyc_A4_dilution, glyc_A4_mean_lum, glyc_A4_mean_conc,  (glyc_A4_dilution*glyc_A4_mean_conc), 
                glyc_A5_dilution, glyc_A5_mean_lum, glyc_A5_mean_conc,  (glyc_A5_dilution*glyc_A5_mean_conc), 
                glyc_A6_dilution, glyc_A6_mean_lum, glyc_A6_mean_conc,  (glyc_A6_dilution*glyc_A6_mean_conc), 
                glyc_A7_dilution, glyc_A7_mean_lum, glyc_A7_mean_conc,  (glyc_A7_dilution*glyc_A7_mean_conc), 
                glyc_A8_dilution, glyc_A8_mean_lum, glyc_A8_mean_conc,  (glyc_A8_dilution*glyc_A8_mean_conc), 
                glyc_B1_dilution, glyc_B1_mean_lum, glyc_B1_mean_conc,  (glyc_B1_dilution*glyc_B1_mean_conc), 
                glyc_B2_dilution, glyc_B2_mean_lum, glyc_B2_mean_conc,  (glyc_B2_dilution*glyc_B2_mean_conc)), ncol=4, byrow=TRUE)
colnames(tab) <- c('Dilution factor','Luminescence','Calculated Glycogen (ug/uL)', 'Total glycogen (ug/uL)')
rownames(tab) <- c('A1','A2','A3','A4','A5','A6','A7','A8','B1','B2' )
tab <- as.table(tab)
tab

print("All samples have calculated glycogen > 20 ug/uL, and are therefore out of the standard curve range")
```

       Dilution factor Luminescence Calculated Glycogen (ug/uL)
    A1         2.00000 140331.66667                    24.10560
    A2         2.00000 141166.00000                    24.24688
    A3         2.00000 141364.33333                    24.28046
    A4         2.00000 145465.00000                    24.97484
    A5         2.00000 135377.00000                    23.26661
    A6         2.00000 148304.33333                    25.45563
    A7         2.00000 134891.00000                    23.18432
    A8         2.00000 141787.33333                    24.35209
    B1         2.00000 145244.00000                    24.93742
    B2         2.00000 145378.66667                    24.96022
       Total glycogen (ug/uL)
    A1               48.21120
    A2               48.49376
    A3               48.56093
    A4               49.94968
    A5               46.53323
    A6               50.91126
    A7               46.36864
    A8               48.70418
    B1               49.87483
    B2               49.92044
    [1] "All samples have calculated glycogen > 20 ug/uL, and are therefore out of the standard curve range"

# 2 STANDARD CURVES

## 2.1 Glucose Standard Curve

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
#Extract glucose sample data - wells A1-E6
glu_sample_cols1 <- c(7,8,9)
glu_A1_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 7]))
glu_A1_luminescence <- as.numeric(raw_luminescence[1, glu_sample_cols1])

glu_A2_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 7]))
glu_A2_luminescence <- as.numeric(raw_luminescence[2, glu_sample_cols1])

glu_A3_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 7]))
glu_A3_luminescence <- as.numeric(raw_luminescence[3, glu_sample_cols1])

glu_A4_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 7]))
glu_A4_luminescence <- as.numeric(raw_luminescence[4, glu_sample_cols1])

glu_A5_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 7]))
glu_A5_luminescence <- as.numeric(raw_luminescence[5, glu_sample_cols1])


glu_sample_cols2 <- c(10,11,12)
glu_A6_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 10]))
glu_A6_luminescence <- as.numeric(raw_luminescence[1, glu_sample_cols2])

glu_A7_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 10]))
glu_A7_luminescence <- as.numeric(raw_luminescence[2, glu_sample_cols2])

glu_A8_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 10]))
glu_A8_luminescence <- as.numeric(raw_luminescence[3, glu_sample_cols2])

glu_B1_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 10]))
glu_B1_luminescence <- as.numeric(raw_luminescence[4, glu_sample_cols2])

glu_B2_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 10]))
glu_B2_luminescence <- as.numeric(raw_luminescence[5, glu_sample_cols2])
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
glu_A1_mean_lum <- numeric(length(glu_A1_dilution))
glu_A1_se_lum <- numeric(length(glu_A1_dilution))
glu_A1_mean_conc <- numeric(length(glu_A1_dilution))

for (i in 1:length(glu_A1_dilution)) {
  df_val <- glu_A1_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A1_lum_values <- c(glu_A1_luminescence[glu_A1_dilution == df_val])
  glu_A1_mean_lum[i] <- mean(glu_A1_lum_values)
  glu_A1_se_lum[i] <- sd(glu_A1_lum_values) / sqrt(length(glu_A1_lum_values))
  # Calculate concentration from mean luminescence
  glu_A1_mean_conc[i] <- (glu_A1_mean_lum[i] - glu_intercept) / glu_slope
}

glu_A1_data <- data.frame(
  glu_A1_dilution_factor = glu_A1_dilution,
  glu_A1_mean_luminescence = glu_A1_mean_lum,
  glu_A1_se = glu_A1_se_lum,
  glu_A1_conc =  glu_A1_mean_conc,
  label = paste0("A1df.", glu_A1_dilution)
)
glu_A1_mean_lum
glu_A1_mean_conc
```

    [1] 1711.667
    [1] 2.870658

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A2_mean_lum <- numeric(length(glu_A2_dilution))
glu_A2_se_lum <- numeric(length(glu_A2_dilution))
glu_A2_mean_conc <- numeric(length(glu_A2_dilution))

for (i in 1:length(glu_A2_dilution)) {
  df_val <- glu_A2_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A2_lum_values <- c(glu_A2_luminescence[glu_A2_dilution == df_val])
  glu_A2_mean_lum[i] <- mean(glu_A2_lum_values)
  glu_A2_se_lum[i] <- sd(glu_A2_lum_values) / sqrt(length(glu_A2_lum_values))
  # Calculate concentration from mean luminescence
  glu_A2_mean_conc[i] <- (glu_A2_mean_lum[i] - glu_intercept) / glu_slope
}

glu_A2_data <- data.frame(
  glu_A2_dilution_factor = glu_A2_dilution,
  glu_A2_mean_luminescence = glu_A2_mean_lum,
  glu_A2_se = glu_A2_se_lum,
  glu_A2_conc =  glu_A2_mean_conc,
  label = paste0("A2df.", glu_A2_dilution)
)
glu_A2_mean_lum
glu_A2_mean_conc
```

    [1] 1745
    [1] 2.896692

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A3_mean_lum <- numeric(length(glu_A3_dilution))
glu_A3_se_lum <- numeric(length(glu_A3_dilution))
glu_A3_mean_conc <- numeric(length(glu_A3_dilution))

for (i in 1:length(glu_A3_dilution)) {
  df_val <- glu_A3_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A3_lum_values <- c(glu_A3_luminescence[glu_A3_dilution == df_val])
  glu_A3_mean_lum[i] <- mean(glu_A3_lum_values)
  glu_A3_se_lum[i] <- sd(glu_A3_lum_values) / sqrt(length(glu_A3_lum_values))
  # Calculate concentration from mean luminescence
  glu_A3_mean_conc[i] <- (glu_A3_mean_lum[i] - glu_intercept) / glu_slope
}

glu_A3_data <- data.frame(
  glu_A3_dilution_factor = glu_A3_dilution,
  glu_A3_mean_luminescence = glu_A3_mean_lum,
  glu_A3_se = glu_A3_se_lum,
  glu_A3_conc =  glu_A3_mean_conc,
  label = paste0("A3df.", glu_A3_dilution)
)
glu_A3_mean_lum
glu_A3_mean_conc
```

    [1] 314
    [1] 1.779062

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A4_mean_lum <- numeric(length(glu_A4_dilution))
glu_A4_se_lum <- numeric(length(glu_A4_dilution))
glu_A4_mean_conc <- numeric(length(glu_A4_dilution))

for (i in 1:length(glu_A4_dilution)) {
  df_val <- glu_A4_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A4_lum_values <- c(glu_A4_luminescence[glu_A4_dilution == df_val])
  glu_A4_mean_lum[i] <- mean(glu_A4_lum_values)
  glu_A4_se_lum[i] <- sd(glu_A4_lum_values) / sqrt(length(glu_A4_lum_values))
  # Calculate concentration from mean luminescence
  glu_A4_mean_conc[i] <- (glu_A4_mean_lum[i] - glu_intercept) / glu_slope
}

glu_A4_data <- data.frame(
  glu_A4_dilution_factor = glu_A4_dilution,
  glu_A4_mean_luminescence = glu_A4_mean_lum,
  glu_A4_se = glu_A4_se_lum,
  glu_A4_conc =  glu_A4_mean_conc,
  label = paste0("A4df.", glu_A4_dilution)
)
glu_A4_mean_lum
glu_A4_mean_conc
```

    [1] 218
    [1] 1.704084

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A5_mean_lum <- numeric(length(glu_A5_dilution))
glu_A5_se_lum <- numeric(length(glu_A5_dilution))
glu_A5_mean_conc <- numeric(length(glu_A5_dilution))

for (i in 1:length(glu_A5_dilution)) {
  df_val <- glu_A5_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A5_lum_values <- c(glu_A5_luminescence[glu_A5_dilution == df_val])
  glu_A5_mean_lum[i] <- mean(glu_A5_lum_values)
  glu_A5_se_lum[i] <- sd(glu_A5_lum_values) / sqrt(length(glu_A5_lum_values))
  # Calculate concentration from mean luminescence
  glu_A5_mean_conc[i] <- (glu_A5_mean_lum[i] - glu_intercept) / glu_slope
}

glu_A5_data <- data.frame(
  glu_A5_dilution_factor = glu_A5_dilution,
  glu_A5_mean_luminescence = glu_A5_mean_lum,
  glu_A5_se = glu_A5_se_lum,
  glu_A5_conc =  glu_A5_mean_conc,
  label = paste0("A5df.", glu_A5_dilution)
)
glu_A5_mean_lum
glu_A5_mean_conc
```

    [1] 717.6667
    [1] 2.094331

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

    [1] 2395
    [1] 3.40435

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

    [1] 383.6667
    [1] 1.833472

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

    [1] 417.6667
    [1] 1.860027

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

    [1] 375.6667
    [1] 1.827224

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

    [1] 402.6667
    [1] 1.848312

``` r
# Create the plot
glu_plot <- ggplot(glucose_summary_data, aes(x = glu_concentration, y = glu_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glu_mean_luminescence - glu_se, ymax = glu_mean_luminescence + glu_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +
  
  geom_errorbar(data = glu_A1_data, aes(x = glu_A1_conc, y = glu_A1_mean_luminescence,
                ymin = glu_A1_mean_luminescence - glu_A1_se, ymax = glu_A1_mean_luminescence + glu_A1_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A1_data, aes(x = glu_A1_conc, y = glu_A1_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_A2_data, aes(x = glu_A2_conc, y = glu_A2_mean_luminescence,
                ymin = glu_A2_mean_luminescence - glu_A2_se, ymax = glu_A2_mean_luminescence + glu_A2_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A2_data, aes(x = glu_A2_conc, y = glu_A2_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_A3_data, aes(x = glu_A3_conc, y = glu_A3_mean_luminescence,
                ymin = glu_A3_mean_luminescence - glu_A3_se, ymax = glu_A3_mean_luminescence + glu_A3_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A3_data, aes(x = glu_A3_conc, y = glu_A3_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_A4_data, aes(x = glu_A4_conc, y = glu_A4_mean_luminescence,
                ymin = glu_A4_mean_luminescence - glu_A4_se, ymax = glu_A4_mean_luminescence + glu_A4_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A4_data, aes(x = glu_A4_conc, y = glu_A4_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_A5_data, aes(x = glu_A5_conc, y = glu_A5_mean_luminescence,
                ymin = glu_A5_mean_luminescence - glu_A5_se, ymax = glu_A5_mean_luminescence + glu_A5_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A5_data, aes(x = glu_A5_conc, y = glu_A5_mean_luminescence,
             color = label, shape = label), size = 4) +
  
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

  
  
  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                                "A1df.2" = "darkred",
                                "A2df.2" = "darkorange",
                                "A3df.2" = "darkgreen",
                                "A4df.2" = "purple",
                                "A5df.2" = "brown",
                                "A6df.2" = "green3",
                                "A7df.2" = "firebrick1",
                                "A8df.2" = "cyan",
                                "B1df.2" = "yellow3",
                                "B2df.2" = "thistle3"
                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                 "A1df.2" = 17,
                                 "A2df.2" = 15,
                                 "A3df.2" = 18,
                                 "A4df.2" = 8,
                                 "A5df.2" = 4,
                                 "A6df.2" = 5,
                                 "A7df.2" = 0,
                                 "A8df.2" = 2,
                                 "B1df.2" = 19,
                                 "B2df.2" = 20
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

![](Gen5-20260123-mgig-glycogenglo_files/figure-gfm/plot%20glucose%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glu_A1_data
glu_A2_data
glu_A3_data
glu_A4_data
glu_A5_data
glu_A6_data
glu_A7_data
glu_A8_data
glu_B1_data
glu_B2_data

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

      glu_A1_dilution_factor glu_A1_mean_luminescence glu_A1_se glu_A1_conc  label
    1                      2                 1711.667  19.54766    2.870658 A1df.2
      glu_A2_dilution_factor glu_A2_mean_luminescence glu_A2_se glu_A2_conc  label
    1                      2                     1745  1196.212    2.896692 A2df.2
      glu_A3_dilution_factor glu_A3_mean_luminescence glu_A3_se glu_A3_conc  label
    1                      2                      314  5.773503    1.779062 A3df.2
      glu_A4_dilution_factor glu_A4_mean_luminescence glu_A4_se glu_A4_conc  label
    1                      2                      218  29.73774    1.704084 A4df.2
      glu_A5_dilution_factor glu_A5_mean_luminescence glu_A5_se glu_A5_conc  label
    1                      2                 717.6667  69.21063    2.094331 A5df.2
      glu_A6_dilution_factor glu_A6_mean_luminescence glu_A6_se glu_A6_conc  label
    1                      2                     2395  45.90207     3.40435 A6df.2
      glu_A7_dilution_factor glu_A7_mean_luminescence glu_A7_se glu_A7_conc  label
    1                      2                 383.6667  59.02636    1.833472 A7df.2
      glu_A8_dilution_factor glu_A8_mean_luminescence glu_A8_se glu_A8_conc  label
    1                      2                 417.6667  10.86789    1.860027 A8df.2
      glu_B1_dilution_factor glu_B1_mean_luminescence glu_B1_se glu_B1_conc  label
    1                      2                 375.6667   9.83757    1.827224 B1df.2
      glu_B2_dilution_factor glu_B2_mean_luminescence glu_B2_se glu_B2_conc  label
    1                      2                 402.6667  14.31006    1.848312 B2df.2
    glucose Standard Curve Summary:
    ==================================================
    Concentration: 100 µg/µL
      Mean Luminescence: 126707.67
      Standard Error: 2378.21
      CV%: 3.25%

    Concentration: 10 µg/µL
      Mean Luminescence: 4377.33
      Standard Error: 506.00
      CV%: 20.02%

    Concentration: 1 µg/µL
      Mean Luminescence: 453.00
      Standard Error: 116.51
      CV%: 44.55%

    Concentration: 0.1 µg/µL
      Mean Luminescence: 315.00
      Standard Error: 149.16
      CV%: 82.02%

    Concentration: 0 µg/µL
      Mean Luminescence: 578.67
      Standard Error: 435.17
      CV%: 130.25%

``` r
tab <- matrix(c(glu_A1_dilution, glu_A1_mean_lum, glu_A1_mean_conc,  (glu_A1_dilution*glu_A1_mean_conc), 
                glu_A2_dilution, glu_A2_mean_lum, glu_A2_mean_conc,  (glu_A2_dilution*glu_A2_mean_conc), 
                glu_A3_dilution, glu_A3_mean_lum, glu_A3_mean_conc,  (glu_A3_dilution*glu_A3_mean_conc), 
                glu_A4_dilution, glu_A4_mean_lum, glu_A4_mean_conc,  (glu_A4_dilution*glu_A4_mean_conc), 
                glu_A5_dilution, glu_A5_mean_lum, glu_A5_mean_conc,  (glu_A5_dilution*glu_A5_mean_conc), 
                glu_A6_dilution, glu_A6_mean_lum, glu_A6_mean_conc,  (glu_A6_dilution*glu_A6_mean_conc), 
                glu_A7_dilution, glu_A7_mean_lum, glu_A7_mean_conc,  (glu_A7_dilution*glu_A7_mean_conc), 
                glu_A8_dilution, glu_A8_mean_lum, glu_A8_mean_conc,  (glu_A8_dilution*glu_A8_mean_conc), 
                glu_B1_dilution, glu_B1_mean_lum, glu_B1_mean_conc,  (glu_B1_dilution*glu_B1_mean_conc), 
                glu_B2_dilution, glu_B2_mean_lum, glu_B2_mean_conc,  (glu_B2_dilution*glu_B2_mean_conc)), ncol=4, byrow=TRUE)
colnames(tab) <- c('Dilution factor','Luminescence','Calculated glucose (uM)', 'Total glucose (uM)')
rownames(tab) <- c('A1','A2','A3','A4','A5','A6','A7','A8','B1','B2' )
tab <- as.table(tab)
tab
```

       Dilution factor Luminescence Calculated glucose (uM) Total glucose (uM)
    A1        2.000000  1711.666667                2.870658           5.741316
    A2        2.000000  1745.000000                2.896692           5.793383
    A3        2.000000   314.000000                1.779062           3.558123
    A4        2.000000   218.000000                1.704084           3.408169
    A5        2.000000   717.666667                2.094331           4.188661
    A6        2.000000  2395.000000                3.404350           6.808700
    A7        2.000000   383.666667                1.833472           3.666945
    A8        2.000000   417.666667                1.860027           3.720054
    B1        2.000000   375.666667                1.827224           3.654448
    B2        2.000000   402.666667                1.848312           3.696623
