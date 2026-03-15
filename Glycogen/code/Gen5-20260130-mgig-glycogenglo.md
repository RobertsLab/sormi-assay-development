Gen5-20260130-mgig-glycogenglo
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
plate_layout <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-Gen5-20260130-mgig-glycogenglo.csv", header = FALSE)
raw_luminescence <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-Gen5-20260130-mgig-glycogenglo.csv", header = FALSE)

cat("Plate layout:\n")
str(plate_layout)

cat("\n\n")

cat("Raw luminescence:\n")
str(raw_luminescence)
```

    Plate layout:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : chr  "A1-glyc-25-df.20" "A2-glyc-50-df.20" "A3-glyc-14-df.20" "A4-glyc-21-df.20" ...
     $ V2 : chr  "A1-glyc-25-df.20" "A2-glyc-50-df.20" "A3-glyc-14-df.20" "A4-glyc-21-df.20" ...
     $ V3 : chr  "A1-glyc-25-df.20" "A2-glyc-50-df.20" "A3-glyc-14-df.20" "A4-glyc-21-df.20" ...
     $ V4 : chr  "A1-glyc-25-df.100" "A2-glyc-50-df.100" "A3-glyc-14-df.100" "A4-glyc-21-df.100" ...
     $ V5 : chr  "A1-glyc-25-df.100" "A2-glyc-50-df.100" "A3-glyc-14-df.100" "A4-glyc-21-df.100" ...
     $ V6 : chr  "A1-glyc-25-df.100" "A2-glyc-50-df.100" "A3-glyc-14-df.100" "A4-glyc-21-df.100" ...
     $ V7 : chr  "A1-glu-25-df.20" "A2-glu-50-df.20" "A3-glu-14-df.20" "A4-glu-21-df.20" ...
     $ V8 : chr  "A1-glu-25-df.20" "A2-glu-50-df.20" "A3-glu-14-df.20" "A4-glu-21-df.20" ...
     $ V9 : chr  "A1-glu-25-df.20" "A2-glu-50-df.20" "A3-glu-14-df.20" "A4-glu-21-df.20" ...
     $ V10: chr  "A1-glu-1-25-df.100" "A2-glu-50-df.100" "A3-glu-14-df.100" "A4-glu-21-df.100" ...
     $ V11: chr  "A1-glu-1-25-df.100" "A2-glu-50-df.100" "A3-glu-14-df.100" "A4-glu-21-df.100" ...
     $ V12: chr  "A1-glu-1-25-df.100" "A2-glu-50-df.100" "A3-glu-14-df.100" "A4-glu-21-df.100" ...


    Raw luminescence:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : int  91415 44239 15614 25960 38685 160217 172362 156178
     $ V2 : int  94894 42939 15322 26973 43131 14166 13534 13733
     $ V3 : int  98759 42483 15679 26748 37794 2075 2015 448
     $ V4 : int  17978 8569 4500 6141 8454 1290 1273 1387
     $ V5 : int  18337 8474 4440 6075 8487 1172 1310 2468
     $ V6 : int  18563 8942 4630 6298 9004 NA NA NA
     $ V7 : int  665 492 415 400 763 150265 149715 151050
     $ V8 : int  750 617 621 494 692 15780 15103 16252
     $ V9 : int  597 555 422 626 646 1633 1708 1745
     $ V10: int  487 398 378 443 450 435 468 555
     $ V11: int  462 377 409 346 448 403 398 407
     $ V12: int  452 395 394 371 456 NA NA NA

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
glyc_sample_cols20 <- c(1,2,3)
glyc_A1_dilution20 <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 1]))
glyc_A1_luminescence20 <- as.numeric(raw_luminescence[1, glyc_sample_cols20])

glyc_A2_dilution20 <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 1]))
glyc_A2_luminescence20 <- as.numeric(raw_luminescence[2, glyc_sample_cols20])

glyc_A3_dilution20 <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 1]))
glyc_A3_luminescence20 <- as.numeric(raw_luminescence[3, glyc_sample_cols20])

glyc_A4_dilution20 <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 1]))
glyc_A4_luminescence20 <- as.numeric(raw_luminescence[4, glyc_sample_cols20])

glyc_A5_dilution20 <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 1]))
glyc_A5_luminescence20 <- as.numeric(raw_luminescence[5, glyc_sample_cols20])


glyc_sample_cols100 <- c(4,5,6)
glyc_A1_dilution100 <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 4]))
glyc_A1_luminescence100 <- as.numeric(raw_luminescence[1, glyc_sample_cols100])

glyc_A2_dilution100 <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 4]))
glyc_A2_luminescence100 <- as.numeric(raw_luminescence[2, glyc_sample_cols100])

glyc_A3_dilution100 <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 4]))
glyc_A3_luminescence100 <- as.numeric(raw_luminescence[3, glyc_sample_cols100])

glyc_A4_dilution100 <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 4]))
glyc_A4_luminescence100 <- as.numeric(raw_luminescence[4, glyc_sample_cols100])

glyc_A5_dilution100 <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 4]))
glyc_A5_luminescence100 <- as.numeric(raw_luminescence[5, glyc_sample_cols100])
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
glyc_slope
```

    glyc_concentration 
              8126.693 

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A1_mean_lum20 <- numeric(length(glyc_A1_dilution20))
glyc_A1_se_lum20 <- numeric(length(glyc_A1_dilution20))
glyc_A1_mean_conc20 <- numeric(length(glyc_A1_dilution20))

for (i in 1:length(glyc_A1_dilution20)) {
  df_val <- glyc_A1_dilution20[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A1_lum_values20 <- c(glyc_A1_luminescence20[glyc_A1_dilution20 == df_val])
  glyc_A1_mean_lum20[i] <- mean(glyc_A1_lum_values20)
  glyc_A1_se_lum20[i] <- sd(glyc_A1_lum_values20) / sqrt(length(glyc_A1_lum_values20))
  # Calculate concentration from mean luminescence
  glyc_A1_mean_conc20[i] <- (glyc_A1_mean_lum20[i] - glyc_intercept) / glyc_slope
}

glyc_A1_data20 <- data.frame(
  glyc_A1_dilution_factor20 = glyc_A1_dilution20,
  glyc_A1_mean_luminescence20 = glyc_A1_mean_lum20,
  glyc_A1_se20 = glyc_A1_se_lum20,
  glyc_A1_conc20 =  glyc_A1_mean_conc20,
  label = paste0("A1df.", glyc_A1_dilution20)
)
glyc_A1_mean_lum20
glyc_A1_mean_conc20
```

    [1] 95022.67
    [1] 11.67705

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A2_mean_lum20 <- numeric(length(glyc_A2_dilution20))
glyc_A2_se_lum20 <- numeric(length(glyc_A2_dilution20))
glyc_A2_mean_conc20 <- numeric(length(glyc_A2_dilution20))

for (i in 1:length(glyc_A2_dilution20)) {
  df_val <- glyc_A2_dilution20[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A2_lum_values20 <- c(glyc_A2_luminescence20[glyc_A2_dilution20 == df_val])
  glyc_A2_mean_lum20[i] <- mean(glyc_A2_lum_values20)
  glyc_A2_se_lum20[i] <- sd(glyc_A2_lum_values20) / sqrt(length(glyc_A2_lum_values20))
  # Calculate concentration from mean luminescence
  glyc_A2_mean_conc20[i] <- (glyc_A2_mean_lum20[i] - glyc_intercept) / glyc_slope
}

glyc_A2_data20 <- data.frame(
  glyc_A2_dilution_factor20 = glyc_A2_dilution20,
  glyc_A2_mean_luminescence20 = glyc_A2_mean_lum20,
  glyc_A2_se20 = glyc_A2_se_lum20,
  glyc_A2_conc20 =  glyc_A2_mean_conc20,
  label = paste0("A2df.", glyc_A2_dilution20)
)
glyc_A2_mean_lum20
glyc_A2_mean_conc20
```

    [1] 43220.33
    [1] 5.302709

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A3_mean_lum20 <- numeric(length(glyc_A3_dilution20))
glyc_A3_se_lum20 <- numeric(length(glyc_A3_dilution20))
glyc_A3_mean_conc20 <- numeric(length(glyc_A3_dilution20))

for (i in 1:length(glyc_A3_dilution20)) {
  df_val <- glyc_A3_dilution20[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A3_lum_values20 <- c(glyc_A3_luminescence20[glyc_A3_dilution20 == df_val])
  glyc_A3_mean_lum20[i] <- mean(glyc_A3_lum_values20)
  glyc_A3_se_lum20[i] <- sd(glyc_A3_lum_values20) / sqrt(length(glyc_A3_lum_values20))
  # Calculate concentration from mean luminescence
  glyc_A3_mean_conc20[i] <- (glyc_A3_mean_lum20[i] - glyc_intercept) / glyc_slope
}

glyc_A3_data20 <- data.frame(
  glyc_A3_dilution_factor20 = glyc_A3_dilution20,
  glyc_A3_mean_luminescence20 = glyc_A3_mean_lum20,
  glyc_A3_se20 = glyc_A3_se_lum20,
  glyc_A3_conc20 =  glyc_A3_mean_conc20,
  label = paste0("A3df.", glyc_A3_dilution20)
)
glyc_A3_mean_lum20
glyc_A3_mean_conc20
```

    [1] 15538.33
    [1] 1.896404

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A4_mean_lum20 <- numeric(length(glyc_A4_dilution20))
glyc_A4_se_lum20 <- numeric(length(glyc_A4_dilution20))
glyc_A4_mean_conc20 <- numeric(length(glyc_A4_dilution20))

for (i in 1:length(glyc_A4_dilution20)) {
  df_val <- glyc_A4_dilution20[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A4_lum_values20 <- c(glyc_A4_luminescence20[glyc_A4_dilution20 == df_val])
  glyc_A4_mean_lum20[i] <- mean(glyc_A4_lum_values20)
  glyc_A4_se_lum20[i] <- sd(glyc_A4_lum_values20) / sqrt(length(glyc_A4_lum_values20))
  # Calculate concentration from mean luminescence
  glyc_A4_mean_conc20[i] <- (glyc_A4_mean_lum20[i] - glyc_intercept) / glyc_slope
}

glyc_A4_data20 <- data.frame(
  glyc_A4_dilution_factor20 = glyc_A4_dilution20,
  glyc_A4_mean_luminescence20 = glyc_A4_mean_lum20,
  glyc_A4_se20 = glyc_A4_se_lum20,
  glyc_A4_conc20 =  glyc_A4_mean_conc20,
  label = paste0("A4df.", glyc_A4_dilution20)
)
glyc_A4_mean_lum20
glyc_A4_mean_conc20
```

    [1] 26560.33
    [1] 3.252675

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A5_mean_lum20 <- numeric(length(glyc_A5_dilution20))
glyc_A5_se_lum20 <- numeric(length(glyc_A5_dilution20))
glyc_A5_mean_conc20 <- numeric(length(glyc_A5_dilution20))

for (i in 1:length(glyc_A5_dilution20)) {
  df_val <- glyc_A5_dilution20[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A5_lum_values20 <- c(glyc_A5_luminescence20[glyc_A5_dilution20 == df_val])
  glyc_A5_mean_lum20[i] <- mean(glyc_A5_lum_values20)
  glyc_A5_se_lum20[i] <- sd(glyc_A5_lum_values20) / sqrt(length(glyc_A5_lum_values20))
  # Calculate concentration from mean luminescence
  glyc_A5_mean_conc20[i] <- (glyc_A5_mean_lum20[i] - glyc_intercept) / glyc_slope
}

glyc_A5_data20 <- data.frame(
  glyc_A5_dilution_factor20 = glyc_A5_dilution20,
  glyc_A5_mean_luminescence20 = glyc_A5_mean_lum20,
  glyc_A5_se20 = glyc_A5_se_lum20,
  glyc_A5_conc20 =  glyc_A5_mean_conc20,
  label = paste0("A5df.", glyc_A5_dilution20)
)
glyc_A5_mean_lum20
glyc_A5_mean_conc20
```

    [1] 39870
    [1] 4.890446

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A1_mean_lum100 <- numeric(length(glyc_A1_dilution100))
glyc_A1_se_lum100 <- numeric(length(glyc_A1_dilution100))
glyc_A1_mean_conc100 <- numeric(length(glyc_A1_dilution100))

for (i in 1:length(glyc_A1_dilution100)) {
  df_val <- glyc_A1_dilution100[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A1_lum_values100 <- c(glyc_A1_luminescence100[glyc_A1_dilution100 == df_val])
  glyc_A1_mean_lum100[i] <- mean(glyc_A1_lum_values100)
  glyc_A1_se_lum100[i] <- sd(glyc_A1_lum_values100) / sqrt(length(glyc_A1_lum_values100))
  # Calculate concentration from mean luminescence
  glyc_A1_mean_conc100[i] <- (glyc_A1_mean_lum100[i] - glyc_intercept) / glyc_slope
}

glyc_A1_data100 <- data.frame(
  glyc_A1_dilution_factor100 = glyc_A1_dilution100,
  glyc_A1_mean_luminescence100 = glyc_A1_mean_lum100,
  glyc_A1_se100 = glyc_A1_se_lum100,
  glyc_A1_conc100 =  glyc_A1_mean_conc100,
  label = paste0("A1df.", glyc_A1_dilution100)
)
glyc_A1_mean_lum100
glyc_A1_mean_conc100
```

    [1] 18292.67
    [1] 2.235328

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A2_mean_lum100 <- numeric(length(glyc_A2_dilution100))
glyc_A2_se_lum100 <- numeric(length(glyc_A2_dilution100))
glyc_A2_mean_conc100 <- numeric(length(glyc_A2_dilution100))

for (i in 1:length(glyc_A2_dilution100)) {
  df_val <- glyc_A2_dilution100[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A2_lum_values100 <- c(glyc_A2_luminescence100[glyc_A2_dilution100 == df_val])
  glyc_A2_mean_lum100[i] <- mean(glyc_A2_lum_values100)
  glyc_A2_se_lum100[i] <- sd(glyc_A2_lum_values100) / sqrt(length(glyc_A2_lum_values100))
  # Calculate concentration from mean luminescence
  glyc_A2_mean_conc100[i] <- (glyc_A2_mean_lum100[i] - glyc_intercept) / glyc_slope
}

glyc_A2_data100 <- data.frame(
  glyc_A2_dilution_factor100 = glyc_A2_dilution100,
  glyc_A2_mean_luminescence100 = glyc_A2_mean_lum100,
  glyc_A2_se100 = glyc_A2_se_lum100,
  glyc_A2_conc100 =  glyc_A2_mean_conc100,
  label = paste0("A2df.", glyc_A2_dilution100)
)
glyc_A2_mean_lum100
glyc_A2_mean_conc100
```

    [1] 8661.667
    [1] 1.050221

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A3_mean_lum100 <- numeric(length(glyc_A3_dilution100))
glyc_A3_se_lum100 <- numeric(length(glyc_A3_dilution100))
glyc_A3_mean_conc100 <- numeric(length(glyc_A3_dilution100))

for (i in 1:length(glyc_A3_dilution100)) {
  df_val <- glyc_A3_dilution100[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A3_lum_values100 <- c(glyc_A3_luminescence100[glyc_A3_dilution100 == df_val])
  glyc_A3_mean_lum100[i] <- mean(glyc_A3_lum_values100)
  glyc_A3_se_lum100[i] <- sd(glyc_A3_lum_values100) / sqrt(length(glyc_A3_lum_values100))
  # Calculate concentration from mean luminescence
  glyc_A3_mean_conc100[i] <- (glyc_A3_mean_lum100[i] - glyc_intercept) / glyc_slope
}

glyc_A3_data100 <- data.frame(
  glyc_A3_dilution_factor100 = glyc_A3_dilution100,
  glyc_A3_mean_luminescence100 = glyc_A3_mean_lum100,
  glyc_A3_se100 = glyc_A3_se_lum100,
  glyc_A3_conc100 =  glyc_A3_mean_conc100,
  label = paste0("A3df.", glyc_A3_dilution100)
)
glyc_A3_mean_lum100
glyc_A3_mean_conc100
```

    [1] 4523.333
    [1] 0.540994

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A4_mean_lum100 <- numeric(length(glyc_A4_dilution100))
glyc_A4_se_lum100 <- numeric(length(glyc_A4_dilution100))
glyc_A4_mean_conc100 <- numeric(length(glyc_A4_dilution100))

for (i in 1:length(glyc_A4_dilution100)) {
  df_val <- glyc_A4_dilution100[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A4_lum_values100 <- c(glyc_A4_luminescence100[glyc_A4_dilution100 == df_val])
  glyc_A4_mean_lum100[i] <- mean(glyc_A4_lum_values100)
  glyc_A4_se_lum100[i] <- sd(glyc_A4_lum_values100) / sqrt(length(glyc_A4_lum_values100))
  # Calculate concentration from mean luminescence
  glyc_A4_mean_conc100[i] <- (glyc_A4_mean_lum100[i] - glyc_intercept) / glyc_slope
}

glyc_A4_data100 <- data.frame(
  glyc_A4_dilution_factor100 = glyc_A4_dilution100,
  glyc_A4_mean_luminescence100 = glyc_A4_mean_lum100,
  glyc_A4_se100 = glyc_A4_se_lum100,
  glyc_A4_conc100 =  glyc_A4_mean_conc100,
  label = paste0("A4df.", glyc_A4_dilution100)
)
glyc_A4_mean_lum100
glyc_A4_mean_conc100
```

    [1] 6171.333
    [1] 0.7437825

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A5_mean_lum100 <- numeric(length(glyc_A5_dilution100))
glyc_A5_se_lum100 <- numeric(length(glyc_A5_dilution100))
glyc_A5_mean_conc100 <- numeric(length(glyc_A5_dilution100))

for (i in 1:length(glyc_A5_dilution100)) {
  df_val <- glyc_A5_dilution100[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A5_lum_values100 <- c(glyc_A5_luminescence100[glyc_A5_dilution100 == df_val])
  glyc_A5_mean_lum100[i] <- mean(glyc_A5_lum_values100)
  glyc_A5_se_lum100[i] <- sd(glyc_A5_lum_values100) / sqrt(length(glyc_A5_lum_values100))
  # Calculate concentration from mean luminescence
  glyc_A5_mean_conc100[i] <- (glyc_A5_mean_lum100[i] - glyc_intercept) / glyc_slope
}

glyc_A5_data100 <- data.frame(
  glyc_A5_dilution_factor100 = glyc_A5_dilution100,
  glyc_A5_mean_luminescence100 = glyc_A5_mean_lum100,
  glyc_A5_se100 = glyc_A5_se_lum100,
  glyc_A5_conc100 =  glyc_A5_mean_conc100,
  label = paste0("A5df.", glyc_A5_dilution100)
)
glyc_A5_mean_lum100
glyc_A5_mean_conc100
```

    [1] 8648.333
    [1] 1.04858

``` r
# Create the plot
glyc_plot <- ggplot(glycogen_summary_data, aes(x = glyc_concentration, y = glyc_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glyc_mean_luminescence - glyc_se, ymax = glyc_mean_luminescence + glyc_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +
  
  geom_errorbar(data = glyc_A1_data20, aes(x = glyc_A1_conc20, y = glyc_A1_mean_luminescence20,
                                          ymin = glyc_A1_mean_luminescence20 - glyc_A1_se20, ymax = glyc_A1_mean_luminescence20 + glyc_A1_se20,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A1_data20, aes(x = glyc_A1_conc20, y = glyc_A1_mean_luminescence20,
                                       color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_A2_data20, aes(x = glyc_A2_conc20, y = glyc_A2_mean_luminescence20,
                                          ymin = glyc_A2_mean_luminescence20 - glyc_A2_se20, ymax = glyc_A2_mean_luminescence20 + glyc_A2_se20,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A2_data20, aes(x = glyc_A2_conc20, y = glyc_A2_mean_luminescence20,
                                       color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_A3_data20, aes(x = glyc_A3_conc20, y = glyc_A3_mean_luminescence20,
                                          ymin = glyc_A3_mean_luminescence20 - glyc_A3_se20, ymax = glyc_A3_mean_luminescence20 + glyc_A3_se20,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A3_data20, aes(x = glyc_A3_conc20, y = glyc_A3_mean_luminescence20,
                                       color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_A4_data20, aes(x = glyc_A4_conc20, y = glyc_A4_mean_luminescence20,
                                          ymin = glyc_A4_mean_luminescence20 - glyc_A4_se20, ymax = glyc_A4_mean_luminescence20 + glyc_A4_se20,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A4_data20, aes(x = glyc_A4_conc20, y = glyc_A4_mean_luminescence20,
                                       color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_A5_data20, aes(x = glyc_A5_conc20, y = glyc_A5_mean_luminescence20,
                                          ymin = glyc_A5_mean_luminescence20 - glyc_A5_se20, ymax = glyc_A5_mean_luminescence20 + glyc_A5_se20,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A5_data20, aes(x = glyc_A5_conc20, y = glyc_A5_mean_luminescence20,
                                       color = label, shape = label), size = 4) +
  
  
  geom_errorbar(data = glyc_A1_data100, aes(x = glyc_A1_conc100, y = glyc_A1_mean_luminescence100,
                                          ymin = glyc_A1_mean_luminescence100 - glyc_A1_se100, ymax = glyc_A1_mean_luminescence100 + glyc_A1_se100,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A1_data100, aes(x = glyc_A1_conc100, y = glyc_A1_mean_luminescence100,
                                       color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_A2_data100, aes(x = glyc_A2_conc100, y = glyc_A2_mean_luminescence100,
                                          ymin = glyc_A2_mean_luminescence100 - glyc_A2_se100, ymax = glyc_A2_mean_luminescence100 + glyc_A2_se100,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A2_data100, aes(x = glyc_A2_conc100, y = glyc_A2_mean_luminescence100,
                                       color = label, shape = label), size = 4) +
  
  
  geom_errorbar(data = glyc_A3_data100, aes(x = glyc_A3_conc100, y = glyc_A3_mean_luminescence100,
                                          ymin = glyc_A3_mean_luminescence100 - glyc_A3_se100, ymax = glyc_A3_mean_luminescence100 + glyc_A3_se100,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A3_data100, aes(x = glyc_A3_conc100, y = glyc_A3_mean_luminescence100,
                                       color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_A4_data100, aes(x = glyc_A4_conc100, y = glyc_A4_mean_luminescence100,
                                          ymin = glyc_A4_mean_luminescence100 - glyc_A4_se100, ymax = glyc_A4_mean_luminescence100 + glyc_A4_se100,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A4_data100, aes(x = glyc_A4_conc100, y = glyc_A4_mean_luminescence100,
                                       color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_A5_data100, aes(x = glyc_A5_conc100, y = glyc_A5_mean_luminescence100,
                                          ymin = glyc_A5_mean_luminescence100 - glyc_A5_se100, ymax = glyc_A5_mean_luminescence100 + glyc_A5_se100,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A5_data100, aes(x = glyc_A5_conc100, y = glyc_A5_mean_luminescence100,
                                       color = label, shape = label), size = 4) +

  
  
  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                                "A1df.20" = "darkred",
                                "A2df.20" = "darkorange",
                                "A3df.20" = "darkgreen",
                                "A4df.20" = "purple",
                                "A5df.20" = "brown",
                                "A1df.100" = "green3",
                                "A2df.100" = "firebrick1",
                                "A3df.100" = "cyan",
                                "A4df.100" = "yellow3",
                                "A5df.100" = "thistle3"
                     )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                "A1df.20" = 17,
                                "A2df.20" = 15,
                                "A3df.20" = 18,
                                "A4df.20" = 8,
                                "A5df.20" = 4,
                                "A1df.100" = 5,
                                "A2df.100" = 0,
                                "A3df.100" = 2,
                                "A4df.100" = 19,
                                "A5df.100" = 20
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

![](Gen5-20260130-mgig-glycogenglo_files/figure-gfm/plot%20glycogen%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glyc_A1_data20
glyc_A2_data20
glyc_A3_data20
glyc_A4_data20
glyc_A5_data20
glyc_A1_data100
glyc_A2_data100
glyc_A3_data100
glyc_A4_data100
glyc_A5_data100


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

      glyc_A1_dilution_factor20 glyc_A1_mean_luminescence20 glyc_A1_se20
    1                        20                    95022.67     2121.006
      glyc_A1_conc20   label
    1       11.67705 A1df.20
      glyc_A2_dilution_factor20 glyc_A2_mean_luminescence20 glyc_A2_se20
    1                        20                    43220.33     526.0689
      glyc_A2_conc20   label
    1       5.302709 A2df.20
      glyc_A3_dilution_factor20 glyc_A3_mean_luminescence20 glyc_A3_se20
    1                        20                    15538.33     109.7821
      glyc_A3_conc20   label
    1       1.896404 A3df.20
      glyc_A4_dilution_factor20 glyc_A4_mean_luminescence20 glyc_A4_se20
    1                        20                    26560.33     307.1136
      glyc_A4_conc20   label
    1       3.252675 A4df.20
      glyc_A5_dilution_factor20 glyc_A5_mean_luminescence20 glyc_A5_se20
    1                        20                       39870     1650.663
      glyc_A5_conc20   label
    1       4.890446 A5df.20
      glyc_A1_dilution_factor100 glyc_A1_mean_luminescence100 glyc_A1_se100
    1                        100                     18292.67      170.3235
      glyc_A1_conc100    label
    1        2.235328 A1df.100
      glyc_A2_dilution_factor100 glyc_A2_mean_luminescence100 glyc_A2_se100
    1                        100                     8661.667      142.8243
      glyc_A2_conc100    label
    1        1.050221 A2df.100
      glyc_A3_dilution_factor100 glyc_A3_mean_luminescence100 glyc_A3_se100
    1                        100                     4523.333      56.07535
      glyc_A3_conc100    label
    1        0.540994 A3df.100
      glyc_A4_dilution_factor100 glyc_A4_mean_luminescence100 glyc_A4_se100
    1                        100                     6171.333      66.13706
      glyc_A4_conc100    label
    1       0.7437825 A4df.100
      glyc_A5_dilution_factor100 glyc_A5_mean_luminescence100 glyc_A5_se100
    1                        100                     8648.333      178.0883
      glyc_A5_conc100    label
    1         1.04858 A5df.100
    glycogen Standard Curve Summary:
    ==================================================
    Concentration: 20 µg/µL
      Mean Luminescence: 162919.00
      Standard Error: 4863.33
      CV%: 5.17%

    Concentration: 2 µg/µL
      Mean Luminescence: 13811.00
      Standard Error: 186.56
      CV%: 2.34%

    Concentration: 0.2 µg/µL
      Mean Luminescence: 1512.67
      Standard Error: 532.62
      CV%: 60.99%

    Concentration: 0.02 µg/µL
      Mean Luminescence: 1316.67
      Standard Error: 35.51
      CV%: 4.67%

    Concentration: 0 µg/µL
      Mean Luminescence: 1650.00
      Standard Error: 410.94
      CV%: 43.14%

``` r
tab <- matrix(c(glyc_A1_dilution20, glyc_A1_mean_lum20, glyc_A1_mean_conc20,  (glyc_A1_dilution20*glyc_A1_mean_conc20), 
                glyc_A2_dilution20, glyc_A2_mean_lum20, glyc_A2_mean_conc20,  (glyc_A2_dilution20*glyc_A2_mean_conc20),
                glyc_A3_dilution20, glyc_A3_mean_lum20, glyc_A3_mean_conc20,  (glyc_A3_dilution20*glyc_A3_mean_conc20),
                glyc_A4_dilution20, glyc_A4_mean_lum20, glyc_A4_mean_conc20,  (glyc_A4_dilution20*glyc_A4_mean_conc20),
                glyc_A5_dilution20, glyc_A5_mean_lum20, glyc_A5_mean_conc20,  (glyc_A5_dilution20*glyc_A5_mean_conc20),
                glyc_A1_dilution100, glyc_A1_mean_lum100, glyc_A1_mean_conc100,  (glyc_A1_dilution100*glyc_A1_mean_conc100), 
                glyc_A2_dilution100, glyc_A2_mean_lum100, glyc_A2_mean_conc100,  (glyc_A2_dilution100*glyc_A2_mean_conc100),
                glyc_A3_dilution100, glyc_A3_mean_lum100, glyc_A3_mean_conc100,  (glyc_A3_dilution100*glyc_A3_mean_conc100),
                glyc_A4_dilution100, glyc_A4_mean_lum100, glyc_A4_mean_conc100,  (glyc_A4_dilution100*glyc_A4_mean_conc100),
                glyc_A5_dilution100, glyc_A5_mean_lum100, glyc_A5_mean_conc100,  (glyc_A5_dilution100*glyc_A5_mean_conc100)), ncol=4, byrow=TRUE)
colnames(tab) <- c('Dilution factor','Luminescence','Calculated Glycogen (ug/uL)', 'Total glycogen (ug/uL)')
rownames(tab) <- c('A1.df20','A2.df20','A3.df20','A4.df20','A5.df20','A1.df100','A2.df100','A3.df100','A4.df100','Af.df100')
tab <- as.table(tab)
tab
```

             Dilution factor Luminescence Calculated Glycogen (ug/uL)
    A1.df20     2.000000e+01 9.502267e+04                1.167705e+01
    A2.df20     2.000000e+01 4.322033e+04                5.302709e+00
    A3.df20     2.000000e+01 1.553833e+04                1.896404e+00
    A4.df20     2.000000e+01 2.656033e+04                3.252675e+00
    A5.df20     2.000000e+01 3.987000e+04                4.890446e+00
    A1.df100    1.000000e+02 1.829267e+04                2.235328e+00
    A2.df100    1.000000e+02 8.661667e+03                1.050221e+00
    A3.df100    1.000000e+02 4.523333e+03                5.409940e-01
    A4.df100    1.000000e+02 6.171333e+03                7.437825e-01
    Af.df100    1.000000e+02 8.648333e+03                1.048580e+00
             Total glycogen (ug/uL)
    A1.df20            2.335410e+02
    A2.df20            1.060542e+02
    A3.df20            3.792808e+01
    A4.df20            6.505350e+01
    A5.df20            9.780893e+01
    A1.df100           2.235328e+02
    A2.df100           1.050221e+02
    A3.df100           5.409940e+01
    A4.df100           7.437825e+01
    Af.df100           1.048580e+02

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
glu_sample_cols20 <- c(7,8,9)
glu_A1_dilution20 <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 7]))
glu_A1_luminescence20 <- as.numeric(raw_luminescence[1, glu_sample_cols20])

glu_A2_dilution20 <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 7]))
glu_A2_luminescence20 <- as.numeric(raw_luminescence[2, glu_sample_cols20])

glu_A3_dilution20 <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 7]))
glu_A3_luminescence20 <- as.numeric(raw_luminescence[3, glu_sample_cols20])

glu_A4_dilution20 <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 7]))
glu_A4_luminescence20 <- as.numeric(raw_luminescence[4, glu_sample_cols20])

glu_A5_dilution20 <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 7]))
glu_A5_luminescence20 <- as.numeric(raw_luminescence[5, glu_sample_cols20])


glu_sample_cols100 <- c(10,11,12)
glu_A1_dilution100 <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 10]))
glu_A1_luminescence100 <- as.numeric(raw_luminescence[1, glu_sample_cols100])

glu_A2_dilution100 <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 10]))
glu_A2_luminescence100 <- as.numeric(raw_luminescence[2, glu_sample_cols100])

glu_A3_dilution100 <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 10]))
glu_A3_luminescence100 <- as.numeric(raw_luminescence[3, glu_sample_cols100])

glu_A4_dilution100 <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 10]))
glu_A4_luminescence100 <- as.numeric(raw_luminescence[4, glu_sample_cols100])

glu_A5_dilution100 <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 10]))
glu_A5_luminescence100 <- as.numeric(raw_luminescence[5, glu_sample_cols100])
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
glu_A1_mean_lum20 <- numeric(length(glu_A1_dilution20))
glu_A1_se_lum20 <- numeric(length(glu_A1_dilution20))
glu_A1_mean_conc20 <- numeric(length(glu_A1_dilution20))

for (i in 1:length(glu_A1_dilution20)) {
  df_val <- glu_A1_dilution20[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A1_lum_values20 <- c(glu_A1_luminescence20[glu_A1_dilution20 == df_val])
  glu_A1_mean_lum20[i] <- mean(glu_A1_lum_values20)
  glu_A1_se_lum20[i] <- sd(glu_A1_lum_values20) / sqrt(length(glu_A1_lum_values20))
  # Calculate concentration from mean luminescence
  glu_A1_mean_conc20[i] <- (glu_A1_mean_lum20[i] - glu_intercept) / glu_slope
}

glu_A1_data20 <- data.frame(
  glu_A1_dilution_factor20 = glu_A1_dilution20,
  glu_A1_mean_luminescence20 = glu_A1_mean_lum20,
  glu_A1_se20 = glu_A1_se_lum20,
  glu_A1_conc20 =  glu_A1_mean_conc20,
  label = paste0("A1df.", glu_A1_dilution20)
)
glu_A1_mean_lum20
glu_A1_mean_conc20
```

    [1] 670.6667
    [1] 0.1770656

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A2_mean_lum20 <- numeric(length(glu_A2_dilution20))
glu_A2_se_lum20 <- numeric(length(glu_A2_dilution20))
glu_A2_mean_conc20 <- numeric(length(glu_A2_dilution20))

for (i in 1:length(glu_A2_dilution20)) {
  df_val <- glu_A2_dilution20[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A2_lum_values20 <- c(glu_A2_luminescence20[glu_A2_dilution20 == df_val])
  glu_A2_mean_lum20[i] <- mean(glu_A2_lum_values20)
  glu_A2_se_lum20[i] <- sd(glu_A2_lum_values20) / sqrt(length(glu_A2_lum_values20))
  # Calculate concentration from mean luminescence
  glu_A2_mean_conc20[i] <- (glu_A2_mean_lum20[i] - glu_intercept) / glu_slope
}

glu_A2_data20 <- data.frame(
  glu_A2_dilution_factor20 = glu_A2_dilution20,
  glu_A2_mean_luminescence20 = glu_A2_mean_lum20,
  glu_A2_se20 = glu_A2_se_lum20,
  glu_A2_conc20 =  glu_A2_mean_conc20,
  label = paste0("A2df.", glu_A2_dilution20)
)
glu_A2_mean_lum20
glu_A2_mean_conc20
```

    [1] 554.6667
    [1] 0.09971525

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A3_mean_lum20 <- numeric(length(glu_A3_dilution20))
glu_A3_se_lum20 <- numeric(length(glu_A3_dilution20))
glu_A3_mean_conc20 <- numeric(length(glu_A3_dilution20))

for (i in 1:length(glu_A3_dilution20)) {
  df_val <- glu_A3_dilution20[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A3_lum_values20 <- c(glu_A3_luminescence20[glu_A3_dilution20 == df_val])
  glu_A3_mean_lum20[i] <- mean(glu_A3_lum_values20)
  glu_A3_se_lum20[i] <- sd(glu_A3_lum_values20) / sqrt(length(glu_A3_lum_values20))
  # Calculate concentration from mean luminescence
  glu_A3_mean_conc20[i] <- (glu_A3_mean_lum20[i] - glu_intercept) / glu_slope
}

glu_A3_data20 <- data.frame(
  glu_A3_dilution_factor20 = glu_A3_dilution20,
  glu_A3_mean_luminescence20 = glu_A3_mean_lum20,
  glu_A3_se20 = glu_A3_se_lum20,
  glu_A3_conc20 =  glu_A3_mean_conc20,
  label = paste0("A3df.", glu_A3_dilution20)
)
glu_A3_mean_lum20
glu_A3_mean_conc20
```

    [1] 486
    [1] 0.0539274

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A4_mean_lum20 <- numeric(length(glu_A4_dilution20))
glu_A4_se_lum20 <- numeric(length(glu_A4_dilution20))
glu_A4_mean_conc20 <- numeric(length(glu_A4_dilution20))

for (i in 1:length(glu_A4_dilution20)) {
  df_val <- glu_A4_dilution20[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A4_lum_values20 <- c(glu_A4_luminescence20[glu_A4_dilution20 == df_val])
  glu_A4_mean_lum20[i] <- mean(glu_A4_lum_values20)
  glu_A4_se_lum20[i] <- sd(glu_A4_lum_values20) / sqrt(length(glu_A4_lum_values20))
  # Calculate concentration from mean luminescence
  glu_A4_mean_conc20[i] <- (glu_A4_mean_lum20[i] - glu_intercept) / glu_slope
}

glu_A4_data20 <- data.frame(
  glu_A4_dilution_factor20 = glu_A4_dilution20,
  glu_A4_mean_luminescence20 = glu_A4_mean_lum20,
  glu_A4_se20 = glu_A4_se_lum20,
  glu_A4_conc20 =  glu_A4_mean_conc20,
  label = paste0("A4df.", glu_A4_dilution20)
)
glu_A4_mean_lum20
glu_A4_mean_conc20
```

    [1] 506.6667
    [1] 0.06770821

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A5_mean_lum20 <- numeric(length(glu_A5_dilution20))
glu_A5_se_lum20 <- numeric(length(glu_A5_dilution20))
glu_A5_mean_conc20 <- numeric(length(glu_A5_dilution20))

for (i in 1:length(glu_A5_dilution20)) {
  df_val <- glu_A5_dilution20[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A5_lum_values20 <- c(glu_A5_luminescence20[glu_A5_dilution20 == df_val])
  glu_A5_mean_lum20[i] <- mean(glu_A5_lum_values20)
  glu_A5_se_lum20[i] <- sd(glu_A5_lum_values20) / sqrt(length(glu_A5_lum_values20))
  # Calculate concentration from mean luminescence
  glu_A5_mean_conc20[i] <- (glu_A5_mean_lum20[i] - glu_intercept) / glu_slope
}

glu_A5_data20 <- data.frame(
  glu_A5_dilution_factor20 = glu_A5_dilution20,
  glu_A5_mean_luminescence20 = glu_A5_mean_lum20,
  glu_A5_se20 = glu_A5_se_lum20,
  glu_A5_conc20 =  glu_A5_mean_conc20,
  label = paste0("A5df.", glu_A5_dilution20)
)
glu_A5_mean_lum20
glu_A5_mean_conc20
```

    [1] 700.3333
    [1] 0.1968477

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A1_mean_lum100 <- numeric(length(glu_A1_dilution100))
glu_A1_se_lum100 <- numeric(length(glu_A1_dilution100))
glu_A1_mean_conc100 <- numeric(length(glu_A1_dilution100))

for (i in 1:length(glu_A1_dilution100)) {
  df_val <- glu_A1_dilution100[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A1_lum_values100 <- c(glu_A1_luminescence100[glu_A1_dilution100 == df_val])
  glu_A1_mean_lum100[i] <- mean(glu_A1_lum_values100)
  glu_A1_se_lum100[i] <- sd(glu_A1_lum_values100) / sqrt(length(glu_A1_lum_values100))
  # Calculate concentration from mean luminescence
  glu_A1_mean_conc100[i] <- (glu_A1_mean_lum100[i] - glu_intercept) / glu_slope
}

glu_A1_data100 <- data.frame(
  glu_A1_dilution_factor100 = glu_A1_dilution100,
  glu_A1_mean_luminescence100 = glu_A1_mean_lum100,
  glu_A1_se100 = glu_A1_se_lum100,
  glu_A1_conc100 =  glu_A1_mean_conc100,
  label = paste0("A1df.", glu_A1_dilution100)
)
glu_A1_mean_lum100
glu_A1_mean_conc100
```

    [1] 467
    [1] 0.04125795

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A2_mean_lum100 <- numeric(length(glu_A2_dilution100))
glu_A2_se_lum100 <- numeric(length(glu_A2_dilution100))
glu_A2_mean_conc100 <- numeric(length(glu_A2_dilution100))

for (i in 1:length(glu_A2_dilution100)) {
  df_val <- glu_A2_dilution100[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A2_lum_values100 <- c(glu_A2_luminescence100[glu_A2_dilution100 == df_val])
  glu_A2_mean_lum100[i] <- mean(glu_A2_lum_values100)
  glu_A2_se_lum100[i] <- sd(glu_A2_lum_values100) / sqrt(length(glu_A2_lum_values100))
  # Calculate concentration from mean luminescence
  glu_A2_mean_conc100[i] <- (glu_A2_mean_lum100[i] - glu_intercept) / glu_slope
}

glu_A2_data100 <- data.frame(
  glu_A2_dilution_factor100 = glu_A2_dilution100,
  glu_A2_mean_luminescence100 = glu_A2_mean_lum100,
  glu_A2_se100 = glu_A2_se_lum100,
  glu_A2_conc100 =  glu_A2_mean_conc100,
  label = paste0("A2df.", glu_A2_dilution100)
)
glu_A2_mean_lum100
glu_A2_mean_conc100
```

    [1] 390
    [1] -0.01008667

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A3_mean_lum100 <- numeric(length(glu_A3_dilution100))
glu_A3_se_lum100 <- numeric(length(glu_A3_dilution100))
glu_A3_mean_conc100 <- numeric(length(glu_A3_dilution100))

for (i in 1:length(glu_A3_dilution100)) {
  df_val <- glu_A3_dilution100[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A3_lum_values100 <- c(glu_A3_luminescence100[glu_A3_dilution100 == df_val])
  glu_A3_mean_lum100[i] <- mean(glu_A3_lum_values100)
  glu_A3_se_lum100[i] <- sd(glu_A3_lum_values100) / sqrt(length(glu_A3_lum_values100))
  # Calculate concentration from mean luminescence
  glu_A3_mean_conc100[i] <- (glu_A3_mean_lum100[i] - glu_intercept) / glu_slope
}

glu_A3_data100 <- data.frame(
  glu_A3_dilution_factor100 = glu_A3_dilution100,
  glu_A3_mean_luminescence100 = glu_A3_mean_lum100,
  glu_A3_se100 = glu_A3_se_lum100,
  glu_A3_conc100 =  glu_A3_mean_conc100,
  label = paste0("A3df.", glu_A3_dilution100)
)
glu_A3_mean_lum100
glu_A3_mean_conc100
```

    [1] 393.6667
    [1] -0.007641686

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A4_mean_lum100 <- numeric(length(glu_A4_dilution100))
glu_A4_se_lum100 <- numeric(length(glu_A4_dilution100))
glu_A4_mean_conc100 <- numeric(length(glu_A4_dilution100))

for (i in 1:length(glu_A4_dilution100)) {
  df_val <- glu_A4_dilution100[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A4_lum_values100 <- c(glu_A4_luminescence100[glu_A4_dilution100 == df_val])
  glu_A4_mean_lum100[i] <- mean(glu_A4_lum_values100)
  glu_A4_se_lum100[i] <- sd(glu_A4_lum_values100) / sqrt(length(glu_A4_lum_values100))
  # Calculate concentration from mean luminescence
  glu_A4_mean_conc100[i] <- (glu_A4_mean_lum100[i] - glu_intercept) / glu_slope
}

glu_A4_data100 <- data.frame(
  glu_A4_dilution_factor100 = glu_A4_dilution100,
  glu_A4_mean_luminescence100 = glu_A4_mean_lum100,
  glu_A4_se100 = glu_A4_se_lum100,
  glu_A4_conc100 =  glu_A4_mean_conc100,
  label = paste0("A4df.", glu_A4_dilution100)
)
glu_A4_mean_lum100
glu_A4_mean_conc100
```

    [1] 386.6667
    [1] -0.01230938

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A5_mean_lum100 <- numeric(length(glu_A5_dilution100))
glu_A5_se_lum100 <- numeric(length(glu_A5_dilution100))
glu_A5_mean_conc100 <- numeric(length(glu_A5_dilution100))

for (i in 1:length(glu_A5_dilution100)) {
  df_val <- glu_A5_dilution100[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A5_lum_values100 <- c(glu_A5_luminescence100[glu_A5_dilution100 == df_val])
  glu_A5_mean_lum100[i] <- mean(glu_A5_lum_values100)
  glu_A5_se_lum100[i] <- sd(glu_A5_lum_values100) / sqrt(length(glu_A5_lum_values100))
  # Calculate concentration from mean luminescence
  glu_A5_mean_conc100[i] <- (glu_A5_mean_lum100[i] - glu_intercept) / glu_slope
}

glu_A5_data100 <- data.frame(
  glu_A5_dilution_factor100 = glu_A5_dilution100,
  glu_A5_mean_luminescence100 = glu_A5_mean_lum100,
  glu_A5_se100 = glu_A5_se_lum100,
  glu_A5_conc100 =  glu_A5_mean_conc100,
  label = paste0("A5df.", glu_A5_dilution100)
)
glu_A5_mean_lum100
glu_A5_mean_conc100
```

    [1] 451.3333
    [1] 0.03081121

``` r
# Create the plot
glu_plot <- ggplot(glucose_summary_data, aes(x = glu_concentration, y = glu_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glu_mean_luminescence - glu_se, ymax = glu_mean_luminescence + glu_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +
  
  geom_errorbar(data = glu_A1_data20, aes(x = glu_A1_conc20, y = glu_A1_mean_luminescence20,
                                          ymin = glu_A1_mean_luminescence20 - glu_A1_se20, ymax = glu_A1_mean_luminescence20 + glu_A1_se20,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A1_data20, aes(x = glu_A1_conc20, y = glu_A1_mean_luminescence20,
                                       color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_A2_data20, aes(x = glu_A2_conc20, y = glu_A2_mean_luminescence20,
                                          ymin = glu_A2_mean_luminescence20 - glu_A2_se20, ymax = glu_A2_mean_luminescence20 + glu_A2_se20,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A2_data20, aes(x = glu_A2_conc20, y = glu_A2_mean_luminescence20,
                                       color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_A3_data20, aes(x = glu_A3_conc20, y = glu_A3_mean_luminescence20,
                                          ymin = glu_A3_mean_luminescence20 - glu_A3_se20, ymax = glu_A3_mean_luminescence20 + glu_A3_se20,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A3_data20, aes(x = glu_A3_conc20, y = glu_A3_mean_luminescence20,
                                       color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_A4_data20, aes(x = glu_A4_conc20, y = glu_A4_mean_luminescence20,
                                          ymin = glu_A4_mean_luminescence20 - glu_A4_se20, ymax = glu_A4_mean_luminescence20 + glu_A4_se20,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A4_data20, aes(x = glu_A4_conc20, y = glu_A4_mean_luminescence20,
                                       color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_A5_data20, aes(x = glu_A5_conc20, y = glu_A5_mean_luminescence20,
                                          ymin = glu_A5_mean_luminescence20 - glu_A5_se20, ymax = glu_A5_mean_luminescence20 + glu_A5_se20,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A5_data20, aes(x = glu_A5_conc20, y = glu_A5_mean_luminescence20,
                                       color = label, shape = label), size = 4) +
  
  
  geom_errorbar(data = glu_A1_data100, aes(x = glu_A1_conc100, y = glu_A1_mean_luminescence100,
                                          ymin = glu_A1_mean_luminescence100 - glu_A1_se100, ymax = glu_A1_mean_luminescence100 + glu_A1_se100,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A1_data100, aes(x = glu_A1_conc100, y = glu_A1_mean_luminescence100,
                                       color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_A2_data100, aes(x = glu_A2_conc100, y = glu_A2_mean_luminescence100,
                                          ymin = glu_A2_mean_luminescence100 - glu_A2_se100, ymax = glu_A2_mean_luminescence100 + glu_A2_se100,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A2_data100, aes(x = glu_A2_conc100, y = glu_A2_mean_luminescence100,
                                       color = label, shape = label), size = 4) +
  
  
  geom_errorbar(data = glu_A3_data100, aes(x = glu_A3_conc100, y = glu_A3_mean_luminescence100,
                                          ymin = glu_A3_mean_luminescence100 - glu_A3_se100, ymax = glu_A3_mean_luminescence100 + glu_A3_se100,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A3_data100, aes(x = glu_A3_conc100, y = glu_A3_mean_luminescence100,
                                       color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_A4_data100, aes(x = glu_A4_conc100, y = glu_A4_mean_luminescence100,
                                          ymin = glu_A4_mean_luminescence100 - glu_A4_se100, ymax = glu_A4_mean_luminescence100 + glu_A4_se100,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A4_data100, aes(x = glu_A4_conc100, y = glu_A4_mean_luminescence100,
                                       color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_A5_data100, aes(x = glu_A5_conc100, y = glu_A5_mean_luminescence100,
                                          ymin = glu_A5_mean_luminescence100 - glu_A5_se100, ymax = glu_A5_mean_luminescence100 + glu_A5_se100,
                                          color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A5_data100, aes(x = glu_A5_conc100, y = glu_A5_mean_luminescence100,
                                       color = label, shape = label), size = 4) +

  
  
  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                                "A1df.20" = "darkred",
                                "A2df.20" = "darkorange",
                                "A3df.20" = "darkgreen",
                                "A4df.20" = "purple",
                                "A5df.20" = "brown",
                                "A1df.100" = "green3",
                                "A2df.100" = "firebrick1",
                                "A3df.100" = "cyan",
                                "A4df.100" = "yellow3",
                                "A5df.100" = "thistle3"
                     )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                "A1df.20" = 17,
                                "A2df.20" = 15,
                                "A3df.20" = 18,
                                "A4df.20" = 8,
                                "A5df.20" = 4,
                                "A1df.100" = 5,
                                "A2df.100" = 0,
                                "A3df.100" = 2,
                                "A4df.100" = 19,
                                "A5df.100" = 20
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

![](Gen5-20260130-mgig-glycogenglo_files/figure-gfm/plot%20glucose%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glu_A1_data20
glu_A2_data20
glu_A3_data20
glu_A4_data20
glu_A5_data20
glu_A1_data100
glu_A2_data100
glu_A3_data100
glu_A4_data100
glu_A5_data100


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

      glu_A1_dilution_factor20 glu_A1_mean_luminescence20 glu_A1_se20 glu_A1_conc20
    1                       20                   670.6667    44.25808     0.1770656
        label
    1 A1df.20
      glu_A2_dilution_factor20 glu_A2_mean_luminescence20 glu_A2_se20 glu_A2_conc20
    1                       20                   554.6667    36.08478    0.09971525
        label
    1 A2df.20
      glu_A3_dilution_factor20 glu_A3_mean_luminescence20 glu_A3_se20 glu_A3_conc20
    1                       20                        486    67.53024     0.0539274
        label
    1 A3df.20
      glu_A4_dilution_factor20 glu_A4_mean_luminescence20 glu_A4_se20 glu_A4_conc20
    1                       20                   506.6667    65.54727    0.06770821
        label
    1 A4df.20
      glu_A5_dilution_factor20 glu_A5_mean_luminescence20 glu_A5_se20 glu_A5_conc20
    1                       20                   700.3333    34.03103     0.1968477
        label
    1 A5df.20
      glu_A1_dilution_factor100 glu_A1_mean_luminescence100 glu_A1_se100
    1                       100                         467     10.40833
      glu_A1_conc100    label
    1     0.04125795 A1df.100
      glu_A2_dilution_factor100 glu_A2_mean_luminescence100 glu_A2_se100
    1                       100                         390     6.557439
      glu_A2_conc100    label
    1    -0.01008667 A2df.100
      glu_A3_dilution_factor100 glu_A3_mean_luminescence100 glu_A3_se100
    1                       100                    393.6667     8.950481
      glu_A3_conc100    label
    1   -0.007641686 A3df.100
      glu_A4_dilution_factor100 glu_A4_mean_luminescence100 glu_A4_se100
    1                       100                    386.6667     29.07653
      glu_A4_conc100    label
    1    -0.01230938 A4df.100
      glu_A5_dilution_factor100 glu_A5_mean_luminescence100 glu_A5_se100
    1                       100                    451.3333     2.403701
      glu_A5_conc100    label
    1     0.03081121 A5df.100
    glucose Standard Curve Summary:
    ==================================================
    Concentration: 100 µg/µL
      Mean Luminescence: 150343.33
      Standard Error: 387.37
      CV%: 0.45%

    Concentration: 10 µg/µL
      Mean Luminescence: 15711.67
      Standard Error: 333.44
      CV%: 3.68%

    Concentration: 1 µg/µL
      Mean Luminescence: 1695.33
      Standard Error: 32.95
      CV%: 3.37%

    Concentration: 0.1 µg/µL
      Mean Luminescence: 486.00
      Standard Error: 35.79
      CV%: 12.76%

    Concentration: 0 µg/µL
      Mean Luminescence: 402.67
      Standard Error: 2.60
      CV%: 1.12%

``` r
tab <- matrix(c(glu_A1_dilution20, glu_A1_mean_lum20, glu_A1_mean_conc20,  (glu_A1_dilution20*glu_A1_mean_conc20), 
                glu_A2_dilution20, glu_A2_mean_lum20, glu_A2_mean_conc20,  (glu_A2_dilution20*glu_A2_mean_conc20),
                glu_A3_dilution20, glu_A3_mean_lum20, glu_A3_mean_conc20,  (glu_A3_dilution20*glu_A3_mean_conc20),
                glu_A4_dilution20, glu_A4_mean_lum20, glu_A4_mean_conc20,  (glu_A4_dilution20*glu_A4_mean_conc20),
                glu_A5_dilution20, glu_A5_mean_lum20, glu_A5_mean_conc20,  (glu_A5_dilution20*glu_A5_mean_conc20),
                glu_A1_dilution100, glu_A1_mean_lum100, glu_A1_mean_conc100,  (glu_A1_dilution100*glu_A1_mean_conc100), 
                glu_A2_dilution100, glu_A2_mean_lum100, glu_A2_mean_conc100,  (glu_A2_dilution100*glu_A2_mean_conc100),
                glu_A3_dilution100, glu_A3_mean_lum100, glu_A3_mean_conc100,  (glu_A3_dilution100*glu_A3_mean_conc100),
                glu_A4_dilution100, glu_A4_mean_lum100, glu_A4_mean_conc100,  (glu_A4_dilution100*glu_A4_mean_conc100),
                glu_A5_dilution100, glu_A5_mean_lum100, glu_A5_mean_conc100,  (glu_A5_dilution100*glu_A5_mean_conc100)), ncol=4, byrow=TRUE)
colnames(tab) <- c('Dilution factor','Luminescence','Calculated Glycogen (ug/uL)', 'Total glucose (ug/uL)')
rownames(tab) <- c('A1.df20','A2.df20','A3.df20','A4.df20','A5.df20','A1.df100','A2.df100','A3.df100','A4.df100','Af.df100' )
tab <- as.table(tab)
tab
```

             Dilution factor  Luminescence Calculated Glycogen (ug/uL)
    A1.df20     20.000000000 670.666666667                 0.177065583
    A2.df20     20.000000000 554.666666667                 0.099715247
    A3.df20     20.000000000 486.000000000                 0.053927404
    A4.df20     20.000000000 506.666666667                 0.067708211
    A5.df20     20.000000000 700.333333333                 0.196847710
    A1.df100   100.000000000 467.000000000                 0.041257952
    A2.df100   100.000000000 390.000000000                -0.010086668
    A3.df100   100.000000000 393.666666667                -0.007641686
    A4.df100   100.000000000 386.666666667                -0.012309379
    Af.df100   100.000000000 451.333333333                 0.030811211
             Total glucose (ug/uL)
    A1.df20            3.541311665
    A2.df20            1.994304932
    A3.df20            1.078548073
    A4.df20            1.354164215
    A5.df20            3.936954192
    A1.df100           4.125795195
    A2.df100          -1.008666807
    A3.df100          -0.764168616
    A4.df100          -1.230937889
    Af.df100           3.081121108
