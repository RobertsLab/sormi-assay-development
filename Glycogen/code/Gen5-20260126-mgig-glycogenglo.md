Gen5-20260126-mgig-glycogenglo
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
plate_layout <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-Gen5-20260126-mgig-glycogenglo.csv", header = FALSE)
raw_luminescence <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-Gen5-20260126-mgig-glycogenglo.csv", header = FALSE)

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
     $ V4 : chr  "A1-glyc-25-df.4" "A2-glyc-50-df.4" "A3-glyc-14-df.4" "A4-glyc-21-df.4" ...
     $ V5 : chr  "A1-glyc-25-df.4" "A2-glyc-50-df.4" "A3-glyc-14-df.4" "A4-glyc-21-df.4" ...
     $ V6 : chr  "A1-glyc-25-df.4" "A2-glyc-50-df.4" "A3-glyc-14-df.4" "A4-glyc-21-df.4" ...
     $ V7 : chr  "A1-glu-25-df.2" "A2-glu-50-df.2" "A3-glu-14-df.4" "A4-glu-21-df.2" ...
     $ V8 : chr  "A1-glu-25-df.2" "A2-glu-50-df.2" "A3-glu-14-df.4" "A4-glu-21-df.2" ...
     $ V9 : chr  "A1-glu-25-df.2" "A2-glu-50-df.2" "A3-glu-14-df.4" "A4-glu-21-df.2" ...
     $ V10: chr  "A1-glu-25-df.4" "A2-glu-50-df.4" "A3-glu-14-df.4" "A4-glu-21-df.4" ...
     $ V11: chr  "A1-glu-25-df.4" "A2-glu-50-df.4" "A3-glu-14-df.4" "A4-glu-21-df.4" ...
     $ V12: chr  "A1-glu-25-df.4" "A2-glu-50-df.4" "A3-glu-14-df.4" "A4-glu-21-df.4" ...


    Raw luminescence:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : int  143812 147908 125829 140429 142510 110151 112651 99683
     $ V2 : int  141349 147869 127277 155864 144097 5805 6002 4879
     $ V3 : int  144808 147082 130280 151956 145982 809 775 759
     $ V4 : int  141567 145223 69352 127963 143395 603 535 500
     $ V5 : int  147471 144960 69836 135731 159390 453 370 440
     $ V6 : int  166397 148216 73758 136132 162756 NA NA NA
     $ V7 : int  1233 733 378 304 1205 126683 124267 129340
     $ V8 : int  1010 411 325 215 803 7986 8034 8242
     $ V9 : int  1108 619 355 223 1053 636 691 673
     $ V10: int  558 261 254 209 651 205 208 272
     $ V11: int  485 292 242 202 653 155 175 221
     $ V12: int  591 303 236 220 622 NA NA NA

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
glyc_sample_cols2 <- c(1,2,3)
glyc_A1_dilution2 <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 1]))
glyc_A1_luminescence2 <- as.numeric(raw_luminescence[1, glyc_sample_cols2])

glyc_A2_dilution2 <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 1]))
glyc_A2_luminescence2 <- as.numeric(raw_luminescence[2, glyc_sample_cols2])

glyc_A3_dilution2 <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 1]))
glyc_A3_luminescence2 <- as.numeric(raw_luminescence[3, glyc_sample_cols2])

glyc_A4_dilution2 <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 1]))
glyc_A4_luminescence2 <- as.numeric(raw_luminescence[4, glyc_sample_cols2])

glyc_A5_dilution2 <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 1]))
glyc_A5_luminescence2 <- as.numeric(raw_luminescence[5, glyc_sample_cols2])


glyc_sample_cols4 <- c(4,5,6)
glyc_A1_dilution4 <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 4]))
glyc_A1_luminescence4 <- as.numeric(raw_luminescence[1, glyc_sample_cols4])

glyc_A2_dilution4 <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 4]))
glyc_A2_luminescence4 <- as.numeric(raw_luminescence[2, glyc_sample_cols4])

glyc_A3_dilution4 <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 4]))
glyc_A3_luminescence4 <- as.numeric(raw_luminescence[3, glyc_sample_cols4])

glyc_A4_dilution4 <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 4]))
glyc_A4_luminescence4 <- as.numeric(raw_luminescence[4, glyc_sample_cols4])

glyc_A5_dilution4 <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 4]))
glyc_A5_luminescence4 <- as.numeric(raw_luminescence[5, glyc_sample_cols4])
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
glyc_A1_mean_lum2 <- numeric(length(glyc_A1_dilution2))
glyc_A1_se_lum2 <- numeric(length(glyc_A1_dilution2))
glyc_A1_mean_conc2 <- numeric(length(glyc_A1_dilution2))

for (i in 1:length(glyc_A1_dilution2)) {
  df_val <- glyc_A1_dilution2[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A1_lum_values2 <- c(glyc_A1_luminescence2[glyc_A1_dilution2 == df_val])
  glyc_A1_mean_lum2[i] <- mean(glyc_A1_lum_values2)
  glyc_A1_se_lum2[i] <- sd(glyc_A1_lum_values2) / sqrt(length(glyc_A1_lum_values2))
  # Calculate concentration from mean luminescence
  glyc_A1_mean_conc2[i] <- (glyc_A1_mean_lum2[i] - glyc_intercept) / glyc_slope
}

glyc_A1_data2 <- data.frame(
  glyc_A1_dilution_factor2 = glyc_A1_dilution2,
  glyc_A1_mean_luminescence2 = glyc_A1_mean_lum2,
  glyc_A1_se2 = glyc_A1_se_lum2,
  glyc_A1_conc2 =  glyc_A1_mean_conc2,
  label = paste0("A1df.", glyc_A1_dilution2)
)
glyc_A1_mean_lum2
glyc_A1_mean_conc2
```

    [1] 143323
    [1] 26.70077

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A2_mean_lum2 <- numeric(length(glyc_A2_dilution2))
glyc_A2_se_lum2 <- numeric(length(glyc_A2_dilution2))
glyc_A2_mean_conc2 <- numeric(length(glyc_A2_dilution2))

for (i in 1:length(glyc_A2_dilution2)) {
  df_val <- glyc_A2_dilution2[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A2_lum_values2 <- c(glyc_A2_luminescence2[glyc_A2_dilution2 == df_val])
  glyc_A2_mean_lum2[i] <- mean(glyc_A2_lum_values2)
  glyc_A2_se_lum2[i] <- sd(glyc_A2_lum_values2) / sqrt(length(glyc_A2_lum_values2))
  # Calculate concentration from mean luminescence
  glyc_A2_mean_conc2[i] <- (glyc_A2_mean_lum2[i] - glyc_intercept) / glyc_slope
}

glyc_A2_data2 <- data.frame(
  glyc_A2_dilution_factor2 = glyc_A2_dilution2,
  glyc_A2_mean_luminescence2 = glyc_A2_mean_lum2,
  glyc_A2_se2 = glyc_A2_se_lum2,
  glyc_A2_conc2 =  glyc_A2_mean_conc2,
  label = paste0("A2df.", glyc_A2_dilution2)
)
glyc_A2_mean_lum2
glyc_A2_mean_conc2
```

    [1] 147619.7
    [1] 27.49529

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A3_mean_lum2 <- numeric(length(glyc_A3_dilution2))
glyc_A3_se_lum2 <- numeric(length(glyc_A3_dilution2))
glyc_A3_mean_conc2 <- numeric(length(glyc_A3_dilution2))

for (i in 1:length(glyc_A3_dilution2)) {
  df_val <- glyc_A3_dilution2[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A3_lum_values2 <- c(glyc_A3_luminescence2[glyc_A3_dilution2 == df_val])
  glyc_A3_mean_lum2[i] <- mean(glyc_A3_lum_values2)
  glyc_A3_se_lum2[i] <- sd(glyc_A3_lum_values2) / sqrt(length(glyc_A3_lum_values2))
  # Calculate concentration from mean luminescence
  glyc_A3_mean_conc2[i] <- (glyc_A3_mean_lum2[i] - glyc_intercept) / glyc_slope
}

glyc_A3_data2 <- data.frame(
  glyc_A3_dilution_factor2 = glyc_A3_dilution2,
  glyc_A3_mean_luminescence2 = glyc_A3_mean_lum2,
  glyc_A3_se2 = glyc_A3_se_lum2,
  glyc_A3_conc2 =  glyc_A3_mean_conc2,
  label = paste0("A3df.", glyc_A3_dilution2)
)
glyc_A3_mean_lum2
glyc_A3_mean_conc2
```

    [1] 127795.3
    [1] 23.82947

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A4_mean_lum2 <- numeric(length(glyc_A4_dilution2))
glyc_A4_se_lum2 <- numeric(length(glyc_A4_dilution2))
glyc_A4_mean_conc2 <- numeric(length(glyc_A4_dilution2))

for (i in 1:length(glyc_A4_dilution2)) {
  df_val <- glyc_A4_dilution2[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A4_lum_values2 <- c(glyc_A4_luminescence2[glyc_A4_dilution2 == df_val])
  glyc_A4_mean_lum2[i] <- mean(glyc_A4_lum_values2)
  glyc_A4_se_lum2[i] <- sd(glyc_A4_lum_values2) / sqrt(length(glyc_A4_lum_values2))
  # Calculate concentration from mean luminescence
  glyc_A4_mean_conc2[i] <- (glyc_A4_mean_lum2[i] - glyc_intercept) / glyc_slope
}

glyc_A4_data2 <- data.frame(
  glyc_A4_dilution_factor2 = glyc_A4_dilution2,
  glyc_A4_mean_luminescence2 = glyc_A4_mean_lum2,
  glyc_A4_se2 = glyc_A4_se_lum2,
  glyc_A4_conc2 =  glyc_A4_mean_conc2,
  label = paste0("A4df.", glyc_A4_dilution2)
)
glyc_A4_mean_lum2
glyc_A4_mean_conc2
```

    [1] 149416.3
    [1] 27.82752

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A5_mean_lum2 <- numeric(length(glyc_A5_dilution2))
glyc_A5_se_lum2 <- numeric(length(glyc_A5_dilution2))
glyc_A5_mean_conc2 <- numeric(length(glyc_A5_dilution2))

for (i in 1:length(glyc_A5_dilution2)) {
  df_val <- glyc_A5_dilution2[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A5_lum_values2 <- c(glyc_A5_luminescence2[glyc_A5_dilution2 == df_val])
  glyc_A5_mean_lum2[i] <- mean(glyc_A5_lum_values2)
  glyc_A5_se_lum2[i] <- sd(glyc_A5_lum_values2) / sqrt(length(glyc_A5_lum_values2))
  # Calculate concentration from mean luminescence
  glyc_A5_mean_conc2[i] <- (glyc_A5_mean_lum2[i] - glyc_intercept) / glyc_slope
}

glyc_A5_data2 <- data.frame(
  glyc_A5_dilution_factor2 = glyc_A5_dilution2,
  glyc_A5_mean_luminescence2 = glyc_A5_mean_lum2,
  glyc_A5_se2 = glyc_A5_se_lum2,
  glyc_A5_conc2 =  glyc_A5_mean_conc2,
  label = paste0("A5df.", glyc_A5_dilution2)
)
glyc_A5_mean_lum2
glyc_A5_mean_conc2
```

    [1] 144196.3
    [1] 26.86227

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A1_mean_lum4 <- numeric(length(glyc_A1_dilution4))
glyc_A1_se_lum4 <- numeric(length(glyc_A1_dilution4))
glyc_A1_mean_conc4 <- numeric(length(glyc_A1_dilution4))

for (i in 1:length(glyc_A1_dilution4)) {
  df_val <- glyc_A1_dilution4[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A1_lum_values4 <- c(glyc_A1_luminescence4[glyc_A1_dilution4 == df_val])
  glyc_A1_mean_lum4[i] <- mean(glyc_A1_lum_values4)
  glyc_A1_se_lum4[i] <- sd(glyc_A1_lum_values4) / sqrt(length(glyc_A1_lum_values4))
  # Calculate concentration from mean luminescence
  glyc_A1_mean_conc4[i] <- (glyc_A1_mean_lum4[i] - glyc_intercept) / glyc_slope
}

glyc_A1_data4 <- data.frame(
  glyc_A1_dilution_factor4 = glyc_A1_dilution4,
  glyc_A1_mean_luminescence4 = glyc_A1_mean_lum4,
  glyc_A1_se4 = glyc_A1_se_lum4,
  glyc_A1_conc4 =  glyc_A1_mean_conc4,
  label = paste0("A1df.", glyc_A1_dilution4)
)
glyc_A1_mean_lum4
glyc_A1_mean_conc4
```

    [1] 151811.7
    [1] 28.27046

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A2_mean_lum4 <- numeric(length(glyc_A2_dilution4))
glyc_A2_se_lum4 <- numeric(length(glyc_A2_dilution4))
glyc_A2_mean_conc4 <- numeric(length(glyc_A2_dilution4))

for (i in 1:length(glyc_A2_dilution4)) {
  df_val <- glyc_A2_dilution4[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A2_lum_values4 <- c(glyc_A2_luminescence4[glyc_A2_dilution4 == df_val])
  glyc_A2_mean_lum4[i] <- mean(glyc_A2_lum_values4)
  glyc_A2_se_lum4[i] <- sd(glyc_A2_lum_values4) / sqrt(length(glyc_A2_lum_values4))
  # Calculate concentration from mean luminescence
  glyc_A2_mean_conc4[i] <- (glyc_A2_mean_lum4[i] - glyc_intercept) / glyc_slope
}

glyc_A2_data4 <- data.frame(
  glyc_A2_dilution_factor4 = glyc_A2_dilution4,
  glyc_A2_mean_luminescence4 = glyc_A2_mean_lum4,
  glyc_A2_se4 = glyc_A2_se_lum4,
  glyc_A2_conc4 =  glyc_A2_mean_conc4,
  label = paste0("A2df.", glyc_A2_dilution4)
)
glyc_A2_mean_lum4
glyc_A2_mean_conc4
```

    [1] 146133
    [1] 27.22039

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A3_mean_lum4 <- numeric(length(glyc_A3_dilution4))
glyc_A3_se_lum4 <- numeric(length(glyc_A3_dilution4))
glyc_A3_mean_conc4 <- numeric(length(glyc_A3_dilution4))

for (i in 1:length(glyc_A3_dilution4)) {
  df_val <- glyc_A3_dilution4[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A3_lum_values4 <- c(glyc_A3_luminescence4[glyc_A3_dilution4 == df_val])
  glyc_A3_mean_lum4[i] <- mean(glyc_A3_lum_values4)
  glyc_A3_se_lum4[i] <- sd(glyc_A3_lum_values4) / sqrt(length(glyc_A3_lum_values4))
  # Calculate concentration from mean luminescence
  glyc_A3_mean_conc4[i] <- (glyc_A3_mean_lum4[i] - glyc_intercept) / glyc_slope
}

glyc_A3_data4 <- data.frame(
  glyc_A3_dilution_factor4 = glyc_A3_dilution4,
  glyc_A3_mean_luminescence4 = glyc_A3_mean_lum4,
  glyc_A3_se4 = glyc_A3_se_lum4,
  glyc_A3_conc4 =  glyc_A3_mean_conc4,
  label = paste0("A3df.", glyc_A3_dilution4)
)
glyc_A3_mean_lum4
glyc_A3_mean_conc4
```

    [1] 70982
    [1] 13.32382

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A4_mean_lum4 <- numeric(length(glyc_A4_dilution4))
glyc_A4_se_lum4 <- numeric(length(glyc_A4_dilution4))
glyc_A4_mean_conc4 <- numeric(length(glyc_A4_dilution4))

for (i in 1:length(glyc_A4_dilution4)) {
  df_val <- glyc_A4_dilution4[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A4_lum_values4 <- c(glyc_A4_luminescence4[glyc_A4_dilution4 == df_val])
  glyc_A4_mean_lum4[i] <- mean(glyc_A4_lum_values4)
  glyc_A4_se_lum4[i] <- sd(glyc_A4_lum_values4) / sqrt(length(glyc_A4_lum_values4))
  # Calculate concentration from mean luminescence
  glyc_A4_mean_conc4[i] <- (glyc_A4_mean_lum4[i] - glyc_intercept) / glyc_slope
}

glyc_A4_data4 <- data.frame(
  glyc_A4_dilution_factor4 = glyc_A4_dilution4,
  glyc_A4_mean_luminescence4 = glyc_A4_mean_lum4,
  glyc_A4_se4 = glyc_A4_se_lum4,
  glyc_A4_conc4 =  glyc_A4_mean_conc4,
  label = paste0("A4df.", glyc_A4_dilution4)
)
glyc_A4_mean_lum4
glyc_A4_mean_conc4
```

    [1] 133275.3
    [1] 24.84281

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_A5_mean_lum4 <- numeric(length(glyc_A5_dilution4))
glyc_A5_se_lum4 <- numeric(length(glyc_A5_dilution4))
glyc_A5_mean_conc4 <- numeric(length(glyc_A5_dilution4))

for (i in 1:length(glyc_A5_dilution4)) {
  df_val <- glyc_A5_dilution4[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_A5_lum_values4 <- c(glyc_A5_luminescence4[glyc_A5_dilution4 == df_val])
  glyc_A5_mean_lum4[i] <- mean(glyc_A5_lum_values4)
  glyc_A5_se_lum4[i] <- sd(glyc_A5_lum_values4) / sqrt(length(glyc_A5_lum_values4))
  # Calculate concentration from mean luminescence
  glyc_A5_mean_conc4[i] <- (glyc_A5_mean_lum4[i] - glyc_intercept) / glyc_slope
}

glyc_A5_data4 <- data.frame(
  glyc_A5_dilution_factor4 = glyc_A5_dilution4,
  glyc_A5_mean_luminescence4 = glyc_A5_mean_lum4,
  glyc_A5_se4 = glyc_A5_se_lum4,
  glyc_A5_conc4 =  glyc_A5_mean_conc4,
  label = paste0("A5df.", glyc_A5_dilution4)
)
glyc_A5_mean_lum4
glyc_A5_mean_conc4
```

    [1] 155180.3
    [1] 28.89338

``` r
# Create the plot
glyc_plot <- ggplot(glycogen_summary_data, aes(x = glyc_concentration, y = glyc_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glyc_mean_luminescence - glyc_se, ymax = glyc_mean_luminescence + glyc_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +
  
  geom_errorbar(data = glyc_A1_data2, aes(x = glyc_A1_conc2, y = glyc_A1_mean_luminescence2,
                ymin = glyc_A1_mean_luminescence2 - glyc_A1_se2, ymax = glyc_A1_mean_luminescence2 + glyc_A1_se2,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A1_data2, aes(x = glyc_A1_conc2, y = glyc_A1_mean_luminescence2,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_A2_data2, aes(x = glyc_A2_conc2, y = glyc_A2_mean_luminescence2,
                ymin = glyc_A2_mean_luminescence2 - glyc_A2_se2, ymax = glyc_A2_mean_luminescence2 + glyc_A2_se2,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A2_data2, aes(x = glyc_A2_conc2, y = glyc_A2_mean_luminescence2,
             color = label, shape = label), size = 4) +

  geom_errorbar(data = glyc_A3_data2, aes(x = glyc_A3_conc2, y = glyc_A3_mean_luminescence2,
                ymin = glyc_A3_mean_luminescence2 - glyc_A3_se2, ymax = glyc_A3_mean_luminescence2 + glyc_A3_se2,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A3_data2, aes(x = glyc_A3_conc2, y = glyc_A3_mean_luminescence2,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_A4_data2, aes(x = glyc_A4_conc2, y = glyc_A4_mean_luminescence2,
                ymin = glyc_A4_mean_luminescence2 - glyc_A4_se2, ymax = glyc_A4_mean_luminescence2 + glyc_A4_se2,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A4_data2, aes(x = glyc_A4_conc2, y = glyc_A4_mean_luminescence2,
             color = label, shape = label), size = 4) +

  geom_errorbar(data = glyc_A5_data2, aes(x = glyc_A5_conc2, y = glyc_A5_mean_luminescence2,
                ymin = glyc_A5_mean_luminescence2 - glyc_A5_se2, ymax = glyc_A5_mean_luminescence2 + glyc_A5_se2,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A5_data2, aes(x = glyc_A5_conc2, y = glyc_A5_mean_luminescence2,
             color = label, shape = label), size = 4) +

  
    geom_errorbar(data = glyc_A1_data4, aes(x = glyc_A1_conc4, y = glyc_A1_mean_luminescence4,
                ymin = glyc_A1_mean_luminescence4 - glyc_A1_se4, ymax = glyc_A1_mean_luminescence4 + glyc_A1_se4,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A1_data4, aes(x = glyc_A1_conc4, y = glyc_A1_mean_luminescence4,
             color = label, shape = label), size = 4) +
  
   geom_errorbar(data = glyc_A2_data4, aes(x = glyc_A2_conc4, y = glyc_A2_mean_luminescence4,
                ymin = glyc_A2_mean_luminescence4 - glyc_A2_se4, ymax = glyc_A2_mean_luminescence4 + glyc_A2_se4,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A2_data4, aes(x = glyc_A2_conc4, y = glyc_A2_mean_luminescence4,
             color = label, shape = label), size = 4) +


  geom_errorbar(data = glyc_A3_data4, aes(x = glyc_A3_conc4, y = glyc_A3_mean_luminescence4,
                ymin = glyc_A3_mean_luminescence4 - glyc_A3_se4, ymax = glyc_A3_mean_luminescence4 + glyc_A3_se4,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A3_data4, aes(x = glyc_A3_conc4, y = glyc_A3_mean_luminescence4,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_A4_data4, aes(x = glyc_A4_conc4, y = glyc_A4_mean_luminescence4,
                ymin = glyc_A4_mean_luminescence4 - glyc_A4_se4, ymax = glyc_A4_mean_luminescence4 + glyc_A4_se4,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A4_data4, aes(x = glyc_A4_conc4, y = glyc_A4_mean_luminescence4,
             color = label, shape = label), size = 4) +

  geom_errorbar(data = glyc_A5_data4, aes(x = glyc_A5_conc4, y = glyc_A5_mean_luminescence4,
                ymin = glyc_A5_mean_luminescence4 - glyc_A5_se4, ymax = glyc_A5_mean_luminescence4 + glyc_A5_se4,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_A5_data4, aes(x = glyc_A5_conc4, y = glyc_A5_mean_luminescence4,
             color = label, shape = label), size = 4) +
  
  
  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                                "A1df.2" = "darkred",
                                "A2df.2" = "darkorange",
                                "A3df.2" = "darkgreen",
                                "A4df.2" = "purple",
                                "A5df.2" = "brown",
                                "A1df.4" = "green3",
                                "A2df.4" = "firebrick1",
                                "A3df.4" = "cyan",
                                "A4df.4" = "yellow3",
                                "A5df.4" = "thistle3"
                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                 "A1df.2" = 17,
                                 "A2df.2" = 15,
                                 "A3df.2" = 18,
                                 "A4df.2" = 8,
                                 "A5df.2" = 4,
                                 "A1df.4" = 5,
                                 "A2df.4" = 0,
                                 "A3df.4" = 2,
                                 "A4df.4" = 19,
                                 "A5df.4" = 20
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

![](Gen5-20260126-mgig-glycogenglo_files/figure-gfm/plot%20glycogen%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glyc_A1_data2
glyc_A2_data2
glyc_A3_data2
glyc_A4_data2
glyc_A5_data2
glyc_A1_data4
glyc_A2_data4
glyc_A3_data4
glyc_A4_data4
glyc_A5_data4


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

      glyc_A1_dilution_factor2 glyc_A1_mean_luminescence2 glyc_A1_se2 glyc_A1_conc2
    1                        2                     143323    1028.026      26.70077
       label
    1 A1df.2
      glyc_A2_dilution_factor2 glyc_A2_mean_luminescence2 glyc_A2_se2 glyc_A2_conc2
    1                        2                   147619.7     269.069      27.49529
       label
    1 A2df.2
      glyc_A3_dilution_factor2 glyc_A3_mean_luminescence2 glyc_A3_se2 glyc_A3_conc2
    1                        2                   127795.3     1310.77      23.82947
       label
    1 A3df.2
      glyc_A4_dilution_factor2 glyc_A4_mean_luminescence2 glyc_A4_se2 glyc_A4_conc2
    1                        2                   149416.3    4633.114      27.82752
       label
    1 A4df.2
      glyc_A5_dilution_factor2 glyc_A5_mean_luminescence2 glyc_A5_se2 glyc_A5_conc2
    1                        2                   144196.3     1003.51      26.86227
       label
    1 A5df.2
      glyc_A1_dilution_factor4 glyc_A1_mean_luminescence4 glyc_A1_se4 glyc_A1_conc4
    1                        4                   151811.7    7489.176      28.27046
       label
    1 A1df.4
      glyc_A2_dilution_factor4 glyc_A2_mean_luminescence4 glyc_A2_se4 glyc_A2_conc4
    1                        4                     146133    1044.264      27.22039
       label
    1 A2df.4
      glyc_A3_dilution_factor4 glyc_A3_mean_luminescence4 glyc_A3_se4 glyc_A3_conc4
    1                        4                      70982    1395.014      13.32382
       label
    1 A3df.4
      glyc_A4_dilution_factor4 glyc_A4_mean_luminescence4 glyc_A4_se4 glyc_A4_conc4
    1                        4                   133275.3    2658.688      24.84281
       label
    1 A4df.4
      glyc_A5_dilution_factor4 glyc_A5_mean_luminescence4 glyc_A5_se4 glyc_A5_conc4
    1                        4                   155180.3    5972.243      28.89338
       label
    1 A5df.4
    glycogen Standard Curve Summary:
    ==================================================
    Concentration: 20 µg/µL
      Mean Luminescence: 107495.00
      Standard Error: 3972.11
      CV%: 6.40%

    Concentration: 2 µg/µL
      Mean Luminescence: 5562.00
      Standard Error: 346.20
      CV%: 10.78%

    Concentration: 0.2 µg/µL
      Mean Luminescence: 781.00
      Standard Error: 14.74
      CV%: 3.27%

    Concentration: 0.02 µg/µL
      Mean Luminescence: 546.00
      Standard Error: 30.24
      CV%: 9.59%

    Concentration: 0 µg/µL
      Mean Luminescence: 421.00
      Standard Error: 25.77
      CV%: 10.60%

``` r
tab <- matrix(c(glyc_A1_dilution2, glyc_A1_mean_lum2, glyc_A1_mean_conc2,  (glyc_A1_dilution2*glyc_A1_mean_conc2), 
                glyc_A2_dilution2, glyc_A2_mean_lum2, glyc_A2_mean_conc2,  (glyc_A2_dilution2*glyc_A2_mean_conc2),
                glyc_A3_dilution2, glyc_A3_mean_lum2, glyc_A3_mean_conc2,  (glyc_A3_dilution2*glyc_A3_mean_conc2),
                glyc_A4_dilution2, glyc_A4_mean_lum2, glyc_A4_mean_conc2,  (glyc_A4_dilution2*glyc_A4_mean_conc2),
                glyc_A5_dilution2, glyc_A5_mean_lum2, glyc_A5_mean_conc2,  (glyc_A5_dilution2*glyc_A5_mean_conc2),
                glyc_A1_dilution4, glyc_A1_mean_lum4, glyc_A1_mean_conc4,  (glyc_A1_dilution4*glyc_A1_mean_conc4), 
                glyc_A2_dilution4, glyc_A2_mean_lum4, glyc_A2_mean_conc4,  (glyc_A2_dilution4*glyc_A2_mean_conc4),
                glyc_A3_dilution4, glyc_A3_mean_lum4, glyc_A3_mean_conc4,  (glyc_A3_dilution4*glyc_A3_mean_conc4),
                glyc_A4_dilution4, glyc_A4_mean_lum4, glyc_A4_mean_conc4,  (glyc_A4_dilution4*glyc_A4_mean_conc4),
                glyc_A5_dilution4, glyc_A5_mean_lum4, glyc_A5_mean_conc4,  (glyc_A5_dilution4*glyc_A5_mean_conc4)), ncol=4, byrow=TRUE)
colnames(tab) <- c('Dilution factor','Luminescence','Calculated Glycogen (ug/uL)', 'Total glycogen (ug/uL)')
rownames(tab) <- c('A1.df2','A2.df2','A3.df2','A4.df2','A5.df2','A1.df4','A2.df4','A3.df4','A4.df4','Af.df4' )
tab <- as.table(tab)
tab
```

           Dilution factor Luminescence Calculated Glycogen (ug/uL)
    A1.df2         2.00000 143323.00000                    26.70077
    A2.df2         2.00000 147619.66667                    27.49529
    A3.df2         2.00000 127795.33333                    23.82947
    A4.df2         2.00000 149416.33333                    27.82752
    A5.df2         2.00000 144196.33333                    26.86227
    A1.df4         4.00000 151811.66667                    28.27046
    A2.df4         4.00000 146133.00000                    27.22039
    A3.df4         4.00000  70982.00000                    13.32382
    A4.df4         4.00000 133275.33333                    24.84281
    Af.df4         4.00000 155180.33333                    28.89338
           Total glycogen (ug/uL)
    A1.df2               53.40155
    A2.df2               54.99059
    A3.df2               47.65894
    A4.df2               55.65505
    A5.df2               53.72453
    A1.df4              113.08183
    A2.df4              108.88154
    A3.df4               53.29527
    A4.df4               99.37123
    Af.df4              115.57351

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
glu_sample_cols2 <- c(7,8,9)
glu_A1_dilution2 <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 7]))
glu_A1_luminescence2 <- as.numeric(raw_luminescence[1, glu_sample_cols2])

glu_A2_dilution2 <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 7]))
glu_A2_luminescence2 <- as.numeric(raw_luminescence[2, glu_sample_cols2])

glu_A3_dilution2 <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 7]))
glu_A3_luminescence2 <- as.numeric(raw_luminescence[3, glu_sample_cols2])

glu_A4_dilution2 <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 7]))
glu_A4_luminescence2 <- as.numeric(raw_luminescence[4, glu_sample_cols2])

glu_A5_dilution2 <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 7]))
glu_A5_luminescence2 <- as.numeric(raw_luminescence[5, glu_sample_cols2])


glu_sample_cols4 <- c(10,11,12)
glu_A1_dilution4 <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 10]))
glu_A1_luminescence4 <- as.numeric(raw_luminescence[1, glu_sample_cols4])

glu_A2_dilution4 <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 10]))
glu_A2_luminescence4 <- as.numeric(raw_luminescence[2, glu_sample_cols4])

glu_A3_dilution4 <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 10]))
glu_A3_luminescence4 <- as.numeric(raw_luminescence[3, glu_sample_cols4])

glu_A4_dilution4 <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 10]))
glu_A4_luminescence4 <- as.numeric(raw_luminescence[4, glu_sample_cols4])

glu_A5_dilution4 <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 10]))
glu_A5_luminescence4 <- as.numeric(raw_luminescence[5, glu_sample_cols4])
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
glu_A1_mean_lum2 <- numeric(length(glu_A1_dilution2))
glu_A1_se_lum2 <- numeric(length(glu_A1_dilution2))
glu_A1_mean_conc2 <- numeric(length(glu_A1_dilution2))

for (i in 1:length(glu_A1_dilution2)) {
  df_val <- glu_A1_dilution2[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A1_lum_values2 <- c(glu_A1_luminescence2[glu_A1_dilution2 == df_val])
  glu_A1_mean_lum2[i] <- mean(glu_A1_lum_values2)
  glu_A1_se_lum2[i] <- sd(glu_A1_lum_values2) / sqrt(length(glu_A1_lum_values2))
  # Calculate concentration from mean luminescence
  glu_A1_mean_conc2[i] <- (glu_A1_mean_lum2[i] - glu_intercept) / glu_slope
}

glu_A1_data2 <- data.frame(
  glu_A1_dilution_factor2 = glu_A1_dilution2,
  glu_A1_mean_luminescence2 = glu_A1_mean_lum2,
  glu_A1_se2 = glu_A1_se_lum2,
  glu_A1_conc2 =  glu_A1_mean_conc2,
  label = paste0("A1df.", glu_A1_dilution2)
)
glu_A1_mean_lum2
glu_A1_mean_conc2
```

    [1] 1117
    [1] 1.78701

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A2_mean_lum2 <- numeric(length(glu_A2_dilution2))
glu_A2_se_lum2 <- numeric(length(glu_A2_dilution2))
glu_A2_mean_conc2 <- numeric(length(glu_A2_dilution2))

for (i in 1:length(glu_A2_dilution2)) {
  df_val <- glu_A2_dilution2[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A2_lum_values2 <- c(glu_A2_luminescence2[glu_A2_dilution2 == df_val])
  glu_A2_mean_lum2[i] <- mean(glu_A2_lum_values2)
  glu_A2_se_lum2[i] <- sd(glu_A2_lum_values2) / sqrt(length(glu_A2_lum_values2))
  # Calculate concentration from mean luminescence
  glu_A2_mean_conc2[i] <- (glu_A2_mean_lum2[i] - glu_intercept) / glu_slope
}

glu_A2_data2 <- data.frame(
  glu_A2_dilution_factor2 = glu_A2_dilution2,
  glu_A2_mean_luminescence2 = glu_A2_mean_lum2,
  glu_A2_se2 = glu_A2_se_lum2,
  glu_A2_conc2 =  glu_A2_mean_conc2,
  label = paste0("A2df.", glu_A2_dilution2)
)
glu_A2_mean_lum2
glu_A2_mean_conc2
```

    [1] 587.6667
    [1] 1.372115

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A3_mean_lum2 <- numeric(length(glu_A3_dilution2))
glu_A3_se_lum2 <- numeric(length(glu_A3_dilution2))
glu_A3_mean_conc2 <- numeric(length(glu_A3_dilution2))

for (i in 1:length(glu_A3_dilution2)) {
  df_val <- glu_A3_dilution2[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A3_lum_values2 <- c(glu_A3_luminescence2[glu_A3_dilution2 == df_val])
  glu_A3_mean_lum2[i] <- mean(glu_A3_lum_values2)
  glu_A3_se_lum2[i] <- sd(glu_A3_lum_values2) / sqrt(length(glu_A3_lum_values2))
  # Calculate concentration from mean luminescence
  glu_A3_mean_conc2[i] <- (glu_A3_mean_lum2[i] - glu_intercept) / glu_slope
}

glu_A3_data2 <- data.frame(
  glu_A3_dilution_factor2 = glu_A3_dilution2,
  glu_A3_mean_luminescence2 = glu_A3_mean_lum2,
  glu_A3_se2 = glu_A3_se_lum2,
  glu_A3_conc2 =  glu_A3_mean_conc2,
  label = paste0("A3df.", glu_A3_dilution2)
)
glu_A3_mean_lum2
glu_A3_mean_conc2
```

    [1] 352.6667
    [1] 1.18792

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A4_mean_lum2 <- numeric(length(glu_A4_dilution2))
glu_A4_se_lum2 <- numeric(length(glu_A4_dilution2))
glu_A4_mean_conc2 <- numeric(length(glu_A4_dilution2))

for (i in 1:length(glu_A4_dilution2)) {
  df_val <- glu_A4_dilution2[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A4_lum_values2 <- c(glu_A4_luminescence2[glu_A4_dilution2 == df_val])
  glu_A4_mean_lum2[i] <- mean(glu_A4_lum_values2)
  glu_A4_se_lum2[i] <- sd(glu_A4_lum_values2) / sqrt(length(glu_A4_lum_values2))
  # Calculate concentration from mean luminescence
  glu_A4_mean_conc2[i] <- (glu_A4_mean_lum2[i] - glu_intercept) / glu_slope
}

glu_A4_data2 <- data.frame(
  glu_A4_dilution_factor2 = glu_A4_dilution2,
  glu_A4_mean_luminescence2 = glu_A4_mean_lum2,
  glu_A4_se2 = glu_A4_se_lum2,
  glu_A4_conc2 =  glu_A4_mean_conc2,
  label = paste0("A4df.", glu_A4_dilution2)
)
glu_A4_mean_lum2
glu_A4_mean_conc2
```

    [1] 247.3333
    [1] 1.105359

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A5_mean_lum2 <- numeric(length(glu_A5_dilution2))
glu_A5_se_lum2 <- numeric(length(glu_A5_dilution2))
glu_A5_mean_conc2 <- numeric(length(glu_A5_dilution2))

for (i in 1:length(glu_A5_dilution2)) {
  df_val <- glu_A5_dilution2[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A5_lum_values2 <- c(glu_A5_luminescence2[glu_A5_dilution2 == df_val])
  glu_A5_mean_lum2[i] <- mean(glu_A5_lum_values2)
  glu_A5_se_lum2[i] <- sd(glu_A5_lum_values2) / sqrt(length(glu_A5_lum_values2))
  # Calculate concentration from mean luminescence
  glu_A5_mean_conc2[i] <- (glu_A5_mean_lum2[i] - glu_intercept) / glu_slope
}

glu_A5_data2 <- data.frame(
  glu_A5_dilution_factor2 = glu_A5_dilution2,
  glu_A5_mean_luminescence2 = glu_A5_mean_lum2,
  glu_A5_se2 = glu_A5_se_lum2,
  glu_A5_conc2 =  glu_A5_mean_conc2,
  label = paste0("A5df.", glu_A5_dilution2)
)
glu_A5_mean_lum2
glu_A5_mean_conc2
```

    [1] 1020.333
    [1] 1.711242

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A1_mean_lum4 <- numeric(length(glu_A1_dilution4))
glu_A1_se_lum4 <- numeric(length(glu_A1_dilution4))
glu_A1_mean_conc4 <- numeric(length(glu_A1_dilution4))

for (i in 1:length(glu_A1_dilution4)) {
  df_val <- glu_A1_dilution4[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A1_lum_values4 <- c(glu_A1_luminescence4[glu_A1_dilution4 == df_val])
  glu_A1_mean_lum4[i] <- mean(glu_A1_lum_values4)
  glu_A1_se_lum4[i] <- sd(glu_A1_lum_values4) / sqrt(length(glu_A1_lum_values4))
  # Calculate concentration from mean luminescence
  glu_A1_mean_conc4[i] <- (glu_A1_mean_lum4[i] - glu_intercept) / glu_slope
}

glu_A1_data4 <- data.frame(
  glu_A1_dilution_factor4 = glu_A1_dilution4,
  glu_A1_mean_luminescence4 = glu_A1_mean_lum4,
  glu_A1_se4 = glu_A1_se_lum4,
  glu_A1_conc4 =  glu_A1_mean_conc4,
  label = paste0("A1df.", glu_A1_dilution4)
)
glu_A1_mean_lum4
glu_A1_mean_conc4
```

    [1] 544.6667
    [1] 1.338411

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A2_mean_lum4 <- numeric(length(glu_A2_dilution4))
glu_A2_se_lum4 <- numeric(length(glu_A2_dilution4))
glu_A2_mean_conc4 <- numeric(length(glu_A2_dilution4))

for (i in 1:length(glu_A2_dilution4)) {
  df_val <- glu_A2_dilution4[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A2_lum_values4 <- c(glu_A2_luminescence4[glu_A2_dilution4 == df_val])
  glu_A2_mean_lum4[i] <- mean(glu_A2_lum_values4)
  glu_A2_se_lum4[i] <- sd(glu_A2_lum_values4) / sqrt(length(glu_A2_lum_values4))
  # Calculate concentration from mean luminescence
  glu_A2_mean_conc4[i] <- (glu_A2_mean_lum4[i] - glu_intercept) / glu_slope
}

glu_A2_data4 <- data.frame(
  glu_A2_dilution_factor4 = glu_A2_dilution4,
  glu_A2_mean_luminescence4 = glu_A2_mean_lum4,
  glu_A2_se4 = glu_A2_se_lum4,
  glu_A2_conc4 =  glu_A2_mean_conc4,
  label = paste0("A2df.", glu_A2_dilution4)
)
glu_A2_mean_lum4
glu_A2_mean_conc4
```

    [1] 285.3333
    [1] 1.135143

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A3_mean_lum4 <- numeric(length(glu_A3_dilution4))
glu_A3_se_lum4 <- numeric(length(glu_A3_dilution4))
glu_A3_mean_conc4 <- numeric(length(glu_A3_dilution4))

for (i in 1:length(glu_A3_dilution4)) {
  df_val <- glu_A3_dilution4[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A3_lum_values4 <- c(glu_A3_luminescence4[glu_A3_dilution4 == df_val])
  glu_A3_mean_lum4[i] <- mean(glu_A3_lum_values4)
  glu_A3_se_lum4[i] <- sd(glu_A3_lum_values4) / sqrt(length(glu_A3_lum_values4))
  # Calculate concentration from mean luminescence
  glu_A3_mean_conc4[i] <- (glu_A3_mean_lum4[i] - glu_intercept) / glu_slope
}

glu_A3_data4 <- data.frame(
  glu_A3_dilution_factor4 = glu_A3_dilution4,
  glu_A3_mean_luminescence4 = glu_A3_mean_lum4,
  glu_A3_se4 = glu_A3_se_lum4,
  glu_A3_conc4 =  glu_A3_mean_conc4,
  label = paste0("A3df.", glu_A3_dilution4)
)
glu_A3_mean_lum4
glu_A3_mean_conc4
```

    [1] 244
    [1] 1.102746

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A4_mean_lum4 <- numeric(length(glu_A4_dilution4))
glu_A4_se_lum4 <- numeric(length(glu_A4_dilution4))
glu_A4_mean_conc4 <- numeric(length(glu_A4_dilution4))

for (i in 1:length(glu_A4_dilution4)) {
  df_val <- glu_A4_dilution4[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A4_lum_values4 <- c(glu_A4_luminescence4[glu_A4_dilution4 == df_val])
  glu_A4_mean_lum4[i] <- mean(glu_A4_lum_values4)
  glu_A4_se_lum4[i] <- sd(glu_A4_lum_values4) / sqrt(length(glu_A4_lum_values4))
  # Calculate concentration from mean luminescence
  glu_A4_mean_conc4[i] <- (glu_A4_mean_lum4[i] - glu_intercept) / glu_slope
}

glu_A4_data4 <- data.frame(
  glu_A4_dilution_factor4 = glu_A4_dilution4,
  glu_A4_mean_luminescence4 = glu_A4_mean_lum4,
  glu_A4_se4 = glu_A4_se_lum4,
  glu_A4_conc4 =  glu_A4_mean_conc4,
  label = paste0("A4df.", glu_A4_dilution4)
)
glu_A4_mean_lum4
glu_A4_mean_conc4
```

    [1] 210.3333
    [1] 1.076358

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_A5_mean_lum4 <- numeric(length(glu_A5_dilution4))
glu_A5_se_lum4 <- numeric(length(glu_A5_dilution4))
glu_A5_mean_conc4 <- numeric(length(glu_A5_dilution4))

for (i in 1:length(glu_A5_dilution4)) {
  df_val <- glu_A5_dilution4[i]
  # Get luminescence values for this dilution factor from both rows
  glu_A5_lum_values4 <- c(glu_A5_luminescence4[glu_A5_dilution4 == df_val])
  glu_A5_mean_lum4[i] <- mean(glu_A5_lum_values4)
  glu_A5_se_lum4[i] <- sd(glu_A5_lum_values4) / sqrt(length(glu_A5_lum_values4))
  # Calculate concentration from mean luminescence
  glu_A5_mean_conc4[i] <- (glu_A5_mean_lum4[i] - glu_intercept) / glu_slope
}

glu_A5_data4 <- data.frame(
  glu_A5_dilution_factor4 = glu_A5_dilution4,
  glu_A5_mean_luminescence4 = glu_A5_mean_lum4,
  glu_A5_se4 = glu_A5_se_lum4,
  glu_A5_conc4 =  glu_A5_mean_conc4,
  label = paste0("A5df.", glu_A5_dilution4)
)
glu_A5_mean_lum4
glu_A5_mean_conc4
```

    [1] 642
    [1] 1.414702

``` r
# Create the plot
glu_plot <- ggplot(glucose_summary_data, aes(x = glu_concentration, y = glu_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glu_mean_luminescence - glu_se, ymax = glu_mean_luminescence + glu_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +
  
  geom_errorbar(data = glu_A1_data2, aes(x = glu_A1_conc2, y = glu_A1_mean_luminescence2,
                ymin = glu_A1_mean_luminescence2 - glu_A1_se2, ymax = glu_A1_mean_luminescence2 + glu_A1_se2,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A1_data2, aes(x = glu_A1_conc2, y = glu_A1_mean_luminescence2,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_A2_data2, aes(x = glu_A2_conc2, y = glu_A2_mean_luminescence2,
                ymin = glu_A2_mean_luminescence2 - glu_A2_se2, ymax = glu_A2_mean_luminescence2 + glu_A2_se2,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A2_data2, aes(x = glu_A2_conc2, y = glu_A2_mean_luminescence2,
             color = label, shape = label), size = 4) +

  geom_errorbar(data = glu_A3_data2, aes(x = glu_A3_conc2, y = glu_A3_mean_luminescence2,
                ymin = glu_A3_mean_luminescence2 - glu_A3_se2, ymax = glu_A3_mean_luminescence2 + glu_A3_se2,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A3_data2, aes(x = glu_A3_conc2, y = glu_A3_mean_luminescence2,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_A4_data2, aes(x = glu_A4_conc2, y = glu_A4_mean_luminescence2,
                ymin = glu_A4_mean_luminescence2 - glu_A4_se2, ymax = glu_A4_mean_luminescence2 + glu_A4_se2,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A4_data2, aes(x = glu_A4_conc2, y = glu_A4_mean_luminescence2,
             color = label, shape = label), size = 4) +

  geom_errorbar(data = glu_A5_data2, aes(x = glu_A5_conc2, y = glu_A5_mean_luminescence2,
                ymin = glu_A5_mean_luminescence2 - glu_A5_se2, ymax = glu_A5_mean_luminescence2 + glu_A5_se2,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A5_data2, aes(x = glu_A5_conc2, y = glu_A5_mean_luminescence2,
             color = label, shape = label), size = 4) +

  
    geom_errorbar(data = glu_A1_data4, aes(x = glu_A1_conc4, y = glu_A1_mean_luminescence4,
                ymin = glu_A1_mean_luminescence4 - glu_A1_se4, ymax = glu_A1_mean_luminescence4 + glu_A1_se4,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A1_data4, aes(x = glu_A1_conc4, y = glu_A1_mean_luminescence4,
             color = label, shape = label), size = 4) +
  
   geom_errorbar(data = glu_A2_data4, aes(x = glu_A2_conc4, y = glu_A2_mean_luminescence4,
                ymin = glu_A2_mean_luminescence4 - glu_A2_se4, ymax = glu_A2_mean_luminescence4 + glu_A2_se4,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A2_data4, aes(x = glu_A2_conc4, y = glu_A2_mean_luminescence4,
             color = label, shape = label), size = 4) +


  geom_errorbar(data = glu_A3_data4, aes(x = glu_A3_conc4, y = glu_A3_mean_luminescence4,
                ymin = glu_A3_mean_luminescence4 - glu_A3_se4, ymax = glu_A3_mean_luminescence4 + glu_A3_se4,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A3_data4, aes(x = glu_A3_conc4, y = glu_A3_mean_luminescence4,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_A4_data4, aes(x = glu_A4_conc4, y = glu_A4_mean_luminescence4,
                ymin = glu_A4_mean_luminescence4 - glu_A4_se4, ymax = glu_A4_mean_luminescence4 + glu_A4_se4,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A4_data4, aes(x = glu_A4_conc4, y = glu_A4_mean_luminescence4,
             color = label, shape = label), size = 4) +

  geom_errorbar(data = glu_A5_data4, aes(x = glu_A5_conc4, y = glu_A5_mean_luminescence4,
                ymin = glu_A5_mean_luminescence4 - glu_A5_se4, ymax = glu_A5_mean_luminescence4 + glu_A5_se4,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_A5_data4, aes(x = glu_A5_conc4, y = glu_A5_mean_luminescence4,
             color = label, shape = label), size = 4) +
  
  
  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                                "A1df.2" = "darkred",
                                "A2df.2" = "darkorange",
                                "A3df.2" = "darkgreen",
                                "A4df.2" = "purple",
                                "A5df.2" = "brown",
                                "A1df.4" = "green3",
                                "A2df.4" = "firebrick1",
                                "A3df.4" = "cyan",
                                "A4df.4" = "yellow3",
                                "A5df.4" = "thistle3"
                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                 "A1df.2" = 17,
                                 "A2df.2" = 15,
                                 "A3df.2" = 18,
                                 "A4df.2" = 8,
                                 "A5df.2" = 4,
                                 "A1df.4" = 5,
                                 "A2df.4" = 0,
                                 "A3df.4" = 2,
                                 "A4df.4" = 19,
                                 "A5df.4" = 20
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

![](Gen5-20260126-mgig-glycogenglo_files/figure-gfm/plot%20glucose%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glu_A1_data2
glu_A2_data2
glu_A3_data2
glu_A4_data2
glu_A5_data2
glu_A1_data4
glu_A2_data4
glu_A3_data4
glu_A4_data4
glu_A5_data4



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

      glu_A1_dilution_factor2 glu_A1_mean_luminescence2 glu_A1_se2 glu_A1_conc2
    1                       2                      1117   64.53165      1.78701
       label
    1 A1df.2
      glu_A2_dilution_factor2 glu_A2_mean_luminescence2 glu_A2_se2 glu_A2_conc2
    1                       2                  587.6667    94.2644     1.372115
       label
    1 A2df.2
      glu_A3_dilution_factor2 glu_A3_mean_luminescence2 glu_A3_se2 glu_A3_conc2
    1                       4                  352.6667    15.3442      1.18792
       label
    1 A3df.4
      glu_A4_dilution_factor2 glu_A4_mean_luminescence2 glu_A4_se2 glu_A4_conc2
    1                       2                  247.3333    28.4273     1.105359
       label
    1 A4df.2
      glu_A5_dilution_factor2 glu_A5_mean_luminescence2 glu_A5_se2 glu_A5_conc2
    1                       2                  1020.333   117.1912     1.711242
       label
    1 A5df.2
      glu_A1_dilution_factor4 glu_A1_mean_luminescence4 glu_A1_se4 glu_A1_conc4
    1                       4                  544.6667   31.31737     1.338411
       label
    1 A1df.4
      glu_A2_dilution_factor4 glu_A2_mean_luminescence4 glu_A2_se4 glu_A2_conc4
    1                       4                  285.3333   12.57422     1.135143
       label
    1 A2df.4
      glu_A3_dilution_factor4 glu_A3_mean_luminescence4 glu_A3_se4 glu_A3_conc4
    1                       4                       244   5.291503     1.102746
       label
    1 A3df.4
      glu_A4_dilution_factor4 glu_A4_mean_luminescence4 glu_A4_se4 glu_A4_conc4
    1                       4                  210.3333   5.238745     1.076358
       label
    1 A4df.4
      glu_A5_dilution_factor4 glu_A5_mean_luminescence4 glu_A5_se4 glu_A5_conc4
    1                       4                       642   10.01665     1.414702
       label
    1 A5df.4
    glucose Standard Curve Summary:
    ==================================================
    Concentration: 100 µg/µL
      Mean Luminescence: 126763.33
      Standard Error: 1465.00
      CV%: 2.00%

    Concentration: 10 µg/µL
      Mean Luminescence: 8087.33
      Standard Error: 78.56
      CV%: 1.68%

    Concentration: 1 µg/µL
      Mean Luminescence: 666.67
      Standard Error: 16.19
      CV%: 4.21%

    Concentration: 0.1 µg/µL
      Mean Luminescence: 228.33
      Standard Error: 21.85
      CV%: 16.57%

    Concentration: 0 µg/µL
      Mean Luminescence: 183.67
      Standard Error: 19.54
      CV%: 18.43%

``` r
tab <- matrix(c(glu_A1_dilution2, glu_A1_mean_lum2, glu_A1_mean_conc2,  (glu_A1_dilution2*glu_A1_mean_conc2), 
                glu_A2_dilution2, glu_A2_mean_lum2, glu_A2_mean_conc2,  (glu_A2_dilution2*glu_A2_mean_conc2),
                glu_A3_dilution2, glu_A3_mean_lum2, glu_A3_mean_conc2,  (glu_A3_dilution2*glu_A3_mean_conc2),
                glu_A4_dilution2, glu_A4_mean_lum2, glu_A4_mean_conc2,  (glu_A4_dilution2*glu_A4_mean_conc2),
                glu_A5_dilution2, glu_A5_mean_lum2, glu_A5_mean_conc2,  (glu_A5_dilution2*glu_A5_mean_conc2),
                glu_A1_dilution4, glu_A1_mean_lum4, glu_A1_mean_conc4,  (glu_A1_dilution4*glu_A1_mean_conc4), 
                glu_A2_dilution4, glu_A2_mean_lum4, glu_A2_mean_conc4,  (glu_A2_dilution4*glu_A2_mean_conc4),
                glu_A3_dilution4, glu_A3_mean_lum4, glu_A3_mean_conc4,  (glu_A3_dilution4*glu_A3_mean_conc4),
                glu_A4_dilution4, glu_A4_mean_lum4, glu_A4_mean_conc4,  (glu_A4_dilution4*glu_A4_mean_conc4),
                glu_A5_dilution4, glu_A5_mean_lum4, glu_A5_mean_conc4,  (glu_A5_dilution4*glu_A5_mean_conc4)), ncol=4, byrow=TRUE)
colnames(tab) <- c('Dilution factor','Luminescence','Calculated Glycogen (ug/uL)', 'Total glucose (ug/uL)')
rownames(tab) <- c('A1.df2','A2.df2','A3.df2','A4.df2','A5.df2','A1.df4','A2.df4','A3.df4','A4.df4','Af.df4' )
tab <- as.table(tab)
tab
```

           Dilution factor Luminescence Calculated Glycogen (ug/uL)
    A1.df2        2.000000  1117.000000                    1.787010
    A2.df2        2.000000   587.666667                    1.372115
    A3.df2        4.000000   352.666667                    1.187920
    A4.df2        2.000000   247.333333                    1.105359
    A5.df2        2.000000  1020.333333                    1.711242
    A1.df4        4.000000   544.666667                    1.338411
    A2.df4        4.000000   285.333333                    1.135143
    A3.df4        4.000000   244.000000                    1.102746
    A4.df4        4.000000   210.333333                    1.076358
    Af.df4        4.000000   642.000000                    1.414702
           Total glucose (ug/uL)
    A1.df2              3.574021
    A2.df2              2.744229
    A3.df2              4.751679
    A4.df2              2.210717
    A5.df2              3.422485
    A1.df4              5.353644
    A2.df4              4.540573
    A3.df4              4.410984
    A4.df4              4.305431
    Af.df4              5.658806
