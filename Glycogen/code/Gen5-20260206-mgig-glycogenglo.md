Gen5-20260206-mgig-glycogenglo
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
plate_layout <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-Gen5-20260206-mgig-glycogenglo.csv", header = FALSE)
raw_luminescence <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-Gen5-20260206-mgig-glycogenglo.csv", header = FALSE)

cat("Plate layout:\n")
str(plate_layout)

cat("\n\n")

cat("Raw luminescence:\n")
str(raw_luminescence)
```

    Plate layout:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : chr  "B8-glyc-31-df.20" "C1-glyc-23-df.20" "C2-glyc-16-df.20" "C3-glyc-15-df.20" ...
     $ V2 : chr  "B8-glyc-31-df.20" "C1-glyc-23-df.20" "C2-glyc-16-df.20" "C3-glyc-15-df.20" ...
     $ V3 : chr  "B8-glyc-31-df.20" "C1-glyc-23-df.20" "C2-glyc-16-df.20" "C3-glyc-15-df.20" ...
     $ V4 : chr  "C5-glyc-10-df.20" "C6-glyc-19-df.20" "C7-glyc-35-df.20" "C8-glyc-35-df.20" ...
     $ V5 : chr  "C5-glyc-10-df.20" "C6-glyc-19-df.20" "C7-glyc-35-df.20" "C8-glyc-35-df.20" ...
     $ V6 : chr  "C5-glyc-10-df.20" "C6-glyc-19-df.20" "C7-glyc-35-df.20" "C8-glyc-35-df.20" ...
     $ V7 : chr  "B8-glu-31-df.20" "C1-glu-23-df.20" "C2-glu-16-df.20" "C3-glu-15-df.20" ...
     $ V8 : chr  "B8-glu-31-df.20" "C1-glu-23-df.20" "C2-glu-16-df.20" "C3-glu-15-df.20" ...
     $ V9 : chr  "B8-glu-31-df.20" "C1-glu-23-df.20" "C2-glu-16-df.20" "C3-glu-15-df.20" ...
     $ V10: chr  "C5-glu-10-df.20" "C6-glu-19-df-20" "C7-glu-35-df.20" "C8-glu-35-df.20" ...
     $ V11: chr  "C5-glu-10-df.20" "C6-glu-19-df-20" "C7-glu-35-df.20" "C8-glu-35-df.20" ...
     $ V12: chr  "C5-glu-10-df.20" "C6-glu-19-df-20" "C7-glu-35-df.20" "C8-glu-35-df.20" ...


    Raw luminescence:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : int  51081 32605 6116 17137 39878 74820 73672 73224
     $ V2 : int  53802 30530 6129 17256 42473 5880 5556 6254
     $ V3 : int  55309 31986 6160 17733 42511 1042 1116 1108
     $ V4 : int  9185 11321 87570 16563 49523 673 701 788
     $ V5 : int  8190 11794 89926 16450 50342 624 673 792
     $ V6 : int  7719 12767 87249 17686 52314 NA NA NA
     $ V7 : int  330 277 276 283 293 78148 84416 82266
     $ V8 : int  415 369 221 268 358 6763 7221 9821
     $ V9 : int  414 284 198 263 228 769 879 1642
     $ V10: int  314 245 238 196 241 384 296 283
     $ V11: int  238 255 220 213 219 207 227 237
     $ V12: int  229 237 224 209 230 NA NA NA

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
#Extract glycogen sample data - wells B8-E6
glyc_sample_cols1 <- c(1,2,3)
glyc_B8_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 1]))
glyc_B8_luminescence <- as.numeric(raw_luminescence[1, glyc_sample_cols1])

glyc_C1_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 1]))
glyc_C1_luminescence <- as.numeric(raw_luminescence[2, glyc_sample_cols1])

glyc_C2_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 1]))
glyc_C2_luminescence <- as.numeric(raw_luminescence[3, glyc_sample_cols1])

glyc_C3_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 1]))
glyc_C3_luminescence <- as.numeric(raw_luminescence[4, glyc_sample_cols1])

glyc_C4_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 1]))
glyc_C4_luminescence <- as.numeric(raw_luminescence[5, glyc_sample_cols1])


glyc_sample_cols2 <- c(4,5,6)
glyc_C5_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 4]))
glyc_C5_luminescence <- as.numeric(raw_luminescence[1, glyc_sample_cols2])

glyc_C6_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 4]))
glyc_C6_luminescence <- as.numeric(raw_luminescence[2, glyc_sample_cols2])

glyc_C7_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 4]))
glyc_C7_luminescence <- as.numeric(raw_luminescence[3, glyc_sample_cols2])

glyc_C8_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 4]))
glyc_C8_luminescence <- as.numeric(raw_luminescence[4, glyc_sample_cols2])

glyc_D1_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 4]))
glyc_D1_luminescence <- as.numeric(raw_luminescence[5, glyc_sample_cols2])
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
glyc_B8_mean_lum <- numeric(length(glyc_B8_dilution))
glyc_B8_se_lum <- numeric(length(glyc_B8_dilution))
glyc_B8_mean_conc <- numeric(length(glyc_B8_dilution))

for (i in 1:length(glyc_B8_dilution)) {
  df_val <- glyc_B8_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_B8_lum_values <- c(glyc_B8_luminescence[glyc_B8_dilution == df_val])
  glyc_B8_mean_lum[i] <- mean(glyc_B8_lum_values)
  glyc_B8_se_lum[i] <- sd(glyc_B8_lum_values) / sqrt(length(glyc_B8_lum_values))
  # Calculate concentration from mean luminescence
  glyc_B8_mean_conc[i] <- (glyc_B8_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_B8_data <- data.frame(
  glyc_B8_dilution_factor = glyc_B8_dilution,
  glyc_B8_mean_luminescence = glyc_B8_mean_lum,
  glyc_B8_se = glyc_B8_se_lum,
  glyc_B8_conc =  glyc_B8_mean_conc,
  label = paste0("B8df.", glyc_B8_dilution)
)
glyc_B8_mean_lum
glyc_B8_mean_conc
```

    [1] 53397.33
    [1] 14.47312

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_C1_mean_lum <- numeric(length(glyc_C1_dilution))
glyc_C1_se_lum <- numeric(length(glyc_C1_dilution))
glyc_C1_mean_conc <- numeric(length(glyc_C1_dilution))

for (i in 1:length(glyc_C1_dilution)) {
  df_val <- glyc_C1_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_C1_lum_values <- c(glyc_C1_luminescence[glyc_C1_dilution == df_val])
  glyc_C1_mean_lum[i] <- mean(glyc_C1_lum_values)
  glyc_C1_se_lum[i] <- sd(glyc_C1_lum_values) / sqrt(length(glyc_C1_lum_values))
  # Calculate concentration from mean luminescence
  glyc_C1_mean_conc[i] <- (glyc_C1_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_C1_data <- data.frame(
  glyc_C1_dilution_factor = glyc_C1_dilution,
  glyc_C1_mean_luminescence = glyc_C1_mean_lum,
  glyc_C1_se = glyc_C1_se_lum,
  glyc_C1_conc =  glyc_C1_mean_conc,
  label = paste0("C1df.", glyc_C1_dilution)
)
glyc_C1_mean_lum
glyc_C1_mean_conc
```

    [1] 31707
    [1] 8.583576

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_C2_mean_lum <- numeric(length(glyc_C2_dilution))
glyc_C2_se_lum <- numeric(length(glyc_C2_dilution))
glyc_C2_mean_conc <- numeric(length(glyc_C2_dilution))

for (i in 1:length(glyc_C2_dilution)) {
  df_val <- glyc_C2_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_C2_lum_values <- c(glyc_C2_luminescence[glyc_C2_dilution == df_val])
  glyc_C2_mean_lum[i] <- mean(glyc_C2_lum_values)
  glyc_C2_se_lum[i] <- sd(glyc_C2_lum_values) / sqrt(length(glyc_C2_lum_values))
  # Calculate concentration from mean luminescence
  glyc_C2_mean_conc[i] <- (glyc_C2_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_C2_data <- data.frame(
  glyc_C2_dilution_factor = glyc_C2_dilution,
  glyc_C2_mean_luminescence = glyc_C2_mean_lum,
  glyc_C2_se = glyc_C2_se_lum,
  glyc_C2_conc =  glyc_C2_mean_conc,
  label = paste0("C2df.", glyc_C2_dilution)
)
glyc_C2_mean_lum
glyc_C2_mean_conc
```

    [1] 6135
    [1] 1.640054

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_C3_mean_lum <- numeric(length(glyc_C3_dilution))
glyc_C3_se_lum <- numeric(length(glyc_C3_dilution))
glyc_C3_mean_conc <- numeric(length(glyc_C3_dilution))

for (i in 1:length(glyc_C3_dilution)) {
  df_val <- glyc_C3_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_C3_lum_values <- c(glyc_C3_luminescence[glyc_C3_dilution == df_val])
  glyc_C3_mean_lum[i] <- mean(glyc_C3_lum_values)
  glyc_C3_se_lum[i] <- sd(glyc_C3_lum_values) / sqrt(length(glyc_C3_lum_values))
  # Calculate concentration from mean luminescence
  glyc_C3_mean_conc[i] <- (glyc_C3_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_C3_data <- data.frame(
  glyc_C3_dilution_factor = glyc_C3_dilution,
  glyc_C3_mean_luminescence = glyc_C3_mean_lum,
  glyc_C3_se = glyc_C3_se_lum,
  glyc_C3_conc =  glyc_C3_mean_conc,
  label = paste0("C3df.", glyc_C3_dilution)
)
glyc_C3_mean_lum
glyc_C3_mean_conc
```

    [1] 17375.33
    [1] 4.692123

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_C4_mean_lum <- numeric(length(glyc_C4_dilution))
glyc_C4_se_lum <- numeric(length(glyc_C4_dilution))
glyc_C4_mean_conc <- numeric(length(glyc_C4_dilution))

for (i in 1:length(glyc_C4_dilution)) {
  df_val <- glyc_C4_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_C4_lum_values <- c(glyc_C4_luminescence[glyc_C4_dilution == df_val])
  glyc_C4_mean_lum[i] <- mean(glyc_C4_lum_values)
  glyc_C4_se_lum[i] <- sd(glyc_C4_lum_values) / sqrt(length(glyc_C4_lum_values))
  # Calculate concentration from mean luminescence
  glyc_C4_mean_conc[i] <- (glyc_C4_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_C4_data <- data.frame(
  glyc_C4_dilution_factor = glyc_C4_dilution,
  glyc_C4_mean_luminescence = glyc_C4_mean_lum,
  glyc_C4_se = glyc_C4_se_lum,
  glyc_C4_conc =  glyc_C4_mean_conc,
  label = paste0("C4df.", glyc_C4_dilution)
)
glyc_C4_mean_lum
glyc_C4_mean_conc
```

    [1] 41620.67
    [1] 11.27542

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_C5_mean_lum <- numeric(length(glyc_C5_dilution))
glyc_C5_se_lum <- numeric(length(glyc_C5_dilution))
glyc_C5_mean_conc <- numeric(length(glyc_C5_dilution))

for (i in 1:length(glyc_C5_dilution)) {
  df_val <- glyc_C5_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_C5_lum_values <- c(glyc_C5_luminescence[glyc_C5_dilution == df_val])
  glyc_C5_mean_lum[i] <- mean(glyc_C5_lum_values)
  glyc_C5_se_lum[i] <- sd(glyc_C5_lum_values) / sqrt(length(glyc_C5_lum_values))
  # Calculate concentration from mean luminescence
  glyc_C5_mean_conc[i] <- (glyc_C5_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_C5_data <- data.frame(
  glyc_C5_dilution_factor = glyc_C5_dilution,
  glyc_C5_mean_luminescence = glyc_C5_mean_lum,
  glyc_C5_se = glyc_C5_se_lum,
  glyc_C5_conc =  glyc_C5_mean_conc,
  label = paste0("C5df.", glyc_C5_dilution)
)
glyc_C5_mean_lum
glyc_C5_mean_conc
```

    [1] 8364.667
    [1] 2.245471

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_C6_mean_lum <- numeric(length(glyc_C6_dilution))
glyc_C6_se_lum <- numeric(length(glyc_C6_dilution))
glyc_C6_mean_conc <- numeric(length(glyc_C6_dilution))

for (i in 1:length(glyc_C6_dilution)) {
  df_val <- glyc_C6_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_C6_lum_values <- c(glyc_C6_luminescence[glyc_C6_dilution == df_val])
  glyc_C6_mean_lum[i] <- mean(glyc_C6_lum_values)
  glyc_C6_se_lum[i] <- sd(glyc_C6_lum_values) / sqrt(length(glyc_C6_lum_values))
  # Calculate concentration from mean luminescence
  glyc_C6_mean_conc[i] <- (glyc_C6_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_C6_data <- data.frame(
  glyc_C6_dilution_factor = glyc_C6_dilution,
  glyc_C6_mean_luminescence = glyc_C6_mean_lum,
  glyc_C6_se = glyc_C6_se_lum,
  glyc_C6_conc =  glyc_C6_mean_conc,
  label = paste0("C6df.", glyc_C6_dilution)
)
glyc_C6_mean_lum
glyc_C6_mean_conc
```

    [1] 11960.67
    [1] 3.221887

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_C7_mean_lum <- numeric(length(glyc_C7_dilution))
glyc_C7_se_lum <- numeric(length(glyc_C7_dilution))
glyc_C7_mean_conc <- numeric(length(glyc_C7_dilution))

for (i in 1:length(glyc_C7_dilution)) {
  df_val <- glyc_C7_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_C7_lum_values <- c(glyc_C7_luminescence[glyc_C7_dilution == df_val])
  glyc_C7_mean_lum[i] <- mean(glyc_C7_lum_values)
  glyc_C7_se_lum[i] <- sd(glyc_C7_lum_values) / sqrt(length(glyc_C7_lum_values))
  # Calculate concentration from mean luminescence
  glyc_C7_mean_conc[i] <- (glyc_C7_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_C7_data <- data.frame(
  glyc_C7_dilution_factor = glyc_C7_dilution,
  glyc_C7_mean_luminescence = glyc_C7_mean_lum,
  glyc_C7_se = glyc_C7_se_lum,
  glyc_C7_conc =  glyc_C7_mean_conc,
  label = paste0("C7df.", glyc_C7_dilution)
)
glyc_C7_mean_lum
glyc_C7_mean_conc
```

    [1] 88248.33
    [1] 23.93615

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_C8_mean_lum <- numeric(length(glyc_C8_dilution))
glyc_C8_se_lum <- numeric(length(glyc_C8_dilution))
glyc_C8_mean_conc <- numeric(length(glyc_C8_dilution))

for (i in 1:length(glyc_C8_dilution)) {
  df_val <- glyc_C8_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_C8_lum_values <- c(glyc_C8_luminescence[glyc_C8_dilution == df_val])
  glyc_C8_mean_lum[i] <- mean(glyc_C8_lum_values)
  glyc_C8_se_lum[i] <- sd(glyc_C8_lum_values) / sqrt(length(glyc_C8_lum_values))
  # Calculate concentration from mean luminescence
  glyc_C8_mean_conc[i] <- (glyc_C8_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_C8_data <- data.frame(
  glyc_C8_dilution_factor = glyc_C8_dilution,
  glyc_C8_mean_luminescence = glyc_C8_mean_lum,
  glyc_C8_se = glyc_C8_se_lum,
  glyc_C8_conc =  glyc_C8_mean_conc,
  label = paste0("C8df.", glyc_C8_dilution)
)
glyc_C8_mean_lum
glyc_C8_mean_conc
```

    [1] 16899.67
    [1] 4.562966

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_D1_mean_lum <- numeric(length(glyc_D1_dilution))
glyc_D1_se_lum <- numeric(length(glyc_D1_dilution))
glyc_D1_mean_conc <- numeric(length(glyc_D1_dilution))

for (i in 1:length(glyc_D1_dilution)) {
  df_val <- glyc_D1_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_D1_lum_values <- c(glyc_D1_luminescence[glyc_D1_dilution == df_val])
  glyc_D1_mean_lum[i] <- mean(glyc_D1_lum_values)
  glyc_D1_se_lum[i] <- sd(glyc_D1_lum_values) / sqrt(length(glyc_D1_lum_values))
  # Calculate concentration from mean luminescence
  glyc_D1_mean_conc[i] <- (glyc_D1_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_D1_data <- data.frame(
  glyc_D1_dilution_factor = glyc_D1_dilution,
  glyc_D1_mean_luminescence = glyc_D1_mean_lum,
  glyc_D1_se = glyc_D1_se_lum,
  glyc_D1_conc =  glyc_D1_mean_conc,
  label = paste0("D1df.", glyc_D1_dilution)
)
glyc_D1_mean_lum
glyc_D1_mean_conc
```

    [1] 50726.33
    [1] 13.74786

``` r
# Create the plot
glyc_plot <- ggplot(glycogen_summary_data, aes(x = glyc_concentration, y = glyc_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glyc_mean_luminescence - glyc_se, ymax = glyc_mean_luminescence + glyc_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +
  
     geom_errorbar(data = glyc_B8_data, aes(x = glyc_B8_conc, y = glyc_B8_mean_luminescence,
                ymin = glyc_B8_mean_luminescence - glyc_B8_se, ymax = glyc_B8_mean_luminescence + glyc_B8_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_B8_data, aes(x = glyc_B8_conc, y = glyc_B8_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_C1_data, aes(x = glyc_C1_conc, y = glyc_C1_mean_luminescence,
                ymin = glyc_C1_mean_luminescence - glyc_C1_se, ymax = glyc_C1_mean_luminescence + glyc_C1_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_C1_data, aes(x = glyc_C1_conc, y = glyc_C1_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_C2_data, aes(x = glyc_C2_conc, y = glyc_C2_mean_luminescence,
                ymin = glyc_C2_mean_luminescence - glyc_C2_se, ymax = glyc_C2_mean_luminescence + glyc_C2_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_C2_data, aes(x = glyc_C2_conc, y = glyc_C2_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_C3_data, aes(x = glyc_C3_conc, y = glyc_C3_mean_luminescence,
                ymin = glyc_C3_mean_luminescence - glyc_C3_se, ymax = glyc_C3_mean_luminescence + glyc_C3_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_C3_data, aes(x = glyc_C3_conc, y = glyc_C3_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_C4_data, aes(x = glyc_C4_conc, y = glyc_C4_mean_luminescence,
                ymin = glyc_C4_mean_luminescence - glyc_C4_se, ymax = glyc_C4_mean_luminescence + glyc_C4_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_C4_data, aes(x = glyc_C4_conc, y = glyc_C4_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_C5_data, aes(x = glyc_C5_conc, y = glyc_C5_mean_luminescence,
                ymin = glyc_C5_mean_luminescence - glyc_C5_se, ymax = glyc_C5_mean_luminescence + glyc_C5_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_C5_data, aes(x = glyc_C5_conc, y = glyc_C5_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_C6_data, aes(x = glyc_C6_conc, y = glyc_C6_mean_luminescence,
                ymin = glyc_C6_mean_luminescence - glyc_C6_se, ymax = glyc_C6_mean_luminescence + glyc_C6_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_C6_data, aes(x = glyc_C6_conc, y = glyc_C6_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_C7_data, aes(x = glyc_C7_conc, y = glyc_C7_mean_luminescence,
                ymin = glyc_C7_mean_luminescence - glyc_C7_se, ymax = glyc_C7_mean_luminescence + glyc_C7_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_C7_data, aes(x = glyc_C7_conc, y = glyc_C7_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_C8_data, aes(x = glyc_C8_conc, y = glyc_C8_mean_luminescence,
                ymin = glyc_C8_mean_luminescence - glyc_C8_se, ymax = glyc_C8_mean_luminescence + glyc_C8_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_C8_data, aes(x = glyc_C8_conc, y = glyc_C8_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_D1_data, aes(x = glyc_D1_conc, y = glyc_D1_mean_luminescence,
                ymin = glyc_D1_mean_luminescence - glyc_D1_se, ymax = glyc_D1_mean_luminescence + glyc_D1_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_D1_data, aes(x = glyc_D1_conc, y = glyc_D1_mean_luminescence,
             color = label, shape = label), size = 4) +

  
  
  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                                "B8df.20" = "darkred",
                                "C1df.20" = "darkorange",
                                "C2df.20" = "darkgreen",
                                "C3df.20" = "purple",
                                "C4df.20" = "brown",
                                "C5df.20" = "green3",
                                "C6df.20" = "firebrick1",
                                "C7df.20" = "cyan",
                                "C8df.20" = "yellow3",
                                "D1df.20" = "thistle3"
                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                 "B8df.20" = 17,
                                 "C1df.20" = 15,
                                 "C2df.20" = 18,
                                 "C3df.20" = 8,
                                 "C4df.20" = 4,
                                 "C5df.20" = 5,
                                 "C6df.20" = 0,
                                 "C7df.20" = 2,
                                 "C8df.20" = 19,
                                 "D1df.20" = 20
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

![](Gen5-20260206-mgig-glycogenglo_files/figure-gfm/plot%20glycogen%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glyc_B8_data
glyc_C1_data
glyc_C2_data
glyc_C3_data
glyc_C4_data
glyc_C5_data
glyc_C6_data
glyc_C7_data
glyc_C8_data
glyc_D1_data


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

      glyc_B8_dilution_factor glyc_B8_mean_luminescence glyc_B8_se glyc_B8_conc
    1                      20                  53397.33   1237.176     14.47312
        label
    1 B8df.20
      glyc_C1_dilution_factor glyc_C1_mean_luminescence glyc_C1_se glyc_C1_conc
    1                      20                     31707   615.0304     8.583576
        label
    1 C1df.20
      glyc_C2_dilution_factor glyc_C2_mean_luminescence glyc_C2_se glyc_C2_conc
    1                      20                      6135   13.05118     1.640054
        label
    1 C2df.20
      glyc_C3_dilution_factor glyc_C3_mean_luminescence glyc_C3_se glyc_C3_conc
    1                      20                  17375.33   182.1028     4.692123
        label
    1 C3df.20
      glyc_C4_dilution_factor glyc_C4_mean_luminescence glyc_C4_se glyc_C4_conc
    1                      20                  41620.67   871.4024     11.27542
        label
    1 C4df.20
      glyc_C5_dilution_factor glyc_C5_mean_luminescence glyc_C5_se glyc_C5_conc
    1                      20                  8364.667   432.1151     2.245471
        label
    1 C5df.20
      glyc_C6_dilution_factor glyc_C6_mean_luminescence glyc_C6_se glyc_C6_conc
    1                      20                  11960.67   425.6612     3.221887
        label
    1 C6df.20
      glyc_C7_dilution_factor glyc_C7_mean_luminescence glyc_C7_se glyc_C7_conc
    1                      20                  88248.33   843.9361     23.93615
        label
    1 C7df.20
      glyc_C8_dilution_factor glyc_C8_mean_luminescence glyc_C8_se glyc_C8_conc
    1                      20                  16899.67   394.5176     4.562966
        label
    1 C8df.20
      glyc_D1_dilution_factor glyc_D1_mean_luminescence glyc_D1_se glyc_D1_conc
    1                      20                  50726.33   828.2923     13.74786
        label
    1 D1df.20
    glycogen Standard Curve Summary:
    ==================================================
    Concentration: 20 µg/µL
      Mean Luminescence: 73905.33
      Standard Error: 475.27
      CV%: 1.11%

    Concentration: 2 µg/µL
      Mean Luminescence: 5896.67
      Standard Error: 201.67
      CV%: 5.92%

    Concentration: 0.2 µg/µL
      Mean Luminescence: 1088.67
      Standard Error: 23.45
      CV%: 3.73%

    Concentration: 0.02 µg/µL
      Mean Luminescence: 720.67
      Standard Error: 34.62
      CV%: 8.32%

    Concentration: 0 µg/µL
      Mean Luminescence: 696.33
      Standard Error: 49.88
      CV%: 12.41%

``` r
tab <- matrix(c(glyc_B8_dilution, glyc_B8_mean_lum, glyc_B8_mean_conc,  (glyc_B8_dilution*glyc_B8_mean_conc), 
                glyc_C1_dilution, glyc_C1_mean_lum, glyc_C1_mean_conc,  (glyc_C1_dilution*glyc_C1_mean_conc), 
                glyc_C2_dilution, glyc_C2_mean_lum, glyc_C2_mean_conc,  (glyc_C2_dilution*glyc_C2_mean_conc), 
                glyc_C3_dilution, glyc_C3_mean_lum, glyc_C3_mean_conc,  (glyc_C3_dilution*glyc_C3_mean_conc), 
                glyc_C4_dilution, glyc_C4_mean_lum, glyc_C4_mean_conc,  (glyc_C4_dilution*glyc_C4_mean_conc), 
                glyc_C5_dilution, glyc_C5_mean_lum, glyc_C5_mean_conc,  (glyc_C5_dilution*glyc_C5_mean_conc), 
                glyc_C6_dilution, glyc_C6_mean_lum, glyc_C6_mean_conc,  (glyc_C6_dilution*glyc_C6_mean_conc), 
                glyc_C7_dilution, glyc_C7_mean_lum, glyc_C7_mean_conc,  (glyc_C7_dilution*glyc_C7_mean_conc), 
                glyc_C8_dilution, glyc_C8_mean_lum, glyc_C8_mean_conc,  (glyc_C8_dilution*glyc_C8_mean_conc), 
                glyc_D1_dilution, glyc_D1_mean_lum, glyc_D1_mean_conc,  (glyc_D1_dilution*glyc_D1_mean_conc)), ncol=4, byrow=TRUE)
colnames(tab) <- c('Dilution factor','Luminescence','Calculated Glycogen (ug/uL)', 'Total glycogen (ug/uL)')
rownames(tab) <- c('B8','C1','C2','C3','C4','C5','C6','C7','C8','D1' )
tab <- as.table(tab)
tab
```

       Dilution factor Luminescence Calculated Glycogen (ug/uL)
    B8       20.000000 53397.333333                   14.473115
    C1       20.000000 31707.000000                    8.583576
    C2       20.000000  6135.000000                    1.640054
    C3       20.000000 17375.333333                    4.692123
    C4       20.000000 41620.666667                   11.275417
    C5       20.000000  8364.666667                    2.245471
    C6       20.000000 11960.666667                    3.221887
    C7       20.000000 88248.333333                   23.936148
    C8       20.000000 16899.666667                    4.562966
    D1       20.000000 50726.333333                   13.747863
       Total glycogen (ug/uL)
    B8             289.462305
    C1             171.671515
    C2              32.801078
    C3              93.842451
    C4             225.508338
    C5              44.909430
    C6              64.437746
    C7             478.722967
    C8              91.259312
    D1             274.957263

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
#Extract glucose sample data - wells B8-E6
glu_sample_cols1 <- c(7,8,9)
glu_B8_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 7]))
glu_B8_luminescence <- as.numeric(raw_luminescence[1, glu_sample_cols1])

glu_C1_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 7]))
glu_C1_luminescence <- as.numeric(raw_luminescence[2, glu_sample_cols1])

glu_C2_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 7]))
glu_C2_luminescence <- as.numeric(raw_luminescence[3, glu_sample_cols1])

glu_C3_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 7]))
glu_C3_luminescence <- as.numeric(raw_luminescence[4, glu_sample_cols1])

glu_C4_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 7]))
glu_C4_luminescence <- as.numeric(raw_luminescence[5, glu_sample_cols1])


glu_sample_cols2 <- c(10,11,12)
glu_C5_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 10]))
glu_C5_luminescence <- as.numeric(raw_luminescence[1, glu_sample_cols2])

glu_C6_dilution <- as.numeric(gsub(".*-df\\-", "", plate_layout[2, 10]))
glu_C6_luminescence <- as.numeric(raw_luminescence[2, glu_sample_cols2])

glu_C7_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 10]))
glu_C7_luminescence <- as.numeric(raw_luminescence[3, glu_sample_cols2])

glu_C8_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 10]))
glu_C8_luminescence <- as.numeric(raw_luminescence[4, glu_sample_cols2])

glu_D1_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 10]))
glu_D1_luminescence <- as.numeric(raw_luminescence[5, glu_sample_cols2])
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
glu_B8_mean_lum <- numeric(length(glu_B8_dilution))
glu_B8_se_lum <- numeric(length(glu_B8_dilution))
glu_B8_mean_conc <- numeric(length(glu_B8_dilution))

for (i in 1:length(glu_B8_dilution)) {
  df_val <- glu_B8_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_B8_lum_values <- c(glu_B8_luminescence[glu_B8_dilution == df_val])
  glu_B8_mean_lum[i] <- mean(glu_B8_lum_values)
  glu_B8_se_lum[i] <- sd(glu_B8_lum_values) / sqrt(length(glu_B8_lum_values))
  # Calculate concentration from mean luminescence
  glu_B8_mean_conc[i] <- (glu_B8_mean_lum[i] - glu_intercept) / glu_slope
}

glu_B8_data <- data.frame(
  glu_B8_dilution_factor = glu_B8_dilution,
  glu_B8_mean_luminescence = glu_B8_mean_lum,
  glu_B8_se = glu_B8_se_lum,
  glu_B8_conc =  glu_B8_mean_conc,
  label = paste0("B8df.", glu_B8_dilution)
)
glu_B8_mean_lum
glu_B8_mean_conc
```

    [1] 386.3333
    [1] 0.2991775

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_C1_mean_lum <- numeric(length(glu_C1_dilution))
glu_C1_se_lum <- numeric(length(glu_C1_dilution))
glu_C1_mean_conc <- numeric(length(glu_C1_dilution))

for (i in 1:length(glu_C1_dilution)) {
  df_val <- glu_C1_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_C1_lum_values <- c(glu_C1_luminescence[glu_C1_dilution == df_val])
  glu_C1_mean_lum[i] <- mean(glu_C1_lum_values)
  glu_C1_se_lum[i] <- sd(glu_C1_lum_values) / sqrt(length(glu_C1_lum_values))
  # Calculate concentration from mean luminescence
  glu_C1_mean_conc[i] <- (glu_C1_mean_lum[i] - glu_intercept) / glu_slope
}

glu_C1_data <- data.frame(
  glu_C1_dilution_factor = glu_C1_dilution,
  glu_C1_mean_luminescence = glu_C1_mean_lum,
  glu_C1_se = glu_C1_se_lum,
  glu_C1_conc =  glu_C1_mean_conc,
  label = paste0("C1df.", glu_C1_dilution)
)
glu_C1_mean_lum
glu_C1_mean_conc
```

    [1] 310
    [1] 0.2054407

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_C2_mean_lum <- numeric(length(glu_C2_dilution))
glu_C2_se_lum <- numeric(length(glu_C2_dilution))
glu_C2_mean_conc <- numeric(length(glu_C2_dilution))

for (i in 1:length(glu_C2_dilution)) {
  df_val <- glu_C2_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_C2_lum_values <- c(glu_C2_luminescence[glu_C2_dilution == df_val])
  glu_C2_mean_lum[i] <- mean(glu_C2_lum_values)
  glu_C2_se_lum[i] <- sd(glu_C2_lum_values) / sqrt(length(glu_C2_lum_values))
  # Calculate concentration from mean luminescence
  glu_C2_mean_conc[i] <- (glu_C2_mean_lum[i] - glu_intercept) / glu_slope
}

glu_C2_data <- data.frame(
  glu_C2_dilution_factor = glu_C2_dilution,
  glu_C2_mean_luminescence = glu_C2_mean_lum,
  glu_C2_se = glu_C2_se_lum,
  glu_C2_conc =  glu_C2_mean_conc,
  label = paste0("C2df.", glu_C2_dilution)
)
glu_C2_mean_lum
glu_C2_mean_conc
```

    [1] 231.6667
    [1] 0.1092479

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_C3_mean_lum <- numeric(length(glu_C3_dilution))
glu_C3_se_lum <- numeric(length(glu_C3_dilution))
glu_C3_mean_conc <- numeric(length(glu_C3_dilution))

for (i in 1:length(glu_C3_dilution)) {
  df_val <- glu_C3_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_C3_lum_values <- c(glu_C3_luminescence[glu_C3_dilution == df_val])
  glu_C3_mean_lum[i] <- mean(glu_C3_lum_values)
  glu_C3_se_lum[i] <- sd(glu_C3_lum_values) / sqrt(length(glu_C3_lum_values))
  # Calculate concentration from mean luminescence
  glu_C3_mean_conc[i] <- (glu_C3_mean_lum[i] - glu_intercept) / glu_slope
}

glu_C3_data <- data.frame(
  glu_C3_dilution_factor = glu_C3_dilution,
  glu_C3_mean_luminescence = glu_C3_mean_lum,
  glu_C3_se = glu_C3_se_lum,
  glu_C3_conc =  glu_C3_mean_conc,
  label = paste0("C3df.", glu_C3_dilution)
)
glu_C3_mean_lum
glu_C3_mean_conc
```

    [1] 271.3333
    [1] 0.1579583

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_C4_mean_lum <- numeric(length(glu_C4_dilution))
glu_C4_se_lum <- numeric(length(glu_C4_dilution))
glu_C4_mean_conc <- numeric(length(glu_C4_dilution))

for (i in 1:length(glu_C4_dilution)) {
  df_val <- glu_C4_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_C4_lum_values <- c(glu_C4_luminescence[glu_C4_dilution == df_val])
  glu_C4_mean_lum[i] <- mean(glu_C4_lum_values)
  glu_C4_se_lum[i] <- sd(glu_C4_lum_values) / sqrt(length(glu_C4_lum_values))
  # Calculate concentration from mean luminescence
  glu_C4_mean_conc[i] <- (glu_C4_mean_lum[i] - glu_intercept) / glu_slope
}

glu_C4_data <- data.frame(
  glu_C4_dilution_factor = glu_C4_dilution,
  glu_C4_mean_luminescence = glu_C4_mean_lum,
  glu_C4_se = glu_C4_se_lum,
  glu_C4_conc =  glu_C4_mean_conc,
  label = paste0("C4df.", glu_C4_dilution)
)
glu_C4_mean_lum
glu_C4_mean_conc
```

    [1] 293
    [1] 0.1845648

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_C5_mean_lum <- numeric(length(glu_C5_dilution))
glu_C5_se_lum <- numeric(length(glu_C5_dilution))
glu_C5_mean_conc <- numeric(length(glu_C5_dilution))

for (i in 1:length(glu_C5_dilution)) {
  df_val <- glu_C5_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_C5_lum_values <- c(glu_C5_luminescence[glu_C5_dilution == df_val])
  glu_C5_mean_lum[i] <- mean(glu_C5_lum_values)
  glu_C5_se_lum[i] <- sd(glu_C5_lum_values) / sqrt(length(glu_C5_lum_values))
  # Calculate concentration from mean luminescence
  glu_C5_mean_conc[i] <- (glu_C5_mean_lum[i] - glu_intercept) / glu_slope
}

glu_C5_data <- data.frame(
  glu_C5_dilution_factor = glu_C5_dilution,
  glu_C5_mean_luminescence = glu_C5_mean_lum,
  glu_C5_se = glu_C5_se_lum,
  glu_C5_conc =  glu_C5_mean_conc,
  label = paste0("C5df.", glu_C5_dilution)
)
glu_C5_mean_lum
glu_C5_mean_conc
```

    [1] 260.3333
    [1] 0.1444504

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_C6_mean_lum <- numeric(length(glu_C6_dilution))
glu_C6_se_lum <- numeric(length(glu_C6_dilution))
glu_C6_mean_conc <- numeric(length(glu_C6_dilution))

for (i in 1:length(glu_C6_dilution)) {
  df_val <- glu_C6_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_C6_lum_values <- c(glu_C6_luminescence[glu_C6_dilution == df_val])
  glu_C6_mean_lum[i] <- mean(glu_C6_lum_values)
  glu_C6_se_lum[i] <- sd(glu_C6_lum_values) / sqrt(length(glu_C6_lum_values))
  # Calculate concentration from mean luminescence
  glu_C6_mean_conc[i] <- (glu_C6_mean_lum[i] - glu_intercept) / glu_slope
}

glu_C6_data <- data.frame(
  glu_C6_dilution_factor = glu_C6_dilution,
  glu_C6_mean_luminescence = glu_C6_mean_lum,
  glu_C6_se = glu_C6_se_lum,
  glu_C6_conc =  glu_C6_mean_conc,
  label = paste0("C6df.", glu_C6_dilution)
)
glu_C6_mean_lum
glu_C6_mean_conc
```

    [1] 245.6667
    [1] 0.1264398

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_C7_mean_lum <- numeric(length(glu_C7_dilution))
glu_C7_se_lum <- numeric(length(glu_C7_dilution))
glu_C7_mean_conc <- numeric(length(glu_C7_dilution))

for (i in 1:length(glu_C7_dilution)) {
  df_val <- glu_C7_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_C7_lum_values <- c(glu_C7_luminescence[glu_C7_dilution == df_val])
  glu_C7_mean_lum[i] <- mean(glu_C7_lum_values)
  glu_C7_se_lum[i] <- sd(glu_C7_lum_values) / sqrt(length(glu_C7_lum_values))
  # Calculate concentration from mean luminescence
  glu_C7_mean_conc[i] <- (glu_C7_mean_lum[i] - glu_intercept) / glu_slope
}

glu_C7_data <- data.frame(
  glu_C7_dilution_factor = glu_C7_dilution,
  glu_C7_mean_luminescence = glu_C7_mean_lum,
  glu_C7_se = glu_C7_se_lum,
  glu_C7_conc =  glu_C7_mean_conc,
  label = paste0("C7df.", glu_C7_dilution)
)
glu_C7_mean_lum
glu_C7_mean_conc
```

    [1] 227.3333
    [1] 0.1039266

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_C8_mean_lum <- numeric(length(glu_C8_dilution))
glu_C8_se_lum <- numeric(length(glu_C8_dilution))
glu_C8_mean_conc <- numeric(length(glu_C8_dilution))

for (i in 1:length(glu_C8_dilution)) {
  df_val <- glu_C8_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_C8_lum_values <- c(glu_C8_luminescence[glu_C8_dilution == df_val])
  glu_C8_mean_lum[i] <- mean(glu_C8_lum_values)
  glu_C8_se_lum[i] <- sd(glu_C8_lum_values) / sqrt(length(glu_C8_lum_values))
  # Calculate concentration from mean luminescence
  glu_C8_mean_conc[i] <- (glu_C8_mean_lum[i] - glu_intercept) / glu_slope
}

glu_C8_data <- data.frame(
  glu_C8_dilution_factor = glu_C8_dilution,
  glu_C8_mean_luminescence = glu_C8_mean_lum,
  glu_C8_se = glu_C8_se_lum,
  glu_C8_conc =  glu_C8_mean_conc,
  label = paste0("C8df.", glu_C8_dilution)
)
glu_C8_mean_lum
glu_C8_mean_conc
```

    [1] 206
    [1] 0.07772941

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_D1_mean_lum <- numeric(length(glu_D1_dilution))
glu_D1_se_lum <- numeric(length(glu_D1_dilution))
glu_D1_mean_conc <- numeric(length(glu_D1_dilution))

for (i in 1:length(glu_D1_dilution)) {
  df_val <- glu_D1_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_D1_lum_values <- c(glu_D1_luminescence[glu_D1_dilution == df_val])
  glu_D1_mean_lum[i] <- mean(glu_D1_lum_values)
  glu_D1_se_lum[i] <- sd(glu_D1_lum_values) / sqrt(length(glu_D1_lum_values))
  # Calculate concentration from mean luminescence
  glu_D1_mean_conc[i] <- (glu_D1_mean_lum[i] - glu_intercept) / glu_slope
}

glu_D1_data <- data.frame(
  glu_D1_dilution_factor = glu_D1_dilution,
  glu_D1_mean_luminescence = glu_D1_mean_lum,
  glu_D1_se = glu_D1_se_lum,
  glu_D1_conc =  glu_D1_mean_conc,
  label = paste0("D1df.", glu_D1_dilution)
)
glu_D1_mean_lum
glu_D1_mean_conc
```

    [1] 230
    [1] 0.1072012

``` r
# Create the plot
glu_plot <- ggplot(glucose_summary_data, aes(x = glu_concentration, y = glu_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glu_mean_luminescence - glu_se, ymax = glu_mean_luminescence + glu_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +
  
   geom_errorbar(data = glu_B8_data, aes(x = glu_B8_conc, y = glu_B8_mean_luminescence,
                ymin = glu_B8_mean_luminescence - glu_B8_se, ymax = glu_B8_mean_luminescence + glu_B8_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_B8_data, aes(x = glu_B8_conc, y = glu_B8_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_C1_data, aes(x = glu_C1_conc, y = glu_C1_mean_luminescence,
                ymin = glu_C1_mean_luminescence - glu_C1_se, ymax = glu_C1_mean_luminescence + glu_C1_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_C1_data, aes(x = glu_C1_conc, y = glu_C1_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_C2_data, aes(x = glu_C2_conc, y = glu_C2_mean_luminescence,
                ymin = glu_C2_mean_luminescence - glu_C2_se, ymax = glu_C2_mean_luminescence + glu_C2_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_C2_data, aes(x = glu_C2_conc, y = glu_C2_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_C3_data, aes(x = glu_C3_conc, y = glu_C3_mean_luminescence,
                ymin = glu_C3_mean_luminescence - glu_C3_se, ymax = glu_C3_mean_luminescence + glu_C3_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_C3_data, aes(x = glu_C3_conc, y = glu_C3_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_C4_data, aes(x = glu_C4_conc, y = glu_C4_mean_luminescence,
                ymin = glu_C4_mean_luminescence - glu_C4_se, ymax = glu_C4_mean_luminescence + glu_C4_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_C4_data, aes(x = glu_C4_conc, y = glu_C4_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_C5_data, aes(x = glu_C5_conc, y = glu_C5_mean_luminescence,
                ymin = glu_C5_mean_luminescence - glu_C5_se, ymax = glu_C5_mean_luminescence + glu_C5_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_C5_data, aes(x = glu_C5_conc, y = glu_C5_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_C6_data, aes(x = glu_C6_conc, y = glu_C6_mean_luminescence,
                ymin = glu_C6_mean_luminescence - glu_C6_se, ymax = glu_C6_mean_luminescence + glu_C6_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_C6_data, aes(x = glu_C6_conc, y = glu_C6_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glu_C7_data, aes(x = glu_C7_conc, y = glu_C7_mean_luminescence,
                ymin = glu_C7_mean_luminescence - glu_C7_se, ymax = glu_C7_mean_luminescence + glu_C7_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_C7_data, aes(x = glu_C7_conc, y = glu_C7_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_C8_data, aes(x = glu_C8_conc, y = glu_C8_mean_luminescence,
                ymin = glu_C8_mean_luminescence - glu_C8_se, ymax = glu_C8_mean_luminescence + glu_C8_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_C8_data, aes(x = glu_C8_conc, y = glu_C8_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glu_D1_data, aes(x = glu_D1_conc, y = glu_D1_mean_luminescence,
                ymin = glu_D1_mean_luminescence - glu_D1_se, ymax = glu_D1_mean_luminescence + glu_D1_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_D1_data, aes(x = glu_D1_conc, y = glu_D1_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                                "B8df.20" = "darkred",
                                "C1df.20" = "darkorange",
                                "C2df.20" = "darkgreen",
                                "C3df.20" = "purple",
                                "C4df.20" = "brown",
                                "C5df.20" = "green3",
                                "C6df.20" = "firebrick1",
                                "C7df.20" = "cyan",
                                "C8df.20" = "yellow3",
                                "D1df.20" = "thistle3"
                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                 "B8df.20" = 17,
                                 "C1df.20" = 15,
                                 "C2df.20" = 18,
                                 "C3f.20" = 8,
                                 "C4df.20" = 4,
                                 "C5df.20" = 5,
                                 "C6df.20" = 0,
                                 "C7df.20" = 2,
                                 "C8df.20" = 19,
                                 "D1df.20" = 20
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

![](Gen5-20260206-mgig-glycogenglo_files/figure-gfm/plot%20glucose%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glu_B8_data
glu_C1_data
glu_C2_data
glu_C3_data
glu_C4_data
glu_C5_data
glu_C6_data
glu_C7_data
glu_C8_data
glu_D1_data

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

      glu_B8_dilution_factor glu_B8_mean_luminescence glu_B8_se glu_B8_conc   label
    1                     20                 386.3333  28.16815   0.2991775 B8df.20
      glu_C1_dilution_factor glu_C1_mean_luminescence glu_C1_se glu_C1_conc   label
    1                     20                      310  29.56913   0.2054407 C1df.20
      glu_C2_dilution_factor glu_C2_mean_luminescence glu_C2_se glu_C2_conc   label
    1                     20                 231.6667  23.13967   0.1092479 C2df.20
      glu_C3_dilution_factor glu_C3_mean_luminescence glu_C3_se glu_C3_conc   label
    1                     20                 271.3333  6.009252   0.1579583 C3df.20
      glu_C4_dilution_factor glu_C4_mean_luminescence glu_C4_se glu_C4_conc   label
    1                     20                      293  37.52777   0.1845648 C4df.20
      glu_C5_dilution_factor glu_C5_mean_luminescence glu_C5_se glu_C5_conc   label
    1                     20                 260.3333  26.95882   0.1444504 C5df.20
      glu_C6_dilution_factor glu_C6_mean_luminescence glu_C6_se glu_C6_conc   label
    1                     20                 245.6667  5.206833   0.1264398 C6df.20
      glu_C7_dilution_factor glu_C7_mean_luminescence glu_C7_se glu_C7_conc   label
    1                     20                 227.3333  5.456902   0.1039266 C7df.20
      glu_C8_dilution_factor glu_C8_mean_luminescence glu_C8_se glu_C8_conc   label
    1                     20                      206  5.131601  0.07772941 C8df.20
      glu_D1_dilution_factor glu_D1_mean_luminescence glu_D1_se glu_D1_conc   label
    1                     20                      230  6.350853   0.1072012 D1df.20
    glucose Standard Curve Summary:
    ==================================================
    Concentration: 100 µg/µL
      Mean Luminescence: 81610.00
      Standard Error: 1838.90
      CV%: 3.90%

    Concentration: 10 µg/µL
      Mean Luminescence: 7935.00
      Standard Error: 952.22
      CV%: 20.79%

    Concentration: 1 µg/µL
      Mean Luminescence: 1096.67
      Standard Error: 274.51
      CV%: 43.36%

    Concentration: 0.1 µg/µL
      Mean Luminescence: 321.00
      Standard Error: 31.72
      CV%: 17.12%

    Concentration: 0 µg/µL
      Mean Luminescence: 223.67
      Standard Error: 8.82
      CV%: 6.83%

``` r
tab <- matrix(c(glu_B8_dilution, glu_B8_mean_lum, glu_B8_mean_conc,  (glu_B8_dilution*glu_B8_mean_conc), 
                glu_C1_dilution, glu_C1_mean_lum, glu_C1_mean_conc,  (glu_C1_dilution*glu_C1_mean_conc), 
                glu_C2_dilution, glu_C2_mean_lum, glu_C2_mean_conc,  (glu_C2_dilution*glu_C2_mean_conc), 
                glu_C3_dilution, glu_C3_mean_lum, glu_C3_mean_conc,  (glu_C3_dilution*glu_C3_mean_conc), 
                glu_C4_dilution, glu_C4_mean_lum, glu_C4_mean_conc,  (glu_C4_dilution*glu_C4_mean_conc), 
                glu_C5_dilution, glu_C5_mean_lum, glu_C5_mean_conc,  (glu_C5_dilution*glu_C5_mean_conc), 
                glu_C6_dilution, glu_C6_mean_lum, glu_C6_mean_conc,  (glu_C6_dilution*glu_C6_mean_conc), 
                glu_C7_dilution, glu_C7_mean_lum, glu_C7_mean_conc,  (glu_C7_dilution*glu_C7_mean_conc), 
                glu_C8_dilution, glu_C8_mean_lum, glu_C8_mean_conc,  (glu_C8_dilution*glu_C8_mean_conc), 
                glu_D1_dilution, glu_D1_mean_lum, glu_D1_mean_conc,  (glu_D1_dilution*glu_D1_mean_conc)), ncol=4, byrow=TRUE)
colnames(tab) <- c('Dilution factor','Luminescence','Calculated glucose (ug/uL)', 'Total glucose (ug/uL)')
rownames(tab) <- c('B8','C1','C2','C3','C4','C5','C6','C7','C8','D1' )
tab <- as.table(tab)
tab
```

       Dilution factor Luminescence Calculated glucose (ug/uL)
    B8     20.00000000 386.33333333                 0.29917749
    C1     20.00000000 310.00000000                 0.20544069
    C2     20.00000000 231.66666667                 0.10924790
    C3     20.00000000 271.33333333                 0.15795829
    C4     20.00000000 293.00000000                 0.18456481
    C5     20.00000000 260.33333333                 0.14445037
    C6     20.00000000 245.66666667                 0.12643980
    C7     20.00000000 227.33333333                 0.10392659
    C8     20.00000000 206.00000000                 0.07772941
    D1     20.00000000 230.00000000                 0.10720124
       Total glucose (ug/uL)
    B8            5.98354987
    C1            4.10881378
    C2            2.18495796
    C3            3.15916580
    C4            3.69129613
    C5            2.88900733
    C6            2.52879602
    C7            2.07853190
    C8            1.55458819
    D1            2.14402486
