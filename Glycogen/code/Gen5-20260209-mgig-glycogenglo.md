Gen5-20260209-mgig-glycogenglo
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
plate_layout <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-Gen5-20260209-mgig-glycogenglo.csv", header = FALSE)
raw_luminescence <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-Gen5-20260209-mgig-glycogenglo.csv", header = FALSE)

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
     $ V10: chr  "C5-glu-10-df.20" "C6-glu-19-df.20" "C7-glu-35-df.20" "C8-glu-35-df.20" ...
     $ V11: chr  "C5-glu-10-df.20" "C6-glu-19-df.20" "C7-glu-35-df.20" "C8-glu-35-df.20" ...
     $ V12: chr  "C5-glu-10-df.20" "C6-glu-19-df.20" "C7-glu-35-df.20" "C8-glu-35-df.20" ...


    Raw luminescence:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : int  52786 27923 4928 17861 40231 69150 70758 72048
     $ V2 : int  52524 28427 4797 17804 38807 6419 6206 6705
     $ V3 : int  53311 27466 5420 17876 38886 1506 2148 1113
     $ V4 : int  5316 10583 116343 13773 38259 655 1090 901
     $ V5 : int  5555 10457 127799 14715 39467 620 984 1834
     $ V6 : int  5794 11190 115438 13892 41198 NA NA NA
     $ V7 : int  436 284 233 246 285 79073 76636 78501
     $ V8 : int  264 295 183 232 218 7242 23818 8648
     $ V9 : int  230 276 187 252 218 1056 3323 34298
     $ V10: int  227 194 221 173 209 564 2998 2699
     $ V11: int  267 209 249 185 184 167 273 4930
     $ V12: int  247 210 212 202 249 NA NA NA

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

    [1] 52873.67
    [1] 14.94802

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

    [1] 27938.67
    [1] 7.817358

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

    [1] 5048.333
    [1] 1.271412

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

    [1] 17847
    [1] 4.931445

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

    [1] 39308
    [1] 11.06864

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

    [1] 5555
    [1] 1.416303

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

    [1] 10743.33
    [1] 2.900011

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

    [1] 119860
    [1] 34.10409

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

    [1] 14126.67
    [1] 3.867542

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

    [1] 39641.33
    [1] 11.16397

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

![](Gen5-20260209-mgig-glycogenglo_files/figure-gfm/plot%20glycogen%20standard%20curve%20with%20samples-1.png)<!-- -->

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
    1                      20                  52873.67   231.3773     14.94802
        label
    1 B8df.20
      glyc_C1_dilution_factor glyc_C1_mean_luminescence glyc_C1_se glyc_C1_conc
    1                      20                  27938.67   277.5274     7.817358
        label
    1 C1df.20
      glyc_C2_dilution_factor glyc_C2_mean_luminescence glyc_C2_se glyc_C2_conc
    1                      20                  5048.333   189.6421     1.271412
        label
    1 C2df.20
      glyc_C3_dilution_factor glyc_C3_mean_luminescence glyc_C3_se glyc_C3_conc
    1                      20                     17847   21.93171     4.931445
        label
    1 C3df.20
      glyc_C4_dilution_factor glyc_C4_mean_luminescence glyc_C4_se glyc_C4_conc
    1                      20                     39308   462.0631     11.06864
        label
    1 C4df.20
      glyc_C5_dilution_factor glyc_C5_mean_luminescence glyc_C5_se glyc_C5_conc
    1                      20                      5555   137.9867     1.416303
        label
    1 C5df.20
      glyc_C6_dilution_factor glyc_C6_mean_luminescence glyc_C6_se glyc_C6_conc
    1                      20                  10743.33   226.2759     2.900011
        label
    1 C6df.20
      glyc_C7_dilution_factor glyc_C7_mean_luminescence glyc_C7_se glyc_C7_conc
    1                      20                    119860   3978.088     34.10409
        label
    1 C7df.20
      glyc_C8_dilution_factor glyc_C8_mean_luminescence glyc_C8_se glyc_C8_conc
    1                      20                  14126.67   296.1657     3.867542
        label
    1 C8df.20
      glyc_D1_dilution_factor glyc_D1_mean_luminescence glyc_D1_se glyc_D1_conc
    1                      20                  39641.33   852.8822     11.16397
        label
    1 D1df.20
    glycogen Standard Curve Summary:
    ==================================================
    Concentration: 20 µg/µL
      Mean Luminescence: 70652.00
      Standard Error: 838.26
      CV%: 2.06%

    Concentration: 2 µg/µL
      Mean Luminescence: 6443.33
      Standard Error: 144.56
      CV%: 3.89%

    Concentration: 0.2 µg/µL
      Mean Luminescence: 1589.00
      Standard Error: 301.65
      CV%: 32.88%

    Concentration: 0.02 µg/µL
      Mean Luminescence: 882.00
      Standard Error: 125.93
      CV%: 24.73%

    Concentration: 0 µg/µL
      Mean Luminescence: 1146.00
      Standard Error: 359.69
      CV%: 54.36%

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
    B8    2.000000e+01 5.287367e+04                1.494802e+01
    C1    2.000000e+01 2.793867e+04                7.817358e+00
    C2    2.000000e+01 5.048333e+03                1.271412e+00
    C3    2.000000e+01 1.784700e+04                4.931445e+00
    C4    2.000000e+01 3.930800e+04                1.106864e+01
    C5    2.000000e+01 5.555000e+03                1.416303e+00
    C6    2.000000e+01 1.074333e+04                2.900011e+00
    C7    2.000000e+01 1.198600e+05                3.410409e+01
    C8    2.000000e+01 1.412667e+04                3.867542e+00
    D1    2.000000e+01 3.964133e+04                1.116397e+01
       Total glycogen (ug/uL)
    B8           2.989603e+02
    C1           1.563472e+02
    C2           2.542824e+01
    C3           9.862890e+01
    C4           2.213729e+02
    C5           2.832607e+01
    C6           5.800021e+01
    C7           6.820818e+02
    C8           7.735084e+01
    D1           2.232794e+02

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

    [1] 310
    [1] -7.073337

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

    [1] 285
    [1] -7.107711

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

    [1] 201
    [1] -7.223206

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

    [1] 243.3333
    [1] -7.165

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

    [1] 240.3333
    [1] -7.169125

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

    [1] 247
    [1] -7.159959

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

    [1] NA
    [1] NA

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
    [1] -7.186999

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

    [1] 186.6667
    [1] -7.242914

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

    [1] 214
    [1] -7.205332

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

![](Gen5-20260209-mgig-glycogenglo_files/figure-gfm/plot%20glucose%20standard%20curve%20with%20samples-1.png)<!-- -->

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
    1                     20                      310  63.75997   -7.073337 B8df.20
      glu_C1_dilution_factor glu_C1_mean_luminescence glu_C1_se glu_C1_conc   label
    1                     20                      285  5.507571   -7.107711 C1df.20
      glu_C2_dilution_factor glu_C2_mean_luminescence glu_C2_se glu_C2_conc   label
    1                     20                      201  16.04161   -7.223206 C2df.20
      glu_C3_dilution_factor glu_C3_mean_luminescence glu_C3_se glu_C3_conc   label
    1                     20                 243.3333  5.925463      -7.165 C3df.20
      glu_C4_dilution_factor glu_C4_mean_luminescence glu_C4_se glu_C4_conc   label
    1                     20                 240.3333  22.33333   -7.169125 C4df.20
      glu_C5_dilution_factor glu_C5_mean_luminescence glu_C5_se glu_C5_conc   label
    1                     20                      247  11.54701   -7.159959 C5df.20
      glu_C6_dilution_factor glu_C6_mean_luminescence glu_C6_se glu_C6_conc   label
    1                     NA                       NA        NA          NA C6df.NA
      glu_C7_dilution_factor glu_C7_mean_luminescence glu_C7_se glu_C7_conc   label
    1                     20                 227.3333  11.14052   -7.186999 C7df.20
      glu_C8_dilution_factor glu_C8_mean_luminescence glu_C8_se glu_C8_conc   label
    1                     20                 186.6667  8.412953   -7.242914 C8df.20
      glu_D1_dilution_factor glu_D1_mean_luminescence glu_D1_se glu_D1_conc   label
    1                     20                      214  18.92969   -7.205332 D1df.20
    glucose Standard Curve Summary:
    ==================================================
    Concentration: 100 µg/µL
      Mean Luminescence: 78070.00
      Standard Error: 735.77
      CV%: 1.63%

    Concentration: 10 µg/µL
      Mean Luminescence: 13236.00
      Standard Error: 5306.54
      CV%: 69.44%

    Concentration: 1 µg/µL
      Mean Luminescence: 12892.33
      Standard Error: 10722.82
      CV%: 144.06%

    Concentration: 0.1 µg/µL
      Mean Luminescence: 2087.00
      Standard Error: 766.38
      CV%: 63.60%

    Concentration: 0 µg/µL
      Mean Luminescence: 1790.00
      Standard Error: 1570.30
      CV%: 151.95%

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
    B8       20.000000   310.000000                  -7.073337
    C1       20.000000   285.000000                  -7.107711
    C2       20.000000   201.000000                  -7.223206
    C3       20.000000   243.333333                  -7.165000
    C4       20.000000   240.333333                  -7.169125
    C5       20.000000   247.000000                  -7.159959
    C6                                                        
    C7       20.000000   227.333333                  -7.186999
    C8       20.000000   186.666667                  -7.242914
    D1       20.000000   214.000000                  -7.205332
       Total glucose (ug/uL)
    B8           -141.466743
    C1           -142.154216
    C2           -144.464127
    C3           -143.300005
    C4           -143.382502
    C5           -143.199176
    C6                      
    C7           -143.739988
    C8           -144.858279
    D1           -144.106641
