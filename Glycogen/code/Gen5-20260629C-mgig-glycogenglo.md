Gen5-20260629C-mgig-glycogenglo
================
Hazel Abrahamson-Amerine
2026-07-07

- [1 Load plate reader, luminescence, and sample weight
  data](#1-load-plate-reader-luminescence-and-sample-weight-data)
- [2 Glycogen Standard Curve](#2-glycogen-standard-curve)
  - [2.1 Extract luminescence data](#21-extract-luminescence-data)
  - [2.2 Glycogen standard curve summary statistics and linear
    regression](#22-glycogen-standard-curve-summary-statistics-and-linear-regression)
  - [2.3 Calculate sample glycogen
    levels](#23-calculate-sample-glycogen-levels)
  - [2.4 Plot glycogen standard curve, sample
    points](#24-plot-glycogen-standard-curve-sample-points)
  - [2.5 Sample glycogen table](#25-sample-glycogen-table)

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

# 1 Load plate reader, luminescence, and sample weight data

``` r
#load data
#layout and luminescence
#plate 1 - standard curve
plate_layout1 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-Gen5-20260629A-mgig-glycogenglo.csv", header = FALSE)
raw_luminescence1 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-Gen5-20260629A-mgig-glycogenglo.csv", header = FALSE)

cat("Plate layout1:\n")
str(plate_layout1)

cat("\n\n")

cat("Raw luminescence1:\n")
str(raw_luminescence1)

#plate 2 - samples
plate_layout2 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-Gen5-20260629C-mgig-glycogenglo.csv", header = FALSE)
raw_luminescence2 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-Gen5-20260629C-mgig-glycogenglo.csv", header = FALSE)

cat("Plate layout2:\n")
str(plate_layout2)

cat("\n\n")

cat("Raw luminescence2:\n")
str(raw_luminescence2)

#sample weights
sample_weights <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/20260630_mgig_wholetissue_weights_GlycogenGlo.csv", header = FALSE)

cat("Sample Weights:\n")
str(sample_weights)
```

    Plate layout1:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : chr  "0.0A**-glyc-16.6-df.20" "0.0B**-glyc-10.4-df.20" "0.0C**-glyc-12.0-df.20" "0.0D**-glyc-17.3-df.20" ...
     $ V2 : chr  "0.0A**-glyc-16.6-df.20" "0.0B**-glyc-10.4-df.20" "0.0C**-glyc-12.0-df.20" "0.0D**-glyc-17.3-df.20" ...
     $ V3 : chr  "0.0A**-glyc-16.6-df.20" "0.0B**-glyc-10.4-df.20" "0.0C**-glyc-12.0-df.20" "0.0D**-glyc-17.3-df.20" ...
     $ V4 : chr  "0.0F**-glyc-17.3-df.20" "0.0G**-glyc-14.4-df.20" "0.0H**-glyc-15.5-df.20" "0.0I**-glyc-13.3-df.20" ...
     $ V5 : chr  "0.0F**-glyc-17.3-df.20" "0.0G**-glyc-14.4-df.20" "0.0H**-glyc-15.5-df.20" "0.0I**-glyc-13.3-df.20" ...
     $ V6 : chr  "0.0F**-glyc-17.3-df.20" "0.0G**-glyc-14.4-df.20" "0.0H**-glyc-15.5-df.20" "0.0I**-glyc-13.3-df.20" ...
     $ V7 : chr  "48.0A**-glyc-21.8-df.20" "48.0B**-glyc-26.0-df.20" "48.0C**-glyc-8.8-df.20" "48.0D**-glyc-11.9-df.20" ...
     $ V8 : chr  "48.0A**-glyc-21.8-df.20" "48.0B**-glyc-26.0-df.20" "48.0C**-glyc-8.8-df.20" "48.0D**-glyc-11.9-df.20" ...
     $ V9 : chr  "48.0A**-glyc-21.8-df.20" "48.0B**-glyc-26.0-df.20" "48.0C**-glyc-8.8-df.20" "48.0D**-glyc-11.9-df.20" ...
     $ V10: chr  "48.0I**-glyc-24.5-df.20" "48.0J**-glyc-14.9-df.20" "48.2A**-glyc-20.8-df.20" "48.2B**-glyc-18.5-df.20" ...
     $ V11: chr  "48.0I**-glyc-24.5-df.20" "48.0J**-glyc-14.9-df.20" "48.2A**-glyc-20.8-df.20" "48.2B**-glyc-18.5-df.20" ...
     $ V12: chr  "48.0I**-glyc-24.5-df.20" "48.0J**-glyc-14.9-df.20" "48.2A**-glyc-20.8-df.20" "48.2B**-glyc-18.5-df.20" ...


    Raw luminescence1:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : int  1145 995 922 2652 2056 27200 29458 30274
     $ V2 : int  1136 943 976 2455 2007 2958 2051 1993
     $ V3 : int  1230 967 989 2428 2041 464 457 477
     $ V4 : int  5136 752 2205 1857 1953 333 311 319
     $ V5 : int  4589 847 2289 1722 1789 334 363 352
     $ V6 : int  4806 766 2178 1758 1890 NA NA NA
     $ V7 : int  6471 1155 487 377 771 1249 681 730
     $ V8 : int  6953 1310 457 480 814 1399 698 728
     $ V9 : int  7381 1160 450 393 827 1498 647 734
     $ V10: int  993 1455 2670 2525 1131 1561 1361 2659
     $ V11: int  1099 1522 2576 2693 1125 1691 1251 2927
     $ V12: int  1099 1505 2780 2673 1185 1561 1389 3045
    Plate layout2:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : chr  "N0.A-glyc-10.6-df.20" "N0.B-glyc-17.3-df.20" "N0.C-glyc-5.7-df.20" "H0.A-glyc-9.8-df.20" ...
     $ V2 : chr  "N0.A-glyc-10.6-df.20" "N0.B-glyc-17.3-df.20" "N0.C-glyc-5.7-df.20" "H0.A-glyc-9.8-df.20" ...
     $ V3 : chr  "N0.A-glyc-10.6-df.20" "N0.B-glyc-17.3-df.20" "N0.C-glyc-5.7-df.20" "H0.A-glyc-9.8-df.20" ...
     $ V4 : chr  "HH.E-glyc-2.8-df.20" "HH.F-glyc-8.7-df.20" "NH.A-glyc-13.2-df.20" "NH.C-glyc-8.2-df.20" ...
     $ V5 : chr  "HH.E-glyc-2.8-df.20" "HH.F-glyc-8.7-df.20" "NH.A-glyc-13.2-df.20" "NH.C-glyc-8.2-df.20" ...
     $ V6 : chr  "HH.E-glyc-2.8-df.20" "HH.F-glyc-8.7-df.20" "NH.A-glyc-13.2-df.20" "NH.C-glyc-8.2-df.20" ...
     $ V7 : chr  "HN.C-glyc-9.3-df.20" "HN.D-glyc-12.8-df.20" "NN.A-glyc-10.3-df.20" "NN.B-glyc-11.1-df.20" ...
     $ V8 : chr  "HN.C-glyc-9.3-df.20" "HN.D-glyc-12.8-df.20" "NN.A-glyc-10.3-df.20" "NN.B-glyc-11.1-df.20" ...
     $ V9 : chr  "HN.C-glyc-9.3-df.20" "HN.D-glyc-12.8-df.20" "NN.A-glyc-10.3-df.20" "NN.B-glyc-11.1-df.20" ...
     $ V10: logi  NA NA NA NA NA NA ...
     $ V11: logi  NA NA NA NA NA NA ...
     $ V12: logi  NA NA NA NA NA NA ...


    Raw luminescence2:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : int  2670 4350 1243 983 720 1688 818 757
     $ V2 : int  2421 4406 1425 1143 780 1837 856 779
     $ V3 : int  2522 4312 1145 1099 682 1805 829 867
     $ V4 : int  604 687 797 724 1012 833 3488 1935
     $ V5 : int  583 703 838 732 1020 861 3236 2007
     $ V6 : int  539 713 775 640 930 794 3162 2059
     $ V7 : int  2198 3625 2651 2108 2832 7993 8187 NA
     $ V8 : int  2170 3528 1274 2051 3126 8009 8780 NA
     $ V9 : int  2240 3639 2642 2280 3100 8399 9868 NA
     $ V10: logi  NA NA NA NA NA NA ...
     $ V11: logi  NA NA NA NA NA NA ...
     $ V12: logi  NA NA NA NA NA NA ...
    Sample Weights:
    'data.frame':   24 obs. of  2 variables:
     $ V1: chr  "SampleID" "N0.A" "N0.B" "N0.C" ...
     $ V2: chr  "Weight (mg)" "10.6" "17.3" "5.7" ...

# 2 Glycogen Standard Curve

## 2.1 Extract luminescence data

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
#Extract glycogen sample data
#plate 1
glyc_sample_cols1 <- c(1,2,3)
glyc_N0.A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[1, 1]))
glyc_N0.A_luminescence <- as.numeric(raw_luminescence2[1, glyc_sample_cols1])

glyc_N0.B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[2, 1]))
glyc_N0.B_luminescence <- as.numeric(raw_luminescence2[2, glyc_sample_cols1])

glyc_N0.C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[3, 1]))
glyc_N0.C_luminescence <- as.numeric(raw_luminescence2[3, glyc_sample_cols1])

glyc_H0.A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[4, 1]))
glyc_H0.A_luminescence <- as.numeric(raw_luminescence2[4, glyc_sample_cols1])

glyc_H0.B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[5, 1]))
glyc_H0.B_luminescence <- as.numeric(raw_luminescence2[5, glyc_sample_cols1])

glyc_H0.C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[6, 1]))
glyc_H0.C_luminescence <- as.numeric(raw_luminescence2[6, glyc_sample_cols1])

glyc_HH.A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[7, 1]))
glyc_HH.A_luminescence <- as.numeric(raw_luminescence2[7, glyc_sample_cols1])

glyc_HH.B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[8, 1]))
glyc_HH.B_luminescence <- as.numeric(raw_luminescence2[8, glyc_sample_cols1])


glyc_sample_cols2 <- c(4,5,6)
glyc_HH.E_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[1, 4]))
glyc_HH.E_luminescence <- as.numeric(raw_luminescence2[1, glyc_sample_cols2])

glyc_HH.F_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[2, 4]))
glyc_HH.F_luminescence <- as.numeric(raw_luminescence2[2, glyc_sample_cols2])

glyc_NH.A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[3, 4]))
glyc_NH.A_luminescence <- as.numeric(raw_luminescence2[3, glyc_sample_cols2])

glyc_NH.C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[4, 4]))
glyc_NH.C_luminescence <- as.numeric(raw_luminescence2[4, glyc_sample_cols2])

glyc_NH.D_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[5, 4]))
glyc_NH.D_luminescence <- as.numeric(raw_luminescence2[5, glyc_sample_cols2])

glyc_NH.F_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[6, 4]))
glyc_NH.F_luminescence <- as.numeric(raw_luminescence2[6, glyc_sample_cols2])

glyc_HN.A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[7, 4]))
glyc_HN.A_luminescence <- as.numeric(raw_luminescence2[7, glyc_sample_cols2])

glyc_HN.B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[8, 4]))
glyc_HN.B_luminescence <- as.numeric(raw_luminescence2[8, glyc_sample_cols2])


glyc_sample_cols3 <- c(7,8,9)
glyc_HN.C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[1, 7]))
glyc_HN.C_luminescence <- as.numeric(raw_luminescence2[1, glyc_sample_cols3]) 

glyc_HN.D_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[2, 7]))
glyc_HN.D_luminescence <- as.numeric(raw_luminescence2[2, glyc_sample_cols3]) 

glyc_NN.A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[3, 7]))
glyc_NN.A_luminescence <- as.numeric(raw_luminescence2[3, glyc_sample_cols3]) 

glyc_NN.B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[4, 7]))
glyc_NN.B_luminescence <- as.numeric(raw_luminescence2[4, glyc_sample_cols3]) 

glyc_NN.D_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[5, 7]))
glyc_NN.D_luminescence <- as.numeric(raw_luminescence2[5, glyc_sample_cols3])

glyc_NN.E_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[6, 7]))
glyc_NN.E_luminescence <- as.numeric(raw_luminescence2[6, glyc_sample_cols3]) 

glyc_NN.F_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[7, 7]))
glyc_NN.F_luminescence <- as.numeric(raw_luminescence2[7, glyc_sample_cols3])
```

## 2.2 Glycogen standard curve summary statistics and linear regression

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

glyc_r_squared
glyc_slope
glyc_intercept
```

    [1] 0.9991695
    glyc_concentration 
              1441.608 
    (Intercept) 
       83.09312 

## 2.3 Calculate sample glycogen levels

``` r
# Create sample data frame for N0.A
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_N0.A_mean_lum <- numeric(length(glyc_N0.A_dilution))
glyc_N0.A_se_lum <- numeric(length(glyc_N0.A_dilution))
glyc_N0.A_mean_conc <- numeric(length(glyc_N0.A_dilution))

for (i in 1:length(glyc_N0.A_dilution)) {
  df_val <- glyc_N0.A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_N0.A_lum_values <- c(glyc_N0.A_luminescence[glyc_N0.A_dilution == df_val])
  glyc_N0.A_mean_lum[i] <- mean(glyc_N0.A_lum_values)
  glyc_N0.A_se_lum[i] <- sd(glyc_N0.A_lum_values) / sqrt(length(glyc_N0.A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_N0.A_mean_conc[i] <- (glyc_N0.A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_N0.A_data <- data.frame(
  glyc_N0.A_dilution_factor = glyc_N0.A_dilution,
  glyc_N0.A_mean_luminescence = glyc_N0.A_mean_lum,
  glyc_N0.A_se = glyc_N0.A_se_lum,
  glyc_N0.A_conc =  glyc_N0.A_mean_conc,
  label = paste0("N0.Adf.", glyc_N0.A_dilution)
)
glyc_N0.A_mean_lum
glyc_N0.A_mean_conc
```

    [1] 2537.667
    [1] 1.702663

``` r
# Create sample data frame for N0.B
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_N0.B_mean_lum <- numeric(length(glyc_N0.B_dilution))
glyc_N0.B_se_lum <- numeric(length(glyc_N0.B_dilution))
glyc_N0.B_mean_conc <- numeric(length(glyc_N0.B_dilution))

for (i in 1:length(glyc_N0.B_dilution)) {
  df_val <- glyc_N0.B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_N0.B_lum_values <- c(glyc_N0.B_luminescence[glyc_N0.B_dilution == df_val])
  glyc_N0.B_mean_lum[i] <- mean(glyc_N0.B_lum_values)
  glyc_N0.B_se_lum[i] <- sd(glyc_N0.B_lum_values) / sqrt(length(glyc_N0.B_lum_values))
  # Calculate concentration from mean luminescence
  glyc_N0.B_mean_conc[i] <- (glyc_N0.B_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_N0.B_data <- data.frame(
  glyc_N0.B_dilution_factor = glyc_N0.B_dilution,
  glyc_N0.B_mean_luminescence = glyc_N0.B_mean_lum,
  glyc_N0.B_se = glyc_N0.B_se_lum,
  glyc_N0.B_conc =  glyc_N0.B_mean_conc,
  label = paste0("N0.Bdf.", glyc_N0.B_dilution)
)
glyc_N0.B_mean_lum
glyc_N0.B_mean_conc
```

    [1] 4356
    [1] 2.963986

``` r
# Create sample data frame for N0.C
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_N0.C_mean_lum <- numeric(length(glyc_N0.C_dilution))
glyc_N0.C_se_lum <- numeric(length(glyc_N0.C_dilution))
glyc_N0.C_mean_conc <- numeric(length(glyc_N0.C_dilution))

for (i in 1:length(glyc_N0.C_dilution)) {
  df_val <- glyc_N0.C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_N0.C_lum_values <- c(glyc_N0.C_luminescence[glyc_N0.C_dilution == df_val])
  glyc_N0.C_mean_lum[i] <- mean(glyc_N0.C_lum_values)
  glyc_N0.C_se_lum[i] <- sd(glyc_N0.C_lum_values) / sqrt(length(glyc_N0.C_lum_values))
  # Calculate concentration from mean luminescence
  glyc_N0.C_mean_conc[i] <- (glyc_N0.C_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_N0.C_data <- data.frame(
  glyc_N0.C_dilution_factor = glyc_N0.C_dilution,
  glyc_N0.C_mean_luminescence = glyc_N0.C_mean_lum,
  glyc_N0.C_se = glyc_N0.C_se_lum,
  glyc_N0.C_conc =  glyc_N0.C_mean_conc,
  label = paste0("N0.Cdf.", glyc_N0.C_dilution)
)
glyc_N0.C_mean_lum
glyc_N0.C_mean_conc
```

    [1] 1271
    [1] 0.8240151

``` r
# Create sample data frame for H0.A
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_H0.A_mean_lum <- numeric(length(glyc_H0.A_dilution))
glyc_H0.A_se_lum <- numeric(length(glyc_H0.A_dilution))
glyc_H0.A_mean_conc <- numeric(length(glyc_H0.A_dilution))

for (i in 1:length(glyc_H0.A_dilution)) {
  df_val <- glyc_H0.A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_H0.A_lum_values <- c(glyc_H0.A_luminescence[glyc_H0.A_dilution == df_val])
  glyc_H0.A_mean_lum[i] <- mean(glyc_H0.A_lum_values)
  glyc_H0.A_se_lum[i] <- sd(glyc_H0.A_lum_values) / sqrt(length(glyc_H0.A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_H0.A_mean_conc[i] <- (glyc_H0.A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_H0.A_data <- data.frame(
  glyc_H0.A_dilution_factor = glyc_H0.A_dilution,
  glyc_H0.A_mean_luminescence = glyc_H0.A_mean_lum,
  glyc_H0.A_se = glyc_H0.A_se_lum,
  glyc_H0.A_conc =  glyc_H0.A_mean_conc,
  label = paste0("H0.Adf.", glyc_H0.A_dilution)
)
glyc_H0.A_mean_lum
glyc_H0.A_mean_conc
```

    [1] 1075
    [1] 0.6880558

``` r
# Create sample data frame for H0.B
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_H0.B_mean_lum <- numeric(length(glyc_H0.B_dilution))
glyc_H0.B_se_lum <- numeric(length(glyc_H0.B_dilution))
glyc_H0.B_mean_conc <- numeric(length(glyc_H0.B_dilution))

for (i in 1:length(glyc_H0.B_dilution)) {
  df_val <- glyc_H0.B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_H0.B_lum_values <- c(glyc_H0.B_luminescence[glyc_H0.B_dilution == df_val])
  glyc_H0.B_mean_lum[i] <- mean(glyc_H0.B_lum_values)
  glyc_H0.B_se_lum[i] <- sd(glyc_H0.B_lum_values) / sqrt(length(glyc_H0.B_lum_values))
  # Calculate concentration from mean luminescence
  glyc_H0.B_mean_conc[i] <- (glyc_H0.B_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_H0.B_data <- data.frame(
  glyc_H0.B_dilution_factor = glyc_H0.B_dilution,
  glyc_H0.B_mean_luminescence = glyc_H0.B_mean_lum,
  glyc_H0.B_se = glyc_H0.B_se_lum,
  glyc_H0.B_conc =  glyc_H0.B_mean_conc,
  label = paste0("H0.Bdf.", glyc_H0.B_dilution)
)
glyc_H0.B_mean_lum
glyc_H0.B_mean_conc
```

    [1] 727.3333
    [1] 0.4468899

``` r
# Create sample data frame for H0.C
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_H0.C_mean_lum <- numeric(length(glyc_H0.C_dilution))
glyc_H0.C_se_lum <- numeric(length(glyc_H0.C_dilution))
glyc_H0.C_mean_conc <- numeric(length(glyc_H0.C_dilution))

for (i in 1:length(glyc_H0.C_dilution)) {
  df_val <- glyc_H0.C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_H0.C_lum_values <- c(glyc_H0.C_luminescence[glyc_H0.C_dilution == df_val])
  glyc_H0.C_mean_lum[i] <- mean(glyc_H0.C_lum_values)
  glyc_H0.C_se_lum[i] <- sd(glyc_H0.C_lum_values) / sqrt(length(glyc_H0.C_lum_values))
  # Calculate concentration from mean luminescence
  glyc_H0.C_mean_conc[i] <- (glyc_H0.C_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_H0.C_data <- data.frame(
  glyc_H0.C_dilution_factor = glyc_H0.C_dilution,
  glyc_H0.C_mean_luminescence = glyc_H0.C_mean_lum,
  glyc_H0.C_se = glyc_H0.C_se_lum,
  glyc_H0.C_conc =  glyc_H0.C_mean_conc,
  label = paste0("H0.Cdf.", glyc_H0.C_dilution)
)
glyc_H0.C_mean_lum
glyc_H0.C_mean_conc
```

    [1] 1776.667
    [1] 1.174781

``` r
# Create sample data frame for HH.A
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_HH.A_mean_lum <- numeric(length(glyc_HH.A_dilution))
glyc_HH.A_se_lum <- numeric(length(glyc_HH.A_dilution))
glyc_HH.A_mean_conc <- numeric(length(glyc_HH.A_dilution))

for (i in 1:length(glyc_HH.A_dilution)) {
  df_val <- glyc_HH.A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_HH.A_lum_values <- c(glyc_HH.A_luminescence[glyc_HH.A_dilution == df_val])
  glyc_HH.A_mean_lum[i] <- mean(glyc_HH.A_lum_values)
  glyc_HH.A_se_lum[i] <- sd(glyc_HH.A_lum_values) / sqrt(length(glyc_HH.A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_HH.A_mean_conc[i] <- (glyc_HH.A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_HH.A_data <- data.frame(
  glyc_HH.A_dilution_factor = glyc_HH.A_dilution,
  glyc_HH.A_mean_luminescence = glyc_HH.A_mean_lum,
  glyc_HH.A_se = glyc_HH.A_se_lum,
  glyc_HH.A_conc =  glyc_HH.A_mean_conc,
  label = paste0("HH.Adf.", glyc_HH.A_dilution)
)
glyc_HH.A_mean_lum
glyc_HH.A_mean_conc
```

    [1] 834.3333
    [1] 0.5211126

``` r
# Create sample data frame for HH.B
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_HH.B_mean_lum <- numeric(length(glyc_HH.B_dilution))
glyc_HH.B_se_lum <- numeric(length(glyc_HH.B_dilution))
glyc_HH.B_mean_conc <- numeric(length(glyc_HH.B_dilution))

for (i in 1:length(glyc_HH.B_dilution)) {
  df_val <- glyc_HH.B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_HH.B_lum_values <- c(glyc_HH.B_luminescence[glyc_HH.B_dilution == df_val])
  glyc_HH.B_mean_lum[i] <- mean(glyc_HH.B_lum_values)
  glyc_HH.B_se_lum[i] <- sd(glyc_HH.B_lum_values) / sqrt(length(glyc_HH.B_lum_values))
  # Calculate concentration from mean luminescence
  glyc_HH.B_mean_conc[i] <- (glyc_HH.B_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_HH.B_data <- data.frame(
  glyc_HH.B_dilution_factor = glyc_HH.B_dilution,
  glyc_HH.B_mean_luminescence = glyc_HH.B_mean_lum,
  glyc_HH.B_se = glyc_HH.B_se_lum,
  glyc_HH.B_conc =  glyc_HH.B_mean_conc,
  label = paste0("HH.Bdf.", glyc_HH.B_dilution)
)
glyc_HH.B_mean_lum
glyc_HH.B_mean_conc
```

    [1] 801
    [1] 0.4979903

``` r
# Create sample data frame for HH.E
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_HH.E_mean_lum <- numeric(length(glyc_HH.E_dilution))
glyc_HH.E_se_lum <- numeric(length(glyc_HH.E_dilution))
glyc_HH.E_mean_conc <- numeric(length(glyc_HH.E_dilution))

for (i in 1:length(glyc_HH.E_dilution)) {
  df_val <- glyc_HH.E_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_HH.E_lum_values <- c(glyc_HH.E_luminescence[glyc_HH.E_dilution == df_val])
  glyc_HH.E_mean_lum[i] <- mean(glyc_HH.E_lum_values)
  glyc_HH.E_se_lum[i] <- sd(glyc_HH.E_lum_values) / sqrt(length(glyc_HH.E_lum_values))
  # Calculate concentration from mean luminescence
  glyc_HH.E_mean_conc[i] <- (glyc_HH.E_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_HH.E_data <- data.frame(
  glyc_HH.E_dilution_factor = glyc_HH.E_dilution,
  glyc_HH.E_mean_luminescence = glyc_HH.E_mean_lum,
  glyc_HH.E_se = glyc_HH.E_se_lum,
  glyc_HH.E_conc =  glyc_HH.E_mean_conc,
  label = paste0("HH.Edf.", glyc_HH.E_dilution)
)
glyc_HH.E_mean_lum
glyc_HH.E_mean_conc
```

    [1] 575.3333
    [1] 0.3414521

``` r
# Create sample data frame for HH.F
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_HH.F_mean_lum <- numeric(length(glyc_HH.F_dilution))
glyc_HH.F_se_lum <- numeric(length(glyc_HH.F_dilution))
glyc_HH.F_mean_conc <- numeric(length(glyc_HH.F_dilution))

for (i in 1:length(glyc_HH.F_dilution)) {
  df_val <- glyc_HH.F_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_HH.F_lum_values <- c(glyc_HH.F_luminescence[glyc_HH.F_dilution == df_val])
  glyc_HH.F_mean_lum[i] <- mean(glyc_HH.F_lum_values)
  glyc_HH.F_se_lum[i] <- sd(glyc_HH.F_lum_values) / sqrt(length(glyc_HH.F_lum_values))
  # Calculate concentration from mean luminescence
  glyc_HH.F_mean_conc[i] <- (glyc_HH.F_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_HH.F_data <- data.frame(
  glyc_HH.F_dilution_factor = glyc_HH.F_dilution,
  glyc_HH.F_mean_luminescence = glyc_HH.F_mean_lum,
  glyc_HH.F_se = glyc_HH.F_se_lum,
  glyc_HH.F_conc =  glyc_HH.F_mean_conc,
  label = paste0("HH.Fdf.", glyc_HH.F_dilution)
)
glyc_HH.F_mean_lum
glyc_HH.F_mean_conc
```

    [1] 701
    [1] 0.4286233

``` r
# Create sample data frame for NH.A
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_NH.A_mean_lum <- numeric(length(glyc_NH.A_dilution))
glyc_NH.A_se_lum <- numeric(length(glyc_NH.A_dilution))
glyc_NH.A_mean_conc <- numeric(length(glyc_NH.A_dilution))

for (i in 1:length(glyc_NH.A_dilution)) {
  df_val <- glyc_NH.A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_NH.A_lum_values <- c(glyc_NH.A_luminescence[glyc_NH.A_dilution == df_val])
  glyc_NH.A_mean_lum[i] <- mean(glyc_NH.A_lum_values)
  glyc_NH.A_se_lum[i] <- sd(glyc_NH.A_lum_values) / sqrt(length(glyc_NH.A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_NH.A_mean_conc[i] <- (glyc_NH.A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_NH.A_data <- data.frame(
  glyc_NH.A_dilution_factor = glyc_NH.A_dilution,
  glyc_NH.A_mean_luminescence = glyc_NH.A_mean_lum,
  glyc_NH.A_se = glyc_NH.A_se_lum,
  glyc_NH.A_conc =  glyc_NH.A_mean_conc,
  label = paste0("NH.Adf.", glyc_NH.A_dilution)
)
glyc_NH.A_mean_lum
glyc_NH.A_mean_conc
```

    [1] 803.3333
    [1] 0.4996088

``` r
# Create sample data frame for NH.C
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_NH.C_mean_lum <- numeric(length(glyc_NH.C_dilution))
glyc_NH.C_se_lum <- numeric(length(glyc_NH.C_dilution))
glyc_NH.C_mean_conc <- numeric(length(glyc_NH.C_dilution))

for (i in 1:length(glyc_NH.C_dilution)) {
  df_val <- glyc_NH.C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_NH.C_lum_values <- c(glyc_NH.C_luminescence[glyc_NH.C_dilution == df_val])
  glyc_NH.C_mean_lum[i] <- mean(glyc_NH.C_lum_values)
  glyc_NH.C_se_lum[i] <- sd(glyc_NH.C_lum_values) / sqrt(length(glyc_NH.C_lum_values))
  # Calculate concentration from mean luminescence
  glyc_NH.C_mean_conc[i] <- (glyc_NH.C_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_NH.C_data <- data.frame(
  glyc_NH.C_dilution_factor = glyc_NH.C_dilution,
  glyc_NH.C_mean_luminescence = glyc_NH.C_mean_lum,
  glyc_NH.C_se = glyc_NH.C_se_lum,
  glyc_NH.C_conc =  glyc_NH.C_mean_conc,
  label = paste0("NH.Cdf.", glyc_NH.C_dilution)
)
glyc_NH.C_mean_lum
glyc_NH.C_mean_conc
```

    [1] 698.6667
    [1] 0.4270047

``` r
# Create sample data frame for NH.D
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_NH.D_mean_lum <- numeric(length(glyc_NH.D_dilution))
glyc_NH.D_se_lum <- numeric(length(glyc_NH.D_dilution))
glyc_NH.D_mean_conc <- numeric(length(glyc_NH.D_dilution))

for (i in 1:length(glyc_NH.D_dilution)) {
  df_val <- glyc_NH.D_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_NH.D_lum_values <- c(glyc_NH.D_luminescence[glyc_NH.D_dilution == df_val])
  glyc_NH.D_mean_lum[i] <- mean(glyc_NH.D_lum_values)
  glyc_NH.D_se_lum[i] <- sd(glyc_NH.D_lum_values) / sqrt(length(glyc_NH.D_lum_values))
  # Calculate concentration from mean luminescence
  glyc_NH.D_mean_conc[i] <- (glyc_NH.D_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_NH.D_data <- data.frame(
  glyc_NH.D_dilution_factor = glyc_NH.D_dilution,
  glyc_NH.D_mean_luminescence = glyc_NH.D_mean_lum,
  glyc_NH.D_se = glyc_NH.D_se_lum,
  glyc_NH.D_conc =  glyc_NH.D_mean_conc,
  label = paste0("NH.Ddf.", glyc_NH.D_dilution)
)
glyc_NH.D_mean_lum
glyc_NH.D_mean_conc
```

    [1] 987.3333
    [1] 0.6272441

``` r
# Create sample data frame for NH.F
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_NH.F_mean_lum <- numeric(length(glyc_NH.F_dilution))
glyc_NH.F_se_lum <- numeric(length(glyc_NH.F_dilution))
glyc_NH.F_mean_conc <- numeric(length(glyc_NH.F_dilution))

for (i in 1:length(glyc_NH.F_dilution)) {
  df_val <- glyc_NH.F_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_NH.F_lum_values <- c(glyc_NH.F_luminescence[glyc_NH.F_dilution == df_val])
  glyc_NH.F_mean_lum[i] <- mean(glyc_NH.F_lum_values)
  glyc_NH.F_se_lum[i] <- sd(glyc_NH.F_lum_values) / sqrt(length(glyc_NH.F_lum_values))
  # Calculate concentration from mean luminescence
  glyc_NH.F_mean_conc[i] <- (glyc_NH.F_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_NH.F_data <- data.frame(
  glyc_NH.F_dilution_factor = glyc_NH.F_dilution,
  glyc_NH.F_mean_luminescence = glyc_NH.F_mean_lum,
  glyc_NH.F_se = glyc_NH.F_se_lum,
  glyc_NH.F_conc =  glyc_NH.F_mean_conc,
  label = paste0("NH.Fdf.", glyc_NH.F_dilution)
)
glyc_NH.F_mean_lum
glyc_NH.F_mean_conc
```

    [1] 829.3333
    [1] 0.5176443

``` r
# Create sample data frame for HN.A
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_HN.A_mean_lum <- numeric(length(glyc_HN.A_dilution))
glyc_HN.A_se_lum <- numeric(length(glyc_HN.A_dilution))
glyc_HN.A_mean_conc <- numeric(length(glyc_HN.A_dilution))

for (i in 1:length(glyc_HN.A_dilution)) {
  df_val <- glyc_HN.A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_HN.A_lum_values <- c(glyc_HN.A_luminescence[glyc_HN.A_dilution == df_val])
  glyc_HN.A_mean_lum[i] <- mean(glyc_HN.A_lum_values)
  glyc_HN.A_se_lum[i] <- sd(glyc_HN.A_lum_values) / sqrt(length(glyc_HN.A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_HN.A_mean_conc[i] <- (glyc_HN.A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_HN.A_data <- data.frame(
  glyc_HN.A_dilution_factor = glyc_HN.A_dilution,
  glyc_HN.A_mean_luminescence = glyc_HN.A_mean_lum,
  glyc_HN.A_se = glyc_HN.A_se_lum,
  glyc_HN.A_conc =  glyc_HN.A_mean_conc,
  label = paste0("HN.Adf.", glyc_HN.A_dilution)
)
glyc_HN.A_mean_lum
glyc_HN.A_mean_conc
```

    [1] 3295.333
    [1] 2.228234

``` r
# Create sample data frame for HN.B
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_HN.B_mean_lum <- numeric(length(glyc_HN.B_dilution))
glyc_HN.B_se_lum <- numeric(length(glyc_HN.B_dilution))
glyc_HN.B_mean_conc <- numeric(length(glyc_HN.B_dilution))

for (i in 1:length(glyc_HN.B_dilution)) {
  df_val <- glyc_HN.B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_HN.B_lum_values <- c(glyc_HN.B_luminescence[glyc_HN.B_dilution == df_val])
  glyc_HN.B_mean_lum[i] <- mean(glyc_HN.B_lum_values)
  glyc_HN.B_se_lum[i] <- sd(glyc_HN.B_lum_values) / sqrt(length(glyc_HN.B_lum_values))
  # Calculate concentration from mean luminescence
  glyc_HN.B_mean_conc[i] <- (glyc_HN.B_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_HN.B_data <- data.frame(
  glyc_HN.B_dilution_factor = glyc_HN.B_dilution,
  glyc_HN.B_mean_luminescence = glyc_HN.B_mean_lum,
  glyc_HN.B_se = glyc_HN.B_se_lum,
  glyc_HN.B_conc =  glyc_HN.B_mean_conc,
  label = paste0("HN.Bdf.", glyc_HN.B_dilution)
)
glyc_HN.B_mean_lum
glyc_HN.B_mean_conc
```

    [1] 2000.333
    [1] 1.329932

``` r
# Create sample data frame for HN.C
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_HN.C_mean_lum <- numeric(length(glyc_HN.C_dilution))
glyc_HN.C_se_lum <- numeric(length(glyc_HN.C_dilution))
glyc_HN.C_mean_conc <- numeric(length(glyc_HN.C_dilution))

for (i in 1:length(glyc_HN.C_dilution)) {
  df_val <- glyc_HN.C_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_HN.C_lum_values <- c(glyc_HN.C_luminescence[glyc_HN.C_dilution == df_val])
  glyc_HN.C_mean_lum[i] <- mean(glyc_HN.C_lum_values)
  glyc_HN.C_se_lum[i] <- sd(glyc_HN.C_lum_values) / sqrt(length(glyc_HN.C_lum_values))
  # Calculate concentration from mean luminescence
  glyc_HN.C_mean_conc[i] <- (glyc_HN.C_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_HN.C_data <- data.frame(
  glyc_HN.C_dilution_factor = glyc_HN.C_dilution,
  glyc_HN.C_mean_luminescence = glyc_HN.C_mean_lum,
  glyc_HN.C_se = glyc_HN.C_se_lum,
  glyc_HN.C_conc =  glyc_HN.C_mean_conc,
  label = paste0("HN.Cdf.", glyc_HN.C_dilution)
)
glyc_HN.C_mean_lum
glyc_HN.C_mean_conc
```

    [1] 2202.667
    [1] 1.470284

``` r
# Create sample data frame for HN.D
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_HN.D_mean_lum <- numeric(length(glyc_HN.D_dilution))
glyc_HN.D_se_lum <- numeric(length(glyc_HN.D_dilution))
glyc_HN.D_mean_conc <- numeric(length(glyc_HN.D_dilution))

for (i in 1:length(glyc_HN.D_dilution)) {
  df_val <- glyc_HN.D_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_HN.D_lum_values <- c(glyc_HN.D_luminescence[glyc_HN.D_dilution == df_val])
  glyc_HN.D_mean_lum[i] <- mean(glyc_HN.D_lum_values)
  glyc_HN.D_se_lum[i] <- sd(glyc_HN.D_lum_values) / sqrt(length(glyc_HN.D_lum_values))
  # Calculate concentration from mean luminescence
  glyc_HN.D_mean_conc[i] <- (glyc_HN.D_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_HN.D_data <- data.frame(
  glyc_HN.D_dilution_factor = glyc_HN.D_dilution,
  glyc_HN.D_mean_luminescence = glyc_HN.D_mean_lum,
  glyc_HN.D_se = glyc_HN.D_se_lum,
  glyc_HN.D_conc =  glyc_HN.D_mean_conc,
  label = paste0("HN.Ddf.", glyc_HN.D_dilution)
)
glyc_HN.D_mean_lum
glyc_HN.D_mean_conc
```

    [1] 3597.333
    [1] 2.437722

``` r
# Create sample data frame for NN.A
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_NN.A_mean_lum <- numeric(length(glyc_NN.A_dilution))
glyc_NN.A_se_lum <- numeric(length(glyc_NN.A_dilution))
glyc_NN.A_mean_conc <- numeric(length(glyc_NN.A_dilution))

for (i in 1:length(glyc_NN.A_dilution)) {
  df_val <- glyc_NN.A_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_NN.A_lum_values <- c(glyc_NN.A_luminescence[glyc_NN.A_dilution == df_val])
  glyc_NN.A_mean_lum[i] <- mean(glyc_NN.A_lum_values)
  glyc_NN.A_se_lum[i] <- sd(glyc_NN.A_lum_values) / sqrt(length(glyc_NN.A_lum_values))
  # Calculate concentration from mean luminescence
  glyc_NN.A_mean_conc[i] <- (glyc_NN.A_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_NN.A_data <- data.frame(
  glyc_NN.A_dilution_factor = glyc_NN.A_dilution,
  glyc_NN.A_mean_luminescence = glyc_NN.A_mean_lum,
  glyc_NN.A_se = glyc_NN.A_se_lum,
  glyc_NN.A_conc =  glyc_NN.A_mean_conc,
  label = paste0("NN.Adf.", glyc_NN.A_dilution)
)
glyc_NN.A_mean_lum
glyc_NN.A_mean_conc
```

    [1] 2189
    [1] 1.460804

``` r
# Create sample data frame for NN.B
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_NN.B_mean_lum <- numeric(length(glyc_NN.B_dilution))
glyc_NN.B_se_lum <- numeric(length(glyc_NN.B_dilution))
glyc_NN.B_mean_conc <- numeric(length(glyc_NN.B_dilution))

for (i in 1:length(glyc_NN.B_dilution)) {
  df_val <- glyc_NN.B_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_NN.B_lum_values <- c(glyc_NN.B_luminescence[glyc_NN.B_dilution == df_val])
  glyc_NN.B_mean_lum[i] <- mean(glyc_NN.B_lum_values)
  glyc_NN.B_se_lum[i] <- sd(glyc_NN.B_lum_values) / sqrt(length(glyc_NN.B_lum_values))
  # Calculate concentration from mean luminescence
  glyc_NN.B_mean_conc[i] <- (glyc_NN.B_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_NN.B_data <- data.frame(
  glyc_NN.B_dilution_factor = glyc_NN.B_dilution,
  glyc_NN.B_mean_luminescence = glyc_NN.B_mean_lum,
  glyc_NN.B_se = glyc_NN.B_se_lum,
  glyc_NN.B_conc =  glyc_NN.B_mean_conc,
  label = paste0("NN.Bdf.", glyc_NN.B_dilution)
)
glyc_NN.B_mean_lum
glyc_NN.B_mean_conc
```

    [1] 2146.333
    [1] 1.431207

``` r
# Create sample data frame for NN.D
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_NN.D_mean_lum <- numeric(length(glyc_NN.D_dilution))
glyc_NN.D_se_lum <- numeric(length(glyc_NN.D_dilution))
glyc_NN.D_mean_conc <- numeric(length(glyc_NN.D_dilution))

for (i in 1:length(glyc_NN.D_dilution)) {
  df_val <- glyc_NN.D_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_NN.D_lum_values <- c(glyc_NN.D_luminescence[glyc_NN.D_dilution == df_val])
  glyc_NN.D_mean_lum[i] <- mean(glyc_NN.D_lum_values)
  glyc_NN.D_se_lum[i] <- sd(glyc_NN.D_lum_values) / sqrt(length(glyc_NN.D_lum_values))
  # Calculate concentration from mean luminescence
  glyc_NN.D_mean_conc[i] <- (glyc_NN.D_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_NN.D_data <- data.frame(
  glyc_NN.D_dilution_factor = glyc_NN.D_dilution,
  glyc_NN.D_mean_luminescence = glyc_NN.D_mean_lum,
  glyc_NN.D_se = glyc_NN.D_se_lum,
  glyc_NN.D_conc =  glyc_NN.D_mean_conc,
  label = paste0("NN.Ddf.", glyc_NN.D_dilution)
)
glyc_NN.D_mean_lum
glyc_NN.D_mean_conc
```

    [1] 3019.333
    [1] 2.036781

``` r
# Create sample data frame for NN.E
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_NN.E_mean_lum <- numeric(length(glyc_NN.E_dilution))
glyc_NN.E_se_lum <- numeric(length(glyc_NN.E_dilution))
glyc_NN.E_mean_conc <- numeric(length(glyc_NN.E_dilution))

for (i in 1:length(glyc_NN.E_dilution)) {
  df_val <- glyc_NN.E_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_NN.E_lum_values <- c(glyc_NN.E_luminescence[glyc_NN.E_dilution == df_val])
  glyc_NN.E_mean_lum[i] <- mean(glyc_NN.E_lum_values)
  glyc_NN.E_se_lum[i] <- sd(glyc_NN.E_lum_values) / sqrt(length(glyc_NN.E_lum_values))
  # Calculate concentration from mean luminescence
  glyc_NN.E_mean_conc[i] <- (glyc_NN.E_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_NN.E_data <- data.frame(
  glyc_NN.E_dilution_factor = glyc_NN.E_dilution,
  glyc_NN.E_mean_luminescence = glyc_NN.E_mean_lum,
  glyc_NN.E_se = glyc_NN.E_se_lum,
  glyc_NN.E_conc =  glyc_NN.E_mean_conc,
  label = paste0("NN.Edf.", glyc_NN.E_dilution)
)
glyc_NN.E_mean_lum
glyc_NN.E_mean_conc
```

    [1] 8133.667
    [1] 5.584439

``` r
# Create sample data frame for NN.F
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_NN.F_mean_lum <- numeric(length(glyc_NN.F_dilution))
glyc_NN.F_se_lum <- numeric(length(glyc_NN.F_dilution))
glyc_NN.F_mean_conc <- numeric(length(glyc_NN.F_dilution))

for (i in 1:length(glyc_NN.F_dilution)) {
  df_val <- glyc_NN.F_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_NN.F_lum_values <- c(glyc_NN.F_luminescence[glyc_NN.F_dilution == df_val])
  glyc_NN.F_mean_lum[i] <- mean(glyc_NN.F_lum_values)
  glyc_NN.F_se_lum[i] <- sd(glyc_NN.F_lum_values) / sqrt(length(glyc_NN.F_lum_values))
  # Calculate concentration from mean luminescence
  glyc_NN.F_mean_conc[i] <- (glyc_NN.F_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_NN.F_data <- data.frame(
  glyc_NN.F_dilution_factor = glyc_NN.F_dilution,
  glyc_NN.F_mean_luminescence = glyc_NN.F_mean_lum,
  glyc_NN.F_se = glyc_NN.F_se_lum,
  glyc_NN.F_conc =  glyc_NN.F_mean_conc,
  label = paste0("NN.Fdf.", glyc_NN.F_dilution)
)
glyc_NN.F_mean_lum
glyc_NN.F_mean_conc
```

    [1] 8945
    [1] 6.147237

## 2.4 Plot glycogen standard curve, sample points

``` r
# Create the plot
glyc_plot <- ggplot(glycogen_summary_data, aes(x = glyc_concentration, y = glyc_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glyc_mean_luminescence - glyc_se, ymax = glyc_mean_luminescence + glyc_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +
  
   geom_errorbar(data = glyc_N0.A_data, aes(x = glyc_N0.A_conc, y = glyc_N0.A_mean_luminescence,
                ymin = glyc_N0.A_mean_luminescence - glyc_N0.A_se, ymax = glyc_N0.A_mean_luminescence + glyc_N0.A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_N0.A_data, aes(x = glyc_N0.A_conc, y = glyc_N0.A_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_N0.B_data, aes(x = glyc_N0.B_conc, y = glyc_N0.B_mean_luminescence,
                ymin = glyc_N0.B_mean_luminescence - glyc_N0.B_se, ymax = glyc_N0.B_mean_luminescence + glyc_N0.B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_N0.B_data, aes(x = glyc_N0.B_conc, y = glyc_N0.B_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_N0.C_data, aes(x = glyc_N0.C_conc, y = glyc_N0.C_mean_luminescence,
                ymin = glyc_N0.C_mean_luminescence - glyc_N0.C_se, ymax = glyc_N0.C_mean_luminescence + glyc_N0.C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_N0.C_data, aes(x = glyc_N0.C_conc, y = glyc_N0.C_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_H0.A_data, aes(x = glyc_H0.A_conc, y = glyc_H0.A_mean_luminescence,
                ymin = glyc_H0.A_mean_luminescence - glyc_H0.A_se, ymax = glyc_H0.A_mean_luminescence + glyc_H0.A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_H0.A_data, aes(x = glyc_H0.A_conc, y = glyc_H0.A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_H0.B_data, aes(x = glyc_H0.B_conc, y = glyc_H0.B_mean_luminescence,
                ymin = glyc_H0.B_mean_luminescence - glyc_H0.B_se, ymax = glyc_H0.B_mean_luminescence + glyc_H0.B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_H0.B_data, aes(x = glyc_H0.B_conc, y = glyc_H0.B_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_H0.C_data, aes(x = glyc_H0.C_conc, y = glyc_H0.C_mean_luminescence,
                ymin = glyc_H0.C_mean_luminescence - glyc_H0.C_se, ymax = glyc_H0.C_mean_luminescence + glyc_H0.C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_H0.C_data, aes(x = glyc_H0.C_conc, y = glyc_H0.C_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_HH.A_data, aes(x = glyc_HH.A_conc, y = glyc_HH.A_mean_luminescence,
                ymin = glyc_HH.A_mean_luminescence - glyc_HH.A_se, ymax = glyc_HH.A_mean_luminescence + glyc_HH.A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_HH.A_data, aes(x = glyc_HH.A_conc, y = glyc_HH.A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_HH.B_data, aes(x = glyc_HH.B_conc, y = glyc_HH.B_mean_luminescence,
                ymin = glyc_HH.B_mean_luminescence - glyc_HH.B_se, ymax = glyc_HH.B_mean_luminescence + glyc_HH.B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_HH.B_data, aes(x = glyc_HH.B_conc, y = glyc_HH.B_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_HH.E_data, aes(x = glyc_HH.E_conc, y = glyc_HH.E_mean_luminescence,
                ymin = glyc_HH.E_mean_luminescence - glyc_HH.E_se, ymax = glyc_HH.E_mean_luminescence + glyc_HH.E_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_HH.E_data, aes(x = glyc_HH.E_conc, y = glyc_HH.E_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_HH.F_data, aes(x = glyc_HH.F_conc, y = glyc_HH.F_mean_luminescence,
                ymin = glyc_HH.F_mean_luminescence - glyc_HH.F_se, ymax = glyc_HH.F_mean_luminescence + glyc_HH.F_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_HH.F_data, aes(x = glyc_HH.F_conc, y = glyc_HH.F_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_NH.A_data, aes(x = glyc_NH.A_conc, y = glyc_NH.A_mean_luminescence,
                ymin = glyc_NH.A_mean_luminescence - glyc_NH.A_se, ymax = glyc_NH.A_mean_luminescence + glyc_NH.A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_NH.A_data, aes(x = glyc_NH.A_conc, y = glyc_NH.A_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_NH.C_data, aes(x = glyc_NH.C_conc, y = glyc_NH.C_mean_luminescence,
                ymin = glyc_NH.C_mean_luminescence - glyc_NH.C_se, ymax = glyc_NH.C_mean_luminescence + glyc_NH.C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_NH.C_data, aes(x = glyc_NH.C_conc, y = glyc_NH.C_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_NH.D_data, aes(x = glyc_NH.D_conc, y = glyc_NH.D_mean_luminescence,
                ymin = glyc_NH.D_mean_luminescence - glyc_NH.D_se, ymax = glyc_NH.D_mean_luminescence + glyc_NH.D_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_NH.D_data, aes(x = glyc_NH.D_conc, y = glyc_NH.D_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_NH.F_data, aes(x = glyc_NH.F_conc, y = glyc_NH.F_mean_luminescence,
                ymin = glyc_NH.F_mean_luminescence - glyc_NH.F_se, ymax = glyc_NH.F_mean_luminescence + glyc_NH.F_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_NH.F_data, aes(x = glyc_NH.F_conc, y = glyc_NH.F_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_HN.A_data, aes(x = glyc_HN.A_conc, y = glyc_HN.A_mean_luminescence,
                ymin = glyc_HN.A_mean_luminescence - glyc_HN.A_se, ymax = glyc_HN.A_mean_luminescence + glyc_HN.A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_HN.A_data, aes(x = glyc_HN.A_conc, y = glyc_HN.A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_HN.B_data, aes(x = glyc_HN.B_conc, y = glyc_HN.B_mean_luminescence,
                ymin = glyc_HN.B_mean_luminescence - glyc_HN.B_se, ymax = glyc_HN.B_mean_luminescence + glyc_HN.B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_HN.B_data, aes(x = glyc_HN.B_conc, y = glyc_HN.B_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_HN.C_data, aes(x = glyc_HN.C_conc, y = glyc_HN.C_mean_luminescence,
                ymin = glyc_HN.C_mean_luminescence - glyc_HN.C_se, ymax = glyc_HN.C_mean_luminescence + glyc_HN.C_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_HN.C_data, aes(x = glyc_HN.C_conc, y = glyc_HN.C_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_HN.D_data, aes(x = glyc_HN.D_conc, y = glyc_HN.D_mean_luminescence,
                ymin = glyc_HN.D_mean_luminescence - glyc_HN.D_se, ymax = glyc_HN.D_mean_luminescence + glyc_HN.D_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_HN.D_data, aes(x = glyc_HN.D_conc, y = glyc_HN.D_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_NN.A_data, aes(x = glyc_NN.A_conc, y = glyc_NN.A_mean_luminescence,
                ymin = glyc_NN.A_mean_luminescence - glyc_NN.A_se, ymax = glyc_NN.A_mean_luminescence + glyc_NN.A_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_NN.A_data, aes(x = glyc_NN.A_conc, y = glyc_NN.A_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_NN.B_data, aes(x = glyc_NN.B_conc, y = glyc_NN.B_mean_luminescence,
                ymin = glyc_NN.B_mean_luminescence - glyc_NN.B_se, ymax = glyc_NN.B_mean_luminescence + glyc_NN.B_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_NN.B_data, aes(x = glyc_NN.B_conc, y = glyc_NN.B_mean_luminescence,
             color = label, shape = label), size = 4) +

    geom_errorbar(data = glyc_NN.D_data, aes(x = glyc_NN.D_conc, y = glyc_NN.D_mean_luminescence,
                ymin = glyc_NN.D_mean_luminescence - glyc_NN.D_se, ymax = glyc_NN.D_mean_luminescence + glyc_NN.D_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_NN.D_data, aes(x = glyc_NN.D_conc, y = glyc_NN.D_mean_luminescence,
             color = label, shape = label), size = 4) +
  
      geom_errorbar(data = glyc_NN.E_data, aes(x = glyc_NN.E_conc, y = glyc_NN.E_mean_luminescence,
                ymin = glyc_NN.E_mean_luminescence - glyc_NN.E_se, ymax = glyc_NN.E_mean_luminescence + glyc_NN.E_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_NN.E_data, aes(x = glyc_NN.E_conc, y = glyc_NN.E_mean_luminescence,
             color = label, shape = label), size = 4) +
  
    geom_errorbar(data = glyc_NN.F_data, aes(x = glyc_NN.F_conc, y = glyc_NN.F_mean_luminescence,
                ymin = glyc_NN.F_mean_luminescence - glyc_NN.F_se, ymax = glyc_NN.F_mean_luminescence + glyc_NN.F_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_NN.F_data, aes(x = glyc_NN.F_conc, y = glyc_NN.F_mean_luminescence,
             color = label, shape = label), size = 4) +

  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                               "N0.Adf.20" = "darkorange",
                                "N0.Bdf.20" = "darkgreen",
                                "N0.Cdf.20" = "purple",
                                "H0.Adf.20" = "brown",
                                "H0.Bdf.20" = "green3",
                                "H0.Cdf.20" = "firebrick1",
                                "HH.Adf.20" = "cyan",
                                "HH.Bdf.20" = "yellow3",
                                "HH.Edf.20" = "thistle3",
                                "HH.Fdf.20" = "azure4",
                                "NH.Adf.20" = "bisque4",
                                "NH.Cdf.20" = "chartreuse",
                                "NH.Ddf.20" = "chocolate4",
                                "NH.Fdf.20" = "darkslategray",
                                "HN.Adf.20" = "deeppink2",
                                "HN.Bdf.20" = "goldenrod3",
                                "HN.Cdf.20" = "hotpink3",
                                "HN.Ddf.20" = "mediumpurple2",
                                "NN.Adf.20" = "yellow",
                                "NN.Bdf.20" = "gray1",
                                "NN.Ddf.20" = "darkcyan",
                                "NN.Edf.20" = "darkorange",
                                "NN.Fdf.20" = "darkgreen"
                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                "N0.Adf.20" = 16,
                                "N0.Bdf.20" = 17,
                                "N0.Cdf.20" = 15,
                                "H0.Adf.20" = 18,
                                "H0.Bdf.20" = 8,
                                "H0.Cdf.20" = 4,
                                "HH.Adf.20" = 5,
                                "HH.Bdf.20" = 0,
                                "HH.Edf.20" = 2,
                                "HH.Fdf.20" = 19,
                                "NH.Adf.20" = 20,
                                "NH.Cdf.20" = 17,
                                "NH.Ddf.20" = 15,
                                "NH.Fdf.20" = 18,
                                "HN.Adf.20" = 8,
                                "HN.Bdf.20" = 4,
                                "HN.Cdf.20" = 0,
                                "HN.Ddf.20" = 2,
                                "NN.Adf.20" = 19,
                                "NN.Bdf.20" = 20,
                                "NN.Ddf.20" = 5, 
                                "NN.Edf.20" = 0, 
                                "NN.Fdf.20" = 2 
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

![](Gen5-20260629C-mgig-glycogenglo_files/figure-gfm/plot%20glycogen%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glyc_N0.A_data
glyc_N0.B_data
glyc_N0.C_data

glyc_H0.A_data
glyc_H0.B_data
glyc_H0.C_data

glyc_HH.A_data
glyc_HH.B_data
glyc_HH.E_data
glyc_HH.F_data

glyc_NH.A_data
glyc_NH.C_data
glyc_NH.D_data
glyc_NH.F_data

glyc_HN.A_data
glyc_HN.B_data
glyc_HN.C_data
glyc_HN.D_data

glyc_NN.A_data
glyc_NN.B_data
glyc_NN.D_data
glyc_NN.E_data
glyc_NN.F_data

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

      glyc_N0.A_dilution_factor glyc_N0.A_mean_luminescence glyc_N0.A_se
    1                        20                    2537.667     72.30568
      glyc_N0.A_conc     label
    1       1.702663 N0.Adf.20
      glyc_N0.B_dilution_factor glyc_N0.B_mean_luminescence glyc_N0.B_se
    1                        20                        4356     27.30079
      glyc_N0.B_conc     label
    1       2.963986 N0.Bdf.20
      glyc_N0.C_dilution_factor glyc_N0.C_mean_luminescence glyc_N0.C_se
    1                        20                        1271     82.03251
      glyc_N0.C_conc     label
    1      0.8240151 N0.Cdf.20
      glyc_H0.A_dilution_factor glyc_H0.A_mean_luminescence glyc_H0.A_se
    1                        20                        1075     47.72141
      glyc_H0.A_conc     label
    1      0.6880558 H0.Adf.20
      glyc_H0.B_dilution_factor glyc_H0.B_mean_luminescence glyc_H0.B_se
    1                        20                    727.3333     28.52679
      glyc_H0.B_conc     label
    1      0.4468899 H0.Bdf.20
      glyc_H0.C_dilution_factor glyc_H0.C_mean_luminescence glyc_H0.C_se
    1                        20                    1776.667     45.28551
      glyc_H0.C_conc     label
    1       1.174781 H0.Cdf.20
      glyc_HH.A_dilution_factor glyc_HH.A_mean_luminescence glyc_HH.A_se
    1                        20                    834.3333     11.28913
      glyc_HH.A_conc     label
    1      0.5211126 HH.Adf.20
      glyc_HH.B_dilution_factor glyc_HH.B_mean_luminescence glyc_HH.B_se
    1                        20                         801     33.60556
      glyc_HH.B_conc     label
    1      0.4979903 HH.Bdf.20
      glyc_HH.E_dilution_factor glyc_HH.E_mean_luminescence glyc_HH.E_se
    1                        20                    575.3333     19.15144
      glyc_HH.E_conc     label
    1      0.3414521 HH.Edf.20
      glyc_HH.F_dilution_factor glyc_HH.F_mean_luminescence glyc_HH.F_se
    1                        20                         701     7.571878
      glyc_HH.F_conc     label
    1      0.4286233 HH.Fdf.20
      glyc_NH.A_dilution_factor glyc_NH.A_mean_luminescence glyc_NH.A_se
    1                        20                    803.3333     18.46017
      glyc_NH.A_conc     label
    1      0.4996088 NH.Adf.20
      glyc_NH.C_dilution_factor glyc_NH.C_mean_luminescence glyc_NH.C_se
    1                        20                    698.6667      29.4241
      glyc_NH.C_conc     label
    1      0.4270047 NH.Cdf.20
      glyc_NH.D_dilution_factor glyc_NH.D_mean_luminescence glyc_NH.D_se
    1                        20                    987.3333     28.75954
      glyc_NH.D_conc     label
    1      0.6272441 NH.Ddf.20
      glyc_NH.F_dilution_factor glyc_NH.F_mean_luminescence glyc_NH.F_se
    1                        20                    829.3333     19.42793
      glyc_NH.F_conc     label
    1      0.5176443 NH.Fdf.20
      glyc_HN.A_dilution_factor glyc_HN.A_mean_luminescence glyc_HN.A_se
    1                        20                    3295.333     98.67342
      glyc_HN.A_conc     label
    1       2.228234 HN.Adf.20
      glyc_HN.B_dilution_factor glyc_HN.B_mean_luminescence glyc_HN.B_se
    1                        20                    2000.333     35.95058
      glyc_HN.B_conc     label
    1       1.329932 HN.Bdf.20
      glyc_HN.C_dilution_factor glyc_HN.C_mean_luminescence glyc_HN.C_se
    1                        20                    2202.667     20.34153
      glyc_HN.C_conc     label
    1       1.470284 HN.Cdf.20
      glyc_HN.D_dilution_factor glyc_HN.D_mean_luminescence glyc_HN.D_se
    1                        20                    3597.333     34.90145
      glyc_HN.D_conc     label
    1       2.437722 HN.Ddf.20
      glyc_NN.A_dilution_factor glyc_NN.A_mean_luminescence glyc_NN.A_se
    1                        20                        2189     457.5074
      glyc_NN.A_conc     label
    1       1.460804 NN.Adf.20
      glyc_NN.B_dilution_factor glyc_NN.B_mean_luminescence glyc_NN.B_se
    1                        20                    2146.333      68.8291
      glyc_NN.B_conc     label
    1       1.431207 NN.Bdf.20
      glyc_NN.D_dilution_factor glyc_NN.D_mean_luminescence glyc_NN.D_se
    1                        20                    3019.333      93.9669
      glyc_NN.D_conc     label
    1       2.036781 NN.Ddf.20
      glyc_NN.E_dilution_factor glyc_NN.E_mean_luminescence glyc_NN.E_se
    1                        20                    8133.667      132.747
      glyc_NN.E_conc     label
    1       5.584439 NN.Edf.20
      glyc_NN.F_dilution_factor glyc_NN.F_mean_luminescence glyc_NN.F_se
    1                        20                        8945     492.2259
      glyc_NN.F_conc     label
    1       6.147237 NN.Fdf.20
    glycogen Standard Curve Summary:
    ==================================================
    Concentration: 20 µg/µL
      Mean Luminescence: 28977.33
      Standard Error: 919.36
      CV%: 5.50%

    Concentration: 2 µg/µL
      Mean Luminescence: 2334.00
      Standard Error: 312.45
      CV%: 23.19%

    Concentration: 0.2 µg/µL
      Mean Luminescence: 466.00
      Standard Error: 5.86
      CV%: 2.18%

    Concentration: 0.02 µg/µL
      Mean Luminescence: 321.00
      Standard Error: 6.43
      CV%: 3.47%

    Concentration: 0 µg/µL
      Mean Luminescence: 349.67
      Standard Error: 8.45
      CV%: 4.19%

## 2.5 Sample glycogen table

``` r
#define tissue weights
glyc_N0.A_weight <- as.numeric(sample_weights[2,2])
glyc_N0.B_weight <- as.numeric(sample_weights[3,2])
glyc_N0.C_weight <- as.numeric(sample_weights[4,2])

glyc_H0.A_weight <- as.numeric(sample_weights[5,2])
glyc_H0.B_weight <- as.numeric(sample_weights[6,2])
glyc_H0.C_weight <- as.numeric(sample_weights[7,2])

glyc_HH.A_weight <- as.numeric(sample_weights[8,2])
glyc_HH.B_weight <- as.numeric(sample_weights[9,2])
glyc_HH.E_weight <- as.numeric(sample_weights[10,2])
glyc_HH.F_weight <- as.numeric(sample_weights[11,2])

glyc_NH.A_weight <- as.numeric(sample_weights[12,2])
glyc_NH.C_weight <- as.numeric(sample_weights[13,2])
glyc_NH.D_weight <- as.numeric(sample_weights[14,2])
glyc_NH.F_weight <- as.numeric(sample_weights[15,2])

glyc_HN.A_weight <- as.numeric(sample_weights[16,2])
glyc_HN.B_weight <- as.numeric(sample_weights[17,2])
glyc_HN.C_weight <- as.numeric(sample_weights[18,2])
glyc_HN.D_weight <- as.numeric(sample_weights[19,2])

glyc_NN.A_weight <- as.numeric(sample_weights[20,2])
glyc_NN.B_weight <- as.numeric(sample_weights[21,2])
glyc_NN.D_weight <- as.numeric(sample_weights[22,2])
glyc_NN.E_weight <- as.numeric(sample_weights[23,2])
glyc_NN.F_weight <- as.numeric(sample_weights[24,2])
```

``` r
#normalize calculated glycogen to sample weight
#plate1
glyc_N0.A_mean_conc_normalized <- glyc_N0.A_mean_conc/glyc_N0.A_weight
glyc_N.0B_mean_conc_normalized <- glyc_N0.B_mean_conc/glyc_N0.B_weight
glyc_N0.C_mean_conc_normalized <- glyc_N0.C_mean_conc/glyc_N0.C_weight
glyc_H0.A_mean_conc_normalized <- glyc_H0.A_mean_conc/glyc_H0.A_weight
glyc_H0.B_mean_conc_normalized <- glyc_H0.B_mean_conc/glyc_H0.B_weight
glyc_H0.C_mean_conc_normalized <- glyc_H0.C_mean_conc/glyc_H0.C_weight
glyc_HH.A_mean_conc_normalized <- glyc_HH.A_mean_conc/glyc_HH.A_weight
glyc_HH.B_mean_conc_normalized <- glyc_HH.B_mean_conc/glyc_HH.B_weight
glyc_HH.E_mean_conc_normalized <- glyc_HH.E_mean_conc/glyc_HH.E_weight
glyc_HH.F_mean_conc_normalized <- glyc_HH.F_mean_conc/glyc_HH.F_weight
glyc_NH.A_mean_conc_normalized <- glyc_NH.A_mean_conc/glyc_NH.A_weight
glyc_NH.C_mean_conc_normalized <- glyc_NH.C_mean_conc/glyc_NH.C_weight
glyc_NH.D_mean_conc_normalized <- glyc_NH.D_mean_conc/glyc_NH.D_weight
glyc_NH.F_mean_conc_normalized <- glyc_NH.F_mean_conc/glyc_NH.F_weight
glyc_HN.A_mean_conc_normalized <- glyc_HN.A_mean_conc/glyc_HN.A_weight
glyc_HN.B_mean_conc_normalized <- glyc_HN.B_mean_conc/glyc_HN.B_weight
glyc_HN.C_mean_conc_normalized <- glyc_HN.C_mean_conc/glyc_HN.C_weight
glyc_HN.D_mean_conc_normalized <- glyc_HN.D_mean_conc/glyc_HN.D_weight
glyc_NN.A_mean_conc_normalized <- glyc_NN.A_mean_conc/glyc_NN.A_weight
glyc_NN.B_mean_conc_normalized <- glyc_NN.B_mean_conc/glyc_NN.B_weight
glyc_NN.D_mean_conc_normalized <- glyc_NN.D_mean_conc/glyc_NN.D_weight
glyc_NN.E_mean_conc_normalized <- glyc_NN.E_mean_conc/glyc_NN.E_weight
glyc_NN.F_mean_conc_normalized <- glyc_NN.F_mean_conc/glyc_NN.F_weight
```

Note: To normalize glycogen levels by dilution factor and tissue weight,
the calculated glycogen is multiplied by (dilution factor/weight). This
is reported as “Normalized glycogen (ug/uL/mg)”

``` r
#sample glycogen table
tab_glyc <- matrix(c(glyc_N0.A_mean_lum, glyc_N0.A_mean_conc, glyc_N0.A_dilution, glyc_N0.A_weight, glyc_N0.A_mean_conc*(glyc_N0.A_dilution/glyc_N0.A_weight), glyc_N0.B_mean_lum, glyc_N0.B_mean_conc, glyc_N0.B_dilution, glyc_N0.B_weight, glyc_N0.B_mean_conc*(glyc_N0.B_dilution/glyc_N0.B_weight), 
glyc_N0.C_mean_lum, glyc_N0.C_mean_conc, glyc_N0.C_dilution, glyc_N0.C_weight, glyc_N0.C_mean_conc*(glyc_N0.C_dilution/glyc_N0.C_weight), 

glyc_H0.A_mean_lum, glyc_H0.A_mean_conc, glyc_H0.A_dilution, glyc_H0.A_weight, glyc_H0.A_mean_conc*(glyc_H0.A_dilution/glyc_H0.A_weight), 
glyc_H0.B_mean_lum, glyc_H0.B_mean_conc, glyc_H0.B_dilution, glyc_H0.B_weight, glyc_H0.B_mean_conc*(glyc_H0.B_dilution/glyc_H0.B_weight), 
glyc_H0.C_mean_lum, glyc_H0.C_mean_conc, glyc_H0.C_dilution, glyc_H0.C_weight, glyc_H0.C_mean_conc*(glyc_H0.C_dilution/glyc_H0.C_weight), 

glyc_HH.A_mean_lum, glyc_HH.A_mean_conc, glyc_HH.A_dilution, glyc_HH.A_weight, glyc_HH.A_mean_conc*(glyc_HH.A_dilution/glyc_HH.A_weight), 
glyc_HH.B_mean_lum, glyc_HH.B_mean_conc, glyc_HH.B_dilution, glyc_HH.B_weight, glyc_HH.B_mean_conc*(glyc_HH.B_dilution/glyc_HH.B_weight), 
glyc_HH.E_mean_lum, glyc_HH.E_mean_conc, glyc_HH.E_dilution, glyc_HH.E_weight, glyc_HH.E_mean_conc*(glyc_HH.E_dilution/glyc_HH.E_weight), 
glyc_HH.F_mean_lum, glyc_HH.F_mean_conc, glyc_HH.F_dilution, glyc_HH.F_weight, glyc_HH.F_mean_conc*(glyc_HH.F_dilution/glyc_HH.F_weight), 

glyc_NH.A_mean_lum, glyc_NH.A_mean_conc, glyc_NH.A_dilution, glyc_NH.A_weight, glyc_NH.A_mean_conc*(glyc_NH.A_dilution/glyc_NH.A_weight), glyc_NH.C_mean_lum, glyc_NH.C_mean_conc, glyc_NH.C_dilution, glyc_NH.C_weight, glyc_NH.C_mean_conc*(glyc_NH.C_dilution/glyc_NH.C_weight), 
glyc_NH.D_mean_lum, glyc_NH.D_mean_conc, glyc_NH.D_dilution, glyc_NH.D_weight, glyc_NH.D_mean_conc*(glyc_NH.D_dilution/glyc_NH.D_weight), 
glyc_NH.F_mean_lum, glyc_NH.F_mean_conc, glyc_NH.F_dilution, glyc_NH.F_weight, glyc_NH.F_mean_conc*(glyc_NH.F_dilution/glyc_NH.F_weight), 

glyc_HN.A_mean_lum, glyc_HN.A_mean_conc, glyc_HN.A_dilution, glyc_HN.A_weight, glyc_HN.A_mean_conc*(glyc_HN.A_dilution/glyc_HN.A_weight), 
glyc_HN.B_mean_lum, glyc_HN.B_mean_conc, glyc_HN.B_dilution, glyc_HN.B_weight, glyc_HN.B_mean_conc*(glyc_HN.B_dilution/glyc_HN.B_weight), 
glyc_HN.C_mean_lum, glyc_HN.C_mean_conc, glyc_HN.C_dilution, glyc_HN.C_weight, glyc_HN.C_mean_conc*(glyc_HN.C_dilution/glyc_HN.C_weight), 
glyc_HN.D_mean_lum, glyc_HN.D_mean_conc, glyc_HN.D_dilution, glyc_HN.D_weight, glyc_HN.D_mean_conc*(glyc_HN.D_dilution/glyc_HN.D_weight), 

glyc_NN.A_mean_lum, glyc_NN.A_mean_conc, glyc_NN.A_dilution, glyc_NN.A_weight, glyc_NN.A_mean_conc*(glyc_NN.A_dilution/glyc_NN.A_weight), 
glyc_NN.B_mean_lum, glyc_NN.B_mean_conc, glyc_NN.B_dilution, glyc_NN.B_weight, glyc_NN.B_mean_conc*(glyc_NN.B_dilution/glyc_NN.B_weight), 
glyc_NN.D_mean_lum, glyc_NN.D_mean_conc, glyc_NN.D_dilution, glyc_NN.D_weight, glyc_NN.D_mean_conc*(glyc_NN.D_dilution/glyc_NN.D_weight), glyc_NN.E_mean_lum, glyc_NN.E_mean_conc, glyc_NN.E_dilution, glyc_NN.E_weight, glyc_NN.E_mean_conc*(glyc_NN.E_dilution/glyc_NN.E_weight), 
glyc_NN.F_mean_lum, glyc_NN.F_mean_conc, glyc_NN.F_dilution, glyc_NN.F_weight, glyc_NN.F_mean_conc*(glyc_NN.F_dilution/glyc_NN.F_weight)

), ncol=5, byrow=TRUE)
              
colnames(tab_glyc) <- c('Luminescence','Calculated Glycogen (ug/uL)','Dilution factor', 'Weight (mg)','Normalized glycogen (ug/uL/mg)')
rownames(tab_glyc) <- c('N0.A','N0.B','N0.C','H0.A','H0.B','H0.C','HH.A','HH.B','HH.E','HH.F','NH.A','NH.C','NH.D','NH.F','HN.A','HN.B','HN.C','HN.D','NN.A','NN.B','NN.D','NN.E','NN.F')
tab_glyc <- as.table(tab_glyc)
tab_glyc
```

         Luminescence Calculated Glycogen (ug/uL) Dilution factor  Weight (mg)
    N0.A 2537.6666667                   1.7026634      20.0000000   10.6000000
    N0.B 4356.0000000                   2.9639862      20.0000000   17.3000000
    N0.C 1271.0000000                   0.8240151      20.0000000    5.7000000
    H0.A 1075.0000000                   0.6880558      20.0000000    9.8000000
    H0.B  727.3333333                   0.4468899      20.0000000   16.0000000
    H0.C 1776.6666667                   1.1747807      20.0000000   22.9000000
    HH.A  834.3333333                   0.5211126      20.0000000    9.3000000
    HH.B  801.0000000                   0.4979903      20.0000000   14.0000000
    HH.E  575.3333333                   0.3414521      20.0000000    2.8000000
    HH.F  701.0000000                   0.4286233      20.0000000    8.7000000
    NH.A  803.3333333                   0.4996088      20.0000000   13.2000000
    NH.C  698.6666667                   0.4270047      20.0000000    8.2000000
    NH.D  987.3333333                   0.6272441      20.0000000   11.1000000
    NH.F  829.3333333                   0.5176443      20.0000000   10.8000000
    HN.A 3295.3333333                   2.2282339      20.0000000    6.3000000
    HN.B 2000.3333333                   1.3299315      20.0000000    8.1000000
    HN.C 2202.6666667                   1.4702840      20.0000000    9.3000000
    HN.D 3597.3333333                   2.4377221      20.0000000   12.8000000
    NN.A 2189.0000000                   1.4608039      20.0000000   10.3000000
    NN.B 2146.3333333                   1.4312073      20.0000000   11.1000000
    NN.D 3019.3333333                   2.0367810      20.0000000   13.9000000
    NN.E 8133.6666667                   5.5844393      20.0000000   20.3000000
    NN.F 8945.0000000                   6.1472367      20.0000000   15.6000000
         Normalized glycogen (ug/uL/mg)
    N0.A                      3.2125725
    N0.B                      3.4265737
    N0.C                      2.8912809
    H0.A                      1.4041955
    H0.B                      0.5586124
    H0.C                      1.0260094
    HH.A                      1.1206723
    HH.B                      0.7114147
    HH.E                      2.4389439
    HH.F                      0.9853409
    NH.A                      0.7569831
    NH.C                      1.0414750
    NH.D                      1.1301695
    NH.F                      0.9586005
    HN.A                      7.0737583
    HN.B                      3.2837816
    HN.C                      3.1619012
    HN.D                      3.8089408
    NN.A                      2.8365124
    NN.B                      2.5787519
    NN.D                      2.9306201
    NN.E                      5.5019106
    NN.F                      7.8810727
