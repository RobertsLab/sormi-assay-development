Gen5-20260629AB-mgig-glycogenglo
================
Hazel Abrahamson-Amerine
2026-07-07

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

\#Load plate reader, luminescence, and sample weight data

``` r
#load data
#plate 1 - samples 48.0A-48.2F
plate_layout1 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-Gen5-20260629A-mgig-glycogenglo.csv", header = FALSE)
raw_luminescence1 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-Gen5-20260629A-mgig-glycogenglo.csv", header = FALSE)

cat("Plate layout1:\n")
str(plate_layout1)

cat("\n\n")

cat("Raw luminescence1:\n")
str(raw_luminescence1)

#plate 2 - samples 18.0A-24.0C, 0.0A, 0.0B
plate_layout2 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-Gen5-20260629B-mgig-glycogenglo.csv", header = FALSE)
raw_luminescence2 <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-Gen5-20260629B-mgig-glycogenglo.csv", header = FALSE)

cat("Plate layout2:\n")
str(plate_layout2)

cat("\n\n")

cat("Raw luminescence2:\n")
str(raw_luminescence2)

#sample weights
sample_weights <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/20260625B_mgig_wholetissue_weights_GlycogenGlo.csv", header = FALSE)

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
     $ V1 : chr  "48.2G**-glyc-12.1-df.20" "48.2H**-glyc-10.7-df.20" "48.2I**-glyc-15.4-df.20" "48.2J**-glyc-7.1-df.20" ...
     $ V2 : chr  "48.2G**-glyc-12.1-df.20" "48.2H**-glyc-10.7-df.20" "48.2I**-glyc-15.4-df.20" "48.2J**-glyc-7.1-df.20" ...
     $ V3 : chr  "48.2G**-glyc-12.1-df.20" "48.2H**-glyc-10.7-df.20" "48.2I**-glyc-15.4-df.20" "48.2J**-glyc-7.1-df.20" ...
     $ V4 : chr  "48.7E**-glyc-15.5-df.20" "48.7F**-glyc-15.7-df.20" "48.7G**-glyc-12.8-df.20" "48.7H**-glyc-12.3-df.20" ...
     $ V5 : chr  "48.7E**-glyc-15.5-df.20" "48.7F**-glyc-15.7-df.20" "48.7G**-glyc-12.8-df.20" "48.7H**-glyc-12.3-df.20" ...
     $ V6 : chr  "48.7E**-glyc-15.5-df.20" "48.7F**-glyc-15.7-df.20" "48.7G**-glyc-12.8-df.20" "48.7H**-glyc-12.3-df.20" ...
     $ V7 : logi  NA NA NA NA NA NA ...
     $ V8 : logi  NA NA NA NA NA NA ...
     $ V9 : logi  NA NA NA NA NA NA ...
     $ V10: logi  NA NA NA NA NA NA ...
     $ V11: logi  NA NA NA NA NA NA ...
     $ V12: logi  NA NA NA NA NA NA ...


    Raw luminescence2:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : int  970 1133 742 1183 5143 585 5351 1194
     $ V2 : int  939 1184 784 1182 4660 667 4965 1515
     $ V3 : int  893 1159 745 1231 5150 573 4671 1529
     $ V4 : int  1970 1326 4667 2801 1590 2290 NA NA
     $ V5 : int  1928 1436 4725 2702 1472 2223 NA NA
     $ V6 : int  1971 1399 4864 2705 1848 2446 NA NA
     $ V7 : logi  NA NA NA NA NA NA ...
     $ V8 : logi  NA NA NA NA NA NA ...
     $ V9 : logi  NA NA NA NA NA NA ...
     $ V10: logi  NA NA NA NA NA NA ...
     $ V11: logi  NA NA NA NA NA NA ...
     $ V12: logi  NA NA NA NA NA NA ...
    Sample Weights:
    'data.frame':   41 obs. of  2 variables:
     $ V1: chr  "SampleID" "0.0A**" "0.0B**" "0.0C**" ...
     $ V2: chr  "Weight (mg)" "16.6" "10.4" "12.0" ...

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
#Extract glycogen sample data
#plate 1
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

glyc_sample_cols3 <- c(7,8,9)
glyc_48.0A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[1, 7]))
glyc_48.0A_luminescence <- as.numeric(raw_luminescence1[1, glyc_sample_cols3]) 

glyc_48.0B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[2, 7]))
glyc_48.0B_luminescence <- as.numeric(raw_luminescence1[2, glyc_sample_cols3]) 

glyc_48.0C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[3, 7]))
glyc_48.0C_luminescence <- as.numeric(raw_luminescence1[3, glyc_sample_cols3]) 

glyc_48.0D_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[4, 7]))
glyc_48.0D_luminescence <- as.numeric(raw_luminescence1[4, glyc_sample_cols3]) 

glyc_48.0E_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[5, 7]))
glyc_48.0E_luminescence <- as.numeric(raw_luminescence1[5, glyc_sample_cols3])

glyc_48.0F_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[6, 7]))
glyc_48.0F_luminescence <- as.numeric(raw_luminescence1[6, glyc_sample_cols3]) 

glyc_48.0G_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[7, 7]))
glyc_48.0G_luminescence <- as.numeric(raw_luminescence1[7, glyc_sample_cols3])

glyc_48.0H_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[8, 7]))
glyc_48.0H_luminescence <- as.numeric(raw_luminescence1[8, glyc_sample_cols3]) 

glyc_sample_cols4 <- c(10,11,12)
glyc_48.0I_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[1, 10]))
glyc_48.0I_luminescence <- as.numeric(raw_luminescence1[1, glyc_sample_cols4]) 

glyc_48.0J_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[2, 10]))
glyc_48.0J_luminescence <- as.numeric(raw_luminescence1[2, glyc_sample_cols4]) 

glyc_48.2A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[3, 10]))
glyc_48.2A_luminescence <- as.numeric(raw_luminescence1[3, glyc_sample_cols4])

glyc_48.2B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[4, 10]))
glyc_48.2B_luminescence <- as.numeric(raw_luminescence1[4, glyc_sample_cols4]) 

glyc_48.2C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[5, 10]))
glyc_48.2C_luminescence <- as.numeric(raw_luminescence1[5, glyc_sample_cols4]) 

glyc_48.2D_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[6, 10]))
glyc_48.2D_luminescence <- as.numeric(raw_luminescence1[6, glyc_sample_cols4]) 

glyc_48.2E_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[7, 10]))
glyc_48.2E_luminescence <- as.numeric(raw_luminescence1[7, glyc_sample_cols4]) 

glyc_48.2F_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout1[8, 10]))
glyc_48.2F_luminescence <- as.numeric(raw_luminescence1[8, glyc_sample_cols4]) 

#plate2
glyc_sample_cols1 <- c(1,2,3)
glyc_48.2G_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[1, 1]))
glyc_48.2G_luminescence <- as.numeric(raw_luminescence2[1, glyc_sample_cols1]) 

glyc_48.2H_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[2, 1]))
glyc_48.2H_luminescence <- as.numeric(raw_luminescence2[2, glyc_sample_cols1]) 

glyc_48.2I_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[3, 1]))
glyc_48.2I_luminescence <- as.numeric(raw_luminescence2[3, glyc_sample_cols1])

glyc_48.2J_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[4, 1]))
glyc_48.2J_luminescence <- as.numeric(raw_luminescence2[4, glyc_sample_cols1])

glyc_48.7A_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[5, 1]))
glyc_48.7A_luminescence <- as.numeric(raw_luminescence2[5, glyc_sample_cols1]) 

glyc_48.7B_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[6, 1]))
glyc_48.7B_luminescence <- as.numeric(raw_luminescence2[6, glyc_sample_cols1]) 

glyc_48.7C_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[7, 1]))
glyc_48.7C_luminescence <- as.numeric(raw_luminescence2[7, glyc_sample_cols1]) 

glyc_48.7D_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[8, 1]))
glyc_48.7D_luminescence <- as.numeric(raw_luminescence2[8, glyc_sample_cols1]) 

glyc_sample_cols2 <- c(4,5,6)
glyc_48.7E_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[1, 4]))
glyc_48.7E_luminescence <- as.numeric(raw_luminescence2[1, glyc_sample_cols2]) 

glyc_48.7F_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[2, 4]))
glyc_48.7F_luminescence <- as.numeric(raw_luminescence2[2, glyc_sample_cols2]) 

glyc_48.7G_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[3, 4]))
glyc_48.7G_luminescence <- as.numeric(raw_luminescence2[3, glyc_sample_cols2]) 

glyc_48.7H_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[4, 4]))
glyc_48.7H_luminescence <- as.numeric(raw_luminescence2[4, glyc_sample_cols2]) 

glyc_48.7I_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[5, 4]))
glyc_48.7I_luminescence <- as.numeric(raw_luminescence2[5, glyc_sample_cols2]) 

glyc_48.7J_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout2[6, 4]))
glyc_48.7J_luminescence <- as.numeric(raw_luminescence2[6, glyc_sample_cols2]) 
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
# Create sample data frame for 0.0A**
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

    [1] 1170.333
    [1] 0.7541856

``` r
# Create sample data frame for 0.0B**
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

    [1] 968.3333
    [1] 0.6140644

``` r
# Create sample data frame for 0.0C**
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

    [1] 962.3333
    [1] 0.6099023

``` r
# Create sample data frame for 0.0D**
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

    [1] 2511.667
    [1] 1.684628

``` r
# Create sample data frame for 0.0E**
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

    [1] 2034.667
    [1] 1.353748

``` r
# Create sample data frame for 0.0F**
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

    [1] 4843.667
    [1] 3.302266

``` r
# Create sample data frame for 0.0G**
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

    [1] 788.3333
    [1] 0.4892038

``` r
# Create sample data frame for 0.0H**
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

    [1] 2224
    [1] 1.485082

``` r
# Create sample data frame for 0.0I**
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

    [1] 1779
    [1] 1.176399

``` r
# Create sample data frame for 0.0J**
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

    [1] 1877.333
    [1] 1.24461

``` r
# Create sample data frame for for 48.0A**
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

    [1] 6935
    [1] 4.752961

``` r
# Create sample data frame for 48.0B**
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

    [1] 1208.333
    [1] 0.7805451

``` r
# Create sample data frame 48.0C**
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

    [1] 464.6667
    [1] 0.264686

``` r
# Create sample data frame 48.0D**
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

    [1] 416.6667
    [1] 0.2313899

``` r
# Create sample data frame 48.0E**
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

    [1] 804
    [1] 0.5000713

``` r
# Create sample data frame 48.0F**
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

    [1] 1382
    [1] 0.9010124

``` r
# Create sample data frame 48.0G**
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

    [1] 675.3333
    [1] 0.4108191

``` r
# Create sample data frame 48.0H**
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

    [1] 730.6667
    [1] 0.4492022

``` r
# Create sample data frame 48.0I**
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

    [1] 1063.667
    [1] 0.6801942

``` r
# Create sample data frame 48.0J**
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

    [1] 1494
    [1] 0.9787034

``` r
# Create sample data frame 48.2A**
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

    [1] 2675.333
    [1] 1.798159

``` r
# Create sample data frame for 48.2B**
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

    [1] 2630.333
    [1] 1.766943

``` r
# Create sample data frame for 48.2C**
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

    [1] 1147
    [1] 0.738

``` r
# Create sample data frame for 48.2D**
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

    [1] 1604.333
    [1] 1.055238

``` r
# Create sample data frame for 48.2E**
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

    [1] 1333.667
    [1] 0.867485

``` r
# Create sample data frame for 48.2F**
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

    [1] 2877
    [1] 1.938049

``` r
# Create sample data frame for 48.2G**
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

    [1] 934
    [1] 0.5902484

``` r
# Create sample data frame for 48.2H**
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

    [1] 1158.667
    [1] 0.7460928

``` r
# Create sample data frame for for 48.2I**
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

    [1] 757
    [1] 0.4674688

``` r
# Create sample data frame for 48.2J**
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

    [1] 1198.667
    [1] 0.7738396

``` r
# Create sample data frame for 48.7A**
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

    [1] 4984.333
    [1] 3.399842

``` r
# Create sample data frame for 48.7B**
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

    [1] 608.3333
    [1] 0.3643432

``` r
# Create sample data frame for 48.7C**
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

    [1] 4995.667
    [1] 3.407704

``` r
# Create sample data frame for 48.7D**
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

    [1] 1412.667
    [1] 0.9222849

``` r
# Create sample data frame for 48.7E**
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

    [1] 1956.333
    [1] 1.29941

``` r
# Create sample data frame for 48.7F**
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

    [1] 1387
    [1] 0.9044808

``` r
# Create sample data frame for 48.7G**
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

    [1] 4752
    [1] 3.238679

``` r
# Create sample data frame for 48.7H**
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

    [1] 2736
    [1] 1.840241

``` r
# Create sample data frame for 48.7I**
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

    [1] 1636.667
    [1] 1.077667

``` r
# Create sample data frame for 48.7J**
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

    [1] 2319.667
    [1] 1.551443

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

![](Gen5-20260629AB-mgig-glycogenglo_files/figure-gfm/plot%20glycogen%20standard%20curve%20with%20samples-1.png)<!-- -->

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
    1                        20                    1170.333     29.94625
      glyc_0.0A_conc     label
    1      0.7541856 0.0Adf.20
      glyc_0.0B_dilution_factor glyc_0.0B_mean_luminescence glyc_0.0B_se
    1                        20                    968.3333      15.0259
      glyc_0.0B_conc     label
    1      0.6140644 0.0Bdf.20
      glyc_0.0C_dilution_factor glyc_0.0C_mean_luminescence glyc_0.0C_se
    1                        20                    962.3333     20.51287
      glyc_0.0C_conc     label
    1      0.6099023 0.0Cdf.20
      glyc_0.0D_dilution_factor glyc_0.0D_mean_luminescence glyc_0.0D_se
    1                        20                    2511.667     70.59824
      glyc_0.0D_conc     label
    1       1.684628 0.0Ddf.20
      glyc_0.0E_dilution_factor glyc_0.0E_mean_luminescence glyc_0.0E_se
    1                        20                    2034.667     14.49521
      glyc_0.0E_conc     label
    1       1.353748 0.0Edf.20
      glyc_0.0F_dilution_factor glyc_0.0F_mean_luminescence glyc_0.0F_se
    1                        20                    4843.667     159.0245
      glyc_0.0F_conc     label
    1       3.302266 0.0Fdf.20
      glyc_0.0G_dilution_factor glyc_0.0G_mean_luminescence glyc_0.0G_se
    1                        20                    788.3333     29.61043
      glyc_0.0G_conc     label
    1      0.4892038 0.0Gdf.20
      glyc_0.0H_dilution_factor glyc_0.0H_mean_luminescence glyc_0.0H_se
    1                        20                        2224     33.42155
      glyc_0.0H_conc     label
    1       1.485082 0.0Hdf.20
      glyc_0.0I_dilution_factor glyc_0.0I_mean_luminescence glyc_0.0I_se
    1                        20                        1779     40.36087
      glyc_0.0I_conc     label
    1       1.176399 0.0Idf.20
      glyc_0.0J_dilution_factor glyc_0.0J_mean_luminescence glyc_0.0J_se
    1                        20                    1877.333     47.76447
      glyc_0.0J_conc     label
    1        1.24461 0.0Jdf.20
      glyc_48.0A_dilution_factor glyc_48.0A_mean_luminescence glyc_48.0A_se
    1                         20                         6935      262.8485
      glyc_48.0A_conc      label
    1        4.752961 48.0Adf.20
      glyc_48.0B_dilution_factor glyc_48.0B_mean_luminescence glyc_48.0B_se
    1                         20                     1208.333      50.85382
      glyc_48.0B_conc      label
    1       0.7805451 48.0Bdf.20
      glyc_48.0C_dilution_factor glyc_48.0C_mean_luminescence glyc_48.0C_se
    1                         20                     464.6667      11.34803
      glyc_48.0C_conc      label
    1        0.264686 48.0Cdf.20
      glyc_48.0D_dilution_factor glyc_48.0D_mean_luminescence glyc_48.0D_se
    1                         20                     416.6667      32.00174
      glyc_48.0D_conc      label
    1       0.2313899 48.0Ddf.20
      glyc_48.0E_dilution_factor glyc_48.0E_mean_luminescence glyc_48.0E_se
    1                         20                          804      16.92139
      glyc_48.0E_conc      label
    1       0.5000713 48.0Edf.20
      glyc_48.0F_dilution_factor glyc_48.0F_mean_luminescence glyc_48.0F_se
    1                         20                         1382      72.38094
      glyc_48.0F_conc      label
    1       0.9010124 48.0Fdf.20
      glyc_48.0G_dilution_factor glyc_48.0G_mean_luminescence glyc_48.0G_se
    1                         20                     675.3333      14.99259
      glyc_48.0G_conc      label
    1       0.4108191 48.0Gdf.20
      glyc_48.0H_dilution_factor glyc_48.0H_mean_luminescence glyc_48.0H_se
    1                         20                     730.6667      1.763834
      glyc_48.0H_conc      label
    1       0.4492022 48.0Hdf.20
      glyc_48.0I_dilution_factor glyc_48.0I_mean_luminescence glyc_48.0I_se
    1                         20                     1063.667      35.33333
      glyc_48.0I_conc      label
    1       0.6801942 48.0Idf.20
      glyc_48.0J_dilution_factor glyc_48.0J_mean_luminescence glyc_48.0J_se
    1                         20                         1494      20.10804
      glyc_48.0J_conc      label
    1       0.9787034 48.0Jdf.20
      glyc_48.2A_dilution_factor glyc_48.2A_mean_luminescence glyc_48.2A_se
    1                         20                     2675.333      58.95007
      glyc_48.2A_conc      label
    1        1.798159 48.2Adf.20
      glyc_48.2B_dilution_factor glyc_48.2B_mean_luminescence glyc_48.2B_se
    1                         20                     2630.333      52.98218
      glyc_48.2B_conc      label
    1        1.766943 48.2Bdf.20
      glyc_48.2C_dilution_factor glyc_48.2C_mean_luminescence glyc_48.2C_se
    1                         20                         1147      19.07878
      glyc_48.2C_conc      label
    1           0.738 48.2Cdf.20
      glyc_48.2D_dilution_factor glyc_48.2D_mean_luminescence glyc_48.2D_se
    1                         20                     1604.333      43.33333
      glyc_48.2D_conc      label
    1        1.055238 48.2Ddf.20
      glyc_48.2E_dilution_factor glyc_48.2E_mean_luminescence glyc_48.2E_se
    1                         20                     1333.667      42.11624
      glyc_48.2E_conc      label
    1        0.867485 48.2Edf.20
      glyc_48.2F_dilution_factor glyc_48.2F_mean_luminescence glyc_48.2F_se
    1                         20                         2877      114.1987
      glyc_48.2F_conc      label
    1        1.938049 48.2Fdf.20
      glyc_48.2G_dilution_factor glyc_48.2G_mean_luminescence glyc_48.2G_se
    1                         20                          934      22.36813
      glyc_48.2G_conc      label
    1       0.5902484 48.2Gdf.20
      glyc_48.2H_dilution_factor glyc_48.2H_mean_luminescence glyc_48.2H_se
    1                         20                     1158.667      14.72338
      glyc_48.2H_conc      label
    1       0.7460928 48.2Hdf.20
      glyc_48.2I_dilution_factor glyc_48.2I_mean_luminescence glyc_48.2I_se
    1                         20                          757      13.52775
      glyc_48.2I_conc      label
    1       0.4674688 48.2Idf.20
      glyc_48.2J_dilution_factor glyc_48.2J_mean_luminescence glyc_48.2J_se
    1                         20                     1198.667      16.16924
      glyc_48.2J_conc      label
    1       0.7738396 48.2Jdf.20
      glyc_48.7A_dilution_factor glyc_48.7A_mean_luminescence glyc_48.7A_se
    1                         20                     4984.333      162.1793
      glyc_48.7A_conc      label
    1        3.399842 48.7Adf.20
      glyc_48.7B_dilution_factor glyc_48.7B_mean_luminescence glyc_48.7B_se
    1                         20                     608.3333      29.53717
      glyc_48.7B_conc      label
    1       0.3643432 48.7Bdf.20
      glyc_48.7C_dilution_factor glyc_48.7C_mean_luminescence glyc_48.7C_se
    1                         20                     4995.667       196.897
      glyc_48.7C_conc      label
    1        3.407704 48.7Cdf.20
      glyc_48.7D_dilution_factor glyc_48.7D_mean_luminescence glyc_48.7D_se
    1                         20                     1412.667       109.408
      glyc_48.7D_conc      label
    1       0.9222849 48.7Ddf.20
      glyc_48.7E_dilution_factor glyc_48.7E_mean_luminescence glyc_48.7E_se
    1                         20                     1956.333      14.16961
      glyc_48.7E_conc      label
    1         1.29941 48.7Edf.20
      glyc_48.7F_dilution_factor glyc_48.7F_mean_luminescence glyc_48.7F_se
    1                         20                         1387      32.31615
      glyc_48.7F_conc      label
    1       0.9044808 48.7Fdf.20
      glyc_48.7G_dilution_factor glyc_48.7G_mean_luminescence glyc_48.7G_se
    1                         20                         4752      58.44941
      glyc_48.7G_conc      label
    1        3.238679 48.7Gdf.20
      glyc_48.7H_dilution_factor glyc_48.7H_mean_luminescence glyc_48.7H_se
    1                         20                         2736      32.51154
      glyc_48.7H_conc      label
    1        1.840241 48.7Hdf.20
      glyc_48.7I_dilution_factor glyc_48.7I_mean_luminescence glyc_48.7I_se
    1                         20                     1636.667      111.0215
      glyc_48.7I_conc      label
    1        1.077667 48.7Idf.20
      glyc_48.7J_dilution_factor glyc_48.7J_mean_luminescence glyc_48.7J_se
    1                         20                     2319.667      66.06142
      glyc_48.7J_conc      label
    1        1.551443 48.7Jdf.20
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

### 1.1.5 Sample glycogen table

``` r
#define tissue weights
glyc_0.0A_weight <- as.numeric(sample_weights[2,2])
glyc_0.0B_weight <- as.numeric(sample_weights[3,2])
glyc_0.0C_weight <- as.numeric(sample_weights[4,2])
glyc_0.0D_weight <- as.numeric(sample_weights[5,2])
glyc_0.0E_weight <- as.numeric(sample_weights[6,2])
glyc_0.0F_weight <- as.numeric(sample_weights[7,2])
glyc_0.0G_weight <- as.numeric(sample_weights[8,2])
glyc_0.0H_weight <- as.numeric(sample_weights[9,2])
glyc_0.0I_weight <- as.numeric(sample_weights[10,2])
glyc_0.0J_weight <- as.numeric(sample_weights[11,2])
glyc_48.0A_weight <- as.numeric(sample_weights[12,2])
glyc_48.0B_weight <- as.numeric(sample_weights[13,2])
glyc_48.0C_weight <- as.numeric(sample_weights[14,2])
glyc_48.0D_weight <- as.numeric(sample_weights[15,2])
glyc_48.0E_weight <- as.numeric(sample_weights[16,2])
glyc_48.0F_weight <- as.numeric(sample_weights[17,2])
glyc_48.0G_weight <- as.numeric(sample_weights[18,2])
glyc_48.0H_weight <- as.numeric(sample_weights[19,2])
glyc_48.0I_weight <- as.numeric(sample_weights[20,2])
glyc_48.0J_weight <- as.numeric(sample_weights[21,2])
glyc_48.2A_weight <- as.numeric(sample_weights[22,2])
glyc_48.2B_weight <- as.numeric(sample_weights[23,2])
glyc_48.2C_weight <- as.numeric(sample_weights[24,2])
glyc_48.2D_weight <- as.numeric(sample_weights[25,2])
glyc_48.2E_weight <- as.numeric(sample_weights[26,2])
glyc_48.2F_weight <- as.numeric(sample_weights[27,2])
#plate2
glyc_48.2G_weight <- as.numeric(sample_weights[28,2])
glyc_48.2H_weight <- as.numeric(sample_weights[29,2])
glyc_48.2I_weight <- as.numeric(sample_weights[30,2])
glyc_48.2J_weight <- as.numeric(sample_weights[31,2])
glyc_48.7A_weight <- as.numeric(sample_weights[32,2])
glyc_48.7B_weight <- as.numeric(sample_weights[33,2])
glyc_48.7C_weight <- as.numeric(sample_weights[34,2])
glyc_48.7D_weight <- as.numeric(sample_weights[35,2])
glyc_48.7E_weight <- as.numeric(sample_weights[36,2])
glyc_48.7F_weight <- as.numeric(sample_weights[37,2])
glyc_48.7G_weight <- as.numeric(sample_weights[38,2])
glyc_48.7H_weight <- as.numeric(sample_weights[39,2])
glyc_48.7I_weight <- as.numeric(sample_weights[40,2])
glyc_48.7J_weight <- as.numeric(sample_weights[41,2])
```

``` r
#normalize calculated glycogen to sample weight
#plate1
glyc_0.0A_mean_conc_normalized <- glyc_0.0A_mean_conc/glyc_0.0A_weight
glyc_0.0B_mean_conc_normalized <- glyc_0.0B_mean_conc/glyc_0.0B_weight
glyc_0.0C_mean_conc_normalized <- glyc_0.0C_mean_conc/glyc_0.0C_weight
glyc_0.0D_mean_conc_normalized <- glyc_0.0D_mean_conc/glyc_0.0D_weight
glyc_0.0E_mean_conc_normalized <- glyc_0.0E_mean_conc/glyc_0.0E_weight
glyc_0.0F_mean_conc_normalized <- glyc_0.0F_mean_conc/glyc_0.0F_weight
glyc_0.0G_mean_conc_normalized <- glyc_0.0G_mean_conc/glyc_0.0G_weight
glyc_0.0H_mean_conc_normalized <- glyc_0.0H_mean_conc/glyc_0.0H_weight
glyc_0.0I_mean_conc_normalized <- glyc_0.0I_mean_conc/glyc_0.0I_weight
glyc_0.0J_mean_conc_normalized <- glyc_0.0J_mean_conc/glyc_0.0J_weight
glyc_48.0A_mean_conc_normalized <- glyc_48.0A_mean_conc/glyc_48.0A_weight
glyc_48.0B_mean_conc_normalized <- glyc_48.0B_mean_conc/glyc_48.0B_weight
glyc_48.0C_mean_conc_normalized <- glyc_48.0C_mean_conc/glyc_48.0C_weight
glyc_48.0D_mean_conc_normalized <- glyc_48.0D_mean_conc/glyc_48.0D_weight
glyc_48.0E_mean_conc_normalized <- glyc_48.0E_mean_conc/glyc_48.0E_weight
glyc_48.0F_mean_conc_normalized <- glyc_48.0F_mean_conc/glyc_48.0F_weight
glyc_48.0G_mean_conc_normalized <- glyc_48.0G_mean_conc/glyc_48.0G_weight
glyc_48.0H_mean_conc_normalized <- glyc_48.0H_mean_conc/glyc_48.0H_weight
glyc_48.0I_mean_conc_normalized <- glyc_48.0I_mean_conc/glyc_48.0I_weight
glyc_48.0J_mean_conc_normalized <- glyc_48.0J_mean_conc/glyc_48.0J_weight
glyc_48.2A_mean_conc_normalized <- glyc_48.2A_mean_conc/glyc_48.2A_weight
glyc_48.2B_mean_conc_normalized <- glyc_48.2B_mean_conc/glyc_48.2B_weight
glyc_48.2C_mean_conc_normalized <- glyc_48.2C_mean_conc/glyc_48.2C_weight
glyc_48.2D_mean_conc_normalized <- glyc_48.2D_mean_conc/glyc_48.2D_weight
glyc_48.2E_mean_conc_normalized <- glyc_48.2E_mean_conc/glyc_48.2E_weight
glyc_48.2F_mean_conc_normalized <- glyc_48.2F_mean_conc/glyc_48.2F_weight
#plate2
glyc_48.2G_mean_conc_normalized <- glyc_48.2G_mean_conc/glyc_48.2G_weight
glyc_48.2H_mean_conc_normalized <- glyc_48.2H_mean_conc/glyc_48.2H_weight
glyc_48.2I_mean_conc_normalized <- glyc_48.2I_mean_conc/glyc_48.2I_weight
glyc_48.2J_mean_conc_normalized <- glyc_48.2J_mean_conc/glyc_48.2J_weight
glyc_48.7A_mean_conc_normalized <- glyc_48.7A_mean_conc/glyc_48.7A_weight
glyc_48.7B_mean_conc_normalized <- glyc_48.7B_mean_conc/glyc_48.7B_weight
glyc_48.7C_mean_conc_normalized <- glyc_48.7C_mean_conc/glyc_48.7C_weight
glyc_48.7D_mean_conc_normalized <- glyc_48.7D_mean_conc/glyc_48.7D_weight
glyc_48.7E_mean_conc_normalized <- glyc_48.7E_mean_conc/glyc_48.7E_weight
glyc_48.7F_mean_conc_normalized <- glyc_48.7F_mean_conc/glyc_48.7F_weight
glyc_48.7G_mean_conc_normalized <- glyc_48.7G_mean_conc/glyc_48.7G_weight
glyc_48.7H_mean_conc_normalized <- glyc_48.7H_mean_conc/glyc_48.7H_weight
glyc_48.7I_mean_conc_normalized <- glyc_48.7I_mean_conc/glyc_48.7I_weight
glyc_48.7J_mean_conc_normalized <- glyc_48.7J_mean_conc/glyc_48.7J_weight
```

Note: To normalize glycogen levels by dilution factor and tissue weight,
the calculated glycogen is multiplied by (dilution factor/weight). This
is reported as “Normalized glycogen (ug/uL/mg)”

``` r
#sample glycogen table
tab_glyc <- matrix(c(glyc_0.0A_mean_lum, glyc_0.0A_mean_conc, glyc_0.0A_dilution, glyc_0.0A_weight, glyc_0.0A_mean_conc*(glyc_0.0A_dilution/glyc_0.0A_weight), glyc_0.0B_mean_lum, glyc_0.0B_mean_conc, glyc_0.0B_dilution, glyc_0.0B_weight, glyc_0.0B_mean_conc*(glyc_0.0B_dilution/glyc_0.0B_weight), 
glyc_0.0C_mean_lum, glyc_0.0C_mean_conc, glyc_0.0C_dilution, glyc_0.0C_weight, glyc_0.0C_mean_conc*(glyc_0.0C_dilution/glyc_0.0C_weight), 
glyc_0.0D_mean_lum, glyc_0.0D_mean_conc, glyc_0.0D_dilution, glyc_0.0D_weight, glyc_0.0D_mean_conc*(glyc_0.0D_dilution/glyc_0.0D_weight), 
glyc_0.0E_mean_lum, glyc_0.0E_mean_conc, glyc_0.0E_dilution, glyc_0.0E_weight, glyc_0.0E_mean_conc*(glyc_0.0E_dilution/glyc_0.0E_weight), 
glyc_0.0F_mean_lum, glyc_0.0F_mean_conc, glyc_0.0F_dilution, glyc_0.0F_weight, glyc_0.0F_mean_conc*(glyc_0.0F_dilution/glyc_0.0F_weight), 
glyc_0.0G_mean_lum, glyc_0.0G_mean_conc, glyc_0.0G_dilution, glyc_0.0G_weight, glyc_0.0G_mean_conc*(glyc_0.0G_dilution/glyc_0.0G_weight), 
glyc_0.0H_mean_lum, glyc_0.0H_mean_conc, glyc_0.0H_dilution, glyc_0.0H_weight, glyc_0.0H_mean_conc*(glyc_0.0H_dilution/glyc_0.0H_weight), 
glyc_0.0I_mean_lum, glyc_0.0I_mean_conc, glyc_0.0I_dilution, glyc_0.0I_weight, glyc_0.0I_mean_conc*(glyc_0.0I_dilution/glyc_0.0I_weight), 
glyc_0.0J_mean_lum, glyc_0.0J_mean_conc, glyc_0.0J_dilution, glyc_0.0J_weight, glyc_0.0J_mean_conc*(glyc_0.0J_dilution/glyc_0.0J_weight), 

glyc_48.0A_mean_lum, glyc_48.0A_mean_conc, glyc_48.0A_dilution, glyc_48.0A_weight, glyc_48.0A_mean_conc*(glyc_48.0A_dilution/glyc_48.0A_weight), glyc_48.0B_mean_lum, glyc_48.0B_mean_conc, glyc_48.0B_dilution, glyc_48.0B_weight, glyc_48.0B_mean_conc*(glyc_48.0B_dilution/glyc_48.0B_weight), 
glyc_48.0C_mean_lum, glyc_48.0C_mean_conc, glyc_48.0C_dilution, glyc_48.0C_weight, glyc_48.0C_mean_conc*(glyc_48.0C_dilution/glyc_48.0C_weight), 
glyc_48.0D_mean_lum, glyc_48.0D_mean_conc, glyc_48.0D_dilution, glyc_48.0D_weight, glyc_48.0D_mean_conc*(glyc_48.0D_dilution/glyc_48.0D_weight), 
glyc_48.0E_mean_lum, glyc_48.0E_mean_conc, glyc_48.0E_dilution, glyc_48.0E_weight, glyc_48.0E_mean_conc*(glyc_48.0E_dilution/glyc_48.0E_weight), 
glyc_48.0F_mean_lum, glyc_48.0F_mean_conc, glyc_48.0F_dilution, glyc_48.0F_weight, glyc_48.0F_mean_conc*(glyc_48.0F_dilution/glyc_48.0F_weight), 
glyc_48.0G_mean_lum, glyc_48.0G_mean_conc, glyc_48.0G_dilution, glyc_48.0G_weight, glyc_48.0G_mean_conc*(glyc_48.0G_dilution/glyc_48.0G_weight), 
glyc_48.0H_mean_lum, glyc_48.0H_mean_conc, glyc_48.0H_dilution, glyc_48.0H_weight, glyc_48.0H_mean_conc*(glyc_48.0H_dilution/glyc_48.0H_weight), 
glyc_48.0I_mean_lum, glyc_48.0I_mean_conc, glyc_48.0I_dilution, glyc_48.0I_weight, glyc_48.0I_mean_conc*(glyc_48.0I_dilution/glyc_48.0I_weight), 
glyc_48.0J_mean_lum, glyc_48.0J_mean_conc, glyc_48.0J_dilution, glyc_48.0J_weight, glyc_48.0J_mean_conc*(glyc_48.0J_dilution/glyc_48.0J_weight), 

glyc_48.2A_mean_lum, glyc_48.2A_mean_conc, glyc_48.2A_dilution, glyc_48.2A_weight, glyc_48.2A_mean_conc*(glyc_48.2A_dilution/glyc_48.2A_weight), glyc_48.2B_mean_lum, glyc_48.2B_mean_conc, glyc_48.2B_dilution, glyc_48.2B_weight, glyc_48.2B_mean_conc*(glyc_48.2B_dilution/glyc_48.2B_weight), 
glyc_48.2C_mean_lum, glyc_48.2C_mean_conc, glyc_48.2C_dilution, glyc_48.2C_weight, glyc_48.2C_mean_conc*(glyc_48.2C_dilution/glyc_48.2C_weight), 
glyc_48.2D_mean_lum, glyc_48.2D_mean_conc, glyc_48.2D_dilution, glyc_48.2D_weight, glyc_48.2D_mean_conc*(glyc_48.2D_dilution/glyc_48.2D_weight), 
glyc_48.2E_mean_lum, glyc_48.2E_mean_conc, glyc_48.2E_dilution, glyc_48.2E_weight, glyc_48.2E_mean_conc*(glyc_48.2E_dilution/glyc_48.2E_weight), 
glyc_48.2F_mean_lum, glyc_48.2F_mean_conc, glyc_48.2F_dilution, glyc_48.2F_weight, glyc_48.2F_mean_conc*(glyc_48.2F_dilution/glyc_48.2F_weight), 
glyc_48.2G_mean_lum, glyc_48.2G_mean_conc, glyc_48.2G_dilution, glyc_48.2G_weight, glyc_48.2G_mean_conc*(glyc_48.2G_dilution/glyc_48.2G_weight), 
glyc_48.2H_mean_lum, glyc_48.2H_mean_conc, glyc_48.2H_dilution, glyc_48.2H_weight, glyc_48.2H_mean_conc*(glyc_48.2H_dilution/glyc_48.2H_weight), 
glyc_48.2I_mean_lum, glyc_48.2I_mean_conc, glyc_48.2I_dilution, glyc_48.2I_weight, glyc_48.2I_mean_conc*(glyc_48.2I_dilution/glyc_48.2I_weight), 
glyc_48.2J_mean_lum, glyc_48.2J_mean_conc, glyc_48.2J_dilution, glyc_48.2J_weight, glyc_48.2J_mean_conc*(glyc_48.2J_dilution/glyc_48.2J_weight), 

glyc_48.7A_mean_lum, glyc_48.7A_mean_conc, glyc_48.7A_dilution, glyc_48.7A_weight, glyc_48.7A_mean_conc*(glyc_48.7A_dilution/glyc_48.7A_weight), glyc_48.7B_mean_lum, glyc_48.7B_mean_conc, glyc_48.7B_dilution, glyc_48.7B_weight, glyc_48.7B_mean_conc*(glyc_48.7B_dilution/glyc_48.7B_weight), 
glyc_48.7C_mean_lum, glyc_48.7C_mean_conc, glyc_48.7C_dilution, glyc_48.7C_weight, glyc_48.7C_mean_conc*(glyc_48.7C_dilution/glyc_48.7C_weight), 
glyc_48.7D_mean_lum, glyc_48.7D_mean_conc, glyc_48.7D_dilution, glyc_48.7D_weight, glyc_48.7D_mean_conc*(glyc_48.7D_dilution/glyc_48.7D_weight), 
glyc_48.7E_mean_lum, glyc_48.7E_mean_conc, glyc_48.7E_dilution, glyc_48.7E_weight, glyc_48.7E_mean_conc*(glyc_48.7E_dilution/glyc_48.7E_weight), 
glyc_48.7F_mean_lum, glyc_48.7F_mean_conc, glyc_48.7F_dilution, glyc_48.7F_weight, glyc_48.7F_mean_conc*(glyc_48.7F_dilution/glyc_48.7F_weight), 
glyc_48.7G_mean_lum, glyc_48.7G_mean_conc, glyc_48.7G_dilution, glyc_48.7G_weight, glyc_48.7G_mean_conc*(glyc_48.7G_dilution/glyc_48.7G_weight), 
glyc_48.7H_mean_lum, glyc_48.7H_mean_conc, glyc_48.7H_dilution, glyc_48.7H_weight, glyc_48.7H_mean_conc*(glyc_48.7H_dilution/glyc_48.7H_weight), 
glyc_48.7I_mean_lum, glyc_48.7I_mean_conc, glyc_48.7I_dilution, glyc_48.7I_weight, glyc_48.7I_mean_conc*(glyc_48.7I_dilution/glyc_48.7I_weight), 
glyc_48.7J_mean_lum, glyc_48.7J_mean_conc, glyc_48.7J_dilution, glyc_48.7J_weight, glyc_48.7J_mean_conc*(glyc_48.7J_dilution/glyc_48.7J_weight)
), ncol=5, byrow=TRUE)
              
colnames(tab_glyc) <- c('Luminescence','Calculated Glycogen (ug/uL)','Dilution factor', 'Weight (mg)','Normalized glycogen (ug/uL/mg)')
rownames(tab_glyc) <- c('0.0A**','0.0B**','0.0C**','0.0D**','0.0E**','0.0F**','0.0G**','0.0H**','0.0I**','0.0J**','48.0A**','48.0B**','48.0C**','48.0D**','48.0E**','48.0F**','48.0G**','48.0H**','48.0I**','48.0J**','48.2A**','48.2B**','48.2C**','48.2D**','48.2E**','48.2F**','48.2G**','48.2H**','48.2I**','48.2J**','48.7A**','48.7B**','48.7C**','48.7D**','48.7E**','48.7F**','48.7G**','48.7H**','48.7I**','48.7J**')
tab_glyc <- as.table(tab_glyc)
tab_glyc
```

            Luminescence Calculated Glycogen (ug/uL) Dilution factor  Weight (mg)
    0.0A**  1170.3333333                   0.7541856      20.0000000   16.6000000
    0.0B**   968.3333333                   0.6140644      20.0000000   10.4000000
    0.0C**   962.3333333                   0.6099023      20.0000000   12.0000000
    0.0D**  2511.6666667                   1.6846280      20.0000000   17.3000000
    0.0E**  2034.6666667                   1.3537475      20.0000000   19.1000000
    0.0F**  4843.6666667                   3.3022658      20.0000000   17.3000000
    0.0G**   788.3333333                   0.4892038      20.0000000   14.4000000
    0.0H**  2224.0000000                   1.4850823      20.0000000   15.5000000
    0.0I**  1779.0000000                   1.1763993      20.0000000   13.3000000
    0.0J**  1877.3333333                   1.2446102      20.0000000   17.2000000
    48.0A** 6935.0000000                   4.7529605      20.0000000   21.8000000
    48.0B** 1208.3333333                   0.7805451      20.0000000   26.0000000
    48.0C**  464.6666667                   0.2646860      20.0000000    8.8000000
    48.0D**  416.6666667                   0.2313899      20.0000000   11.9000000
    48.0E**  804.0000000                   0.5000713      20.0000000   19.8000000
    48.0F** 1382.0000000                   0.9010124      20.0000000   25.5000000
    48.0G**  675.3333333                   0.4108191      20.0000000   10.8000000
    48.0H**  730.6666667                   0.4492022      20.0000000   17.8000000
    48.0I** 1063.6666667                   0.6801942      20.0000000   24.5000000
    48.0J** 1494.0000000                   0.9787034      20.0000000   14.9000000
    48.2A** 2675.3333333                   1.7981586      20.0000000   20.8000000
    48.2B** 2630.3333333                   1.7669435      20.0000000   18.5000000
    48.2C** 1147.0000000                   0.7380000      20.0000000   22.5000000
    48.2D** 1604.3333333                   1.0552383      20.0000000   10.6000000
    48.2E** 1333.6666667                   0.8674850      20.0000000   20.0000000
    48.2F** 2877.0000000                   1.9380487      20.0000000   18.7000000
    48.2G**  934.0000000                   0.5902484      20.0000000   12.1000000
    48.2H** 1158.6666667                   0.7460928      20.0000000   10.7000000
    48.2I**  757.0000000                   0.4674688      20.0000000   15.4000000
    48.2J** 1198.6666667                   0.7738396      20.0000000    7.1000000
    48.7A** 4984.3333333                   3.3998421      20.0000000   19.2000000
    48.7B**  608.3333333                   0.3643432      20.0000000   10.0000000
    48.7C** 4995.6666667                   3.4077036      20.0000000   22.1000000
    48.7D** 1412.6666667                   0.9222849      20.0000000   11.9000000
    48.7E** 1956.3333333                   1.2994101      20.0000000   15.5000000
    48.7F** 1387.0000000                   0.9044808      20.0000000   15.7000000
    48.7G** 4752.0000000                   3.2386794      20.0000000   12.8000000
    48.7H** 2736.0000000                   1.8402412      20.0000000   12.3000000
    48.7I** 1636.6666667                   1.0776670      20.0000000    5.5000000
    48.7J** 2319.6666667                   1.5514434      20.0000000   13.8000000
            Normalized glycogen (ug/uL/mg)
    0.0A**                       0.9086574
    0.0B**                       1.1808930
    0.0C**                       1.0165039
    0.0D**                       1.9475468
    0.0E**                       1.4175367
    0.0F**                       3.8176484
    0.0G**                       0.6794497
    0.0H**                       1.9162353
    0.0I**                       1.7690215
    0.0J**                       1.4472211
    48.0A**                      4.3605142
    48.0B**                      0.6004193
    48.0C**                      0.6015592
    48.0D**                      0.3888906
    48.0E**                      0.5051225
    48.0F**                      0.7066764
    48.0G**                      0.7607761
    48.0H**                      0.5047215
    48.0I**                      0.5552606
    48.0J**                      1.3136959
    48.2A**                      1.7289987
    48.2B**                      1.9102092
    48.2C**                      0.6560000
    48.2D**                      1.9910157
    48.2E**                      0.8674850
    48.2F**                      2.0727793
    48.2G**                      0.9756171
    48.2H**                      1.3945660
    48.2I**                      0.6071024
    48.2J**                      2.1798299
    48.7A**                      3.5415021
    48.7B**                      0.7286865
    48.7C**                      3.0838947
    48.7D**                      1.5500587
    48.7E**                      1.6766581
    48.7F**                      1.1522048
    48.7G**                      5.0604366
    48.7H**                      2.9922622
    48.7I**                      3.9187890
    48.7J**                      2.2484687
