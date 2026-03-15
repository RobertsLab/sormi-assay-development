Gen5-20260218-mgig-glycogenglo
================
Hazel Abrahamson-Amerine
2026-03-06

- [1 STANDARD CURVES](#1-standard-curves)
  - [1.1 Glycogen Standard Curve](#11-glycogen-standard-curve)
    - [1.1.1 Extract luminescence data](#111-extract-luminescence-data)

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
plate_layout <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-Gen5-20260218-mgig-glycogenglo.csv", header = FALSE)
raw_luminescence <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-Gen5-20260218-mgig-glycogenglo.csv", header = FALSE)

cat("Plate layout:\n")
str(plate_layout)

cat("\n\n")

cat("Raw luminescence:\n")
str(raw_luminescence)
```

    Plate layout:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : chr  "C7-glyc-35-df.50" "C7-glyc-35-df.100" "D2-glyc-22-df.20" "E4-glyc-31-df.20" ...
     $ V2 : chr  "C7-glyc-35-df.50" "C7-glyc-35-df.100" "D2-glyc-22-df.20" "E4-glyc-31-df.20" ...
     $ V3 : chr  "C7-glyc-35-df.50" "C7-glyc-35-df.100" "D2-glyc-22-df.20" "E4-glyc-31-df.20" ...
     $ V4 : chr  "E6-glyc-7-df.20" "E7-glyc-11-df.20" "" "" ...
     $ V5 : chr  "E6-glyc-7-df.20" "E7-glyc-11-df.20" "" "" ...
     $ V6 : chr  "E6-glyc-7-df.20" "E7-glyc-11-df.20" "" "" ...
     $ V7 : chr  "C7-glu-35-df.50" "C7-glu-35-df.100" "D2-glu-22-df.20" "E4-glu-31-df.20" ...
     $ V8 : chr  "C7-glu-35-df.50" "C7-glu-35-df.100" "D2-glu-22-df.20" "E4-glu-31-df.20" ...
     $ V9 : chr  "C7-glu-35-df.50" "C7-glu-35-df.100" "D2-glu-22-df.20" "E4-glu-31-df.20" ...
     $ V10: chr  "E6-glu-7-df.20" "E7-glu-11-df.20" "" "" ...
     $ V11: chr  "E6-glu-7-df.20" "E7-glu-11-df.20" "" "" ...
     $ V12: chr  "E6-glu-7-df.20" "E7-glu-11-df.20" "" "" ...


    Raw luminescence:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : int  54609 25458 21256 75245 141073 91960 147219 94797
     $ V2 : int  55376 25908 21330 76056 144639 7435 7867 7625
     $ V3 : int  56009 26116 21606 75404 153504 1462 1444 1419
     $ V4 : int  11060 18224 NA NA NA 875 2005 2131
     $ V5 : int  11234 18105 NA NA NA 771 898 2991
     $ V6 : int  11351 18093 NA NA NA NA NA NA
     $ V7 : int  1717 271 488 348 303 120928 120051 126975
     $ V8 : int  333 289 371 297 287 11646 13047 27545
     $ V9 : int  457 244 268 310 1484 1757 1291 1451
     $ V10: int  287 277 NA NA NA 583 314 6135
     $ V11: int  238 258 NA NA NA 5347 15390 8200
     $ V12: int  630 359 NA NA NA NA NA NA

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
#Extract glycogen sample data - wells D2-E6
glyc_sample_cols1 <- c(1,2,3)
glyc_C7_dilution50 <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 1]))
glyc_C7_luminescence50 <- as.numeric(raw_luminescence[1, glyc_sample_cols1])

glyc_C7_dilution100 <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 1]))
glyc_C7_luminescence100 <- as.numeric(raw_luminescence[2, glyc_sample_cols1])

glyc_D2_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 1]))
glyc_D2_luminescence <- as.numeric(raw_luminescence[3, glyc_sample_cols1])

glyc_E4_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 1]))
glyc_E4_luminescence <- as.numeric(raw_luminescence[4, glyc_sample_cols1])

glyc_E5_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 1]))
glyc_E5_luminescence <- as.numeric(raw_luminescence[5, glyc_sample_cols1])


glyc_sample_cols2 <- c(4,5,6)
glyc_E6_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 4]))
glyc_E6_luminescence <- as.numeric(raw_luminescence[1, glyc_sample_cols2])

glyc_E7_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 4]))
glyc_E7_luminescence <- as.numeric(raw_luminescence[2, glyc_sample_cols2])
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
glyc_C7_mean_lum50 <- numeric(length(glyc_C7_dilution50))
glyc_C7_se_lum50 <- numeric(length(glyc_C7_dilution50))
glyc_C7_mean_conc50 <- numeric(length(glyc_C7_dilution50))

for (i in 1:length(glyc_C7_dilution50)) {
  df_val <- glyc_C7_dilution50[i]
  # Get luminescence values for this dilution factor from both rows
   glyc_C7_lum_values50 <- c(glyc_C7_luminescence50[glyc_C7_dilution50 == df_val])
  glyc_C7_mean_lum50[i] <- mean( glyc_C7_lum_values50)
  glyc_C7_se_lum50[i] <- sd( glyc_C7_lum_values50) / sqrt(length( glyc_C7_lum_values50))
  # Calculate concentration from mean luminescence
  glyc_C7_mean_conc50[i] <- (glyc_C7_mean_lum50[i] - glyc_intercept) / glyc_slope
}

glyc_C7_data50 <- data.frame(
  glyc_C7_dilution50_factor = glyc_C7_dilution50,
  glyc_C7_mean_luminescence50 = glyc_C7_mean_lum50,
  glyc_C7_se50 = glyc_C7_se_lum50,
  glyc_C7_conc50 =  glyc_C7_mean_conc50,
  label = paste0("C7df.", glyc_C7_dilution50)
)
glyc_C7_mean_lum50
glyc_C7_mean_conc50
```

    [1] 55331.33
    [1] 9.963897

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_C7_mean_lum100 <- numeric(length(glyc_C7_dilution100))
glyc_C7_se_lum100 <- numeric(length(glyc_C7_dilution100))
glyc_C7_mean_conc100 <- numeric(length(glyc_C7_dilution100))

for (i in 1:length(glyc_C7_dilution100)) {
  df_val <- glyc_C7_dilution100[i]
  # Get luminescence values for this dilution factor from both rows
   glyc_C7_lum_values100 <- c(glyc_C7_luminescence100[glyc_C7_dilution100 == df_val])
  glyc_C7_mean_lum100[i] <- mean( glyc_C7_lum_values100)
  glyc_C7_se_lum100[i] <- sd( glyc_C7_lum_values100) / sqrt(length( glyc_C7_lum_values100))
  # Calculate concentration from mean luminescence
  glyc_C7_mean_conc100[i] <- (glyc_C7_mean_lum100[i] - glyc_intercept) / glyc_slope
}

glyc_C7_data100 <- data.frame(
  glyc_C7_dilution100_factor = glyc_C7_dilution100,
  glyc_C7_mean_luminescence100 = glyc_C7_mean_lum100,
  glyc_C7_se100 = glyc_C7_se_lum100,
  glyc_C7_conc100 =  glyc_C7_mean_conc100,
  label = paste0("C7df.", glyc_C7_dilution100)
)
glyc_C7_mean_lum100
glyc_C7_mean_conc100
```

    [1] 25827.33
    [1] 4.642529

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_D2_mean_lum <- numeric(length(glyc_D2_dilution))
glyc_D2_se_lum <- numeric(length(glyc_D2_dilution))
glyc_D2_mean_conc <- numeric(length(glyc_D2_dilution))

for (i in 1:length(glyc_D2_dilution)) {
  df_val <- glyc_D2_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_D2_lum_values <- c(glyc_D2_luminescence[glyc_D2_dilution == df_val])
  glyc_D2_mean_lum[i] <- mean(glyc_D2_lum_values)
  glyc_D2_se_lum[i] <- sd(glyc_D2_lum_values) / sqrt(length(glyc_D2_lum_values))
  # Calculate concentration from mean luminescence
  glyc_D2_mean_conc[i] <- (glyc_D2_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_D2_data <- data.frame(
  glyc_D2_dilution_factor = glyc_D2_dilution,
  glyc_D2_mean_luminescence = glyc_D2_mean_lum,
  glyc_D2_se = glyc_D2_se_lum,
  glyc_D2_conc =  glyc_D2_mean_conc,
  label = paste0("D2df.", glyc_D2_dilution)
)
glyc_D2_mean_lum
glyc_D2_mean_conc
```

    [1] 21397.33
    [1] 3.84353

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_E4_mean_lum <- numeric(length(glyc_E4_dilution))
glyc_E4_se_lum <- numeric(length(glyc_E4_dilution))
glyc_E4_mean_conc <- numeric(length(glyc_E4_dilution))

for (i in 1:length(glyc_E4_dilution)) {
  df_val <- glyc_E4_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_E4_lum_values <- c(glyc_E4_luminescence[glyc_E4_dilution == df_val])
  glyc_E4_mean_lum[i] <- mean(glyc_E4_lum_values)
  glyc_E4_se_lum[i] <- sd(glyc_E4_lum_values) / sqrt(length(glyc_E4_lum_values))
  # Calculate concentration from mean luminescence
  glyc_E4_mean_conc[i] <- (glyc_E4_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_E4_data <- data.frame(
  glyc_E4_dilution_factor = glyc_E4_dilution,
  glyc_E4_mean_luminescence = glyc_E4_mean_lum,
  glyc_E4_se = glyc_E4_se_lum,
  glyc_E4_conc =  glyc_E4_mean_conc,
  label = paste0("E4df.", glyc_E4_dilution)
)
glyc_E4_mean_lum
glyc_E4_mean_conc
```

    [1] 75568.33
    [1] 13.61386

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_E5_mean_lum <- numeric(length(glyc_E5_dilution))
glyc_E5_se_lum <- numeric(length(glyc_E5_dilution))
glyc_E5_mean_conc <- numeric(length(glyc_E5_dilution))

for (i in 1:length(glyc_E5_dilution)) {
  df_val <- glyc_E5_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_E5_lum_values <- c(glyc_E5_luminescence[glyc_E5_dilution == df_val])
  glyc_E5_mean_lum[i] <- mean(glyc_E5_lum_values)
  glyc_E5_se_lum[i] <- sd(glyc_E5_lum_values) / sqrt(length(glyc_E5_lum_values))
  # Calculate concentration from mean luminescence
  glyc_E5_mean_conc[i] <- (glyc_E5_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_E5_data <- data.frame(
  glyc_E5_dilution_factor = glyc_E5_dilution,
  glyc_E5_mean_luminescence = glyc_E5_mean_lum,
  glyc_E5_se = glyc_E5_se_lum,
  glyc_E5_conc =  glyc_E5_mean_conc,
  label = paste0("E5df.", glyc_E5_dilution)
)
glyc_E5_mean_lum
glyc_E5_mean_conc
```

    [1] 146405.3
    [1] 26.39008

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_E6_mean_lum <- numeric(length(glyc_E6_dilution))
glyc_E6_se_lum <- numeric(length(glyc_E6_dilution))
glyc_E6_mean_conc <- numeric(length(glyc_E6_dilution))

for (i in 1:length(glyc_E6_dilution)) {
  df_val <- glyc_E6_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_E6_lum_values <- c(glyc_E6_luminescence[glyc_E6_dilution == df_val])
  glyc_E6_mean_lum[i] <- mean(glyc_E6_lum_values)
  glyc_E6_se_lum[i] <- sd(glyc_E6_lum_values) / sqrt(length(glyc_E6_lum_values))
  # Calculate concentration from mean luminescence
  glyc_E6_mean_conc[i] <- (glyc_E6_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_E6_data <- data.frame(
  glyc_E6_dilution_factor = glyc_E6_dilution,
  glyc_E6_mean_luminescence = glyc_E6_mean_lum,
  glyc_E6_se = glyc_E6_se_lum,
  glyc_E6_conc =  glyc_E6_mean_conc,
  label = paste0("E6df.", glyc_E6_dilution)
)
glyc_E6_mean_lum
glyc_E6_mean_conc
```

    [1] 11215
    [1] 2.007036

``` r
# Create sample data frame
# Calculate glycogen concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glyc_E7_mean_lum <- numeric(length(glyc_E7_dilution))
glyc_E7_se_lum <- numeric(length(glyc_E7_dilution))
glyc_E7_mean_conc <- numeric(length(glyc_E7_dilution))

for (i in 1:length(glyc_E7_dilution)) {
  df_val <- glyc_E7_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glyc_E7_lum_values <- c(glyc_E7_luminescence[glyc_E7_dilution == df_val])
  glyc_E7_mean_lum[i] <- mean(glyc_E7_lum_values)
  glyc_E7_se_lum[i] <- sd(glyc_E7_lum_values) / sqrt(length(glyc_E7_lum_values))
  # Calculate concentration from mean luminescence
  glyc_E7_mean_conc[i] <- (glyc_E7_mean_lum[i] - glyc_intercept) / glyc_slope
}

glyc_E7_data <- data.frame(
  glyc_E7_dilution_factor = glyc_E7_dilution,
  glyc_E7_mean_luminescence = glyc_E7_mean_lum,
  glyc_E7_se = glyc_E7_se_lum,
  glyc_E7_conc =  glyc_E7_mean_conc,
  label = paste0("E7df.", glyc_E7_dilution)
)
glyc_E7_mean_lum
glyc_E7_mean_conc
```

    [1] 18140.67
    [1] 3.256155

``` r
# Create the plot
glyc_plot <- ggplot(glycogen_summary_data, aes(x = glyc_concentration, y = glyc_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glyc_mean_luminescence - glyc_se, ymax = glyc_mean_luminescence + glyc_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +

  geom_errorbar(data = glyc_C7_data50, aes(x = glyc_C7_conc50, y = glyc_C7_mean_luminescence50,
                ymin = glyc_C7_mean_luminescence50 - glyc_C7_se50, ymax = glyc_C7_mean_luminescence50 + glyc_C7_se50,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_C7_data50, aes(x = glyc_C7_conc50, y = glyc_C7_mean_luminescence50,
             color = label, shape = label), size = 4) +

  geom_errorbar(data = glyc_C7_data100, aes(x = glyc_C7_conc100, y = glyc_C7_mean_luminescence100,
                ymin = glyc_C7_mean_luminescence100 - glyc_C7_se100, ymax = glyc_C7_mean_luminescence100 + glyc_C7_se100,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_C7_data100, aes(x = glyc_C7_conc100, y = glyc_C7_mean_luminescence100,
             color = label, shape = label), size = 4) +
  
     geom_errorbar(data = glyc_D2_data, aes(x = glyc_D2_conc, y = glyc_D2_mean_luminescence,
                ymin = glyc_D2_mean_luminescence - glyc_D2_se, ymax = glyc_D2_mean_luminescence + glyc_D2_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_D2_data, aes(x = glyc_D2_conc, y = glyc_D2_mean_luminescence,
             color = label, shape = label), size = 4) +

   geom_errorbar(data = glyc_E4_data, aes(x = glyc_E4_conc, y = glyc_E4_mean_luminescence,
                ymin = glyc_E4_mean_luminescence - glyc_E4_se, ymax = glyc_E4_mean_luminescence + glyc_E4_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_E4_data, aes(x = glyc_E4_conc, y = glyc_E4_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_E5_data, aes(x = glyc_E5_conc, y = glyc_E5_mean_luminescence,
                ymin = glyc_E5_mean_luminescence - glyc_E5_se, ymax = glyc_E5_mean_luminescence + glyc_E5_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_E5_data, aes(x = glyc_E5_conc, y = glyc_E5_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_E6_data, aes(x = glyc_E6_conc, y = glyc_E6_mean_luminescence,
                ymin = glyc_E6_mean_luminescence - glyc_E6_se, ymax = glyc_E6_mean_luminescence + glyc_E6_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_E6_data, aes(x = glyc_E6_conc, y = glyc_E6_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glyc_E7_data, aes(x = glyc_E7_conc, y = glyc_E7_mean_luminescence,
                ymin = glyc_E7_mean_luminescence - glyc_E7_se, ymax = glyc_E7_mean_luminescence + glyc_E7_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glyc_E7_data, aes(x = glyc_E7_conc, y = glyc_E7_mean_luminescence,
             color = label, shape = label), size = 4) +

  
  
  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                                "C7df.50" = "darkred",
                                "C7df.100" = "darkorange",
                                "D2df.20" = "cyan",
                                "E4df.20" = "darkgreen",
                                "E5df.20" = "purple",
                                "E6df.20" = "brown",
                                "E7df.20" = "green3"
                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                 "C7df.50" = 17,
                                 "C7df.100" = 15,
                                 "D2df.20" = 2,
                                 "E4df.20" = 18,
                                 "E5df.20" = 8,
                                 "E6df.20" = 4,
                                 "E7df.20" = 5
                                )) +
 scale_linetype_manual(name = "",
                        values = c("Std Curve Best Fit Line" = "dashed")) +
  annotate("label", x = (max(glycogen_summary_data$glyc_concentration) * 1.05), 
           y = (max(glycogen_summary_data$glyc_mean_luminescence) * 0.15),
           label = sprintf("y = %.2fx + %.2f\nR² = %.4f", glyc_slope, glyc_intercept, glyc_r_squared),
           size = 3.5, fontface = "bold", fill = "white", 
           color = "coral", label.padding = unit(0.3, "lines")) +
  labs(
    title = "Glycogen Standard Curve",
    x = "Glycogen Concentration (µg/µL)",
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

![](Gen5-20260218-mgig-glycogenglo_files/figure-gfm/plot%20glycogen%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glyc_C7_data50
glyc_C7_data100
glyc_D2_data
glyc_E4_data
glyc_E5_data
glyc_E6_data
glyc_E7_data

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

      glyc_C7_dilution50_factor glyc_C7_mean_luminescence50 glyc_C7_se50
    1                        50                    55331.33     404.7618
      glyc_C7_conc50   label
    1       9.963897 C7df.50
      glyc_C7_dilution100_factor glyc_C7_mean_luminescence100 glyc_C7_se100
    1                        100                     25827.33      194.1832
      glyc_C7_conc100    label
    1        4.642529 C7df.100
      glyc_D2_dilution_factor glyc_D2_mean_luminescence glyc_D2_se glyc_D2_conc
    1                      20                  21397.33   106.4978      3.84353
        label
    1 D2df.20
      glyc_E4_dilution_factor glyc_E4_mean_luminescence glyc_E4_se glyc_E4_conc
    1                      20                  75568.33   248.1158     13.61386
        label
    1 E4df.20
      glyc_E5_dilution_factor glyc_E5_mean_luminescence glyc_E5_se glyc_E5_conc
    1                      20                  146405.3   3695.601     26.39008
        label
    1 E5df.20
      glyc_E6_dilution_factor glyc_E6_mean_luminescence glyc_E6_se glyc_E6_conc
    1                      20                     11215   84.53993     2.007036
        label
    1 E6df.20
      glyc_E7_dilution_factor glyc_E7_mean_luminescence glyc_E7_se glyc_E7_conc
    1                      20                  18140.67   41.81042     3.256155
        label
    1 E7df.20
    glycogen Standard Curve Summary:
    ==================================================
    Concentration: 20 µg/µL
      Mean Luminescence: 111325.33
      Standard Error: 17965.51
      CV%: 27.95%

    Concentration: 2 µg/µL
      Mean Luminescence: 7642.33
      Standard Error: 125.01
      CV%: 2.83%

    Concentration: 0.2 µg/µL
      Mean Luminescence: 1441.67
      Standard Error: 12.47
      CV%: 1.50%

    Concentration: 0.02 µg/µL
      Mean Luminescence: 1670.33
      Standard Error: 399.33
      CV%: 41.41%

    Concentration: 0 µg/µL
      Mean Luminescence: 1553.33
      Standard Error: 719.77
      CV%: 80.26%

``` r
tab <- matrix(c(glyc_C7_dilution50, glyc_C7_mean_lum50, glyc_C7_mean_conc50,  (glyc_C7_dilution50*glyc_C7_mean_conc50), 
                glyc_C7_dilution100, glyc_C7_mean_lum100, glyc_C7_mean_conc100,  (glyc_C7_dilution100*glyc_C7_mean_conc100), 
                glyc_D2_dilution, glyc_D2_mean_lum, glyc_D2_mean_conc,  (glyc_D2_dilution*glyc_D2_mean_conc), 
                glyc_E4_dilution, glyc_E4_mean_lum, glyc_E4_mean_conc,  (glyc_E4_dilution*glyc_E4_mean_conc), 
                glyc_E5_dilution, glyc_E5_mean_lum, glyc_E5_mean_conc,  (glyc_E5_dilution*glyc_E5_mean_conc), 
                glyc_E6_dilution, glyc_E6_mean_lum, glyc_E6_mean_conc,  (glyc_E6_dilution*glyc_E6_mean_conc), 
                glyc_E7_dilution, glyc_E7_mean_lum, glyc_E7_mean_conc,  (glyc_E7_dilution*glyc_E7_mean_conc)), ncol=4, byrow=TRUE)
colnames(tab) <- c('Dilution factor','Luminescence','Calculated Glycogen (ug/uL)', 'Total glycogen (ug/uL)')
rownames(tab) <- c('C7_df50','C7_df100','D2','E4','E5','E6','E7')
tab <- as.table(tab)
tab
```

             Dilution factor Luminescence Calculated Glycogen (ug/uL)
    C7_df50     5.000000e+01 5.533133e+04                9.963897e+00
    C7_df100    1.000000e+02 2.582733e+04                4.642529e+00
    D2          2.000000e+01 2.139733e+04                3.843530e+00
    E4          2.000000e+01 7.556833e+04                1.361386e+01
    E5          2.000000e+01 1.464053e+05                2.639008e+01
    E6          2.000000e+01 1.121500e+04                2.007036e+00
    E7          2.000000e+01 1.814067e+04                3.256155e+00
             Total glycogen (ug/uL)
    C7_df50            4.981948e+02
    C7_df100           4.642529e+02
    D2                 7.687061e+01
    E4                 2.722772e+02
    E5                 5.278017e+02
    E6                 4.014072e+01
    E7                 6.512310e+01

``` r
# Extract glucose standard curve data from plate layout and raw luminescence.
# Rows F, G, H (rows 6, 7, 8) correspond to glucose standards
# Columns 1-5 contain the standard curve concentrations

# Extract concentration values from plate layout (row 6, columns 1-5)
# Parse concentration from labels like "STD-glu-20" -> 20
glu_concentrations <- as.numeric(gsub("STD-glu-", "", plate_layout[6, 7:11]))
glu_concentrations

# Extract standard curve luminescence values for each replicate from raw_luminescence
glu_row_F <- as.numeric(raw_luminescence[6, 7:11])  # Row 6 (F)
glu_row_G <- as.numeric(raw_luminescence[7, 7:11])  # Row 7 (G)
glu_row_H <- as.numeric(raw_luminescence[8, 7:11])  # Row 8 (H)
```

    [1] 100.0  10.0   1.0   0.1   0.0

``` r
glu_sample_cols1 <- c(7,8,9)
glu_C7_dilution50 <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 1]))
glu_C7_luminescence50 <- as.numeric(raw_luminescence[1, glu_sample_cols1])

glu_C7_dilution100 <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 1]))
glu_C7_luminescence100 <- as.numeric(raw_luminescence[2, glu_sample_cols1])

glu_D2_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[3, 1]))
glu_D2_luminescence <- as.numeric(raw_luminescence[3, glu_sample_cols1])

glu_E4_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[4, 1]))
glu_E4_luminescence <- as.numeric(raw_luminescence[4, glu_sample_cols1])

glu_E5_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[5, 1]))
glu_E5_luminescence <- as.numeric(raw_luminescence[5, glu_sample_cols1])


glu_sample_cols2 <- c(10,11,12)
glu_E6_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[1, 4]))
glu_E6_luminescence <- as.numeric(raw_luminescence[1, glu_sample_cols2])

glu_E7_dilution <- as.numeric(gsub(".*-df\\.", "", plate_layout[2, 4]))
glu_E7_luminescence <- as.numeric(raw_luminescence[2, glu_sample_cols2])
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
glu_C7_mean_lum50 <- numeric(length(glu_C7_dilution50))
glu_C7_se_lum50 <- numeric(length(glu_C7_dilution50))
glu_C7_mean_conc50 <- numeric(length(glu_C7_dilution50))

for (i in 1:length(glu_C7_dilution50)) {
  df_val <- glu_C7_dilution50[i]
  # Get luminescence values for this dilution factor from both rows
   glu_C7_lum_values50 <- c(glu_C7_luminescence50[glu_C7_dilution50 == df_val])
  glu_C7_mean_lum50[i] <- mean( glu_C7_lum_values50)
  glu_C7_se_lum50[i] <- sd( glu_C7_lum_values50) / sqrt(length( glu_C7_lum_values50))
  # Calculate concentration from mean luminescence
  glu_C7_mean_conc50[i] <- (glu_C7_mean_lum50[i] - glu_intercept) / glu_slope
}

glu_C7_data50 <- data.frame(
  glu_C7_dilution50_factor = glu_C7_dilution50,
  glu_C7_mean_luminescence50 = glu_C7_mean_lum50,
  glu_C7_se50 = glu_C7_se_lum50,
  glu_C7_conc50 =  glu_C7_mean_conc50,
  label = paste0("C7df.", glu_C7_dilution50)
)
glu_C7_mean_lum50
glu_C7_mean_conc50
```

    [1] 835.6667
    [1] -3.033739

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_C7_mean_lum100 <- numeric(length(glu_C7_dilution100))
glu_C7_se_lum100 <- numeric(length(glu_C7_dilution100))
glu_C7_mean_conc100 <- numeric(length(glu_C7_dilution100))

for (i in 1:length(glu_C7_dilution100)) {
  df_val <- glu_C7_dilution100[i]
  # Get luminescence values for this dilution factor from both rows
   glu_C7_lum_values100 <- c(glu_C7_luminescence100[glu_C7_dilution100 == df_val])
  glu_C7_mean_lum100[i] <- mean( glu_C7_lum_values100)
  glu_C7_se_lum100[i] <- sd( glu_C7_lum_values100) / sqrt(length( glu_C7_lum_values100))
  # Calculate concentration from mean luminescence
  glu_C7_mean_conc100[i] <- (glu_C7_mean_lum100[i] - glu_intercept) / glu_slope
}

glu_C7_data100 <- data.frame(
  glu_C7_dilution100_factor = glu_C7_dilution100,
  glu_C7_mean_luminescence100 = glu_C7_mean_lum100,
  glu_C7_se100 = glu_C7_se_lum100,
  glu_C7_conc100 =  glu_C7_mean_conc100,
  label = paste0("C7df.", glu_C7_dilution100)
)
glu_C7_mean_lum100
glu_C7_mean_conc100
```

    [1] 268
    [1] -3.513595

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_D2_mean_lum <- numeric(length(glu_D2_dilution))
glu_D2_se_lum <- numeric(length(glu_D2_dilution))
glu_D2_mean_conc <- numeric(length(glu_D2_dilution))

for (i in 1:length(glu_D2_dilution)) {
  df_val <- glu_D2_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_D2_lum_values <- c(glu_D2_luminescence[glu_D2_dilution == df_val])
  glu_D2_mean_lum[i] <- mean(glu_D2_lum_values)
  glu_D2_se_lum[i] <- sd(glu_D2_lum_values) / sqrt(length(glu_D2_lum_values))
  # Calculate concentration from mean luminescence
  glu_D2_mean_conc[i] <- (glu_D2_mean_lum[i] - glu_intercept) / glu_slope
}

glu_D2_data <- data.frame(
  glu_D2_dilution_factor = glu_D2_dilution,
  glu_D2_mean_luminescence = glu_D2_mean_lum,
  glu_D2_se = glu_D2_se_lum,
  glu_D2_conc =  glu_D2_mean_conc,
  label = paste0("D2df.", glu_D2_dilution)
)
glu_D2_mean_lum
glu_D2_mean_conc
```

    [1] 375.6667
    [1] -3.422583

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_E4_mean_lum <- numeric(length(glu_E4_dilution))
glu_E4_se_lum <- numeric(length(glu_E4_dilution))
glu_E4_mean_conc <- numeric(length(glu_E4_dilution))

for (i in 1:length(glu_E4_dilution)) {
  df_val <- glu_E4_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_E4_lum_values <- c(glu_E4_luminescence[glu_E4_dilution == df_val])
  glu_E4_mean_lum[i] <- mean(glu_E4_lum_values)
  glu_E4_se_lum[i] <- sd(glu_E4_lum_values) / sqrt(length(glu_E4_lum_values))
  # Calculate concentration from mean luminescence
  glu_E4_mean_conc[i] <- (glu_E4_mean_lum[i] - glu_intercept) / glu_slope
}

glu_E4_data <- data.frame(
  glu_E4_dilution_factor = glu_E4_dilution,
  glu_E4_mean_luminescence = glu_E4_mean_lum,
  glu_E4_se = glu_E4_se_lum,
  glu_E4_conc =  glu_E4_mean_conc,
  label = paste0("E4df.", glu_E4_dilution)
)
glu_E4_mean_lum
glu_E4_mean_conc
```

    [1] 318.3333
    [1] -3.471048

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_E5_mean_lum <- numeric(length(glu_E5_dilution))
glu_E5_se_lum <- numeric(length(glu_E5_dilution))
glu_E5_mean_conc <- numeric(length(glu_E5_dilution))

for (i in 1:length(glu_E5_dilution)) {
  df_val <- glu_E5_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_E5_lum_values <- c(glu_E5_luminescence[glu_E5_dilution == df_val])
  glu_E5_mean_lum[i] <- mean(glu_E5_lum_values)
  glu_E5_se_lum[i] <- sd(glu_E5_lum_values) / sqrt(length(glu_E5_lum_values))
  # Calculate concentration from mean luminescence
  glu_E5_mean_conc[i] <- (glu_E5_mean_lum[i] - glu_intercept) / glu_slope
}

glu_E5_data <- data.frame(
  glu_E5_dilution_factor = glu_E5_dilution,
  glu_E5_mean_luminescence = glu_E5_mean_lum,
  glu_E5_se = glu_E5_se_lum,
  glu_E5_conc =  glu_E5_mean_conc,
  label = paste0("E5df.", glu_E5_dilution)
)
glu_E5_mean_lum
glu_E5_mean_conc
```

    [1] 691.3333
    [1] -3.155746

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_E6_mean_lum <- numeric(length(glu_E6_dilution))
glu_E6_se_lum <- numeric(length(glu_E6_dilution))
glu_E6_mean_conc <- numeric(length(glu_E6_dilution))

for (i in 1:length(glu_E6_dilution)) {
  df_val <- glu_E6_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_E6_lum_values <- c(glu_E6_luminescence[glu_E6_dilution == df_val])
  glu_E6_mean_lum[i] <- mean(glu_E6_lum_values)
  glu_E6_se_lum[i] <- sd(glu_E6_lum_values) / sqrt(length(glu_E6_lum_values))
  # Calculate concentration from mean luminescence
  glu_E6_mean_conc[i] <- (glu_E6_mean_lum[i] - glu_intercept) / glu_slope
}

glu_E6_data <- data.frame(
  glu_E6_dilution_factor = glu_E6_dilution,
  glu_E6_mean_luminescence = glu_E6_mean_lum,
  glu_E6_se = glu_E6_se_lum,
  glu_E6_conc =  glu_E6_mean_conc,
  label = paste0("E6df.", glu_E6_dilution)
)
glu_E6_mean_lum
glu_E6_mean_conc
```

    [1] 385
    [1] -3.414694

``` r
# Create sample data frame
# Calculate glucose concentration from luminescence using the standard curve equation
# Rearranging y = mx + b to solve for x: x = (y - b) / m

# Calculate mean and SE for each dilution factor across replicates
glu_E7_mean_lum <- numeric(length(glu_E7_dilution))
glu_E7_se_lum <- numeric(length(glu_E7_dilution))
glu_E7_mean_conc <- numeric(length(glu_E7_dilution))

for (i in 1:length(glu_E7_dilution)) {
  df_val <- glu_E7_dilution[i]
  # Get luminescence values for this dilution factor from both rows
  glu_E7_lum_values <- c(glu_E7_luminescence[glu_E7_dilution == df_val])
  glu_E7_mean_lum[i] <- mean(glu_E7_lum_values)
  glu_E7_se_lum[i] <- sd(glu_E7_lum_values) / sqrt(length(glu_E7_lum_values))
  # Calculate concentration from mean luminescence
  glu_E7_mean_conc[i] <- (glu_E7_mean_lum[i] - glu_intercept) / glu_slope
}

glu_E7_data <- data.frame(
  glu_E7_dilution_factor = glu_E7_dilution,
  glu_E7_mean_luminescence = glu_E7_mean_lum,
  glu_E7_se = glu_E7_se_lum,
  glu_E7_conc =  glu_E7_mean_conc,
  label = paste0("E7df.", glu_E7_dilution)
)
glu_E7_mean_lum
glu_E7_mean_conc
```

    [1] 298
    [1] -3.488236

``` r
# Create the plot
glu_plot <- ggplot(glucose_summary_data, aes(x = glu_concentration, y = glu_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glu_mean_luminescence - glu_se, ymax = glu_mean_luminescence + glu_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +

  geom_errorbar(data = glu_C7_data50, aes(x = glu_C7_conc50, y = glu_C7_mean_luminescence50,
                ymin = glu_C7_mean_luminescence50 - glu_C7_se50, ymax = glu_C7_mean_luminescence50 + glu_C7_se50,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_C7_data50, aes(x = glu_C7_conc50, y = glu_C7_mean_luminescence50,
             color = label, shape = label), size = 4) +

  geom_errorbar(data = glu_C7_data100, aes(x = glu_C7_conc100, y = glu_C7_mean_luminescence100,
                ymin = glu_C7_mean_luminescence100 - glu_C7_se100, ymax = glu_C7_mean_luminescence100 + glu_C7_se100,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_C7_data100, aes(x = glu_C7_conc100, y = glu_C7_mean_luminescence100,
             color = label, shape = label), size = 4) +
  
     geom_errorbar(data = glu_D2_data, aes(x = glu_D2_conc, y = glu_D2_mean_luminescence,
                ymin = glu_D2_mean_luminescence - glu_D2_se, ymax = glu_D2_mean_luminescence + glu_D2_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_D2_data, aes(x = glu_D2_conc, y = glu_D2_mean_luminescence,
             color = label, shape = label), size = 4) +

   geom_errorbar(data = glu_E4_data, aes(x = glu_E4_conc, y = glu_E4_mean_luminescence,
                ymin = glu_E4_mean_luminescence - glu_E4_se, ymax = glu_E4_mean_luminescence + glu_E4_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_E4_data, aes(x = glu_E4_conc, y = glu_E4_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_E5_data, aes(x = glu_E5_conc, y = glu_E5_mean_luminescence,
                ymin = glu_E5_mean_luminescence - glu_E5_se, ymax = glu_E5_mean_luminescence + glu_E5_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_E5_data, aes(x = glu_E5_conc, y = glu_E5_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_E6_data, aes(x = glu_E6_conc, y = glu_E6_mean_luminescence,
                ymin = glu_E6_mean_luminescence - glu_E6_se, ymax = glu_E6_mean_luminescence + glu_E6_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_E6_data, aes(x = glu_E6_conc, y = glu_E6_mean_luminescence,
             color = label, shape = label), size = 4) +
  
  geom_errorbar(data = glu_E7_data, aes(x = glu_E7_conc, y = glu_E7_mean_luminescence,
                ymin = glu_E7_mean_luminescence - glu_E7_se, ymax = glu_E7_mean_luminescence + glu_E7_se,
                color = label), width = 0.1, linewidth = 1) +
  geom_point(data = glu_E7_data, aes(x = glu_E7_conc, y = glu_E7_mean_luminescence,
             color = label, shape = label), size = 4) +

  
  
  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                                "C7df.50" = "darkred",
                                "C7df.100" = "darkorange",
                                "D2df.20" = "cyan",
                                "E4df.20" = "darkgreen",
                                "E5df.20" = "purple",
                                "E6df.20" = "brown",
                                "E7df.20" = "green3"
                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                 "C7df.50" = 17,
                                 "C7df.100" = 15,
                                 "D2df.20" = 2,
                                 "E4df.20" = 18,
                                 "E5df.20" = 8,
                                 "E6df.20" = 4,
                                 "E7df.20" = 5
                                )) +
 scale_linetype_manual(name = "",
                        values = c("Std Curve Best Fit Line" = "dashed")) +
  annotate("label", x = (max(glucose_summary_data$glu_concentration) * 0.75), 
           y = (max(glucose_summary_data$glu_mean_luminescence) * 0.15),
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

![](Gen5-20260218-mgig-glycogenglo_files/figure-gfm/plot%20glucose%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glu_C7_data50
glu_C7_data100
glu_D2_data
glu_E4_data
glu_E5_data
glu_E6_data
glu_E7_data

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

      glu_C7_dilution50_factor glu_C7_mean_luminescence50 glu_C7_se50 glu_C7_conc50
    1                       50                   835.6667    442.1181     -3.033739
        label
    1 C7df.50
      glu_C7_dilution100_factor glu_C7_mean_luminescence100 glu_C7_se100
    1                       100                         268      13.0767
      glu_C7_conc100    label
    1      -3.513595 C7df.100
      glu_D2_dilution_factor glu_D2_mean_luminescence glu_D2_se glu_D2_conc   label
    1                     20                 375.6667  63.55138   -3.422583 D2df.20
      glu_E4_dilution_factor glu_E4_mean_luminescence glu_E4_se glu_E4_conc   label
    1                     20                 318.3333  15.30069   -3.471048 E4df.20
      glu_E5_dilution_factor glu_E5_mean_luminescence glu_E5_se glu_E5_conc   label
    1                     20                 691.3333  396.3602   -3.155746 E5df.20
      glu_E6_dilution_factor glu_E6_mean_luminescence glu_E6_se glu_E6_conc   label
    1                     20                      385   123.314   -3.414694 E6df.20
      glu_E7_dilution_factor glu_E7_mean_luminescence glu_E7_se glu_E7_conc   label
    1                     20                      298  30.98925   -3.488236 E7df.20
    glucose Standard Curve Summary:
    ==================================================
    Concentration: 100 µg/µL
      Mean Luminescence: 122651.33
      Standard Error: 2176.61
      CV%: 3.07%

    Concentration: 10 µg/µL
      Mean Luminescence: 17412.67
      Standard Error: 5082.28
      CV%: 50.55%

    Concentration: 1 µg/µL
      Mean Luminescence: 1499.67
      Standard Error: 136.71
      CV%: 15.79%

    Concentration: 0.1 µg/µL
      Mean Luminescence: 2344.00
      Standard Error: 1897.09
      CV%: 140.18%

    Concentration: 0 µg/µL
      Mean Luminescence: 9645.67
      Standard Error: 2987.92
      CV%: 53.65%

``` r
tab <- matrix(c(glu_C7_dilution50, glu_C7_mean_lum50, glu_C7_mean_conc50,  (glu_C7_dilution50*glu_C7_mean_conc50), 
                glu_C7_dilution100, glu_C7_mean_lum100, glu_C7_mean_conc100,  (glu_C7_dilution100*glu_C7_mean_conc100), 
                glu_D2_dilution, glu_D2_mean_lum, glu_D2_mean_conc,  (glu_D2_dilution*glu_D2_mean_conc), 
                glu_E4_dilution, glu_E4_mean_lum, glu_E4_mean_conc,  (glu_E4_dilution*glu_E4_mean_conc), 
                glu_E5_dilution, glu_E5_mean_lum, glu_E5_mean_conc,  (glu_E5_dilution*glu_E5_mean_conc), 
                glu_E6_dilution, glu_E6_mean_lum, glu_E6_mean_conc,  (glu_E6_dilution*glu_E6_mean_conc), 
                glu_E7_dilution, glu_E7_mean_lum, glu_E7_mean_conc,  (glu_E7_dilution*glu_E7_mean_conc)), ncol=4, byrow=TRUE)
colnames(tab) <- c('Dilution factor','Luminescence','Calculated glucose (ug/uL)', 'Total glucose (ug/uL)')
rownames(tab) <- c('C7_df50','C7_df100','D2','E4','E5','E6','E7')
tab <- as.table(tab)
tab
```

             Dilution factor Luminescence Calculated glucose (ug/uL)
    C7_df50        50.000000   835.666667                  -3.033739
    C7_df100      100.000000   268.000000                  -3.513595
    D2             20.000000   375.666667                  -3.422583
    E4             20.000000   318.333333                  -3.471048
    E5             20.000000   691.333333                  -3.155746
    E6             20.000000   385.000000                  -3.414694
    E7             20.000000   298.000000                  -3.488236
             Total glucose (ug/uL)
    C7_df50            -151.686948
    C7_df100           -351.359523
    D2                  -68.451663
    E4                  -69.420956
    E5                  -63.114917
    E6                  -68.293871
    E7                  -69.764716
