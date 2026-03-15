Gen5-20260116-mgig-glycogenglo
================
Hazel Abrahamson-Amerine
2026-03-14

- [1 STANDARD CURVES](#1-standard-curves)
  - [1.1 Glycogen Standard Curve](#11-glycogen-standard-curve)
    - [1.1.1 Extract luminescence data](#111-extract-luminescence-data)
- [2 STANDARD CURVES](#2-standard-curves)
  - [2.1 Glucose Standard Curve](#21-glucose-standard-curve)
    - [2.1.1 Extract luminescence data](#211-extract-luminescence-data)

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
plate_layout <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/layout-20260116-mgig-gylcogen_glo.csv", header = FALSE)
raw_luminescence <- read.csv("https://raw.githubusercontent.com/RobertsLab/sormi-assay-development/refs/heads/main/Glycogen/data/raw_luminescence/raw_lum-20260116-mgig-gylcogen_glo.csv", header = FALSE)

cat("Plate layout:\n")
str(plate_layout)

cat("\n\n")

cat("Raw luminescence:\n")
str(raw_luminescence)
```

    Plate layout:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : chr  "A3-gly" "A4-gly" "A3-glu" "A4-glu" ...
     $ V2 : chr  "A3-gly" "A4-gly" "A3-glu" "A4-glu" ...
     $ V3 : chr  "A3-gly" "A4-gly" "A3-glu" "A4-glu" ...
     $ V4 : chr  "" "" "" "" ...
     $ V5 : chr  "" "" "" "" ...
     $ V6 : logi  NA NA NA NA NA NA ...
     $ V7 : chr  "" "" "" "" ...
     $ V8 : chr  "" "" "" "" ...
     $ V9 : chr  "" "" "" "" ...
     $ V10: chr  "" "" "" "" ...
     $ V11: chr  "" "" "" "" ...
     $ V12: logi  NA NA NA NA NA NA ...


    Raw luminescence:
    'data.frame':   8 obs. of  12 variables:
     $ V1 : int  148538 156919 1253 513 NA 143671 147053 155087
     $ V2 : int  162161 171436 1185 468 NA 10701 9993 11134
     $ V3 : int  164823 159759 1442 508 NA 1775 1794 1840
     $ V4 : int  NA NA NA NA NA 1013 1081 1028
     $ V5 : int  NA NA NA NA NA 965 1043 1815
     $ V6 : logi  NA NA NA NA NA NA ...
     $ V7 : int  NA NA NA NA NA 123278 120537 124845
     $ V8 : int  NA NA NA NA NA 11464 11802 11452
     $ V9 : int  NA NA NA NA NA 1253 1204 1229
     $ V10: int  NA NA NA NA NA 380 382 374
     $ V11: int  NA NA NA NA NA 256 263 253
     $ V12: logi  NA NA NA NA NA NA ...

# 1 STANDARD CURVES

## 1.1 Glycogen Standard Curve

### 1.1.1 Extract luminescence data

``` r
# Extract glycogen standard curve data from plate layout and raw luminescence.
# Rows F, G, H (rows 6, 7, 8) correspond to glycogen standards
# Columns 1-5 contain the standard curve concentrations

# Extract concentration values from plate layout (row 6, columns 1-5)
# Parse concentration from labels like "STD-glyc-20" -> 20
glyc_concentrations <- as.numeric(gsub("Std-Glyc-", "", plate_layout[6, 1:5]))
glyc_concentrations

# Extract standard curve luminescence values for each replicate from raw_luminescence
glyc_row_F <- as.numeric(raw_luminescence[6, 1:5])  # Row 6 (F)
glyc_row_G <- as.numeric(raw_luminescence[7, 1:5])  # Row 7 (G)
glyc_row_H <- as.numeric(raw_luminescence[8, 1:5])  # Row 8 (H)
```

    [1] 20.00  2.00  0.20  0.02  0.00

``` r
#Extract glycogen sample data - wells B8-E6
glyc_sample_cols <- c(1,2,3)
glyc_A3_luminescence <- as.numeric(raw_luminescence[1, glyc_sample_cols])
glyc_A3_dilution <- 1

glyc_A4_luminescence <- as.numeric(raw_luminescence[2, glyc_sample_cols])
glyc_A4_dilution <- 1
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

    [1] 158507.3
    [1] 21.3847

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

    [1] 162704.7
    [1] 21.94974

``` r
# Create the plot
glyc_plot <- ggplot(glycogen_summary_data, aes(x = glyc_concentration, y = glyc_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glyc_mean_luminescence - glyc_se, ymax = glyc_mean_luminescence + glyc_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +
  
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


  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                                "A3df.1" = "darkred",
                                "A4df.1" = "darkgreen"
                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                 "A3df.1" = 17,
                                 "A4df.1" = 15
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

![](Gen5-20260116-mgig-glycogenglo_files/figure-gfm/plot%20glycogen%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glyc_A3_data
glyc_A4_data

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

      glyc_A3_dilution_factor glyc_A3_mean_luminescence glyc_A3_se glyc_A3_conc
    1                       1                  158507.3   5043.553      21.3847
       label
    1 A3df.1
      glyc_A4_dilution_factor glyc_A4_mean_luminescence glyc_A4_se glyc_A4_conc
    1                       1                  162704.7   4441.979     21.94974
       label
    1 A4df.1
    glycogen Standard Curve Summary:
    ==================================================
    Concentration: 20 µg/µL
      Mean Luminescence: 148603.67
      Standard Error: 3385.49
      CV%: 3.95%

    Concentration: 2 µg/µL
      Mean Luminescence: 10609.33
      Standard Error: 332.55
      CV%: 5.43%

    Concentration: 0.2 µg/µL
      Mean Luminescence: 1803.00
      Standard Error: 19.30
      CV%: 1.85%

    Concentration: 0.02 µg/µL
      Mean Luminescence: 1040.67
      Standard Error: 20.63
      CV%: 3.43%

    Concentration: 0 µg/µL
      Mean Luminescence: 1274.33
      Standard Error: 271.27
      CV%: 36.87%

``` r
tab <- matrix(c(glyc_A3_dilution, glyc_A3_mean_lum, glyc_A3_mean_conc,  (glyc_A3_dilution*glyc_A3_mean_conc), 
                glyc_A4_dilution, glyc_A4_mean_lum, glyc_A4_mean_conc,  (glyc_A4_dilution*glyc_A4_mean_conc)), ncol=4, byrow=TRUE)
colnames(tab) <- c('Dilution factor','Luminescence','Calculated Glycogen (ug/uL)', 'Total glycogen (ug/uL)')
rownames(tab) <- c('A3','A4')
tab <- as.table(tab)
tab
```

       Dilution factor Luminescence Calculated Glycogen (ug/uL)
    A3         1.00000 158507.33333                    21.38470
    A4         1.00000 162704.66667                    21.94974
       Total glycogen (ug/uL)
    A3               21.38470
    A4               21.94974

# 2 STANDARD CURVES

## 2.1 Glucose Standard Curve

### 2.1.1 Extract luminescence data

``` r
# Extract glucose standard curve data from plate layout and raw luminescence.
# Rows F, G, H (rows 6, 7, 8) correspond to glucose standards
# Columns 1-5 contain the standard curve concentrations

# Extract concentration values from plate layout (row 6, columns 1-5)
# Parse concentration from labels like "STD-gluc-20" -> 20
glu_concentrations <- as.numeric(gsub("Std-Gluc-", "", plate_layout[6, 7:11]))
glu_concentrations

# Extract standard curve luminescence values for each replicate from raw_luminescence
glu_row_F <- as.numeric(raw_luminescence[6, 7:11])  # Row 6 (F)
glu_row_G <- as.numeric(raw_luminescence[7, 7:11])  # Row 7 (G)
glu_row_H <- as.numeric(raw_luminescence[8, 7:11])  # Row 8 (H)
```

    [1] 100.0  10.0   1.0   0.1   0.0

``` r
#Extract glucose sample data - wells B8-E6
glu_sample_cols <- c(1,2,3)
glu_A3_luminescence <- as.numeric(raw_luminescence[3, glu_sample_cols])
glu_A3_dilution <- 1

glu_A4_luminescence <- as.numeric(raw_luminescence[4, glu_sample_cols])
glu_A4_dilution <- 1
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

    [1] 1293.333
    [1] 1.079603

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

    [1] 496.3333
    [1] 0.4308565

``` r
# Create the plot
glu_plot <- ggplot(glucose_summary_data, aes(x = glu_concentration, y = glu_mean_luminescence)) +
  geom_smooth(aes(linetype = "Std Curve Best Fit Line"), method = "lm", se = FALSE, 
              color = "coral", linewidth = 1) +
  geom_errorbar(aes(ymin = glu_mean_luminescence - glu_se, ymax = glu_mean_luminescence + glu_se),
                width = 0.1, linewidth = 1, color = "darkblue") +
  geom_point(aes(color = "Standard Curve", shape = "Standard Curve"), size = 4) +
  
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


  scale_color_manual(name = "",
                     values = c("Standard Curve" = "steelblue",
                                "A3df.1" = "darkred",
                                "A4df.1" = "darkgreen"
                                )) +
  scale_shape_manual(name = "",
                     values = c("Standard Curve" = 16,
                                 "A3df.1" = 17,
                                 "A4df.1" = 15
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

![](Gen5-20260116-mgig-glycogenglo_files/figure-gfm/plot%20glucose%20standard%20curve%20with%20samples-1.png)<!-- -->

``` r
glu_A3_data
glu_A4_data

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

      glu_A3_dilution_factor glu_A3_mean_luminescence glu_A3_se glu_A3_conc  label
    1                      1                 1293.333  76.88158    1.079603 A3df.1
      glu_A4_dilution_factor glu_A4_mean_luminescence glu_A4_se glu_A4_conc  label
    1                      1                 496.3333  14.24001   0.4308565 A4df.1
    glucose Standard Curve Summary:
    ==================================================
    Concentration: 100 µg/µL
      Mean Luminescence: 122886.67
      Standard Error: 1258.91
      CV%: 1.77%

    Concentration: 10 µg/µL
      Mean Luminescence: 11572.67
      Standard Error: 114.72
      CV%: 1.72%

    Concentration: 1 µg/µL
      Mean Luminescence: 1228.67
      Standard Error: 14.15
      CV%: 1.99%

    Concentration: 0.1 µg/µL
      Mean Luminescence: 378.67
      Standard Error: 2.40
      CV%: 1.10%

    Concentration: 0 µg/µL
      Mean Luminescence: 257.33
      Standard Error: 2.96
      CV%: 1.99%

``` r
tab <- matrix(c(glu_A3_dilution, glu_A3_mean_lum, glu_A3_mean_conc,  (glu_A3_dilution*glu_A3_mean_conc), 
                glu_A4_dilution, glu_A4_mean_lum, glu_A4_mean_conc,  (glu_A4_dilution*glu_A4_mean_conc)), ncol=4, byrow=TRUE)
colnames(tab) <- c('Dilution factor','Luminescence','Calculated glucose (ug/uL)', 'Total glucose (ug/uL)')
rownames(tab) <- c('A3','A4')
tab <- as.table(tab)
tab
```

       Dilution factor Luminescence Calculated glucose (ug/uL)
    A3       1.0000000 1293.3333333                  1.0796029
    A4       1.0000000  496.3333333                  0.4308565
       Total glucose (ug/uL)
    A3             1.0796029
    A4             0.4308565
