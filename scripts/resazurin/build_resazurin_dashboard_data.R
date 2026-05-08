suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(tibble)
})

repo_root <- getwd()
if (!dir.exists(file.path(repo_root, "Resazurin"))) {
  repo_root <- normalizePath(file.path(repo_root, ".."), mustWork = TRUE)
}
if (!dir.exists(file.path(repo_root, "Resazurin"))) {
  stop("Could not find Resazurin. Run this script from repository root or scripts/resazurin.")
}

source(file.path(repo_root, "scripts", "resazurin", "parse_plate_exports.R"))

resazurin_root <- file.path(repo_root, "Resazurin")
out_dir <- file.path(repo_root, "output", "resazurin")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

has_lmer_stats <- requireNamespace("lmerTest", quietly = TRUE) &&
  requireNamespace("lme4", quietly = TRUE) &&
  requireNamespace("emmeans", quietly = TRUE)
has_emmeans <- requireNamespace("emmeans", quietly = TRUE)

plate_files <- list.files(
  path = resazurin_root,
  pattern = "(?i)^plate-.*-T[0-9]+(?:\\.[0-9]+)?\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(plate_files) == 0) {
  write_csv(tibble(), file.path(out_dir, "dashboard-data.csv"))
  write_csv(tibble(), file.path(out_dir, "dashboard-qc.csv"))
  message("No plate files found; wrote empty dashboard outputs.")
  quit(save = "no")
}

parsed_results <- map(plate_files, function(path) {
  tryCatch(
    {
      dat <- parse_plate_export(path)
      list(ok = TRUE, data = dat, error = NA_character_, file = path)
    },
    error = function(e) {
      list(ok = FALSE, data = tibble(), error = conditionMessage(e), file = path)
    }
  )
})

plate_data <- parsed_results %>%
  keep(~.x$ok) %>%
  map("data") %>%
  bind_rows()

parse_failures <- parsed_results %>%
  keep(~!.x$ok) %>%
  map_dfr(~tibble(
    experiment_dir = basename(dirname(.x$file)),
    source_file = .x$file,
    parse_ok = FALSE,
    parse_error = .x$error,
    n_wells = 0L
  ))

if (nrow(plate_data) == 0) {
  write_csv(tibble(), file.path(out_dir, "dashboard-data.csv"))
  write_csv(parse_failures, file.path(out_dir, "dashboard-qc.csv"))
  message("All parse attempts failed; wrote QC report.")
  quit(save = "no")
}

# Join optional layout metadata by experiment directory and well.
experiment_dirs <- unique(dirname(plate_data$source_file))
layout_data <- map_dfr(experiment_dirs, function(exp_dir) {
  parsed <- parse_layout_for_experiment(exp_dir)
  parsed %>%
    mutate(experiment_dir = basename(exp_dir))
})

if (nrow(layout_data) == 0) {
  plate_data <- plate_data %>%
    mutate(
      sample_label = NA_character_,
      treatment = NA_character_,
      shell_color = NA_character_,
      treatment_group = NA_character_,
      size_mm = NA_real_,
      weight_mg = NA_real_,
      exclude_from_analysis = FALSE,
      exclude_reason = NA_character_,
      is_blank = NA,
      layout_status = "missing",
      layout_file = NA_character_,
      layout_raw = NA_character_
    )
} else {
  parsed_layout <- layout_data %>% filter(!is.na(well_id))
  measurement_cols <- names(parsed_layout)[str_detect(names(parsed_layout), "_measur(?:e)?ment$")]
  group_cols <- names(parsed_layout)[str_detect(names(parsed_layout), "_group$")]
  dynamic_group_cols <- setdiff(group_cols, "treatment_group")

  # If plate_id is absent in layout rows, expand those mappings across all
  # plate IDs seen in that experiment directory.
  plate_ids_by_exp <- plate_data %>%
    distinct(experiment_dir, plate_id)

  parsed_layout_with_plate <- parsed_layout %>%
    filter(!is.na(plate_id))

  parsed_layout_no_plate <- parsed_layout %>%
    filter(is.na(plate_id)) %>%
    select(-plate_id) %>%
    left_join(plate_ids_by_exp, by = "experiment_dir")

  parsed_layout_join <- bind_rows(parsed_layout_with_plate, parsed_layout_no_plate)

  plate_data <- plate_data %>%
    left_join(
      parsed_layout_join %>%
        select(
          experiment_dir,
          plate_id,
          well_id,
          sample_label,
          treatment,
          shell_color,
          treatment_group,
          size_mm,
          weight_mg,
          exclude_from_analysis,
          exclude_reason,
          any_of(dynamic_group_cols),
          any_of(measurement_cols),
          is_blank,
          layout_status,
          layout_file,
          layout_raw
        ),
      by = c("experiment_dir", "plate_id", "well_id")
    ) %>%
    mutate(layout_status = coalesce(layout_status, "missing_or_unparsed"))

  unparsed_layout <- layout_data %>%
    filter(is.na(well_id)) %>%
    select(experiment_dir, layout_status, layout_file, layout_raw)

  if (nrow(unparsed_layout) > 0) {
    plate_data <- plate_data %>%
      left_join(unparsed_layout, by = "experiment_dir", suffix = c("", "_fallback")) %>%
      mutate(
        layout_status = if_else(
          layout_status == "missing_or_unparsed" & !is.na(layout_status_fallback),
          layout_status_fallback,
          layout_status
        ),
        layout_file = coalesce(layout_file, layout_file_fallback),
        layout_raw = coalesce(layout_raw, layout_raw_fallback)
      ) %>%
      select(-layout_status_fallback, -layout_file_fallback, -layout_raw_fallback)
  }
}

if (!exists("measurement_cols")) {
  measurement_cols <- names(plate_data)[str_detect(names(plate_data), "_measur(?:e)?ment$")]
}
if (!exists("group_cols")) {
  group_cols <- names(plate_data)[str_detect(names(plate_data), "_group$")]
}
dynamic_group_cols <- setdiff(group_cols, "treatment_group")

plate_data <- plate_data %>%
  mutate(
    is_blank = case_when(
      is.na(is_blank) ~ FALSE,
      TRUE ~ as.logical(is_blank)
    ),
    exclude_from_analysis = case_when(
      is.na(exclude_from_analysis) ~ FALSE,
      TRUE ~ as.logical(exclude_from_analysis)
    )
  )

analysis_data <- plate_data %>%
  filter(!exclude_from_analysis)

# Add simple duplicate-timepoint flag by experiment + plate + well.
dup_flags <- analysis_data %>%
  count(experiment_dir, plate_id, well_id, time_hr, name = "n_at_time") %>%
  mutate(duplicate_timepoint = n_at_time > 1)

analysis_data <- analysis_data %>%
  left_join(
    dup_flags %>% select(experiment_dir, plate_id, well_id, time_hr, duplicate_timepoint),
    by = c("experiment_dir", "plate_id", "well_id", "time_hr")
  )

# Compute mean blank fluorescence by plate and timepoint.
blank_ref <- analysis_data %>%
  filter(is_blank) %>%
  group_by(experiment_dir, plate_id, time_hr) %>%
  summarise(mean_blank_value = mean(value, na.rm = TRUE), .groups = "drop")

analysis_data <- analysis_data %>%
  left_join(blank_ref, by = c("experiment_dir", "plate_id", "time_hr")) %>%
  mutate(
    normalized_value = if_else(
      !is.na(mean_blank_value) & mean_blank_value != 0,
      value / mean_blank_value,
      NA_real_
    )
  )

# Compute point-to-point delta fluorescence for each well trajectory.
analysis_data <- analysis_data %>%
  group_by(experiment_dir, plate_id, well_id) %>%
  arrange(time_hr, read_datetime, source_name, .by_group = TRUE) %>%
  mutate(
    delta_value = if_else(
      time_hr == min(time_hr, na.rm = TRUE),
      0,
      value - lag(value)
    )
  ) %>%
  ungroup()

# Compute cumulative and total AUC using sample_id when available, otherwise fall back to wells.
# Sample-level AUC collapses technical wells within each sample/timepoint before trapezoidal integration.
compute_auc_tables <- function(data, unit_cols, unit_type) {
  if (nrow(data) == 0) {
    return(list(
      cumulative = tibble(),
      values = tibble()
    ))
  }

  point_data <- data %>%
    filter(!is.na(.data$time_hr)) %>%
    group_by(across(all_of(unit_cols)), time_hr = .data$time_hr) %>%
    summarise(
      normalized_value = mean(.data$normalized_value, na.rm = TRUE),
      n_measurements = dplyr::n(),
      .groups = "drop"
    )

  cumulative <- point_data %>%
    group_by(across(all_of(unit_cols))) %>%
    arrange(.data$time_hr, .by_group = TRUE) %>%
    mutate(
      valid_for_auc = (!is.na(.data$normalized_value) & is.finite(.data$normalized_value) & !is.na(.data$time_hr) & is.finite(.data$time_hr)),
      next_time = lead(.data$time_hr),
      next_val = lead(.data$normalized_value)
    ) %>%
    mutate(
      inc_area = if_else(
        .data$valid_for_auc & !is.na(.data$next_time) & is.finite(.data$next_time) & (.data$next_time > .data$time_hr) & !is.na(.data$next_val) & is.finite(.data$next_val),
        (.data$next_time - .data$time_hr) * ((.data$normalized_value + .data$next_val) / 2),
        0
      ),
      cumulative_auc = cumsum(replace_na(.data$inc_area, 0))
    ) %>%
    ungroup() %>%
    select(all_of(unit_cols), .data$time_hr, .data$cumulative_auc)

  values <- cumulative %>%
    left_join(
      point_data %>%
        group_by(across(all_of(unit_cols))) %>%
        summarise(n_valid_for_auc = sum(!is.na(.data$normalized_value) & is.finite(.data$normalized_value) & !is.na(.data$time_hr) & is.finite(.data$time_hr)), .groups = "drop"),
      by = unit_cols
    ) %>%
    group_by(across(all_of(unit_cols))) %>%
    summarise(
      auc_value = if (first(.data$n_valid_for_auc) >= 2) max(.data$cumulative_auc, na.rm = TRUE) else NA_real_,
      n_timepoints = n_distinct(.data$time_hr),
      .groups = "drop"
    ) %>%
    mutate(analysis_unit_type = unit_type)

  cumulative %>%
    mutate(analysis_unit_type = unit_type)

  list(cumulative = cumulative, values = values)
}

sample_auc <- compute_auc_tables(
  analysis_data %>% filter(!is.na(sample_id_group) & sample_id_group != ""),
  c("experiment_dir", "sample_id_group"),
  "sample_id"
)

well_auc <- compute_auc_tables(
  analysis_data %>% filter(is.na(sample_id_group) | sample_id_group == ""),
  c("experiment_dir", "plate_id", "well_id"),
  "well"
)

# Join cumulative_auc and auc_value back into analysis_data, preferring sample-level results.
analysis_data <- analysis_data %>%
  left_join(
    sample_auc$cumulative %>% rename(sample_cumulative_auc = cumulative_auc),
    by = c("experiment_dir", "sample_id_group", "time_hr")
  ) %>%
  left_join(
    sample_auc$values %>% rename(sample_auc_value = auc_value, sample_n_timepoints = n_timepoints),
    by = c("experiment_dir", "sample_id_group")
  ) %>%
  left_join(
    well_auc$cumulative %>% rename(well_cumulative_auc = cumulative_auc),
    by = c("experiment_dir", "plate_id", "well_id", "time_hr")
  ) %>%
  left_join(
    well_auc$values %>% rename(well_auc_value = auc_value, well_n_timepoints = n_timepoints),
    by = c("experiment_dir", "plate_id", "well_id")
  ) %>%
  mutate(
    cumulative_auc = coalesce(sample_cumulative_auc, well_cumulative_auc),
    auc_value = coalesce(sample_auc_value, well_auc_value),
    n_timepoints = coalesce(sample_n_timepoints, well_n_timepoints),
    analysis_unit_type = case_when(
      !is.na(sample_auc_value) ~ "sample_id",
      !is.na(well_auc_value) ~ "well",
      TRUE ~ NA_character_
    ),
    analysis_unit_id = case_when(
      !is.na(sample_auc_value) ~ paste(experiment_dir, sample_id_group, sep = "|"),
      !is.na(well_auc_value) ~ paste(experiment_dir, plate_id, well_id, sep = "|"),
      TRUE ~ NA_character_
    )
  )

analysis_cols <- analysis_data %>%
  select(
    experiment_dir,
    source_file,
    plate_id,
    well_id,
    time_hr,
    duplicate_timepoint,
    mean_blank_value,
    normalized_value,
    delta_value
  )

plate_data <- plate_data %>%
  left_join(
    analysis_cols,
    by = c("experiment_dir", "source_file", "plate_id", "well_id", "time_hr")
  ) %>%
  mutate(
    duplicate_timepoint = coalesce(duplicate_timepoint, FALSE)
  )

qc_success <- plate_data %>%
  group_by(experiment_dir, source_file) %>%
  summarise(
    parse_ok = TRUE,
    parse_error = NA_character_,
    n_wells = n(),
    min_time_hr = min(time_hr, na.rm = TRUE),
    max_time_hr = max(time_hr, na.rm = TRUE),
    layout_status = paste(sort(unique(layout_status)), collapse = ";"),
    duplicate_timepoints = any(duplicate_timepoint, na.rm = TRUE),
    .groups = "drop"
  )

qc <- bind_rows(qc_success, parse_failures) %>%
  arrange(experiment_dir, source_file)

plate_data_out <- plate_data %>%
  arrange(experiment_dir, plate_id, well_id, time_hr) %>%
  select(
    experiment_dir,
    source_file,
    source_name,
    plate_id,
    time_hr,
    read_datetime,
    actual_temperature,
    excitation_emission,
    row_id,
    col_id,
    well_id,
    sample_label,
    treatment,
    shell_color,
    treatment_group,
    size_mm,
    weight_mg,
    exclude_from_analysis,
    exclude_reason,
    any_of(dynamic_group_cols),
    any_of(measurement_cols),
    is_blank,
    layout_status,
    duplicate_timepoint,
    value,
    delta_value,
    mean_blank_value,
    normalized_value
  )

  # Write main outputs
  write_csv(plate_data_out, file.path(out_dir, "dashboard-data.csv"))
  write_csv(qc, file.path(out_dir, "dashboard-qc.csv"))

  # --- Compute paper-style AUC statistics for dashboard -----------------------
  valid_group_value <- function(x) {
    x <- str_trim(as.character(x))
    !is.na(x) & x != "" & !str_to_upper(x) %in% c("NA", "N/A", "NULL", "NAN", "<NA>")
  }

  first_non_missing <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA else x[1]
  }

  auc_from_points <- function(time, value) {
    keep <- is.finite(time) & is.finite(value)
    time <- time[keep]
    value <- value[keep]
    if (length(time) < 2) return(NA_real_)

    ord <- order(time)
    time <- time[ord]
    value <- value[ord]

    dt <- diff(time)
    if (!any(is.finite(dt) & dt > 0)) return(NA_real_)
    sum(dt * ((head(value, -1) + tail(value, -1)) / 2), na.rm = TRUE)
  }

  parse_tukey_pair <- function(x) {
    parts <- str_split_fixed(x, "-", 2)
    tibble(group1 = parts[, 2], group2 = parts[, 1])
  }

  add_pair_summaries <- function(pair_tbl, data) {
    group_summary <- data %>%
      group_by(group_val) %>%
      summarise(
        n = dplyr::n(),
        median = median(auc_value, na.rm = TRUE),
        n_wells = sum(n_wells, na.rm = TRUE),
        n_timepoints = sum(n_timepoints, na.rm = TRUE),
        .groups = "drop"
      )

    pair_tbl %>%
      left_join(group_summary, by = c("group1" = "group_val")) %>%
      rename(n1 = n, median1 = median, n_wells1 = n_wells, n_timepoints1 = n_timepoints) %>%
      left_join(group_summary, by = c("group2" = "group_val")) %>%
      rename(n2 = n, median2 = median, n_wells2 = n_wells, n_timepoints2 = n_timepoints)
  }

  fit_auc_stats <- function(per_exp, exp, gcol, size_col) {
    per_exp <- per_exp %>%
      filter(is.finite(.data$auc_value), valid_group_value(.data$group_val)) %>%
      mutate(group_val = factor(.data$group_val))

    if (nrow(per_exp) < 3 || n_distinct(per_exp$group_val) < 2) return(tibble())

    use_lmm <- has_lmer_stats && n_distinct(per_exp$plate_id) > 1
    model_class <- if (use_lmm) "LMM" else "ANOVA"
    model_formula <- if (use_lmm) "auc_value ~ group_val + (1 | plate_id)" else "auc_value ~ group_val"
    p_adjust_method <- if (has_emmeans) "tukey" else "TukeyHSD"

    model <- tryCatch(
      {
        if (use_lmm) {
          lmerTest::lmer(auc_value ~ group_val + (1 | plate_id), data = per_exp)
        } else {
          stats::lm(auc_value ~ group_val, data = per_exp)
        }
      },
      error = function(e) NULL
    )
    if (is.null(model) && use_lmm) {
      model <- tryCatch(stats::lm(auc_value ~ group_val, data = per_exp), error = function(e) NULL)
      model_class <- "ANOVA"
      model_formula <- "auc_value ~ group_val"
    }
    if (is.null(model)) return(tibble())

    omnibus_tbl <- tryCatch(
      {
        a <- stats::anova(model)
        p_col <- intersect(c("Pr(>F)", "Pr(>Chisq)"), names(a))[1]
        stat_col <- intersect(c("F value", "Chisq"), names(a))[1]
        tibble(
          comparison_type = "omnibus",
          comparison = model_class,
          term = "group_val",
          statistic = if (!is.na(stat_col)) as.numeric(a["group_val", stat_col]) else NA_real_,
          p_value = if (!is.na(p_col)) as.numeric(a["group_val", p_col]) else NA_real_,
          p_adj = if (!is.na(p_col)) as.numeric(a["group_val", p_col]) else NA_real_,
          group1 = NA_character_,
          group2 = NA_character_,
          n1 = NA_integer_,
          n2 = NA_integer_,
          median1 = NA_real_,
          median2 = NA_real_,
          n_wells1 = NA_integer_,
          n_wells2 = NA_integer_,
          n_timepoints1 = NA_integer_,
          n_timepoints2 = NA_integer_
        )
      },
      error = function(e) tibble()
    )

    pair_tbl <- if (has_emmeans) {
      tryCatch(
        {
          pairs <- as.data.frame(
            emmeans::contrast(
              emmeans::emmeans(model, specs = "group_val"),
              method = "pairwise",
              adjust = "tukey"
            )
          )
          pair_names <- str_split_fixed(as.character(pairs$contrast), " - ", 2)
          pair_names <- apply(pair_names, 2, function(x) str_remove(x, "^group_val"))
          tibble(
            comparison_type = "pairwise",
            comparison = "emmeans",
            term = "group_val",
            group1 = pair_names[, 1],
            group2 = pair_names[, 2],
            statistic = if ("t.ratio" %in% names(pairs)) as.numeric(pairs$t.ratio) else NA_real_,
            p_value = as.numeric(pairs$p.value),
            p_adj = as.numeric(pairs$p.value)
          )
        },
        error = function(e) tibble()
      )
    } else {
      tryCatch(
        {
          tk <- stats::TukeyHSD(stats::aov(auc_value ~ group_val, data = per_exp), "group_val")$group_val
          pair_names <- parse_tukey_pair(rownames(tk))
          tibble(
            comparison_type = "pairwise",
            comparison = "TukeyHSD",
            term = "group_val",
            group1 = pair_names$group1,
            group2 = pair_names$group2,
            statistic = as.numeric(tk[, "diff"]),
            p_value = as.numeric(tk[, "p adj"]),
            p_adj = as.numeric(tk[, "p adj"])
          )
        },
        error = function(e) tibble()
      )
    }

    if (nrow(pair_tbl) > 0) {
      pair_tbl <- add_pair_summaries(pair_tbl, per_exp)
    }

    bind_rows(omnibus_tbl, pair_tbl) %>%
      mutate(
        experiment_dir = exp,
        plate_id = NA_character_,
        group_col = gcol,
        size_col = size_col,
        metric = "paper_metabolism_auc",
        model_class = model_class,
        model_formula = model_formula,
        response_transform = "none",
        p_adjust_method = p_adjust_method,
        analysis_unit_type = paste(sort(unique(per_exp$analysis_unit_type)), collapse = ";"),
        .before = 1
      )
  }

  compute_paper_auc_units <- function(data, size_col, grouping_cols) {
    data_size <- data %>%
      mutate(
        size_value = suppressWarnings(as.numeric(.data[[size_col]])),
        analysis_unit_type = if_else(!is.na(.data$sample_id_group) & .data$sample_id_group != "", "sample_id", "well"),
        analysis_unit_id = if_else(
          .data$analysis_unit_type == "sample_id",
          paste(.data$experiment_dir, .data$sample_id_group, sep = "|"),
          paste(.data$experiment_dir, .data$plate_id, .data$well_id, sep = "|")
        )
      )

    initial_ref <- data_size %>%
      filter(is.finite(.data$value), is.finite(.data$time_hr)) %>%
      group_by(.data$analysis_unit_id) %>%
      filter(.data$time_hr == min(.data$time_hr, na.rm = TRUE)) %>%
      summarise(initial_value = mean(.data$value, na.rm = TRUE), .groups = "drop")

    fold_data <- data_size %>%
      left_join(initial_ref, by = "analysis_unit_id") %>%
      mutate(
        fold_change = if_else(is.finite(.data$initial_value) & .data$initial_value != 0, .data$value / .data$initial_value, NA_real_)
      )

    blank_fold_ref <- fold_data %>%
      filter(.data$is_blank, is.finite(.data$fold_change)) %>%
      group_by(.data$experiment_dir, .data$plate_id, .data$time_hr) %>%
      summarise(mean_blank_fold_change = mean(.data$fold_change, na.rm = TRUE), .groups = "drop")

    point_data <- fold_data %>%
      left_join(blank_fold_ref, by = c("experiment_dir", "plate_id", "time_hr")) %>%
      filter(!.data$is_blank, is.finite(.data$size_value), .data$size_value > 0) %>%
      mutate(
        mean_blank_fold_change = coalesce(.data$mean_blank_fold_change, 0),
        paper_metabolism_value = (.data$fold_change - .data$mean_blank_fold_change) / .data$size_value
      ) %>%
      filter(is.finite(.data$paper_metabolism_value), is.finite(.data$time_hr)) %>%
      group_by(.data$experiment_dir, .data$analysis_unit_type, .data$analysis_unit_id, .data$time_hr) %>%
      summarise(
        plate_id = first_non_missing(.data$plate_id),
        n_wells = n_distinct(.data$well_id),
        paper_metabolism_value = mean(.data$paper_metabolism_value, na.rm = TRUE),
        across(all_of(grouping_cols), first_non_missing),
        .groups = "drop"
      )

    point_data %>%
      group_by(.data$experiment_dir, .data$analysis_unit_type, .data$analysis_unit_id) %>%
      summarise(
        plate_id = first_non_missing(.data$plate_id),
        auc_value = auc_from_points(.data$time_hr, .data$paper_metabolism_value),
        n_timepoints = n_distinct(.data$time_hr[is.finite(.data$paper_metabolism_value)]),
        n_wells = max(.data$n_wells, na.rm = TRUE),
        across(all_of(grouping_cols), first_non_missing),
        .groups = "drop"
      ) %>%
      filter(is.finite(.data$auc_value), .data$n_timepoints >= 2)
  }

  grouping_cols <- names(plate_data)[str_detect(names(plate_data), "_group$")]
  grouping_cols <- setdiff(grouping_cols, "sample_id_group")
  size_cols <- measurement_cols[
    map_lgl(measurement_cols, ~ any(is.finite(suppressWarnings(as.numeric(analysis_data[[.x]]))) &
      suppressWarnings(as.numeric(analysis_data[[.x]])) > 0, na.rm = TRUE))
  ]

  auc_stats <- tibble()

  if (length(grouping_cols) > 0 && length(size_cols) > 0) {
    for (size_col in size_cols) {
      auc_for_stats <- compute_paper_auc_units(analysis_data, size_col, grouping_cols)
      if (nrow(auc_for_stats) == 0) next

      for (gcol in grouping_cols) {
        per_group <- auc_for_stats %>%
          filter(valid_group_value(.data[[gcol]])) %>%
          mutate(group_val = as.character(.data[[gcol]]))

        if (nrow(per_group) == 0 || n_distinct(per_group$group_val) < 2) next

        for (exp in unique(per_group$experiment_dir)) {
          per_exp <- per_group %>% filter(.data$experiment_dir == exp)
          auc_stats <- bind_rows(
            auc_stats,
            fit_auc_stats(per_exp, exp, gcol, size_col)
          )
        }
      }
    }
  }

  write_csv(auc_stats, file.path(out_dir, "dashboard-stats.csv"))

  message("Wrote: ", file.path(out_dir, "dashboard-data.csv"))
  message("Wrote: ", file.path(out_dir, "dashboard-qc.csv"))
  message("Wrote: ", file.path(out_dir, "dashboard-stats.csv"))
