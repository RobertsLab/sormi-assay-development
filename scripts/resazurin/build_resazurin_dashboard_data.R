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

# Compute cumulative and total AUC per well using blank-normalized values.
# We compute incremental trapezoidal areas between successive valid timepoints
# where `normalized_value` and `time_hr` are finite. We expose two columns:
# - `cumulative_auc`: cumulative area up to that timepoint (0 for the first point)
# - `auc_value`: total AUC for the trajectory (NA if fewer than 2 valid points)
auc_cumulative <- analysis_data %>%
  filter(!is.na(time_hr)) %>%
  group_by(experiment_dir, plate_id, well_id) %>%
  arrange(time_hr, read_datetime, source_name, .by_group = TRUE) %>%
  mutate(
    valid_for_auc = (!is.na(normalized_value) & is.finite(normalized_value) & !is.na(time_hr) & is.finite(time_hr)),
    next_time = lead(time_hr),
    next_val = lead(normalized_value)
  ) %>%
  mutate(
    inc_area = if_else(
      valid_for_auc & !is.na(next_time) & is.finite(next_time) & (next_time > time_hr) & !is.na(next_val) & is.finite(next_val),
      (next_time - time_hr) * ((normalized_value + next_val) / 2),
      0
    )
  ) %>%
  mutate(
    cumulative_auc = cumsum(replace_na(inc_area, 0))
  ) %>%
  ungroup() %>%
  select(experiment_dir, plate_id, well_id, time_hr, cumulative_auc)

# Number of valid points per well (needed to determine availability)
valid_counts <- analysis_data %>%
  group_by(experiment_dir, plate_id, well_id) %>%
  summarise(n_valid_for_auc = sum(!is.na(normalized_value) & is.finite(normalized_value) & !is.na(time_hr) & is.finite(time_hr)), .groups = "drop")

# Derive final auc_value per well (NA if fewer than 2 valid points)
auc_value_per_well <- auc_cumulative %>%
  left_join(valid_counts, by = c("experiment_dir", "plate_id", "well_id")) %>%
  group_by(experiment_dir, plate_id, well_id) %>%
  summarise(
    auc_value = if_else(n_valid_for_auc >= 2, max(cumulative_auc, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# Ensure unique one-row-per-well to avoid many-to-many joins downstream
auc_value_per_well <- auc_value_per_well %>%
  distinct(experiment_dir, plate_id, well_id, .keep_all = TRUE)

# Join cumulative_auc (per timepoint) and auc_value (per well) back into analysis_data
analysis_data <- analysis_data %>%
  left_join(auc_cumulative, by = c("experiment_dir", "plate_id", "well_id", "time_hr")) %>%
  left_join(auc_value_per_well, by = c("experiment_dir", "plate_id", "well_id"))

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

write_csv(plate_data_out, file.path(out_dir, "dashboard-data.csv"))
write_csv(qc, file.path(out_dir, "dashboard-qc.csv"))

message("Wrote: ", file.path(out_dir, "dashboard-data.csv"))
message("Wrote: ", file.path(out_dir, "dashboard-qc.csv"))
