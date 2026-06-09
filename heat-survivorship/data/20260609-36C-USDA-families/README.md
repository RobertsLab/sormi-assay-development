`heat-survivorship/data/20260609-36C-USDA-families`

## Description

Juvenile Pacific oysters (_M.gigas_) from nine USDA families were distributed across nine 12-well plates. They were submerged in 4mL of Instant Ocean at ~15<sup>o</sup>C and then transferred to an incubator at 36<sup>o</sup>C. Mortalities were assessed periodically.

---

## Files

### [survivorship.csv](survivorship.csv)

Tracks alive/dead status of individual oysters across periodic assessment timepoints throughout the heat stress experiment. Each row represents one observation of one well at one timepoint.

| Column | Description |
|---|---|
| `plate_ID` | Plate identifier (plate-A through plate-I) |
| `plate_well` | Well position on the plate (e.g., A01, B03) |
| `individual_id` | Unique numeric ID assigned to each oyster (1–99); blank for empty/blank wells |
| `familly_id.group` | USDA family identifier (1, 3, 5, 6, 7, 8, 9, 9b, 10) |
| `is_blank` | `TRUE` if the well contains no oyster (negative control); `FALSE` otherwise |
| `timepoint_count` | Sequential timepoint number (0 = start of experiment) |
| `timepoint_hrs` | Elapsed time from experiment start in hours |
| `alive.measurement` | `TRUE` if the oyster was alive at assessment; `FALSE` if dead |
| `date` | Date of assessment (MDDYYYY format) |
| `time` | Time of assessment (HH:MM, 24-hour) |
| `notes` | Optional observation notes |
