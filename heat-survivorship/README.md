# Heat survivorship experiments

This directory contains heat-survivorship experiments for juvenile Pacific
oysters (_M. gigas_), primarily comparing USDA family groups exposed to 33 C or
36 C heat challenges. The directory is intended to be a living record: add new
experiment folders, analysis scripts, outputs, and summary notes here as new
heat-survival assays are carried out.

## Directory contents

- [`data/`](data/) contains dated experiment data folders.
- [`code/`](code/) contains R Markdown and Quarto analyses for individual
  experiments and cross-experiment summaries.
- [`outputs/`](outputs/) contains rendered analysis reports, standardized
  survival tables, family summaries, and plots.

## Experiments included to date

| Experiment | Temperature | Families/groups | Individuals | Deaths | Notes |
|---|---:|---|---:|---:|---|
| [`20260427-33C-USDA-families`](data/20260427-33C-USDA-families/) | 33 C | 1, 2, 3, 5, 6, 7, 8, 9, 10 | 128 | 63 (49.2%) | Oysters had previously been heat stressed at 36 C and assayed with resazurin on 2026-04-15. This experiment also includes ImageJ size measurements. |
| [`20260511-33C-USDA-families`](data/20260511-33C-USDA-families/) | 33 C | 1, 3, 5, 6, 7, 8, 9, 9B, 10 | 108 | 107 (99.1%) | 24-well plate heat challenge over approximately 72 h. Groups labelled `9` and `9B` represent unresolved family 2/family 9 identities. |
| [`20260604-36C-USDA-families`](data/20260604-36C-USDA-families/) | 36 C | 1, 2, 3, 5, 6, 8, 9, 10 | 38 | 36 (94.7%) | Data are in wide format and are standardized in the cross-experiment summary. |
| [`20260609-36C-USDA-families`](data/20260609-36C-USDA-families/) | 36 C | 1, 3, 5, 6, 7, 8, 9, 9B, 10 | 99 | 81 (81.8%) | 12-well plate heat challenge at 36 C. Groups labelled `9` and `9B` represent unresolved family 2/family 9 identities. |

Across the four currently summarized experiments, the combined standardized data
set contains 373 individuals and 287 observed deaths (76.9%).

## Primary findings to date

- Survival differs strongly among experiments. The least severe experiment by
  death percentage was 20260427 at 33 C (49.2% deaths), while the most severe
  was 20260511 at 33 C (99.1% deaths).
- The two 36 C experiments produced high mortality, but differed in scale and
  timing: 20260604 had 94.7% deaths among 38 individuals, while 20260609 had
  81.8% deaths among 99 individuals.
- Family-level patterns are present but should be interpreted within each
  experiment because temperature, container format, prior handling, time-point
  schedules, and family coverage differ among experiments.
- Families 1, 3, 5, 6, 8, and 10 are clearly represented across all four
  summarized experiments. Family 7 is absent from 20260604, and family 2 can be
  interpreted directly only in 20260427 and 20260604.
- In 20260511 and 20260609, two groups were originally labelled family 9. One is
  expected to be family 2 and one family 9, but the true identities are
  unresolved, so they are retained as observed groups `9` and `9B`.
- Size data are currently available only for 20260427. In that experiment, all
  128 oysters have ImageJ area measurements, with mean area 779.3 mm2 and median
  area 742.3 mm2.
- The current resazurin-survival comparison suggests late-phase resazurin signal
  is associated with poorer subsequent heat survival. In the date-matched
  20260604 36 C comparison, `corrected_fc` `delta_auc_late_minus_early` had
  Spearman rho = 0.791 with death percent and rho = -0.756 with median survival
  time. This should be treated as descriptive and hypothesis-generating, not as
  a validated predictive model.

## Key outputs

- Cross-experiment analysis:
  [`code/05-mgig-heat-survivorship-experiment-summary.qmd`](code/05-mgig-heat-survivorship-experiment-summary.qmd)
- Rendered cross-experiment report:
  [`code/05-mgig-heat-survivorship-experiment-summary.html`](code/05-mgig-heat-survivorship-experiment-summary.html)
- Standardized individual-level survival table:
  [`outputs/05-mgig-heat-survivorship-experiment-summary/standardized_survival_data.csv`](outputs/05-mgig-heat-survivorship-experiment-summary/standardized_survival_data.csv)
- Family-level survival summary:
  [`outputs/05-mgig-heat-survivorship-experiment-summary/family_survival_summary.csv`](outputs/05-mgig-heat-survivorship-experiment-summary/family_survival_summary.csv)
- Survival plots:
  [`survival_by_experiment.png`](outputs/05-mgig-heat-survivorship-experiment-summary/survival_by_experiment.png),
  [`survival_by_temperature.png`](outputs/05-mgig-heat-survivorship-experiment-summary/survival_by_temperature.png),
  and
  [`survival_by_family_faceted_by_experiment.png`](outputs/05-mgig-heat-survivorship-experiment-summary/survival_by_family_faceted_by_experiment.png)
- Resazurin-survival predictor summary:
  [`outputs/05-mgig-heat-survivorship-experiment-summary/resazurin_survival_predictor_summary.md`](outputs/05-mgig-heat-survivorship-experiment-summary/resazurin_survival_predictor_summary.md)

## Adding a new experiment

Use a sequential number prefix for new analysis code and the corresponding
output directory. The prefix should be the next unused number in [`code/`](code/)
and [`outputs/`](outputs/), followed by a short descriptive name.

Example:

- Code:
  `code/06-mgig-heat-survivorship-YYYYMMDD.qmd`
- Output directory:
  `outputs/06-mgig-heat-survivorship-YYYYMMDD/`

This keeps code and outputs paired and preserves the order in which analyses
were added to the project.

### Data folder setup

Create a dated experiment folder under [`data/`](data/) using the pattern:

`data/YYYYMMDD-temperatureC-description/`

Recommended files:

- `README.md`: short description of the experiment, including species, family or
  treatment groups, heat-stress temperature, container format, seawater volume or
  salinity if relevant, start conditions, observation schedule, and any handling
  notes.
- `survivorship.csv`: curated mortality/survival data used for analysis.
- `metadata.csv`: optional individual, plate, size, treatment, or image-derived
  metadata.
- `images/`: optional photographs or ImageJ measurement images.

For compatibility with the existing cross-experiment summary, record enough
information to identify each individual, family or treatment group, time point,
and alive/dead state.

The current directory contains both long- and wide-format survivorship files.
Either can be analyzed, but choose one format deliberately and document it in the
experiment README.

Long-format `survivorship.csv` pattern:

| Column | Description |
|---|---|
| `plate_ID` | Plate or container identifier |
| `plate_well` | Well or position identifier |
| `individual_id` | Unique oyster ID |
| `familly_id.group` | Family or observed group label; keep this spelling if using the existing cross-experiment parser |
| `is_blank` | `TRUE` for blank/control wells and `FALSE` for oyster wells |
| `timepoint_count` | Sequential time-point number, with 0 as the start |
| `timepoint_hrs` | Elapsed time in hours |
| `alive.measurement` | `TRUE` if alive and `FALSE` if dead |
| `date` | Assessment date |
| `time` | Assessment time |
| `notes` | Optional observations |

Wide-format `survivorship.csv` pattern, used by the Kaplan-Meier template:

- Each row is one individual.
- Include one or more grouping columns, such as `Family`,
  `Hardening_Treatment`, `Treatment`, or `Tank`.
- Include an individual ID column, usually `ID`.
- Use one numeric column name per time point, such as `0`, `6`, `12`, `24`, or
  `42`.
- Use `0` for alive and `1` for dead at each time point. Once an individual is
  dead, later time points should remain `1` or be left blank if censoring is
  handled explicitly in the analysis.

### Using the Kaplan-Meier template

For a new single-experiment survival analysis, start from
[`code/template_kaplan_meier_survival.qmd`](code/template_kaplan_meier_survival.qmd).

1. Copy the template to a new sequentially numbered file in [`code/`](code/),
   such as `06-mgig-heat-survivorship-YYYYMMDD.qmd`.
2. Set `data_path` to the new `survivorship.csv`.
3. Set `group_vars` to the grouping columns to compare, such as `Family` or
   `c("Hardening_Treatment", "Family")`.
4. Set `id_var` to the individual identifier column.
5. Update plot labels, colors, and the take-home interpretation.
6. Save figures and rendered reports to the matching sequential output
   directory under [`outputs/`](outputs/).

After the individual analysis is complete, update
[`code/05-mgig-heat-survivorship-experiment-summary.qmd`](code/05-mgig-heat-survivorship-experiment-summary.qmd)
so the new experiment is included in the standardized survival table, summary
CSVs, plots, and this README.

Cross-experiment comparisons should remain descriptive unless the experiments
are designed with comparable protocols, common family identities, and consistent
time-point schedules.
