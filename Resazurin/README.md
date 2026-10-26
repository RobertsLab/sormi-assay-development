`sormi-assay-development/Resazurin`

Resazurin experiments directory.

Assays were conducted following the [Roberts Lab resazurin assay protocol](https://robertslab.github.io/resources/protocols/resazurin-assay/), which is also detailed in "From Blue to Pink: Resazurin as a High-Throughput Proxy for Metabolic Rate in Oysters", Huffmyer et al. 2025: https://doi.org/10.1101/2025.11.06.686367.


## How to use

Subdirectories created here can be automatically parsed and have the data plotted, as long as the directories contain the following:

1. Exported plate reader files with filenames like: `plate-<label>-T<hour>.txt`

2. A `layout.csv` file. See the `example-layout.csv` file in this directory for an example.

Visit https://sormi.science/resazurin-dashboard.html to view plots.

### Timepoint designs

The analysis Rmd scripts (`code/01.00-resazurin-*.Rmd`) auto-detect which of two designs a `layout.csv` uses. Only one applies per experiment.

**1. One export file per plate per timepoint (most common).** Each well is physically re-read at every hour, so the plate + well pair alone identifies an individual across time. The timepoint comes from the plate reader filename, e.g. `plate-D-T0.txt`, `plate-D-T1.txt`. `sample_ID.group` is optional here.

**2. One export file per plate, several timepoints pooled onto the same plate.** This is the case when, e.g., a single 96-well plate holds reads from the *same individuals* taken at different hours, each hour's sample landing in a different well (so each well is used once). Because `plate_ID` + `plate_well` can't span time here, the layout needs two extra things:

- a `timepoint` column (also accepted: `time_point`, `timepoint_hr`, `time_hr`, `time_hrs`, `time_h`, `hours`, `hour`, `time`) giving the hour for *that specific well*. Accepted formats: bare numbers (`0`, `1.5`), `T`-prefixed (`T0`, `T1.5`), or unit-suffixed (`4h`, `90min`).
- `sample_ID.group` populated and **required** — the same value for every well belonging to the same individual, across all of its timepoints. It's the only thing linking those wells together, and the scripts stop with an explanatory error if it's missing.

Example: individual `56` sampled at T0 into well A01 and at T1 into well B01, both on the same `plate-D.txt`:

```csv
plate_ID,plate_well,is_blank,sample_ID.group,timepoint,treatment.group
plate-D,A01,FALSE,56,T0,control
plate-D,B01,FALSE,56,T1,control
```

If a `timepoint` column is present in the layout, it always takes precedence over any timepoint parsed from the filename.

---


### FILES

- [`example-layout.csv`](https://github.com/RobertsLab/sormi-assay-development/blob/main/Resazurin/example-layout.csv): This is an example/template file for use with the automatic plotting of data found in experiment subdirectories.

    - `plate_ID`: This should match the `plate-<label>` portion of the plate reader filename.
    - `is_blank`: This should be either `TRUE` or `FALSE`.
    - `*.group`: These columns represent groupings you'd like to have. You can add additional column names, as long as they end with `.group`.
    - `*.measurement`: These columns allow for normalization of different organism measurements. You can add additional columns as long as they end with `.measurement`.
    - `imageJ_ID`: Used if organism area (or other measurements) were calculated with ImageJ and will allow for easy data joining of ImageJ data to the rest of the experiment data.

- [`BIORXIV-2025-686367v1-Huffmyer.pdf`](https://github.com/RobertsLab/sormi-assay-development/blob/main/Resazurin/BIORXIV-2025-686367v1-Huffmyer.pdf): Preprint version of "From Blue to Pink: Resazurin as a High-Throughput Proxy for Metabolic Rate in Oysters", Huffmyer et al. 2025: https://doi.org/10.1101/2025.11.06.686367
