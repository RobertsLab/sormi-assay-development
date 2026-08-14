`sormi-assay-development/edit/main/Citrate_synthase/data/BSA/plate_reader_files`

### Agilent Gen5 Plate Reader Experiment Files

### Please follow these naming conventions:

- `Gen5-YYYYMMDD-<species_abbreviation>-<experiment_description>.xpt`

    - For `<species_abbreviation>`, use first letter of genus and first three letters of species. E.g. `mgig` for *M. gigas*.

---

## Files

- [`Gen5-20260813-mgig-BSA-F05_F07.xpt`](Gen5-20260813-mgig-BSA-F05_F07.xpt):
  Native Agilent Gen5 experiment file (protocol + read settings) used for
  the 595&nbsp;nm BSA standard-curve reads of plates `F05` and `F07`.
  Corresponding per-well data exports are
  `../raw_absorbance/Gen5-20260813-mgig-BSA-F05-absorbance.csv` and
  `../raw_absorbance/Gen5-20260813-mgig-BSA-F07-absorbance.csv`.

- [`Gen5-20260813-mgig-BSA-F05_F07-reassay.xpt`](Gen5-20260813-mgig-BSA-F05_F07-reassay.xpt):
  Native Agilent Gen5 experiment file used for the re-assay plate carrying
  samples flagged for re-assay from the `F05`/`F07` analyses. Corresponding
  data export is
  `../raw_absorbance/Gen5-20260813-mgig-BSA-F05-F07-reassay-absorbance.csv`.

---
