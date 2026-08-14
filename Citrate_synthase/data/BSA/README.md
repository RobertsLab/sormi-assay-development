`sormi-assay-development/edit/main/Citrate_synthase/data/BSA`

Raw plate-reader data for BSA protein-quantification (BCA/Bradford-style,
595&nbsp;nm) standard-curve assays, used to determine total protein
concentration in SoRMI samples ahead of citrate synthase activity assays.
Analysis code that consumes this data lives in `../../code`.

---

## Directories

- [`plate_reader_files/`](plate_reader_files/README.md): Native Agilent
  Gen5 experiment files (`.xpt`).

- [`raw_absorbance/`](raw_absorbance/README.md): Per-well absorbance/
  concentration CSV exports from Gen5, one file per plate. These are the
  files read directly by the `.Rmd` scripts in `../../code`.

---
