`sormi-assay-development/edit/main/Citrate_synthase/data/BSA/raw_absorbance`

Per-well Gen5 CSV exports (595&nbsp;nm endpoint absorbance) for BSA
protein-quantification assays. Each file has one row per well: `Well ID`
and `Name` are populated only on the first row of each triplicate group
(sample or standard); the two replicate rows below carry only well-level
data. Standards are labeled `STD1`-`STD8` with a known concentration in
`Conc/Dil` (µg/mL); samples are labeled `SPL<N>`. These are the files read
directly by the corresponding `.Rmd` script in `../../../code`.

---

## Files

- [`Gen5-20260813-mgig-BSA-F05-absorbance.csv`](Gen5-20260813-mgig-BSA-F05-absorbance.csv):
  Plate `F05` -- 16 samples (`F05_01`-`F05_08`, ambient and 36&deg;C) plus
  an 8-point BSA standard curve, triplicate wells. Used by
  `../../../code/Gen5-20260813-mgig-sormi-BSA-F05-protein.Rmd`.

- [`Gen5-20260813-mgig-BSA-F07-absorbance.csv`](Gen5-20260813-mgig-BSA-F07-absorbance.csv):
  Plate `F07` -- same layout as `F05`, samples `F07_01`-`F07_08`. Used by
  `../../../code/Gen5-20260813-mgig-sormi-BSA-F07-protein.Rmd`.

- [`Gen5-20260813-mgig-BSA-F05-F07-reassay-absorbance.csv`](Gen5-20260813-mgig-BSA-F05-F07-reassay-absorbance.csv):
  Single re-assay plate -- 9 samples flagged for re-assay from the `F05`
  and `F07` analyses (5 from `F05`, 4 from `F07`), plus a fresh 8-point BSA
  standard curve. Sample names carry a `-df.<N>` dilution-factor suffix
  (`df.0` = undiluted, `df.N` = diluted N-fold). Used by
  `../../../code/Gen5-20260813-mgig-sormi-BSA-F05-F07-reassay-protein.Rmd`.

---
