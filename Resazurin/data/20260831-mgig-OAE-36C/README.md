`Resazurin/data/20260831-mgig-OAE-36C`

## DESCRIPTION

Juvenile oysters were subjecct to OAE conditions (10<sup>o</sup>C) for 6hrs on 20260828. After exposure, oysters were transferred to standard sea water conditions. On 20260831, 46 OAE oysters and 46 control oysters were subjected to 36<sup>o</sup>C heat stress. Oysters were randomly distributed across four, 24-well clear plates and submerged in 2mL of resazurin working solution. Fluorescence was measured every 30mins in a Synergy HTX (Agilent) plate reader.

Resazurin working-solution temperatures were recorded by the plate reader at each read (mean across the four plates):

| Timepoint (hrs) | Temp (C) |
|-----------------|----------|
| 0               | 22.2     |
| 0.5             | 23.0     |
| 1               | 23.7     |
| 1.5             | 24.4     |
| 2               | 24.7     |
| 2.5             | 25.1     |
| 3               | 25.5     |

See `Resazurin/code/01.00-resazurin-20260831-mgig-OAE-36C.Rmd` for the full analysis.

---

## FILES

- `layout.csv` — Sample layout mapping each well across the four plates to treatment group (`OAE` vs. `CONTROL`), sample ID, blank flag, and ImageJ-derived area measurement (mm²). 96 wells total: 46 OAE, 46 control, 4 blanks.

- `plate-[A-D]-T[0-3].[0,5].txt` — Raw fluorescence output from the Synergy HTX plate reader for the given plate (A-D) at the given timepoint (T0.0 - T3.0, every 30 mins). Tab-delimited export including instrument metadata and a 24-well plate fluorescence grid.

- `Experiment1.xpt` — Native Gen5 experiment project file exported by the Synergy HTX plate reader software. Not parsed directly by the analysis code; retained as the source file for the `plate-*-T*.txt` exports above.

- `OAE-01-ImageJ.csv` — Oyster surface area (mm²) measured in ImageJ for all 46 OAE-treatment individuals, linked to sample IDs.

- `controls-01-ImageJ.csv`, `controls-02-ImageJ.csv` — Oyster surface area (mm²) measured in ImageJ for control-treatment individuals, linked to sample IDs. Control individuals were photographed in two batches; `controls-02-ImageJ.csv` contains the single individual (`C-27`) measured in the second batch.