`Resazurin/data/20260604-mgig-36C`

## Description

Juvenile oysters from nine USDA families were submerged in 4 mL of
resazurin working solution, prepared using Instant Ocean (10<sup>o</sup>C in 12-well plates and held at 36°C. At each
designated timepoint, fluorescence was measured using a Synergy HTX (Agilent)
plate reader.


| time(hrs) | temp_range(C) |
|-----------|---------------|
| 0         | 15            |
| 0.5       | 30.5          |
| 1         | 33-35         |
| 1.5       | 35-36         |
| 2         | 35-37         |
| 4         | 35-37         |

---

## Files

- `layout.csv` — Sample layout mapping each plate well to family ID, sample ID, treatment group, physical measurements (weight, width, length, area in mm²), and ImageJ ID. One row per well across all plates (B, C, D, E, F, H, I, J, K).

- `plate-[ID]-T[time].txt` — Raw fluorescence output from the Synergy HTX plate reader for the given plate at the given timepoint (0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0 hrs). Tab-delimited export including instrument metadata and a 12-well plate fluorescence grid.

- `plate-[ID]-ImageJ.csv` — Oyster surface area (mm²) measured in ImageJ for each well of the given plate, linked to sample IDs.
