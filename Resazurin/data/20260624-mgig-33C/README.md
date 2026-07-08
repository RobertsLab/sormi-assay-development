`Resazurin/data/20260624-mgig-33C`

## DESCRIPTION

Adult oysters from nine USDA families were gently cleaned of barnacles using a flexible metal spatula. Oysters with no barnacles were _also_ gently scraped so all oysters received the same amount of handling. Oysters were placed in plastic cups and submerged in ~100 mL of freshly prepared resazurin working solution at room temperature (~22<sup>o</sup>C). Cups were incubated in a water table at 33<sup>o</sup>C. Three rounds of measurements were performed, with different individuals from each family used in each round. At designated timepoints, 200 µL of resazurin was sampled from each cup and placed in a 96-well plate. Fluorescence measurements were taken with a Synergy HTX (Agilent) plate reader.

---

## FILES

- `layout.csv` — Sample layout mapping each 96-well plate well to family ID, cup ID, round, sample ID, treatment group, physical measurements (weight, width, length, area in mm²), and ImageJ ID. One row per well across all three rounds.

- `20260624_round[1-3]_T[0-3].txt` — Raw fluorescence output from the Synergy HTX plate reader for the given round at the given timepoint (T0, T1, T2, T3). Tab-delimited export including instrument metadata and a 96-well plate fluorescence grid.

- `family-[ID]-[rep]-ImageJ.csv` — Oyster surface area (mm²) measured in ImageJ for each individual within the given family and replicate, linked to sample IDs. Families 01, 02, 03, 05, 06, 07, 08, 09, 10 each have three replicate files (01, 02, 03).
