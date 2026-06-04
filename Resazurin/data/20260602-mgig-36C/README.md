`Resazurin/data/20260602-mgig-36C`

### Description

_M.gigas_ oysters from nine USDA families were placed in clear, 12-well plates and submerged in 4mL of freshly prepared resazurin working solution to assess temperature stress responses. Plates were incubated at 36<sup>o</sup>C for the duration of the experiment.

Fluorescence was measured periodically using a Synergy HTX (Agilent) plate reader with the Gen5 software (Agilent).

Plate temp measurements:

| time(hrs) | temp_range(C) |
|-----------|---------------|
| 0         | 21            |
| 0.5       | 31-32         |
| 1         | 33-34         |
| 1.5       | 35-37         |
| 2         | 35-37         |
| 3         | 35-37         |
| 4         | 35-37         |

---

### Files

- `*-ImageJ.csv`: Area measurements calculated with ImageJ. A corresponding image of the plate can be viewed in the `images` directory.

- `layout.csv`: A metadata file containing plate layouts, sample IDs, oyster measurements, etc.

- `plate-[A-Z]-T[0-9].[0-9].txt`: Data files from the plate reader. The `T[0-9].[0-9]` represents the time, in hours, after the experiment began.