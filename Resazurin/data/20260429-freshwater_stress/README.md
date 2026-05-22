`sormi-assay-development/Resazurin/20260429-freshwater_stress`

### Description

_M.gigas_ oysters from nine USDA families were placed in clear, 12-well plates and submerged in 4mL of resazurin working solution, _made with **TAP WATER** in an attempt to induce, and assess, a freshwater stress response. Plates were left at room temperature (20<sup>o</sup>C) for the duration of the experiment.

Fluorescence was measured periodically using a Synergy HTX (Agilent) plate reader with the Gen5 software (Agilent).

---

See the data dashboard for plots of data:

https://sormi.science/resazurin-dashboard.html


### IMPORTANT

This directory is automatically parsed by GitHub Actions to produce the data dashboard. Please do not alter the plate filenames.

---


### FILES

- `layout.csv`: Metadata file containing all pertinent info on plate layouts, sample IDs, family/group IDs, etc., along with any oyster physical measurements.

- `*-ImageJ.csv`: Comma-separate area measurments of oysters. The file name corresponds to an image file of the same name in the `images` directory.

- `plate-<A-Z>-T<N.N>.txt`: Raw fluorescence data from the plate reader. 

  - `<A-Z>`: Corresponds to plate letter.

  - `T<N.N>`: Corresponds to the measurment timepoint in `N` hours, `N` minutes.
