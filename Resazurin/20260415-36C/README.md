`sormi-assay-development/Resazurin/20260415-36C`

On April 15th, 2026, adult oysters from 9 families were submerged in 85mL of room temperature resazurin working solution in clear, plastic cups. They were held at 36<sup>o</sup>C. At each designated timepoint (see `*-TN.N.txt` in plate data filenames for time point, in hours), 250uL was taken from each cup and put into a clear 96-well plate. Fluorescence was measured using a Synergy HTX plate reader (Agilent).

At some point !! NEEDS UPDATING !!, the temperature was increase to 40<sup>o</sup>C to speed up metabolism.

See the data dashboard for plots of data:

https://sormi.science/resazurin-dashboard.html


### IMPORTANT

This directory is automatically parsed by GitHub Actions to produce the data dashboard. Please do not alter the plate filenames.

---


### FILES

- [`layout.txt`](https://github.com/RobertsLab/sormi-assay-development/blob/main/Resazurin/20260415-36C/layout.txt): This is a tab-delimited plate layout file containing various metadata which is automatically parsed/processed to create the data dashboard.

- `plate-<label>-T<N.N>.txt`: Plate reader files. The filename formatting is important, as they are utilized to help generate the data dashboard. The `*-T<N.N>` portion is the time, in hours, the measurement was taken after time = 0 (e.g. `T0.0`).