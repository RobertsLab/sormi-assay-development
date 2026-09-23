`Resazurin/data/20260921-mgig-sormi-multi_stress`

## DESCRIPTION

On 20260921, juvenile *Magallana gigas* from a single family were assigned to six
distinct stress conditions, one 12-well plate per stress (plates `C`, `R`, `S`,
`T`, `U`, `V`; 12 oysters per plate, one per well, 72 total). There is no
control plate. Oysters were submerged in resazurin working solution and
fluorescence was read in a Synergy HTX (Agilent) at 0, 0.25, 0.5, 0.75, 1, 2 and
18.75 h. No blank wells were included.

Resazurin working-solution temperatures recorded by the plate reader (mean
across the six plates):

| Timepoint (hrs) | Temp (C) |
|-----------------|----------|
| 0               | 20.5     |
| 2               | 24.8     |
| 18.75           | 25.5     |

The full per-read table is in
`Resazurin/outputs/05.00-resazurin-20260921-mgig-multi_stress/plate_temperatures.csv`.

See `Resazurin/code/05.00-resazurin-20260921-mgig-multi_stress.Rmd` and
`Resazurin/code/05.1-resazurin-multi_stress-curve-features-prediction.Rmd` for
the analysis.

---

## FILES

- `layout.csv`: well metadata for all 72 wells. `treatment.group` is currently
  the plate letter (one stress per plate); replace it with stress names when
  available. `area_mm2.measurement` is shell surface area measured from the
  plate photos (see below). `area_note` flags wells with uncertain outlines.

- `plate-[C,R-V]-T*.txt`: raw fluorescence exports (12-well plate grid) for
  each plate at each timepoint. The final read is named `T18.75.0` for most
  plates and `T18.75` for plate V; the analysis parser accepts both.

- `images/plate-*.jpg`: photo of each plate with a mm ruler, used for area
  measurement.

- `Experiment1.xpt`: native Gen5 experiment file (not parsed directly).

## AREA MEASUREMENT

Areas were measured automatically by
`Resazurin/code/05.0-measure-oyster-area-20260921.py`:

1. pixel/mm scale from the periodicity of the ruler's mm ticks (~20–22 px/mm),
2. 3 × 4 well grid fit to the photo (26 mm SBS pitch),
3. per-well oyster segmentation (darkness + brown tint vs that well's
   background, Otsu-based threshold, rim arcs removed).

QC overlays are in `Resazurin/outputs/05.0-measure-oyster-area-20260921/overlays/`.

Known limitations:

- Plate S was photographed with pink resazurin already in the wells, so the
  S-A03 outline is uncertain.
- Photo well orientation is assumed to match the reader (A01 = top-left as
  photographed, notched corner at top-right).
- Areas are projected 2-D shell area, measured at the ruler's scale.
