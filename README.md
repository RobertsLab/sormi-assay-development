# sormi-assay-development

## [Summer Oyster Resilience & Mortality Index (SORMI) — A quantitative tool for improving field survival of Pacific oysters](https://sormi.science/)

Associated [Google Drive Folder](https://drive.google.com/drive/folders/1aNnGCrB0ZZ6RrqaYGUDaaSHQTRL5pYW1)

---

### OVERVIEW

This repository is the canonical landing point for SORMI assay-development work with Pacific oysters (_Magallana gigas_). It contains the durable computational record: assay data, analysis code, protocols, generated outputs, dashboard source, and concise pointers to related planning materials in Google Drive.

Google Drive remains useful for collaborative planning documents, native Google Docs/Sheets, protocol PDFs, and literature collections. When Drive contains material that is not mirrored here, the difference should be visible from this README or from the relevant assay subdirectory README.

### CANONICAL RECORD PLAN

1. Keep reproducible work in the repo: raw assay exports, layout/metadata files, scripts, notebooks, rendered summaries, and finalized protocol/reference files.
2. Keep collaborative drafting in Google Drive: planning docs, outreach docs, native Google Sheets, brainstorming notes, and literature folders that are not direct analysis inputs.
3. Mirror only stable Drive artifacts into the repo when they become part of the assay record.
4. Link Drive-only content from this README so users can tell what exists outside the repo without browsing Drive first.
5. Use each assay directory README to state whether Drive has additional protocol/planning/literature content.

### REPOSITORY STRUCTURE

| Path | What is included |
| --- | --- |
| [`Glycogen/`](Glycogen/) | Development and analysis files for the Promega Glycogen-Glo assay. Includes R Markdown analyses, raw luminescence exports, Gen5 plate-reader experiment files, sample/tissue-weight CSVs, reference PDFs, and generated plots. |
| [`Resazurin/`](Resazurin/) | Resazurin metabolic-rate assay experiments. Includes experiment-specific plate-reader exports (`plate-<label>-T<hour>.txt`), `layout.csv` metadata files, ImageJ measurements/images, R Markdown analyses, summary CSV outputs, figures, an example layout template, and the Huffmyer et al. preprint. |
| [`heat-survivorship/`](heat-survivorship/) | Heat survivorship experiment data and analyses. Includes survivorship CSVs, family/image metadata, R Markdown analysis files, and Kaplan-Meier plot outputs. |
| [`NaK-ATPase/`](NaK-ATPase/) | Na/K ATPase assay protocol material, currently including the sampling protocol PDF. |
| [`Protein assay/`](Protein%20assay/) | Landing directory for protein-assay protocol notes and future assay data. Current detailed protocol/manual materials are in Google Drive. |
| [`Total Antioxidant Capacity/`](Total%20Antioxidant%20Capacity/) | Landing directory for total antioxidant capacity assay development. Current kit documentation and question notes are in Google Drive. |
| [`Succinate/`](Succinate/) | Landing directory for succinate assay development. Current kit documentation is in Google Drive. |
| [`sampling-event-metadata/`](sampling-event-metadata/) | Sampling-event metadata and images, including June 2026 USDA family sampling, ImageJ sizing measurements, sizing images, and sex-determination image folders. |
| [`scripts/resazurin/`](scripts/resazurin/) | Shared R scripts for parsing resazurin plate-reader exports and building dashboard-ready data and QC files. |
| [`.github/workflows/`](.github/workflows/) | GitHub Actions workflow for rebuilding the resazurin dashboard data, rendering the Quarto dashboard, and deploying GitHub Pages from `main`. |

### GOOGLE DRIVE DIFFERENCES

The linked [Google Drive folder](https://drive.google.com/drive/folders/1aNnGCrB0ZZ6RrqaYGUDaaSHQTRL5pYW1) is broader and more planning-oriented than this repo.

| Drive item | Repo status |
| --- | --- |
| `Sampling Plan May 2026`, `SORMI Summary`, and `Engagement Outreach Tracking` | Drive-only planning/admin documents. Summaries or finalized methods should be copied into repo READMEs when they become part of the durable record. |
| [`Assay List`](https://docs.google.com/spreadsheets/d/1GBMtLY4aFRvle37hthge4n-eMNNRA2LS01NZbu8G8Gs/edit) Google Sheet | Canonical editable assay list. The repo no longer keeps a separate `Assay List.xlsx` copy. |
| `Glycogen` Drive folder | Contains protocol/reference PDFs that are also represented in [`Glycogen/`](Glycogen/). The repo additionally contains raw data, code, and outputs. |
| `Resazurin` Drive folder | Contains the Huffmyer preprint, also represented in [`Resazurin/`](Resazurin/). The repo additionally contains raw data, code, images, and outputs. |
| `Na/K ATPase` Drive folder | Contains the sampling protocol PDF, also represented in [`NaK-ATPase/`](NaK-ATPase/). |
| `Protein assay`, `Total Antioxidant Capacity`, and `Succinate` Drive folders | Drive currently holds protocol/manual/question documents. Matching repo landing directories now exist for future canonical data and analysis. |
| `Reproductive investment / Sex determination` Drive folder | Drive holds brainstorming and literature PDFs. Related measured sampling metadata lives under [`sampling-event-metadata/`](sampling-event-metadata/). |
| `photos` Drive folder | Drive-only small photo collection, separate from repo experiment image datasets. |

### KEY ROOT FILES

- [`resazurin-dashboard.qmd`](resazurin-dashboard.qmd): Quarto source for the [resazurin data dashboard](https://sormi.science/resazurin-dashboard.html).
- [`index.html`](index.html): Static site landing page copied into the GitHub Pages build.
- [`sormi-assay-development.Rproj`](sormi-assay-development.Rproj): RStudio project file for the repository.

### ORGANIZATION NOTES

- Assay folders generally use `code/`, `data/`, and `outputs/` or `output/` subdirectories.
- R Markdown files in assay `code/` folders contain analysis workflows; rendered `.md`/`.html` files and plots are stored alongside or under matching output folders.
- Raw instrument exports and layout files are kept with each assay's `data/` directory so analyses can be rerun from source data.
- Resazurin experiment folders are designed to be parsed automatically when they contain plate-reader files named like `plate-<label>-T<hour>.txt` and a matching `layout.csv`.
- Glycogen Gen5 experiment and raw luminescence files follow date/species/experiment naming conventions documented in the Glycogen subdirectory READMEs.

### ASSAY LIST

Canonical editable source: [`Assay List`](https://docs.google.com/spreadsheets/d/1GBMtLY4aFRvle37hthge4n-eMNNRA2LS01NZbu8G8Gs/edit) in Google Drive.

| assay | live animal? | assay format | target tissue | sample prep | storage | protocol | citations | notes relevant to phenotyping performance |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Glycogen | N | 96-well plate | mantle (gonad adjacent) | snap frozen? | ? | GitHub for protocols? | Berthelin et al (2000) | mantle adj. to gonad highest levels; glygocen depleted during gametogenesis |
| Na/K ATPase | N | 96-well plate | gill | SEI buffer, on ice then -80 | -80 up to 6 months |  | George et al 2023 | gill and mantle tested, gill worked best, stressed triploids elevated ATPase |
| Resazurin | Y | lots of sizes | whole animal; gill tissue\* | live tissue/animal | na | https://robertslab.github.io/resources/protocols/resazurin-assay/ | Huffmyer et al bioRxiv | \* gill testing in progress as of Nov 25 |
| Citrate Synthase | N | 96-well plate |  | snap frozen? |  |  |  |  |
| Succinate | N | 96-well plate |  | snap frozen? |  |  |  |  |
| Mitochondrial flow cytometer assays | Y | individual samples | hemolymph | Guava Cytek Muse kits | na |  |  | testing in progress |
