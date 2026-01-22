`sormi-assay-development/edit/main/Glycogen/data/raw_luminescence/README.md`

# Raw Luminescence Data for Glycogen Assay Development

This directory contains raw luminescence data files and corresponding plate layout images and CSV files for glycogen assay development experiments. Each experiment is identified by the sample names and the date of the experiment.

## Filename Conventions

Files in this directory follow a specific naming convention to ensure clarity and organization:

- `<data_type>_<date>_<species>_<misc_info>.<extension>`


Where `<data_type>` should be one of the following:

- `full_report*`: Tab-delimited files containing all information regarding the luminescence readings.

- `layout*`: Comma-separated files containing the plate layout sample information. Sample info should be in the format of `<sample>-<assay_type>-<tissue_weight>`. Standards should be labeled as `STD-<assay_type>-<concentration>`.

- `raw_lum*`: Comma-separated files containing _only_ the raw luminescence readings from the plate reader. See the corresponding plate layout file for sample information.

`<date>`: Date of the experiment in `YYYYMMDD` format.

`<species>`: Species designation, e.g., `mgig` for _Magallana gigas_.

`<misc_info>`: Additional information about the experiment, such as assay type or specific conditions.

`<extension>`: File extension indicating the file type, such as `.txt` for tab-delimited files or `.csv` for comma-separated files.

## File Descriptions

- `*20260112-mgig-glycogen_glo-dilutions_test`: Initial dilution test using Sample A2 (50mg tissue weight).

- `*20260116-mgig-gylcogen_glo*`: Samples A3 and A4.
