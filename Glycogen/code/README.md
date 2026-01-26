`sormi-assay-development/edit/main/Glycogen/code`

Code directory for SORMI glycogen assays.

All file names should adhere to this format:

- `Gen5-YYYYMMDD-<species_abbrevation>-<experiment_description>.Rmd`

    - For `<species_abbrevation>`, use first letter of genus and first three letters of species. E.g. `mgig` for *M. gigas*.

- `.Rmd`: Most code is anticipated to be run using R Markdown. However, other formats are fine (e.g. Jupyter Notebooks).

This naming convention keeps with the format used for plate reader experiment files in `../data/plate_reader_files` and allows for easy identification of which code corresponds to which experiment.

All outputs from a script should be directed to a corresponding directory with the same name (excluding the suffix) in the `../ouput` directory.

---