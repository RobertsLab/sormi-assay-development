`sormi-assay-development/Resazurin`

Resazurin experiments directory.


### How to use

Subdirectories created here can be automatically parsed and have the data plotted, as long as the directories contain the following:

1. Exported plate reader files with filenames like: `plate-<label>-T<hour>.txt`

2. A `layout.txt` file. See the `example-layout.txt` file in this directory for an example.

Visit https://sormi.science/resazurin-dashboard.html to view plots.

---

- [`example-layout.txt`](https://github.com/RobertsLab/sormi-assay-development/blob/main/Resazurin/example-layout.txt): This is an example/template file for use with the automatic plotting of data found in experiment subdirectories.

    - `plate_ID`: This should match the `plate-<label>` portion of the plate reader filename.
    - `is_blank`: This should be either `TRUE` or `FALSE`.
    - `*.group`: These columns represent groupings you'd like to have. You can add additional column names, as long as they end with `.group`.
    - `*.measurement`: These columns allow for normalization of different organism measurements. You an add additional columns as long as they end with `.measurement`.
    - `imageJ_ID`: Used if organism area (or other measurements) were calculated with ImageJ and will allow for easy data joining of ImageJ data to the rest of the experiment data.