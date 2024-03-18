# src

The analyses were performed on R version 4.3.2 (2023-10-31) and RStudio version 2023.12.1+402.

Click `src.Rproj` to open an RStudio session and R project/workspace. Then run the code (`.Rmd` or `.R` files) from that R session/project to rerun the analyses in the manuscript. `utils.R` contains utility functions. The code reads data from `../data` and saves data/output to other directories (see parent directory and `README.md` for details).

Essential R libraries used in the analyses include

- `data.table` 1.15.0
- `tidyverse` 2.0.0
- `fixest` 0.11.2 (for fixed-effects models)
- `meta` 7.0.0 (for meta-analysis)
- `glue` 1.7.0
- `patchwork` 1.2.0

Installation time for these libraries should be less than a few minutes Expected run time should be under a minute. Results/output will be shown in the RStudio console. 
