# README

Repository for manuscript [Reducing misinformation sharing at scale using digital accuracy prompt ads](https://osf.io/preprints/psyarxiv/u8anb)

# Directories

- `data`: data files
- `figures`: figures
- `results`: results and output
- `src`: source code or scripts for analyses (performed on R version 4.5.2 2025-10-31)
	- All scripts assume that the working directory is the repository root (managed by `here` package)
	- `utils.R`: utility functions

Essential R libraries used in the analyses include

- `here` 1.0.2 (for file paths)
- `data.table` 1.15.0
- `tidyverse` 2.0.0
- `fixest` 0.11.2 (for fixed-effects models)
- `meta` 7.0.0 (for meta-analysis)
- `glue` 1.7.0
- `patchwork` 1.2.0

Installation time for these libraries should be less than a few minutes Expected run time should be under a minute.

# Citation

Lin, H., Garro, H., Wernerfelt, N., Shore, J. C., Hughes, A., Deisenroth, D., Barr, N., Berinsky, A., Eckles, D., Pennycook, G., & Rand, D. (2024). Reducing misinformation sharing at scale using digital accuracy prompt ads. *PsyArxiv*. https://doi.org/10.31234/osf.io/u8anb

```
@article{Lin2024Feb,
	author = {Lin, Hause and Garro, Haritz and Wernerfelt, Nils and Shore, Jesse Conan and Hughes, Adam and Deisenroth, Daniel and Barr, Nathaniel and Berinsky, Adam and Eckles, Dean and Pennycook, Gordon and Rand, David},
	title = {{Reducing misinformation sharing at scale using digital accuracy prompt ads}},
	journal = {PsyArXiv},
	year = {2024},
	month = feb,
	urldate = {2024-02-26},
	publisher = {OSF},
	doi = {10.31234/osf.io/u8anb},
	keywords = {accuracy prompts, fake news, interventions, misinformation, social media}
}
```

