# Understanding fishing site choice and distribution of deep-sea fisheries in India — Supplementary files

This repository contains R scripts, documentation files, figures, and supplementary summary tables used for analysing fishing site choice and the spatial distribution of deep-sea fisheries in India through participatory methods.

The analysis focuses on fishing distance, vessel characteristics, technology use, regulatory awareness, model selection, relative variable importance (RVI), model diagnostics, FAMD-based clustering, and conditional effects related to AIS use.

## Folder structure

```text
Github_2nd_Chapter/
├── README.md
├── .gitignore
├── data/
│   └── README.md
├── docs/
│   ├── coding_framework.md
│   └── interview_questionnaire.md
├── outputs/
│   ├── figures/
│   └── tables/
└── R/
    ├── 00_setup.R
    ├── 01_load_clean.R
    ├── 02_diagnostics.R
    ├── 03_model_selection_rvi.R
    ├── 04_main_figures.R
    ├── 05_supplementary_figures.R
    ├── 06_model_averaging_dharma.R
    ├── 07_famd_cluster_profiles.R
    └── 08_ais_paradox.R
```

## Repository contents

- `R/` contains the analysis scripts.
- `docs/` contains supporting documentation, including the interview questionnaire and coding framework.
- `data/` is a placeholder for local input data. Raw data are not included in this repository.
- `outputs/figures/` contains generated figures.
- `outputs/tables/` contains selected machine-readable supplementary summary tables.

## Data files required

The scripts require the following input files, which should be placed locally inside the `data/` folder:

1. `model1_distance.csv`  
   Required for data cleaning, diagnostics, model selection, RVI calculation, and figure generation.

2. `cluster_assignments.csv`  
   Required for the FAMD scree plot, cluster profile table, and AIS conditional effects analysis.

These raw data files are not included in this repository because they may contain sensitive fisher-level information. Raw fisher-level data, GPS coordinates, harbour-identifiable information, and confidential survey responses should not be uploaded to GitHub unless fully anonymised and approved for sharing.

## How to run the scripts

Open the `Github_2nd_Chapter/` folder in RStudio and run the scripts in the following order:

```r
source("R/00_setup.R")
source("R/01_load_clean.R")
source("R/02_diagnostics.R")
source("R/03_model_selection_rvi.R")
source("R/04_main_figures.R")
source("R/05_supplementary_figures.R")
source("R/06_model_averaging_dharma.R")
source("R/07_famd_cluster_profiles.R")
source("R/08_ais_paradox.R")
```

## Outputs

Figures are saved in:

```text
outputs/figures/
```

Summary tables are saved in:

```text
outputs/tables/
```

The repository includes selected machine-readable supplementary summary tables and figures, but not the raw data used to generate them.

## Required R packages

The scripts use the following R packages:

```r
data.table
dplyr
tidyr
car
corrplot
broom
stringr
ggplot2
ggeffects
patchwork
ggcorrplot
MuMIn
DHARMa
FactoMineR
factoextra
```

Install missing packages before running the scripts. For example:

```r
install.packages(c(
  "data.table", "dplyr", "tidyr", "car", "corrplot", "broom",
  "stringr", "ggplot2", "ggeffects", "patchwork", "ggcorrplot",
  "MuMIn", "DHARMa", "FactoMineR", "factoextra"
))
```

## Notes

- The scripts use Gamma GLMs with a log link, so `mean_distance` must be strictly positive.
- The all-subsets model selection can take time when many predictors are included.
- The supplementary `.csv` files in `outputs/tables/` are derived summary tables, not raw fisher-level data.
- The `data/` folder should be used only for local input files and should not be used to upload confidential data to GitHub.

## Author

Abhishek Dudi
