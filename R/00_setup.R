# ============================================================
# 00 SETUP: PACKAGES, PATHS, AND HELPER FUNCTIONS
# ============================================================

# -----------------------------
# Required packages
# -----------------------------
required_packages <- c(
  "data.table",
  "dplyr",
  "tidyr",
  "car",
  "corrplot",
  "broom",
  "stringr",
  "ggplot2",
  "ggeffects",
  "patchwork",
  "ggcorrplot",
  "MuMIn",
  "DHARMa",
  "FactoMineR",
  "factoextra"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages before running the scripts:\n",
    paste(missing_packages, collapse = ", "),
    "\n\nUse:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

# -----------------------------
# Project paths
# -----------------------------
data_dir <- "data"
output_dir <- "outputs"
figure_dir <- file.path(output_dir, "figures")
table_dir <- file.path(output_dir, "tables")

main_fig_dir <- file.path(figure_dir, "main")
supp_fig_dir <- file.path(figure_dir, "supplementary")
diag_fig_dir <- file.path(figure_dir, "diagnostics")

dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(main_fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(supp_fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(diag_fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

main_data_path <- file.path(data_dir, "model1_distance.csv")
cluster_data_path <- file.path(data_dir, "cluster_assignments.csv")

# -----------------------------
# Shared variables
# -----------------------------
num_vars <- c(
  "Fishing.experience",
  "vessel.length",
  "Income.per.trip",
  "cost.per.trip"
)

cat_vars <- c(
  "AIS",
  "VMS",
  "navigation.apps",
  "target.sharks",
  "subsidy",
  "Site",
  "reg.awareness",
  "Bycatch",
  "communication.with.fishers",
  "Top.species.group",
  "gear.type"
)

# Candidate predictors for initial all-combinations model selection.
# This follows the original script's intent of excluding gear.type and cost.per.trip here.
candidate_predictors <- c(
  "Fishing.experience_std",
  "vessel.length_std",
  "Income.per.trip_std",
  "AIS",
  "VMS",
  "navigation.apps",
  "target.sharks",
  "subsidy",
  "Site",
  "reg.awareness",
  "Bycatch",
  "communication.with.fishers",
  "Top.species.group"
)

# Predictors used in the final model-averaging and DHARMa diagnostic sections.
final_predictors <- c(
  "cost.per.trip_std",
  "Fishing.experience_std",
  "vessel.length_std",
  "AIS",
  "VMS",
  "navigation.apps",
  "target.sharks",
  "subsidy",
  "Site",
  "reg.awareness",
  "communication.with.fishers",
  "Top.species.group"
)

# Variables used for the FAMD and cluster profile scripts.
cluster_active_vars <- c(
  "cost.per.trip",
  "Fishing.experience",
  "vessel.length",
  "AIS",
  "VMS",
  "navigation.apps",
  "target.sharks",
  "subsidy",
  "Site",
  "reg.awareness",
  "communication.with.fishers",
  "Top.species.group"
)

# -----------------------------
# Helper functions
# -----------------------------
check_file_exists <- function(path, purpose = "input data") {
  if (!file.exists(path)) {
    stop(
      "Missing ", purpose, ": ", path,
      "\nPlace the file in the correct folder and rerun the script.",
      call. = FALSE
    )
  }
}

scale_if_present <- function(df, input_var, output_var) {
  if (input_var %in% names(df)) {
    df[, (output_var) := as.numeric(scale(get(input_var)))]
  }
  df
}

check_required_columns <- function(df, required_cols, script_name = "this script") {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      script_name, " needs these missing columns:\n",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
}

clean_plot_theme <- theme_bw() +
  theme(
    panel.grid = element_blank(),
    text = element_text(size = 12),
    plot.title = element_text(face = "bold")
  )

cat("Setup complete. Paths and packages are ready.\n")
