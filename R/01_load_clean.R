# ============================================================
# 01 LOAD AND CLEAN DATA
# ============================================================

check_file_exists(main_data_path, "main dataset")

dt <- data.table::fread(main_data_path)
data.table::setnames(dt, make.names(names(dt)))

cat("RAW rows in CSV:", nrow(dt), "\n")

# -----------------------------
# Target cleaning
# -----------------------------
check_required_columns(dt, "mean_distance", "01_load_clean.R")

dt[, mean_distance := as.numeric(mean_distance)]

cat("Rows with non-missing mean_distance:", sum(!is.na(dt$mean_distance)), "\n")

dt <- dt[!is.na(mean_distance)]
cat("After dropping NA mean_distance:", nrow(dt), "\n")

dt <- dt[mean_distance > 0]
cat("After filtering mean_distance > 0:", nrow(dt), "\n")

# -----------------------------
# Variable type conversion
# -----------------------------
for (v in num_vars) {
  if (v %in% names(dt)) {
    dt[, (v) := as.numeric(get(v))]
  }
}

for (v in cat_vars) {
  if (v %in% names(dt)) {
    dt[, (v) := as.factor(get(v))]
  }
}

# -----------------------------
# Standardised numeric predictors
# -----------------------------
dt <- scale_if_present(dt, "Fishing.experience", "Fishing.experience_std")
dt <- scale_if_present(dt, "vessel.length", "vessel.length_std")
dt <- scale_if_present(dt, "Income.per.trip", "Income.per.trip_std")
dt <- scale_if_present(dt, "cost.per.trip", "cost.per.trip_std")

# -----------------------------
# Diagnostic dataset
# -----------------------------
all_vars <- c("mean_distance", num_vars, cat_vars)
vars_present <- all_vars[all_vars %in% names(dt)]

dt_check_raw <- dt %>%
  dplyr::select(dplyr::any_of(vars_present))

cat("Rows before drop_na (diagnostics set):", nrow(dt_check_raw), "\n")
cat("Rows that are complete cases (diagnostics set):", sum(complete.cases(dt_check_raw)), "\n")

na_counts <- sapply(dt_check_raw, function(x) sum(is.na(x)))
na_counts <- sort(na_counts[na_counts > 0], decreasing = TRUE)

if (length(na_counts) > 0) {
  cat("\nNA counts by variable (only >0 shown):\n")
  print(na_counts)
} else {
  cat("\nNo NAs found in diagnostics variables.\n")
}

dt_check <- dt_check_raw %>%
  tidyr::drop_na()

cat("\nDiagnostic data size after drop_na:", nrow(dt_check), "rows.\n")

# -----------------------------
# Model-selection dataset
# -----------------------------
model_predictors <- intersect(candidate_predictors, names(dt))
vars_needed <- c("mean_distance", model_predictors)

dt_model_raw <- dt %>%
  dplyr::select(dplyr::any_of(vars_needed))

cat("\nRows before drop_na (model-selection set):", nrow(dt_model_raw), "\n")
cat("Rows complete cases (model-selection set):", sum(complete.cases(dt_model_raw)), "\n")

dt_model <- dt_model_raw %>%
  tidyr::drop_na()

cat("Sample size for model selection:", nrow(dt_model), "\n")
cat("Load and cleaning step complete.\n")
