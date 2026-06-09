# ============================================================
# 03 MODEL SELECTION AND RELATIVE VARIABLE IMPORTANCE
# ============================================================

cat("\n--- MODEL SELECTION: ALL COMBINATIONS (Gamma log-link) ---\n")

predictors <- intersect(candidate_predictors, names(dt_model))

if (length(predictors) == 0) {
  stop("No candidate predictors available for model selection.", call. = FALSE)
}

get_all_formulas <- function(dep_var, vars) {
  formulas <- list()

  for (i in seq_along(vars)) {
    combos <- utils::combn(vars, i, simplify = FALSE)

    for (combo in combos) {
      formulas[[length(formulas) + 1]] <- paste(dep_var, "~", paste(combo, collapse = " + "))
    }
  }

  formulas
}

all_formulas <- get_all_formulas("mean_distance", predictors)
cat("Total models to run:", length(all_formulas), "\n")

results_list <- list()

for (i in seq_along(all_formulas)) {
  f <- as.formula(all_formulas[[i]])

  tryCatch(
    {
      m <- glm(f, data = dt_model, family = Gamma(link = "log"))

      results_list[[length(results_list) + 1]] <- data.frame(
        Formula = all_formulas[[i]],
        AIC = AIC(m),
        Deviance = summary(m)$deviance,
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      # Some models can fail because of sparse factor levels or separation-like issues.
      # Failed models are skipped.
    }
  )

  if (i %% 500 == 0) {
    cat(round((i / length(all_formulas)) * 100), "% complete...\n")
  }
}

if (length(results_list) == 0) {
  stop("No models fitted successfully. Check variable types and missing data.", call. = FALSE)
}

results_df <- dplyr::bind_rows(results_list) %>%
  dplyr::arrange(AIC) %>%
  dplyr::mutate(Delta_AIC = AIC - min(AIC))

cat("\n--- TOP 5 MODELS ---\n")
print(head(results_df, 5))

write.csv(
  results_df,
  file.path(table_dir, "Final_Model_Combinations_Ranked.csv"),
  row.names = FALSE
)

cat("Saved model selection table.\n")

# -----------------------------
# Relative variable importance from top models
# -----------------------------
top_models <- results_df %>%
  dplyr::filter(Delta_AIC <= 2)

if (nrow(top_models) == 0) {
  stop("No top models found with Delta_AIC <= 2.", call. = FALSE)
}

top_models <- top_models %>%
  dplyr::mutate(
    exp_term = exp(-0.5 * Delta_AIC),
    weight = exp_term / sum(exp_term)
  )

all_vars_rvi <- unique(
  unlist(stringr::str_split(gsub("mean_distance ~ ", "", top_models$Formula), " \\+ "))
)

importance_df <- data.frame(
  Variable = all_vars_rvi,
  Importance = 0
)

for (i in seq_len(nrow(importance_df))) {
  var <- importance_df$Variable[i]
  contains_var <- grepl(paste0("\\b", var, "\\b"), top_models$Formula)
  importance_df$Importance[i] <- sum(top_models$weight[contains_var])
}

importance_df <- importance_df %>%
  dplyr::arrange(dplyr::desc(Importance))

cat("\n--- RELATIVE VARIABLE IMPORTANCE (RVI) ---\n")
print(importance_df)

write.csv(
  importance_df,
  file.path(table_dir, "Table_RVI.csv"),
  row.names = FALSE
)

cat("Saved RVI table.\n")
