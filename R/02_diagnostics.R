# ============================================================
# 02 DIAGNOSTICS: LOW VARIANCE, CORRELATION, AND VIF/GVIF
# ============================================================

cat("\n--- TEST 1: LOW VARIANCE CHECK ---\n")

check_variance <- function(df, threshold = 95) {
  bad_vars <- c()

  for (col in names(df)) {
    freqs <- table(df[[col]])
    max_pct <- (max(freqs) / sum(freqs)) * 100

    if (max_pct >= threshold) {
      cat(sprintf("WARNING: '%s' low variance (%.1f%% same value)\n", col, max_pct))
      bad_vars <- c(bad_vars, col)
    }
  }

  bad_vars
}

low_var_columns <- check_variance(dt_check, threshold = 95)

if (length(low_var_columns) == 0) {
  cat("All variables have acceptable variance.\n")
}

cat("\n--- TEST 2: NUMERIC CORRELATION MATRIX ---\n")

existing_num_vars <- num_vars[num_vars %in% names(dt_check)]
numeric_df <- dt_check %>%
  dplyr::select(dplyr::any_of(existing_num_vars))

if (ncol(numeric_df) >= 2) {
  cor_matrix <- cor(numeric_df, use = "complete.obs")
  print(round(cor_matrix, 2))

  png(
    filename = file.path(diag_fig_dir, "Correlation_Plot.png"),
    width = 800,
    height = 800,
    res = 150
  )

  corrplot::corrplot(
    cor_matrix,
    method = "number",
    type = "upper",
    tl.col = "black",
    title = "Numeric Correlation",
    mar = c(0, 0, 1, 0)
  )

  dev.off()

  write.csv(
    round(cor_matrix, 3),
    file.path(table_dir, "Correlation_Matrix.csv"),
    row.names = TRUE
  )

  cat("Saved correlation plot and correlation matrix.\n")
} else {
  cat("Not enough numeric variables present to compute correlation matrix.\n")
}

cat("\n--- TEST 3: VARIANCE INFLATION FACTOR (VIF/GVIF) ---\n")

good_vars <- setdiff(names(dt_check), low_var_columns)
good_predictors <- setdiff(good_vars, "mean_distance")

if (length(good_predictors) == 0) {
  cat("No predictors available for VIF/GVIF after low-variance filtering.\n")
} else {
  vif_formula <- as.formula(
    paste("mean_distance ~", paste(good_predictors, collapse = " + "))
  )

  m_vif <- lm(vif_formula, data = dt_check)
  vif_raw <- car::vif(m_vif)

  if (is.matrix(vif_raw)) {
    vif_df <- as.data.frame(vif_raw)
    vif_df$Variable <- rownames(vif_df)

    vif_df <- vif_df %>%
      dplyr::mutate(GVIF_adj = GVIF^(1 / (2 * Df))) %>%
      dplyr::select(Variable, GVIF, Df, GVIF_adj) %>%
      dplyr::arrange(dplyr::desc(GVIF_adj))

    print(vif_df)
    cat("\nRule: GVIF^(1/(2*Df)) > 3 is moderate; > 5 is severe.\n")
  } else {
    vif_df <- data.frame(
      Variable = names(vif_raw),
      VIF = as.numeric(vif_raw)
    ) %>%
      dplyr::arrange(dplyr::desc(VIF))

    print(vif_df)
    cat("\nRule: VIF > 3 is moderate; > 5 is severe.\n")
  }

  write.csv(vif_df, file.path(table_dir, "VIF_GVIF_Table.csv"), row.names = FALSE)
  cat("Saved VIF/GVIF table.\n")
}
