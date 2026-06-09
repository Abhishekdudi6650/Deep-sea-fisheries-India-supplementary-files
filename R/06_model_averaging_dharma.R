# ============================================================
# 06 MODEL AVERAGING AND DHARMA GLM DIAGNOSTICS
# ============================================================

cat("\n--- BUILDING DATASET AND RUNNING MODEL AVERAGING ---\n")

available_final_predictors <- intersect(final_predictors, names(dt))
missing_final_predictors <- setdiff(final_predictors, names(dt))

if (length(missing_final_predictors) > 0) {
  warning(
    "These final predictors are missing and will be excluded: ",
    paste(missing_final_predictors, collapse = ", ")
  )
}

if (length(available_final_predictors) == 0) {
  stop("No final predictors available for model averaging.", call. = FALSE)
}

dt_final <- dt %>%
  dplyr::select(dplyr::any_of(c("mean_distance", available_final_predictors))) %>%
  tidyr::drop_na() %>%
  dplyr::filter(mean_distance > 0)

cat("Sample size for model averaging:", nrow(dt_final), "rows.\n")

old_na_action <- getOption("na.action")
options(na.action = "na.fail")

global_form <- as.formula(
  paste("mean_distance ~", paste(available_final_predictors, collapse = " + "))
)

global_glm <- glm(
  global_form,
  data = dt_final,
  family = Gamma(link = "log")
)

cat("Running all subsets with MuMIn::dredge.\n")

model_set <- MuMIn::dredge(global_glm, rank = "AICc")

top_models_averaged <- MuMIn::model.avg(model_set, subset = delta < 2)

param_estimates <- as.data.frame(summary(top_models_averaged)$coefmat.full)
param_estimates$Variable <- rownames(param_estimates)

param_estimates <- param_estimates %>%
  dplyr::select(Variable, Estimate, `Std. Error`, `Adjusted SE`, `z value`, `Pr(>|z|)`) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 3)))

print(param_estimates)

write.csv(
  param_estimates,
  file.path(table_dir, "Table_S4_Averaged_Parameters.csv"),
  row.names = FALSE
)

# Restore previous global option.
options(na.action = old_na_action)

cat("Saved averaged parameter estimates.\n")

# -----------------------------
# DHARMa GLM diagnostics
# -----------------------------
cat("\n--- GENERATING DHARMA RESIDUAL DIAGNOSTICS ---\n")

sim_residuals <- DHARMa::simulateResiduals(
  fittedModel = global_glm,
  n = 1000
)

png(
  filename = file.path(supp_fig_dir, "Supp_Fig_S2_GLM_Diagnostics_DHARMa.png"),
  width = 2400,
  height = 1200,
  res = 300
)

par(mfrow = c(1, 2), mar = c(5, 4.5, 4, 2) + 0.1)

plot(
  sim_residuals,
  title = "Figure S2: GLM Diagnostics",
  xlab = "Model predicted values",
  ylab = "Standardized residuals"
)

dev.off()

cat("Saved DHARMa diagnostic plot.\n")

cat("\n--- FORMAL DIAGNOSTIC TEST RESULTS ---\n")
print(DHARMa::testUniformity(sim_residuals))
print(DHARMa::testDispersion(sim_residuals))
