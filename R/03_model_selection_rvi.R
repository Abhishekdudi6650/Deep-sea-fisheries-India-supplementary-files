# ============================================================
# 03 MODEL SELECTION AND RELATIVE VARIABLE IMPORTANCE
# ============================================================
cat("\n--- MODEL SELECTION: ALL COMBINATIONS (Gamma log-link) ---\n")

# Load the required package
library(MuMIn)

# 1. Define the exact predictors present in your 'dt' object,
# excluding 'gear.type' and 'Income.per.trip' as per your methods text.
predictors <- c(
  "Site", "vessel.length_std", "cost.per.trip_std", 
  "Fishing.experience_std", "navigation.apps", "AIS", "VMS", 
  "Top.species.group", "target.sharks", "subsidy", 
  "communication.with.fishers", "reg.awareness"
)

# 2. Rebuild a clean dt_model with only the response and the chosen predictors
model_cols <- c("mean_distance", predictors)

check_required_columns(dt, model_cols, "03_model_selection")

dt_model <- as.data.frame(data.table::copy(dt[, ..model_cols]))

# Ensure categorical/character columns are treated as factors for the GLM
dt_model$Site <- as.factor(dt_model$Site)
dt_model$Top.species.group <- as.factor(dt_model$Top.species.group)

# 3. Fit the global model
global_model <- glm(
  mean_distance ~ ., 
  data = dt_model, 
  family = Gamma(link = "log"),
  na.action = "na.fail" 
)

# 4. Run all-subsets selection via MuMIn (Replaces your manual loops)
cat("Running multi-model inference via MuMIn...\n")
model_set <- dredge(global_model)

# Save the full ranked model combinations table
write.csv(
  as.data.frame(model_set),
  file.path(table_dir, "Final_Model_Combinations_Ranked.csv"),
  row.names = FALSE
)
cat("Saved model selection table.\n")

# 5. Subset the top models (Delta AICc <= 2)
top_models <- get.models(model_set, subset = delta <= 2)

if (length(top_models) == 0) {
  stop("No top models found with Delta_AICc <= 2.", call. = FALSE)
}

# 6. Perform Model Averaging
averaged_model <- model.avg(top_models)

cat("\n--- MODEL AVERAGED ESTIMATES & RVI ---\n")
summary_avg <- summary(averaged_model)
print(summary_avg)

# 7. Extract and Save the RVI Results
rvi_values <- sw(averaged_model)
importance_df <- data.frame(
  Variable = names(rvi_values),
  Importance = as.numeric(rvi_values)
)

write.csv(
  importance_df,
  file.path(table_dir, "Table_RVI.csv"),
  row.names = FALSE
)
cat("Saved RVI table.\n")

# 8. Extract and Save Model Averaged Coefficients ("Full" average)
coef_df <- as.data.frame(summary_avg$coefmat.full)
coef_df$Variable <- rownames(coef_df)
coef_df <- coef_df[, c("Variable", "Estimate", "Std. Error", "Adjusted SE", "z value")]

write.csv(
  coef_df,
  file.path(table_dir, "Table_Averaged_Estimates.csv"),
  row.names = FALSE
)
cat("Saved Averaged Estimates table.\n")
