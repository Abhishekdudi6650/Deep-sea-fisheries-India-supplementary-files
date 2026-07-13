cat("\n--- GENERATING REVISED FAMD PLOTS WITH FISHING DISTANCE INCLUDED ---\n")

# ------------------------------------------------------------
# Input file
# ------------------------------------------------------------

famd_input_path <- main_data_path
check_file_exists(famd_input_path, "main dataset for revised FAMD")

dt_cluster <- data.table::fread(famd_input_path)
data.table::setnames(dt_cluster, make.names(names(dt_cluster)))

# Optional compatibility fixes if older column names are present
if ("Starting.point" %in% names(dt_cluster) && !"Site" %in% names(dt_cluster)) {
  data.table::setnames(dt_cluster, "Starting.point", "Site")
}

if ("Top.species.catch.group" %in% names(dt_cluster) && !"Top.species.group" %in% names(dt_cluster)) {
  data.table::setnames(dt_cluster, "Top.species.catch.group", "Top.species.group")
}

if ("Fishing.distance" %in% names(dt_cluster) && !"mean_distance" %in% names(dt_cluster)) {
  data.table::setnames(dt_cluster, "Fishing.distance", "mean_distance")
}

# ------------------------------------------------------------
# Revised active variables for FAMD
# Fishing distance is now included as an active variable
# ------------------------------------------------------------

famd_predictors <- c(
  "mean_distance",
  "Fishing.experience",
  "vessel.length",
  "AIS",
  "VMS",
  "navigation.apps",
  "target.sharks",
  "Top.species.group",
  "subsidy",
  "cost.per.trip",
  "reg.awareness",
  "Site"
)

binary_vars <- c(
  "AIS",
  "VMS",
  "navigation.apps",
  "target.sharks",
  "subsidy"
)

categorical_vars <- c(
  "Top.species.group",
  "reg.awareness",
  "Site"
)

continuous_vars <- c(
  "mean_distance",
  "Fishing.experience",
  "vessel.length",
  "cost.per.trip"
)

check_required_columns(
  dt_cluster,
  famd_predictors,
  "07_revised_famd_cluster_profiles.R"
)

# ------------------------------------------------------------
# Clean numeric variables
# ------------------------------------------------------------

for (v in continuous_vars) {
  if (v %in% names(dt_cluster)) {
    dt_cluster[, (v) := as.numeric(get(v))]
  }
}

# Keep only positive fishing distance
dt_cluster <- dt_cluster[!is.na(mean_distance) & mean_distance > 0]

# ------------------------------------------------------------
# Clean binary variables as No / Yes factors
# ------------------------------------------------------------

for (v in binary_vars) {
  if (v %in% names(dt_cluster)) {
    dt_cluster[, (v) := trimws(as.character(get(v)))]
    dt_cluster[get(v) == "0", (v) := "No"]
    dt_cluster[get(v) == "1", (v) := "Yes"]
    dt_cluster[, (v) := factor(get(v), levels = c("No", "Yes"))]
  }
}

# ------------------------------------------------------------
# Clean categorical variables
# ------------------------------------------------------------

for (v in categorical_vars) {
  if (v %in% names(dt_cluster)) {
    dt_cluster[, (v) := factor(trimws(as.character(get(v))))]
  }
}

# ------------------------------------------------------------
# Prepare FAMD dataset
# ------------------------------------------------------------

dt_famd <- dt_cluster %>%
  dplyr::select(dplyr::all_of(famd_predictors)) %>%
  tidyr::drop_na()

cat("Rows used in revised FAMD:", nrow(dt_famd), "\n")
cat("Variables used in revised FAMD:\n")
print(names(dt_famd))

# ------------------------------------------------------------
# Run revised FAMD
# ------------------------------------------------------------

res_final_famd <- FactoMineR::FAMD(
  dt_famd,
  ncp = 10,
  graph = FALSE
)

cat("\n--- REVISED FAMD DIMENSION PERCENTAGES ---\n")
print(round(res_final_famd$eig[1:5, 2], 1))

# ------------------------------------------------------------
# Run HCPC using the revised FAMD
# ------------------------------------------------------------
# nb.clust = -1 lets HCPC choose the number of clusters automatically.
# If you want exactly 3 clusters, change nb.clust = 3.

res_final_hcpc <- FactoMineR::HCPC(
  res_final_famd,
  nb.clust = -1,
  consol = TRUE,
  graph = FALSE
)

cluster_assignments_revised <- dt_cluster %>%
  dplyr::select(dplyr::all_of(famd_predictors)) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    Cluster = factor(res_final_hcpc$data.clust$clust)
  )

cat("\nRevised cluster sizes:\n")
print(table(cluster_assignments_revised$Cluster))

write.csv(
  cluster_assignments_revised,
  file.path(table_dir, "Table_S5_Revised_Cluster_Assignments_With_Distance.csv"),
  row.names = FALSE
)

cat("Saved revised cluster assignments with fishing distance included.\n")

# ------------------------------------------------------------
# Figure S4: FAMD scree plot
# ------------------------------------------------------------

scree_ylim <- max(25, ceiling(max(res_final_famd$eig[, 2]) / 5) * 5)

fig_scree <- factoextra::fviz_eig(
  res_final_famd,
  addlabels = TRUE,
  ylim = c(0, scree_ylim),
  barfill = "steelblue",
  barcolor = "black",
  ggtheme = theme_bw()
) +
  labs(
    title = "Figure S4: FAMD Dimensions Explained Variance",
    x = "Principal dimensions",
    y = "Percentage of explained variance (%)"
  ) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    text = element_text(size = 12)
  )

ggsave(
  filename = file.path(supp_fig_dir, "Supp_Fig_S4_FAMD_Scree.png"),
  plot = fig_scree,
  width = 8,
  height = 5,
  dpi = 600
)

cat("Saved FAMD scree plot.\n")


# ------------------------------------------------------------
# Figure S5: FAMD variable contribution bar plot
# ------------------------------------------------------------

# Extract variable contributions from the FAMD result
famd_var <- factoextra::get_famd_var(res_final_famd, element = "var")

contrib_df <- as.data.frame(famd_var$contrib[, 1:2, drop = FALSE])
contrib_df$Variable <- rownames(contrib_df)

# Calculate total contribution to Dimensions 1 and 2
contrib_df <- contrib_df %>%
  dplyr::rename(
    Dim1 = Dim.1,
    Dim2 = Dim.2
  ) %>%
  dplyr::mutate(
    Total_contribution = Dim1 + Dim2,
    Variable_label = dplyr::recode(
      Variable,
      "mean_distance" = "Mean fishing distance",
      "Fishing.experience" = "Fishing experience",
      "vessel.length" = "Vessel length",
      "AIS" = "AIS",
      "VMS" = "VMS",
      "navigation.apps" = "Navigation apps",
      "target.sharks" = "Target sharks",
      "Top.species.group" = "Top species catch group",
      "subsidy" = "Subsidy",
      "cost.per.trip" = "Cost per trip",
      "reg.awareness" = "Regulation awareness",
      "Site" = "Site"
    )
  ) %>%
  dplyr::arrange(Total_contribution)

fig_vars <- ggplot(
  contrib_df,
  aes(
    x = reorder(Variable_label, Total_contribution),
    y = Total_contribution
  )
) +
  geom_col(
    fill = "steelblue",
    color = "black",
    width = 0.75
  ) +
  geom_text(
    aes(label = round(Total_contribution, 1)),
    hjust = -0.15,
    size = 3.5
  ) +
  coord_flip() +
  labs(
    title = "Figure S5: Variable Contributions to FAMD Dimensions",
    x = "Active variables",
    y = "Contribution to Dimensions 1 and 2 (%)"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    text = element_text(size = 12)
  ) +
  expand_limits(y = max(contrib_df$Total_contribution, na.rm = TRUE) * 1.12)

ggsave(
  filename = file.path(supp_fig_dir, "Supp_Fig_S5_FAMD_Variable_Contributions.png"),
  plot = fig_vars,
  width = 8,
  height = 6,
  dpi = 600
)

cat("Saved FAMD variable contribution bar plot.\n")
# ------------------------------------------------------------
# Revised cluster profile table
# ------------------------------------------------------------

cluster_assignments_revised <- cluster_assignments_revised %>%
  dplyr::mutate(cluster = as.numeric(as.character(Cluster)))

continuous_present <- intersect(continuous_vars, names(cluster_assignments_revised))
categorical_present <- intersect(c(binary_vars, categorical_vars), names(cluster_assignments_revised))

summary_continuous <- cluster_assignments_revised %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(continuous_present),
      list(
        mean = ~ mean(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE)
      )
    ),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols = -cluster,
    names_to = c("Variable", ".value"),
    names_pattern = "(.*)_(mean|sd)"
  ) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 1))) %>%
  dplyr::mutate(Reported_Value = paste0(mean, " ± ", sd)) %>%
  dplyr::select(Variable, cluster, Reported_Value) %>%
  tidyr::pivot_wider(
    names_from = cluster,
    names_prefix = "Cluster_",
    values_from = Reported_Value
  )

summary_categorical <- list()

for (var in categorical_present) {
  pct_df <- cluster_assignments_revised %>%
    dplyr::group_by(cluster, !!rlang::sym(var)) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop_last") %>%
    dplyr::mutate(pct = round((n / sum(n)) * 100, 1)) %>%
    dplyr::mutate(Reported_Value = paste0(n, " (", pct, "%)")) %>%
    dplyr::select(-n, -pct) %>%
    dplyr::mutate(Variable = paste0(var, " [", !!rlang::sym(var), "]")) %>%
    dplyr::select(Variable, cluster, Reported_Value) %>%
    tidyr::pivot_wider(
      names_from = cluster,
      names_prefix = "Cluster_",
      values_from = Reported_Value
    )
  
  summary_categorical[[var]] <- pct_df
}

final_profile_table <- dplyr::bind_rows(
  summary_continuous,
  dplyr::bind_rows(summary_categorical)
)

final_profile_table[is.na(final_profile_table)] <- "0 (0.0%)"

write.csv(
  final_profile_table,
  file.path(table_dir, "Table_S5_Revised_Cluster_Profiles_With_Distance.csv"),
  row.names = FALSE
)

cat("Saved revised cluster profile table with fishing distance included.\n")
