# ============================================================
# 07 FAMD SCREE PLOT AND CLUSTER PROFILE TABLES
# ============================================================

if (!file.exists(cluster_data_path)) {
  cat(
    "\nSkipping 07_famd_cluster_profiles.R because cluster_assignments.csv is missing.\n",
    "Place cluster_assignments.csv in the data/ folder if you want to run this section.\n"
  )
} else {
  cat("\n--- GENERATING FAMD SCREE PLOT AND CLUSTER TABLES ---\n")

  dt_cluster <- data.table::fread(cluster_data_path)
  data.table::setnames(dt_cluster, make.names(names(dt_cluster)))

  # -----------------------------
  # Figure S4: FAMD scree plot
  # -----------------------------
  columns_to_run <- intersect(cluster_active_vars, names(dt_cluster))

  if (length(columns_to_run) < 2) {
    warning("Not enough active FAMD variables found in cluster_assignments.csv.")
  } else {
    dt_famd <- dt_cluster %>%
      dplyr::select(dplyr::all_of(columns_to_run)) %>%
      tidyr::drop_na()

    res_final_famd <- FactoMineR::FAMD(
      dt_famd,
      ncp = 10,
      graph = FALSE
    )

    cat("\n--- VERIFICATION OF DIMENSION PERCENTAGES ---\n")
    print(round(res_final_famd$eig[1:2, 2], 1))

    fig_scree <- factoextra::fviz_eig(
      res_final_famd,
      addlabels = TRUE,
      ylim = c(0, 25),
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
  }

  # -----------------------------
  # Cluster profile table
  # -----------------------------
  if (!"Cluster" %in% names(dt_cluster)) {
    warning("Column 'Cluster' not found. Skipping cluster profile table.")
  } else {
    dt_cluster[, cluster := as.numeric(gsub("Cluster ", "", Cluster))]

    cat("Verified synchronized sample sizes from active file:\n")
    print(table(dt_cluster$cluster))

    continuous_vars <- c(
      "mean_distance",
      "cost.per.trip",
      "vessel.length",
      "Fishing.experience"
    )

    categorical_vars <- c(
      "Site",
      "AIS",
      "VMS",
      "navigation.apps",
      "target.sharks",
      "subsidy",
      "Top.species.group"
    )

    continuous_present <- intersect(continuous_vars, names(dt_cluster))
    categorical_present <- intersect(categorical_vars, names(dt_cluster))

    summary_continuous <- dt_cluster %>%
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
      pct_df <- dt_cluster %>%
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
      file.path(table_dir, "Table_S5_Final_Cluster_Profiles.csv"),
      row.names = FALSE
    )

    cat("Saved final cluster profile table.\n")
  }
}
