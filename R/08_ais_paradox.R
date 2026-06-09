# ============================================================
# 08 AIS CONDITIONAL EFFECT PLOT
# ============================================================

if (!file.exists(cluster_data_path)) {
  cat(
    "\nSkipping 08_ais_paradox.R because cluster_assignments.csv is missing.\n",
    "Place cluster_assignments.csv in the data/ folder if you want to run this section.\n"
  )
} else {
  cat("\n--- GENERATING AIS CONDITIONAL EFFECT PLOT ---\n")

  dt_model_data <- data.table::fread(cluster_data_path)
  data.table::setnames(dt_model_data, make.names(names(dt_model_data)))

  check_required_columns(
    dt_model_data,
    c(
      "mean_distance",
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
      "Top.species.group"
    ),
    "08_ais_paradox.R"
  )

  dt_glm_clean <- dt_model_data %>%
    dplyr::mutate(
      cost.per.trip_std = as.numeric(scale(cost.per.trip)),
      Fishing.experience_std = as.numeric(scale(Fishing.experience)),
      vessel.length_std = as.numeric(scale(vessel.length))
    ) %>%
    dplyr::select(
      mean_distance,
      cost.per.trip_std,
      Fishing.experience_std,
      vessel.length_std,
      AIS,
      VMS,
      navigation.apps,
      target.sharks,
      subsidy,
      Site,
      reg.awareness,
      Top.species.group
    ) %>%
    tidyr::drop_na() %>%
    dplyr::filter(mean_distance > 0)

  cat("Cleaned sample size for GLM fitting:", nrow(dt_glm_clean), "vessels.\n")

  ais_predictors <- c(
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
    "Top.species.group"
  )

  global_form <- as.formula(
    paste("mean_distance ~", paste(ais_predictors, collapse = " + "))
  )

  final_glm <- glm(
    global_form,
    data = dt_glm_clean,
    family = Gamma(link = "log")
  )

  pred_ais <- ggeffects::ggpredict(final_glm, terms = "AIS")

  pred_ais$x <- factor(
    pred_ais$x,
    levels = c("No", "Yes"),
    labels = c("AIS absent", "AIS present")
  )

  fig_ais <- ggplot(pred_ais, aes(x = x, y = predicted)) +
    geom_point(size = 4, color = "#08519C") +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0.15,
      linewidth = 1,
      color = "#08519C"
    ) +
    geom_line(
      aes(group = 1),
      linetype = "dashed",
      color = "#4B5563",
      linewidth = 0.8
    ) +
    theme_bw() +
    labs(
      title = "Figure S5: Conditional adjusted effect of AIS on fishing distance",
      x = "Tracking technology configuration",
      y = "Predicted mean fishing distance (km)"
    ) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#E5E7EB", linewidth = 0.5),
      plot.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 11, colour = "#111827"),
      axis.title = element_text(face = "bold", size = 11),
      text = element_text(size = 12)
    )

  print(fig_ais)

  ggsave(
    filename = file.path(supp_fig_dir, "Supp_Fig_S5_AIS_Conditional_Effect.png"),
    plot = fig_ais,
    width = 6.5,
    height = 5.0,
    dpi = 600
  )

  cat("Saved AIS conditional effect plot.\n")
}
