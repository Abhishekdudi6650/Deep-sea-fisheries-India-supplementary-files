# ============================================================
# 04 MAIN FIGURES
# ============================================================

cat("\n--- GENERATING MAIN FIGURES ---\n")

# -----------------------------
# Figure 1: Relative Variable Importance
# -----------------------------
fig1_rvi <- ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue", color = "black", alpha = 0.8, width = 0.7) +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "red", linewidth = 1) +
  coord_flip() +
  clean_plot_theme +
  labs(
    title = "Relative Variable Importance (RVI)",
    x = "Predictor variables",
    y = "Sum of Akaike weights (RVI)"
  ) +
  annotate(
    "text",
    x = 1.5,
    y = 0.55,
    label = "0.50 threshold",
    color = "red",
    hjust = 0
  )

print(fig1_rvi)

ggsave(
  filename = file.path(main_fig_dir, "Figure1_RVI_Analysis.png"),
  plot = fig1_rvi,
  device = "png",
  width = 8,
  height = 6,
  dpi = 600
)

# -----------------------------
# Figure 2: Categorical drivers and outliers
# -----------------------------
check_required_columns(
  dt_model,
  c("Site", "AIS", "target.sharks", "mean_distance"),
  "04_main_figures.R / Figure 2"
)

p_site <- ggplot(dt_model, aes(x = Site, y = mean_distance)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2, fill = "lightgray") +
  clean_plot_theme +
  labs(x = "Departure port", y = "Distance")

p_ais <- ggplot(dt_model, aes(x = AIS, y = mean_distance)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2, fill = "lightgray") +
  clean_plot_theme +
  labs(x = "AIS possession", y = "Distance")

p_sharks <- ggplot(dt_model, aes(x = target.sharks, y = mean_distance)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2, fill = "lightgray") +
  clean_plot_theme +
  labs(x = "Targeted shark fishing", y = "Distance")

fig2_categorical <- p_site + p_ais + p_sharks +
  patchwork::plot_annotation(
    title = "Independent effects of categorical drivers on fishing distance",
    subtitle = "Red points indicate long-distance outliers",
    theme = theme(plot.title = element_text(face = "bold"))
  )

print(fig2_categorical)

ggsave(
  filename = file.path(main_fig_dir, "Figure2_Categorical_Outliers.png"),
  plot = fig2_categorical,
  device = "png",
  width = 12,
  height = 6,
  dpi = 600
)

# -----------------------------
# Figure 3: Vessel length marginal effect
# -----------------------------
check_required_columns(
  dt,
  c("mean_distance", "vessel.length", "Site", "AIS", "target.sharks"),
  "04_main_figures.R / Figure 3"
)

dt_clean_length <- dt %>%
  dplyr::select(mean_distance, vessel.length, Site, AIS, target.sharks) %>%
  tidyr::drop_na() %>%
  dplyr::filter(mean_distance > 0)

viz_model <- glm(
  mean_distance ~ vessel.length + Site + AIS + target.sharks,
  data = dt_clean_length,
  family = Gamma(link = "log")
)

pred_length <- ggeffects::ggpredict(viz_model, terms = "vessel.length [all]")

fig3_length <- ggplot() +
  geom_point(
    data = dt_clean_length,
    aes(x = vessel.length, y = mean_distance),
    alpha = 0.4,
    color = "gray40",
    size = 2
  ) +
  geom_line(
    data = pred_length,
    aes(x = x, y = predicted),
    color = "blue",
    linewidth = 1.2
  ) +
  geom_ribbon(
    data = pred_length,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    alpha = 0.2,
    fill = "blue"
  ) +
  clean_plot_theme +
  labs(
    title = "Effect of vessel length on fishing distance",
    x = "Vessel length (feet)",
    y = "Distance (km)"
  )

print(fig3_length)

ggsave(
  filename = file.path(main_fig_dir, "Figure3_Vessel_Length_Effect.png"),
  plot = fig3_length,
  device = "png",
  width = 8,
  height = 6,
  dpi = 600
)

ggsave(
  filename = file.path(main_fig_dir, "Figure3_Vessel_Length_Effect.tiff"),
  plot = fig3_length,
  device = "tiff",
  compression = "lzw",
  width = 8,
  height = 6,
  dpi = 600
)

cat("Main figures saved.\n")
