# ============================================================
# 05 SUPPLEMENTARY FIGURES
# ============================================================

cat("\n--- GENERATING SUPPLEMENTARY FIGURES ---\n")

supp_theme <- theme_bw() +
  theme(
    panel.grid = element_blank(),
    text = element_text(size = 12),
    plot.title = element_text(face = "bold")
  )

# -----------------------------
# Figure S1: Pearson correlation matrix
# -----------------------------
check_required_columns(
  dt,
  c("mean_distance", "vessel.length", "Fishing.experience", "Income.per.trip", "cost.per.trip"),
  "05_supplementary_figures.R / Figure S1"
)

dt_numeric <- dt %>%
  dplyr::select(mean_distance, vessel.length, Fishing.experience, Income.per.trip, cost.per.trip) %>%
  tidyr::drop_na()

corr_matrix <- cor(dt_numeric, method = "pearson")

fig_s1_corr <- ggcorrplot::ggcorrplot(
  corr_matrix,
  method = "square",
  type = "lower",
  lab = TRUE,
  lab_size = 4,
  colors = c("firebrick", "white", "steelblue"),
  title = "Figure S1: Pearson Correlation Matrix"
) +
  theme(plot.title = element_text(face = "bold"))

print(fig_s1_corr)

ggsave(
  filename = file.path(supp_fig_dir, "Supp_Fig_S1_Correlogram.png"),
  plot = fig_s1_corr,
  width = 8,
  height = 8,
  dpi = 600
)

# -----------------------------
# Figure S2: Pearson residuals vs fitted values
# -----------------------------
check_required_columns(
  dt,
  c("mean_distance", "vessel.length", "Site", "AIS", "target.sharks"),
  "05_supplementary_figures.R / Figure S2"
)

dt_clean_supp <- dt %>%
  dplyr::select(mean_distance, vessel.length, Site, AIS, target.sharks) %>%
  tidyr::drop_na() %>%
  dplyr::filter(mean_distance > 0)

viz_model_supp <- glm(
  mean_distance ~ vessel.length + Site + AIS + target.sharks,
  data = dt_clean_supp,
  family = Gamma(link = "log")
)

diag_data <- data.frame(
  Fitted = fitted(viz_model_supp),
  Residuals = residuals(viz_model_supp, type = "pearson")
)

fig_s2_diag <- ggplot(diag_data, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5, color = "gray30", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  supp_theme +
  labs(
    title = "Figure S2: Pearson Residuals vs Fitted Values",
    x = "Fitted values (predicted mean distance)",
    y = "Pearson residuals"
  )

print(fig_s2_diag)

ggsave(
  filename = file.path(supp_fig_dir, "Supp_Fig_S2_Diagnostics.png"),
  plot = fig_s2_diag,
  width = 8,
  height = 6,
  dpi = 600
)

# -----------------------------
# Figure S3: Marginal effects of weaker socio-economic variables
# -----------------------------
check_required_columns(
  dt,
  c(
    "mean_distance",
    "vessel.length",
    "Site",
    "AIS",
    "target.sharks",
    "Income.per.trip",
    "Fishing.experience",
    "subsidy"
  ),
  "05_supplementary_figures.R / Figure S3"
)

dt_global <- dt %>%
  dplyr::select(
    mean_distance,
    vessel.length,
    Site,
    AIS,
    target.sharks,
    Income.per.trip,
    Fishing.experience,
    subsidy
  ) %>%
  tidyr::drop_na() %>%
  dplyr::filter(mean_distance > 0)

global_model <- glm(
  mean_distance ~ vessel.length + Site + AIS + target.sharks +
    Income.per.trip + Fishing.experience + subsidy,
  data = dt_global,
  family = Gamma(link = "log")
)

pred_income <- ggeffects::ggpredict(global_model, terms = "Income.per.trip [all]")
pred_exp <- ggeffects::ggpredict(global_model, terms = "Fishing.experience [all]")

p_income <- ggplot() +
  geom_point(
    data = dt_global,
    aes(x = Income.per.trip, y = mean_distance),
    alpha = 0.3,
    color = "gray50"
  ) +
  geom_line(
    data = pred_income,
    aes(x = x, y = predicted),
    color = "darkred",
    linewidth = 1
  ) +
  geom_ribbon(
    data = pred_income,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    alpha = 0.15,
    fill = "darkred"
  ) +
  supp_theme +
  labs(x = "Income per trip", y = "Distance")

p_exp <- ggplot() +
  geom_point(
    data = dt_global,
    aes(x = Fishing.experience, y = mean_distance),
    alpha = 0.3,
    color = "gray50"
  ) +
  geom_line(
    data = pred_exp,
    aes(x = x, y = predicted),
    color = "darkred",
    linewidth = 1
  ) +
  geom_ribbon(
    data = pred_exp,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    alpha = 0.15,
    fill = "darkred"
  ) +
  supp_theme +
  labs(x = "Fishing experience (years)", y = "Distance")

fig_s3_discarded <- p_income + p_exp +
  patchwork::plot_annotation(
    title = "Figure S3: Marginal effects of non-significant socio-economic drivers",
    subtitle = "Flat trendlines and wide confidence intervals indicate weak predictive power",
    theme = theme(plot.title = element_text(face = "bold"))
  )

print(fig_s3_discarded)

ggsave(
  filename = file.path(supp_fig_dir, "Supp_Fig_S3_Discarded_Vars.png"),
  plot = fig_s3_discarded,
  width = 12,
  height = 6,
  dpi = 600
)

ggsave(
  filename = file.path(supp_fig_dir, "Supp_Fig_S3_Discarded_Vars.tiff"),
  plot = fig_s3_discarded,
  device = "tiff",
  compression = "lzw",
  width = 12,
  height = 6,
  dpi = 600
)

cat("Supplementary figures saved.\n")
