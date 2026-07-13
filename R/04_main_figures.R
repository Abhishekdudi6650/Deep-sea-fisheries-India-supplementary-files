# ============================================================
# 04 CORRECTED MAIN FIGURE 
# A = weighted model-averaged effect sizes
# B = relative variable importance
# C = raw descriptive boxplots
# ============================================================

cat("\n--- GENERATING CORRECTED FIGURE: EFFECT SIZE + RVI + RAW BOXPLOTS ---\n")

library(ggplot2)
library(patchwork)
library(dplyr)
library(stringr)
library(MuMIn)

# -----------------------------
# Make sure folders exist
# -----------------------------

dir.create(main_fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Theme
# -----------------------------

modern_theme <- theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.tag = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold")
  )

# ============================================================
# 01. Get the weighted model-averaged object
# ============================================================

if (exists("averaged_model")) {
  avg_obj <- averaged_model
} else if (exists("avg_model")) {
  avg_obj <- avg_model
} else if (exists("top_models")) {
  avg_obj <- MuMIn::model.avg(top_models, fit = TRUE, revised.var = TRUE)
} else {
  stop("No averaged model object found. Need averaged_model, avg_model, or top_models.")
}

summary_avg <- summary(avg_obj)

# ============================================================
# PANEL A: TRUE EFFECT SIZE PLOT
# This is the weighted model-averaged coefficient plot.
# AIS should appear around -0.42 if your model output is the same.
# ============================================================

coef_df <- as.data.frame(summary_avg$coefmat.full)
coef_df$Term <- rownames(coef_df)

se_col <- if ("Adjusted SE" %in% names(coef_df)) {
  "Adjusted SE"
} else {
  "Std. Error"
}

# Clean labels for effect-size plot
coef_df <- coef_df %>%
  filter(Term != "(Intercept)") %>%
  mutate(
    Term_clean = case_when(
      Term == "AIS" ~ "AIS use",
      str_detect(Term, "^AIS") ~ "AIS use",
      Term == "cost.per.trip" ~ "Cost per trip",
      Term == "Fishing.experience" ~ "Fishing experience",
      Term == "vessel.length" ~ "Vessel length",
      str_detect(Term, "^SiteB") ~ "Starting site: B vs A",
      str_detect(Term, "^SiteC") ~ "Starting site: C vs A",
      str_detect(Term, "^target.sharks") ~ "Target sharks",
      str_detect(Term, "^subsidy") ~ "Subsidy",
      TRUE ~ Term
    ),
    conf.low = Estimate - 1.96 * .data[[se_col]],
    conf.high = Estimate + 1.96 * .data[[se_col]]
  )

write.csv(
  coef_df,
  file.path(table_dir, "Table_WEIGHTED_Model_Averaged_Effect_Sizes.csv"),
  row.names = FALSE
)

cat("\nCheck AIS effect size here:\n")
print(coef_df %>% filter(str_detect(Term_clean, "AIS")))

fig_A_effect <- ggplot(
  coef_df,
  aes(x = Estimate, y = reorder(Term_clean, Estimate))
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.8
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.18,
    linewidth = 0.8,
    color = "black"
  ) +
  geom_point(
    size = 3,
    color = "black"
  ) +
  modern_theme +
  labs(
    tag = "A",
    title = "Model-averaged effect sizes",
    x = "Weighted model-averaged estimate",
    y = ""
  )

# ============================================================
# PANEL B: RVI PLOT
# This is variable support only, not effect size.
# ============================================================

rvi_values <- MuMIn::sw(avg_obj)

importance_df_new <- data.frame(
  Variable = names(rvi_values),
  Importance = as.numeric(rvi_values)
)

importance_df_new <- importance_df_new %>%
  mutate(
    Variable_clean = case_when(
      Variable == "cost.per.trip" ~ "Cost per trip",
      Variable == "vessel.length" ~ "Vessel length",
      Variable == "Fishing.experience" ~ "Fishing experience",
      Variable == "target.sharks" ~ "Target sharks",
      Variable == "subsidy" ~ "Subsidy",
      TRUE ~ Variable
    )
  )

write.csv(
  importance_df_new,
  file.path(table_dir, "Table_RVI_Model_Support.csv"),
  row.names = FALSE
)

fig_B_rvi <- ggplot(
  importance_df_new,
  aes(x = reorder(Variable_clean, Importance), y = Importance)
) +
  geom_col(
    fill = "#0072B2",
    color = "black",
    alpha = 0.85,
    width = 0.7
  ) +
  geom_hline(
    yintercept = 0.50,
    linetype = "dashed",
    color = "black",
    linewidth = 0.8
  ) +
  coord_flip() +
  modern_theme +
  labs(
    tag = "B",
    title = "Relative variable importance",
    x = "",
    y = "RVI"
  )

# -----------------------------
# PANEL C: Raw descriptive plots for cost per trip, target sharks, and site
# -----------------------------

# Make sure cost is in USD for plotting
if (!"cost.per.trip.usd" %in% names(dt_model)) {
  inr_per_usd <- 95.23
  dt_model <- dt_model %>%
    mutate(cost.per.trip.usd = cost.per.trip / inr_per_usd)
}

# C1: Cost per trip
plot_cost_raw <- ggplot(
  dt_model,
  aes(x = cost.per.trip.usd, y = mean_distance)
) +
  geom_point(alpha = 0.45, size = 2, color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "grey70") +
  modern_theme +
  labs(
    tag = "C",
    title = "Cost per trip",
    x = "Cost per trip (US$)",
    y = "Mean fishing distance (km)"
  )

# C2: Target sharks
box_sharks <- ggplot(
  dt_model,
  aes(x = factor(target.sharks, labels = c("No", "Yes")), y = mean_distance)
) +
  geom_boxplot(
    fill = "#56B4E9",
    color = "black",
    outlier.shape = 16,
    outlier.size = 2,
    alpha = 0.8
  ) +
  modern_theme +
  theme(axis.title.y = element_blank()) +
  labs(
    title = "Target sharks",
    x = "",
    y = ""
  )

# C3: Site
box_site <- ggplot(
  dt_model,
  aes(x = Site, y = mean_distance)
) +
  geom_boxplot(
    fill = "#56B4E9",
    color = "black",
    outlier.shape = 16,
    outlier.size = 2,
    alpha = 0.8
  ) +
  modern_theme +
  theme(axis.title.y = element_blank()) +
  labs(
    title = "Starting site",
    x = "Site",
    y = ""
  )

# Combine all three into Panel C
fig_C_boxplots <- plot_cost_raw | box_sharks | box_site
# ============================================================
# FINAL CORRECTED FIGURE
# ============================================================

final_figure_corrected <- (fig_A_effect | fig_B_rvi) / fig_C_boxplots +
  plot_layout(heights = c(1, 0.8))

print(final_figure_corrected)

# Save corrected figure with NEW name
ggsave(
  filename = file.path(main_fig_dir, "Figure4_CORRECTED_EffectSize_RVI_Boxplots.png"),
  plot = final_figure_corrected,
  device = "png",
  width = 12,
  height = 9,
  dpi = 600
)

ggsave(
  filename = file.path(main_fig_dir, "Figure4_CORRECTED_EffectSize_RVI_Boxplots.tiff"),
  plot = final_figure_corrected,
  device = "tiff",
  width = 12,
  height = 9,
  dpi = 600,
  compression = "lzw"
)

# Also overwrite old filename so you don't attach the old one by mistake
ggsave(
  filename = file.path(main_fig_dir, "Figure4_Modern_Composite_USD.tiff"),
  plot = final_figure_corrected,
  device = "tiff",
  width = 12,
  height = 9,
  dpi = 600,
  compression = "lzw"
)

cat("\nDONE. Corrected figure saved.\n")
cat("Use this file:\n")
cat(file.path(main_fig_dir, "Figure4_CORRECTED_EffectSize_RVI_Boxplots.tiff"), "\n")
