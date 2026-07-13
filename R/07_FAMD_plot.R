library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(FactoMineR)
library(factoextra)
library(patchwork)
library(scales)
library(stringr)

# ============================================================
# CONFIGURATION
# ============================================================

input_file <- "E:/PhD/7th Semester/Modelling for 2nd chapter/Fishing distance model/model1_distance.csv"
output_dir <- getwd()
set.seed(123)

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

binary_vars <- c("AIS", "VMS", "navigation.apps", "target.sharks", "subsidy")
categorical_vars <- c("Top.species.group", "reg.awareness", "Site")
continuous_vars <- c("mean_distance", "Fishing.experience", "vessel.length", "cost.per.trip")

display_labels <- c(
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

# ============================================================
# HELPER FUNCTIONS
# ============================================================

pretty_level <- function(x) {
  x <- gsub("([a-z])([A-Z])", "\\1 \\2", x)
  x <- gsub("_", " ", x)
  x <- gsub("\\.", " ", x)
  x <- gsub("Some what", "Somewhat", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x <- tools::toTitleCase(tolower(x))
  
  ifelse(nchar(x) == 1, toupper(x), x)
}

wrap_text <- function(x, width = 40) {
  vapply(
    x,
    function(value) paste(strwrap(as.character(value), width = width), collapse = "\n"),
    character(1)
  )
}

clean_binary <- function(dt, vars) {
  vars_present <- intersect(vars, names(dt))
  
  for (v in vars_present) {
    dt[, (v) := trimws(as.character(get(v)))]
    dt[get(v) == "0", (v) := "No"]
    dt[get(v) == "1", (v) := "Yes"]
    dt[, (v) := factor(get(v), levels = c("No", "Yes"))]
  }
  
  dt
}

clean_factors <- function(dt, vars) {
  vars_present <- intersect(vars, names(dt))
  
  for (v in vars_present) {
    dt[, (v) := factor(trimws(as.character(get(v))))]
  }
  
  dt
}

# Unified theme with major grid lines removed and tighter y-axis margins
analysis_theme <- function() {
  theme_minimal(base_size = 12.5) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(), # Removes grid lines completely
      panel.background = element_rect(fill = "#FFFFFF", color = NA),
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      legend.background = element_rect(fill = "#FFFFFF", color = NA),
      legend.key = element_rect(fill = "#FFFFFF", color = NA),
      strip.background = element_rect(fill = "#F3F4F6", color = NA),
      strip.text = element_text(face = "bold", color = "#1F2937"),
      axis.title.x = element_text(face = "bold", color = "#111827", margin = margin(t = 10)),
      
      # Pulls the Y-axis label much closer to the plot
      axis.title.y = element_text(face = "bold", color = "#111827", margin = margin(r = 8)), 
      
      axis.text = element_text(color = "#374151"),
      legend.title = element_text(face = "bold", color = "#111827"),
      legend.text = element_text(color = "#374151"),
      plot.title = element_blank(),
      plot.tag = element_text(face = "bold", size = 18, color = "#111827"),
      plot.margin = margin(15, 15, 15, 15)
    )
}

quanti_descriptor_label <- function(feature, mean_cluster, mean_overall) {
  feature_label <- display_labels[[feature]]
  
  if (feature == "mean_distance") {
    if (mean_cluster >= mean_overall) return("Greater fishing distance")
    return("Shorter fishing distance")
  }
  
  if (feature == "Fishing.experience") {
    if (mean_cluster >= mean_overall) return("More fishing experience")
    return("Less fishing experience")
  }
  
  if (feature == "vessel.length") {
    if (mean_cluster >= mean_overall) return("Longer vessels")
    return("Shorter vessels")
  }
  
  if (feature == "cost.per.trip") {
    if (mean_cluster >= mean_overall) return("Higher cost per trip")
    return("Lower cost per trip")
  }
  
  if (mean_cluster >= mean_overall) {
    paste("Higher", feature_label)
  } else {
    paste("Lower", feature_label)
  }
}

extract_quanti_profiles <- function(hcpc_res) {
  bind_rows(lapply(names(hcpc_res$desc.var$quanti), function(cluster_id) {
    x <- hcpc_res$desc.var$quanti[[cluster_id]]
    
    if (is.null(x) || nrow(x) == 0) {
      return(NULL)
    }
    
    df <- as.data.frame(x)
    df$Feature <- rownames(df)
    df$Cluster <- as.integer(cluster_id)
    df
  })) %>%
    rename(
      mean_cluster = `Mean in category`,
      mean_overall = `Overall mean`,
      sd_cluster = `sd in category`,
      sd_overall = `Overall sd`,
      p_value = `p.value`,
      v_test = `v.test`
    ) %>%
    mutate(
      Descriptor = mapply(
        quanti_descriptor_label,
        Feature,
        mean_cluster,
        mean_overall,
        USE.NAMES = FALSE
      ),
      Type = "Continuous",
      Score = abs(v_test),
      Detail = sprintf("Cluster mean = %.2f", mean_cluster)
    ) %>%
    select(Cluster, Descriptor, Type, Score, Detail, p_value)
}

extract_category_profiles <- function(hcpc_res) {
  bind_rows(lapply(names(hcpc_res$desc.var$category), function(cluster_id) {
    x <- hcpc_res$desc.var$category[[cluster_id]]
    
    if (is.null(x) || nrow(x) == 0) {
      return(NULL)
    }
    
    df <- as.data.frame(x)
    df$FeatureLevel <- rownames(df)
    df$Cluster <- as.integer(cluster_id)
    df
  })) %>%
    rename(
      within_level = `Cla/Mod`,
      within_cluster = `Mod/Cla`,
      global_share = `Global`,
      p_value = `p.value`,
      v_test = `v.test`
    ) %>%
    mutate(
      Feature = sub("=.*$", "", FeatureLevel),
      Level = sub("^.*?=", "", FeatureLevel),
      FeatureLabel = unname(display_labels[Feature]),
      LevelLabel = pretty_level(Level),
      Descriptor = paste0(FeatureLabel, ": ", LevelLabel),
      Type = "Categorical",
      Score = v_test,
      Detail = sprintf("%.1f%% of cluster", within_cluster)
    ) %>%
    filter(v_test > 0) %>%
    select(Cluster, Descriptor, Type, Score, Detail, p_value)
}

# ============================================================
# DATA PREPARATION
# ============================================================

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

dt <- fread(input_file)
setnames(dt, make.names(names(dt)))

dt[, ID := as.character(ID)]
dt[, mean_distance := as.numeric(mean_distance)]
dt[, Fishing.experience := as.numeric(Fishing.experience)]
dt[, vessel.length := as.numeric(vessel.length)]
dt[, cost.per.trip := as.numeric(cost.per.trip)]

dt <- dt[!is.na(mean_distance) & mean_distance > 0]
dt <- clean_binary(dt, binary_vars)
dt <- clean_factors(dt, categorical_vars)

analysis_dt <- dt %>%
  select(ID, all_of(famd_predictors)) %>%
  drop_na()

famd_dt <- analysis_dt %>%
  select(all_of(famd_predictors))

# ============================================================
# FAMD + HCPC
# ============================================================

res_famd <- FactoMineR::FAMD(famd_dt, graph = FALSE)
res_hcpc <- FactoMineR::HCPC(res_famd, nb.clust = -1, consol = TRUE, graph = FALSE)

cluster_assignments <- analysis_dt %>%
  mutate(Cluster = factor(res_hcpc$data.clust$clust))

cluster_sizes <- cluster_assignments %>%
  count(Cluster, name = "n") %>%
  arrange(Cluster)

cluster_labels <- setNames(
  paste0("Cluster ", cluster_sizes$Cluster, " (n = ", cluster_sizes$n, ")"),
  cluster_sizes$Cluster
)

cluster_palette <- setNames(
  hcl.colors(length(cluster_labels), palette = "Dark 3"),
  names(cluster_labels)
)

cluster_assignments <- cluster_assignments %>%
  mutate(
    ClusterLabel = factor(cluster_labels[as.character(Cluster)], levels = unname(cluster_labels))
  )

# ============================================================
# PANEL A: FAMD MAP
# ============================================================

dim1_var <- round(res_famd$eig[1, 2], 1)
dim2_var <- round(res_famd$eig[2, 2], 1)

ind_plot_df <- as.data.frame(res_famd$ind$coord[, 1:2, drop = FALSE])
ind_plot_df$Cluster <- cluster_assignments$Cluster
ind_plot_df$ClusterLabel <- cluster_assignments$ClusterLabel

centroids_df <- ind_plot_df %>%
  group_by(Cluster, ClusterLabel) %>%
  summarise(
    Dim.1 = mean(Dim.1),
    Dim.2 = mean(Dim.2),
    .groups = "drop"
  )

panel_a <- ggplot(ind_plot_df, aes(x = Dim.1, y = Dim.2, color = Cluster)) +
  stat_ellipse(aes(fill = Cluster), geom = "polygon", alpha = 0.14, color = NA, level = 0.8) +
  geom_point(size = 2.2, alpha = 0.82) +
  geom_hline(yintercept = 0, linewidth = 0.35, color = "#D1D5DB") +
  geom_vline(xintercept = 0, linewidth = 0.35, color = "#D1D5DB") +
  geom_label_repel(
    data = centroids_df,
    aes(label = ClusterLabel),
    fill = "white",
    color = "#111827",
    fontface = "bold",
    label.size = 0.15,
    size = 3.5,
    show.legend = FALSE,
    min.segment.length = 0
  ) +
  scale_color_manual(values = cluster_palette, labels = cluster_labels) +
  scale_fill_manual(values = cluster_palette, guide = "none") +
  labs(
    x = paste0("FAMD Dimension 1 (", dim1_var, "%)"),
    y = paste0("FAMD Dimension 2 (", dim2_var, "%)")
  ) +
  analysis_theme() +
  theme(
    legend.position = "none" 
  )

# ============================================================
# PANEL B: FISHING DISTANCE DISTRIBUTION BY CLUSTER
# ============================================================

panel_b <- ggplot(cluster_assignments, aes(x = ClusterLabel, y = mean_distance, fill = Cluster)) +
  geom_boxplot(
    alpha = 0.8, 
    outlier.shape = 21, 
    outlier.size = 2, 
    outlier.fill = "white",
    color = "#1F2937"
  ) +
  scale_fill_manual(values = cluster_palette, guide = "none") +
  labs(
    x = NULL,
    y = "Average Fishing Distance"
  ) +
  analysis_theme() +
  theme(
    axis.text.x = element_text(face = "bold", angle = 45, hjust = 1)
  )

# ============================================================
# PANEL C: CLUSTER PROFILE BUBBLE PLOT
# ============================================================

quanti_profiles <- extract_quanti_profiles(res_hcpc)
category_profiles <- extract_category_profiles(res_hcpc)

profile_top <- bind_rows(quanti_profiles, category_profiles) %>%
  filter(p_value <= 0.05) %>%
  group_by(Cluster) %>%
  arrange(desc(abs(Score)), .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  mutate(
    Cluster = factor(Cluster),
    ClusterLabel = factor(
      cluster_labels[as.character(Cluster)],
      levels = unname(cluster_labels)
    ),
    Strength = abs(Score),
    StrengthClass = cut(
      abs(Score),
      breaks = c(-Inf, 2, 4, 6, Inf),
      labels = c("Low", "Moderate", "High", "Very high"),
      right = TRUE
    ),
    DescriptorWrapped = wrap_text(Descriptor, width = 40)
  )

profile_top <- profile_top %>%
  mutate(
    DescriptorBase = Descriptor %>%
      gsub("\\s*=\\s*Yes$", "", .) %>%
      gsub("\\s*=\\s*No$", "", .) %>%
      gsub("\\s*:\\s*Yes$", "", .) %>%
      gsub("\\s*:\\s*No$", "", .) %>%
      gsub("\\s*Yes$", "", .) %>%
      gsub("\\s*No$", "", .),
    BinaryLevel = case_when(
      grepl("Yes", Descriptor, ignore.case = TRUE) ~ "Yes",
      grepl("No", Descriptor, ignore.case = TRUE) ~ "No",
      TRUE ~ ""
    )
  )

profile_bubble_df <- expand.grid(
  DescriptorWrapped = unique(profile_top$DescriptorWrapped),
  ClusterLabel = levels(profile_top$ClusterLabel),
  stringsAsFactors = FALSE
) %>%
  left_join(
    profile_top %>%
      select(
        DescriptorWrapped, Descriptor, DescriptorBase, BinaryLevel,
        ClusterLabel, Score, Strength, StrengthClass, Type
      ),
    by = c("DescriptorWrapped", "ClusterLabel")
  )

descriptor_order <- profile_top %>%
  group_by(DescriptorBase, DescriptorWrapped, BinaryLevel) %>%
  summarise(
    MaxStrength = max(Strength, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    BinaryOrder = case_when(
      BinaryLevel == "No" ~ 1,
      BinaryLevel == "Yes" ~ 2,
      TRUE ~ 3
    )
  ) %>%
  arrange(DescriptorBase, BinaryOrder, desc(MaxStrength))

profile_bubble_df$DescriptorWrapped <- factor(
  profile_bubble_df$DescriptorWrapped,
  levels = rev(unique(descriptor_order$DescriptorWrapped))
)

panel_c <- ggplot(
  profile_bubble_df,
  aes(
    x = ClusterLabel,
    y = DescriptorWrapped
  )
) +
  geom_point(
    aes(
      size = StrengthClass,
      fill = StrengthClass
    ),
    shape = 21,
    colour = "#1F2937",
    stroke = 0.25,
    alpha = 0.90,
    na.rm = TRUE
  ) +
  geom_label(
    aes(label = ifelse(is.na(Score), "", sprintf("%.1f", Score))),
    fill = "white",
    colour = "#111827",
    fontface = "bold",
    label.size = 0.12,
    label.padding = unit(0.10, "lines"),
    size = 3.0,
    na.rm = TRUE
  ) +
  scale_size_manual(
    values = c(
      "Low" = 5,
      "Moderate" = 7.5,
      "High" = 10,
      "Very high" = 13
    ),
    name = "Descriptor\nstrength",
    na.translate = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "Low" = "#CFE8F3",
      "Moderate" = "#6BAED6",
      "High" = "#2171B5",
      "Very high" = "#08306B"
    ),
    name = "Descriptor\nstrength",
    na.translate = FALSE
  ) +
  guides(
    size = guide_legend(
      title = "Descriptor\nstrength",
      override.aes = list(shape = 21, colour = "#1F2937")
    ),
    fill = guide_legend(
      title = "Descriptor\nstrength",
      override.aes = list(shape = 21, colour = "#1F2937")
    )
  ) +
  labs(x = NULL, y = NULL) +
  analysis_theme() +
  theme(
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = 9.5, lineheight = 0.85),
    legend.position = "right",
    legend.box = "vertical"
  )

# ============================================================
# COMBINE PANELS WITH PATCHWORK
# ============================================================

# Adjusted heights: Gives more relative vertical space to Panels A and B (1 vs 1.25)
combined_plot <- (panel_a | panel_b) / panel_c + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(heights = c(1, 1.25), widths = c(1, 1)) 

# ============================================================
# SAVE THE PLOT
# ============================================================

output_filename <- file.path(output_dir, "cluster_analysis_results.png")

# Exporting at 800 dpi for high-quality publication output
ggsave(
  filename = output_filename,
  plot = combined_plot,
  width = 12,
  height = 12, 
  dpi = 800,
  bg = "white"
)

message(paste("Plot saved to:", output_filename))
