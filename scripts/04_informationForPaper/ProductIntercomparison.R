rm(list=ls())
renv::activate()
library(dplyr)
all_statistics <-"T:/DLR-DFD/Analysis3/Results_10percDisrMay2026/all_stats_df.csv" 


all_stats_df <- read.csv(all_statistics, header = TRUE, sep = ",")
unique(all_stats_df$Dataset)
all_stats_df_subset <- all_stats_df %>%
  filter(Dataset != "Li_no_frozen") %>% 
  mutate(Dataset = recode(Dataset, "Li-strict" = "LSE", "NASAFlood" = "NRT-FP", "Arlie" = "ARLIE" ))
unique(all_stats_df_subset$Dataset)
glimpse(all_stats_df_subset) 

library(ggplot2)

# define colors for each dataset
dataset_colors <- c("ARLIE" = "#E69F00", "LSE" = "#56B4E9", "NRT-FP"="#009E73")

# facet labels
facet_labels <- c(
  "Area-perc" = "Area [%]",
  "Area-normalized" = "Area [z-score]"
)
library(patchwork)

# Linientypen pro Dataset (zusätzlich zur Farbe)
dataset_linetypes <- c("ARLIE" = "solid", "LSE" = "dashed", "NRT-FP" = "dotdash")

# Gemeinsame y-Skala
y_scale <- scale_y_continuous(
  limits = c(0, 1),
  breaks = seq(0, 1, 0.1),
  labels = c("0.0","","0.2","","0.4","","0.6","","0.8","","1.0")
)

# Rahmen + Grundtheme in einem Objekt (an jeden Plot gehängt)
framed_theme <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )

# --- Spearman (linkes Panel) ---
p_spearman <- ggplot(all_stats_df_subset %>% filter(value.type == "Area-perc"),
                     aes(x = spearman_cor, color = Dataset, linetype = Dataset)) +
  stat_ecdf(linewidth = 0.8) +
  scale_color_manual(values = dataset_colors) +
  scale_linetype_manual(values = dataset_linetypes) +
  scale_x_continuous(breaks = seq(-1, 1, 0.2)) +
  y_scale +
  labs(x = "Spearman correlation", y = "Cumulative proportion",
       title = "Spearman correlation") +
  framed_theme

# --- RMSE-Panels ---
make_rmse_plot <- function(vt, show_y = FALSE) {
  ggplot(all_stats_df_subset %>% filter(RMSE > 0, value.type == vt),
         aes(x = RMSE, color = Dataset, linetype = Dataset)) +
    stat_ecdf(linewidth = 0.8) +
    scale_x_log10() +
    scale_color_manual(values = dataset_colors) +
    scale_linetype_manual(values = dataset_linetypes) +
    y_scale +
    labs(x = "RMSE (log scale)", y = NULL, title = facet_labels[[vt]]) +
    framed_theme +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

p_rmse_area <- make_rmse_plot("Area-perc")
p_rmse_norm <- make_rmse_plot("Area-normalized")

# --- Kombiniert ---
p_combined <- (p_spearman | p_rmse_area | p_rmse_norm) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = "bottom")

p_combined

ggsave("T:/DLR-DFD/DFD-GWPComparison/Results_10percDisr/Results_10percDisr/ecdf_validation.png",
       p_combined, width = 12, height = 4.5, dpi = 300)












p_spearman <- ggplot(all_stats_df_subset %>% filter(value.type == "Area-perc"), aes(x = spearman_cor, color = Dataset)) +
stat_ecdf(linewidth = 0.8) +
  scale_color_manual(values = dataset_colors) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.1),
    labels = c("", "0.2", "", "0.4", "", "0.6", "", "0.8", "", "1.0", "")
  ) +
  labs(x = "Spearman correlation", y = "Cumulative proportion") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
p_spearman

# --- ECall_stats_df_subset: RMSE ---
p_rmse <- ggplot(all_stats_df_subset %>% filter(RMSE > 0), aes(x = RMSE, color = Dataset)) +
  stat_ecdf(linewidth = 0.8) +
  facet_wrap(~value.type, labeller = labeller(value.type = facet_labels), scales = "free_x") +
  scale_x_log10() +
  scale_color_manual(values = dataset_colors) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.1),
    labels = c("0.0", "", "0.2", "", "0.4", "", "0.6", "", "0.8", "", "1.0")
  )+
  labs(x = "RMSE (log scale)", y = "Cumulative proportion") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p_rmse
library(patchwork)

p_combined <- p_spearman / p_rmse +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = "bottom")
p_combined
# --- Save ---
ggsave("T:/DLR-DFD/DFD-GWPComparison/Results_10percDisr/Results_10percDisr/ecdf_validation.png", p_combined, width = 8, height = 7, dpi = 300)


summary <- all_stats_df_subset %>%
  group_by(Dataset, value.type) %>%
  summarise(
    median_cor = median(spearman_cor, na.rm = TRUE),
    q25_cor = quantile(spearman_cor, 0.25, na.rm = TRUE),
    q75_cor = quantile(spearman_cor, 0.75, na.rm = TRUE),
    median_rmse = median(RMSE, na.rm = TRUE),
    q25_rmse = quantile(RMSE, 0.25, na.rm = TRUE),
    q75_rmse = quantile(RMSE, 0.75, na.rm = TRUE),
    n = n()
  )
summary

spearman_interpretation <- all_stats_df_subset %>%
  filter(value.type == "Area-perc") %>%
  group_by(Dataset) %>%
  summarise(
    prop_above_0.5 = mean(spearman_cor > 0.5),
    prop_above_0.7 = mean(spearman_cor > 0.7)
  )
spearman_interpretation

rmse_perc <- all_stats_df_subset %>%
  filter(value.type == "Area-perc") %>%
  group_by(Dataset) %>%
  summarise(
    prop_below_5 = mean(RMSE < 5),
    prop_below_10 = mean(RMSE < 10),
    median_rmse = median(RMSE),
    q25_rmse = quantile(RMSE, 0.25),
    q75_rmse = quantile(RMSE, 0.75)
  )

rmse_perc

rmse_zscore <- all_stats_df_subset %>%
  filter(value.type == "Area-normalized") %>%
  group_by(Dataset) %>%
  summarise(
    prop_below_5 = mean(RMSE < 5),
    prop_below_10 = mean(RMSE < 10),
    median_rmse = median(RMSE),
    q25_rmse = quantile(RMSE, 0.25),
    q75_rmse = quantile(RMSE, 0.75)
  )
rmse_zscore
# --- Compute all metrics ---
# Correlation thresholds (only need one value.type since they're identical)
cor_stats <- all_stats_df_subset %>%
  filter(value.type == "Area-perc") %>%
  group_by(Dataset) %>%
  summarise(
    n = n(),
    median_cor = round(median(spearman_cor, na.rm = TRUE), 2),
    prop_above_0.5 = round(mean(spearman_cor > 0.5) * 100, 1),
    prop_above_0.7 = round(mean(spearman_cor > 0.7) * 100, 1),
    prop_negative = round(mean(spearman_cor < 0) * 100, 1)
  )

# RMSE Area [%]
rmse_perc_stats <- all_stats_df_subset %>%
  filter(value.type == "Area-perc") %>%
  group_by(Dataset) %>%
  summarise(
    median_rmse_perc = round(median(RMSE, na.rm = TRUE), 2),
    iqr_rmse_perc = paste0(round(quantile(RMSE, 0.25), 2), "–", round(quantile(RMSE, 0.75), 2)),
    prop_below_5 = round(mean(RMSE < 5) * 100, 1),
    prop_below_10 = round(mean(RMSE < 10) * 100, 1)
  )

# Standard deviations
sd_stats <- all_stats_df_subset %>%
  filter(value.type == "Area-perc") %>%
  group_by(Dataset) %>%
  summarise(
    median_gwp_sd = round(median(gwp_stdev, na.rm = TRUE), 2),
    median_val_sd = round(median(val_stdev, na.rm = TRUE), 2)
  )

# RMSE z-score
rmse_z_stats <- all_stats_df_subset %>%
  filter(value.type == "Area-normalized") %>%
  group_by(Dataset) %>%
  summarise(
    median_rmse_z = round(median(RMSE, na.rm = TRUE), 3),
    iqr_rmse_z = paste0(round(quantile(RMSE, 0.25), 3), "–", round(quantile(RMSE, 0.75), 3))
  )

# --- Combine into one table ---
summary_table <- cor_stats %>%
  left_join(rmse_perc_stats, by = "Dataset") %>%
  left_join(sd_stats, by = "Dataset") %>%
  left_join(rmse_z_stats, by = "Dataset") %>%
  left_join(temp_res, by = "Dataset")

# Reorder columns
summary_table <- summary_table %>%
  select(Dataset, n, temp_resolution,
         median_cor, prop_above_0.5, prop_above_0.7, prop_negative,
         median_rmse_perc, iqr_rmse_perc, prop_below_5, prop_below_10,
         median_gwp_sd, median_val_sd,
         median_rmse_z, iqr_rmse_z)

View(summary_table)

library(ggplot2)
library(dplyr)

# --- Data ---
data <- tribble(
  ~Dataset,   ~Start1,       ~End1,         ~Start2,       ~End2,
  "GWP",      "01.01.2003",  "31.12.2024",  NA,            NA,
  "ARLIE",    "01.09.2016",  "01.04.2025",  "01.09.2016",  "31.12.2024",
  "LSE",      "01.01.2001",  "31.12.2023",  "01.01.2003",  "31.12.2023",
  "NRT-FP",   "01.01.2010",  "31.12.2025",  "01.01.2010",  "31.12.2010",
  "NRT-FP",   "01.01.2010",  "31.12.2025",  "01.01.2021",  "31.12.2021"
)

parse_date <- function(x) as.Date(x, format = "%d.%m.%Y")
data <- data |>
  mutate(across(c(Start1, End1, Start2, End2), parse_date))

# --- Colors ---
ds_colors <- c(ARLIE = "#72B6A1", LSE = "#95A3C3", `NRT-FP` = "#E99675")

datasets  <- c("ARLIE", "LSE", "NRT-FP")
y_pos     <- setNames(seq_along(datasets) - 1, datasets)

gwp <- data |> filter(Dataset == "GWP")

light_segs <- data |>
  filter(Dataset != "GWP") |>
  group_by(Dataset) |>
  summarise(xmin = min(Start1), xmax = max(End1), .groups = "drop") |>
  mutate(y = y_pos[Dataset], color = ds_colors[Dataset])

dark_segs <- data |>
  filter(Dataset != "GWP", !is.na(Start2), !is.na(End2)) |>
  mutate(y = y_pos[Dataset], color = ds_colors[Dataset])

x_breaks <- seq(as.Date("2000-01-01"), as.Date("2026-01-01"), by = "year")

# --- Plot ---
p <- ggplot() +
  # GWP grey background
  annotate("rect",
    xmin = gwp$Start1, xmax = gwp$End1,
    ymin = -Inf, ymax = Inf,
    fill = "lightgrey", alpha = 0.4
  ) +
  # Vertical grid lines (white, visible on grey GWP background)
  geom_vline(xintercept = x_breaks, linetype = "dashed",
             color = "white", linewidth = 0.3) +
  # Horizontal dashed lines per dataset
  geom_hline(yintercept = y_pos, linetype = "dashed",
             color = "grey50", linewidth = 0.3) +
  # Light segments (full coverage)
  geom_segment(
    data = light_segs,
    aes(x = xmin, xend = xmax, y = y, yend = y, color = Dataset),
    linewidth = 8, alpha = 0.3, lineend = "butt"
  ) +
  # Dark segments (used periods)
  geom_segment(
    data = dark_segs,
    aes(x = Start2, xend = End2, y = y, yend = y, color = Dataset),
    linewidth = 8, alpha = 1.0, lineend = "butt"
  ) +
  # Dataset labels on y-axis (instead of legend)
  scale_y_continuous(
    breaks = y_pos,
    labels = names(y_pos),
    expand = expansion(add = 0.5)
  ) +
  scale_color_manual(values = ds_colors, guide = "none") +
  scale_x_date(
    breaks = x_breaks,
    date_labels = "%Y",
    limits = range(x_breaks)
  ) +
  labs(
    x = "Year",
    y = NULL,
    title = "Temporal Data Coverage (2000–2025)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text          = element_text(color = "black"),
    axis.text     = element_text(color = "black"),
    axis.text.x   = element_text(angle = 45, hjust = 1, color = "black"),
    axis.title    = element_text(color = "black", face = "bold"),
    plot.title    = element_text(hjust = 0.5, color = "black", face = "bold"),
    panel.grid    = element_blank(),
    panel.border  = element_rect(color = "black", fill = NA, linewidth = 0.6),
    legend.position = "none"
  )



p
ggsave("T:/DLR-DFD/PlotsForPaper/temporal_coverage.png", plot = p, width = 16, height = 4.2, units = "cm", dpi = 300)
