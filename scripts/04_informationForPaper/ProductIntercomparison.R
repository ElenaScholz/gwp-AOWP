rm(list=ls())
renv::activate()
library(dplyr)
all_statistics <-"T:/DLR-DFD/DFD-GWPComparison/Results_10percDisr/Results_10percDisr/all_stats_df.csv" 

all_stats_df <- read.csv(all_statistics, header = TRUE, sep = ",")
all_stats_df_subset <- all_stats_df %>%
  filter(Dataset != "Li") %>% 
  mutate(Dataset = recode(Dataset, "Li_no_frozen" = "LSE", "NASAFlood" = "MFP", "Arlie" = "ARLIE" ))
glimpse(all_stats_df_subset) 

library(ggplot2)

# define colors for each dataset
dataset_colors <- c("ARLIE" = "#E69F00", "LSE" = "#56B4E9", "MFP"="#009E73")

# facet labels
facet_labels <- c(
  "Area-perc" = "Area [%]",
  "Area-normalized" = "Area [z-score]"
)
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

# RMSE z-score
rmse_z_stats <- all_stats_df_subset %>%
  filter(value.type == "Area-normalized") %>%
  group_by(Dataset) %>%
  summarise(
    median_rmse_z = round(median(RMSE, na.rm = TRUE), 3),
    iqr_rmse_z = paste0(round(quantile(RMSE, 0.25), 3), "–", round(quantile(RMSE, 0.75), 3))
  )

# --- Combine into one table ---
# Add temporal resolution
temp_res <- data.frame(
  Dataset = c("ARLIE", "LSE", "MFP"),
  temp_resolution = c("daily", "monthly", "daily")
)

summary_table <- cor_stats %>%
  left_join(rmse_perc_stats, by = "Dataset") %>%
  left_join(rmse_z_stats, by = "Dataset") %>%
  left_join(temp_res, by = "Dataset")

# Reorder columns
summary_table <- summary_table %>%
  select(Dataset, n, temp_resolution,
         median_cor, prop_above_0.5, prop_above_0.7, prop_negative,
         median_rmse_perc, iqr_rmse_perc, prop_below_5, prop_below_10,
         median_rmse_z, iqr_rmse_z)

print(summary_table)
