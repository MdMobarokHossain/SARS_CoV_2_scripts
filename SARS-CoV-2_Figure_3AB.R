library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(cowplot)
library(ggthemes)
library(viridis)
library(ggrepel)

# Set the locale to English for date formatting
Sys.setlocale("LC_TIME", "C")

# Load the corrected metadata file
metadata_file <- "corrected_metadata_with_who_variant.tsv"
df_bd <- read.csv(metadata_file, sep="\t")

# Filter the data to include only the WHO variant "Omicron"
df_bd_omicron <- df_bd %>%
  filter(WHO_variant == "Omicron")

# Find the top 5 pangolin lineages within "Omicron"
top_pangolin_lineages_omicron <- df_bd_omicron %>%
  count(pangolin_lineage) %>%
  top_n(5, n) %>%
  pull(pangolin_lineage)

# Filter the data to include only these top pangolin lineages within "Omicron"
df_bd_omicron <- df_bd_omicron %>%
  filter(pangolin_lineage %in% top_pangolin_lineages_omicron)

# Create the date columns
df_bd_omicron$date <- as.Date(df_bd_omicron$date)
df_bd_omicron$days <- as.Date(cut(df_bd_omicron$date, breaks = "day"))
df_bd_omicron$date2 <- as.Date(cut(df_bd_omicron$date, breaks = "2 weeks"))
df_bd_omicron$date3 <- as.Date(cut(df_bd_omicron$date, breaks = "1 month"))

# Filter the data to include only the WHO variant "Delta"
df_bd_delta <- df_bd %>%
  filter(WHO_variant == "Delta")

# Find the top 5 pangolin lineages within "Delta"
top_pangolin_lineages_delta <- df_bd_delta %>%
  count(pangolin_lineage) %>%
  top_n(5, n) %>%
  pull(pangolin_lineage)

# Filter the data to include only these top pangolin lineages within "Delta"
df_bd_delta <- df_bd_delta %>%
  filter(pangolin_lineage %in% top_pangolin_lineages_delta)

# Create the date columns
df_bd_delta$date <- as.Date(df_bd_delta$date)
df_bd_delta$days <- as.Date(cut(df_bd_delta$date, breaks = "day"))
df_bd_delta$date2 <- as.Date(cut(df_bd_delta$date, breaks = "2 weeks"))
df_bd_delta$date3 <- as.Date(cut(df_bd_delta$date, breaks = "1 month"))

# Determine the overall date range for the x-axis limits
overall_min_date <- min(df_bd_delta$date, df_bd_omicron$date)
overall_max_date <- max(df_bd_delta$date, df_bd_omicron$date)

# the plot for Omicron
plot_omicron <- ggplot(data = df_bd_omicron) + 
  theme_classic() +
  geom_segment(aes(x = overall_min_date, y = reorder(division, division, function(x) length(unique(x))), xend = overall_max_date, yend = division, group = division), colour = "grey80", size = 10) +
  geom_point(aes(x = days, y = reorder(division, division, function(x) length(unique(x))), fill = pangolin_lineage), position = position_jitter(width = 0.2, height = 0.2), stroke = 0.2, shape = 21, col = 'black', size = 4, alpha = 1) +
  ylab('') + 
  xlab('month') + 
  ggtitle('Bangladesh with sequencing data (Omicron)') +
  scale_fill_viridis_d(option = "plasma", name = 'Top 5 Lineages of Omicron') +
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 10)) +
  theme(axis.title.x = element_text(color = "black", size = 15, face = "bold")) +
  theme(axis.text.x = element_text(color = "black", size = 8)) +
  theme(axis.title.y = element_text(color = "black", size = 15, face = "bold")) +
  theme(axis.text.y = element_text(color = "black", size = 12)) +
  theme(axis.text.x = element_text(color = "black", size = 11)) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 month", limits = c(overall_min_date, overall_max_date))

# the plot for Delta with different colors
plot_delta <- ggplot(data = df_bd_delta) + 
  theme_classic() +
  geom_segment(aes(x = overall_min_date, y = reorder(division, division, function(x) length(unique(x))), xend = overall_max_date, yend = division, group = division), colour = "grey80", size = 10) +
  geom_point(aes(x = days, y = reorder(division, division, function(x) length(unique(x))), fill = pangolin_lineage), position = position_jitter(width = 0.2, height = 0.2), stroke = 0.2, shape = 21, col = 'black', size = 4, alpha = 1) +
  ylab('') + 
  xlab('month') + 
  ggtitle('Bangladesh with sequencing data (Delta)') +
  scale_fill_viridis_d(option = "magma", name = 'Top 5 Lineages of Delta') +  # Different color set
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 10)) +
  theme(axis.title.x = element_text(color = "black", size = 15, face = "bold")) +
  theme(axis.text.x = element_text(color = "black", size = 8)) +
  theme(axis.title.y = element_text(color = "black", size = 15, face = "bold")) +
  theme(axis.text.y = element_text(color = "black", size = 12)) +
  theme(axis.text.x = element_text(color = "black", size = 11)) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 month", limits = c(overall_min_date, overall_max_date))

# Combine the two plots using cowplot
combined_plot <- plot_grid(
  plot_delta + ggtitle(" Delta"),
  plot_omicron + ggtitle("Omicron"),
  labels = c("A", "B"),
  ncol = 1
)

# Display the combined plot
print(combined_plot)
# Save the combined plot with specified resolution
output_file <- "combined_plot.png"

# Specify the resolution (dpi) and dimensions (in inches)
dpi <- 300
width <- 16 # in inches
height <- 9 # in inches

# Save the plot
ggsave(filename = output_file, plot = combined_plot, dpi = dpi, width = width, height = height)

# To display the saved plot
system(paste("open", output_file))
