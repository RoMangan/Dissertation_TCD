#Ireland LULC frnm National Landcover Data
library(ggplot2)
library(grid)

# Data Preparation
Level_1VA <- c("Artificial surfaces", "Exposed surfaces", "Cultivated land", 
               "Forest, woodland and scrub", "Grassland, swamp and saltmarsh", 
               "Peatland", "Heath and bracken", "Waterbodies")
percentages <- c(3.80, 1.88, 6.05, 18.27, 54.21, 6.55, 6.47, 2.79)
colors <- c("black", "magenta", "darkgreen", "yellow", "#99CC00", "chocolate4", "purple", "#0066CC")

# Create a data frame
land_cover_data <- data.frame(
  land_cover = Level_1VA,
  percentages = percentages,
  colors = colors
)

# Sort data frame by percentages (lowest at top)
land_cover_data <- land_cover_data[order(land_cover_data$percentages, decreasing = FALSE), ]

# Create the Horizontal Arrow Plot
library(ggplot2)
library(grid)  # arrow functionality

ggplot(land_cover_data, aes(x = percentages, y = land_cover)) +
  geom_segment(aes(x = 0, xend = percentages, y = land_cover, yend = land_cover, color = land_cover), 
               arrow = arrow(length = unit(1.0, "cm"), type = "closed"), 
               size = 1.2) +  # arrow thickness
  scale_x_continuous(limits = c(0, 60), expand = c(0, 0), breaks = seq(0, 60, 10)) +  # xaxis max at 60%
  scale_y_discrete(limits = rev(land_cover_data$land_cover)) +  # reverse the order for lowest datapoint at top
  scale_color_manual(values = setNames(land_cover_data$colors, land_cover_data$land_cover)) +  # map colours explicitly
  labs(
    title = "National landcover in Ireland",
    x = "Percentage of area (%)",
    y = "LULC classifcation"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),  # Add grid lines
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 14),  # Adjust y-axis title size
    axis.text.y = element_text(size = 12),   # Adjust y-axis text size
    axis.text.x = element_text(size = 12),   # Adjust x-axis text size
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Center and bold title
    legend.position = "none"  # Remove legend
  )





