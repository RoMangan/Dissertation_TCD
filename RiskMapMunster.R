#Risk Map Munster counties
# This script was developed by Rosie Mangan
# 
# 1 School of Computer Science and Statistics at Trinity College Dublin, Ireland.

# Script originated May 15, 2024
# Last modified Feb 12, 2025

# load libraries

library(sf)# sf is a library to read geospatial information, like maps
library(dplyr) # dplyr is a 
data manipulation library
library(stringr) #stingr is library to manipulate string type data
library(tidyverse)
library(sp)
library(raster) #geographic data analysis 
library(tmap) #thematic maps
library(tidyr)
library(ggplot2)
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD")

gpkg_file <- "Counties___OSi_National_Statutory_Boundaries_7992851692716515542.gpkg"
county_boundaries <- st_read(gpkg_file)
colnames(county_boundaries)
print(unique(county_boundaries$COUNTY))

Munster <-county_boundaries %>%
  filter(PROVINCE == "Munster")

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster")
list.files()

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Clare")
list.files()
Clare <-st_read("NCLM_Clare_Shapefile.shp") #loads


# Define 5x5 km grid
bounds <- st_bbox(Clare)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Clare))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Clare, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()


setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD")
gpkg_file <- "Counties___OSi_National_Statutory_Boundaries_7992851692716515542.gpkg"
county_boundaries <- st_read(gpkg_file)
colnames(county_boundaries)
unique(county_boundaries$COUNTY)

clare_district <- county_boundaries %>%
  filter(COUNTY == "CLARE")

ggplot() +
  geom_sf(data = clare_district, fill = "blue", color = "black") +
  labs(title = "District of Clare") +
  theme_minimal()



# Define the extent and create a grid over Clare district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(clare_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, clare_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
Clare_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Clare_municipal_with_risk)
unique(Clare_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = clare_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Clare with grid overlay")

# Plotting
Clare_map<-ggplot() +
  geom_sf(data = Leinster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Clare_municipal_with_risk[Clare_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Clare_municipal_with_risk[Clare_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Clae district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Clare_map)

saveRDS(Clare_municipal_with_risk, "Clare_municipal_with_risk.rds")

# Load it back later
Clare_risk <- readRDS("Clare_municipal_with_risk.rds")


setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Limerick")
list.files()
Limerick <-st_read("NLCM_Limerick_Shapefile.shp") #loads


# Define 5x5 km grid
bounds <- st_bbox(Limerick)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Limerick))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Limerick, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD")
gpkg_file <- "Counties___OSi_National_Statutory_Boundaries_7992851692716515542.gpkg"
county_boundaries <- st_read(gpkg_file)
colnames(county_boundaries)
unique(county_boundaries$COUNTY)

Limerick_district <- county_boundaries %>%
  filter(COUNTY == "LIMERICK")

ggplot() +
  geom_sf(data = Limerick_district, fill = "blue", color = "black") +
  labs(title = "District of Limerick") +
  theme_minimal()



# Define the extent and create a grid over Limerick district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Limerick_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Limerick_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
Limerick_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Limerick_municipal_with_risk)
unique(Limerick_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Limerick_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Limerick with grid overlay")

# Plotting
Limerick_map<-ggplot() +
  geom_sf(data = Leinster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Limerick_municipal_with_risk[Limerick_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Limerick_municipal_with_risk[Limerick_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Limerick district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Limerick_map)

saveRDS(Limerick_municipal_with_risk, "Limerick_municipal_with_risk.rds")

# Load it back later
Limerick_risk <- readRDS("Limerick_municipal_with_risk.rds")


setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Waterford")
list.files()
Waterford <-st_read("NLCM_Waterford_Shapefile.shp") #loads


# Define 5x5 km grid
bounds <- st_bbox(Waterford)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Waterford))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Waterford, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()


setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD")
gpkg_file <- "Counties___OSi_National_Statutory_Boundaries_7992851692716515542.gpkg"
county_boundaries <- st_read(gpkg_file)
colnames(county_boundaries)
unique(county_boundaries$COUNTY)

Waterford_district <- county_boundaries %>%
  filter(COUNTY == "WATERFORD")

ggplot() +
  geom_sf(data = Waterford_district, fill = "blue", color = "black") +
  labs(title = "District of Waterford") +
  theme_minimal()



# Define the extent and create a grid over Waterford district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Waterford_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Waterford_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
Waterford_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Waterford_municipal_with_risk)
unique(Waterford_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Waterford_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Waterford with grid overlay")

# Plotting
Waterford_map<-ggplot() +
  geom_sf(data = Munster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Waterford_municipal_with_risk[Waterford_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Waterford_municipal_with_risk[Waterford_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Waterford district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Waterford_map)

saveRDS(Waterford_municipal_with_risk, "Waterford_municipal_with_risk.rds")

# Load it back later
Waterford_risk <- readRDS("Waterford_municipal_with_risk.rds")

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Tipperary")
list.files()
Tipperary <-st_read("NLCM_Tipperary_Shapefile.shp") #loads


# Define 5x5 km grid
bounds <- st_bbox(Tipperary)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Tipperary))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Tipperary, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()


setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD")
gpkg_file <- "Counties___OSi_National_Statutory_Boundaries_7992851692716515542.gpkg"
county_boundaries <- st_read(gpkg_file)
colnames(county_boundaries)
unique(county_boundaries$COUNTY)

Tipperary_district <- county_boundaries %>%
  filter(COUNTY == "TIPPERARY")

ggplot() +
  geom_sf(data = Tipperary_district, fill = "blue", color = "black") +
  labs(title = "District of Tipperary") +
  theme_minimal()



# Define the extent and create a grid over Tipperary district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Tipperary_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Tipperary_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
Tipperary_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Tipperary_municipal_with_risk)
unique(Tipperary_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Tipperary_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Tipperary with grid overlay")

# Plotting
Tipperary_map<-ggplot() +
  geom_sf(data = Leinster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Tipperary_municipal_with_risk[Tipperary_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Tipperary_municipal_with_risk[Tipperary_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Tipperary district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

print(Tipperary_map)

saveRDS(Tipperary_municipal_with_risk, "Tipperary_municipal_with_risk.rds")

# Load it back later
Tipperary_risk <- readRDS("Tipperary_municipal_with_risk.rds")




#Kerry by municipal district
# Filter and list municipal districts in Kerry
Kerry_districts <- Municipal_Districts %>%
  filter(COUNTY == "KERRY") %>%
  dplyr::select(ENGLISH) %>% 
  distinct()  s

# Print the results to check
print(Kerry_districts)
#Municipal District of Castleisland-Corca Dhuibhne 
#Municipal District of Kenmare
#Municipal District of Listowel
#Municipal District of Killarney
#Municipal District of Killarney
                                                                                                                                                                                                          4                      Municipal District of Tralee MULTIPOLYGON (((473122.1 61...
                                                                                                                                                                                                                                                                              5                   Municipal District of Killarney


setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Kerry MD/Kerry_Castleisland")
list.files()
Castleisland<-st_read("NCLM_Castleisland_Shapefile.shp") #loads

# Define 5x5 km grid
bounds <- st_bbox(Castleisland)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Castleisland))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Castleisland, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()


gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

Castleisland_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Castleisland-Corca Dhuibhne")

ggplot() +
  geom_sf(data = Castleisland_district, fill = "blue", color = "black") +
  labs(title = "Kenmare district") +
  theme_minimal()



# Define the extent and create a grid over Kenmare district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Castleisland_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Castleisland_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
Castleisland_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Castleisland_municipal_with_risk)
unique(Castleisland_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Castleisland_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Castleisland with grid overlay")

# Plotting
Castleisland_map<-ggplot() +
  geom_sf(data = Munster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Castleisland_municipal_with_risk[Castleisland_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Castleisland_municipal_with_risk[Castleisland_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Castleisland district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

print(Castleisland_map)

saveRDS(Castleisland_municipal_with_risk, "Castleisland_municipal_with_risk.rds")

# Load it back later
Castleisland_risk <- readRDS("Castleisland_municipal_with_risk.rds")




setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Kerry MD/Kerry_Kenmare")
list.files()
Kenmare<-st_read("NLCM_Kenmare_Shapefile.shp") #loads

# Define 5x5 km grid
bounds <- st_bbox(Kenmare)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Kenmare))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Kenmare, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()


gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

Kenmare_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Kenmare")

ggplot() +
  geom_sf(data = Kenmare_district, fill = "blue", color = "black") +
  labs(title = "Kenmare district") +
  theme_minimal()



# Define the extent and create a grid over Kenmare district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Kenmare_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Kenmare_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
Kenmare_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Kenmare_municipal_with_risk)
unique(Kenmare_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Kenmare_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Kenmare with grid overlay")

# Plotting
Kenmare_map<-ggplot() +
  geom_sf(data = Munster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Kenmare_municipal_with_risk[Kenmare_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Kenmare_municipal_with_risk[Kenmare_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Kenmare district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

print(Kenmare_map)

saveRDS(Kenmare_municipal_with_risk, "Kenmare_municipal_with_risk.rds")

# Load it back later
Kenmare_risk <- readRDS("Kenmare_municipal_with_risk.rds")




setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Kerry MD/Kerry_Killarney") #worked
list.files()
Killarney<-st_read("NLCM_Killarney_Shapefile.shp") #loads



# Define 5x5 km grid
bounds <- st_bbox(Killarney)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Killarney))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Killarney, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()


gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

Killarney_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Killarney")

ggplot() +
  geom_sf(data = Killarney_district, fill = "blue", color = "black") +
  labs(title = "Killarney district") +
  theme_minimal()



# Define the extent and create a grid over Kenmare district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Killarney_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Killarney_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
Killarney_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Killarney_municipal_with_risk)
unique(Killarney_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Killarney_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Kenmare with grid overlay")

# Plotting
Killarney_map<-ggplot() +
  geom_sf(data = Munster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Killarney_municipal_with_risk[Killarney_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Killarney_municipal_with_risk[Killarney_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Killarney district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

print(Killarney_map)

saveRDS(Killarney_municipal_with_risk, "Killarney_municipal_with_risk.rds")

# Load it back later
Killarney_risk <- readRDS("Killarney_municipal_with_risk.rds")






setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Kerry MD/Kerry_Listowel") 
list.files()
Listowel<-st_read("NLCM_Listowel_Shapefile.shp") #loads
# Define 5x5 km grid
bounds <- st_bbox(Listowel)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Listowel))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Listowel, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()


gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

Listowel_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Listowel")

ggplot() +
  geom_sf(data = Listowel_district, fill = "blue", color = "black") +
  labs(title = "Listowel district") +
  theme_minimal()



# Define the extent and create a grid over Listowel district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Listowel_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Listowel_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
Listowel_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Listowel_municipal_with_risk)
unique(Listowel_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Listowel_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Listowel with grid overlay")

# Plotting
Listowel_map<-ggplot() +
  geom_sf(data = Munster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Listowel_municipal_with_risk[Listowel_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Listowel_municipal_with_risk[Listowel_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Listowel district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

print(Listowel_map)

saveRDS(Listowel_municipal_with_risk, "Listowel_municipal_with_risk.rds")

# Load it back later
Listowel_risk <- readRDS("Listowel_municipal_with_risk.rds")



setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Kerry MD/Kerry_Tralee")
list.files()
Tralee<-st_read("NLCM_Tralee_Shapefile.shp") #loads
# Define 5x5 km grid
bounds <- st_bbox(Tralee)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Tralee))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Tralee, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()


gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

tralee_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Tralee")

ggplot() +
  geom_sf(data = tralee_district, fill = "blue", color = "black") +
  labs(title = "Tralee district") +
  theme_minimal()



# Define the extent and create a grid over Tralee district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(tralee_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, tralee_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
Tralee_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Tralee_municipal_with_risk)
unique(Tralee_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Tralee_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Tralee with grid overlay")

# Plotting
Tralee_map<-ggplot() +
  geom_sf(data = Munster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Tralee_municipal_with_risk[Tralee_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Tralee_municipal_with_risk[Tralee_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Tralee district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

print(Tralee_map)

saveRDS(Tralee_municipal_with_risk, "Tralee_municipal_with_risk.rds")

# Load it back later
Tralee_risk <- readRDS("Tralee_municipal_with_risk.rds")

##Cork by municipal district
gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

cork_districts <- Municipal_Districts %>%
  filter(COUNTY == "CORK") %>%
  dplyr::select(ENGLISH) %>%
  distinct()

print(cork_districts$ENGLISH)

#Municipal District of Bandon - Kinsale

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Cork MD/Bandon_Kinsale")
list.files()
BandonKinsale<-st_read("NLCM_BandonKinsale_Shapefile.shp")  #loads
# Define 5x5 km grid
bounds <- st_bbox(BandonKinsale)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(BandonKinsale))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, BandonKinsale, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()


gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

BandonKinsale_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Bandon - Kinsale")

ggplot() +
  geom_sf(data = BandonKinsale_district, fill = "blue", color = "black") +
  labs(title = "Tralee district") +
  theme_minimal()



# Define the extent and create a grid over BandonKinsale district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(BandonKinsale_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, BandonKinsale_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
BandonKinsale_district_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(BandonKinsale_district_municipal_with_risk)
unique(BandonKinsale_district_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = BandonKinsale_district_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of BandonKinsale with grid overlay")

# Plotting
BandonKinsale_map<-ggplot() +
  geom_sf(data = Munster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = BandonKinsale_district_municipal_with_risk[BandonKinsale_district_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = BandonKinsale_district_municipal_with_risk[BandonKinsale_district_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Tralee district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

print(BandonKinsale_map)

saveRDS(BandonKinsale_district_municipal_with_risk, "BandonKinsale_district_municipal_with_risk.rds")

# Load it back later
BandonKinsale_risk <- readRDS("BandonKinsale_district_municipal_with_risk.rds")



#Municipal District of EastCork

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Cork MD/East_Cork")
list.files()
EastCork<-st_read("NLCM_EastCork_Shapefile.shp") #loads
# Define 5x5 km grid
bounds <- st_bbox(EastCork)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(EastCork))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, EastCork, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()


gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

EastCork_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of East Cork")

ggplot() +
  geom_sf(data = EastCork_district, fill = "blue", color = "black") +
  labs(title = "EastCork district") +
  theme_minimal()



# Define the extent and create a grid over EastCork district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(EastCork_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, EastCork_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
EastCork_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(EastCork_municipal_with_risk)
unique(EastCork_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = EastCork_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of EastCork with grid overlay")

# Plotting
EastCork_map<-ggplot() +
  geom_sf(data = Munster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = EastCork_municipal_with_risk[EastCork_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = EastCork_municipal_with_risk[EastCork_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in EastCork district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

print(EastCork_map)

saveRDS(EastCork_municipal_with_risk, "EastCork_municipal_with_risk.rds")

# Load it back later
EastCork_risk <- readRDS("EastCork_municipal_with_risk.rds")




#Municipal District of Cobh


setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Cork MD/Cobh")
Cobh<-st_read("NLCM_Cobh_Shapefile.shp") #loads
# Define 5x5 km grid
bounds <- st_bbox(Cobh)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Cobh))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Cobh, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()


gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

Cobh_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Cobh")

ggplot() +
  geom_sf(data = Cobh_district, fill = "blue", color = "black") +
  labs(title = "Cobh district") +
  theme_minimal()



# Define the extent and create a grid over Cobh district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Cobh_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Cobh_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
Cobh_district_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Cobh_district_municipal_with_risk)
unique(Cobh_district_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Cobh_district_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District Cobh with grid overlay")

# Plotting
Cobh_map<-ggplot() +
  geom_sf(data = Munster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Cobh_district_municipal_with_risk[Cobh_district_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Cobh_district_municipal_with_risk[Cobh_district_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Cobh district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

print(Cobh_map)

saveRDS(Cobh_district_municipal_with_risk, "Cobh_district_municipal_with_risk.rds")

# Load it back later
Cobh_risk <- readRDS("Cobh_district_municipal_with_risk.rds")



#Municipal District of Fermoy
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Cork MD/Fermoy")
Fermoy<-st_read("NLCM_Fermoy_Shapefile.shp") #LOADS
# Define 5x5 km grid
bounds <- st_bbox(Fermoy)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Fermoy))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Fermoy, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()


gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

Fermoy_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Fermoy")

ggplot() +
  geom_sf(data = Fermoy_district, fill = "blue", color = "black") +
  labs(title = "Fermoy district") +
  theme_minimal()



# Define the extent and create a grid over Fermoy district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Fermoy_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Fermoy_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
Fermoy_district_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Fermoy_district_municipal_with_risk)
unique(Fermoy_district_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Fermoy_district_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Fermoy with grid overlay")

# Plotting
Fermoy_map<-ggplot() +
  geom_sf(data = Munster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Fermoy_district_municipal_with_risk[Fermoy_district_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Fermoy_district_municipal_with_risk[Fermoy_district_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Fermoy district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

print(Fermoy_map)

saveRDS(Fermoy_district_municipal_with_risk, "Fermoy_district_municipal_with_risk.rds")

# Load it back later
Fermoy_risk <- readRDS("Fermoy_district_municipal_with_risk.rds")

#Municipal District of West Cork

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Cork MD/West_Cork")
WestCork<-st_read("NLCM_WestCork_Shapefile.shp")

# Define 5x5 km grid
bounds <- st_bbox(WestCork)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(WestCork))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, WestCork, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()


gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

WestCork_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of West Cork")

ggplot() +
  geom_sf(data = WestCork_district, fill = "blue", color = "black") +
  labs(title = "WestCork district") +
  theme_minimal()



# Define the extent and create a grid over WestCork district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(WestCork_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, WestCork_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
WestCork_district_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(WestCork_district_municipal_with_risk)
unique(WestCork_district_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = WestCork_district_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of WestCork with grid overlay")

# Plotting
WestCork_map<-ggplot() +
  geom_sf(data = Munster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = WestCork_district_municipal_with_risk[WestCork_district_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = WestCork_district_municipal_with_risk[WestCork_district_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in WestCork district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

print(WestCork_map)

saveRDS(WestCork_district_municipal_with_risk, "WestCork_district_municipal_with_risk.rds")

# Load it back later
WestCork_risk <- readRDS("WestCork_district_municipal_with_risk.rds")



#Municipal District of Macroom  
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Cork MD/Macroom")

list.files()

Macroom<-st_read("NLCM_Macroom_Shapefile.shp")
# Define 5x5 km grid
bounds <- st_bbox(Macroom)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Macroom))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Macroom, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()


gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

Macroom_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Macroom")

ggplot() +
  geom_sf(data = Macroom_district, fill = "blue", color = "black") +
  labs(title = "Macroom district") +
  theme_minimal()



# Define the extent and create a grid over Macroom district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Macroom_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Macroom_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
Macroom_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Macroom_municipal_with_risk)
unique(Macroom_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Macroom_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Macroom with grid overlay")

# Plotting
Macroom_map<-ggplot() +
  geom_sf(data = Munster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Macroom_municipal_with_risk[Macroom_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Macroom_municipal_with_risk[Macroom_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Macroom district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

print(Macroom_map)

saveRDS(Macroom_municipal_with_risk, "Macroom_municipal_with_risk.rds")

# Load it back later
Macroom_risk <- readRDS("Macroom_municipal_with_risk.rds")



#Municipal District of Kanturk - Mallow
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Cork MD/Kenturk_Mallow")
list.files()
KanturkMallow<-st_read("NLCM_KanturkMallow_Shapefile.shp") #LOADS
# Define 5x5 km grid
bounds <- st_bbox(KanturkMallow)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(KanturkMallow))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, KanturkMallow, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()


gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

KanturkMallow_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Kanturk - Mallow")

ggplot() +
  geom_sf(data = KanturkMallow_district, fill = "blue", color = "black") +
  labs(title = "KanturkMallow district") +
  theme_minimal()



# Define the extent and create a grid over  Kanturk - MalloW district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Macroom_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, KanturkMallow_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
KanturkMallow_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(KanturkMallow_municipal_with_risk)
unique(KanturkMallow_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = KanturkMallow_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of KanturkMallow with grid overlay")

# Plotting
KanturkMallow_map<-ggplot() +
  geom_sf(data = Munster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = KanturkMallow_municipal_with_risk[KanturkMallow_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = KanturkMallow_municipal_with_risk[KanturkMallow_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Macroom district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

print(KanturkMallow_map)

saveRDS(KanturkMallow_municipal_with_risk, "KanturkMallow_municipal_with_risk.rds")

# Load it back later
KanturkMallow_risk <- readRDS("KanturkMallow_municipal_with_risk.rds")


#Municipal District of Carrigaline"
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Munster/Cork MD/Carrigaline")

Carrigaline<-st_read("NLCM_Carrigaline_Shapefile.shp") #LOADS
# Define 5x5 km grid
bounds <- st_bbox(Carrigaline)
buffer_size <- 5000  # Additional buffer to ensure full coverage
extended_bounds <- c(
  xmin = bounds$xmin - buffer_size,
  xmax = bounds$xmax + buffer_size,
  ymin = bounds$ymin - buffer_size,
  ymax = bounds$ymax + buffer_size
)
print(extended_bounds)
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)




# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Carrigaline))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Carrigaline, join = st_intersects)




# Summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

# Check the structure of the summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Check the wide data structure
print(grid_summary_wide)

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("CULTIVATED LAND", "ARTIFICIAL SURFACES", "EXPOSED SURFACES") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("PEATLAND", "GRASSLAND, SALTMARSH and SWAMP") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("WATERBODIES", "FOREST, WOODLAND AND SCRUB", "HEATH and BRACKEN") ~ "Neutral Risk"
)

# Plotting
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level), color = NA) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow")) +
  labs(fill = "Risk Level") +
  theme_minimal()


gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

Carrigaline_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Carrigaline")

ggplot() +
  geom_sf(data = Carrigaline_district, fill = "blue", color = "black") +
  labs(title = "Carrigaline district") +
  theme_minimal()



# Define the extent and create a grid over Carrigaline district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Carrigaline_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Carrigaline_district)
grid_buffered <- st_buffer(grid_clipped, dist = 10)

#check CRS and transform if necessary
if (st_crs(grid_summary_wide) != st_crs(grid_clipped)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(grid_clipped))
}

#check and convert grid_clipped to sf
if (!"sf" %in% class(grid_clipped)) {
  grid_clipped <- st_sf(grid_clipped)
}

#check and convert grid_summary_wide to sf
if (!"sf" %in% class(grid_summary_wide)) {
  grid_summary_wide <- st_sf(grid_summary_wide)
}

st_crs(grid_clipped) == st_crs(grid_summary_wide)
Carrigaline_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Carrigaline_municipal_with_risk)
unique(Carrigaline_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Carrigaline_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Carrigaline with grid overlay")

# Plotting
Carrigaline_map<-ggplot() +
  geom_sf(data = Munster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Carrigaline_municipal_with_risk[Carrigaline_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Carrigaline_municipal_with_risk[Carrigaline_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Carrigaline district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

print(Carrigaline_map)

saveRDS(Carrigaline_municipal_with_risk, "Carrigaline_municipal_with_risk.rds")

# Load it back later
Carrigaline_risk <- readRDS("Carrigaline_municipal_with_risk.rds")





###########Munster map with all counties

# Load risk files
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/RDS_files/Munster")
list.files()

clare_risk<- readRDS("Clare_municipal_with_risk.rds")
limerick_risk<-readRDS("Limerick_municipal_with_risk.rds")
Tipperary_risk<-readRDS("Tipperary_municipal_with_risk.rds")              
Waterford_risk<-readRDS("Waterford_municipal_with_risk.rds")

##Cork
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/RDS_files/Munster/Cork MD")
BandonKinsale_risk <-readRDS("BandonKinsale_municipal_with_risk.rds")
EastCork_risk <-readRDS("EastCork_municipal_with_risk.rds")
Cobh_risk <-readRDS("Cobh_municipal_with_risk.rds")
Fermoy_risk <-readRDS("Fermoy_municipal_with_risk.rds")
WestCork_risk <-readRDS("WestCork_municipal_with_risk.rds")
Macroom_risk <-readRDS("Macroom_municipal_with_risk.rds")
KanturkMallow_risk <-readRDS("KanturkMallow_municipal_with_risk.rds")
Carrigaline_risk <-readRDS("Carrigaline_municipal_with_risk.rds")


##Kerry
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/RDS_files/Munster/Kerry MD")
Tralee_risk <- readRDS("Tralee_municipal_with_risk.rds")
Killarney_risk <- readRDS("Killarney_municipal_with_risk.rds")
Listowel_risk <- readRDS("Listowel_municipal_with_risk.rds")
Kenmare_risk <- readRDS("Kenmare_municipal_with_risk.rds")
Castleisland_risk <- readRDS("Castleisland_municipal_with_risk.rds")

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD")
gpkg_file <- "Counties___OSi_National_Statutory_Boundaries_7992851692716515542.gpkg"
county_boundaries <- st_read(gpkg_file)
colnames(county_boundaries)
print(unique(county_boundaries$COUNTY))

Munster <-county_boundaries %>%
  filter(PROVINCE == "Munster")



# Plotting
Munster_map<-ggplot() +
  geom_sf(data = Munster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = clare_risk[clare_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = clare_risk[clare_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = limerick_risk[limerick_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = limerick_risk[limerick_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Tipperary_risk[Tipperary_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Tipperary_risk[Tipperary_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Waterford_risk[Waterford_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Waterford_risk[Waterford_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Tralee_risk[Tralee_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Tralee_risk[Tralee_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Killarney_risk[Killarney_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Killarney_risk[Killarney_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
 geom_sf(data = Listowel_risk[Listowel_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Listowel_risk[Listowel_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Kenmare_risk[Kenmare_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Kenmare_risk[Kenmare_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Castleisland_risk[Castleisland_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Castleisland_risk[Castleisland_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Munster district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_text(size = 16)
        )
print(Munster_map)







#Final Munster plot with all counties included

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD")
gpkg_file <- "Counties___OSi_National_Statutory_Boundaries_7992851692716515542.gpkg"
county_boundaries <- st_read(gpkg_file)
colnames(county_boundaries)
unique(county_boundaries$COUNTY)
unique(county_boundaries$PROVINCE)


#INSET MAP
munster_counties <- county_boundaries %>% 
  filter(PROVINCE == "Munster")


# Base plot with provinces
munster_inset_map <- ggplot() +
  geom_sf(data = county_boundaries, fill = NA, color = "grey50", size = 0.25) +
  geom_sf(data = munster_counties, fill = "blue", color = "blue", size = 0.25) +
  labs(title = "") +
  theme_minimal()

print(munster_inset_map)

# Load risk files
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/RDS_files/Munster")
list.files()

clare_risk<- readRDS("Clare_municipal_with_risk.rds")
limerick_risk<-readRDS("Limerick_municipal_with_risk.rds")
Tipperary_risk<-readRDS("Tipperary_municipal_with_risk.rds")              
Waterford_risk<-readRDS("Waterford_municipal_with_risk.rds")

##Cork
BandonKinsale_risk <-readRDS("BandonKinsale_municipal_with_risk.rds")
EastCork_risk <-readRDS("EastCork_municipal_with_risk.rds")
Cobh_risk <-readRDS("Cobh_municipal_with_risk.rds")
Fermoy_risk <-readRDS("Fermoy_municipal_with_risk.rds")
Macroom_risk <-readRDS("Macroom_municipal_with_risk.rds")
KanturkMallow_risk <-readRDS("KanturkMallow_municipal_with_risk.rds")
Carrigaline_risk <-readRDS("Carrigaline_municipal_with_risk.rds")
WestCork_risk <- readRDS("WestCork_municipal_with_risk.rds")



##Kerry
Tralee_risk <- readRDS("Tralee_municipal_with_risk.rds")
Killarney_risk <- readRDS("Killarney_municipal_with_risk.rds")
Listowel_risk <- readRDS("Listowel_municipal_with_risk.rds")
Kenmare_risk <- readRDS("Kenmare_municipal_with_risk.rds")
Castleisland_risk <- readRDS("Castleisland_municipal_with_risk.rds")


# Plotting MUNSTER WITH ALL COUNTY RISK
Munster_map<-ggplot() +
  geom_sf(data = Munster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Carrigaline_risk[Carrigaline_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Carrigaline_risk[Carrigaline_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = BandonKinsale_risk[BandonKinsale_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = BandonKinsale_risk[BandonKinsale_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Cobh_risk[Cobh_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Cobh_risk[Cobh_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Fermoy_risk[Fermoy_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Fermoy_risk[Fermoy_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = EastCork_risk[EastCork_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = EastCork_risk[EastCork_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = WestCork_risk[WestCork_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = WestCork_risk[WestCork_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Macroom_risk[Macroom_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Macroom_risk[Macroom_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = KanturkMallow_risk[KanturkMallow_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = KanturkMallow_risk[KanturkMallow_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = clare_risk[clare_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = clare_risk[clare_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = limerick_risk[limerick_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = limerick_risk[limerick_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Tipperary_risk[Tipperary_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Tipperary_risk[Tipperary_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Waterford_risk[Waterford_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Waterford_risk[Waterford_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Tralee_risk[Tralee_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Tralee_risk[Tralee_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Killarney_risk[Killarney_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Killarney_risk[Killarney_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Listowel_risk[Listowel_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Listowel_risk[Listowel_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Kenmare_risk[Kenmare_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Kenmare_risk[Kenmare_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Castleisland_risk[Castleisland_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Castleisland_risk[Castleisland_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right", 
        axis.text = element_text(size = 16)) #CORRECT PLOT

print(Munster_map)

