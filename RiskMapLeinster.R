#Risk Map Leinster Counties

# load libraries

library(sf)# sf is a library to read geospatial information, like maps
library(dplyr) # dplyr is a data manipulation library
library(stringr) #stingr is library to manipulate string type data
library(tidyverse)
library(sp)
library(raster) #geographic data analysis 
library(tmap) #thematic maps
library(tidyr)
library(ggplot2)
library(cowplot)

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD")
gpkg_file <- "Counties___OSi_National_Statutory_Boundaries_7992851692716515542.gpkg"
county_boundaries <- st_read(gpkg_file)
colnames(county_boundaries)
print(unique(county_boundaries$COUNTY))

Leinster <-county_boundaries %>%
  filter(PROVINCE == "Leinster")

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Leinster")
list.files()

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Leinster/Carlow")
list.files()
Carlow <-st_read("NLCM_Carlow_Shapefile.shp") #


# Define 5x5 km grid
bounds <- st_bbox(Carlow)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Carlow))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Carlow, join = st_intersects)




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

carlow_district <- county_boundaries %>%
  filter(COUNTY == "CARLOW")

ggplot() +
  geom_sf(data = carlow_district, fill = "blue", color = "black") +
  labs(title = "District of Carlow") +
  theme_minimal()



# Define the extent and create a grid over carlow district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(carlow_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, carlow_district)
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
Carlow_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Carlow_municipal_with_risk)
unique(Carlow_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = carlow_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Carlow with grid overlay")

# Plotting
Carlow_map<-ggplot() +
  geom_sf(data = Leinster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Carlow_municipal_with_risk[Carlow_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Carlow_municipal_with_risk[Carlow_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Carlow district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

saveRDS(Carlow_municipal_with_risk, "Carlow_municipal_with_risk.rds")

# Load it back later
Carlow_risk <- readRDS("Carlow_municipal_with_risk.rds")



setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Leinster/Dublin")
list.files()
Dublin <-st_read("NLCM_Dublin_Shapefile.shp") #

# Define 5x5 km grid
bounds <- st_bbox(Dublin)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Dublin))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Dublin, join = st_intersects)


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

dublin_district <- county_boundaries %>%
  filter(COUNTY == "DUBLIN")

ggplot() +
  geom_sf(data = dublin_district, fill = "blue", color = "black") +
  labs(title = "District of Dublin") +
  theme_minimal()



# Define the extent and create a grid over Dublin district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(dublin_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, dublin_district)
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
Dublin_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Dublin_municipal_with_risk)
unique(Dublin_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = dublin_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Dublin with grid overlay")

# Plotting
Dublin_map<-ggplot() +
  geom_sf(data = Leinster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Dublin_municipal_with_risk[Dublin_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Dublin_municipal_with_risk[Dublin_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Dublin district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

saveRDS(Dublin_municipal_with_risk, "Dublin_municipal_with_risk.rds")

# Load it back later
Dublin_risk <- readRDS("Dublin_municipal_with_risk.rds")

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Leinster/Kildare")
list.files()
Kildare <-st_read("NLCM_Kildare_Shapefile.shp")

# Define 5x5 km grid
bounds <- st_bbox(Kildare)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Kildare))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Kildare, join = st_intersects)




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

kildare_district <- county_boundaries %>%
  filter(COUNTY == "KILDARE")

ggplot() +
  geom_sf(data = kildare_district, fill = "blue", color = "black") +
  labs(title = "District of Kildare") +
  theme_minimal()



# Define the extent and create a grid over Kildare district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(kildare_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, kildare_district)
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
Kildare_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Kildare_municipal_with_risk)
unique(Kildare_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = kildare_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Kildare with grid overlay")

# Plotting
kildare_map<-ggplot() +
  geom_sf(data = Leinster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Kildare_municipal_with_risk[Kildare_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Kildare_municipal_with_risk[Kildare_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Kildare district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

saveRDS(Kildare_municipal_with_risk, "Kildare_municipal_with_risk.rds")

# Load it back later
Kildare_risk <- readRDS("Kildare_municipal_with_risk.rds")




setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Leinster/Kilkenny")
list.files()
Kilkenny <-st_read("NLCM_Kilkenny_Shapefile.shp") #

# Define 5x5 km grid
bounds <- st_bbox(Kilkenny)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Kilkenny))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Kilkenny, join = st_intersects)




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

kilkenny_district <- county_boundaries %>%
  filter(COUNTY == "KILKENNY")

ggplot() +
  geom_sf(data = kilkenny_district, fill = "blue", color = "black") +
  labs(title = "District of Kilkenny") +
  theme_minimal()



# Define the extent and create a grid over Kilkenny district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(kilkenny_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, kilkenny_district)
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
Kilkenny_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Kilkenny_municipal_with_risk)
unique(Kilkenny_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = kilkenny_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Kilkenny with grid overlay")

# Plotting
Kilkenny_map<-ggplot() +
  geom_sf(data = Leinster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Kilkenny_municipal_with_risk[Kilkenny_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Kilkenny_municipal_with_risk[Kilkenny_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Kilkenny district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

saveRDS(Kilkenny_municipal_with_risk, "Kilkenny_municipal_with_risk.rds")

# Load it back later
Kilkenny_risk <- readRDS("Kilkenny_municipal_with_risk.rds")


setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Leinster/Laois")
list.files()
Laois <-st_read("NLCM_Laois_Shapefile.shp")

# Define 5x5 km grid
bounds <- st_bbox(Laois)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Laois))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Laois, join = st_intersects)




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

laois_district <- county_boundaries %>%
  filter(COUNTY == "LAOIS")

ggplot() +
  geom_sf(data = laois_district, fill = "blue", color = "black") +
  labs(title = "District of Laois") +
  theme_minimal()



# Define the extent and create a grid over Laois district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(laois_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, laois_district)
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
Laois_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Laois_municipal_with_risk)
unique(Laois_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = laois_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Laois with grid overlay")

# Plotting
Laois_map<-ggplot() +
  geom_sf(data = Leinster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Laois_municipal_with_risk[Laois_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Laois_municipal_with_risk[Laois_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Laois district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

saveRDS(Laois_municipal_with_risk, "Laois_municipal_with_risk.rds")

# Load it back later
Laois_risk <- readRDS("Laois_municipal_with_risk.rds")

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Leinster/Longford") #XXXXXXXXXXXX
list.files()
Longford<-st_read("NLCM_Longford_Shapefile.shp") #loads


# Define 5x5 km grid
bounds <- st_bbox(Longford)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Longford))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Longford, join = st_intersects)




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

longford_district <- county_boundaries %>%
  filter(COUNTY == "LONGFORD")

ggplot() +
  geom_sf(data = longford_district, fill = "blue", color = "black") +
  labs(title = "District of Longford") +
  theme_minimal()



# Define the extent and create a grid over Longford district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(longford_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, longford_district)
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
Longford_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Longford_municipal_with_risk)
unique(Longford_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = longford_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Longford with grid overlay")

# Plotting
Longford_map<-ggplot() +
  geom_sf(data = Leinster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Longford_municipal_with_risk[Longford_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Longford_municipal_with_risk[Longford_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Longford district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Longford_map)

saveRDS(Longford_municipal_with_risk, "Longford_municipal_with_risk.rds")

# Load it back later
Longford_risk <- readRDS("Longford_municipal_with_risk.rds")



setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Leinster/Louth")
list.files()
Louth <-st_read("NLCM_Louth_Shapefile.shp")

# Define 5x5 km grid
bounds <- st_bbox(Louth)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Louth))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Louth, join = st_intersects)


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

louth_district <- county_boundaries %>%
  filter(COUNTY == "LOUTH")

Leinster <-county_boundaries %>%
  filter(PROVINCE == "Leinster")

ggplot() +
  geom_sf(data = louth_district, fill = "blue", color = "black") +
  labs(title = "District of Louth") +
  theme_minimal()



# Define the extent and create a grid over Louth district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(louth_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, louth_district)
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
Louth_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Louth_municipal_with_risk)
unique(Louth_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = louth_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Louth with grid overlay")

# Plotting
Louth_map<-ggplot() +
  geom_sf(data = Leinster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Louth_municipal_with_risk[Louth_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Louth_municipal_with_risk[Louth_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Laois district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

saveRDS(Louth_municipal_with_risk, "Louth_municipal_with_risk.rds")

# Load it back later
Louth_risk <- readRDS("Louth_municipal_with_risk.rds")


setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Leinster//Meath")
list.files()
Meath <-st_read("NLCM_Meath_Shapefile.shp")

# Define 5x5 km grid
bounds <- st_bbox(Meath)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Meath))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Meath, join = st_intersects)




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

meath_district <- county_boundaries %>%
  filter(COUNTY == "MEATH")

Leinster <-county_boundaries %>%
  filter(PROVINCE == "Leinster")

ggplot() +
  geom_sf(data = meath_district, fill = "blue", color = "black") +
  labs(title = "District of Meath") +
  theme_minimal()



# Define the extent and create a grid over Meath district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(meath_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, meath_district)
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
Meath_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Meath_municipal_with_risk)
unique(Meath_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = meath_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Meath with grid overlay")

# Plotting
Meath_map<-ggplot() +
  geom_sf(data = Leinster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Meath_municipal_with_risk[Meath_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Meath_municipal_with_risk[Meath_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Meath district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

saveRDS(Meath_municipal_with_risk, "Meath_municipal_with_risk.rds")

# Load it back later
Meath_risk <- readRDS("Meath_municipal_with_risk.rds")





setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Leinster/Offaly")
list.files()
Offaly <-st_read("NLCM_Offaly_Shapefile.shp")

# Define 5x5 km grid
bounds <- st_bbox(Offaly)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Offaly))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Offaly, join = st_intersects)




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

offaly_district <- county_boundaries %>%
  filter(COUNTY == "OFFALY")

Leinster <-county_boundaries %>%
  filter(PROVINCE == "Leinster")

ggplot() +
  geom_sf(data = offaly_district, fill = "blue", color = "black") +
  labs(title = "District of Offaly") +
  theme_minimal()



# Define the extent and create a grid over Offaly district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(offaly_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, offaly_district)
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
Offaly_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Offaly_municipal_with_risk)
unique(Offaly_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = offaly_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Offaly with grid overlay")

# Plotting
  geom_sf(data = Leinster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Offaly_municipal_with_risk[Offaly_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Offaly_municipal_with_risk[Offaly_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Meath district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

saveRDS(Offaly_municipal_with_risk, "Offaly_municipal_with_risk.rds")

# Load it back later
Offaly_risk <- readRDS("Offaly_municipal_with_risk.rds")


setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Leinster/Westmeath")
list.files()
Westmeath <-st_read("NLCM_Westmeath_Shapefile.shp") #loads

# Define 5x5 km grid
bounds <- st_bbox(Westmeath)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Westmeath))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Westmeath, join = st_intersects)




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

westmeath_district <- county_boundaries %>%
  filter(COUNTY == "WESTMEATH")

Leinster <-county_boundaries %>%
  filter(PROVINCE == "Leinster")

ggplot() +
  geom_sf(data = westmeath_district, fill = "blue", color = "black") +
  labs(title = "District of Westmeath") +
  theme_minimal()



# Define the extent and create a grid over westmeath district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(westmeath_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, westmeath_district)
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
Westmeath_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Westmeath_municipal_with_risk)
unique(Westmeath_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = westmeath_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Westmeath with grid overlay")

# Plotting
Westmeath_map<-ggplot() +
  geom_sf(data = Leinster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Westmeath_municipal_with_risk[Westmeath_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Westmeath_municipal_with_risk[Westmeath_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Westmeath district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Westmeath_map)

ggplot() +
  geom_sf(data = grid_summary_wide[grid_summary_wide$risk_level == "High Risk",], color = "red") +
  geom_sf(data = grid_clipped, color = "blue", fill = NA, alpha = 0.5) +
  labs(title = "High Risk Areas Overlap Check for Westmeath") +
  theme_minimal()

saveRDS(Westmeath_municipal_with_risk, "Westmeath_municipal_with_risk.rds")

# Load it back later
Westmeath_risk <- readRDS("Westmeath_municipal_with_risk.rds")

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Leinster/Wexford")
list.files()
Wexford <-st_read("NLCM_Wexford_Shapefile.shp")

bounds <- st_bbox(Wexford)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Wexford))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Wexford, join = st_intersects)

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
unique(county_boundaries$PROVINCE)

wexford_district <- county_boundaries %>%
  filter(COUNTY == "WEXFORD")

Leinster <-county_boundaries %>%
  filter(PROVINCE == "Leinster")

ggplot() +
  geom_sf(data = wexford_district, fill = "blue", color = "black") +
  labs(title = "District of Wexford") +
  theme_minimal()



# Define the extent and create a grid over Wexford district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(wexford_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, wexford_district)
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
Wexford_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Wexford_municipal_with_risk)
unique(Wexford_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = wexford_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Wexford with grid overlay")

# Plotting
Wexford_map<-ggplot() +
  geom_sf(data = Leinster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Wexford_municipal_with_risk[Wexford_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Wexford_municipal_with_risk[Wexford_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Wexford district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

saveRDS(Wexford_municipal_with_risk, "Wexford_municipal_with_risk.rds")

# Load it back later
Wexford_risk <- readRDS("Wexford_municipal_with_risk.rds")

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Leinster/Wicklow")
list.files()
Wicklow <-st_read("NLCM_Wicklow_Shapefile.shp")

bounds <- st_bbox(Wicklow)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Wicklow))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Wicklow, join = st_intersects)




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

wicklow_district <- county_boundaries %>%
  filter(COUNTY == "WICKLOW")

Leinster <-county_boundaries %>%
  filter(PROVINCE == "Leinster")

ggplot() +
  geom_sf(data = wicklow_district, fill = "blue", color = "black") +
  labs(title = "District of Wicklow") +
  theme_minimal()



# Define the extent and create a grid over Wicklow district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(wicklow_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, wicklow_district)
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
Wicklow_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Wicklow_municipal_with_risk)
unique(Wicklow_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = wicklow_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Wicklow with grid overlay")

# Plotting
Wicklow_map<-ggplot() +
  geom_sf(data = Leinster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Wicklow_municipal_with_risk[Wicklow_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Wicklow_municipal_with_risk[Wicklow_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Wicklow district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

saveRDS(Wicklow_municipal_with_risk, "Wicklow_municipal_with_risk.rds")

# Load it back later
Wicklow_risk <- readRDS("Wicklow_municipal_with_risk.rds")




#Final Leinster plot with all counties included

Wicklow_risk <- readRDS("Wicklow_municipal_with_risk.rds")
Wexford_risk <- readRDS("Wexford_municipal_with_risk.rds")
Westmeath_risk <- readRDS("Westmeath_municipal_with_risk.rds") 
Offaly_risk <- readRDS("Offaly_municipal_with_risk.rds")
Meath_risk <- readRDS("Meath_municipal_with_risk.rds")
Louth_risk <- readRDS("Louth_municipal_with_risk.rds")
Longford_risk <- readRDS("Longford_municipal_with_risk.rds")
Laois_risk <- readRDS("Laois_municipal_with_risk.rds")
Kilkenny_risk <- readRDS("Kilkenny_municipal_with_risk.rds")
Carlow_risk <- readRDS("Carlow_municipal_with_risk.rds")
Dublin_risk <- readRDS("Dublin_municipal_with_risk.rds")
Kildare_risk <- readRDS("Kildare_municipal_with_risk.rds")


Leinster <-county_boundaries %>%
  filter(PROVINCE == "Leinster")


Leinster_map<-ggplot() +geom_sf(data = Leinster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Wicklow_risk[Wicklow_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Wicklow_risk[Wicklow_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Wexford_risk[Wexford_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Wexford_risk[Wexford_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Offaly_risk[Offaly_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Offaly_risk[Offaly_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Carlow_risk[Carlow_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Carlow_risk[Carlow_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Dublin_risk[Dublin_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Dublin_risk[Dublin_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Kildare_risk[Kildare_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Kildare_risk[Kildare_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Kilkenny_risk[Kilkenny_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Kilkenny_risk[Kilkenny_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Laois_risk[Laois_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Laois_risk[Laois_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Louth_risk[Louth_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Louth_risk[Louth_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Meath_risk[Meath_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Meath_risk[Meath_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Longford_risk[Longford_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Longford_risk[Longford_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Westmeath_risk[Westmeath_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Westmeath_risk[Westmeath_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_text(size = 16))
  print(Leinster_map)

#inset map

#import provinces shapefile for mapping
setwd("/Users/rosiemangan/Desktop/MSc/Semester 2/Dissertation/R script")
ireland_map <- st_read("Provinces")


setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD")
gpkg_file <- "Counties___OSi_National_Statutory_Boundaries_7992851692716515542.gpkg"
county_boundaries <- st_read(gpkg_file)
colnames(county_boundaries)
unique(county_boundaries$COUNTY)
unique(county_boundaries$PROVINCE)

leinster_counties <- county_boundaries %>% 
  filter(PROVINCE == "Leinster")


# Base plot with provinces
leinster_inset_map <- ggplot() +
  geom_sf(data = county_boundaries, fill = NA, color = "grey50", size = 0.25) +
  geom_sf(data = leinster_counties, fill = "blue", color = "blue", size = 0.25) +
  labs(title = "") +
  theme_minimal()

print(leinster_inset_map)




