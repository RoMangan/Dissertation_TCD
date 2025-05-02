#Risk Map Ulster Counties
# This script was developed by Rosie Mangan
# 
# 1 School of Computer Science and Statistics at Trinity College Dublin, Ireland.

# Script originated June 1, 2024
# Last modified Feb 19, 2025
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

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD")

gpkg_file <- "Counties___OSi_National_Statutory_Boundaries_7992851692716515542.gpkg"
county_boundaries <- st_read(gpkg_file)
colnames(county_boundaries)
print(unique(county_boundaries$COUNTY))

Ulster <-county_boundaries %>%
  filter(PROVINCE == "Ulster")

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Ulster")
list.files()




#######Cavan
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Ulster/Cavan")
list.files()
Cavan <-st_read("NLCM_Cavan_Shapefile.shp") #


# Define 5x5 km grid
bounds <- st_bbox(Cavan)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Cavan))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Cavan, join = st_intersects)




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

cavan_district <- county_boundaries %>%
  filter(COUNTY == "CAVAN")

ggplot() +
  geom_sf(data = cavan_district, fill = "blue", color = "black") +
  labs(title = "District of Cavan") +
  theme_minimal()



# Define the extent and create a grid over cavan district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(cavan_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, cavan_district)
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
Cavan_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Cavan_municipal_with_risk)
unique(Cavan_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = cavan_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Cavan with grid overlay")

# Plotting
Cavan_map<-ggplot() +
  geom_sf(data = Ulster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Cavan_municipal_with_risk[Cavan_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Cavan_municipal_with_risk[Cavan_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Cavan district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

saveRDS(Cavan_municipal_with_risk, "Cavan_municipal_with_risk.rds")

# Load it back later
Cavan_risk <- readRDS("Cavan_municipal_with_risk.rds")

#######Monaghan
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Ulster/Monaghan")
list.files()
Monaghan <-st_read("NLCM_Monaghan_Shapefile.shp") #

# Define 5x5 km grid
bounds <- st_bbox(Monaghan)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Monaghan))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Monaghan, join = st_intersects)


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

monaghan_district <- county_boundaries %>%
  filter(COUNTY == "MONAGHAN")

ggplot() +
  geom_sf(data = monaghan_district, fill = "blue", color = "black") +
  labs(title = "District of Monaghan") +
  theme_minimal()



# Define the extent and create a grid over monaghan district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(monaghan_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, monaghan_district)
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
Monaghan_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Monaghan_municipal_with_risk)
unique(Monaghan_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = monaghan_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Monaghan with grid overlay")

ggplot() +
  geom_sf(data = grid_summary_wide[grid_summary_wide$risk_level == "High Risk",], color = "red") +
  geom_sf(data = grid_clipped, color = "blue", fill = NA, alpha = 0.5) +
  labs(title = "High Risk Areas Overlap Check") +
  theme_minimal()

# Plotting
monaghan_map<-ggplot() +
  geom_sf(data = Ulster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Monaghan_municipal_with_risk[Monaghan_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Monaghan_municipal_with_risk[Monaghan_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Monaghan district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

saveRDS(Monaghan_municipal_with_risk, "Monaghan_municipal_with_risk.rds")

# Load it back later
Monaghan_risk <- readRDS("Monaghan_municipal_with_risk.rds")

#DONEGAL by municipal district
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$COUNTY)

donegal_districts <- Municipal_Districts %>%
  filter(COUNTY == "DONEGAL") %>%
  dplyr::select(ENGLISH) %>%
  distinct()

print(donegal_districts$ENGLISH)

#Municipal District of Letterkenny - Milford
#Municipal District of Lifford - Stranorlar
#Municipal District of Glenties
#Municipal District of Inishowen
#Municipal District of Donegal 



#Donegal_Glenties
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Ulster/Donegal MD")
list.files()
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Ulster/Donegal MD/Donegal_Glenties")
list.files()
Donegal_Glenties <-st_read("NLCM_Donegal_Glenties_Shapefile.shp")

# Define 5x5 km grid
bounds <- st_bbox(Donegal_Glenties)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Donegal_Glenties))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Donegal_Glenties, join = st_intersects)

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
gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
municipal_boundaries <- st_read(gpkg_file)
colnames(municipal_boundaries)
unique(municipal_boundaries$ENGLISH)

donegal_glenties <- municipal_boundaries %>%
  filter(ENGLISH == "Municipal District of Glenties")

ggplot() +
  geom_sf(data = donegal_glenties, fill = "blue", color = "black") +
  labs(title = "District of Donegal Glenties") +
  theme_minimal()



# Define the extent and create a grid over Glenties district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(donegal_glenties))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, donegal_glenties)
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
donegal_glenties_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(donegal_glenties_municipal_with_risk)
unique(donegal_glenties_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = donegal_glenties, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Donegal Glenties with grid overlay")

ggplot() +
  geom_sf(data = grid_summary_wide[grid_summary_wide$risk_level == "High Risk",], color = "red") +
  geom_sf(data = grid_clipped, color = "blue", fill = NA, alpha = 0.5) +
  labs(title = "High Risk Areas Overlap Check") +
  theme_minimal()

# Plotting
donegal_glenties<-ggplot() +
  geom_sf(data = Ulster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = donegal_glenties_municipal_with_risk[donegal_glenties_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = donegal_glenties_municipal_with_risk[donegal_glenties_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Donegal Glenties district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(donegal_glenties)

saveRDS(donegal_glenties_municipal_with_risk, "donegal_glenties_municipal_with_risk.rds")

# Load it back later
Donegal_glenties<- readRDS("donegal_glenties_municipal_with_risk.rds")



#Donegal_Inishowen
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Ulster/Donegal MD")
list.files()
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Ulster/Donegal MD/Donegal_Inishowen")
list.files()
Donegal_Inishowen <-st_read("NLCM_Inishowen_Shapefile.shp")

# Define 5x5 km grid
bounds <- st_bbox(Donegal_Inishowen)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Donegal_Inishowen))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Donegal_Inishowen, join = st_intersects)

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
gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
municipal_boundaries <- st_read(gpkg_file)
colnames(municipal_boundaries)
unique(municipal_boundaries$ENGLISH)

Donegal_Inishowen_district <- municipal_boundaries %>%
  filter(ENGLISH == "Municipal District of Inishowen")

ggplot() +
  geom_sf(data = Donegal_Inishowen_district, fill = "blue", color = "black") +
  labs(title = "District of Donegal Inishowen") +
  theme_minimal()



# Define the extent and create a grid over Donegal Inishowen district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Donegal_Inishowen_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Donegal_Inishowen_district)
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
Donegal_Inishowen_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Donegal_Inishowen_municipal_with_risk)
unique(Donegal_Inishowen_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Donegal_Inishowen_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Donegal Inishowen with grid overlay")

# Plotting
DonegalInishowen_map<-ggplot() +
  geom_sf(data = Ulster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Donegal_Inishowen_municipal_with_risk[Donegal_Inishowen_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_Inishowen_municipal_with_risk[Donegal_Inishowen_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Donegal Inishowen district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(DonegalInishowen_map)

saveRDS(Donegal_Inishowen_municipal_with_risk, "Donegal_Inishowen_municipal_with_risk.rds")

# Load it back later
Donegal_Inishowen_risk <- readRDS("Donegal_Inishowen_municipal_with_risk.rds")
Donegal_glenties<- readRDS("donegal_glenties_municipal_with_risk.rds")




##Donegal_Donegal
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Ulster/Donegal MD")
list.files()

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Ulster/Donegal MD/Donegal_Donegal")
list.files()

Donegal_Donegal <-st_read("NLCM_Donegal_Donegal_Shapefile.shp")

# Define 5x5 km grid
bounds <- st_bbox(Donegal_Donegal)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Donegal_Donegal))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Donegal_Donegal, join = st_intersects)

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
gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
municipal_boundaries <- st_read(gpkg_file)
colnames(municipal_boundaries)
unique(municipal_boundaries$ENGLISH)

Donegal_Donegal_district <- municipal_boundaries %>%
  filter(ENGLISH == "Municipal District of Donegal")

ggplot() +
  geom_sf(data = Donegal_Donegal_district, fill = "blue", color = "black") +
  labs(title = "District of Donegal Donegal") +
  theme_minimal()



# Define the extent and create a grid over Donegal Donegal district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Donegal_Donegal_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Donegal_Donegal_district)
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
Donegal_Donegal_district_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Donegal_Donegal_district_municipal_with_risk)
unique(Donegal_Donegal_district_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Donegal_Donegal_district_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Donegal Donegal with grid overlay")

ggplot() +
  geom_sf(data = grid_summary_wide[grid_summary_wide$risk_level == "High Risk",], color = "red") +
  geom_sf(data = grid_clipped, color = "blue", fill = NA, alpha = 0.5) +
  labs(title = "High Risk Areas Overlap Check") +
  theme_minimal()


# Plotting
Donegal_Donegal_map<-ggplot() +
  geom_sf(data = Ulster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Donegal_Donegal_district_municipal_with_risk[Donegal_Donegal_district_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_Donegal_district_municipal_with_risk[Donegal_Donegal_district_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Donegal district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")


print(Donegal_Donegal_map)

saveRDS(Donegal_Donegal_district_municipal_with_risk, "Donegal_Donegal_district_municipal_with_risk.rds")

# Load it back later
Donegal_Donegal_risk <- readRDS("Donegal_Donegal_district_municipal_with_risk.rds")
Donegal_Inishowen_risk <- readRDS("Donegal_Inishowen_municipal_with_risk.rds")
Donegal_glenties<- readRDS("donegal_glenties_municipal_with_risk.rds")



####onegal_Letterkenny
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Ulster/Donegal MD")
list.files()

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Ulster/Donegal MD/Donegal_Letterkenny")
list.files()
Donegal_Letterkenny <-st_read("NLCM_Letterkenny_Shapefile.shp")

# Define 5x5 km grid
bounds <- st_bbox(Donegal_Letterkenny)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Donegal_Letterkenny))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Donegal_Letterkenny, join = st_intersects)

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
gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
municipal_boundaries <- st_read(gpkg_file)
colnames(municipal_boundaries)
unique(municipal_boundaries$ENGLISH)

Donegal_Letterkenny_district <- municipal_boundaries %>%
  filter(ENGLISH == "Municipal District of Letterkenny - Milford")

ggplot() +
  geom_sf(data = Donegal_Letterkenny_district, fill = "blue", color = "black") +
  labs(title = "District of Donegal Letterkenny") +
  theme_minimal()



# Define the extent and create a grid over Donegal Donegal district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Donegal_Letterkenny_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Donegal_Letterkenny_district)
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
Donegal_Letterkenny_district_district_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Donegal_Letterkenny_district_district_municipal_with_risk)
unique(Donegal_Letterkenny_district_district_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Donegal_Letterkenny_district_district_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Donegal Letterkenny with grid overlay")

# Plotting
kildare_map<-ggplot() +
  geom_sf(data = Ulster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Donegal_Letterkenny_district_district_municipal_with_risk[Donegal_Letterkenny_district_district_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_Letterkenny_district_district_municipal_with_risk[Donegal_Letterkenny_district_district_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Donegal Letterkenny district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")

saveRDS(Donegal_Letterkenny_district_district_municipal_with_risk, "Donegal_Letterkenny_district_district_municipal_with_risk.rds")

# Load it back later
Donegal_Letterkenny_risk <- readRDS("Donegal_Letterkenny_district_district_municipal_with_risk.rds")




setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Ulster/Donegal MD")
list.files()
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Ulster/Donegal MD/Donegal_LiffordStranorlar")
list.files()
Donegal_LiffordStranorlar <-st_read("NLCM_LiffordStranorlar_Shapefile.shp")

# Define 5x5 km grid
bounds <- st_bbox(Donegal_LiffordStranorlar)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Donegal_LiffordStranorlar))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Donegal_LiffordStranorlar, join = st_intersects)

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
gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
municipal_boundaries <- st_read(gpkg_file)
colnames(municipal_boundaries)
unique(municipal_boundaries$ENGLISH)

Donegal_LiffordStranorlar_district <- municipal_boundaries %>%
  filter(ENGLISH == "Municipal District of Lifford - Stranorlar")

ggplot() +
  geom_sf(data = Donegal_LiffordStranorlar_district, fill = "blue", color = "black") +
  labs(title = "District of Donegal LiffordStranorlar") +
  theme_minimal()



# Define the extent and create a grid over Donegal LiffordStranorlar district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Donegal_LiffordStranorlar_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Donegal_LiffordStranorlar_district)
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
Donegal_LiffordStranorlar_district_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Donegal_LiffordStranorlar_district_municipal_with_risk)
unique(Donegal_LiffordStranorlar_district_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Donegal_LiffordStranorlar_district_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Donegal LiffordStranorlar with grid overlay")

# Plotting
Donegal_LiffordStranorlar_map<-ggplot() +
  geom_sf(data = Ulster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Donegal_LiffordStranorlar_district_municipal_with_risk[Donegal_LiffordStranorlar_district_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_LiffordStranorlar_district_municipal_with_risk[Donegal_LiffordStranorlar_district_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Donegal LiffordStranorlar district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Donegal_LiffordStranorlar_map)

saveRDS(Donegal_LiffordStranorlar_district_municipal_with_risk, "Donegal_LiffordStranorlar_district_municipal_with_risk.rds")

# Load it back later
Donegal_LiffordStranorlar_risk <- readRDS("Donegal_LiffordStranorlar_district_municipal_with_risk.rds")


#Final Ulster plot with all counties included


# Load risk files
Cavan_risk <- readRDS("Cavan_municipal_with_risk.rds")
Monaghan_risk <- readRDS("Monaghan_municipal_with_risk.rds")
Donegal_Letterkenny_risk <- readRDS("Donegal_Letterkenny_district_municipal_with_risk.rds")
Donegal_Donegal_risk <- readRDS("Donegal_Donegal_district_municipal_with_risk.rds")
Donegal_Inishowen_risk <- readRDS("Donegal_Inishowen_municipal_with_risk.rds")
Donegal_glenties<- readRDS("donegal_glenties_municipal_with_risk.rds")
Donegal_Letterkenny_risk <- readRDS("Donegal_Letterkenny_district_municipal_with_risk.rds")
Donegal_LiffordStranorlar_risk <- readRDS("Donegal_LiffordStranorlar_district_municipal_with_risk.rds")




# Plot
gpkg_file <- "Counties___OSi_National_Statutory_Boundaries_7992851692716515542.gpkg"
county_boundaries <- st_read(gpkg_file)
colnames(county_boundaries)
print(unique(county_boundaries$COUNTY))

Ulster <-county_boundaries %>%
  filter(PROVINCE == "Ulster")



Ulster_map<-ggplot() +
  geom_sf(data = Ulster, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Cavan_risk[Cavan_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Cavan_risk[Cavan_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Monaghan_risk[Monaghan_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Monaghan_risk[Monaghan_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_Donegal_risk[Donegal_Donegal_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_Donegal_risk[Donegal_Donegal_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_Letterkenny_risk[Donegal_Letterkenny_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_Letterkenny_risk[Donegal_Letterkenny_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_Inishowen_risk[Donegal_Inishowen_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_Inishowen_risk[Donegal_Inishowen_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_glenties[Donegal_glenties$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_glenties[Donegal_glenties$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_Letterkenny_risk[Donegal_Letterkenny_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_Letterkenny_risk[Donegal_Letterkenny_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_Letterkenny_risk[Donegal_Letterkenny_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_Letterkenny_risk[Donegal_Letterkenny_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_LiffordStranorlar_risk[Donegal_LiffordStranorlar_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Donegal_LiffordStranorlar_risk[Donegal_LiffordStranorlar_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) + 
  labs(title = "Risk Levels in Ulster district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_text(size = 16)
  )

print(Ulster_map)

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD")
gpkg_file <- "Counties___OSi_National_Statutory_Boundaries_7992851692716515542.gpkg"
county_boundaries <- st_read(gpkg_file)
colnames(county_boundaries)
unique(county_boundaries$COUNTY)
unique(county_boundaries$PROVINCE)


ulster_counties <- county_boundaries %>% 
  filter(PROVINCE == "Ulster")


# Base plot with provinces
ulster_map <- ggplot() +
  geom_sf(data = county_boundaries, fill = NA, color = "grey50", size = 0.25) +
  geom_sf(data = ulster_counties, fill = "blue", color = "blue", size = 0.25) +
  labs(title = "") +
  theme_minimal()

print(ulster_map)





