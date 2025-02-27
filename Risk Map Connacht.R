#Risk Map Connacht Counties

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
print(unique(county_boundaries$PROVINCE))

Connacht <-county_boundaries %>%
  filter(PROVINCE == "Connacht")

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht")
list.files()


####Leitrim
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht/Leitrim") #LOADS
list.files()
Leitrim<-st_read("NLCM_Leitrim_Shapefile.shp") #


# Define 5x5 km grid
bounds <- st_bbox(Leitrim)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Leitrim))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Leitrim, join = st_intersects)




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

Leitrim_district <- county_boundaries %>%
  filter(COUNTY == "LEITRIM")

ggplot() +
  geom_sf(data = Leitrim_district, fill = "blue", color = "black") +
  labs(title = "District of Leitrim") +
  theme_minimal()



# Define the extent and create a grid over Leitrim district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Leitrim_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Leitrim_district)
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
Leitrim_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Leitrim_municipal_with_risk)
unique(Leitrim_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Leitrim_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Leitrim with grid overlay")

# Plotting
Leitrim_map<-ggplot() +
  geom_sf(data = Connacht, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Leitrim_municipal_with_risk[Leitrim_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Leitrim_municipal_with_risk[Leitrim_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Leitrim district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Leitrim_map)

saveRDS(Leitrim_municipal_with_risk, "Leitrim_municipal_with_risk.rds")

# Load it back later
Leitrim_risk <- readRDS("Leitrim_municipal_with_risk.rds")

####Roscommon
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht/Roscommon")  #LOADS
list.files()
Roscommon<-st_read("NLCM_Roscommon_Shapefile.shp") #


# Define 5x5 km grid
bounds <- st_bbox(Roscommon)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Roscommon))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Roscommon, join = st_intersects)




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

Roscommon_district <- county_boundaries %>%
  filter(COUNTY == "ROSCOMMON")

ggplot() +
  geom_sf(data = Roscommon_district, fill = "blue", color = "black") +
  labs(title = "District of Roscommon") +
  theme_minimal()



# Define the extent and create a grid over Roscommon district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Roscommon_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Roscommon_district)
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
Roscommon_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Roscommon_municipal_with_risk)
unique(Roscommon_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Roscommon_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Roscommon with grid overlay")

# Plotting
Roscommon_map<-ggplot() +
  geom_sf(data = Connacht, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Roscommon_municipal_with_risk[Roscommon_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Roscommon_municipal_with_risk[Roscommon_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Roscommon district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Roscommon_map)

saveRDS(Roscommon_municipal_with_risk, "Roscommon_municipal_with_risk.rds")

# Load it back later
Roscommon_risk <- readRDS("Roscommon_municipal_with_risk.rds")



###Sligo
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht/Sligo")
list.files()
Sligo<-st_read("NLCM_Sligo_Shapefile.shp") ##LOADS


# Define 5x5 km grid
bounds <- st_bbox(Sligo)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Sligo))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Sligo, join = st_intersects)




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

sligo_district <- county_boundaries %>%
  filter(COUNTY == "SLIGO")

ggplot() +
  geom_sf(data = sligo_district, fill = "blue", color = "black") +
  labs(title = "District of sligo") +
  theme_minimal()



# Define the extent and create a grid over sligo district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(sligo_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, sligo_district)
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
sligo_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(sligo_municipal_with_risk)
unique(sligo_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = sligo_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of sligo with grid overlay")

ggplot() +
  geom_sf(data = grid_summary_wide[grid_summary_wide$risk_level == "High Risk",], color = "red") +
  geom_sf(data = grid_clipped, color = "blue", fill = NA, alpha = 0.5) +
  labs(title = "High Risk Areas Overlap Check") +
  theme_minimal()

# Plotting
sligo_map<-ggplot() +
  geom_sf(data = Connacht, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = sligo_municipal_with_risk[sligo_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = sligo_municipal_with_risk[sligo_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Sligo district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(sligo_map)

saveRDS(sligo_municipal_with_risk, "sligo_municipal_with_riskk.rds")

# Load it back later
Sligo_risk <- readRDS("sligo_municipal_with_risk.rds")


####Mayo (by municipal district)

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht")
list.files()
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht/Mayo MD")
list.files()


#Mayo by municipal district
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

mayo_districts <- Municipal_Districts %>%
  filter(COUNTY == "MAYO") %>%
  dplyr::select(ENGLISH) %>%
  distinct()

print(mayo_districts$ENGLISH)


#Municipal District of Castlebar
#Municipal District of Claremorris - Swinford
#Municipal District of Westport - Belmullet
#Municipal District of Ballina           

#Mayo_Castlebar
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht/Mayo MD/Mayo_Castlebar")
list.files()
Castlebar<-st_read("NCLM_Castlebar_Shapefile.shp") #LOADS


# Define 5x5 km grid
bounds <- st_bbox(Castlebar)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Castlebar))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Castlebar, join = st_intersects)




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
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

Castlebar_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Castlebar")

ggplot() +
  geom_sf(data = Castlebar_district, fill = "blue", color = "black") +
  labs(title = "District of Castlebar") +
  theme_minimal()



# Define the extent and create a grid over Castlebar district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Castlebar_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Castlebar_district)
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
Castlebar_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)

# verify that join worked
print(Castlebar_municipal_with_risk)
unique(Castlebar_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Castlebar_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Castlebar with grid overlay")

# Plotting
Castlebar_map<-ggplot() +
  geom_sf(data = Connacht, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Castlebar_municipal_with_risk[Castlebar_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Castlebar_municipal_with_risk[Castlebar_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Castlebar district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Castlebar_map)

saveRDS(Castlebar_municipal_with_risk, "Castlebar_municipal_with_risk.rds")

# Load it back later
Castlebar_risk <- readRDS("Castlebar_municipal_with_risk.rds")


#Mayo_Claremorris
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht/Mayo MD")
list.files()
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht/Mayo MD/Mayo_Claremorris")
list.files()
Claremorris<-st_read("NLCM_Claremorris_Shapefile.shp") #LOADS

# Define 5x5 km grid
bounds <- st_bbox(Claremorris)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Claremorris))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Claremorris, join = st_intersects)


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
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

Claremorris_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Claremorris - Swinford")

ggplot() +
  geom_sf(data = Claremorris_district, fill = "blue", color = "black") +
  labs(title = "District of Claremorris") +
  theme_minimal()



# Define the extent and create a grid over Claremorris district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Claremorris_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Claremorris_district)
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
Claremorris_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)

# verify that join worked
print(Claremorris_municipal_with_risk)
unique(Claremorris_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Claremorris_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Claremorris with grid overlay")

# Plotting
Claremorris_map<-ggplot() +
  geom_sf(data = Connacht, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Claremorris_municipal_with_risk[Claremorris_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Claremorris_municipal_with_risk[Claremorris_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Claremorris district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Claremorris_map)

saveRDS(Claremorris_municipal_with_risk, "Claremorris_municipal_with_risk.rds")

# Load it back later
Claremorris_risk <- readRDS("Claremorris_municipal_with_risk.rds")



setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht/Mayo MD")
list.files()
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht/Mayo MD/Mayo_Westport")
list.files()
Westport<-st_read("NLCM_WestportBelmullet_Shapefile.shp") #LOADS

# Define 5x5 km grid
bounds <- st_bbox(Westport)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Westport))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Westport, join = st_intersects)


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
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

Westport_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Westport - Belmullet")

ggplot() +
  geom_sf(data = Westport_district, fill = "blue", color = "black") +
  labs(title = "District of Westport") +
  theme_minimal()



# Define the extent and create a grid over Westport district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Westport_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Westport_district)
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
Westport_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)

# verify that join worked
print(Westport_municipal_with_risk)
unique(Westport_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Westport_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Westport with grid overlay")

ggplot() +
  geom_sf(data = grid_summary_wide[grid_summary_wide$risk_level == "High Risk",], color = "red") +
  geom_sf(data = grid_clipped, color = "blue", fill = NA, alpha = 0.5) +
  labs(title = "High Risk Areas Overlap Check") +
  theme_minimal()

# Plotting
Westport_map<-ggplot() +
  geom_sf(data = Connacht, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Westport_municipal_with_risk[Westport_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Westport_municipal_with_risk[Westport_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Westport district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Westport_map)

saveRDS(Westport_municipal_with_risk, "Westport_municipal_with_risk.rds")

# Load it back later
Westport_risk <- readRDS("Westport_municipal_with_risk.rds")


###Ballina
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht/Mayo MD/Mayo_Ballina")
list.files()
Ballina<-st_read("NLCM_Ballina_Shapefile.shp") #LOADS

# Define 5x5 km grid
bounds <- st_bbox(Ballina)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Ballina))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Ballina, join = st_intersects)


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
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

Ballina_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Ballina")

ggplot() +
  geom_sf(data = Ballina_district, fill = "blue", color = "black") +
  labs(title = "District of Ballina") +
  theme_minimal()



# Define the extent and create a grid over Ballina district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Ballina_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Ballina_district)
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
Ballina_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)

# verify that join worked
print(Ballina_municipal_with_risk)
unique(Ballina_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Ballina_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Ballina with grid overlay")

ggplot() +
  geom_sf(data = grid_summary_wide[grid_summary_wide$risk_level == "High Risk",], color = "red") +
  geom_sf(data = grid_clipped, color = "blue", fill = NA, alpha = 0.5) +
  labs(title = "High Risk Areas Overlap Check") +
  theme_minimal()

# Plotting
Ballina_map<-ggplot() +
  geom_sf(data = Connacht, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Ballina_municipal_with_risk[Ballina_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Ballina_municipal_with_risk[Ballina_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Ballina district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Ballina_map)

saveRDS(Ballina_municipal_with_risk, "Ballina_municipal_with_risk.rds")

# Load it back later
Ballina_risk <- readRDS("Ballina_municipal_with_risk.rds")



##Galway by municipal district
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

galway_districts <- Municipal_Districts %>%
  filter(COUNTY == "GALWAY") %>%
  dplyr::select(ENGLISH) %>%
  distinct()

print(galway_districts$ENGLISH)


#Municipal District of Athenry
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht/Galway MD/Galway_Athenery")
list.files()
Athenry<-st_read("NLCM_Athenry_Shapefile.shp") #loads

# Define 5x5 km grid
bounds <- st_bbox(Athenry)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Athenry))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Athenry, join = st_intersects)


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
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)

Athenry_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Athenry")

ggplot() +
  geom_sf(data = Athenry_district, fill = "blue", color = "black") +
  labs(title = "District of Athenry") +
  theme_minimal()



# Define the extent and create a grid over Athenry district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Athenry_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Athenry_district)
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
Athenry_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)

# verify that join worked
print(Athenry_municipal_with_risk)
unique(Athenry_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Athenry_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Athenry with grid overlay")

# Plotting
Athenry_map<-ggplot() +
  geom_sf(data = Connacht, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Athenry_municipal_with_risk[Athenry_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Athenry_municipal_with_risk[Athenry_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Athenry district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Athenry_map)

saveRDS(Athenry_municipal_with_risk, "Athenry_municipal_with_risk.rds")

# Load it back later
Athenry_risk <- readRDS("Athenry_municipal_with_risk.rds")

#Municipal District of Ballinasloe

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht/Galway MD/Galway_Ballinasloe")
list.files()
Ballinasloe<-st_read("NLCM_Ballinasloe_Shapefile.shp") #loads



# Define 5x5 km grid
bounds <- st_bbox(Ballinasloe)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Ballinasloe))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Ballinasloe, join = st_intersects)


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
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)
print(galway_districts$ENGLISH)

Ballinasloe_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Ballinasloe")

ggplot() +
  geom_sf(data = Ballinasloe_district, fill = "blue", color = "black") +
  labs(title = "District of Ballinasloe") +
  theme_minimal()



# Define the extent and create a grid over ballinasloe district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Ballinasloe_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Ballinasloe_district)
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
Ballinasloe_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)

# verify that join worked
print(Ballinasloe_municipal_with_risk)
unique(Ballinasloe_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Ballinasloe_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Ballinasloe with grid overlay")

# Plotting
Ballinasloe_map<-ggplot() +
  geom_sf(data = Connacht, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Ballinasloe_municipal_with_risk[Ballinasloe_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Ballinasloe_municipal_with_risk[Ballinasloe_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Ballinasloe district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Ballinasloe_map)

saveRDS(Ballinasloe_municipal_with_risk, "Ballinasloe_municipal_with_risk.rds")

# Load it back later
Ballinasloe__risk <- readRDS("Ballinasloe_municipal_with_risk.rds")





#Municipal District of Conamara

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht/Galway MD/Galway_Conamara")
list.files()
Conamara<-st_read("NLCM_Conamara_Shapefile.shp") 



# Define 5x5 km grid
bounds <- st_bbox(Conamara)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Conamara))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Conamara, join = st_intersects)


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
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)
print(galway_districts$ENGLISH)

Conamara_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Conamara")

ggplot() +
  geom_sf(data = Conamara_district, fill = "blue", color = "black") +
  labs(title = "District of Conamara") +
  theme_minimal()



# Define the extent and create a grid over Conamara district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Conamara_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Conamara_district)
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
Conamara_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)

# verify that join worked
print(Conamara_municipal_with_risk)
unique(Conamara_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Conamara_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Conamara with grid overlay")

# Plotting
Conamara_map<-ggplot() +
  geom_sf(data = Connacht, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Conamara_municipal_with_risk[Conamara_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Conamara_municipal_with_risk[Conamara_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Conamara district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Conamara_map)

saveRDS(Conamara_municipal_with_risk, "Conamara_municipal_with_risk.rds")

# Load it back later
Conamara__risk <- readRDS("Conamara_municipal_with_risk.rds")




#Municipal District of Loughrea
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht/Galway MD/Galway_Loughrea")
list.files()
Loughrea<-st_read("NLCM_Loughrea_Shapefile.shp") 



# Define 5x5 km grid
bounds <- st_bbox(Loughrea)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Loughrea))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Loughrea, join = st_intersects)


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
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)
print(galway_districts$ENGLISH)

Loughrea_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Loughrea")

ggplot() +
  geom_sf(data = Loughrea_district, fill = "blue", color = "black") +
  labs(title = "District of Loughrea") +
  theme_minimal()



# Define the extent and create a grid over Loughrea district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Loughrea_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Loughrea_district)
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
Loughrea_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)

# verify that join worked
print(Loughrea_municipal_with_risk)
unique(Loughrea_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Loughrea_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Loughrea with grid overlay")

# Plotting
Loughrea_map<-ggplot() +
  geom_sf(data = Connacht, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Loughrea_municipal_with_risk[Loughrea_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Loughrea_municipal_with_risk[Loughrea_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Loughrea district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Loughrea_map)

saveRDS(Loughrea_municipal_with_risk, "Loughrea__municipal_with_risk.rds")

# Load it back later
Loughrea__risk <- readRDS("Loughrea_municipal_with_risk.rds")


#Municipal District of Tuam 
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/Shapefiles/Connacht/Galway MD/Galway_Tuam")
list.files()
Tuam<-st_read("NLCM_Tuam_Shapefile.shp") #LOADS



# Define 5x5 km grid
bounds <- st_bbox(Tuam)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Tuam))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Tuam, join = st_intersects)


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
Municipal_Districts <- st_read(gpkg_file)
colnames(Municipal_Districts)
unique(Municipal_Districts$ENGLISH)
print(galway_districts$ENGLISH)

Tuam_district <- Municipal_Districts %>%
  filter(ENGLISH == "Municipal District of Tuam")

ggplot() +
  geom_sf(data = Tuam_district, fill = "blue", color = "black") +
  labs(title = "District of Tuam") +
  theme_minimal()



# Define the extent and create a grid over Tuam district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Tuam_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, Tuam_district)
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
Tuam_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)

# verify that join worked
print(Tuam_municipal_with_risk)
unique(Tuam_municipal_with_risk$risk_level)

ggplot() +
  geom_sf(data = Tuam_municipal_with_risk, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "District of Tuam with grid overlay")

# Plotting
Tuam_map<-ggplot() +
  geom_sf(data = Connacht, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Tuam_municipal_with_risk[Tuam_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Tuam_municipal_with_risk[Tuam_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Tuam district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")
print(Tuam_map)

saveRDS(Tuam_municipal_with_risk, "Tuam_municipal_with_risk.rds")

# Load it back later
Tuam_risk<-readRDS("Tuam_municipal_with_risk.rds")



############################### Connacht map with all counties
# Load risk files
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/RDS_files/Connacht")
Leitrim_risk <- readRDS("Leitrim_municipal_with_risk.rds")
Sligo_risk <- readRDS("sligo_municipal_with_risk.rds")
Roscommon_risk <- readRDS("Roscommon_municipal_with_risk.rds")


##Galway 
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/RDS_files/Connacht/Galway MD")
Athenry_risk <- readRDS("Athenry_municipal_with_risk.rds")
Ballinasloe_risk <- readRDS("Ballinasloe_municipal_with_risk.rds")
Conamara_risk <- readRDS("Conamara_municipal_with_risk.rds")
Loughrea_risk <-readRDS("Loughrea_municipal_with_risk.rds")
Tuam_risk <-readRDS("Tuam_municipal_with_risk.rds")



##Mayo
setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD/RDS_files/Connacht/Mayo MD")
Castlebar_risk <- readRDS("Castlebar_municipal_with_risk.rds")
Claremorris_risk <- readRDS("Claremorris_municipal_with_risk.rds")
Ballina_risk <- readRDS("Ballina_municipal_with_risk.rds")
Westport_risk <- readRDS("Westport_municipal_with_risk.rds")


# Plotting
Connacht_map<-ggplot() +
  geom_sf(data = Connacht, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Leitrim_risk[Leitrim_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Leitrim_risk[Leitrim_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Roscommon_risk[Roscommon_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Roscommon_risk[Roscommon_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Sligo_risk[Sligo_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Sligo_risk[Sligo_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Ballina_risk[Ballina_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Ballina_risk[Ballina_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Westport_risk[Westport_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Westport_risk[Westport_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Claremorris_risk[Claremorris_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Claremorris_risk[Claremorris_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Castlebar_risk[Castlebar_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Castlebar_risk[Castlebar_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Athenry_risk[Athenry_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Athenry_risk[Athenry_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Ballinasloe_risk[Ballinasloe_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Ballinasloe_risk[Ballinasloe_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Conamara_risk[Conamara_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Conamara_risk[Conamara_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Loughrea_risk[Loughrea_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Loughrea_risk[Loughrea_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Tuam_risk[Tuam_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Tuam_risk[Tuam_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Connacht district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right",
      axis.text = element_text(size = 16)
)
print(Connacht_map)

#Inset map

setwd("~/Desktop/MSc/Semester 2/Dissertation/Diss files relevant to analysis from HD")
gpkg_file <- "Counties___OSi_National_Statutory_Boundaries_7992851692716515542.gpkg"
county_boundaries <- st_read(gpkg_file)
colnames(county_boundaries)
unique(county_boundaries$COUNTY)
unique(county_boundaries$PROVINCE)


connacht_counties <- county_boundaries %>% 
  filter(PROVINCE == "Connacht")


connacht_inset_map <- ggplot() +
  geom_sf(data = county_boundaries, fill = NA, color = "grey50", size = 0.25) +
  geom_sf(data = connacht_counties, fill = "blue", color = "blue", size = 0.25) +
  labs(title = "") +
  theme_minimal()

print(connacht_inset_map)





