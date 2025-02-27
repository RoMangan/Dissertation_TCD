#Risk Map Municipal Districts

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
library(patchwork)


###############MEATH###############
###############ASHBOURNE 
setwd("/Volumes/Seagate/Municipal Districts")
      /NLCM_Meath_Ashbourne")
Ashbourne <- st_read("NLCM_Meath_Ashbourne_Shapefile.shp")

#define 5x5 km grid
bounds <- st_bbox(Ashbourne)
bounds <- st_bbox(st_buffer(Ashbourne, dist = 500)) #
x_seq <- seq(bounds$xmin, bounds$xmax, by = 5000)  # grid step size 5 km
y_seq <- seq(bounds$ymin, bounds$ymax, by = 5000)



# create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Ashbourne))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Ashbourne, join = st_intersects)
table(rowSums(intersections))
# if Error in base::rowSums(x, na.rm = na.rm, dims = dims, ...) : use code below
#'x' must be numeric
intersections <- st_intersects(grid_clipped, grid_summary_wide, sparse = FALSE)
table(rowSums(intersections))

table(rowSums(intersections))


#summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA

grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

#check structure of summarized data
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


setwd("/Volumes/Seagate/GeoHive_Counties")
gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
municipal_boundaries <- st_read(gpkg_file)
ashbourne_district <- municipal_boundaries %>%
  filter(ENGLISH == "Municipal District of Ashbourne")


ggplot() +
  geom_sf(data = ashbourne_district, fill = "blue", color = "black") +
  labs(title = "Municipal District of Ashbourne") +
  theme_minimal()

# Define the extent and create a grid over Ashbourne district

bounds <- st_bbox(ashbourne_district)
bounds <- st_bbox(st_buffer(ashbourne_district, dist = 500))
x_seq <- seq(bounds$xmin, bounds$xmax, by = 5000)  # Grid step size 5 km
y_seq <- seq(bounds$ymin, bounds$ymax, by = 5000)

grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(ashbourne_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, ashbourne_district)

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

Ashbourne_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)

# Now verify that the join has worked
print(class(municipal_with_risk))

ggplot() +
  geom_sf(data = ashbourne_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "Municipal district of Ashbourne with grid overlay")

# Plotting

ggplot() +
  geom_sf(data = municipal_with_risk, aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow", "NA" = "grey50")) +
  labs(title = "Risk levels in Ashbourne Municipal District", fill = "Risk Level") +
  theme_minimal()

Ashbourne_map<-ggplot() +
  geom_sf(data = grid_clipped, fill = NA, color = "black", lwd = 0.5) +
  geom_sf(data = municipal_with_risk, aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(
    values = c(
      "High Risk" = "red",
      "Low Risk" = "green",
      "Neutral Risk" = "yellow",
      "Unknown Risk" = "gray"
    )
  ) +
  labs(title = "Risk Levels in Ashbourne Municipal District", fill = "Risk Level") +
  theme_minimal()

#################BETTYSTWOWN
setwd("/Volumes/Seagate/Municipal Districts/NLCM_Meath_Bettystown")
Bettystown <-st_read("NLCM_Meath_Bettystown_Shapefile.shp")

# Define 5x5 km grid
bounds <- st_bbox(Bettystown)
x_seq <- seq(bounds$xmin, bounds$xmax, by = 5000)  # Grid step size 5 km
y_seq <- seq(bounds$ymin, bounds$ymax, by = 5000)



# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Bettystown))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Bettystown, join = st_intersects)


# summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

#check structure of summarized data
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


setwd("/Volumes/Seagate/GeoHive_Counties")
gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
municipal_boundaries <- st_read(gpkg_file)
bettystown_district <- municipal_boundaries %>%
  filter(ENGLISH == "Municipal District of Laytown - Bettystown")

ggplot() +
  geom_sf(data = bettystown_district, fill = "blue", color = "black") +
  labs(title = "Municipal District of Laytown - Bettystown") +
  theme_minimal()

# Define the extent and create a grid over Bettystown district
bounds <- st_bbox(bettystown_district)
x_seq <- seq(bounds$xmin, bounds$xmax, by = 5000)  # Grid step size 5 km
y_seq <- seq(bounds$ymin, bounds$ymax, by = 5000)

grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(bettystown_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, bettystown_district)

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

bettystown_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(class(bettystown_municipal_with_risk))

ggplot() +
  geom_sf(data = bettystown_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "Municipal district of Bettystown with grid overlay")

# Plotting
ggplot() +
  geom_sf(data = municipal_with_risk, aes(fill = risk_level), color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow", "NA" = "grey50")) +
  labs(title = "Risk levels in Bettystown Municipal District", fill = "Risk Level") +
  theme_minimal()




Bettystown_map<-ggplot() +
  geom_sf(data = municipal_with_risk, fill = NA, color = "black", lwd = 0.5) +
  geom_sf(data = municipal_with_risk, aes(fill = risk_level), color = "black", alpha = 0.8) +
  scale_fill_manual(
    values = c(
      "High Risk" = "red",
      "Low Risk" = "green",
      "Neutral Risk" = "yellow",
      "Unknown Risk" = "gray"
    )
  ) +
  labs(title = "Risk levels in Bettystown Municipal District", fill = "Risk Level") +
  theme_minimal()

#############NAVAN 
setwd("/Volumes/Seagate/Municipal Districts")
list.files()
setwd("/Volumes/Seagate/Municipal Districts/NLCM_Meath_Navan")
list.files()
Navan <-st_read("NLCM_Meath_Navan_Shapefile.shp")

# Define 5x5 km grid
bounds <- st_bbox(Navan)
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
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Navan))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Navan, join = st_intersects)




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


setwd("/Volumes/Seagate/GeoHive_Counties")
gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
municipal_boundaries <- st_read(gpkg_file)
colnames(municipal_boundaries)
unique(municipal_boundaries$ENGLISH)

navan_district <- municipal_boundaries %>%
  filter(ENGLISH == "Municipal District of Navan")

ggplot() +
  geom_sf(data = navan_district, fill = "blue", color = "black") +
  labs(title = "Municipal District of Navan") +
  theme_minimal()



# Define the extent and create a grid over Navan district
x_seq <- seq(from = extended_bounds["xmin.xmin"], to = extended_bounds["xmax.xmax"], by = 5000)
y_seq <- seq(from = extended_bounds["ymin.ymin"], to = extended_bounds["ymax.ymax"], by = 5000)


grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(navan_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, navan_district)
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
Navan_municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(Navan_municipal_with_risk)

ggplot() +
  geom_sf(data = navan_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "Municipal district of Navan with grid overlay")

# Plotting
Navan_map<-ggplot() +
  geom_sf(data = navan_district, fill = "white", color = "black", size = 0.1) +  # Base map
  geom_sf(data = Navan_municipal_with_risk[Navan_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Navan_municipal_with_risk[Navan_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("Low Risk" = "green", "Neutral Risk" = "yellow", "High Risk" = "red")) +
  labs(title = "Risk Levels in Navan municipal district", fill = "Risk Level") +
  theme_minimal() +
  theme(legend.position = "right")


#################TRIM
setwd("/Volumes/Seagate/Municipal Districts/NLCM_Meath_Trim")
Trim <-st_read("NLCM_Meath_Trim_Shapefile.shp")

list.files()
# Define 5x5 km grid
bounds <- st_bbox(Trim)
x_seq <- seq(bounds$xmin, bounds$xmax, by = 5000)  # Grid step size 5 km
y_seq <- seq(bounds$ymin, bounds$ymax, by = 5000)


# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Trim))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Trim, join = st_intersects)


# summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

#check structure of summarized data
print(n=89, grid_summary)

# Pivot the data wider to create columns for each LEVEL_1_VA category with their corresponding AREA
grid_summary_wide <- grid_summary %>%
  pivot_wider(names_from = LEVEL_1_VA, values_from = total_AREA, values_fill = list(total_AREA = 0))

# Determine the majority land cover by area using a custom function to find the maximum column name by values
grid_summary_wide$majority_land_cover <- apply(grid_summary_wide[,-1], 1, function(x) names(x)[which.max(x)])

# Assign risk based on the majority land cover
grid_summary_wide$risk_level <- case_when(
  grid_summary_wide$majority_land_cover %in% c("ARTIFICIAL SURFACES", "EXPOSED SURFACES", "CULTIVATED LAND") ~ "High Risk",
  grid_summary_wide$majority_land_cover %in% c("GRASSLAND, SALTMARSH and SWAMP", "PEATLAND") ~ "Low Risk",
  grid_summary_wide$majority_land_cover %in% c("FOREST, WOODLAND AND SCRUB", "WATERBODIES", "HEATH and BRACKEN") ~ "Neutral Risk",
  TRUE ~ "Unknown Risk"  # Ensure all cases are covered
)
# Check the wide data structure
print(grid_summary_wide)
unique(grid_summary_wide$majority_land_cover)
unique(grid_summary_wide$risk_level)

table(grid_summary_wide$risk_level)


# plot to visually inspect
ggplot(grid_summary_wide) +
  geom_sf(aes(fill = risk_level)) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow", "Unknown Risk" = "gray")) +
  labs(title = "Risk levels in Trim municipal district", fill = "Risk Level") +
  theme_minimal()


setwd("/Volumes/Seagate/GeoHive_Counties")
gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
municipal_boundaries <- st_read(gpkg_file)
colnames(municipal_boundaries)
unique(municipal_boundaries$ENGLISH)
trim_district <- municipal_boundaries %>%
  filter(ENGLISH == "Municipal District of Trim")

library(ggplot2)
ggplot() +
  geom_sf(data = trim_district, fill = "blue", color = "black") +
  labs(title = "Municipal District of Trim") +
  theme_minimal()

# Define the extent and create a grid over Trim district
bounds <- st_bbox(trim_district)
x_seq <- seq(bounds$xmin, bounds$xmax, by = 5000)  # Grid step size 5 km
y_seq <- seq(bounds$ymin, bounds$ymax, by = 5000)

grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(trim_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, trim_district)

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
st_crs(grid_clipped) <- st_crs(grid_summary_wide)
municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)
print(unique(municipal_with_risk$risk_level))


# ensure the CRS matches before joining
if (st_crs(grid_summary_wide) != st_crs(municipal_with_risk)) {
  grid_summary_wide <- st_transform(grid_summary_wide, st_crs(municipal_with_risk))
}

municipal_with_risk_updated <- st_join(municipal_boundaries, grid_summary_wide, join = st_intersects)
print(unique(municipal_with_risk_updated$risk_level))


# verify that join worked
print(class(municipal_with_risk))
table(municipal_with_risk$risk_level)
table(municipal_with_risk_updated$risk_level)

ggplot() +
  geom_sf(data = trim_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "Municipal district of Trim with grid overlay")

# Plotting
ggplot() +
  geom_sf(data = municipal_with_risk, aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow", "NA" = "grey50")) +
  labs(title = "Risk levels in Trim municipal district", fill = "Risk Level") +
  theme_minimal()




Trim_map<-ggplot() +
  geom_sf(data = trim_district, fill = NA, color = "black", lwd = 0.5) +
  geom_sf(data = municipal_with_risk, aes(fill = risk_level), color = "black", alpha = 0.8) +
  scale_fill_manual(
    values = c(
      "High Risk" = "red",
      "Low Risk" = "green",
      "Neutral Risk" = "yellow",
      "Unknown Risk" = "gray"
    )
  ) +
  labs(title = "Risk levels in Trim Municipal District", fill = "Risk Level") +
  theme_minimal()

ggplot() +
  geom_sf(data = grid_summary_wide[grid_summary_wide$risk_level == "High Risk",], color = "red") +
  geom_sf(data = grid_clipped, color = "blue", fill = NA, alpha = 0.5) +
  labs(title = "High Risk Areas Overlap Check") +
  theme_minimal()


#################Rathoat
setwd("/Volumes/Seagate/Municipal Districts/NLCM_Meath_Ratoath")
Ratoath <-st_read("NLCM_Meath_Ratoath_Shapefile.shp",quiet = TRUE)




# Define 5x5 km grid
bounds <- st_bbox(Ratoath)
x_seq <- seq(bounds$xmin, bounds$xmax, by = 5000)  # Grid step size 5 km
y_seq <- seq(bounds$ymin, bounds$ymax, by = 5000)



# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Ratoath))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Ratoath, join = st_intersects)


# summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

#check structure of summarized data
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


setwd("/Volumes/Seagate/GeoHive_Counties")
gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
municipal_boundaries <- st_read(gpkg_file)
colnames(municipal_boundaries)
unique(municipal_boundaries$ENGLISH)
ratoath_district <- municipal_boundaries %>%
  filter(ENGLISH == "Municipal District of Ratoath")

ggplot() +
  geom_sf(data = ratoath_district, fill = "blue", color = "black") +
  labs(title = "Municipal District of Ratoath") +
  theme_minimal()

# Define the extent and create a grid over Ratoath district
bounds <- st_bbox(ratoath_district)
x_seq <- seq(bounds$xmin, bounds$xmax, by = 5000)  # Grid step size 5 km
y_seq <- seq(bounds$ymin, bounds$ymax, by = 5000)

grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(ratoath_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, ratoath_district)

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

municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(class(municipal_with_risk))

ggplot() +
  geom_sf(data = ratoath_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "Municipal district of Ratoath with grid overlay")

# Plotting
ggplot() +
  geom_sf(data = municipal_with_risk, aes(fill = risk_level), color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow", "NA" = "grey50")) +
  labs(title = "Risk levels in Ratoath municipal district", fill = "Risk Level") +
  theme_minimal()




Ratoath_map<-ggplot() +
  geom_sf(data = municipal_with_risk, fill = NA, color = "black", lwd = 0.5) +
  geom_sf(data = municipal_with_risk, aes(fill = risk_level), color = "black", alpha = 0.8) +
  scale_fill_manual(
    values = c(
      "High Risk" = "red",
      "Low Risk" = "green",
      "Neutral Risk" = "yellow",
      "Unknown Risk" = "gray"
    )
  ) +
  labs(title = "Risk levels in Ratoath municipal district", fill = "Risk Level") +
  theme_minimal()


#################Kells
setwd("/Volumes/Seagate/Municipal Districts/NLCM_Meath_Kells")
Sys.setenv(OGR_GEOMETRY_ACCEPT_UNCLOSED_RING = "NO")
Kells <-st_read("NLCM_Meath_Kells_Shapefile.shp", quiet = TRUE)

setwd("/Volumes/Seagate/Municipal Districts/NLCM_Meath_Kells_Repaired")
list.files()
Kells<-st_read("NLCM_MEATHKELLS_Shapefile_Repaired.shp")


# Define 5x5 km grid
bounds <- st_bbox(Kells)
x_seq <- seq(bounds$xmin, bounds$xmax, by = 5000)  # Grid step size 5 km
y_seq <- seq(bounds$ymin, bounds$ymax, by = 5000)



# Create grid cells as sf polygons
grid <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
coordinates <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(Kells))
grid <- st_make_grid(coordinates, cellsize = c(5000, 5000), square = TRUE)
grid <- st_sf(gr_id = as.character(seq_along(grid)), geometry = grid)  # Assigning an identifier

# Perform spatial join using st_join
intersections <- st_join(grid, Kells, join = st_intersects)


# summarize land cover types by area within each grid cell, grouped also by LEVEL_1_VA
grid_summary <- intersections %>%
  group_by(gr_id, LEVEL_1_VA) %>%
  summarize(total_AREA = sum(AREA, na.rm = TRUE), .groups = "drop")

#check structure of summarized data
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


setwd("/Volumes/Seagate/GeoHive_Counties")
gpkg_file <- "Municipal_Districts___OSi_National_Statutory_Boundaries_-180092851620979653.gpkg"
municipal_boundaries <- st_read(gpkg_file)
colnames(municipal_boundaries)
unique(municipal_boundaries$ENGLISH)
kells_district <- municipal_boundaries %>%
  filter(ENGLISH == "Municipal District of Kells")

ggplot() +
  geom_sf(data = kells_district, fill = "blue", color = "black") +
  labs(title = "Municipal District of Kells") +
  theme_minimal()

# Define the extent and create a grid over Kells district
bounds <- st_bbox(kells_district)
x_seq <- seq(bounds$xmin, bounds$xmax, by = 5000)  # Grid step size 5 km
y_seq <- seq(bounds$ymin, bounds$ymax, by = 5000)

grid <- expand.grid(x = x_seq, y = y_seq)
grid <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(kells_district))
grid_polygons <- st_make_grid(grid, cellsize = c(5000, 5000), square = TRUE)

grid_clipped <- st_intersection(grid_polygons, kells_district)

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

municipal_with_risk <- st_join(grid_clipped, grid_summary_wide, join = st_intersects)


# verify that join worked
print(class(municipal_with_risk))

ggplot() +
  geom_sf(data = kells_district, fill = "white", color = "black") +
  geom_sf(data = grid_clipped, fill = NA, color = "red", lwd = 0.2, alpha = 0.5) +  # semi-transparent grid lines
  theme_minimal() +
  labs(title = "Municipal district of Kells with grid overlay")

# Plotting
ggplot() +
  geom_sf(data = municipal_with_risk, aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow", "NA" = "grey50")) +
  labs(title = "Risk levels in Kells Municipal District", fill = "Risk Level") +
  theme_minimal()




Kells_map<-ggplot() +
  geom_sf(data = municipal_with_risk, fill = NA, color = "black", lwd = 0.5) +
  geom_sf(data = municipal_with_risk, aes(fill = risk_level), color = "black", alpha = 0.8) +
  scale_fill_manual(
    values = c(
      "High Risk" = "red",
      "Low Risk" = "green",
      "Neutral Risk" = "yellow",
      "Unknown Risk" = "gray"
    )
  ) +
  labs(title = "Risk levels in Kells Municipal District", fill = "Risk Level") +
  theme_minimal()

meath_districts <- municipal_boundaries %>%
  filter(COUNTY == "MEATH") %>%
  pull(ENGLISH)

# Print the list of municipal districts in Meath
print(meath_districts)
municipal_boundaries <- st_read(gpkg_file)

meath_districts_table <- municipal_boundaries %>%
  filter(COUNTY == "MEATH") %>%
  dplyr::select(ENGLISH, COUNTY)

print(meath_districts_table)

Leinster <-municipal_boundaries %>%
  filter(PROVINCE == "Leinster")

# Plotting
ggplot() +
  geom_sf(data = Leinster, fill = "white", color = "black") +
  geom_sf(data = Trim_municipal_with_risk, aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Navan_municipal_with_risk[Navan_municipal_with_risk$risk_level != "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Navan_municipal_with_risk[Navan_municipal_with_risk$risk_level == "Neutral Risk", ], aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = Ashbourne_municipal_with_risk, aes(fill = risk_level), color = "black", alpha = 1.0) +
  geom_sf(data = bettystown_municipal_with_risk, aes(fill = risk_level), color = "black", alpha = 1.0) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Neutral Risk" = "yellow", "NA" = "grey50")) +
  labs(title = "Risk levels in Leinster", fill = "Risk Level") +
  theme_minimal()


         
NLCM_Meath_Kells          
NLCM_Meath_Kells_Repaired       
NLCM_Meath_Ratoath             


