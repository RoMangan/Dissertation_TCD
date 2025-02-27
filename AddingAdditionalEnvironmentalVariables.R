#Merging Level1VA datasets to NAP sampling location data and NAP & Coloss data after sampling 
setwd("/Users/rosiemangan/Desktop/MSc/Semester 2/Dissertation/R script")
library(dplyr)
install.packages("sf")
install.packages("nngeo")  # if not already installed
library(sf)
library(nngeo)
library(dplyr)
library(stringr)

####################Level 1 VA #########################

L1VA<-read.csv("Path_Pest_Pollen_Honey_Landcover_L1VA.csv")

Date<-read.csv("NAP_sampling_and_location_data.csv")

Date <- dplyr::select(Date, NAP_number, County, Month_sampled, Date_sampled, Days_in_minus_20_freezer, No._colonies_sampled)


ColossAfter<-read.csv("NAP_COLOSS_data_after_sampling.csv")
colnames(ColossAfter)

ColossAfter <- dplyr::select(ColossAfter, NAP_number, Q2_No._apiaries_after, Q5_No._queen_problems_after, Q6_No._natural_disaster_after, Q8c_signs_of_starvation_after, Q17_treated_for_Varroa_prev_year_after)

L1VA_Date<-left_join(L1VA, Date)

L1VA_Date <- L1VA_Date %>% 
  select(-Row_Sum)

L1VA_Date_Coloss<-left_join(L1VA_Date, ColossAfter)

write.csv(L1VA_Date_Coloss,"Path_Pest_Pollen_Honey_Landcover_Date_Coloss_L1VA.csv")
L1VA_Date_Coloss<-read.csv("Path_Pest_Pollen_Honey_Landcover_Date_Coloss_L1VA.csv")


#### ADD VARIABLES TO EACH DATA-FRAME THAT WILL BE USE IN MODELLING
#L1VA
#Environmental Covariates

####1_LANDCOVER TYPE-Proportions of different land cover types (agricultural, urban, forest, grassland)

# Calculate the proportion of Cultivated Land (already present)
L1VA_Date_Coloss$CULTIVATED.LAND <- as.numeric(L1VA_Date_Coloss$CULTIVATED.LAND)
L1VA_Date_Coloss$Prop_Cultivated_Land <- L1VA_Date_Coloss$CULTIVATED.LAND
head(L1VA_Date_Coloss)



####2_PROXIMITY TO WATER SOURCES
library(osmdata)
library(sf)

#2A_PROPORTION OF WATER BODIES IN 1 KM RADIUS

# Calculate the proportion of Water Bodies
L1VA_Date_Coloss$WATERBODIES <- as.numeric(L1VA_Date_Coloss$WATERBODIES)
L1VA_Date_Coloss$Prop_Waterbodies <- L1VA_Date_Coloss$WATERBODIES
head(L1VA_Date_Coloss)

head(L1VA_Date_Coloss)


#2B_PROXIMITY TO NEARST NATURAL WATER SOURCE

# Create bounding box
min_latitude <- min(L1VA_Date_Coloss$Latitude, na.rm = TRUE)
max_latitude <- max(L1VA_Date_Coloss$Latitude, na.rm = TRUE)
min_longitude <- min(L1VA_Date_Coloss$Longitude, na.rm = TRUE)
max_longitude <- max(L1VA_Date_Coloss$Longitude, na.rm = TRUE)

bbox <- c(min_longitude, min_latitude, max_longitude, max_latitude)
bbox

water_query <- opq(bbox = bbox) %>% 
  add_osm_feature(key = "natural", value = "water") %>%
  add_osm_feature(key = "waterway", value = "river") %>% 
  add_osm_feature(key = "waterway", value = "stream") %>%
  osmdata_sf()
print(water_query)

locations_sf <- st_as_sf(L1VA_Date_Coloss, coords = c("Longitude", "Latitude"), crs = 2157)

# Re-project to WGS 84 for compatibility
locations_sf_wgs84 <- st_transform(locations_sf, 4326)

water_query <- opq(bbox = bbox) %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()
print(water_query)

locations_sf <- st_as_sf(L1VA_Date_Coloss, coords = c("Longitude", "Latitude"), crs = 4326)  # original CRS definition
locations_sf_itm <- st_transform(locations_sf, 2157)  # transform to Irish Transverse Mercator


#Transform the multipolygons to Irish Transverse Mercator
water_multipolygons_itm <- st_transform(water_query$osm_multipolygons, 2157)

# Confirm transformation
print(st_crs(water_multipolygons_itm))

# Calculate the minimum distance to nearest water feature
distances <- st_distance(locations_sf_itm, water_multipolygons_itm)

min_distances <- apply(distances, 1, min)  # Find the minimum distance for each location

# Check the minimum distances to confirm they vary and make sense
print(min_distances)

# Add min distances to nearest water feature to dataframe
L1VA_Date_Coloss$Min_Dist_Water_metre <- min_distances

# View the updated dataframe
head(L1VA_Date_Coloss)

write.csv(L1VA_Date_Coloss, "L1VA_Date_Coloss_withGAM_#2B_PROXIMITyNEARSTNATURALWATERSOURCE.csv")



#2C_PROXIMITY TO COAST

# Assuming you have the bounding box defined as follows:
bbox <- c(min_longitude, min_latitude, max_longitude, max_latitude)

# Query for coastal features (typically tagged as "natural" = "coastline")
coast_query <- opq(bbox = bbox) %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()
print(coast_query)

# Extract coastline data as linestrings
coastline_sf <- coast_query$osm_lines 

# Transform coastline data to Irish Transverse Mercator for consistency with your location data
coastline_itm <- st_transform(coastline_sf, 2157)

# Ensure locations data is in the same CRS
locations_sf_itm <- st_transform(locations_sf, 2157)

# Calculate distance to the nearest coastline
distances_to_coast <- st_distance(locations_sf_itm, coastline_itm)
min_distances_to_coast <- apply(distances_to_coast, 1, min)  # Find the minimum distance for each location
print(min_distances_to_coast)

# Add min distances to coast feature to dataframe
L1VA_Date_Coloss$Min_Dist_Coast_metre <- min_distances_to_coast
head(L1VA_Date_Coloss)

write.csv(L1VA_Date_Coloss, "L1VA_Date_Coloss_withGAM_#2B_PROXIMITY TO COAST.csv")

L1VA_Date_Coloss<-read.csv("L1VA_Date_Coloss_withGAM_#2B_PROXIMITY TO COAST.csv")

View(L1VA_Date_Coloss)

####3_CLIMATE VARIABLES
#3A_AVERAGE TEMP
#3B_AVERAGE HUMIDITY
#3c_AVERAGE RAINFALL

closest_stations<-read.csv("closest_stations_July29.csv")
weather<-read.csv("final_weather_data_2021.csv")

#names correction
name_corrections <- c("PhoenixPark" = "Phoenix Park",
                      "Casement" = "Casement Aerodrome",
                      "Dunsany" = "Dunsany",
                      "Newport" = "Newport Furnace",
                      "JohnstownCastle" = "Johnstown Castle",
                      "Oak Park" = "Oak Park",
                      "Moore Park" = "Moore Park",
                      "Malin Head " = "Malin Head",  # Notice the trailing space
                      "Ballyhaise" = "Ballyhaise",
                      "Mt Dillion" = "Mount Dillon",
                      "Shannon Airport" = "Shannon Airport",
                      "Gurteen" = "Gurteen",
                      "Mullingar" = "Mullingar",
                      "Knock Airport" = "Knock Airport",
                      "Claremorris" = "Claremorris",
                      "Macehead" = "Mace Head",
                      "Valentia Observatory" = "Valentia Observatory",
                      "SherkinIsland" = "Sherkin Island",
                      "Cork Airport" = "Cork Airport",
                      "Roches Point" = "Roches Point",
                      "Finner" = "Finner",
                      "Belmullet" = "Belmullet")

# Apply corrections
closest_stations$Closest_Station <- sapply(closest_stations$Closest_Station, function(x) name_corrections[x])

# Merge the datasets again with corrected station names
enhanced_station_data <- merge(closest_stations, weather, by.x = "Closest_Station", by.y = "Station", all.x = TRUE)
View(enhanced_station_data)
# Display the first few rows to verify correct merging
head(enhanced_station_data)

# Now, merge this enhanced station data with the L1VA_Date_Coloss dataframe
L1VA_Date_Coloss <- merge(L1VA_Date_Coloss, enhanced_station_data, by = "NAP_number", all.x = TRUE)
View(L1VA_Date_Coloss)
# View the head of the final merged data to verify correctness
print(head(L1VA_Date_Coloss))


View(L1VA_Date_Coloss)

write.csv(L1VA_Date_Coloss, "L1VA_Date_Coloss_withGAM_#2B_CLIMATEVARIABLES.csv")

L1VA_Date_Coloss<-read.csv("L1VA_Date_Coloss_withGAM_#2B_CLIMATEVARIABLES.csv")

head(L1VA_Date_Coloss)
View(L1VA_Date_Coloss)

#Agricultural Practices

#1_Pesticide Usage

#1_Fertilizer Usage

# Calculate the proportion of GRASSLAND (already present) and these areas are heavily managed to maximise production- heavily fertilised.
L1VA_Date_Coloss$Prop_Fert_Improved.Grassland <- L1VA_Date_Coloss$GRASSLAND..SALTMARSH.and.SWAMP
View(L1VA_Date_Coloss)


#Hive Management

####1_Beehive Location (Elevation and geographical coordinates)

##1_Elevation
library(elevatr)
library(dplyr)
library(sf) 
library(sp)
library(rgdax)
library(readr)

L1VA_Date_Coloss$Latitude <- as.numeric(L1VA_Date_Coloss$Latitude)
L1VA_Date_Coloss$Longitude <- as.numeric(L1VA_Date_Coloss$Longitude)

#Get elevation

locations <- data.frame(x = L1VA_Date_Coloss$Longitude, y = L1VA_Date_Coloss$Latitude)
elevation_data <- get_elev_point(locations = locations, 
                                 prj = "+proj=longlat +datum=WGS84", 
                                 src = "aws")

# Combine elevation data back with the original data, handling NAs appropriately
locations$elevation <- NA  # Initialize elevation with NA
locations$elevation[!is.na(locations$x) & !is.na(locations$y)] <- elevation_data$elevation


# Append elevation data back to the original data frame
L1VA_Date_Coloss$Elevation <- elevation_data$elevation
head(L1VA_Date_Coloss)

####2_Hive Density

#Convert data to spatial object to create an SF object
L1VA_Date_Coloss_sf <- st_as_sf(L1VA_Date_Coloss, coords = c("Longitude", "Latitude"), crs = 4326)
L1VA_Date_Coloss_sf <- st_transform(L1VA_Date_Coloss_sf, 2157)  # Convert to ITM

#Calculate distances and count apiaries

# Calculate distance matrix
distance_matrix <- st_distance(L1VA_Date_Coloss_sf)

# Define thresholds for distances in meters
thresholds <- c(5000, 10000, 25000)  #5km, 10km, and 25km 

# Count apiaries within each distance threshold
for (threshold in thresholds) {
  # Adding calculated values directly to the original dataframe
  L1VA_Date_Coloss[[paste0("Apiaries_within_", threshold / 1000, "km")]] <- apply(distance_matrix, 1, function(x) sum(x <= threshold, na.rm = TRUE) - 1)
}

View(L1VA_Date_Coloss)



####3_Beekeeper practices
#Treatments for pests and diseases or hive maintenance routines-(NAP COLOSS_Q17_treated_for_Varroa_prev_year_after)
BEFORE<-read_csv("NAP_COLOSS_data_before_sampling.csv")
colnames(BEFORE)
head(BEFORE)

BEFORE <- dplyr::select(BEFORE, NAP_number, Q19_treated_for_Varroa_prev_year)
head(BEFORE)

L1VA_Date_Coloss<-left_join(L1VA_Date_Coloss, BEFORE, by = "NAP_number")
View(L1VA_Date_Coloss)
write.csv(L1VA_Date_Coloss, "L1VA_Date_Coloss_Beekeeperpractices.csv")





#2_Human Activity
#Proximity to urban areas

# Query OSM for features tagged as 'landuse' with 'residential', 'commercial', or 'industrial'
urban_areas <- opq(bbox) %>%
  add_osm_feature(key = 'landuse', value = 'residential') %>%
  add_osm_feature(key = 'landuse', value = 'commercial') %>%
  add_osm_feature(key = 'landuse', value = 'industrial') %>%
  osmdata_sf()

print(urban_areas)

# Transform urban area polygons to ITM
urban_polygons_itm <- st_transform(urban_areas$osm_polygons, 2157)

# Calculate the minimum distance to the nearest urban polygon
urban_distances <- st_distance(locations_sf_itm, urban_polygons_itm)
min_urban_distances <- apply(urban_distances, 1, min)  # Find the minimum distance for each location

# Add min distances to nearest urban area to dataframe
L1VA_Date_Coloss$Min_Dist_Urban_metre <- min_urban_distances
head(L1VA_Date_Coloss)



#Traffic density and associated pollution

#Query major roads
road_query <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway", value = "motorway") %>%
  add_osm_feature(key = "highway", value = "trunk") %>%
  add_osm_feature(key = "highway", value = "primary") %>%
  osmdata_sf()

#transform and calculate distances

road_lines_itm <- st_transform(road_query$osm_lines, 2157)

# Calculate distances to the nearest major road
road_distances <- st_distance(locations_sf_itm, road_lines_itm)

min_road_distances <- apply(road_distances, 1, min)
L1VA_Date_Coloss$Min_Dist_Road_metre <- min_road_distances
head(L1VA_Date_Coloss)
write.csv(L1VA_Date_Coloss, "L1VA_Dataset_final.csv")

L1VA_Date_Coloss<-read.csv("L1VA_Dataset_final.csv")

####4_SOIL

library(sf)  # for handling spatial data
library(raster)  # for handling raster and spatial operations
library(dplyr)  # for data manipulation
library(sp)

#1 August

#Load Soil Data
# Set wd to where shapefiles are stored
path_to_shapefile <- setwd("/Users/rosiemangan/Desktop/MSc/Semester 2/Dissertation/GAM covariates/SOIL_SISNationalSoils_shp/Data")
list.files()
# Load shapefile
soil_data <- st_read(path_to_shapefile)
View(soil_data)

# check data read correctly
if (!is.null(soil_data)) {
  print("Shapefile loaded successfully.")
  print(soil_data)
} else {
  print("Failed to load shapefile. Please check the file path and ensure all necessary files are present.")
}

#remove columns
columns_to_keep <- setdiff(names(soil_data), c("UniqueId", "Associatio", "Associat_1", "Associat_2", "ha", "DEPTH", "SOC", "URL", "Shape_STAr", "Shape_STLe"))
soil_data <- soil_data[, columns_to_keep]

print(colnames(soil_data))

#Load Beehive Data
path_to_shapefile <-setwd("/Users/rosiemangan/Desktop/MSc/Semester 2/Dissertation/1km_Buffer_Land_Use")
list.files()
beehive_data <- st_read(path_to_shapefile)

# check data read correctly
if (!is.null(beehive_data)) {
  print("Shapefile loaded successfully.")
  print(beehive_data)
} else {
  print("Failed to load shapefile. Please check the file path and ensure all necessary files are present.")
}

# Check CRS of both datasets
print(st_crs(beehive_data)) #IRENET95 / Irish Transverse Mercator
print(st_crs(soil_data)) #TM65 / Irish Grid

# Transform soil data CRS to match beehive data CRS
soil_data_transformed <- st_transform(soil_data, st_crs(beehive_data))

# Perform the spatial join
combined_data <- st_join(beehive_data, soil_data_transformed, left = TRUE)

# View combined data
head(combined_data)

#filter the combined data to keep only rows where descriptio == 1
filtered_data <- combined_data %>%
  filter(descriptio == 1)

#summarize the unique values for DRAINAGE, Texture_Su, and PlainEngli
drainage_values <- unique(filtered_data$DRAINAGE)
texture_su_values <- unique(filtered_data$Texture_Su)
plainengli_values <- unique(filtered_data$PlainEngli)

# print unique values
print("Unique DRAINAGE values:")
print(drainage_values)

print("Unique Texture_Su values:")
print(texture_su_values)

print("Unique PlainEngli values:")
print(plainengli_values)
#more than one value for plainengli, texture_su, and drainage.

# Function to calculate the mode (most frequent value)
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Function to summarize unique values for a given descriptio
summarize_values <- function(data, descriptio_value) {
  filtered_data <- data %>% filter(descriptio == descriptio_value)
  list(
    descriptio = descriptio_value,
    DRAINAGE = calculate_mode(filtered_data$DRAINAGE),
    Texture_Su = calculate_mode(filtered_data$Texture_Su),
    PlainEngli = calculate_mode(filtered_data$PlainEngli)
  )
}


# get all unique descriptio values
descriptio_values <- unique(combined_data$descriptio)


#apply summarise_values function to all descriptio values
summary_list <- lapply(descriptio_values, function(x) summarize_values(combined_data, x))

# convert list to a data frame
summary_df <- do.call(rbind, lapply(summary_list, as.data.frame))
View(summary_df)
colnames(summary_df)


names(summary_df)[names(summary_df) == "descriptio"] <- "Site_Id"



summary_df<- summary_df %>%
  filter(!grepl("Repeat", Site_Id))

# Add the new prefix to Site_Id
summary_df$Beekeeper_number <- NA

for(i in 1:nrow(summary_df)) {
  site_length <- str_length(summary_df$Site_Id[i])
  if(site_length == 1) {
    summary_df$Beekeeper_number[i] <- paste0("NAP_00", summary_df$Site_Id[i])
  } else if(site_length == 2) {
    summary_df$Beekeeper_number[i] <- paste0("NAP_0", summary_df$Site_Id[i])
  } else if(site_length == 3) {
    summary_df$Beekeeper_number[i] <- paste0("NAP_", summary_df$Site_Id[i])
  }
}

summary_df <- summary_df %>% 
  dplyr::rename(Soil_drainage = DRAINAGE, 
                Soil_texture = Texture_Su, 
                Soil_description = PlainEngli)


# Perform join
L1VA_Date_Coloss <- L1VA_Date_Coloss %>%
  left_join(summary_df, by = "Beekeeper_number")

write.csv(L1VA_Date_Coloss,"L1VA_Date_Coloss_Soil.csv")

write.csv(L1VA_Date_Coloss,"L1VA_Dateset_Final.csv")




#ANTROPOGENIC

#1_Pollution Levels 
#Presence of industrial activities
#Pollutant Release and Transfer Register 2020
setwd("/Users/rosiemangan/Desktop/MSc/Semester 2/Dissertation/Variable collection/Pollutants release 2020")

path_to_shapefile <- setwd("/Users/rosiemangan/Desktop/MSc/Semester 2/Dissertation/Variable collection/Pollutants release 2020")

# Load shapefile
pollutants <- st_read(path_to_shapefile)

# check data read correctly
if (!is.null(pollutants)) {
  print("Shapefile loaded successfully.")
  print(pollutants)
} else {
  print("Failed to load shapefile. Please check the file path and ensure all necessary files are present.")
}


#Load Beehive Data
path_to_shapefile <-setwd("/Users/rosiemangan/Desktop/MSc/Semester 2/Dissertation/1km_Buffer_Land_Use")
beehive_data <- st_read(path_to_shapefile)

# check data read correctly
if (!is.null(beehive_data)) {
  print("Shapefile loaded successfully.")
  print(beehive_data)
} else {
  print("Failed to load shapefile. Please check the file path and ensure all necessary files are present.")
}


# Check CRS of both datasets
print(st_crs(beehive_data)) #IRENET95 / Irish Transverse Mercator
print(st_crs(pollutants)) #TM65 / Irish Grid


# transform pollutants data CRS to match beehive data CRS
pollutants_transformed <- st_transform(pollutants, st_crs(beehive_data))
print(st_crs(pollutants_transformed)) #IRENET95 / Irish Transverse Mercator

# Perform the spatial join
combined_data <- st_join(beehive_data, pollutants_transformed, left = TRUE)

View(combined_data)


#filter the combined data to keep only rows where descriptio == 1
filtered_data <- combined_data %>%
  filter(descriptio == 1)

#summarize the unique values for Nearest_Pollutant
Nearest_Pollutant_values <- unique(filtered_data$Nearest_Pollutant)

#more than one value for for Nearest_Pollutant

# Function to calculate the mode (most frequent value)
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Function to summarize unique values for a given descriptio
summarize_values <- function(data, descriptio_value) {
  filtered_data <- data %>% filter(descriptio == descriptio_value)
  list(
    descriptio = descriptio_value,
    Nearest_Pollutant = calculate_mode(filtered_data$Nearest_Pollutant))
}


# get all unique descriptio values
descriptio_values <- unique(combined_data$descriptio)


#apply summarise_values function to all descriptio values
summary_list <- lapply(descriptio_values, function(x) summarize_values(combined_data, x))

# convert list to a data frame
summarypoll_df <- do.call(rbind, lapply(summary_list, as.data.frame))
View(summarypoll_df)

summarypoll_df %>% rename(Site_Id = descriptio) -> summarypoll_df
View(summarypoll_df)


summarypoll_df<- summarypoll_df %>%
  filter(!grepl("Repeat", Site_Id))

# Add the new prefix to Site_Id
summarypoll_df$Beekeeper_number <- NA

for(i in 1:nrow(summarypoll_df)) {
  site_length <- str_length(summarypoll_df$Site_Id[i])
  if(site_length == 1) {
    summarypoll_df$Beekeeper_number[i] <- paste0("NAP_00", summarypoll_df$Site_Id[i])
  } else if(site_length == 2) {
    summarypoll_df$Beekeeper_number[i] <- paste0("NAP_0", summarypoll_df$Site_Id[i])
  } else if(site_length == 3) {
    summarypoll_df$Beekeeper_number[i] <- paste0("NAP_", summarypoll_df$Site_Id[i])
  }
}

#Add proximity between each beehive site and all pollutant sites
distances <- st_distance(beehive_data, pollutants_transformed)

# Find the minimum distance for each beehive site
min_distances <- apply(distances, 1, min)

# Find the index of the nearest pollutant for each beehive site
nearest_pollutant_index <- apply(distances, 1, which.min)

# extract names of the nearest pollutant sites
nearest_pollutant_names <- pollutants$Name[nearest_pollutant_index]

# Add the nearest pollutant names and distances to the beehive_data dataframe
beehive_data$Nearest_Pollutant <- nearest_pollutant_names
beehive_data$Proximity_to_Pollutant <- min_distances


# make sure beekeeper_number present in beehive_data
beehive_data <- beehive_data %>%
  mutate(Beekeeper_number = ifelse(nchar(descriptio) == 1, paste0("NAP_00", descriptio),
                                   ifelse(nchar(descriptio) == 2, paste0("NAP_0", descriptio), paste0("NAP_", descriptio))))

# Convert beehive_data to a data frame, excluding the geometry
beehive_data_df <- as.data.frame(st_drop_geometry(beehive_data))

# Ensure each Beekeeper_number is unique
beehive_distances <- beehive_data_df %>%
  distinct(Beekeeper_number, .keep_all = TRUE) %>%
  dplyr::select(Beekeeper_number, Proximity_to_Pollutant, Nearest_Pollutant)

str(beehive_distances)
View(beehive_distances)

#remove repeats

beehive_distances<- beehive_distances %>%
  filter(!grepl("Repeat", Beekeeper_number))

# Merge distance information with L1VA_Date_Coloss
L1VA_Date_Coloss <- L1VA_Date_Coloss %>%
  left_join(beehive_distances, by = "Beekeeper_number")
View(L1VA_Date_Coloss)

write.csv(L1VA_Date_Coloss,"L1VA_Dataset_final.csv")
read.csv("L1VA_Dataset_final.csv")
colnames(L1VA_Date_Coloss)
