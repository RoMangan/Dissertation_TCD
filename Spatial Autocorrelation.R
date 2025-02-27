#Spatial autocorrelation Mapping Final

library(ade4)
library(adespatial)
library(adegraphics)
library(dplyr)
library(spdep)
library(patchwork)
library(sf)       # For spatial data structures and transformations
library(vegan)    # For multivariate statistics, including RDA and CCA
library(spdep)   # For spatial dependency and autocorrelation
library(ggplot2)
library(rnaturalearth)
vignette("tutorial", package = "adespatial")
library(units)
library(spdep)
library(dplyr)

setwd("/Users/rosiemangan/Desktop/MSc/Semester 2/Dissertation/R script")
bee_data <- read.csv("Path_Pest_Pollen_Honey_Landcover_L2VA.csv")
glimpse(bee_data)
View(bee_data)

# filter out rows where the repeat column is Y
bee_data <- bee_data %>%
  filter(Repeat != "Y")

# create sf object from dataframe and then transform to XY
bee_data_sf <- bee_data %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(2157)

distances <- as.matrix(dist(st_coordinates(bee_data_sf)))

min_distance_km <- round(min(distances[distances > 0] / 1000), 2)
avg_distance_km <- round(mean(distances[distances > 0]/1000), 2)
max_distance_km <- max(distances[distances > 0]/1000, 2)
print(paste("Minimum distance km:", min_distance_km))
print(paste("Average distance km: ", avg_distance_km))
print(paste("Maxiumum distance km: ", max_distance_km))



View(bee_data_sf)

#####K-Nearest Neighbors Weights###
#1_Visualization

# Filter out rows where the 'Repeat' column is 'Y'
bee_data_filtered <- bee_data %>%
  filter(Repeat != "Y")

View(bee_data_filtered)
bee_data_filtered_sf <- st_as_sf(bee_data_filtered, coords = c("Longitude", "Latitude"), crs = 2157, agr = "constant")
View(bee_data_filtered_sf)

#Spatial coordinates in 'bee_data_filtered_sf'
coordinates <- st_coordinates(bee_data_filtered_sf)

#Mapping
coordinates <- st_coordinates(bee_data_filtered_sf)

# Calculate k-Nearest Neighbors
k <- 5
knn_weights <- knn2nb(knearneigh(coordinates, k = k), sym = TRUE)


# Create a dataframe of edges for plotting
edges <- do.call(rbind, lapply(1:length(knn_weights), function(i) {
  if(length(knn_weights[[i]]) > 0) {
    data.frame(
      x = coordinates[i, 1],
      y = coordinates[i, 2],
      xend = coordinates[knn_weights[[i]], 1],
      yend = coordinates[knn_weights[[i]], 2]
    )
  }
}))

edges <- na.omit(edges)

ggplot() +
  geom_point(data = as.data.frame(coordinates), aes(x = X, y = Y), color = "black") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), color = "red", alpha = 0.5) +
  labs(title = "k-Nearest Neighbors Visualisation (k=5)") +
  theme_minimal() +
  coord_fixed()

ggplot() +
  geom_point(data = as.data.frame(coordinates), aes(x = X, y = Y), color = "black") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), color = "red", alpha = 0.5) +
  labs(
    title = "k = 5",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16), # Center and size up the title
    axis.title = element_text(size = 14), # Size up axis titles
    axis.text = element_text(size = 14) # Size up axis texts
  ) +
  coord_fixed()

k <- 3
knn_weights <- knn2nb(knearneigh(coordinates, k = k), sym = TRUE)


# Create a dataframe of edges for plotting
edges <- do.call(rbind, lapply(1:length(knn_weights), function(i) {
  if(length(knn_weights[[i]]) > 0) {
    data.frame(
      x = coordinates[i, 1],
      y = coordinates[i, 2],
      xend = coordinates[knn_weights[[i]], 1],
      yend = coordinates[knn_weights[[i]], 2]
    )
  }
}))

edges <- na.omit(edges)

ggplot() +
  geom_point(data = as.data.frame(coordinates), aes(x = X, y = Y), color = "black") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), color = "red", alpha = 0.5) +
  labs(title = "k-Nearest Neighbors Visualisation (k=3)") +
  theme_minimal() +
  coord_fixed()

ggplot() +
  geom_point(data = as.data.frame(coordinates), aes(x = X, y = Y), color = "black") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), color = "red", alpha = 0.5) +
  labs(
    title = "k = 3",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16), # Center and size up the title
    axis.title = element_text(size = 14), # Size up axis titles
    axis.text = element_text(size = 14) # Size up axis texts
  ) +
  coord_fixed()

k <- 2
knn_weights <- knn2nb(knearneigh(coordinates, k = k), sym = TRUE)


# Create a dataframe of edges for plotting
edges <- do.call(rbind, lapply(1:length(knn_weights), function(i) {
  if(length(knn_weights[[i]]) > 0) {
    data.frame(
      x = coordinates[i, 1],
      y = coordinates[i, 2],
      xend = coordinates[knn_weights[[i]], 1],
      yend = coordinates[knn_weights[[i]], 2]
    )
  }
}))

edges <- na.omit(edges)

ggplot() +
  geom_point(data = as.data.frame(coordinates), aes(x = X, y = Y), color = "black") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), color = "red", alpha = 0.5) +
  labs(title = "k-Nearest Neighbors Visualisation (k=2)") +
  theme_minimal() +
  coord_fixed()


ggplot() +
  geom_point(data = as.data.frame(coordinates), aes(x = X, y = Y), color = "black") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), color = "red", alpha = 0.5) +
  labs(
    title = "k = 2",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16), # Center and size up the title
    axis.title = element_text(size = 14), # Size up axis titles
    axis.text = element_text(size = 14) # Size up axis texts
  ) +
  coord_fixed()


k <- 1
knn_weights <- knn2nb(knearneigh(coordinates, k = k), sym = TRUE)


# Create a dataframe of edges for plotting
edges <- do.call(rbind, lapply(1:length(knn_weights), function(i) {
  if(length(knn_weights[[i]]) > 0) {
    data.frame(
      x = coordinates[i, 1],
      y = coordinates[i, 2],
      xend = coordinates[knn_weights[[i]], 1],
      yend = coordinates[knn_weights[[i]], 2]
    )
  }
}))

edges <- na.omit(edges)

ggplot() +
  geom_point(data = as.data.frame(coordinates), aes(x = X, y = Y), color = "black") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), color = "red", alpha = 0.5) +
  labs(title = "k-Nearest Neighbors Visualisation (k=1)") +
  theme_minimal() +
  coord_fixed()

ggplot() +
  geom_point(data = as.data.frame(coordinates), aes(x = X, y = Y), color = "black") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), color = "red", alpha = 0.5) +
  labs(
    title = "k = 1",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16), # Center and size up the title
    axis.title = element_text(size = 14), # Size up axis titles
    axis.text = element_text(size = 14) # Size up axis texts
  ) +
  coord_fixed()

#2_Spatial auto-correlation
# filter out rows where the 'Repeat' column is 'Y'
bee_data_filtered <- bee_data %>%
  filter(Repeat != "Y")
#spatial coordinates in 'bee_data_filtered_sf'
bee_data_filtered_sf <- st_as_sf(bee_data_filtered, coords = c("Longitude", "Latitude"), crs = 2157, agr = "constant")

#Total_pesticides_pollen_over_LOQ

#filter out rows where 'Total_pesticides_pollen_over_LOQ' is NA
complete_data <- bee_data_filtered %>% 
  filter(!is.na(Total_pesticides_pollen_over_LOQ))

#update spatial points
complete_points <- st_as_sf(complete_data, coords = c("Longitude", "Latitude"), crs = 2157, remove = FALSE)


#determine k-nearest neighbors
k <- 1  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$Total_pesticides_pollen_over_LOQ, lw_knn)

k <- 2  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$Total_pesticides_pollen_over_LOQ, lw_knn)

k <- 3  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$Total_pesticides_pollen_over_LOQ, lw_knn)


k <- 5  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$Total_pesticides_pollen_over_LOQ, lw_knn)


#Total_pesticides_pollen_over_MRL

#filter out rows where 'Total_pesticides_pollen_over_LOQ' is NA
complete_data <- bee_data_filtered %>% 
  filter(!is.na(Total_pesticides_pollen_over_MRL))

#update spatial points
complete_points <- st_as_sf(complete_data, coords = c("Longitude", "Latitude"), crs = 2157, remove = FALSE)


#determine k-nearest neighbors
k <- 1  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$Total_pesticides_pollen_over_MRL, lw_knn)

k <- 2  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$Total_pesticides_pollen_over_MRL, lw_knn)

k <- 3  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$Total_pesticides_pollen_over_MRL, lw_knn)


k <- 5  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$Total_pesticides_pollen_over_MRL, lw_knn)


############################
#Total_pesticides_honey_over_LOQ

#filter out rows where 'Total_pesticides_pollen_over_LOQ' is NA
complete_data <- bee_data_filtered %>% 
  filter(!is.na(Total_pesticides_honey_over_LOQ))

#update spatial points
complete_points <- st_as_sf(complete_data, coords = c("Longitude", "Latitude"), crs = 2157, remove = FALSE)


#determine k-nearest neighbors
k <- 1  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$Total_pesticides_honey_over_LOQ, lw_knn)

k <- 2  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$Total_pesticides_honey_over_LOQ, lw_knn)

k <- 3  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$Total_pesticides_honey_over_LOQ, lw_knn)


k <- 5  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$Total_pesticides_honey_over_LOQ, lw_knn)


#Total_pesticides_honey_over_MRL

#filter out rows where 'Total_pesticides_pollen_over_LOQ' is NA
complete_data <- bee_data_filtered %>% 
  filter(!is.na(Total_pesticides_honey_over_MRL))

#update spatial points
complete_points <- st_as_sf(complete_data, coords = c("Longitude", "Latitude"), crs = 2157, remove = FALSE)


#determine k-nearest neighbors
k <- 1  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$Total_pesticides_honey_over_MRL, lw_knn)

k <- 2  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$Total_pesticides_honey_over_MRL, lw_knn)

k <- 3  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$Total_pesticides_honey_over_MRL, lw_knn)


k <- 5  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$Total_pesticides_honey_over_MRL, lw_knn)

#No pathogens

#filter out rows where 'No of pathogens' is NA
complete_data <- bee_data_filtered %>% 
  filter(!is.na(no._pathogens_workers))

#update spatial points
complete_points <- st_as_sf(complete_data, coords = c("Longitude", "Latitude"), crs = 2157, remove = FALSE)


#determine k-nearest neighbors
k <- 1  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$no._pathogens_workers, lw_knn)

k <- 2  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$no._pathogens_workers, lw_knn)

k <- 3  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$no._pathogens_workers, lw_knn)


k <- 5  
knn_weights <- knn2nb(knearneigh(st_coordinates(complete_points), k = k), sym = TRUE)

# Convert to spatial weights
lw_knn <- nb2listw(knn_weights, style = "W", zero.policy = TRUE)

# Moran's I test to check spatial autocorrelation
moran.test(complete_data$no._pathogens_workers, lw_knn)


#####Linear distances#####

#1_Visualization
bee_data %>% st_as_sf(coords = c("Latitude", "Longitude"), crs = 4326) %>% st_transform(2157) -> bee_data_sf # Transform CRS to Irish Transverse Mercator

# Define neighbors for different thresholds
nbnear0 <- dnearneigh(st_coordinates(bee_data_sf), 0, 1000)
nbnear1 <- dnearneigh(st_coordinates(bee_data_sf), 0, 5000)
nbnear2 <- dnearneigh(st_coordinates(bee_data_sf), 0, 10000)
nbnear3 <- dnearneigh(st_coordinates(bee_data_sf), 0, 20000)
nbnear4 <- dnearneigh(st_coordinates(bee_data_sf), 0, 50000)
nbnear5 <- dnearneigh(st_geometry(bee_data_sf), 0, 100000)

lw0 <- nb2listw(nbnear0, style = "W", zero.policy = TRUE)
lw1 <- nb2listw(nbnear1, style = "W", zero.policy = TRUE)
lw2 <- nb2listw(nbnear2, style = "W", zero.policy = TRUE)
lw3 <- nb2listw(nbnear3, style = "W", zero.policy = TRUE)
lw4 <- nb2listw(nbnear4, style = "W", zero.policy = TRUE)
lw5 <- nb2listw(nbnear5, style = "W", zero.policy = TRUE)


bee_data_sf <- st_as_sf(bee_data, coords = c("Longitude", "Latitude"), crs = 2157)
coordinates_XY = st_coordinates(bee_data_sf)
str(bee_data_sf)

#Graphing visualisation of Linear distances
#10km

i=5
# Create a dataframe of edges for plotting
edges <- do.call(rbind, lapply(1:length(nbnear2), function(i) {
  if(sum(nbnear2[[i]]) > 0) {
    data.frame(
      x = coordinates_XY[i, 1],
      y = coordinates_XY[i, 2],
      xend = coordinates_XY[nbnear2[[i]], 1],
      yend = coordinates_XY[nbnear2[[i]], 2]
    )
  }
}))

edges <- na.omit(edges)

ggplot() +
  geom_point(data = as.data.frame(coordinates_XY), aes(x = X, y = Y), color = "black") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), color = "red", alpha = 0.5) +
  labs(title = "10km distance visualisation") +
  theme_minimal() +
  coord_fixed() +
  ylim(51.5, 55) + 
  theme(plot.title = element_text(size = 20),# Increase title size
        axis.title = element_text(size = 16),  # Increase axis title size
        axis.text = element_text(size = 14)    # Increase axis number size
  )

ggplot() +
  geom_point(data = as.data.frame(coordinates_XY), aes(x = X, y = Y), color = "black") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), color = "red", alpha = 0.5) +
  labs(
    title = "10km",
    x = "Longitude",  # Adding X axis label
    y = "Latitude"    # Adding Y axis label
  ) +
  theme_minimal() +
  coord_fixed() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16), # Center and increase title size
    axis.title = element_text(size = 14), # Increase axis title size
    axis.text = element_text(size = 14) # Increase axis text size
  )


#20km

i=5
# Create a dataframe of edges for plotting
edges <- do.call(rbind, lapply(1:length(nbnear3), function(i) {
  if(sum(nbnear3[[i]]) > 0) {
    data.frame(
      x = coordinates_XY[i, 1],
      y = coordinates_XY[i, 2],
      xend = coordinates_XY[nbnear3[[i]], 1],
      yend = coordinates_XY[nbnear3[[i]], 2]
    )
  }
}))

edges <- na.omit(edges)

ggplot() +
  geom_point(data = as.data.frame(coordinates_XY), aes(x = X, y = Y), color = "black") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), color = "red", alpha = 0.5) +
  labs(title = "20km distance visualisation") +
  theme_minimal() +
  coord_fixed() +
  ylim(51.5, 55) + 
  theme(plot.title = element_text(size = 20),# Increase title size
        axis.title = element_text(size = 16),  # Increase axis title size
        axis.text = element_text(size = 14)    # Increase axis number size
  )

ggplot() +
  geom_point(data = as.data.frame(coordinates_XY), aes(x = X, y = Y), color = "black") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), color = "red", alpha = 0.5) +
  labs(
    title = "20km",
    x = "Longitude",  # Adding X axis label
    y = "Latitude"    # Adding Y axis label
  ) +
  theme_minimal() +
  coord_fixed() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16), # Center and increase title size
    axis.title = element_text(size = 14), # Increase axis title size
    axis.text = element_text(size = 14) # Increase axis text size
  )


#50km

i=5
# Create a dataframe of edges for plotting
edges <- do.call(rbind, lapply(1:length(nbnear4), function(i) {
  if(sum(nbnear4[[i]]) > 0) {
    data.frame(
      x = coordinates_XY[i, 1],
      y = coordinates_XY[i, 2],
      xend = coordinates_XY[nbnear4[[i]], 1],
      yend = coordinates_XY[nbnear4[[i]], 2]
    )
  }
}))

edges <- na.omit(edges)

ggplot() +
  geom_point(data = as.data.frame(coordinates_XY), aes(x = X, y = Y), color = "black") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), color = "red", alpha = 0.5) +
  labs(title = "50km distance visualisation") +
  theme_minimal() +
  coord_fixed() +
  ylim(51.5, 55) + 
  theme(plot.title = element_text(size = 20),# Increase title size
        axis.title = element_text(size = 16),  # Increase axis title size
        axis.text = element_text(size = 14)    # Increase axis number size
  )

ggplot() +
  geom_point(data = as.data.frame(coordinates_XY), aes(x = X, y = Y), color = "black") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), color = "red", alpha = 0.5) +
  labs(
    title = "50km",
    x = "Longitude",  # Adding X axis label
    y = "Latitude"    # Adding Y axis label
  ) +
  theme_minimal() +
  coord_fixed() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16), # Center and increase title size
    axis.title = element_text(size = 14), # Increase axis title size
    axis.text = element_text(size = 14) # Increase axis text size
  )

#100km

i=5
# Create a dataframe of edges for plotting
edges <- do.call(rbind, lapply(1:length(nbnear5), function(i) {
  if(sum(nbnear5[[i]]) > 0) {
    data.frame(
      x = coordinates_XY[i, 1],
      y = coordinates_XY[i, 2],
      xend = coordinates_XY[nbnear5[[i]], 1],
      yend = coordinates_XY[nbnear5[[i]], 2]
    )
  }
}))

edges <- na.omit(edges)

ggplot() +
  geom_point(data = as.data.frame(coordinates_XY), aes(x = X, y = Y), color = "black") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), color = "red", alpha = 0.5) +
  labs(title = "100km distance visualisation") +
  theme_minimal() +
  coord_fixed() +
  ylim(51.5, 55) + 
  theme(plot.title = element_text(size = 20),# Increase title size
        axis.title = element_text(size = 16),  # Increase axis title size
        axis.text = element_text(size = 14)    # Increase axis number size
  )

ggplot() +
  geom_point(data = as.data.frame(coordinates_XY), aes(x = X, y = Y), color = "black") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), color = "red", alpha = 0.5) +
  labs(
    title = "100km",
    x = "Longitude",  # Adding X axis label
    y = "Latitude"    # Adding Y axis label
  ) +
  theme_minimal() +
  coord_fixed() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16), # Center and increase title size
    axis.title = element_text(size = 14), # Increase axis title size
    axis.text = element_text(size = 14) # Increase axis text size
  )


#2_Spatial auto-correlation

#Total_pesticides_pollen_over_LOQ
complete_data <- bee_data_sf %>% 
  filter(!is.na(Total_pesticides_pollen_over_LOQ)) 

# Define neighbors for different thresholds
nbnear0 <- dnearneigh(st_geometry(complete_data), 0, 1000)
nbnear1 <- dnearneigh(st_coordinates(complete_data), 0, 5000)
nbnear2 <- dnearneigh(st_coordinates(complete_data), 0, 10000)
nbnear3 <- dnearneigh(st_coordinates(complete_data), 0, 20000)
nbnear4 <- dnearneigh(st_coordinates(complete_data), 0, 50000)
nbnear5 <- dnearneigh(st_geometry(complete_data), 0, 100000)

lw1km <- nb2listw(nbnear0, style = "W", zero.policy = TRUE)
lw5km <- nb2listw(nbnear1, style = "W", zero.policy = TRUE)
lw10km <- nb2listw(nbnear2, style = "W", zero.policy = TRUE)
lw20km <- nb2listw(nbnear3, style = "W", zero.policy = TRUE)
lw50km <- nb2listw(nbnear4, style = "W", zero.policy = TRUE)
lw100km <- nb2listw(nbnear5, style = "W", zero.policy = TRUE)



# Moran's I test
#10km
moran_test_result <- moran.test(complete_data$Total_pesticides_pollen_over_LOQ, lw10km, zero.policy = TRUE)
print(moran_test_result)
#20km
moran_test_result <- moran.test(complete_data$Total_pesticides_pollen_over_LOQ, lw20km, zero.policy = TRUE)
print(moran_test_result)
#50km
moran_test_result <- moran.test(complete_data$Total_pesticides_pollen_over_LOQ, lw50km, zero.policy = TRUE)
print(moran_test_result)
#100km
moran_test_result <- moran.test(complete_data$Total_pesticides_pollen_over_LOQ, lw100km, zero.policy = TRUE)
print(moran_test_result)


#Total_pesticides_pollen_over_MRL
complete_data <- bee_data_sf %>% 
  filter(!is.na(Total_pesticides_pollen_over_MRL)) 

# Define neighbors for different thresholds
nbnear0 <- dnearneigh(st_geometry(complete_data), 0, 1000)
nbnear1 <- dnearneigh(st_coordinates(complete_data), 0, 5000)
nbnear2 <- dnearneigh(st_coordinates(complete_data), 0, 10000)
nbnear3 <- dnearneigh(st_coordinates(complete_data), 0, 20000)
nbnear4 <- dnearneigh(st_coordinates(complete_data), 0, 50000)
nbnear5 <- dnearneigh(st_geometry(complete_data), 0, 100000)

lw1km <- nb2listw(nbnear0, style = "W", zero.policy = TRUE)
lw5km <- nb2listw(nbnear1, style = "W", zero.policy = TRUE)
lw10km <- nb2listw(nbnear2, style = "W", zero.policy = TRUE)
lw20km <- nb2listw(nbnear3, style = "W", zero.policy = TRUE)
lw50km <- nb2listw(nbnear4, style = "W", zero.policy = TRUE)
lw100km <- nb2listw(nbnear5, style = "W", zero.policy = TRUE)



# Moran's I test
#10km
moran_test_result <- moran.test(complete_data$Total_pesticides_pollen_over_MRL, lw10km, zero.policy = TRUE)
print(moran_test_result)
#20km
moran_test_result <- moran.test(complete_data$Total_pesticides_pollen_over_MRL, lw20km, zero.policy = TRUE)
print(moran_test_result)
#50km
moran_test_result <- moran.test(complete_data$Total_pesticides_pollen_over_MRL, lw50km, zero.policy = TRUE)
print(moran_test_result)
#100km
moran_test_result <- moran.test(complete_data$Total_pesticides_pollen_over_MRL, lw100km, zero.policy = TRUE)
print(moran_test_result)


#Total_pesticides_honey_over_LOQ
complete_data <- bee_data_sf %>% 
  filter(!is.na(Total_pesticides_honey_over_LOQ)) 

# Define neighbors for different thresholds
nbnear0 <- dnearneigh(st_geometry(complete_data), 0, 1000)
nbnear1 <- dnearneigh(st_coordinates(complete_data), 0, 5000)
nbnear2 <- dnearneigh(st_coordinates(complete_data), 0, 10000)
nbnear3 <- dnearneigh(st_coordinates(complete_data), 0, 20000)
nbnear4 <- dnearneigh(st_coordinates(complete_data), 0, 50000)
nbnear5 <- dnearneigh(st_geometry(complete_data), 0, 100000)

lw1km <- nb2listw(nbnear0, style = "W", zero.policy = TRUE)
lw5km <- nb2listw(nbnear1, style = "W", zero.policy = TRUE)
lw10km <- nb2listw(nbnear2, style = "W", zero.policy = TRUE)
lw20km <- nb2listw(nbnear3, style = "W", zero.policy = TRUE)
lw50km <- nb2listw(nbnear4, style = "W", zero.policy = TRUE)
lw100km <- nb2listw(nbnear5, style = "W", zero.policy = TRUE)



# Moran's I test
#10km
moran_test_result <- moran.test(complete_data$Total_pesticides_honey_over_LOQ, lw10km, zero.policy = TRUE)
print(moran_test_result)
#20km
moran_test_result <- moran.test(complete_data$Total_pesticides_honey_over_LOQ, lw20km, zero.policy = TRUE)
print(moran_test_result)
#50km
moran_test_result <- moran.test(complete_data$Total_pesticides_honey_over_LOQ, lw50km, zero.policy = TRUE)
print(moran_test_result)
#100km
moran_test_result <- moran.test(complete_data$Total_pesticides_honey_over_LOQ, lw100km, zero.policy = TRUE)
print(moran_test_result)


#Total_pesticides_honey_over_MRL
complete_data <- bee_data_sf %>% 
  filter(!is.na(Total_pesticides_honey_over_MRL)) 

# Define neighbors for different thresholds
nbnear0 <- dnearneigh(st_geometry(complete_data), 0, 1000)
nbnear1 <- dnearneigh(st_coordinates(complete_data), 0, 5000)
nbnear2 <- dnearneigh(st_coordinates(complete_data), 0, 10000)
nbnear3 <- dnearneigh(st_coordinates(complete_data), 0, 20000)
nbnear4 <- dnearneigh(st_coordinates(complete_data), 0, 50000)
nbnear5 <- dnearneigh(st_geometry(complete_data), 0, 100000)

lw1km <- nb2listw(nbnear0, style = "W", zero.policy = TRUE)
lw5km <- nb2listw(nbnear1, style = "W", zero.policy = TRUE)
lw10km <- nb2listw(nbnear2, style = "W", zero.policy = TRUE)
lw20km <- nb2listw(nbnear3, style = "W", zero.policy = TRUE)
lw50km <- nb2listw(nbnear4, style = "W", zero.policy = TRUE)
lw100km <- nb2listw(nbnear5, style = "W", zero.policy = TRUE)



# Moran's I test
#10km
moran_test_result <- moran.test(complete_data$Total_pesticides_honey_over_MRL, lw10km, zero.policy = TRUE)
print(moran_test_result)
#20km
moran_test_result <- moran.test(complete_data$Total_pesticides_honey_over_MRL, lw20km, zero.policy = TRUE)
print(moran_test_result)
#50km
moran_test_result <- moran.test(complete_data$Total_pesticides_honey_over_MRL, lw50km, zero.policy = TRUE)
print(moran_test_result)
#100km
moran_test_result <- moran.test(complete_data$Total_pesticides_honey_over_MRL, lw100km, zero.policy = TRUE)
print(moran_test_result)

#Pathogens
complete_data <- bee_data_sf %>% 
  filter(!is.na(no._pathogens_workers))

# Define neighbors for different thresholds
nbnear0 <- dnearneigh(st_geometry(complete_data), 0, 1000)
nbnear1 <- dnearneigh(st_coordinates(complete_data), 0, 5000)
nbnear2 <- dnearneigh(st_coordinates(complete_data), 0, 10000)
nbnear3 <- dnearneigh(st_coordinates(complete_data), 0, 20000)
nbnear4 <- dnearneigh(st_coordinates(complete_data), 0, 50000)
nbnear5 <- dnearneigh(st_geometry(complete_data), 0, 100000)

lw1km <- nb2listw(nbnear0, style = "W", zero.policy = TRUE)
lw5km <- nb2listw(nbnear1, style = "W", zero.policy = TRUE)
lw10km <- nb2listw(nbnear2, style = "W", zero.policy = TRUE)
lw20km <- nb2listw(nbnear3, style = "W", zero.policy = TRUE)
lw50km <- nb2listw(nbnear4, style = "W", zero.policy = TRUE)
lw100km <- nb2listw(nbnear5, style = "W", zero.policy = TRUE)



# Moran's I test
#10km
moran_test_result <- moran.test(complete_data$no._pathogens_workers, lw10km, zero.policy = TRUE)
print(moran_test_result)
#20km
moran_test_result <- moran.test(complete_data$no._pathogens_workers, lw20km, zero.policy = TRUE)
print(moran_test_result)
#50km
moran_test_result <- moran.test(complete_data$no._pathogens_workers, lw50km, zero.policy = TRUE)
print(moran_test_result)
#100km
moran_test_result <- moran.test(complete_data$no._pathogens_workers, lw100km, zero.policy = TRUE)
print(moran_test_result)

 

