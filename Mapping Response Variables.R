#Mapping of Response Variables: 
#1)Total pesticides pollen (over LOQ)
#2) Total pesticides pollen (over MRL) 
#3)Total pesticides honey (over LOQ)
#4) Total pesticides honey (over MRL)
#5) No. of pathogen workers

# load libraries
library(readr)
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
library(maps)
library(viridis)
library(ggplot2)
library(RColorBrewer)
library(scales)


## Import data ####
# read data in frame and examine it
setwd("/Users/rosiemangan/Desktop/MSc/Semester 2/Dissertation/R script")
bee_df <- read.csv("Path_Pest_Pollen_Honey_Landcover_L1VA.csv")

# filter out rows where the 'Repeat' column is 'Y'
bee_df <- bee_df %>%
  filter(Repeat != "Y")

# convert factors or characters to numeric
bee_df$Latitude <- as.numeric(as.character(bee_df$Latitude))
bee_df$Longitude <- as.numeric(as.character(bee_df$Longitude))

# convert dataframe to an sf object
bee_data_sf <- st_as_sf(bee_df, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)


#check and transform coordinate reference system (CRS)
# transform CRS to Irish Transverse Mercator
st_crs(bee_data_sf) 
bee_data_sf <- st_transform(bee_data_sf, 2157)

#import provinces shapefile for mapping
ireland_map <- st_read("Provinces")
ireland_map <- st_transform(ireland_map, 2157)

#Beehive locations

ggplot() +
  geom_sf(data = ireland_map, fill = "springgreen3", color = "white") +  #map of Ireland
  geom_sf(data = bee_data_sf, color = "black", size = 1, shape = 20, show.legend = FALSE) +  #beehives locations
  labs(title = "National Apiculture Programme", subtitle = "Beehive Locations in Ireland", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  
    plot.subtitle = element_text(hjust = 0.5),  
    plot.caption = element_text(hjust = 0.5)  
  )

#Total_pesticides_pollen_over_LOQ
# filter out NA values 
bee_data_sf <- bee_data_sf[!is.na(bee_data_sf$Total_pesticides_pollen_over_LOQ), ]
num_levels <- length(unique(bee_data_sf$Total_pesticides_pollen_over_LOQ))
unique_values <- unique(bee_data_sf$Total_pesticides_pollen_over_LOQ)
unique_values

# Define specific colors for each value
value_colors <- c("0" = "black", 
                  "1" = "blue",      
                  "2" = "hotpink",     
                  "3" = "red",     
                  "4" = "orange",   
                  "5" = "yellow") 

ggplot() +
  geom_sf(data = ireland_map, fill = "springgreen3", color = "white") + 
  geom_sf(data = bee_data_sf, aes(color = as.factor(Total_pesticides_pollen_over_LOQ)), size = 1, shape = 20) +
  scale_color_manual(values = value_colors) +
  labs(title = "Total Pesticides in Pollen",
       subtitle = "(Over LOQ)",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 0.5),  
    legend.background = element_rect(fill = "lightgrey", color = "darkgrey"),
    legend.text = element_text(color = "black"),  
    legend.title = element_text(color = "black")  
  ) +
  guides(color = guide_legend(title = NULL))

#Total_pesticides_pollen_over_MRL
# filter out NA values 
bee_data_sf <- bee_data_sf[!is.na(bee_data_sf$Total_pesticides_pollen_over_MRL), ]
num_levels <- length(unique(bee_data_sf$Total_pesticides_pollen_over_MRL))
unique_values <- unique(bee_data_sf$Total_pesticides_pollen_over_MRL)
unique_values

# Define specific colors for each value
value_colors <- c("0" = "black", 
                  "1" = "blue",      
                  "2" = "hotpink",     
                  "3" = "red",     
                  "4" = "orange",   
                  "5" = "yellow") 

ggplot() +
  geom_sf(data = ireland_map, fill = "springgreen3", color = "white") + 
  geom_sf(data = bee_data_sf, aes(color = as.factor(Total_pesticides_pollen_over_MRL)), size = 1, shape = 20) +
  scale_color_manual(values = value_colors) +
  labs(title = "Total Pesticides in Pollen",
       subtitle = "(Over MRL)",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 0.5),  
    legend.background = element_rect(fill = "lightgrey", color = "darkgrey"),
    legend.text = element_text(color = "black"),  
    legend.title = element_text(color = "black")  
  ) +
  guides(color = guide_legend(title = NULL))




#Total_pesticides_honey_over_LOQ
# filter out NA values 
bee_data_sf <- bee_data_sf[!is.na(bee_data_sf$Total_pesticides_honey_over_LOQ), ]
num_levels <- length(unique(bee_data_sf$Total_pesticides_honey_over_LOQ))
unique_values <- unique(bee_data_sf$Total_pesticides_honey_over_LOQ)
unique_values


# Define the colors for each level
colors <- c('0' = 'black', '1' = 'yellow')

ggplot() +
  geom_sf(data = ireland_map, fill = "springgreen3", color = "white") +
  #plot points where Total_pesticides_honey_over_MRL is "0" first
  geom_sf(data = bee_data_sf %>% 
            filter(as.factor(Total_pesticides_honey_over_LOQ) == "0"),
          aes(color = as.factor(Total_pesticides_honey_over_LOQ)), 
          size = 1, shape = 20) +
  #then plot points where Total_pesticides_honey_over_LOQ is "1"
  geom_sf(data = bee_data_sf %>% 
            filter(as.factor(Total_pesticides_honey_over_LOQ) == "1"),
          aes(color = as.factor(Total_pesticides_honey_over_LOQ)), 
          size = 1, shape = 20) +
  scale_color_manual(values = colors, 
                     labels = c("0" = "0", "1" = "1"),  
                     name = "") +  #remove legend title
  labs(title = "Total Pesticides in Honey",
       subtitle = "(Over LOQ)",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  #center main title
    plot.subtitle = element_text(hjust = 0.5),  #center subtitle
    plot.caption = element_text(hjust = 0.5),  #center caption
    legend.position = "right",
    legend.background = element_rect(fill = "grey", colour = "darkgrey"),  # grey legend background
    legend.text = element_text(color = "black"),  
    legend.title = element_text(color = "black", size = 0)  #no legend title
  ) +
  guides(color = guide_legend(title = NULL))  #no legend title


#Total_pesticides_honey_over_MRL
# filter out NA values 
bee_data_sf <- bee_data_sf[!is.na(bee_data_sf$Total_pesticides_honey_over_MRL), ]
num_levels <- length(unique(bee_data_sf$Total_pesticides_honey_over_MRL))
unique_values <- unique(bee_data_sf$Total_pesticides_honey_over_MRL)
unique_values

# Define the colors for each level
colors <- c('0' = 'black', '1' = 'yellow')

ggplot() +
  geom_sf(data = ireland_map, fill = "springgreen3", color = "white") +
  # Plot points where Total_pesticides_honey_over_MRL is "0" first
  geom_sf(data = bee_data_sf %>% 
            filter(as.factor(Total_pesticides_honey_over_MRL) == "0"),
          aes(color = as.factor(Total_pesticides_honey_over_MRL)), 
          size = 1, shape = 20) +
  # Then plot points where Total_pesticides_honey_over_MRL is "1"
  geom_sf(data = bee_data_sf %>% 
            filter(as.factor(Total_pesticides_honey_over_MRL) == "1"),
          aes(color = as.factor(Total_pesticides_honey_over_MRL)), 
          size = 1, shape = 20) +
  scale_color_manual(values = colors, 
                     labels = c("0" = "0", "1" = "1"),
                     name = "") +  #remove legend title
  labs(title = "Total Pesticides in Honey",
       subtitle = "(Over MRL)",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  
    plot.subtitle = element_text(hjust = 0.5),  
    plot.caption = element_text(hjust = 0.5),  
    legend.position = "right",
    legend.background = element_rect(fill = "grey", colour = "darkgrey"),  #grey legend background  
    legend.text = element_text(color = "black"),  
    legend.title = element_text(color = "black", size = 0)  # no visible title for the legend
  ) +
  guides(color = guide_legend(title = NULL))  


#Total_no_of_pathogens
# filter out NA values 
bee_data_sf <- bee_data_sf[!is.na(bee_data_sf$no._pathogens_workers), ]
num_levels <- length(unique(bee_data_sf$no._pathogens_workers))
unique_values <- unique(bee_data_sf$no._pathogens_workers)
unique_values


# Define specific colors for each value
value_colors <- c("0" = "black", 
                  "1" = "blue",      
                  "2" = "hotpink",     
                  "3" = "red",     
                  "4" = "orange",   
                  "5" = "lightyellow",
                  "6" = "yellow") 

ggplot() +
  geom_sf(data = ireland_map, fill = "springgreen3", color = "white") + 
  geom_sf(data = bee_data_sf, aes(color = as.factor(no._pathogens_workers)), size = 1, shape = 20) +
  scale_color_manual(values = value_colors) +
  labs(title = "No. of pathogens workers",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 0.5),  
    legend.background = element_rect(fill = "lightgrey", color = "darkgrey"),
    legend.text = element_text(color = "black"),  
    legend.title = element_text(color = "black")  
  ) +
  guides(color = guide_legend(title = NULL))

