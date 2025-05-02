# This script was developed by Rosie Mangan and Cian White
# 
# 1 School of Computer Science and Statistics, Trinity College Dublin, Ireland.
# 2 School of Natural Sciences, Trinity College Dublin, Ireland.


# Script originated April 15, 2024
# Last modified Jan 12, 2025
# load libraries

library(sf) # sf library to read geospatial information, like maps
library(dplyr) # dplyr data manipulation library
library(stringr) #stingr manipulate string type data
library(tidyverse) #data import, tidying, & manipulation
library(sp) #spatial data


#Read data into R
One_KM_Land_Cover <- st_read("1km_Buffer_Land_Use.shp")

#Manipulate data into a usable form

#Remove unecessary columns by selecting those we want
One_KM_Land_Cover %>% select(Name, descriptio, LEVEL_1_VA, AREA) -> One_KM_Land_Cover_subset

# columns of smaller dataset
colnames(One_KM_Land_Cover_subset)

# rename poorly names colums
One_KM_Land_Cover_subset %>% rename(Lat_Long = Name, Site_Id = descriptio) -> One_KM_Land_Cover_subset
View(One_KM_Land_Cover_subset)
#habitats
habitats<-unique(One_KM_Land_Cover_subset$LEVEL_2_VA)
#habitats
habitats<-unique(One_KM_Land_Cover_subset$LEVEL_1_VA)


#One_KM_Land_Cover_sub <- One_KM_Land_Cover[1:200000,]
#One_KM_Land_Cover_sub <- One_KM_Land_Cover[One_KM_Land_Cover$descriptio == "24",]



One_KM_Land_Cover_subset %>% mutate(AREA_Recalc = as.numeric(st_area(geometry))) %>% # recalculate area to ensure the 1km boundary is respected 
  st_drop_geometry() %>%                                                             # drop geometry so i can group and sum rows
  dplyr::group_by(Site_Id, LEVEL_1_VA) %>%                                           # group by columns of interest                                 
  dplyr::summarise(                                                                  # apply functions to groups of interest
    Proportional_Area = round( (sum(AREA_Recalc, na.rm = T) / (10^6 *3.14159) ) ,4)
  ) %>% 
  tidyr::pivot_wider(names_from = LEVEL_1_VA, values_from = Proportional_Area) -> Wide_1km_LandCover # pivot data to wide format to get one row per site

View(Wide_1km_LandCover)
write.csv(Wide_1km_LandCover,"Wide_1KM_LandCover_L1_VA.csv")

#Add Long_Lat from One_KM Land_Cover to Wide_1km_LandCover
library(dplyr)

# Load the datasets
onekmdata <- read.csv("One_KM_Land_Cover_subset_L1VA.csv")
wide1kmdata <- read.csv("Wide_1KM_LandCover_L1_VA.csv")

# Inspect the data
head(onekmdata)
head(wide1kmdata)

View(onekmdata)
View(wide1kmdata)

# Prepare the subset data by selecting the first occurrence of each Site_Id
onekmdataunique <- onekmdata %>%
  group_by(Site_Id) %>%
  slice(1) %>%
  ungroup()
View(onekmdataunique)

# Merge the unique geographic data into the wide dataset
wide_1km_data <- left_join(wide1kmdata, onekmdataunique[, c("Site_Id", "Lat_Long")], by = "Site_Id")
View(wide_1km_data)

wide_1km_data <- left_join(wide_1km_data, one_km_data_unique[, c("Site_Id", "Lat_Long")], by = "Site_Id")



#save as CSV file
write.csv(wide_1km_data, "Updated_Wide_1KM_LandCover_L1_VA.csv", row.names = FALSE)



View(wide_1km_data)


wide_1km_data %>% {\(.) {replace(.,is.na(.),0)}}() -> Wide_1km_LandCover # changing NA to 0 across dataset

View(Wide_1km_LandCover)

# test whether summarise function really worked by summing each row and see if it's = 1

dim(Wide_1km_LandCover) # getting number of columns

rowSums(Wide_1km_LandCover[, 3:10]) # summing the columns of each row to see if the rows sum to ~ 1

min(rowSums(Wide_1km_LandCover[, 3:10])) # seeing if there is a low value in the row sums

# Look low value, what row is it?

Wide_1km_LandCover %>%  mutate(Row_Sum = rowSums(across(where(is.numeric)))) -> Wide_1km_LandCover # adding the rowsums as a column and sorting by the column in the viewer pane to see what site it is

# site 77 is the issue, checking whether the data manipulation calculations were right
One_KM_Land_Cover_subset %>% filter(Site_Id == 77) %>% mutate(AREA_Recalc = as.numeric(st_area(geometry))) -> Site_77

#calculations were right, maybe the data itself is an issue
sum(Site_77$AREA_Recalc) / (10^6 *3.14159)

# so site 77 is on the northern irish border and we don't have sufficient landcover data to include in the analysis, excluding site 77

Wide_1km_LandCover %>% filter(Site_Id != 77) -> Wide_1km_LandCover

min(Wide_1km_LandCover$Row_Sum)


#set the site Number
Wide_1km_LandCover$Apiary <- str_sub(Wide_1km_LandCover$Site_Id, end = 3)


for(i in 1:length(Wide_1km_LandCover$Apiary)) {
  Wide_1km_LandCover$Apiary[i] <- str_split(Wide_1km_LandCover$Site_Id, " ", n = Inf, simplify = FALSE)[[i]][1]
}

str_split(Wide_1km_LandCover$Site_Id, " ", n = Inf, simplify = FALSE)

#say whether it's a repeat or not
Wide_1km_LandCover$Repeat <- "N"
i=2
for(i in 1:length(Wide_1km_LandCover$Repeat)) {
  
  if(!is.na(str_split(Wide_1km_LandCover$Site_Id[i], " ", n = Inf, simplify = FALSE)[[1]][2]))
    Wide_1km_LandCover$Repeat[i] <- "Y"
}



i=1

Wide_1km_LandCover$Beekeeper_number <- NA

for(i in 1:length(Wide_1km_LandCover$Repeat)) {
  
  if( str_length(Wide_1km_LandCover$Apiary[i]) == 1  ) 
    Wide_1km_LandCover$Beekeeper_number[i] <- paste0("NAP_00", Wide_1km_LandCover$Apiary[i])
  
  if( str_length(Wide_1km_LandCover$Apiary[i]) == 2  ) 
    Wide_1km_LandCover$Beekeeper_number[i] <- paste0("NAP_0", Wide_1km_LandCover$Apiary[i])
  
  if( str_length(Wide_1km_LandCover$Apiary[i]) == 3  ) 
    Wide_1km_LandCover$Beekeeper_number[i] <- paste0("NAP_", Wide_1km_LandCover$Apiary[i])
}

#separate the Lat_Long column into two distinct columns

Wide_1km_LandCover <- Wide_1km_LandCover %>%
  separate(Lat_Long, into = c("Latitude", "Longitude"), sep = "\\s+", convert = TRUE)


write.csv(Wide_1km_LandCover, "Wide_1KM_LandCover_L1_VA.csv")


#Match landcover dataset up with honeybee information dataset

```{r}

Pesticides_Honey <- read.csv("NAP_pesticides_honey.csv")

View(Pesticides_Honey)

Pesticides_Honey %>% select(Beekeeper_number, Repeat, Year, NAP_number, Total_pesticides_honey_over_LOQ, Total_pesticides_honey_over_MRL) -> Pesticides_Honey_Subset


Pesticides_Pollen <- read.csv("NAP_pesticides_pollen.csv")

View(Pesticides_Pollen)
colnames(Pesticides_Pollen)

Pesticides_Pollen %>% select(Beekeeper_number, Repeat, Year, NAP_number, Total_pesticides_pollen_over_LOQ, Total_pesticides_pollen_over_MRL) -> Pesticides_Pollen_Subset

Pathogens_Workers <- read.csv("NAP_pathogens_workers.csv")

View(Pathogens_Workers)
colnames(Pathogens_Workers)

Pathogens_Workers %>% select(Beekeeper_number, Repeat, Year, NAP_number, no._pathogens_workers, any_pathogens_workers) -> Pathogens_Workers_Subset

```

Join datasets to allow modelling

```{r}

# need to create a column that is the same in all datasets to allow joining
# will create a NAP_number column in the Wide landcover dataset

Pathogen_Landcover <- left_join(Wide_1km_LandCover, Pathogens_Workers_Subset)

colnames(Pathogen_Landcover)

Pathogen_Pesticides_Pollen_Landcover <- left_join(Pathogen_Landcover, Pesticides_Pollen_Subset)

colnames(Pathogen_Pesticides_Pollen_Landcover)

Pathogen_Pesticides_Pollen_Honey_Landcover <- left_join(Pathogen_Pesticides_Pollen_Landcover, Pesticides_Honey_Subset)

colnames(Pathogen_Pesticides_Pollen_Honey_Landcover)
View (Pathogen_Pesticides_Pollen_Honey_Landcover)


Pathogen_Pesticides_Pollen_Honey_Landcover <- Pathogen_Pesticides_Pollen_Honey_Landcover %>%
  mutate(
    Landscape_total = rowSums(.[, 3:10], na.rm = TRUE)
  )

View(beedata)

write.csv(Pathogen_Pesticides_Pollen_Honey_Landcover,"Path_Pest_Pollen_Honey_Landcover_L1VA.csv")

```

