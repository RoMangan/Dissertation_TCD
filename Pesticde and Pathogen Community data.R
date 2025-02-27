#Pesticide and Pathogen Community data

#Load libraries
library(tidyr)
library(dplyr)
library(stringr)
library(geosphere)
library(vegan)
library(betapart)
library(ggplot2)
library(gridExtra)


setwd("~/Desktop/MSc/Semester 2/Dissertation/Goggle doc/Data/Datasets for analyses")
list.files()

#Housekeeping to produce binary pesticide and pathogen CSVs from orginal data
################POLLEN
pollen_data <-read.csv("NAP_pesticides_pollen.csv")
pollen_data <- pollen_data %>%
  filter(Repeat != "Y")

colnames(pollen_data)
#inspect pesticides in pollen
gc_loq <- pollen_data$GC_pesticides_over_pollen_LOQ
unique(gc_loq)
gc_mrl <- pollen_data$GC_pesticides_pollen_over_MRL
unique(gc_mrl)
lc_loq <- pollen_data$LC_pesticides_pollen_over_LOQ
unique(lc_loq)
lc_mrl <- pollen_data$LC_pesticides_pollen_over_MRL
unique(lc_mrl)

# Extract and return unique pesticides from a specific column
extract_unique_pesticides <- function(data, column) {
  # Pull the specific column, ignoring NA and "N" 
  entries <- data[[column]]
  entries <- entries[!is.na(entries) & entries != "N"]
  
# Split each entry by commas and unlist to a single vector
pesticide_list <- unlist(strsplit(entries, ",\\s*"))
  
# Clean up entries: trim whitespace and convert to consistent case (e.g., all lowercase)
pesticide_list <- tolower(trimws(pesticide_list))
  
# Return unique pesticides
unique(pesticide_list)
}

columns_of_interest <- c("GC_pesticides_over_pollen_LOQ", "GC_pesticides_pollen_over_MRL", 
                         "LC_pesticides_pollen_over_LOQ", "LC_pesticides_pollen_over_MRL")

# Extract unique pesticides from each column and combine them
all_unique_pesticides_pollen <- unique(unlist(lapply(columns_of_interest, function(col) {
  extract_unique_pesticides(pollen_data, col)
})))

# Print all unique pesticides
print(all_unique_pesticides_pollen)

unique_pesticides_pollen <- c("tau-fluvalinate", "propargite", "coumaphos", "boscalid", "fludioxonil",
                       "azoxystrobin", "pendimethalin", "trifluralin", "cyprodinil", "myclobutanil",
                       "pyraclostrobin", "prothioconazole", "prosulfocarb", "pyrimethanil", "mcpa",
                       "quizalofop", "2_4_d")


# Initialize a dataframe to store binary indicators
binary_data_pollen <- data.frame(matrix(0, nrow = nrow(pollen_data), ncol = length(unique_pesticides_pollen)))
colnames(binary_data_pollen) <- unique_pesticides_pollen
binary_data_pollen$NAP_number <- pollen_data$NAP_number

# Populate the binary data
for (i in 1:nrow(pollen_data)) {
  # Combine and clean relevant columns
  combined_entries <- tolower(paste(pollen_data[i, columns_of_interest], collapse = ","))
  combined_entries <- gsub("[^a-z0-9,\\s-]", "", combined_entries)  #clean special characters
  
  #split into individual pesticides
  pesticides_in_row <- tolower(trimws(unlist(strsplit(combined_entries, ",\\s*"))))
  
  # Convert to lowercase and split into individual pesticides
  pesticides_in_row <- tolower(trimws(unlist(strsplit(combined_entries, ",\\s*"))))
  
  # Match against the known list of unique pesticides
  pesticides_in_row <- tolower(trimws(unlist(strsplit(combined_entries, ",\\s*"))))
  # Populate binary matrix for each pesticide
  for (pesticide in unique_pesticides_pollen) {
    if (pesticide %in% pesticides_in_row) {
      binary_data_pollen[i, pesticide] <- 1
    }
  }
}

# Add total pesticides column
binary_data_pollen$Total_pesticides <- rowSums(binary_data_pollen[, unique_pesticides_pollen])

# check problematic pesticide-> tau-fluvalinate
for (i in 1:nrow(pollen_data)) {
  combined_entries <- tolower(paste(pollen_data[i, columns_of_interest], collapse = " "))
  combined_entries <- gsub("[^a-z0-9,\\s-]", "", combined_entries)  # clean special characters
  if (grepl("\\btau-fluvalinate\\b", combined_entries)) {
    print(paste("Row:", i, "contains tau-fluvalinate:", combined_entries))
    binary_data_pollen[i, "tau-fluvalinate"] <- 1
  }
}
# Verify binary data for specific NAP_numbers
specific_rows <- c("NAP_029_2021", "NAP_040_2021", "NAP_013_2020", "NAP_137_2021")
print(binary_data_pollen[binary_data_pollen$NAP_number %in% specific_rows, ])


filtered_NAPs <- c("NAP_029_2021", "NAP_040_2021", "NAP_013_2020", "NAP_037_2021", 
                   "NAP_006_2021", "NAP_006_2020", "NAP_032_2021", "NAP_052_2021", 
                   "NAP_075_2021", "NAP_013_2021", "NAP_137_2021", "NAP_030_2021", 
                   "NAP_034_2021", "NAP_036_2021", "NAP_038_2021", "NAP_039_2021", 
                   "NAP_046_2021", "NAP_063_2021", "NAP_065_2021", "NAP_066_2021", 
                   "NAP_073_2021", "NAP_076_2021", "NAP_096_2021", "NAP_097_2021", 
                   "NAP_098_2021", "NAP_102_2021", "NAP_107_2021", "NAP_123_2021", 
                   "NAP_124_2021", "NAP_132_2021", "NAP_135_2021", "NAP_149_2021", 
                   "NAP_150_2021", "NAP_158_2021")

# Print the filtered rows
filtered_NAP_check<-print(binary_data_pollen[binary_data_pollen$NAP_number %in% filtered_NAPs, ])

#Pollen binary dataframe
head(binary_data_pollen)
View(binary_data_pollen)


#Check it aligns with NAP_pesticides_pollen.csv
#Dataset
NAP_029_2021 <- pollen_data %>% 
  filter(NAP_number == "NAP_029_2021")

lc_loq_nap_029 <- NAP_029_2021$LC_pesticides_pollen_over_LOQ
print(lc_loq_nap_029)

# [1] "Boscalid, Cyprodinil, Fludioxonil, Myclobutanil, Pyraclostrobin"

#New binary data
# Filter for NAP_029_2021
NAP_029_2021_data <- filter(binary_data_pollen, NAP_number == "NAP_029_2021")

# If the beehive exists in the dataset, proceed to extract pesticide names
if(nrow(NAP_029_2021_data) > 0) {
  # Extracting names of pesticides where the value is 1 (detected)
  detected_pesticides <- names(NAP_029_2021_data[, -which(names(NAP_029_2021_data) == "NAP_number")])[NAP_029_2021_data[-which(names(NAP_029_2021_data) == "NAP_number")] == 1]
  
  # Print the list of detected pesticides
  print(detected_pesticides)
} else {
  cat("No data available for Beekeeper_number 'NAP_029'\n")
}
# "boscalid"       "fludioxonil"    "cyprodinil"     "myclobutanil"   "pyraclostrobin"

NAP_040_2021 <- pollen_data %>% 
  filter(NAP_number == "NAP_040_2021")

gc_loq_nap_040 <- NAP_040_2021$LC_pesticides_pollen_over_LOQ
print(gc_loq_nap_040)
#[1] "Azoxystrobin, Boscalid, Fludioxonil, Pyrimethanil"


#New binary data
# Filter for NAP_029_2021
NAP_040_2021_data <- filter(binary_data_pollen, NAP_number == "NAP_040_2021")

# If the beehive exists in the dataset, proceed to extract pesticide names
if(nrow(NAP_040_2021_data) > 0) {
  # Extracting names of pesticides where the value is 1 (detected)
  detected_pesticides <- names(NAP_040_2021_data[, -which(names(NAP_040_2021_data) == "NAP_number")])[NAP_040_2021_data[-which(names(NAP_040_2021_data) == "NAP_number")] == 1]
  
  # Print the list of detected pesticides
  print(detected_pesticides)
} else {
  cat("No data available for Beekeeper_number 'NAP_029'\n")
}
#"boscalid"     "fludioxonil"  "azoxystrobin" "pyrimethanil"



###########HONEY
honey_data<-read.csv("NAP_pesticides_honey.csv")
honey_data <- honey_data %>%
  filter(Repeat != "Y")


#check pesticides in honey
gc_loq <- honey_data$GC_pesticides_honey_over_LOQ
unique(gc_loq)
gc_mrl <- honey_data$GC_pesticides_honey_over_MRL
unique(gc_mrl)
lc_loq <- honey_data$LC_pesticides_honey_over_LOQ
unique(lc_loq)
lc_mrl <- honey_data$LC_pesticides_honey_over_MRL
unique(lc_mrl)


# Extract and return unique pesticides from a specific column
extract_unique_pesticides <- function(data, column) {
  # Pull the specific column, ignoring NA and "N" 
  entries <- data[[column]]
  entries <- entries[!is.na(entries) & entries != "N"]
  
  # Split each entry by commas and unlist to a single vector
  pesticide_list <- unlist(strsplit(entries, ",\\s*"))
  
  # Clean up entries: trim whitespace and convert to consistent case (e.g., all lowercase)
  pesticide_list <- tolower(trimws(pesticide_list))
  
  # Return unique pesticides
  unique(pesticide_list)
}


columns_of_interest <- c("GC_pesticides_honey_over_LOQ", "GC_pesticides_honey_over_MRL", 
                         "LC_pesticides_honey_over_LOQ", "LC_pesticides_honey_over_MRL")

# Extract unique pesticides from each column and combine them
all_unique_pesticides_honey <- unique(unlist(lapply(columns_of_interest, function(col) {
  extract_unique_pesticides(honey_data, col)
})))

# Print all unique pesticides
print(all_unique_pesticides_honey)

unique_pesticides_honey<-c("propargite", "azoxystrobin", "boscalid", "antraquinone", "ddac", "mcpa") 


# Define columns of interest
columns_of_interest <- c("GC_pesticides_honey_over_LOQ", "GC_pesticides_honey_over_MRL", 
                         "LC_pesticides_honey_over_LOQ", "LC_pesticides_honey_over_MRL")

# Initialize a dataframe for binary indicators
binary_data_honey <- data.frame(matrix(0, nrow = nrow(honey_data), ncol = length(all_unique_pesticides_honey)))
names(binary_data_honey) <- all_unique_pesticides_honey
binary_data_honey$NAP_number <- honey_data$NAP_number

#populate the binary data
for (i in 1:nrow(honey_data)) {
  # Combine all relevant pesticide entries into one string
  combined_entries <- paste(honey_data[i, columns_of_interest], collapse = ", ")
  combined_entries <- tolower(combined_entries)  # Convert to lowercase
  combined_entries <- gsub("[^a-z0-9,\\s]", "", combined_entries)  # Remove special characters
  
# log combined entries to check
cat("Combined Entries for Row", i, ":", combined_entries, "\n")
  
#iterate over each pesticide to check presence
  for (pesticide in all_unique_pesticides_honey) {
    # Check for the presence of the pesticide using word boundaries
    if (grepl(paste0("\\b", pesticide, "\\b"), combined_entries)) {
      binary_data_honey[i, pesticide] <- 1
    }
  }
}

# Check the populated binary data
head(binary_data_honey)
View(binary_data_honey)

#Check it aligns with NAP_pesticides_honey.csv
#Dataset
NAP_004_2020 <- honey_data %>% 
  filter(NAP_number == "NAP_004_2020")

gc_loq_nap_004 <- NAP_004_2020$GC_pesticides_honey_over_LOQ
print(gc_loq_nap_004)
# [1] "Propargite"

#New binary data
# Filter for NAP_004_2020
NAP_004_2020_data <- filter(binary_data_honey, NAP_number== "NAP_004_2020")

# If the beehive exists in the dataset, proceed to extract pesticide names
if(nrow(NAP_004_2020_data) > 0) {
  # Extracting names of pesticides where the value is 1 (detected)
  detected_pesticides <- names(NAP_004_2020_data[, -which(names(NAP_004_2020_data) == "NAP_number")])[NAP_004_2020_data[-which(names(NAP_004_2020_data) == "NAP_number")] == 1]
  
  # Print the list of detected pesticides
  print(detected_pesticides)
} else {
  cat("No data available for Beekeeper_ID 'NAP_004'\n")
}
# "propargite"



###############PATHOGENS

pathogens_data<-read.csv("NAP_pathogens_workers.csv")
colnames(pathogens_data)
pathogens_data <- pathogens_data %>%
  filter(Repeat != "Y")
pathogen_columns <- c("Varroa_workers", "Acarapis_woodi_workers", "Nosema_apis_workers", 
                      "Nosema_ceranae_workers", "Crithidia_mellificae_workers", 
                      "Lotmaria_passim_workers", "DWV_workers", "BQCV_workers", 
                      "SBV_workers", "IAPV_workers", "ABPV_workers", "CBPV_workers", 
                      "KBV_workers", "Paenibacillus_larvae_workers", "Melissococcus_plutonius_workers", 
                      "Ascosphaera_apis_workers")

# Create a binary DataFrame
binary_data_pathogens <- pathogens_data %>%
  mutate(across(all_of(pathogen_columns), ~ as.integer(tolower(.) == "positive"), .names = "binary_{col}")) %>%
  dplyr::select(NAP_number, starts_with("binary_"))



# Check the structure and head of the binary data frame
str(binary_data_pathogens)
head(binary_data_pathogens)
View(binary_data_pathogens)


#add landcover and spatial data to each dataframe

setwd("~/Desktop/MSc/Semester 2/Dissertation/R script")
list.files()
comp_bee_data <- read.csv("L1VA_Dateset_Final.csv")
colnames(comp_bee_data)
comp_bee_data <- comp_bee_data %>%
  filter(Repeat != "Y")

selected_land_cover <- comp_bee_data %>%
  dplyr::select(1:9, 11:14)


combined_binary_data_pollen <- binary_data_pollen %>%
  inner_join(selected_land_cover, by = "NAP_number")

combined_binary_data_honey <- binary_data_honey %>%
  inner_join(selected_land_cover, by = "NAP_number")

combined_binary_data_pathogens<- binary_data_pathogens %>%
  inner_join(selected_land_cover, by = "NAP_number")

#write to CSV for analysis
write.csv(combined_binary_data_pollen, "combined_binary_data_pollen.csv", row.names = FALSE)

#write to CSV if needed for analysis
write.csv(combined_binary_data_honey, "combined_binary_data_honey.csv", row.names = FALSE)

# write to CSV if needed for analysis
write.csv(combined_binary_data_pathogens, "combined_binary_data_pathogens.csv", row.names = FALSE)



#########Beta diversity of pesticides, pathogens, and land cover
install.packages(c("vegan", "betapart", "adespatial"))
library(vegan)
library(betapart)
library(adespatial)

#Pesticides in pollen
setwd("~/Desktop/MSc/Semester 2/Dissertation/R script")
pollen<-read.csv("combined_binary_data_pollen.csv")
colnames(pollen)

####CONSTRUCT PESTICIDE (POLLEN) MATRIX

pesticide_pollen_columns <- c("tau.fluvalinate", "propargite", "coumaphos", "boscalid",
                       "fludioxonil", "azoxystrobin", "pendimethalin", "trifluralin",
                       "cyprodinil", "myclobutanil", "pyraclostrobin", "prothioconazole",
                       "prosulfocarb", "pyrimethanil", "mcpa", "quizalofop", "X2_4_d")

# create the pesticide matrix
pesticide_pollen_matrix <- pollen %>%
  dplyr::select(all_of(pesticide_pollen_columns)) %>%
  as.matrix()

# set the row names to NAP_number
rownames(pesticide_pollen_matrix) <- pollen$NAP_number

print(pesticide_pollen_matrix)
pesticide_pollen_matrix <- pesticide_pollen_matrix[rowSums(pesticide_pollen_matrix) > 0, ] #remove rows/columns with all zeros: cause issues for beta diversity
print(pesticide_pollen_matrix)

#Pesticides in honey
setwd("~/Desktop/MSc/Semester 2/Dissertation/R script")
honey<-read.csv("combined_binary_data_honey.csv")
colnames(honey)

####CONSTRUCT PESTICIDE (HONEY) MATRIX
pesticide_honey_columns <- c("propargite", "azoxystrobin", "boscalid", "antraquinone", "ddac", "mcpa")

# create the pesticide matrix
pesticide_honey_matrix <- honey %>%
  dplyr::select(all_of(pesticide_honey_columns)) %>%
  as.matrix()

# set the row names to NAP_number
rownames(pesticide_honey_matrix) <- honey$NAP_number
print(pesticide_honey_matrix)
pesticide_honey_matrix <- pesticide_honey_matrix[rowSums(pesticide_honey_matrix) > 0, ] #remove rows/columns with all zeros: they can cause issues for beta diversity
print(pesticide_honey_matrix)



#Pathogens
pathogens<-read.csv("combined_binary_data_pathogens.csv")
colnames(pathogens)

pathogens <- pathogens %>%
  filter(Repeat != "Y")



####CONSTRUCT PATHOGEN MATRIX
pathogens_columns <- c("binary_Varroa_workers", "binary_Acarapis_woodi_workers", "binary_Nosema_apis_workers", "binary_Nosema_ceranae_workers", "binary_Crithidia_mellificae_workers", "binary_Lotmaria_passim_workers", "binary_DWV_workers", "binary_BQCV_workers", "binary_SBV_workers", "binary_IAPV_workers", "binary_ABPV_workers", "binary_CBPV_workers", "binary_KBV_workers", "binary_Paenibacillus_larvae_workers", "binary_Melissococcus_plutonius_workers", "binary_Ascosphaera_apis_workers")

# create the pathogen matrix
pathogen_matrix <- pathogens %>%
  dplyr::select(all_of(pathogens_columns)) %>%
  as.matrix()

# set the row names to NAP_number
rownames(pathogen_matrix) <- pathogens$NAP_number
print(pathogen_matrix)
pathogen_matrix <- pathogen_matrix[rowSums(pathogen_matrix) > 0, ] #remove rows or columns with all zeros as they can cause issues for beta diversity
pathogen_matrix <- pathogen_matrix[complete.cases(pathogen_matrix), ] #exclude rows that contain any NA values

print(pathogen_matrix)


####CONSTRUCT LANDCOVER MATRIX
setwd("~/Desktop/MSc/Semester 2/Dissertation/R script")
list.files()
comp_bee_data <- read.csv("L1VA_Dateset_Final.csv")
colnames(comp_bee_data)
comp_bee_data <- comp_bee_data %>%
  filter(Repeat != "Y")

selected_land_cover <- comp_bee_data %>%
  dplyr::select(1:9, 11:14)

colnames(selected_land_cover)
land_cover_columns <- c("ARTIFICIAL.SURFACES", "GRASSLAND..SALTMARSH.and.SWAMP", "HEATH.and.BRACKEN", "WATERBODIES", "CULTIVATED.LAND", "EXPOSED.SURFACES", "PEATLAND", "FOREST..WOODLAND.AND.SCRUB")

#Add column for other so it sums to 1
selected_land_cover$OTHER <- 1 - rowSums(selected_land_cover[, land_cover_columns])

#updated land cover columns
land_cover_columns <- c("ARTIFICIAL.SURFACES", "GRASSLAND..SALTMARSH.and.SWAMP", "HEATH.and.BRACKEN", "WATERBODIES", "CULTIVATED.LAND", "EXPOSED.SURFACES", "PEATLAND", "FOREST..WOODLAND.AND.SCRUB", "OTHER")

# create the landcover matrix
landcover_matrix <- selected_land_cover %>%
  dplyr::select(all_of(land_cover_columns)) %>%
  as.matrix()

# set the row names to NAP_number
rownames(landcover_matrix) <- selected_land_cover$NAP_number
print(landcover_matrix)


# convert matrix to a data frame
landcover_df <- as.data.frame(landcover_matrix)
                                

####CONSTRUCT SPATIAL MATRIX
colnames(selected_land_cover)
coords <- selected_land_cover %>%
  dplyr::select(NAP_number, Latitude, Longitude)

spatial_matrix <- distm(coords[, c("Longitude", "Latitude")], fun = distHaversine)
rownames(spatial_matrix) <- coords$NAP_number
colnames(spatial_matrix) <- coords$NAP_number

View(spatial_matrix)
write.csv(spatial_matrix, "spatial_matrix.csv", row.names = FALSE)

# Find the shortest and longest distances to see if they align with spatial autocrrelatiuon findings
shortest_distance <- min(spatial_matrix[spatial_matrix > 0])  # exclude zero (self-distance)
longest_distance <- max(spatial_matrix)

# Print results
print(paste("Shortest Distance:", shortest_distance, "meters"))
print(paste("Longest Distance:", longest_distance, "meters"))
#Shortest Distance: 454.203332342958 meters
#Longest Distance: 439448.730293319 meters

#Check if this aligns with Spatial autocorrelation findings
#Minimum distance km: 0.45
#Maxiumum distance km:  439.46



########BETA DIVERSITY########

#LANDCOVER
#Calculate Beta Diversity (Bray-Curtis Dissimilarity)
landcover_beta <- vegdist(landcover_matrix, method = "bray")
print(landcover_beta)
landcover_beta_matrix <- as.matrix(landcover_beta)


write.csv(as.matrix(landcover_beta_matrix), "landcover_beta.csv")

spatial_dist <- as.dist(spatial_matrix)
landcover_dist <- as.dist(landcover_beta_matrix)
print(length(landcover_dist))
print(length(spatial_dist))

mantel_test_landcover <- mantel(landcover_dist, spatial_dist, permutations = 999)
print(mantel_test_landcover)


#POLLEN
pollen_pesticide_beta <- beta.pair(pesticide_pollen_matrix, index.family = "sorensen")
print(pollen_pesticide_beta)
print(pollen_pesticide_beta$beta.sim) # turnover
print(pollen_pesticide_beta$beta.sne) # nestedness
print(pollen_pesticide_beta$beta.sor) # total beta diversity

pollen_pesticide_beta_matrix <- as.matrix(pollen_pesticide_beta$beta.sor)



# convert matrix to a data frame
pollen_pesticide_beta_df <- as.data.frame(pollen_pesticide_beta_matrix)


sum(is.na(pollen_pesticide_beta_matrix))

write.csv(as.matrix(pollen_pesticide_beta_matrix), "pollen_beta.csv")


#mantel test for spatial relationships
pollen_dist <- as.dist(pollen_pesticide_beta$beta.sor)
pollen_dist <- as.dist(pollen_pesticide_beta_matrix)
spatial_dist <- as.dist(spatial_matrix)

length(pollen_dist)
length(spatial_dist)

nrow(pollen_pesticide_beta_matrix)
nrow(spatial_matrix)

common_samples <- intersect(rownames(pollen_pesticide_beta_matrix), rownames(spatial_matrix))

# Subset matrices to contain only common samples
pollen_pesticide_beta_matrix_aligned <- pollen_pesticide_beta_matrix[common_samples, common_samples]
spatial_matrix_aligned <- spatial_matrix[common_samples, common_samples]

pollen_dist <- as.dist(pollen_pesticide_beta_matrix_aligned)
spatial_dist <- as.dist(spatial_matrix_aligned)



length(pollen_dist)
length(spatial_dist)

mantel_test_pollen <- mantel(pollen_dist, spatial_dist, permutations = 999)
print(mantel_test_pollen)

#test whether colonies in more similar landscapes have similar pesticide profiles
mantel(landcover_beta, pollen_dist, permutations = 999)
dim(landcover_beta)
dim(pollen_dist)

#convert landcover matrix to a proper matrix
landcover_beta_matrix <- as.matrix(landcover_beta)

#find common samples between two datasets
common_samples <- intersect(rownames(landcover_beta_matrix), rownames(pollen_pesticide_beta_matrix))

# align both matrices to only include common samples
landcover_beta_aligned <- landcover_beta_matrix[common_samples, common_samples]
pollen_pesticide_beta_aligned <- pollen_pesticide_beta_matrix[common_samples, common_samples]

#convert aligned matrices back to dist objects for the Mantel test
landcover_dist_aligned <- as.dist(landcover_beta_aligned)
pollen_dist_aligned <- as.dist(pollen_pesticide_beta_aligned)

mantel_test_pollen_landcover <- mantel(landcover_dist_aligned, pollen_dist_aligned, permutations = 999)
print(mantel_test_pollen_landcover)


#HONEY
honey_pesticide_beta <- beta.pair(pesticide_honey_matrix, index.family = "sorensen")
print(honey_pesticide_beta)
print(honey_pesticide_beta$beta.sim) # turnover
print(honey_pesticide_beta$beta.sne) # nestedness
print(honey_pesticide_beta$beta.sor) # total beta diversity

honey_pesticide_beta_matrix <- as.matrix(honey_pesticide_beta$beta.sor)

# convert matrix to a data frame
honey_pesticide_beta_df <- as.data.frame(honey_pesticide_beta_matrix)
sum(is.na(honey_pesticide_beta_matrix))

write.csv(as.matrix(honey_pesticide_beta_matrix), "honey_beta.csv")

#mantel test for spatial relationships
spatial_dist <- as.dist(spatial_matrix)
length(spatial_dist)
length(honey_pesticide_beta_matrix)

dim(honey_pesticide_beta_matrix)
dim(spatial_matrix)


common_samples <- intersect(rownames(honey_pesticide_beta_matrix), rownames(spatial_matrix))

honey_pesticide_beta_matrix_aligned <- honey_pesticide_beta_matrix[common_samples, common_samples]
spatial_matrix_aligned <- spatial_matrix[common_samples, common_samples]

honey_dist <- as.dist(honey_pesticide_beta_matrix_aligned)
spatial_dist <- as.dist(spatial_matrix_aligned)

length(honey_dist)
length(spatial_dist)

mantel_test_honey <- mantel(honey_dist, spatial_dist, permutations = 999)
print(mantel_test_honey)

#mantel test for similar landscapes: similar pesticide profiles
mantel(landcover_beta, honey_dist, permutations = 999)
print(class(landcover_beta)) #should be 'dist'
print(class(honey_pesticide_beta_matrix))
# convert 'landcover_beta' to a matrix 
landcover_beta_matrix <- as.matrix(landcover_beta)
common_samples <- intersect(rownames(landcover_beta_matrix), rownames(honey_pesticide_beta_matrix))

landcover_beta_aligned <- landcover_beta_matrix[common_samples, common_samples]
honey_beta_aligned <- honey_pesticide_beta_matrix[common_samples, common_samples]


# Convert to distance objects
landcover_dist_aligned <- as.dist(landcover_beta_aligned)
honey_dist_aligned <- as.dist(honey_beta_aligned)

# Run Mantel test again
mantel_test_honey_landcover <- mantel(landcover_dist_aligned, honey_dist_aligned, permutations = 999)
print(mantel_test_honey_landcover)




#PATHOGEN
pathogen_beta <- beta.pair(pathogen_matrix, index.family = "sorensen")
print(pathogen_beta)

print(pathogen_beta$beta.sim) # turnover
print(pathogen_beta$beta.sne) # nestedness
print(pathogen_beta$beta.sor) # total beta diversity


pathogen_matrix <- as.matrix(pathogen_beta$beta.sor) 

# convert matrix to a data frame
pathogen_beta_df <- as.data.frame(pathogen_matrix)
write.csv(as.matrix(pathogen_matrix), "pathogen_beta.csv")

sum(is.na(pathogen_matrix))

#mantel test for spatial relationships
pathogen_dist <- as.dist(pathogen_matrix)
pathogen_dist <- as.dist(pathogen_beta$beta.sor)
spatial_dist <- as.dist(spatial_matrix)
mantel_test <- mantel(pathogen_dist, spatial_dist)

print(length(pathogen_dist))
print(length(spatial_dist))


common_samples <- intersect(rownames(pathogen_matrix), rownames(spatial_matrix))

# Subset the original data to include only common samples
pathogen_aligned <- pathogen_matrix[common_samples, common_samples]
spatial_aligned <- spatial_matrix[common_samples, common_samples]

# Convert the spatial aligned matrix to a dist object if needed for further analysis
spatial_dist_aligned <- as.dist(spatial_aligned)
pathogen_dist_aligned <- as.dist(pathogen_aligned)


mantel_test_result <- mantel(pathogen_dist_aligned, spatial_dist_aligned, permutations = 999)
print(mantel_test_result)

#test whether colonies in more similar landscapes have similar pathogen profiles
mantel(landcover_beta, pathogen_dist, permutations = 999)

print(class(landcover_beta))
print(class(pathogen_dist))

# Check dimensions by converting to full matrices temporarily
print(dim(as.matrix(landcover_beta)))
print(dim(as.matrix(pathogen_dist)))


common_samples <- intersect(rownames(as.matrix(landcover_beta)), rownames(as.matrix(pathogen_dist)))

landcover_matrix <- as.matrix(landcover_beta)
pathogen_matrix <- as.matrix(pathogen_dist)

# Subset based on common samples
landcover_aligned <- landcover_matrix[common_samples, common_samples]
pathogen_aligned <- pathogen_matrix[common_samples, common_samples]


# Check dimensions are now equal
print(dim(landcover_aligned))  
print(dim(pathogen_aligned))

# Run the Mantel test
mantel_test_aligned <- mantel(landcover_aligned, pathogen_aligned, permutations = 999)
print(mantel_test_aligned)


#Z-Score calculation for visualising beta diversity

#####Pesticides in pollen 
# Generate null distributions for beta diversity (Sorensen)
set.seed(42)  # for reproducibility
n_permutations <- 999

# Generate null distribution of beta diversity
null_distributions_pollen <- replicate(n_permutations, {
  shuffled_matrix <- apply(pesticide_pollen_matrix, 2, sample)
  beta_null <- beta.pair(shuffled_matrix, index.family = "sorensen")$beta.sim
  as.vector(as.dist(beta_null))
})

# Calculate empirical beta diversity
beta_empirical_pollen <- as.vector(as.dist(pollen_pesticide_beta$beta.sim))

# Calculate Z-scores
beta_mean_null <- apply(null_distributions_pollen, 1, mean)
beta_sd_null <- apply(null_distributions_pollen, 1, sd)
pollen_pesticide_z_scores <- (beta_empirical_pollen - beta_mean_null) / beta_sd_null



# Create data frames for each group with spatial distance, landcover beta, and beta diversity

dim(spatial_matrix)         
dim(as.matrix(landcover_beta))
dim(as.matrix(pollen_pesticide_beta$beta.sor))


pollen_data <- data.frame(
  group = "pollen_pesticide",
  spatial_distance = as.vector(as.dist(spatial_matrix)), # Convert spatial matrix to vector
  landcover_beta = as.vector(as.dist(landcover_beta)),  # Convert landcover beta to vector
  beta_diversity = as.vector(as.dist(pollen_pesticide_beta$beta.sor)) # Total beta diversity
)

dim(spatial_matrix)         
dim(as.matrix(landcover_beta))
dim(as.matrix(honey_pesticide_beta$beta.sor))

honey_data <- data.frame(
  group = "honey_pesticide",
  spatial_distance = as.vector(as.dist(spatial_matrix)),
  landcover_beta = as.vector(as.dist(landcover_beta)),
  beta_diversity = as.vector(as.dist(honey_pesticide_beta$beta.sor))
)


dim(spatial_matrix)  # Should match the dimensions of other matrices
dim(as.matrix(landcover_beta))  # Should be the same as spatial_matrix
dim(as.matrix(pathogen_beta$beta.sor))

rownames(spatial_matrix)
rownames(as.matrix(landcover_beta))
rownames(as.matrix(pathogen_beta$beta.sor))

setdiff(rownames(spatial_matrix), rownames(as.matrix(pathogen_beta$beta.sor)))
setdiff(rownames(spatial_matrix), rownames(as.matrix(landcover_beta)))

common_rows <- intersect(intersect(rownames(spatial_matrix), rownames(as.matrix(landcover_beta))), rownames(as.matrix(pathogen_beta$beta.sor)))

spatial_matrix <- spatial_matrix[common_rows, common_rows]
landcover_beta <- as.matrix(landcover_beta)[common_rows, common_rows]
pathogen_beta_sor <- as.matrix(pathogen_beta$beta.sor)[common_rows, common_rows]

pathogen_data <- data.frame(
  group = "pathogen",
  spatial_distance = as.vector(as.dist(spatial_matrix)),
  landcover_beta = as.vector(as.dist(landcover_beta)),
  beta_diversity = as.vector(as.dist(pathogen_beta_sor))
)

# Combine all groups into a single dataframe
combined_data <- rbind(pollen_data, honey_data, pathogen_data)
colnames(combined_data)
colnames(combined_data)[colnames(combined_data) == "spatial_distance"] <- "spatial_distance (km)"
combined_data$spatial_distance (km)` <- combined_data$spatial_distance (km)` / 1000
combined_data$group <- recode(
  combined_data$group,
  "pollen_pesticide" = "pesticide(pollen)",
  "honey_pesticide" = "pesticide(honey)"
)



#spatial distance vs beta diversity
ggplot(combined_data, aes(x = `spatial_distance (km)`, y = beta_diversity, color = group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(group = group), color= "black") + # add regression line
  facet_wrap(~ group, scales = "free") +   # separate panels for each group
  theme_minimal() +
  labs(
    title = "Spatial distance vs beta diversity",
    x = "Spatial distance (km)",
    y = "Beta diversity (Sorensen)"
  )


#environmental dissimilarity vs beta diversity
ggplot(combined_data, aes(x = landcover_beta, y = beta_diversity, color = group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(group = group), color= "black") + # Add regression lines
  facet_wrap(~ group, scales = "free") +   # Separate panels for each group
  theme_minimal() +
  labs(
    title = "Environmental dissimilarity vs beta diversity",
    x = "Environmental dissimilarity (Landcover Beta)",
    y = "Beta diversity (Sorensen)"
  )






# Relationship between spatial distance and environmental dissimilarity with  beta diversity

# Compute empirical Beta diversity (β_empirical)
pollen_pesticide_beta_sor <- as.vector(as.dist(pollen_pesticide_beta$beta.sor))
honey_pesticide_beta_sor <- as.vector(as.dist(honey_pesticide_beta$beta.sor))
pathogen_beta_sor <- as.vector(as.dist(pathogen_beta$beta.sor))

# Generate randomized resampled data (β_resampled)
set.seed(123)  # Ensure reproducibility
num_permutations <- 1000  # Number of randomizations

resample_beta_values <- function(matrix_data, method = "sorensen") {
  # Ensure binary data and remove columns with all zeros
  matrix_data <- ifelse(matrix_data > 0, 1, 0)
  matrix_data <- matrix_data[, colSums(matrix_data) > 0, drop = FALSE]
  
  if (ncol(matrix_data) == 0) stop("All columns removed; check input data.")
  
  resampled_betas <- numeric(num_permutations)
  
  for (i in 1:num_permutations) {
    shuffled_matrix <- matrix_data[sample(seq_len(nrow(matrix_data)), replace = TRUE), , drop = FALSE]
    beta_result <- beta.pair(shuffled_matrix, index.family = method)$beta.sor
    resampled_betas[i] <- mean(as.dist(beta_result))
  }
  
  return(resampled_betas)
}


# Re-sample beta diversity for each dataset
#pollen
resampled_pollen_beta <- resample_beta_values(pesticide_pollen_matrix)
summary(resampled_pollen_beta)
sd(resampled_pollen_beta)

#honey
resampled_honey_beta <- resample_beta_values(pesticide_honey_matrix)
summary(resampled_honey_beta)
sd(resampled_honey_beta)

#pathogen
resampled_pathogen_beta <- resample_beta_values(pathogen_matrix)
summary(resampled_pathogen_beta)
sd(resampled_pathogen_beta)

# Calculate mean and standard deviation of resampled data
mean_pollen_resampled <- mean(resampled_pollen_beta)
sd_pollen_resampled <- sd(resampled_pollen_beta)

mean_honey_resampled <- mean(resampled_honey_beta)
sd_honey_resampled <- sd(resampled_honey_beta)

mean_pathogen_resampled <- mean(resampled_pathogen_beta)
sd_pathogen_resampled <- sd(resampled_pathogen_beta)

# Calculate Z-scores
z_scores_pollen <- (pollen_pesticide_beta_sor - mean_pollen_resampled) / sd_pollen_resampled
z_scores_honey <- (honey_pesticide_beta_sor - mean_honey_resampled) / sd_honey_resampled
z_scores_pathogen <- (pathogen_beta_sor - mean_pathogen_resampled) / sd_pathogen_resampled


#prepare individual dfs

pollen_data <- data.frame(
  spatial_distance_km = as.vector(as.dist(spatial_matrix))[1:length(z_scores_pollen)] / 1000,
  environmental_dissimilarity = as.vector(as.dist(landcover_beta))[1:length(z_scores_pollen)],
  z_score = z_scores_pollen
)
colnames(pollen_data)
pollen_data$environmental_dissimilarity

pollen_data$environmental_dissimilarity <- pollen_data$environmental_dissimilarity * 100

honey_data <- data.frame(
  spatial_distance_km = as.vector(as.dist(spatial_matrix))[1:length(z_scores_honey)] / 1000,
  environmental_dissimilarity = as.vector(as.dist(landcover_beta))[1:length(z_scores_honey)],
  z_score = z_scores_honey
)

honey_data$environmental_dissimilarity <- honey_data$environmental_dissimilarity * 100

pathogen_data <- data.frame(
  spatial_distance_km = as.vector(as.dist(spatial_matrix))[1:length(z_scores_pathogen)] / 1000,
  environmental_dissimilarity = as.vector(as.dist(landcover_beta))[1:length(z_scores_pathogen)],
  z_score = z_scores_pathogen
)

pathogen_data$environmental_dissimilarity <- pathogen_data$environmental_dissimilarity * 100


# Spatial distance plots
spatial_pollen_plot <- ggplot(pollen_data, aes(x = spatial_distance_km, y = z_score)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  xlim(0, 400) +
  ylim(-30, 10)

spatial_honey_plot <- ggplot(honey_data, aes(x = spatial_distance_km, y = z_score)) +
  geom_point(color = "green", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  xlim(0, 400) +
  ylim(-30, 10)

spatial_pathogen_plot <- ggplot(pathogen_data, aes(x = spatial_distance_km, y = z_score)) +
  geom_point(color = "purple", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) 

# Environmental dissimilarity plots
es_pollen_plot <- ggplot(pollen_data, aes(x = environmental_dissimilarity, y = z_score)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  xlim(0, 100) +
  ylim(-30, 20)

es_honey_plot <- ggplot(honey_data, aes(x = environmental_dissimilarity, y = z_score)) +
  geom_point(color = "green", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() + theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  xlim(0, 100) +
  ylim(-30, 20)

es_pathogen_plot <- ggplot(pathogen_data, aes(x = environmental_dissimilarity, y = z_score)) +
  geom_point(color = "purple", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() + theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  xlim(0, 100) +
  ylim(-30, 20)

# Arrange the plots in a 2x3 grid
grid.arrange(
  spatial_pollen_plot, spatial_honey_plot, spatial_pathogen_plot,
  es_pollen_plot, es_honey_plot, es_pathogen_plot,
  ncol = 3
)



grid.arrange(
  spatial_pollen_plot + theme(plot.margin = margin(5, 10, 5, 10)),
  spatial_honey_plot + theme(plot.margin = margin(5, 10, 5, 10)),
  spatial_pathogen_plot + theme(plot.margin = margin(5, 10, 5, 10)),
  es_pollen_plot + theme(plot.margin = margin(5, 10, 5, 10)),
  es_honey_plot + theme(plot.margin = margin(5, 10, 5, 10)),
  es_pathogen_plot + theme(plot.margin = margin(5, 10, 5, 10)),
  ncol = 3
)

