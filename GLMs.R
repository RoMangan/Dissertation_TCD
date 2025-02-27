#GLM beedata

# Load required libraries
library(compositions) # for compositional data analysis
library(MASS)  # for stepAIC
library(dplyr)  # for data manipulation
library(glmmTMB) #for GLMMs
library(pscl) #zero-inflated and hurdle models 
library(dplyr) # data manipulation
library(glm2) #GLMs
library(gridExtra)
library(sf)
library(units) 


setwd("/Users/rosiemangan/Desktop/MSc/Semester 2/Dissertation/R script")
list.files()

#Level1VA
bee_data <- read.csv("L1VA_Dateset_Final.csv")

# Filter out rows where the 'Repeat' column is 'Y'
bee_data <- bee_data %>%
  filter(Repeat != "Y")

View(bee_data)
colnames(bee_data)


#otal_pesticides_pollen_over_LOQ
# calculate the number of zeros in the dependent variable, ignoring NAs
num_zeros <- sum(bee_data$Total_pesticides_pollen_over_LOQ == 0, na.rm = TRUE)
total_observations <- nrow(bee_data)
percent_zeros <- (num_zeros / total_observations) * 100
# print results
print(paste("Number of zeros:", num_zeros))
print(paste("Total observations:", total_observations))
print(paste("Percentage of zeros:", round(percent_zeros, 2), "%"))

# count NA values in the dependent variable
num_nas <- sum(is.na(bee_data$Total_pesticides_pollen_over_LOQ))
print(paste("Number of NAs:", num_nas))

#Total_pesticides_pollen_over_MRL
# calculate the number of zeros in the dependent variable, ignoring NAs
num_zeros <- sum(bee_data$Total_pesticides_pollen_over_MRL == 0, na.rm = TRUE)
total_observations <- nrow(bee_data)
percent_zeros <- (num_zeros / total_observations) * 100
#print results
print(paste("Number of zeros:", num_zeros))
print(paste("Total observations:", total_observations))
print(paste("Percentage of zeros:", round(percent_zeros, 2), "%"))
# count NA values in the dependent variable
num_nas <- sum(is.na(bee_data$Total_pesticides_pollen_over_MRL))
print(paste("Number of NAs:", num_nas))

#Total_pesticides_honey_over_LOQ
# calculate the number of zeros in the dependent variable, ignoring NAs
num_zeros <- sum(bee_data$Total_pesticides_honey_over_LOQ == 0, na.rm = TRUE)
total_observations <- nrow(bee_data)
percent_zeros <- (num_zeros / total_observations) * 100
#print results
print(paste("Number of zeros:", num_zeros))
print(paste("Total observations:", total_observations))
print(paste("Percentage of zeros:", round(percent_zeros, 2), "%"))
# count NA values in the dependent variable
num_nas <- sum(is.na(bee_data$Total_pesticides_honey_over_LOQ))
print(paste("Number of NAs:", num_nas))

#Total_pesticides_honey_over_MRL
# calculate the number of zeros in the dependent variable, ignoring NAs
num_zeros <- sum(bee_data$Total_pesticides_honey_over_MRL == 0, na.rm = TRUE)
total_observations <- nrow(bee_data)
percent_zeros <- (num_zeros / total_observations) * 100
#print results
print(paste("Number of zeros:", num_zeros))
print(paste("Total observations:", total_observations))
print(paste("Percentage of zeros:", round(percent_zeros, 2), "%"))
# count NA values in the dependent variable
num_nas <- sum(is.na(bee_data$Total_pesticides_honey_over_MRL))
print(paste("Number of NAs:", num_nas))

#no._pathogens_workers
# calculate the number of zeros in the dependent variable, ignoring NAs
num_zeros <- sum(bee_data$no._pathogens_workers == 0, na.rm = TRUE)
total_observations <- nrow(bee_data)
percent_zeros <- (num_zeros / total_observations) * 100
#print results
print(paste("Number of zeros:", num_zeros))
print(paste("Total observations:", total_observations))
print(paste("Percentage of zeros:", round(percent_zeros, 2), "%"))
# count NA values in the dependent variable
num_nas <- sum(is.na(bee_data$Total_pno._pathogens_workers))
print(paste("Number of NAs:", num_nas))



# Add county per province
bee_data <- bee_data %>%
  mutate(Province = case_when(
    County %in% c("Carlow", "Dublin", "Kildare", "Kilkenny", "Laois", "Longford", "Louth", "Meath", "Offaly", "Westmeath", "Wexford", "Wicklow") ~ "Leinster",
    County %in% c("Clare", "Cork", "Kerry", "Limerick", "Tipperary", "Waterford") ~ "Munster",
    County %in% c("Galway", "Leitrim", "Mayo", "Roscommon", "Sligo") ~ "Connacht",
    County %in% c("Derry", "Antrim", "Down", "Tyrone", "Armagh", "Fermanagh", "Cavan", "Monaghan", "Donegal") ~ "Ulster",
    TRUE ~ "Unknown"  #In case I forgot to list any county 
  ))

# Define area per province
province_area <- data.frame(
  Province = c("Connacht", "Leinster", "Munster", "Ulster"),
  Area_km2 = c(17713, 19801, 24684, 22067)
)

# Summarize the number of beehives per province
province_summary <- bee_data %>%
  group_by(Province) %>%
  summarise(
    num_beehives = n()
  ) %>%
  left_join(province_area, by = "Province") %>%
  mutate(beehive_density = num_beehives / Area_km2)

print(province_summary)

bee_data <- bee_data %>%
  left_join(province_summary %>% dplyr::select(Province, Area_km2), by = "Province")



#recalculate beehive density at a local Level
bee_data <- bee_data %>%
  group_by(Province, NAP_number) %>%  # ensures per-location density
  mutate(local_beehive_density = n() / unique(Area_km2)) %>%  # avoids multiple Area_km2 values per group
  ungroup()  # always ungroup after grouped calculations

# Convert bee_data into a spatial object
bee_data_sf <- st_as_sf(bee_data, coords = c("Longitude", "Latitude"), crs = 4326)

# Define 5 km radius for counting nearby hives
radius_m <- set_units(10000, "m")

# compute local beehive density (number of beehives within 5 km for each point)
bee_data$local_beehive_density <- sapply(1:nrow(bee_data_sf), function(i) {
  sum(st_distance(bee_data_sf[i, ], bee_data_sf) <= radius_m, na.rm = TRUE)
})



# check local beehive density has variation
summary(bee_data$local_beehive_density)


bee_data$total_pesticides = rowSums(bee_data[, c("Total_pesticides_honey_over_MRL",
                                                 "Total_pesticides_honey_over_LOQ",
                                                 "Total_pesticides_pollen_over_MRL",
                                                 "Total_pesticides_pollen_over_LOQ")], na.rm = TRUE)

bee_data$pesticides_MRL = rowSums(bee_data[, c("Total_pesticides_honey_over_MRL",
                                               "Total_pesticides_pollen_over_MRL")], na.rm = TRUE)
bee_data$pesticides_LOQ = rowSums(bee_data[, c("Total_pesticides_pollen_over_LOQ",
                                               "Total_pesticides_honey_over_LOQ")], na.rm = TRUE)
head(bee_data)

# check for zeros and replace
adjusted_bee_data <- as.data.frame(lapply(bee_data[, c("ARTIFICIAL.SURFACES", "GRASSLAND..SALTMARSH.and.SWAMP", 
                                                       "HEATH.and.BRACKEN", "WATERBODIES", "CULTIVATED.LAND", "EXPOSED.SURFACES", 
                                                       "PEATLAND", "FOREST..WOODLAND.AND.SCRUB")], function(x) ifelse(x == 0, 1e-5, x)))
View(adjusted_bee_data)
comp_data <- acomp(adjusted_bee_data)  # convert to Aitchison composition
View(comp_data)
print(colnames(comp_data))


# Apply ALR transformation using FOREST..WOODLAND.&SCRUB as denominator
alr_data <- alr(comp_data, denominator = 8) 
colnames(alr_data) <- paste("alr", colnames(alr_data), sep="_")
print(colnames(alr_data))


# Apply CLR transformation
clr_data <- clr(comp_data)
# Generating names for CLR-transformed columns, usually simply using original names with a prefix
clr_column_names <- paste0("clr_", colnames(adjusted_bee_data))

# Assign these names to the CLR data
colnames(clr_data) <- clr_column_names
print(colnames(clr_data))

# Define the landcover types
landcover_types <- c("ARTIFICIAL.SURFACES", "GRASSLAND..SALTMARSH.and.SWAMP", "HEATH.and.BRACKEN", "WATERBODIES", "CULTIVATED.LAND", "EXPOSED.SURFACES", "PEATLAND", "FOREST..WOODLAND.AND.SCRUB")


combined_bee_data <- cbind(bee_data[, !(names(bee_data) %in% landcover_types)], alr_data, clr_data)
View(combined_bee_data)

#ILR

# create partition matrix for ILR transformation (8 rows, 2 columns)
partition_matrix <- matrix(0, nrow = 8, ncol = 2, byrow = TRUE, dimnames = list(NULL, c("Human_Impacted", "Natural_Areas")))

# Human_Impacted components: 1, 5, 6 representing ARTIFICIAL.SURFACES, CULTIVATED.LAND, and EXPOSED.SURFACES. 
# Natural_Areas components: 2, 3, 4, 7, 8 representing GRASSLAND..SALTMARSH.and.SWAMP, HEATH.and.BRACKEN, WATERBODIES, PEATLAND, and FOREST..WOODLAND.AND.SCRUB.
partition_matrix[1, "Human_Impacted"] <- 1  #ARTIFICIAL.SURFACES
partition_matrix[5, "Human_Impacted"] <- 1  #CULTIVATED.LAND
partition_matrix[6, "Human_Impacted"] <- 1  #EXPOSED.SURFACES


partition_matrix[2:4, "Natural_Areas"] <- 1 #GRASSLAND..SALTMARSH.and.SWAMP, HEATH.and.BRACKEN, WATERBODIES
partition_matrix[7:8, "Natural_Areas"] <- 1 #PEATLAND, and FOREST..WOODLAND.AND.SCRUB

# Display the partition matrix
print(partition_matrix)


#apply ILR transformation
ilr_coordinates <- ilr(comp_data, V = partition_matrix)
ilr_df <- as.data.frame(ilr_coordinates)
colnames(ilr_df) <- c("Human_Impacted", "Natural_Areas")
plot(ilr_coordinates)
combined_bee_data <- cbind(combined_bee_data, ilr_df)
View(combined_bee_data)


# filter out rows where the repeat column is Y
combined_bee_data <- combined_bee_data %>%
  filter(Repeat != "Y")

#Creating a new data frame for Leinster
leinster_data <- combined_bee_data %>%
  filter(Province == "Leinster")

#Creating a new data frame for Munster
munster_data <- combined_bee_data %>%
  filter(Province == "Munster")

#GLMs
#Long-Lat influence
#Total_pesticides_pollen_over_LOQ
model<-glm(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude, family = poisson(link = "log"), data = combined_bee_data)
summary(model)

model<-glm(Total_pesticides_pollen_over_LOQ ~ Longitude * Latitude, family = poisson(link = "log"), data = combined_bee_data)
summary(model)

#Total_pesticides_pollen_over_MRL
model<-glm(Total_pesticides_pollen_over_MRL ~ Longitude + Latitude, family = poisson(link = "log"), data = combined_bee_data)
summary(model)

model<-glm(Total_pesticides_pollen_over_MRL ~ Longitude * Latitude, family = poisson(link = "log"), data = combined_bee_data)
summary(model)

#Total_pesticides_honey_over_LOQ
model<-glm(Total_pesticides_honey_over_LOQ ~ Longitude + Latitude, family = poisson(link = "log"), data = combined_bee_data)
summary(model)

model<-glm(Total_pesticides_honey_over_LOQ ~ Longitude * Latitude, family = poisson(link = "log"), data = combined_bee_data)
summary(model)

#Total_pesticides_honey_over_MRL
model<-glm(Total_pesticides_honey_over_MRL ~ Longitude + Latitude, family = poisson(link = "log"), data = combined_bee_data)
summary(model)

model<-glm(Total_pesticides_honey_over_MRL ~ Longitude * Latitude, family = poisson(link = "log"), data = combined_bee_data)
summary(model)

#Pathogens
model<-glm(no._pathogens_workers ~ Longitude + Latitude, family = poisson(link = "log"), data = combined_bee_data)
summary(model)

model<-glm(no._pathogens_workers ~ Longitude * Latitude, family = poisson(link = "log"), data = combined_bee_data)
summary(model)


#LandCovers
#GLMs of ALR
#Only landcover
zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND +
                        alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES + alr_HEATH.and.BRACKEN | 1
                      ,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data, na.action = na.omit) # | 1 indicates zero-inflation is constant across all observations, does not vary with changes in predictors

summary(zip_model)
AIC(zip_model)

#Total_pesticides_pollen_over_LOQ 

zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude +
                        alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND +
                        alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES + alr_HEATH.and.BRACKEN | 1
                      ,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data, na.action = na.omit) # | 1 indicates zero-inflation is constant across all observations, does not vary with changes in predictors

summary(zip_model)
AIC(zip_model)

# Extract model coefficients and their standard errors
coef_summary <- summary(zip_model)$coefficients$count 

# Critical value for 95% CI, two-sided z-test
critical_value <- qnorm(0.975)

# Calculate CIs
ci_lower <- coef_summary[, "Estimate"] - critical_value * coef_summary[, "Std. Error"]
ci_upper <- coef_summary[, "Estimate"] + critical_value * coef_summary[, "Std. Error"]

# Combine into a data frame
confidence_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Lower = ci_lower,
  Upper = ci_upper
)
confidence_intervals





#Total_pesticides_pollen_over_MRL

zip_model <- zeroinfl(Total_pesticides_pollen_over_MRL ~ Longitude + Latitude  +
                        alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND +
                        alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES + alr_HEATH.and.BRACKEN | 1,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data, na.action = na.omit)
summary(zip_model)
AIC(zip_model)


zip_summary <- summary(zip_model)
parametric_table <- zip_summary$coefficients$count

# Convert to a structured data frame
parametric_df <- data.frame(
  Term = rownames(parametric_table),
  Estimate = parametric_table[, "Estimate"],
  Std.Error = parametric_table[, "Std. Error"],
  Z.Value = parametric_table[, "z value"],
  P.Value = parametric_table[, "Pr(>|z|)"]
)

print(parametric_df)

zero_table <- zip_summary$coefficients$zero

# Convert to a structured data frame
zero_df <- data.frame(
  Term = rownames(zero_table),
  Estimate = zero_table[, "Estimate"],
  Std.Error = zero_table[, "Std. Error"],
  Z.Value = zero_table[, "z value"],
  P.Value = zero_table[, "Pr(>|z|)"]
)

print(zero_df)

# Extract model coefficients and their standard errors
coef_summary <- summary(zip_model)$coefficients$count 

# Critical value for 95% CI, two-sided z-test
critical_value <- qnorm(0.975)

# Calculate CIs
ci_lower <- coef_summary[, "Estimate"] - critical_value * coef_summary[, "Std. Error"]
ci_upper <- coef_summary[, "Estimate"] + critical_value * coef_summary[, "Std. Error"]

# Combine into a data frame
confidence_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Lower = ci_lower,
  Upper = ci_upper
)
confidence_intervals


#Total_pesticides_honey_over_LOQ
zip_model <- zeroinfl(Total_pesticides_honey_over_LOQ ~ Longitude + Latitude +
                        alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND +
                        alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES + alr_HEATH.and.BRACKEN | 1,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data, na.action = na.omit)
summary(zip_model)
AIC(zip_model)

# Extract model coefficients and their standard errors
coef_summary <- summary(zip_model)$coefficients$count 

# Critical value for 95% CI, two-sided z-test
critical_value <- qnorm(0.975)

# Calculate CIs
ci_lower <- coef_summary[, "Estimate"] - critical_value * coef_summary[, "Std. Error"]
ci_upper <- coef_summary[, "Estimate"] + critical_value * coef_summary[, "Std. Error"]

# Combine into a data frame
confidence_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Lower = ci_lower,
  Upper = ci_upper
)
confidence_intervals

#Total_pesticides_honey_over_MRL
zip_model <- zeroinfl(Total_pesticides_honey_over_MRL ~ Longitude + Latitude +
                        alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND +
                        alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES + alr_HEATH.and.BRACKEN | 1,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data, na.action = na.omit)
summary(zip_model)
AIC(zip_model)

zip_summary <- summary(zip_model)
parametric_table <- zip_summary$coefficients$count

# Convert to a structured data frame
parametric_df <- data.frame(
  Term = rownames(parametric_table),
  Estimate = parametric_table[, "Estimate"],
  Std.Error = parametric_table[, "Std. Error"],
  Z.Value = parametric_table[, "z value"],
  P.Value = parametric_table[, "Pr(>|z|)"]
)

print(parametric_df)

zero_table <- zip_summary$coefficients$zero

# Convert to a structured data frame
zero_df <- data.frame(
  Term = rownames(zero_table),
  Estimate = zero_table[, "Estimate"],
  Std.Error = zero_table[, "Std. Error"],
  Z.Value = zero_table[, "z value"],
  P.Value = zero_table[, "Pr(>|z|)"]
)

print(zero_df)

# Extract model coefficients and their standard errors
coef_summary <- summary(zip_model)$coefficients$count 

# Critical value for 95% CI, two-sided z-test
critical_value <- qnorm(0.975)

# Calculate CIs
ci_lower <- coef_summary[, "Estimate"] - critical_value * coef_summary[, "Std. Error"]
ci_upper <- coef_summary[, "Estimate"] + critical_value * coef_summary[, "Std. Error"]

# Combine into a data frame
confidence_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Lower = ci_lower,
  Upper = ci_upper
)
confidence_intervals

#No pathogens

model<-glm(no._pathogens_workers ~ Longitude + Latitude, family = poisson(link = "log"), data = combined_bee_data)
summary(model)
#All Ireland
model<-glm(no._pathogens_workers ~ Longitude + Latitude +
             alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND +
             alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES + alr_HEATH.and.BRACKEN,
           family = poisson(link = "log"), data = combined_bee_data, na.action = na.omit)
summary(model)


# Extract model coefficients and their standard errors
coef_summary <- summary(model)$coefficients

# Critical value for 95% CI, two-sided z-test
critical_value <- qnorm(0.975)

# Calculate CIs
ci_lower <- coef_summary[, "Estimate"] - critical_value * coef_summary[, "Std. Error"]
ci_upper <- coef_summary[, "Estimate"] + critical_value * coef_summary[, "Std. Error"]

# Combine into a data frame
confidence_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Lower = ci_lower,
  Upper = ci_upper
)
confidence_intervals

#Leinster
leinster_model <- glm(no._pathogens_workers ~ Longitude + Latitude +
                        clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP +
                        clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES +
                        clr_HEATH.and.BRACKEN + clr_FOREST..WOODLAND.AND.SCRUB,
                      family = poisson(link = "log"), data = leinster_data, na.action = na.omit)

summary(leinster_model)

#Munster
munster_model <- glm(no._pathogens_workers ~ Longitude + Latitude +
                        clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP +
                        clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES +
                        clr_HEATH.and.BRACKEN + clr_FOREST..WOODLAND.AND.SCRUB,
                      family = poisson(link = "log"), data = munster_data, na.action = na.omit)


summary(munster_model)



#Beehive density
leinster_data <- bee_data %>% filter(Province == "Leinster")
munster_data <- bee_data %>% filter(Province == "Munster")

leinster_model <- glm(no._pathogens_workers ~ local_beehive_density, 
                      family = poisson(link = "log"), 
                      data = leinster_data, 
                      na.action = na.omit)
summary(leinster_model)

dispersion_ratio <- sum(residuals(leinster_model, type="pearson")^2) / df.residual(leinster_model)
print(dispersion_ratio)

munster_model <- glm(no._pathogens_workers ~ local_beehive_density, 
                     family = poisson(link = "log"), 
                     data = munster_data, 
                     na.action = na.omit)
summary(munster_model)

dispersion_ratio <- sum(residuals(munster_model, type="pearson")^2) / df.residual(munster_model)
print(dispersion_ratio)



###GLMs of CLR
#Total_pesticides_pollen_over_LOQ


zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude   +
                        clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES + clr_PEATLAND   | 1,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data,  na.action = na.omit)

summary(zip_model)
AIC(zip_model)
hist(combined_bee_data$clr_PEATLAND)
hist(combined_bee_data$clr_EXPOSED.SURFACES)
hist(combined_bee_data$clr_CULTIVATED.LAND)


#REMOVE CLR_PEATLAND
zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude   +
                        clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES   | 1,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data,  na.action = na.omit)

summary(zip_model)
AIC(zip_model)

zip_summary <- summary(zip_model)
parametric_table <- zip_summary$coefficients$count

# Convert to a structured data frame
parametric_df <- data.frame(
  Term = rownames(parametric_table),
  Estimate = parametric_table[, "Estimate"],
  Std.Error = parametric_table[, "Std. Error"],
  Z.Value = parametric_table[, "z value"],
  P.Value = parametric_table[, "Pr(>|z|)"]
)

print(parametric_df)

zero_table <- zip_summary$coefficients$zero

# Convert to a structured data frame
zero_df <- data.frame(
  Term = rownames(zero_table),
  Estimate = zero_table[, "Estimate"],
  Std.Error = zero_table[, "Std. Error"],
  Z.Value = zero_table[, "z value"],
  P.Value = zero_table[, "Pr(>|z|)"]
)

print(zero_df)

# Extract model coefficients and their standard errors
coef_summary <- summary(zip_model)$coefficients$count 

# Critical value for 95% CI, two-sided z-test
critical_value <- qnorm(0.975)

# Calculate CIs
ci_lower <- coef_summary[, "Estimate"] - critical_value * coef_summary[, "Std. Error"]
ci_upper <- coef_summary[, "Estimate"] + critical_value * coef_summary[, "Std. Error"]

# Combine into a data frame
confidence_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Lower = ci_lower,
  Upper = ci_upper
)
confidence_intervals

#Total_pesticides_pollen_over_MRL
zip_model <-zeroinfl(Total_pesticides_pollen_over_MRL ~ Longitude + Latitude + clr_FOREST..WOODLAND.AND.SCRUB +
                       clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP  +
                       clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES + clr_HEATH.and.BRACKEN | 1,
                     dist = "poisson", link = "logit",
                     data = combined_bee_data, na.action = na.omit)
summary(zip_model)
AIC(zip_model)

# Extract model coefficients and their standard errors
coef_summary <- summary(zip_model)$coefficients$count 

# Critical value for 95% CI, two-sided z-test
critical_value <- qnorm(0.975)

# Calculate CIs
ci_lower <- coef_summary[, "Estimate"] - critical_value * coef_summary[, "Std. Error"]
ci_upper <- coef_summary[, "Estimate"] + critical_value * coef_summary[, "Std. Error"]

# Combine into a data frame
confidence_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Lower = ci_lower,
  Upper = ci_upper
)
confidence_intervals

#Total_pesticides_honey_over_LOQ
zip_model <- zeroinfl(Total_pesticides_honey_over_LOQ ~ Longitude + Latitude +
                        clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES | 1,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data)

summary(zip_model)
AIC(zip_model)


# Extract model coefficients and their standard errors
coef_summary <- summary(zip_model)$coefficients$count 

# Critical value for 95% CI, two-sided z-test
critical_value <- qnorm(0.975)

# Calculate CIs
ci_lower <- coef_summary[, "Estimate"] - critical_value * coef_summary[, "Std. Error"]
ci_upper <- coef_summary[, "Estimate"] + critical_value * coef_summary[, "Std. Error"]

# Combine into a data frame
confidence_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Lower = ci_lower,
  Upper = ci_upper
)
confidence_interval

zip_model <- zeroinfl(Total_pesticides_honey_over_LOQ ~ Longitude + Latitude +
                        clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES | 1,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data)

summary(zip_model)
AIC(zip_model)

# Extract model coefficients and their standard errors
coef_summary <- summary(zip_model)$coefficients$count 

# Critical value for 95% CI, two-sided z-test
critical_value <- qnorm(0.975)

# Calculate CIs
ci_lower <- coef_summary[, "Estimate"] - critical_value * coef_summary[, "Std. Error"]
ci_upper <- coef_summary[, "Estimate"] + critical_value * coef_summary[, "Std. Error"]

# Combine into a data frame
confidence_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Lower = ci_lower,
  Upper = ci_upper
)
confidence_interval
library(car)  # for vif() function
vif_model <- vif(zip_model)
print(zip_model)

#normality of residuals 
resid_plot <- resid(zip_model, type = "response")
hist(resid_plot, breaks = "Sturges", main = "Histogram of Residuals", xlab = "Residuals")

#variance of residuals
plot(fitted(model), resid_plot,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")

# Checking for overdispersion
overdispersion_test <- sum(resid(zip_model, type = "pearson")^2) / model$df.residual
print(paste("Overdispersion test statistic:", overdispersion_test))

# Negative binomial model
library(MASS)
nb_model <- glm.nb(Total_pesticides_honey_over_LOQ ~ Longitude + Latitude +
                     clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES +clr_PEATLAND  | 1,
                   dist = "poisson", link = "logit",
                   data = combined_bee_data)
summary(nb_model)


#Total_pesticides_honey_over_MRL

zip_model <- zeroinfl(Total_pesticides_honey_over_MRL ~ Longitude + Latitude +
                        clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES + clr_PEATLAND | 1,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data)

summary(zip_model)
AIC(zip_model)

zip_model <- zeroinfl(Total_pesticides_honey_over_MRL ~ Longitude + Latitude +
                        clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES | 1,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data)

summary(zip_model)
AIC(zip_model)

# Extract model coefficients and their standard errors
coef_summary <- summary(zip_model)$coefficients$count 

# Critical value for 95% CI, two-sided z-test
critical_value <- qnorm(0.975)

# Calculate CIs
ci_lower <- coef_summary[, "Estimate"] - critical_value * coef_summary[, "Std. Error"]
ci_upper <- coef_summary[, "Estimate"] + critical_value * coef_summary[, "Std. Error"]

# Combine into a data frame
confidence_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Lower = ci_lower,
  Upper = ci_upper
)
confidence_interval



#no._pathogens_workers
model<-glm(no._pathogens_workers ~ Longitude + Latitude, family = poisson(link = "log"), data = combined_bee_data)
summary(model)

model<-glm(no._pathogens_workers ~ Longitude + Latitude +
             clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP+
             clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES + clr_HEATH.and.BRACKEN + clr_FOREST..WOODLAND.AND.SCRUB,
           family = poisson(link = "log"), data = combined_bee_data, na.action = na.omit)
summary(model)

model_summary <- summary(model)
parametric_table <- model_summary$coefficients

parametric_df <- data.frame(
  Term = rownames(parametric_table),
  Estimate = parametric_table[, "Estimate"],
  Std.Error = parametric_table[, "Std. Error"],
  Z.Value = parametric_table[, "z value"],
  P.Value = parametric_table[, "Pr(>|z|)"]
)




# Extract model coefficients and their standard errors
coef_summary <- summary(model)$coefficients

# Critical value for 95% CI, two-sided z-test
critical_value <- qnorm(0.975)

# Calculate CIs
ci_lower <- coef_summary[, "Estimate"] - critical_value * coef_summary[, "Std. Error"]
ci_upper <- coef_summary[, "Estimate"] + critical_value * coef_summary[, "Std. Error"]

# Combine into a data frame
confidence_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Lower = ci_lower,
  Upper = ci_upper
)
confidence_intervals


######ILR
zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude + Human_Impacted| 1, 
                      dist = "poisson", 
                      data = combined_bee_data, 
                      na.action = na.omit)
summary(zip_model)
AIC(zip_model)


# Extract model coefficients and their standard errors
coef_summary <- summary(zip_model)$coefficients$count

# Critical value for 95% CI, two-sided z-test
critical_value <- qnorm(0.975)

# Calculate CIs
ci_lower <- coef_summary[, "Estimate"] - critical_value * coef_summary[, "Std. Error"]
ci_upper <- coef_summary[, "Estimate"] + critical_value * coef_summary[, "Std. Error"]

# Combine into a data frame
confidence_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Lower = ci_lower,
  Upper = ci_upper
)
confidence_intervals
library(pscl)

#zero-inflated negative binomial model
zinb_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude + Human_Impacted | 1,
                       dist = "negbin",
                       data = combined_bee_data,
                       na.action = na.omit)
summary(zinb_model)
AIC(zinb_model)


zip_model <- zeroinfl(Total_pesticides_pollen_over_MRL ~ Longitude + Latitude + Human_Impacted | 1, 
                      dist = "poisson", 
                      data = combined_bee_data, 
                      na.action = na.omit)
summary(zip_model)


# Extract model coefficients and their standard errors
coef_summary <- summary(zip_model)$coefficients$count

# Critical value for 95% CI, two-sided z-test
critical_value <- qnorm(0.975)

# Calculate CIs
ci_lower <- coef_summary[, "Estimate"] - critical_value * coef_summary[, "Std. Error"]
ci_upper <- coef_summary[, "Estimate"] + critical_value * coef_summary[, "Std. Error"]

# Combine into a data frame
confidence_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Lower = ci_lower,
  Upper = ci_upper
)
confidence_intervals



zip_model <- zeroinfl(Total_pesticides_honey_over_LOQ ~ Longitude + Latitude + Human_Impacted | 1, 
                      dist = "poisson", 
                      data = combined_bee_data, 
                      na.action = na.omit)
summary(zip_model)
# Extract model coefficients and their standard errors
coef_summary <- summary(zip_model)$coefficients$count

# Critical value for 95% CI, two-sided z-test
critical_value <- qnorm(0.975)

# Calculate CIs
ci_lower <- coef_summary[, "Estimate"] - critical_value * coef_summary[, "Std. Error"]
ci_upper <- coef_summary[, "Estimate"] + critical_value * coef_summary[, "Std. Error"]

# Combine into a data frame
confidence_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Lower = ci_lower,
  Upper = ci_upper
)
confidence_intervals


zip_model <- zeroinfl(Total_pesticides_honey_over_MRL ~ Longitude + Latitude + Human_Impacted | 1, 
                      dist = "poisson", 
                      data = combined_bee_data, 
                      na.action = na.omit)
summary(zip_model)
residuals<-simulateResiduals(zip_model)
plot(residuals)

# Extract model coefficients and their standard errors
coef_summary <- summary(zip_model)$coefficients$count

# Critical value for 95% CI, two-sided z-test
critical_value <- qnorm(0.975)

# Calculate CIs
ci_lower <- coef_summary[, "Estimate"] - critical_value * coef_summary[, "Std. Error"]
ci_upper <- coef_summary[, "Estimate"] + critical_value * coef_summary[, "Std. Error"]

# Combine into a data frame
confidence_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Lower = ci_lower,
  Upper = ci_upper
)
confidence_intervals

model <- glm(no._pathogens_workers ~ Longitude + Latitude + Human_Impacted, family = poisson(link = "log"),
                      data = combined_bee_data, 
                      na.action = na.omit)
summary(model)
residuals<-simulateResiduals(model)
plot(residuals)

# Extract model coefficients and their standard errors
coef_summary <- summary(model)$coefficients

# Critical value for 95% CI, two-sided z-test
critical_value <- qnorm(0.975)

# Calculate CIs
ci_lower <- coef_summary[, "Estimate"] - critical_value * coef_summary[, "Std. Error"]
ci_upper <- coef_summary[, "Estimate"] + critical_value * coef_summary[, "Std. Error"]

# Combine into a data frame
confidence_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Lower = ci_lower,
  Upper = ci_upper
)
confidence_intervals

