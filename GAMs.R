#GAM beedata
library(mgcv) #for GAMs
library(compositions) # for compositional data analysis
library(MASS)  # for stepAIC
library(dplyr)  # for data manipulation
library(mgcv)
library(glmmTMB)
library(pscl)

setwd("/Users/rosiemangan/Desktop/MSc/Semester 2/Dissertation/R script")
list.files()

#L1VA
bee_data <- read.csv("L1VA_Dateset_Final.csv")
glimpse(bee_data)
View(bee_data)
colnames(bee_data)


# Add county per province
bee_data <- bee_data %>%
  mutate(Province = case_when(
    County %in% c("Carlow", "Dublin", "Kildare", "Kilkenny", "Laois", "Longford", "Louth", "Meath", "Offaly", "Westmeath", "Wexford", "Wicklow") ~ "Leinster",
    County %in% c("Clare", "Cork", "Kerry", "Limerick", "Tipperary", "Waterford") ~ "Munster",
    County %in% c("Galway", "Leitrim", "Mayo", "Roscommon", "Sligo") ~ "Connacht",
    County %in% c("Derry", "Antrim", "Down", "Tyrone", "Armagh", "Fermanagh", "Cavan", "Monaghan", "Donegal") ~ "Ulster",
    TRUE ~ "Unknown"  #In case I forgot to list any county 
  ))

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

# generate names for CLR-transformed columns
clr_column_names <- paste0("clr_", colnames(comp_data))

# Assign these names to the CLR data
colnames(clr_data) <- clr_column_names
print(colnames(clr_data))

# Check the variability in the transformed data
var(clr_data$clr_ARTIFICIAL.SURFACES)

# Model fitting to see the significance and influence in the model
summary(model)$coefficients["clr_ARTIFICIAL.SURFACES", ]

# Plot to check distribution post-transformation
hist(clr_data$clr_ARTIFICIAL.SURFACES, main="CLR Transformed: Artifical surfaces", xlab="CLR Values", breaks=20, col="green")

cor_matrix <- cor(clr_data)
print(cor_matrix["clr_ARTIFICIAL.SURFACES", ])
print(cor_matrix["clr_GRASSLAND..SALTMARSH.and.SWAMP", ])
print(cor_matrix["clr_HEATH.and.BRACKEN", ])
print(cor_matrix["clr_WATERBODIES", ])
print(cor_matrix["clr_CULTIVATED.LAND", ])
print(cor_matrix["clr_EXPOSED.SURFACES", ])
print(cor_matrix["clr_PEATLAND", ])
print(cor_matrix["clr_FOREST..WOODLAND.AND.SCRUB", ])


# Define the landcover types
landcover_types <- c("ARTIFICIAL.SURFACES", "GRASSLAND..SALTMARSH.and.SWAMP", 
                     "HEATH.and.BRACKEN", "WATERBODIES", "CULTIVATED.LAND", "EXPOSED.SURFACES", 
                     "FOREST..WOODLAND.AND.SCRUB", "PEATLAND")

combined_bee_data <- cbind(bee_data[, !(names(bee_data) %in% landcover_types)], alr_data, clr_data)

View(combined_bee_data)

#GLMs of ILR

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




###############Spatial effect on response variable

#Total_pesticides_pollen_over_LOQ
model <- gam(Total_pesticides_pollen_over_LOQ ~ s(Longitude, Latitude,k = 20), data = combined_bee_data, family = poisson())
summary(model)


model <- gam(Total_pesticides_pollen_over_LOQ ~ s(Longitude, k = 10) + s(Latitude, k = 10), 
             data = combined_bee_data, 
             family = poisson())

summary(model)

# Plot the spatial effect
plot(model, select = 1)
plot(model, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, 
     main = "ALR spatial effect of pesticides in pollen (LOQ)", 
     xlab = "Longitude", ylab = "Latitude", 
     cex.axis = 3.5, cex.lab = 3.5)



#The plot shows contour lines that represent the smooth effect of spatial coordinates (latitude and longitude) on the response variable (Total_pesticides_pollen_over_LOQ).
#Solid Black Lines: These lines represent areas where the smooth function has the same value. Essentially, they indicate regions of equal predicted pesticide levels.
#Dashed Lines: These lines represent standard errors or confidence intervals around the smooth effect. The different colors (red, green) indicate different levels of these intervals, often at 1 and 2 standard errors.


#Total_pesticides_pollen_over_MRL
model <- gam(Total_pesticides_pollen_over_MRL ~ s(Longitude, Latitude, k=20), data = combined_bee_data, family = poisson())
summary(model)
model <- gam(Total_pesticides_pollen_over_MRL ~ s(Longitude, k = 10) + s(Latitude, k = 10), 
             data = combined_bee_data, 
             family = poisson())
# Model summary
summary(model)

# Plot the spatial effect
plot(model, select = 1)
plot(model, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, 
     main = "Spatial effect of pesticides in pollen (over the maximum residue level (MRL))", 
     cex.axis = 3.5, cex.lab = 3.5)


#Total_pesticides_honey_over_LOQ
model <- gam(Total_pesticides_honey_over_LOQ ~ s(Longitude, Latitude, k=20), data = bee_data, family = poisson())
summary(model)

model <- gam(Total_pesticides_honey_over_LOQ ~ s(Longitude, k = 10) + s(Latitude, k = 10), 
             data = combined_bee_data, 
             family = poisson())
summary(model)

# Plot the spatial effect
plot(model, select = 1)
plot(model, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, 
     main = "Spatial effect of pesticides in honey (over the limit of quantification (LOQ))", 
     xlab = "Longitude", ylab = "Latitude", 
     cex.axis = 2.5, cex.lab = 2.5)



#The plot shows contour lines that represent the smooth effect of spatial coordinates (latitude and longitude) on the response variable (Total_pesticides_pollen_over_LOQ).
#Solid Black Lines: These lines represent areas where the smooth function has the same value. Essentially, they indicate regions of equal predicted pesticide levels.
#Dashed Lines: These lines represent standard errors or confidence intervals around the smooth effect. The different colors (red, green) indicate different levels of these intervals, often at 1 and 2 standard errors.


#Total_pesticides_honey_over_MRL
model <- gam(Total_pesticides_honey_over_MRL ~ s(Longitude, Latitude, k=20), data = bee_data, family = poisson())
summary(model)


model <- gam(Total_pesticides_honey_over_MRL ~ s(Longitude, k = 10) + s(Latitude, k = 10), 
             data = combined_bee_data, 
             family = poisson())

summary(model)

# Plot the spatial effect
# Plot the spatial effect
plot(model, select = 1)
plot(model, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, 
     main = "Spatial effect of pesticides in honey (over the maximum residue level (MRL))", 
     xlab = "Longitude", ylab = "Latitude", 
     cex.axis = 2.5, cex.lab = 2.5)


#No of pathogen
model <- gam(no._pathogens_workers ~ s(Longitude, Latitude, k=20), data = bee_data, family = poisson())
summary(model)
model <- gam(no._pathogens_workers ~ s(Longitude, k = 10) + s(Latitude, k = 10), 
             data = combined_bee_data, 
             family = poisson())

summary(model)

# Plot the spatial effect
# Plot the spatial effect
plot(model, select = 1)
plot(model, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, 
     main = "Spatial effect of no. of pathogen workers", 
     xlab = "Longitude", ylab = "Latitude", 
     cex.axis = 1.5, cex.lab = 1.5)





################################ALR

#Response: Total_pesticides_pollen_over_LOQ

model <- gam(Total_pesticides_pollen_over_LOQ ~ s(Longitude, Latitude)  + alr_ARTIFICIAL.SURFACES + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_HEATH.and.BRACKEN + alr_WATERBODIES + alr_CULTIVATED.LAND + alr_EXPOSED.SURFACES + alr_PEATLAND, family = poisson(link = "log"), data =  combined_bee_data, , na.action = na.omit)
model_summary<-summary(model)

# extract parametric coefficients
parametric_table <- model_summary$p.table
print(parametric_table)

report_df <- data.frame(Term = rownames(parametric_table),
                        Estimate = parametric_table[, "Estimate"],
                        Std.Error = parametric_table[, "Std. Error"],
                        Z.Value = parametric_table[, "z value"],
                        P.Value = parametric_table[, "Pr(>|z|)"])

# extract smooth coefficients
smooth_table <- model_summary$s.table
smooth_df <- as.data.frame(smooth_table)


# Plot smooth term for spatial coordinates
plot_smooth_spatial <- plot(model, select = 1, shade = TRUE, rug = TRUE, main = "Spatial effects of pesticides in pollen (LOQ) for ALR transformation", xlab = "Longitude", ylab = "Latitude", 
                            cex.axis = 3.5, cex.lab = 3.5)

#model with zero-inflation

zi_model <- glmmTMB(Total_pesticides_pollen_over_LOQ ~ s(Longitude, Latitude) +
                      alr_ARTIFICIAL.SURFACES + alr_GRASSLAND..SALTMARSH.and.SWAMP +
                      alr_HEATH.and.BRACKEN + alr_WATERBODIES + alr_CULTIVATED.LAND + 
                      alr_EXPOSED.SURFACES + alr_PEATLAND,
                    ziformula = ~ 1,  # Zero-inflation formula
                    family = poisson(link = "logit"), 
                    data = combined_bee_data)

summary(zi_model)

#GAM without Zero-Inflation: AIC = 143.1015
#Zero-Inflated Model: AIC = 161.9
#GAM deviance 64.5%
#high Std. Error in zi_model intercept

#Response: Total_pesticides_pollen_over_MRL

model <- gam(Total_pesticides_pollen_over_MRL ~ s(Longitude, Latitude) + alr_ARTIFICIAL.SURFACES + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_HEATH.and.BRACKEN + alr_WATERBODIES + alr_CULTIVATED.LAND + alr_EXPOSED.SURFACES + alr_PEATLAND, family = poisson(link = "log"), data =  combined_bee_data, , na.action = na.omit)

model_summary<-summary(model)
AIC(model)

zi_model <- glmmTMB(Total_pesticides_pollen_over_MRL ~ s(Longitude, Latitude) +
                      alr_ARTIFICIAL.SURFACES + alr_GRASSLAND..SALTMARSH.and.SWAMP +
                      alr_HEATH.and.BRACKEN + alr_WATERBODIES + alr_CULTIVATED.LAND + 
                      alr_EXPOSED.SURFACES + alr_PEATLAND,
                    ziformula = ~ 1,  # Zero-inflation formula
                    family = poisson(link = "logit"), 
                    data = combined_bee_data)

summary(zi_model)
AIC(zi_model)


#GAM without Zero-Inflation: AIC = 78.5936
#Zero-Inflated Model: AIC = 87.73255
#GAM deviance 58%
#high Std. Error in zi_model intercept

# extract parametric coefficients
parametric_table <- model_summary$p.table
print(parametric_table)

report_df <- data.frame(Term = rownames(parametric_table),
                        Estimate = parametric_table[, "Estimate"],
                        Std.Error = parametric_table[, "Std. Error"],
                        Z.Value = parametric_table[, "z value"],
                        P.Value = parametric_table[, "Pr(>|z|)"])

# extract smooth coefficients
smooth_table <- model_summary$s.table
smooth_df <- as.data.frame(smooth_table)

# Plot smooth term for spatial coordinates
plot_smooth_spatial <- plot(model, select = 1, shade = TRUE, rug = TRUE, main = "Spatial effects of pesticides in pollen (over maximum residue limit (MRL)) for alr landscape model", xlab = "Longitude", ylab = "Latitude", 
                            cex.axis = 3.5, cex.lab = 3.5)


#Response: Total_pesticides_honey_over_LOQ
model <- gam(Total_pesticides_honey_over_LOQ ~ s(Longitude, Latitude) + alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND + alr_HEATH.and.BRACKEN + alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES, family = poisson(link = "log"), data =  combined_bee_data)
model_summary<-summary(model)

model <- gam(Total_pesticides_honey_over_LOQ ~ s(Longitude, k = 10) + s(Latitude, k = 10) + alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND + alr_HEATH.and.BRACKEN + alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES, family = poisson(link = "log"), data =  combined_bee_data)
model_summary<-summary(model)
#model with zero-inflation


zi_model <- glmmTMB(Total_pesticides_honey_over_LOQ ~ s(Longitude, Latitude) +
                      alr_ARTIFICIAL.SURFACES + alr_GRASSLAND..SALTMARSH.and.SWAMP +
                      alr_HEATH.and.BRACKEN + alr_WATERBODIES + alr_CULTIVATED.LAND + 
                      alr_EXPOSED.SURFACES + alr_PEATLAND,
                    ziformula = ~ 1,  # Zero-inflation formula
                    family = poisson(link = "logit"), 
                    data = combined_bee_data)

summary(zi_model)

# extract parametric coefficients
parametric_table <- model_summary$p.table
print(parametric_table)

report_df <- data.frame(Term = rownames(parametric_table),
                        Estimate = parametric_table[, "Estimate"],
                        Std.Error = parametric_table[, "Std. Error"],
                        Z.Value = parametric_table[, "z value"],
                        P.Value = parametric_table[, "Pr(>|z|)"])

# extract smooth coefficients
smooth_table <- model_summary$s.table
smooth_df <- as.data.frame(smooth_table)


#Response: Total_pesticides_honey_over_MRL
model <- gam(Total_pesticides_honey_over_MRL ~ s(Longitude, Latitude) + alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND + alr_HEATH.and.BRACKEN + alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES, family = poisson(link = "log"), data =  combined_bee_data)
model_summary<-summary(model)

model <- gam(Total_pesticides_honey_over_MRL ~ s(Longitude, k = 10) + s(Latitude, k = 10) + alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND + alr_HEATH.and.BRACKEN + alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES, family = poisson(link = "log"), data =  combined_bee_data)
model_summary<-summary(model)

zi_model <- glmmTMB(Total_pesticides_honey_over_MRL ~ s(Longitude, Latitude) +
                      alr_ARTIFICIAL.SURFACES + alr_GRASSLAND..SALTMARSH.and.SWAMP +
                      alr_HEATH.and.BRACKEN + alr_WATERBODIES + alr_CULTIVATED.LAND + 
                      alr_EXPOSED.SURFACES + alr_PEATLAND,
                    ziformula = ~ 1,  # Zero-inflation formula
                    family = poisson(link = "logit"), 
                    data = combined_bee_data)

summary(zi_model)



# extract parametric coefficients
parametric_table <- model_summary$p.table
print(parametric_table)

report_df <- data.frame(Term = rownames(parametric_table),
                        Estimate = parametric_table[, "Estimate"],
                        Std.Error = parametric_table[, "Std. Error"],
                        Z.Value = parametric_table[, "z value"],
                        P.Value = parametric_table[, "Pr(>|z|)"])

# extract smooth coefficients
smooth_table <- model_summary$s.table
smooth_df <- as.data.frame(smooth_table)

# Plot smooth term for spatial coordinates
plot_smooth_spatial <- plot(model, select = 1, shade = TRUE, rug = TRUE, main = "Spatial effects of pesticides in honey (over maximum residue limit (MRL)) for alr landscape model", xlab = "Longitude", ylab = "Latitude", 
                            cex.axis = 3.5, cex.lab = 3.5)


#Response: No_pathogen

model <- gam(no._pathogens_workers ~ s(Longitude, Latitude) + + alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND + alr_HEATH.and.BRACKEN + alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES, family = poisson(link = "log"), data =  combined_bee_data)
model_summary <-summary(model)

# extract parametric coefficients
parametric_table <- model_summary$p.table
print(parametric_table)

report_df <- data.frame(Term = rownames(parametric_table),
                        Estimate = parametric_table[, "Estimate"],
                        Std.Error = parametric_table[, "Std. Error"],
                        Z.Value = parametric_table[, "z value"],
                        P.Value = parametric_table[, "Pr(>|z|)"])

# extract smooth coefficients
smooth_table <- model_summary$s.table
smooth_df <- as.data.frame(smooth_table)

# Plot smooth term for spatial coordinates
plot_smooth_spatial <- plot(model, select = 1, shade = TRUE, rug = TRUE, main = "Spatial effects of pesticides in honey (over maximum residue limit (MRL)) for alr landscape model", xlab = "Longitude", ylab = "Latitude", 
                            cex.axis = 3.5, cex.lab = 3.5)




# Plot smooth term for spatial coordinates
plot_smooth_spatial <- plot(model, select = 1, shade = TRUE, rug = TRUE, main = "Spatial effects of number of pathogens_workers for alr landscape model", xlab = "Longitude", ylab = "Latitude", 
                            cex.axis = 2.5, cex.lab = 2.5)






########################CLR


#Response: Total_pesticides_pollen_over_LOQ
model <- gam(Total_pesticides_pollen_over_LOQ ~ s(Longitude, Latitude) + clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES, family = poisson(link = "log"), data =  combined_bee_data)
model_summary<-summary(model)

# extract parametric coefficients
parametric_table <- model_summary$p.table
print(parametric_table)

report_df <- data.frame(Term = rownames(parametric_table),
                        Estimate = parametric_table[, "Estimate"],
                        Std.Error = parametric_table[, "Std. Error"],
                        Z.Value = parametric_table[, "z value"],
                        P.Value = parametric_table[, "Pr(>|z|)"])

# extract smooth coefficients
smooth_table <- model_summary$s.table
smooth_df <- as.data.frame(smooth_table)



# Plot smooth term for spatial coordinates
plot_smooth_spatial <- plot(model, select = 1, shade = TRUE, rug = TRUE, main = "Spatial effects of pesticides in pollen (over limit of quantification (LOQ)) for clr landscape model", xlab = "Longitude", ylab = "Latitude", 
                            cex.axis = 3.5, cex.lab = 3.5)



#Response: Total_pesticides_pollen_over_MRL

model <- gam(Total_pesticides_pollen_over_MRL ~ s(Longitude, Latitude) + clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES, family = poisson(link = "log"), data =  combined_bee_data)
model_summary<-summary(model)

# Plot smooth term for spatial coordinates
plot_smooth_spatial <- plot(model, select = 1, shade = TRUE, rug = TRUE, main = "Spatial effects of pesticides in pollen (over maximum residue limit (MRL)) for clr landscape model", xlab = "Longitude", ylab = "Latitude", 
                            cex.axis = 3.5, cex.lab = 3.5)
# extract parametric coefficients
parametric_table <- model_summary$p.table
print(parametric_table)

report_df <- data.frame(Term = rownames(parametric_table),
                        Estimate = parametric_table[, "Estimate"],
                        Std.Error = parametric_table[, "Std. Error"],
                        Z.Value = parametric_table[, "z value"],
                        P.Value = parametric_table[, "Pr(>|z|)"])

# extract smooth coefficients
smooth_table <- model_summary$s.table
smooth_df <- as.data.frame(smooth_table)


#Response: Total_pesticides_honey_over_LOQ
model <- gam(Total_pesticides_honey_over_LOQ ~ s(Longitude, Latitude) + clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES, family = poisson(link = "log"), data =  combined_bee_data)
model_summary<-summary(model)

# extract parametric coefficients
parametric_table <- model_summary$p.table
print(parametric_table)

report_df <- data.frame(Term = rownames(parametric_table),
                        Estimate = parametric_table[, "Estimate"],
                        Std.Error = parametric_table[, "Std. Error"],
                        Z.Value = parametric_table[, "z value"],
                        P.Value = parametric_table[, "Pr(>|z|)"])

# extract smooth coefficients
smooth_table <- model_summary$s.table
smooth_df <- as.data.frame(smooth_table)



# Plot smooth term for spatial coordinates
plot_smooth_spatial <- plot(model, select = 1, shade = TRUE, rug = TRUE, main = "Spatial effects of pesticides in pollen (over limit of quantification (LOQ)) for clr landscape model", xlab = "Longitude", ylab = "Latitude", 
                            cex.axis = 3.5, cex.lab = 3.5)

#Response: Total_pesticides_honey_over_MRL
model <- gam(Total_pesticides_honey_over_MRL ~ s(Longitude, Latitude) + clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES, family = poisson(link = "log"), data =  combined_bee_data)
model_summary<-summary(model)

# Plot smooth term for spatial coordinates
plot_smooth_spatial <- plot(model, select = 1, shade = TRUE, rug = TRUE, main = "Spatial effects of pesticides in honey (over maximum residue limit (MRL)) for clr landscape model", xlab = "Longitude", ylab = "Latitude", 
                            cex.axis = 3.5, cex.lab = 3.5)

# extract parametric coefficients
parametric_table <- model_summary$p.table
print(parametric_table)

report_df <- data.frame(Term = rownames(parametric_table),
                        Estimate = parametric_table[, "Estimate"],
                        Std.Error = parametric_table[, "Std. Error"],
                        Z.Value = parametric_table[, "z value"],
                        P.Value = parametric_table[, "Pr(>|z|)"])

# extract smooth coefficients
smooth_table <- model_summary$s.table
smooth_df <- as.data.frame(smooth_table)


#Response: No_pathogen

model <- gam(no._pathogens_workers ~ s(Longitude, Latitude)  + clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES, family = poisson(link = "log"), data =  combined_bee_data)
model_summary<-summary(model)

# extract parametric coefficients
parametric_table <- model_summary$p.table
print(parametric_table)

report_df <- data.frame(Term = rownames(parametric_table),
                        Estimate = parametric_table[, "Estimate"],
                        Std.Error = parametric_table[, "Std. Error"],
                        Z.Value = parametric_table[, "z value"],
                        P.Value = parametric_table[, "Pr(>|z|)"])

# extract smooth coefficients
smooth_table <- model_summary$s.table
smooth_df <- as.data.frame(smooth_table)



# Plot smooth term for spatial coordinates
plot_smooth_spatial <- plot(model, select = 1, shade = TRUE, rug = TRUE, main = "Spatial effects of number of pathogens_workers in beehives for clr landscape model", xlab = "Longitude", ylab = "Latitude", 
                            cex.axis = 2.5, cex.lab = 2.5)



#ILR
gam_model <- gam(Total_pesticides_pollen_over_LOQ ~ s(Longitude, Latitude) + Human_Impacted,
                 family = poisson(link = "log"), 
                 data = combined_bee_data, 
                 method = "REML")
summary(gam_model)


gam_model <- gam(Total_pesticides_pollen_over_MRL ~ s(Longitude, Latitude) + Human_Impacted,
                 family = poisson(link = "log"), 
                 data = combined_bee_data, 
                 method = "REML")
summary(gam_model)

gam_model <- gam(Total_pesticides_honey_over_LOQ ~ s(Longitude, Latitude) + Human_Impacted,
                 family = poisson(link = "log"), 
                 data = combined_bee_data, 
                 method = "REML")
summary(gam_model)

gam_model <- gam(Total_pesticides_honey_over_MRL ~ s(Longitude, Latitude) + Human_Impacted,
                 family = poisson(link = "log"), 
                 data = combined_bee_data, 
                 method = "REML")
summary(gam_model)

gam_model <- gam(no._pathogens_workers ~ s(Longitude, Latitude) + Human_Impacted,
                 family = poisson(link = "log"), 
                 data = combined_bee_data, 
                 method = "REML")
summary(gam_model)




#12 Sept Environ variables

# clear existing objects and reload dplyr
rm(list = ls())
library(dplyr)

#create new dataframe
new_bee_data <- bee_data %>%
  dplyr::select(
    no._pathogens_workers,
    any_pathogens_workers,
    Total_pesticides_pollen_over_LOQ,
    Total_pesticides_pollen_over_MRL,
    Total_pesticides_honey_over_LOQ,
    Total_pesticides_honey_over_MRL,
    Landscape_total,
    County,
    Month_sampled,
    Date_sampled,
    Days_in_minus_20_freezer,
    No._colonies_sampled,
    Q2_No._apiaries_after,
    Q5_No._queen_problems_after,
    Q6_No._natural_disaster_after,
    Q8c_signs_of_starvation_after,
    Q17_treated_for_Varroa_prev_year_after,
    Prop_Cultivated_Land,
    Prop_Waterbodies,
    Min_Dist_Water_metre,
    Min_Dist_Coast_metre,
    Closest_Station,
    Distance,
    Total_Rainfall_mm,
    Mean_Dry_Bulb_Temperature_degC,
    Mean_10cm_Soil_Temperature_degC,
    Total_Global_Solar_Radiation_MJ_per_m2,
    Total_Potential_Evaporation_mm,
    Total_Evaporation_mm,
    Degree_Days_Below_15_5_degC,
    Prop_Fert_Improved.Grassland,
    Elevation,
    Apiaries_within_5km,
    Apiaries_within_10km,
    Apiaries_within_25km,
    Q19_treated_for_Varroa_prev_year,
    Min_Dist_Urban_metre,
    Min_Dist_Road_metre,
    Site.ID,
    Site_Id,
    Soil_drainage,
    Soil_texture,
    Soil_description,
    Proximity_to_Pollutant,
    Nearest_Pollutant
  )

glm_poisson <- glm(Total_pesticides_pollen_over_MRL ~ Mean_Dry_Bulb_Temperature_degC + 
                     Prop_Cultivated_Land + Prop_Fert_Improved.Grassland, 
                   family = poisson(), data = new_bee_data)
summary(glm_poisson)





glm_poisson <- glm(Total_pesticides_honey_over_MRL ~ Proximity_to_Pollutant, 
                   family = poisson(), data = new_bee_data)

glm_poisson <- glm(no._pathogens_workers ~ Min_Dist_Water_metre + Min_Dist_Road_metre + Apiaries_within_5km,
                   family = poisson(), data = new_bee_data)

glm_poisson <- glm(no._pathogens_workers ~       
                     Q17_treated_for_Varroa_prev_year_after,
                   family = poisson(), data = new_bee_data)
summary(glm_poisson)



# View the summary of the model
summary(glm_poisson)
coefficients(glm_poisson)

#overdispersion check
with(glm_poisson, sum(resid(type="pearson")^2) / df.residual)


dispersiontest <- sum(residuals(glm_poisson, type = "pearson")^2) / df.residual(glm_poisson)
print(paste("Dispersion parameter:", dispersiontest))

glm_quasi <- glm(Total_pesticides_pollen_over_MRL ~ Prop_Cultivated_Land +
                   Total_Rainfall_mm + Mean_Dry_Bulb_Temperature_degC + 
                   Min_Dist_Water_metre + Elevation + 
                   Apiaries_within_5km + Proximity_to_Pollutant,
                 family = quasipoisson(), data = new_bee_data)

summary(glm_quasi)

library(MASS)

glm_nb <- glm.nb(Total_pesticides_pollen_over_MRL ~ Prop_Cultivated_Land +
                   Total_Rainfall_mm + Mean_Dry_Bulb_Temperature_degC + 
                   Min_Dist_Water_metre + Elevation + 
                   Apiaries_within_5km + Proximity_to_Pollutant,
                 data = new_bee_data)

summary(glm_nb)

library(lme4)

# rescale numeric predictors
new_bee_data <- new_bee_data %>%
  mutate_at(vars(Prop_Cultivated_Land, Total_Rainfall_mm, Mean_Dry_Bulb_Temperature_degC, 
                 Min_Dist_Water_metre, Elevation, Apiaries_within_5km, Proximity_to_Pollutant), scale)

# Retry fitting the model
glm_mixed <- glmer(Total_pesticides_pollen_over_MRL ~ Prop_Cultivated_Land +
                     Total_Rainfall_mm + Mean_Dry_Bulb_Temperature_degC + 
                     Min_Dist_Water_metre + Elevation + 
                     Apiaries_within_5km + Proximity_to_Pollutant + 
                     (1 | County),
                   family = poisson(), data = new_bee_data)

summary(glm_mixed)




# Convert categorical variables to factors
categorical_vars <- c("County", "Month_sampled", "Closest_Station", "Site.ID", "Soil_drainage", "Soil_texture", "Soil_description", "Proximity_to_Pollutant", "Nearest_Pollutant")
bee_data[categorical_vars] <- lapply(bee_data[categorical_vars], factor)

# Removing rows with any NA values
bee_data <- na.omit(bee_data)

# Fit initial model
full_model <- glm(Total_pesticides_pollen_over_MRL ~ ., family = binomial(), data = new_bee_data)

# Stepwise model selection based on AIC
stepwise_model <- stepAIC(full_model, direction = "both")

# Summary of the stepwise model
summary(stepwise_model)










glm_model <- glm(Total_pesticides_honey_over_LOQ ~ Total_Rainfall_mm + Q17_treated_for_Varroa_prev_year_after + Min_Dist_Water_metre,
                 family = poisson(), data = bee_data)
summary(glm_model)

####GLM


#Total_pesticides_pollen_over_MRL
model<-glm(Total_pesticides_pollen_over_MRL~ Date_sampled + Days_in_minus_20_freezer + No._colonies_sampled + Q2_No._apiaries_after +                
             Q5_No._queen_problems_after + Q6_No._natural_disaster_after + Q8c_signs_of_starvation_after + Q17_treated_for_Varroa_prev_year_after + Prop_Cultivated_Land + Prop_Waterbodies +  Min_Dist_Water_metre +  Min_Dist_Coast_metre +             
             Total_Rainfall_mm + Mean_Dry_Bulb_Temperature_degC +  Mean_10cm_Soil_Temperature_degC + Total_Global_Solar_Radiation_MJ_per_m2 +
             Total_Potential_Evaporation_mm +  Total_Evaporation_mm + Degree_Days_Below_15_5_degC, 
           family = poisson(link = "log"), data = combined_bee_data)

summary(model)

#Total_pesticides_honey_over_LOQ
#Total_pesticides_honey_over_MRL
#Total_pesticides_honey_over_LOQ
##no._pathogens_workers


####GAM
#Total_pesticides_pollen_over_MRL
#Total_pesticides_honey_over_LOQ
#Total_pesticides_honey_over_MRL
#Total_pesticides_honey_over_LOQ
##no._pathogens_workers


#Exploratory modelling of explanatory variables

#Total_pesticides_pollen_over_LOQ
# Longitude and Latitude
model1 <- gam(Total_pesticides_pollen_over_LOQ ~ s(Longitude, Latitude, k = 20), 
              family = poisson(link = "log"), data = combined_bee_data)
summary(model1)
plot(model1)



# Model for County (treated as a factor) 
model_County <- gam(Total_pesticides_pollen_over_LOQ ~ County, data = combined_bee_data, family = poisson(link = "log")) 
summary(model_County)

# Model for Month_sampled (treated as a factor)

model_Month_sampled <- gam(Total_pesticides_pollen_over_LOQ ~ Month_sampled, data = combined_bee_data, family = poisson(link = "log"))

summary(model_Month_sampled)


# Model for Date_sampled (treated as a factor)
model_Date_sampled <- gam(Total_pesticides_pollen_over_LOQ ~ Date_sampled, data = combined_bee_data, family = poisson(link = "log"))
summary(model_Date_sampled)

# Model for Days_in_minus_20_freezer
model_Days_in_minus_20_freezer <- gam(Total_pesticides_pollen_over_LOQ ~ s(Days_in_minus_20_freezer, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Days_in_minus_20_freezer)

# Replace 'Y' with 1 and 'N' with 0 in the '17_treated_for_Varroa_prev_year_after' column combined_bee_data$Q17_treated_for_Varroa_prev_year_after` <- ifelse(combined_bee_data$`17_treated_for_Varroa_prev_year_after` == 'Y', 1, 0)
combined_bee_data$Q17_treated_for_Varroa_prev_year_after <- ifelse(combined_bee_data$Q17_treated_for_Varroa_prev_year_after == 'Y', 1, 0)


# Model for Q17_treated_for_Varroa_prev_year_after
model_Q17_treated_for_Varroa_prev_year_after <- gam(Total_pesticides_pollen_over_LOQ ~ s(Q17_treated_for_Varroa_prev_year_after, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Q17_treated_for_Varroa_prev_year_after)

# Model for Prop_Cultivated_Land
model_Prop_Cultivated_Land <- gam(Total_pesticides_pollen_over_LOQ ~ s(Prop_Cultivated_Land, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Prop_Cultivated_Land)

# Model for Prop_Water_Bodies
model_Prop_Water_Bodies <- gam(Total_pesticides_pollen_over_LOQ ~ s(Prop_Waterbodies, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Prop_Water_Bodies)

# Model for Min_Dist_Water_metre
model_Min_Dist_Water_metre <- gam(Total_pesticides_pollen_over_LOQ ~ s(Min_Dist_Water_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Water_metre)

# Model for Min_Dist_Coast_metre
model_Min_Dist_Coast_metre <- gam(Total_pesticides_pollen_over_LOQ ~ s(Min_Dist_Coast_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Coast_metre)



# Model for Total_Rainfall_mm
model_Total_Rainfall_mm <- gam(Total_pesticides_pollen_over_LOQ ~ s(Total_Rainfall_mm, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Rainfall_mm)

# Model for Mean_Dry_Bulb_Temperature_degC
model_Mean_Dry_Bulb_Temperature_degC <- gam(Total_pesticides_pollen_over_LOQ ~ s(Mean_Dry_Bulb_Temperature_degC, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Mean_Dry_Bulb_Temperature_degC)

# Model for Mean_10cm_Soil_Temperature_degC
model_Mean_10cm_Soil_Temperature_degC <- gam(Total_pesticides_pollen_over_LOQ ~ s(Mean_10cm_Soil_Temperature_degC, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Mean_10cm_Soil_Temperature_degC)

# Model for Total_Global_Solar_Radiation_MJ_per_m2
model_Total_Global_Solar_Radiation_MJ_per_m2 <- gam(Total_pesticides_pollen_over_LOQ ~ s(Total_Global_Solar_Radiation_MJ_per_m2, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Global_Solar_Radiation_MJ_per_m2)

# Model for Total_Potential_Evaporation_mm
model_Total_Potential_Evaporation_mm <- gam(Total_pesticides_pollen_over_LOQ ~ s(Total_Potential_Evaporation_mm, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Potential_Evaporation_mm)

# Model for Total_Evaporation_mm
model_Total_Evaporation_mm <- gam(Total_pesticides_pollen_over_LOQ ~ s(Total_Evaporation_mm, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Evaporation_mm)

# Model for Degree_Days_Below_15_5_degC
model_Degree_Days_Below_15_5_degC <- gam(Total_pesticides_pollen_over_LOQ ~ s(Degree_Days_Below_15_5_degC, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Degree_Days_Below_15_5_degC)

# Model for Prop_Fert_Improved.Grassland
model_Prop_Fert_Improved_Grassland <- gam(Total_pesticides_pollen_over_LOQ ~ s(Prop_Fert_Improved.Grassland, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Prop_Fert_Improved_Grassland)

# Model for Elevation
model_Elevation <- gam(Total_pesticides_pollen_over_LOQ ~ s(Elevation, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Elevation)

# Model for Apiaries_within_5km
model_Apiaries_within_5km <- gam(Total_pesticides_pollen_over_LOQ ~ s(Apiaries_within_5km, k = 7), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Apiaries_within_5km)

# Model for Apiaries_within_10km
model_Apiaries_within_10km <- gam(Total_pesticides_pollen_over_LOQ ~ s(Apiaries_within_10km, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Apiaries_within_10km)

# Model for Apiaries_within_25km
model_Apiaries_within_25km <- gam(Total_pesticides_pollen_over_LOQ ~ s(Apiaries_within_25km, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Apiaries_within_25km)

# Replace 'Y' with 1 and 'N' with 0 in the 'Q19_treated_for_Varroa_prev_year' `
combined_bee_data$Q19_treated_for_Varroa_prev_year <- ifelse(combined_bee_data$Q19_treated_for_Varroa_prev_year == 'Y', 1, 0)



# Model for Q19_treated_for_Varroa_prev_year
model_Q19_treated_for_Varroa_prev_year <- gam(Total_pesticides_pollen_over_LOQ ~ s(Q19_treated_for_Varroa_prev_year, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Q19_treated_for_Varroa_prev_year)

# Model for Min_Dist_Urban_metre
model_Min_Dist_Urban_metre <- gam(Total_pesticides_pollen_over_LOQ ~ s(Min_Dist_Urban_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Urban_metre)

# Model for Min_Dist_Road_metre
model_Min_Dist_Road_metre <- gam(Total_pesticides_pollen_over_LOQ ~ s(Min_Dist_Road_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Road_metre)



#Total_pesticides_pollen_over_MRL

# Longitude and Latitude
model1 <- gam(Total_pesticides_pollen_over_MRL ~ s(Longitude, Latitude, k = 20), 
              family = poisson(link = "log"), data = combined_bee_data)
summary(model1)
plot(model1)



# Model for County (treated as a factor) 
model_County <- gam(Total_pesticides_pollen_over_MRL ~ County, data = combined_bee_data, family = poisson(link = "log")) 
summary(model_County)

# Model for Month_sampled (treated as a factor)

model_Month_sampled <- gam(Total_pesticides_pollen_over_MRL ~ Month_sampled, data = combined_bee_data, family = poisson(link = "log"))

summary(model_Month_sampled)


# Model for Date_sampled (treated as a factor)
model_Date_sampled <- gam(Total_pesticides_pollen_over_MRL ~ Date_sampled, data = combined_bee_data, family = poisson(link = "log"))
summary(model_Date_sampled)

# Model for Days_in_minus_20_freezer
model_Days_in_minus_20_freezer <- gam(Total_pesticides_pollen_over_MRL ~ s(Days_in_minus_20_freezer, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Days_in_minus_20_freezer)

# Replace 'Y' with 1 and 'N' with 0 in the '17_treated_for_Varroa_prev_year_after' column combined_bee_data$Q17_treated_for_Varroa_prev_year_after` <- ifelse(combined_bee_data$`17_treated_for_Varroa_prev_year_after` == 'Y', 1, 0)
combined_bee_data$Q17_treated_for_Varroa_prev_year_after <- ifelse(combined_bee_data$Q17_treated_for_Varroa_prev_year_after == 'Y', 1, 0)


# Model for Q17_treated_for_Varroa_prev_year_after
model_Q17_treated_for_Varroa_prev_year_after <- gam(Total_pesticides_pollen_over_MRL ~ s(Q17_treated_for_Varroa_prev_year_after, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Q17_treated_for_Varroa_prev_year_after)

# Model for Prop_Cultivated_Land
model_Prop_Cultivated_Land <- gam(Total_pesticides_pollen_over_MRL ~ s(Prop_Cultivated_Land, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Prop_Cultivated_Land)

# Model for Prop_Water_Bodies
model_Prop_Water_Bodies <- gam(Total_pesticides_pollen_over_MRL ~ s(Prop_Water_Bodies, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Prop_Water_Bodies)

# Model for Min_Dist_Water_metre
model_Min_Dist_Water_metre <- gam(Total_pesticides_pollen_over_MRL ~ s(Min_Dist_Water_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Water_metre)

# Model for Min_Dist_Coast_metre
model_Min_Dist_Coast_metre <- gam(Total_pesticides_pollen_over_MRL ~ s(Min_Dist_Coast_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Coast_metre)



# Model for Total_Rainfall_mm
model_Total_Rainfall_mm <- gam(Total_pesticides_pollen_over_MRL ~ s(Total_Rainfall_mm, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Rainfall_mm)

# Model for Mean_Dry_Bulb_Temperature_degC
model_Mean_Dry_Bulb_Temperature_degC <- gam(Total_pesticides_pollen_over_MRL ~ s(Mean_Dry_Bulb_Temperature_degC, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Mean_Dry_Bulb_Temperature_degC)

# Model for Mean_10cm_Soil_Temperature_degC
model_Mean_10cm_Soil_Temperature_degC <- gam(Total_pesticides_pollen_over_MRL ~ s(Mean_10cm_Soil_Temperature_degC, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Mean_10cm_Soil_Temperature_degC)

# Model for Total_Global_Solar_Radiation_MJ_per_m2
model_Total_Global_Solar_Radiation_MJ_per_m2 <- gam(Total_pesticides_pollen_over_MRL ~ s(Total_Global_Solar_Radiation_MJ_per_m2, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Global_Solar_Radiation_MJ_per_m2)

# Model for Total_Potential_Evaporation_mm
model_Total_Potential_Evaporation_mm <- gam(Total_pesticides_pollen_over_MRL ~ s(Total_Potential_Evaporation_mm, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Potential_Evaporation_mm)

# Model for Total_Evaporation_mm
model_Total_Evaporation_mm <- gam(Total_pesticides_pollen_over_MRL ~ s(Total_Evaporation_mm, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Evaporation_mm)

# Model for Degree_Days_Below_15_5_degC
model_Degree_Days_Below_15_5_degC <- gam(Total_pesticides_pollen_over_MRL ~ s(Degree_Days_Below_15_5_degC, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Degree_Days_Below_15_5_degC)

# Model for Prop_Fert_Improved.Grassland
model_Prop_Fert_Improved_Grassland <- gam(Total_pesticides_pollen_over_MRL ~ s(Prop_Fert_Improved.Grassland, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Prop_Fert_Improved_Grassland)

# Model for Elevation
model_Elevation <- gam(Total_pesticides_pollen_over_MRL ~ s(Elevation, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Elevation)

# Model for Apiaries_within_5km
model_Apiaries_within_5km <- gam(Total_pesticides_pollen_over_MRL ~ s(Apiaries_within_5km, k = 7), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Apiaries_within_5km)

# Model for Apiaries_within_10km
model_Apiaries_within_10km <- gam(Total_pesticides_pollen_over_MRL ~ s(Apiaries_within_10km, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Apiaries_within_10km)

# Model for Apiaries_within_25km
model_Apiaries_within_25km <- gam(Total_pesticides_pollen_over_MRL ~ s(Apiaries_within_25km, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Apiaries_within_25km)

# Replace 'Y' with 1 and 'N' with 0 in the 'Q19_treated_for_Varroa_prev_year' `
combined_bee_data$Q19_treated_for_Varroa_prev_year <- ifelse(combined_bee_data$Q19_treated_for_Varroa_prev_year == 'Y', 1, 0)



# Model for Q19_treated_for_Varroa_prev_year
model_Q19_treated_for_Varroa_prev_year <- gam(Total_pesticides_pollen_over_MRL ~ s(Q19_treated_for_Varroa_prev_year, k = 1), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Q19_treated_for_Varroa_prev_year)

# Model for Min_Dist_Urban_metre
model_Min_Dist_Urban_metre <- gam(Total_pesticides_pollen_over_MRL ~ s(Min_Dist_Urban_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Urban_metre)

# Model for Min_Dist_Road_metre
model_Min_Dist_Road_metre <- gam(Total_pesticides_pollen_over_MRL ~ s(Min_Dist_Road_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Road_metre)





#Total_pesticides_honey_over_LOQ
# Longitude and Latitude
model1 <- gam(Total_pesticides_honey_over_LOQ ~ s(Longitude, Latitude, k = 20), 
              family = poisson(link = "log"), data = combined_bee_data)
summary(model1)
plot(model1)



# Model for County (treated as a factor) 
model_County <- gam(Total_pesticides_honey_over_LOQ ~ County, data = combined_bee_data, family = poisson(link = "log")) 
summary(model_County)

# Model for Month_sampled (treated as a factor)

model_Month_sampled <- gam(Total_pesticides_honey_over_LOQ ~ Month_sampled, data = combined_bee_data, family = poisson(link = "log"))

summary(model_Month_sampled)


# Model for Date_sampled (treated as a factor)
model_Date_sampled <- gam(Total_pesticides_honey_over_LOQ ~ Date_sampled, data = combined_bee_data, family = poisson(link = "log"))
summary(model_Date_sampled)

# Model for Days_in_minus_20_freezer
model_Days_in_minus_20_freezer <- gam(Total_pesticides_honey_over_LOQ ~ s(Days_in_minus_20_freezer, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Days_in_minus_20_freezer)

# Replace 'Y' with 1 and 'N' with 0 in the '17_treated_for_Varroa_prev_year_after' column combined_bee_data$Q17_treated_for_Varroa_prev_year_after` <- ifelse(combined_bee_data$`17_treated_for_Varroa_prev_year_after` == 'Y', 1, 0)
combined_bee_data$Q17_treated_for_Varroa_prev_year_after <- ifelse(combined_bee_data$Q17_treated_for_Varroa_prev_year_after == 'Y', 1, 0)


# Model for Q17_treated_for_Varroa_prev_year_after
model_Q17_treated_for_Varroa_prev_year_after <- gam(Total_pesticides_honey_over_LOQ ~ s(Q17_treated_for_Varroa_prev_year_after, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Q17_treated_for_Varroa_prev_year_after)

# Model for Prop_Cultivated_Land
model_Prop_Cultivated_Land <- gam(Total_pesticides_honey_over_LOQ ~ s(Prop_Cultivated_Land, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Prop_Cultivated_Land)

# Model for Prop_Water_Bodies
model_Prop_Water_Bodies <- gam(Total_pesticides_honey_over_LOQ ~ s(Prop_Water_Bodies, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Prop_Water_Bodies)

# Model for Min_Dist_Water_metre
model_Min_Dist_Water_metre <- gam(Total_pesticides_honey_over_LOQ ~ s(Min_Dist_Water_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Water_metre)

# Model for Min_Dist_Coast_metre
model_Min_Dist_Coast_metre <- gam(Total_pesticides_honey_over_LOQ ~ s(Min_Dist_Coast_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Coast_metre)



# Model for Total_Rainfall_mm
model_Total_Rainfall_mm <- gam(Total_pesticides_honey_over_LOQ ~ s(Total_Rainfall_mm, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Rainfall_mm)

# Model for Mean_Dry_Bulb_Temperature_degC
model_Mean_Dry_Bulb_Temperature_degC <- gam(Total_pesticides_honey_over_LOQ ~ s(Mean_Dry_Bulb_Temperature_degC, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Mean_Dry_Bulb_Temperature_degC)

# Model for Mean_10cm_Soil_Temperature_degC
model_Mean_10cm_Soil_Temperature_degC <- gam(Total_pesticides_honey_over_LOQ ~ s(Mean_10cm_Soil_Temperature_degC, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Mean_10cm_Soil_Temperature_degC)

# Model for Total_Global_Solar_Radiation_MJ_per_m2
model_Total_Global_Solar_Radiation_MJ_per_m2 <- gam(Total_pesticides_honey_over_LOQ ~ s(Total_Global_Solar_Radiation_MJ_per_m2, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Global_Solar_Radiation_MJ_per_m2)

# Model for Total_Potential_Evaporation_mm
model_Total_Potential_Evaporation_mm <- gam(Total_pesticides_honey_over_LOQ ~ s(Total_Potential_Evaporation_mm, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Potential_Evaporation_mm)

# Model for Total_Evaporation_mm
model_Total_Evaporation_mm <- gam(Total_pesticides_honey_over_LOQ ~ s(Total_Evaporation_mm, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Evaporation_mm)

# Model for Degree_Days_Below_15_5_degC
model_Degree_Days_Below_15_5_degC <- gam(Total_pesticides_honey_over_LOQ ~ s(Degree_Days_Below_15_5_degC, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Degree_Days_Below_15_5_degC)

# Model for Prop_Fert_Improved.Grassland
model_Prop_Fert_Improved_Grassland <- gam(Total_pesticides_honey_over_LOQ ~ s(Prop_Fert_Improved.Grassland, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Prop_Fert_Improved_Grassland)

# Model for Elevation
model_Elevation <- gam(Total_pesticides_honey_over_LOQ ~ s(Elevation, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Elevation)

# Model for Apiaries_within_5km
model_Apiaries_within_5km <- gam(Total_pesticides_honey_over_LOQ ~ s(Apiaries_within_5km, k = 7), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Apiaries_within_5km)

# Model for Apiaries_within_10km
model_Apiaries_within_10km <- gam(Total_pesticides_honey_over_LOQ ~ s(Apiaries_within_10km, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Apiaries_within_10km)

# Model for Apiaries_within_25km
model_Apiaries_within_25km <- gam(Total_pesticides_honey_over_LOQ ~ s(Apiaries_within_25km, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Apiaries_within_25km)

# Replace 'Y' with 1 and 'N' with 0 in the 'Q19_treated_for_Varroa_prev_year' `
combined_bee_data$Q19_treated_for_Varroa_prev_year <- ifelse(combined_bee_data$Q19_treated_for_Varroa_prev_year == 'Y', 1, 0)



# Model for Q19_treated_for_Varroa_prev_year
model_Q19_treated_for_Varroa_prev_year <- gam(Total_pesticides_honey_over_LOQ ~ s(Q19_treated_for_Varroa_prev_year, k = 1), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Q19_treated_for_Varroa_prev_year)

# Model for Min_Dist_Urban_metre
model_Min_Dist_Urban_metre <- gam(Total_pesticides_honey_over_LOQ ~ s(Min_Dist_Urban_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Urban_metre)

# Model for Min_Dist_Road_metre
model_Min_Dist_Road_metre <- gam(Total_pesticides_honey_over_LOQ ~ s(Min_Dist_Road_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Road_metre)




#Total_pesticides_honey_over_MRL

# Longitude and Latitude
model1 <- gam(Total_pesticides_honey_over_MRL ~ s(Longitude, Latitude, k = 20), 
              family = poisson(link = "log"), data = combined_bee_data)
summary(model1)
plot(model1)



# Model for County (treated as a factor) 
model_County <- gam(Total_pesticides_honey_over_MRL ~ County, data = combined_bee_data, family = poisson(link = "log")) 
summary(model_County)

# Model for Month_sampled (treated as a factor)

model_Month_sampled <- gam(Total_pesticides_honey_over_MRL ~ Month_sampled, data = combined_bee_data, family = poisson(link = "log"))

summary(model_Month_sampled)


# Model for Date_sampled (treated as a factor)
model_Date_sampled <- gam(Total_pesticides_honey_over_MRL ~ Date_sampled, data = combined_bee_data, family = poisson(link = "log"))
summary(model_Date_sampled)

# Model for Days_in_minus_20_freezer
model_Days_in_minus_20_freezer <- gam(Total_pesticides_honey_over_MRL ~ s(Days_in_minus_20_freezer, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Days_in_minus_20_freezer)

# Replace 'Y' with 1 and 'N' with 0 in the '17_treated_for_Varroa_prev_year_after' column combined_bee_data$Q17_treated_for_Varroa_prev_year_after` <- ifelse(combined_bee_data$`17_treated_for_Varroa_prev_year_after` == 'Y', 1, 0)
combined_bee_data$Q17_treated_for_Varroa_prev_year_after <- ifelse(combined_bee_data$Q17_treated_for_Varroa_prev_year_after == 'Y', 1, 0)


# Model for Q17_treated_for_Varroa_prev_year_after
model_Q17_treated_for_Varroa_prev_year_after <- gam(Total_pesticides_honey_over_MRL ~ s(Q17_treated_for_Varroa_prev_year_after, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Q17_treated_for_Varroa_prev_year_after)

# Model for Prop_Cultivated_Land
model_Prop_Cultivated_Land <- gam(Total_pesticides_honey_over_MRL ~ s(Prop_Cultivated_Land, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Prop_Cultivated_Land)

# Model for Prop_Water_Bodies
model_Prop_Water_Bodies <- gam(Total_pesticides_honey_over_MRL ~ s(Prop_Water_Bodies, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Prop_Water_Bodies)

# Model for Min_Dist_Water_metre
model_Min_Dist_Water_metre <- gam(Total_pesticides_honey_over_MRL ~ s(Min_Dist_Water_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Water_metre)

# Model for Min_Dist_Coast_metre
model_Min_Dist_Coast_metre <- gam(Total_pesticides_honey_over_MRL ~ s(Min_Dist_Coast_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Coast_metre)



# Model for Total_Rainfall_mm
model_Total_Rainfall_mm <- gam(Total_pesticides_honey_over_MRL ~ s(Total_Rainfall_mm, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Rainfall_mm)

# Model for Mean_Dry_Bulb_Temperature_degC
model_Mean_Dry_Bulb_Temperature_degC <- gam(Total_pesticides_honey_over_MRL ~ s(Mean_Dry_Bulb_Temperature_degC, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Mean_Dry_Bulb_Temperature_degC)

# Model for Mean_10cm_Soil_Temperature_degC
model_Mean_10cm_Soil_Temperature_degC <- gam(Total_pesticides_honey_over_MRL ~ s(Mean_10cm_Soil_Temperature_degC, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Mean_10cm_Soil_Temperature_degC)

# Model for Total_Global_Solar_Radiation_MJ_per_m2
model_Total_Global_Solar_Radiation_MJ_per_m2 <- gam(Total_pesticides_honey_over_MRL ~ s(Total_Global_Solar_Radiation_MJ_per_m2, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Global_Solar_Radiation_MJ_per_m2)

# Model for Total_Potential_Evaporation_mm
model_Total_Potential_Evaporation_mm <- gam(Total_pesticides_honey_over_MRL ~ s(Total_Potential_Evaporation_mm, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Potential_Evaporation_mm)

# Model for Total_Evaporation_mm
model_Total_Evaporation_mm <- gam(Total_pesticides_honey_over_MRL ~ s(Total_Evaporation_mm, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Evaporation_mm)

# Model for Degree_Days_Below_15_5_degC
model_Degree_Days_Below_15_5_degC <- gam(Total_pesticides_honey_over_MRL ~ s(Degree_Days_Below_15_5_degC, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Degree_Days_Below_15_5_degC)

# Model for Prop_Fert_Improved.Grassland
model_Prop_Fert_Improved_Grassland <- gam(Total_pesticides_honey_over_MRL ~ s(Prop_Fert_Improved.Grassland, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Prop_Fert_Improved_Grassland)

# Model for Elevation
model_Elevation <- gam(Total_pesticides_honey_over_MRL ~ s(Elevation, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Elevation)

# Model for Apiaries_within_5km
model_Apiaries_within_5km <- gam(Total_pesticides_honey_over_MRL ~ s(Apiaries_within_5km, k = 7), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Apiaries_within_5km)

# Model for Apiaries_within_10km
model_Apiaries_within_10km <- gam(Total_pesticides_honey_over_MRL ~ s(Apiaries_within_10km, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Apiaries_within_10km)

# Model for Apiaries_within_25km
model_Apiaries_within_25km <- gam(Total_pesticides_honey_over_MRL ~ s(Apiaries_within_25km, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Apiaries_within_25km)

# Replace 'Y' with 1 and 'N' with 0 in the 'Q19_treated_for_Varroa_prev_year' `
combined_bee_data$Q19_treated_for_Varroa_prev_year <- ifelse(combined_bee_data$Q19_treated_for_Varroa_prev_year == 'Y', 1, 0)



# Model for Q19_treated_for_Varroa_prev_year
model_Q19_treated_for_Varroa_prev_year <- gam(Total_pesticides_honey_over_MRL ~ s(Q19_treated_for_Varroa_prev_year, k = 1), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Q19_treated_for_Varroa_prev_year)

# Model for Min_Dist_Urban_metre
model_Min_Dist_Urban_metre <- gam(Total_pesticides_honey_over_MRL ~ s(Min_Dist_Urban_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Urban_metre)

# Model for Min_Dist_Road_metre
model_Min_Dist_Road_metre <- gam(Total_pesticides_honey_over_MRL ~ s(Min_Dist_Road_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Road_metre)


##no._pathogens_workers
# Longitude and Latitude
model1 <- gam(no._pathogens_workers~ s(Longitude, Latitude, k = 20), 
              family = poisson(link = "log"), data = combined_bee_data)
summary(model1)
plot(model1)



# Model for County (treated as a factor) 
model_County <- gam(no._pathogens_workers~ County, data = combined_bee_data, family = poisson(link = "log")) 
summary(model_County)

# Model for Month_sampled (treated as a factor)

model_Month_sampled <- gam(no._pathogens_workers~ Month_sampled, data = combined_bee_data, family = poisson(link = "log"))

summary(model_Month_sampled)


# Model for Date_sampled (treated as a factor)
model_Date_sampled <- gam(no._pathogens_workers~ Date_sampled, data = combined_bee_data, family = poisson(link = "log"))
summary(model_Date_sampled)

# Model for Days_in_minus_20_freezer
model_Days_in_minus_20_freezer <- gam(no._pathogens_workers~ s(Days_in_minus_20_freezer, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Days_in_minus_20_freezer)

# Replace 'Y' with 1 and 'N' with 0 in the '17_treated_for_Varroa_prev_year_after' column combined_bee_data$Q17_treated_for_Varroa_prev_year_after` <- ifelse(combined_bee_data$`17_treated_for_Varroa_prev_year_after` == 'Y', 1, 0)
combined_bee_data$Q17_treated_for_Varroa_prev_year_after <- ifelse(combined_bee_data$Q17_treated_for_Varroa_prev_year_after == 'Y', 1, 0)


# Model for Q17_treated_for_Varroa_prev_year_after
model_Q17_treated_for_Varroa_prev_year_after <- gam(no._pathogens_workers~ s(Q17_treated_for_Varroa_prev_year_after, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Q17_treated_for_Varroa_prev_year_after)

# Model for Prop_Cultivated_Land
model_Prop_Cultivated_Land <- gam(no._pathogens_workers~ s(Prop_Cultivated_Land, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Prop_Cultivated_Land)

# Model for Prop_Water_Bodies
model_Prop_Water_Bodies <- gam(no._pathogens_workers~ s(Prop_Water_Bodies, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Prop_Water_Bodies)

# Model for Min_Dist_Water_metre
model_Min_Dist_Water_metre <- gam(no._pathogens_workers~ s(Min_Dist_Water_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Water_metre)

# Model for Min_Dist_Coast_metre
model_Min_Dist_Coast_metre <- gam(no._pathogens_workers~ s(Min_Dist_Coast_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Coast_metre)



# Model for Total_Rainfall_mm
model_Total_Rainfall_mm <- gam(no._pathogens_workers~ s(Total_Rainfall_mm, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Rainfall_mm)

# Model for Mean_Dry_Bulb_Temperature_degC
model_Mean_Dry_Bulb_Temperature_degC <- gam(no._pathogens_workers~ s(Mean_Dry_Bulb_Temperature_degC, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Mean_Dry_Bulb_Temperature_degC)

# Model for Mean_10cm_Soil_Temperature_degC
model_Mean_10cm_Soil_Temperature_degC <- gam(no._pathogens_workers~ s(Mean_10cm_Soil_Temperature_degC, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Mean_10cm_Soil_Temperature_degC)

# Model for Total_Global_Solar_Radiation_MJ_per_m2
model_Total_Global_Solar_Radiation_MJ_per_m2 <- gam(no._pathogens_workers~ s(Total_Global_Solar_Radiation_MJ_per_m2, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Global_Solar_Radiation_MJ_per_m2)

# Model for Total_Potential_Evaporation_mm
model_Total_Potential_Evaporation_mm <- gam(no._pathogens_workers~ s(Total_Potential_Evaporation_mm, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Potential_Evaporation_mm)

# Model for Total_Evaporation_mm
model_Total_Evaporation_mm <- gam(no._pathogens_workers~ s(Total_Evaporation_mm, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Total_Evaporation_mm)

# Model for Degree_Days_Below_15_5_degC
model_Degree_Days_Below_15_5_degC <- gam(no._pathogens_workers~ s(Degree_Days_Below_15_5_degC, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Degree_Days_Below_15_5_degC)

# Model for Prop_Fert_Improved.Grassland
model_Prop_Fert_Improved_Grassland <- gam(no._pathogens_workers~ s(Prop_Fert_Improved.Grassland, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Prop_Fert_Improved_Grassland)

# Model for Elevation
model_Elevation <- gam(no._pathogens_workers~ s(Elevation, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Elevation)

# Model for Apiaries_within_5km
model_Apiaries_within_5km <- gam(no._pathogens_workers~ s(Apiaries_within_5km, k = 7), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Apiaries_within_5km)

# Model for Apiaries_within_10km
model_Apiaries_within_10km <- gam(no._pathogens_workers~ s(Apiaries_within_10km, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Apiaries_within_10km)

# Model for Apiaries_within_25km
model_Apiaries_within_25km <- gam(no._pathogens_workers~ s(Apiaries_within_25km, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Apiaries_within_25km)

# Replace 'Y' with 1 and 'N' with 0 in the 'Q19_treated_for_Varroa_prev_year' `
combined_bee_data$Q19_treated_for_Varroa_prev_year <- ifelse(combined_bee_data$Q19_treated_for_Varroa_prev_year == 'Y', 1, 0)



# Model for Q19_treated_for_Varroa_prev_year
model_Q19_treated_for_Varroa_prev_year <- gam(no._pathogens_workers~ s(Q19_treated_for_Varroa_prev_year, k = 1), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Q19_treated_for_Varroa_prev_year)

# Model for Min_Dist_Urban_metre
model_Min_Dist_Urban_metre <- gam(no._pathogens_workers~ s(Min_Dist_Urban_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Urban_metre)

# Model for Min_Dist_Road_metre
model_Min_Dist_Road_metre <- gam(no._pathogens_workers~ s(Min_Dist_Road_metre, k = 10), data = combined_bee_data, family = poisson(link = "log"))
summary(model_Min_Dist_Road_metre)



#8 Aug


# Convert categorical variables to factors
combined_bee_data$County <- as.factor(combined_bee_data$County)
combined_bee_data$Month_sampled <- as.factor(combined_bee_data$Month_sampled)
combined_bee_data$Date_sampled <- as.factor(combined_bee_data$Date_sampled)
combined_bee_data$Q2_No._apiaries_after <- as.factor(combined_bee_data$Q2_No._apiaries_after)
combined_bee_data$Q5_No._queen_problems_after <- as.factor(combined_bee_data$Q5_No._queen_problems_after)
combined_bee_data$Q6_No._natural_disaster_after <- as.factor(combined_bee_data$Q6_No._natural_disaster_after)
combined_bee_data$Q8c_signs_of_starvation_after <- as.factor(combined_bee_data$Q8c_signs_of_starvation_after)
combined_bee_data$Q17_treated_for_Varroa_prev_year_after <- as.factor(combined_bee_data$Q17_treated_for_Varroa_prev_year_after)
combined_bee_data$Q19_treated_for_Varroa_prev_year <- as.factor(combined_bee_data$Q19_treated_for_Varroa_prev_year)
combined_bee_data$Site_Id <- as.factor(combined_bee_data$Site_Id)
combined_bee_data$Soil_drainage <- as.factor(combined_bee_data$Soil_drainage)
combined_bee_data$Soil_texture <- as.factor(combined_bee_data$Soil_texture)
combined_bee_data$Soil_description <- as.factor(combined_bee_data$Soil_description)
combined_bee_data$Proximity_to_Pollutant <- as.factor(combined_bee_data$Proximity_to_Pollutant)
combined_bee_data$Nearest_Pollutant <- as.factor(combined_bee_data$Nearest_Pollutant)

# Check the number of levels for each factor variable
for (var in categorical_vars) {
  print(paste(var, "levels:"))
  print(length(levels(as.factor(combined_bee_data[[var]]))))
}

# Fit a model with all alr_* variables

alr_model <- gam(Total_pesticides_pollen_over_LOQ ~ s(Latitude, Longitude) + 
                   alr_Amenity.Grassland + 
                   alr_Broadleaved.Forest.and.Woodland + 
                   alr_Buildings + 
                   alr_Dry.Grassland + 
                   alr_Dry.Heath + 
                   alr_Hedgerows + 
                   alr_Improved.Grassland + 
                   alr_Lakes.and.Ponds + 
                   alr_Mixed.Forest + 
                   alr_Other.Artificial.Surfaces + 
                   alr_Rivers.and.Streams + 
                   alr_Scrub + 
                   alr_Treelines + 
                   alr_Ways + 
                   alr_Wet.Grassland + 
                   alr_Artificial.Waterbodies + 
                   alr_Cultivated.Land + 
                   alr_Coniferous.Forest + 
                   alr_Transitional.Forest + 
                   alr_Bare.Soil.and.Disturbed.Ground + 
                   alr_Blanket.Bog + 
                   alr_Cutover.Bog + 
                   alr_Exposed.Rock.and.Sediments + 
                   alr_Fens + 
                   alr_Wet.Heath + 
                   alr_Bracken + 
                   alr_Swamp + 
                   alr_Bare.Peat + 
                   alr_Raised.Bog + 
                   alr_Marine.Water + 
                   alr_Mudflats + 
                   alr_Transitional.Waterbodies + 
                   alr_Saltmarsh + 
                   alr_Coastal.Sediments + 
                   alr_Sand.Dunes, 
                 data = combined_bee_data, family = poisson(link = "log"))


summary(alr_model)


#Fit a model for each alr_* landscape variable separately with the smooth term 

# Function to fit and summarize models for each alr_* variable
fit_and_summarize <- function(response, alr_var) {
  model <- gam(as.formula(paste(response, "~ s(Latitude, Longitude, k=50) +", alr_var)),
               data = combined_bee_data, family = poisson(link = "log"))
  summary(model)
}

# List of alr_* variables
alr_vars <- c("alr_Amenity.Grassland", "alr_Broadleaved.Forest.and.Woodland", "alr_Buildings", 
              "alr_Dry.Grassland", "alr_Dry.Heath", "alr_Hedgerows", "alr_Improved.Grassland", 
              "alr_Lakes.and.Ponds", "alr_Mixed.Forest", "alr_Other.Artificial.Surfaces", 
              "alr_Rivers.and.Streams", "alr_Scrub", "alr_Treelines", "alr_Ways", 
              "alr_Wet.Grassland", "alr_Artificial.Waterbodies", "alr_Cultivated.Land", 
              "alr_Coniferous.Forest", "alr_Transitional.Forest", "alr_Bare.Soil.and.Disturbed.Ground", 
              "alr_Blanket.Bog", "alr_Cutover.Bog", "alr_Exposed.Rock.and.Sediments", "alr_Fens", 
              "alr_Wet.Heath", "alr_Bracken", "alr_Swamp", "alr_Bare.Peat", "alr_Raised.Bog", 
              "alr_Marine.Water", "alr_Mudflats", "alr_Transitional.Waterbodies", "alr_Saltmarsh", 
              "alr_Coastal.Sediments", "alr_Sand.Dunes")

# Fit and summarize models for each alr_* variable
for (alr_var in alr_vars) {
  cat("Model for:", alr_var, "\n")
  print(fit_and_summarize("Total_pesticides_honey_over_MRL", alr_var))
  cat("\n\n")
}


alr_Cultivated.Land_alr_model <- gam(Total_pesticides_pollen_over_LOQ ~ s(Latitude, Longitude, k=50) +
                                       alr_Cultivated.Land,
                                     data = combined_bee_data, family = poisson(link = "log"))


summary(alr_Cultivated.Land_alr_model)




Coniferous.Forest_alr_model <- gam(Total_pesticides_pollen_over_LOQ ~ s(Latitude, Longitude) +
                                     alr_Coniferous.Forest,
                                   data = combined_bee_data, family = poisson(link = "log"))


summary(Coniferous.Forest_alr_model)

Improved.Grassland_alr_model <- gam(Total_pesticides_pollen_over_LOQ ~ s(Latitude, Longitude) +
                                      alr_Improved.Grassland,
                                    data = combined_bee_data, family = poisson(link = "log"))


summary(Improved.Grassland_alr_model)



# Fit the model with valid variables

full_model <- gam(Total_pesticides_pollen_over_LOQ ~ s(Latitude, Longitude, k = 50) +
                    alr_Cultivated.Land +
                    County + Month_sampled + Date_sampled + Days_in_minus_20_freezer +
                    No._colonies_sampled + Q2_No._apiaries_after + Q5_No._queen_problems_after +
                    Q6_No._natural_disaster_after + Q8c_signs_of_starvation_after +
                    Q17_treated_for_Varroa_prev_year_after + Prop_Cultivated_Land +
                    Prop_Water_Bodies + Min_Dist_Water_metre + Min_Dist_Coast_metre +
                    Closest_Station + Distance + Total_Rainfall_mm + Mean_Dry_Bulb_Temperature_degC +
                    Mean_10cm_Soil_Temperature_degC + Total_Global_Solar_Radiation_MJ_per_m2 +
                    Total_Potential_Evaporation_mm + Total_Evaporation_mm +
                    Degree_Days_Below_15_5_degC + Prop_Fert_Improved.Grassland +
                    Elevation + Apiaries_within_5km + Apiaries_within_10km + 
                    Apiaries_within_25km + Q19_treated_for_Varroa_prev_year + 
                    Min_Dist_Urban_metre + Min_Dist_Road_metre + Site_Id + 
                    Soil_drainage + Soil_texture + Soil_description + 
                    Proximity_to_Pollutant + Nearest_Pollutant,
                  data = combined_bee_data, family = poisson(link = "log"))

# Summarize the model
summary(full_model)

#12 August

#Response: Total_pesticides_pollen_over_LOQ

# Simple model
simple_model <- gam(Total_pesticides_pollen_over_LOQ ~ s(Latitude, Longitude, k = 50),
                    data = combined_bee_data, family = poisson(link = "log"))
summary(simple_model)

# Add variables incrementally
simple_model <- update(simple_model, . ~ . + alr_Cultivated.Land)
summary(simple_model)

simple_model <- update(simple_model, . ~ . + County)
summary(simple_model)

simple_model <- update(simple_model, . ~ . + Month_sampled)
summary(simple_model)
simple_model <- update(simple_model, . ~ . + Degree_Days_Below_15_5_degC)
summary(simple_model)

#County and Month_sampled variables do not add meaningful information to the model 

simple_model <- gam(Total_pesticides_pollen_over_LOQ ~ s(Latitude, Longitude) +
                      alr_Cultivated.Land,
                    data = combined_bee_data, family = poisson(link = "log"))


summary(simple_model)

model1<-gam(Total_pesticides_pollen_over_LOQ ~ s(Latitude, Longitude, k=50) +
              alr_Cultivated.Land + Prop_Fert_Improved.Grassland + Soil_drainage,
            data = combined_bee_data, family = poisson(link = "log"))
summary(model1)

#model diagnostics
par(mfrow = c(2, 2))

# residuals vs fitted
plot(model1, residuals = TRUE, pch = 16, main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# QQ Plot
qqnorm(residuals(model1), main = "Normal Q-Q Plot")
qqline(residuals(model1), col = "red")

# Scale-Location Plot
plot(model1, scale = TRUE, main = "Scale-Location Plot")

# Cook's Distance Plot
plot(model1, which = 4, main = "Cook's Distance Plot")

#Response: Total_pesticides_pollen_over_MRL


# Simple model
simple_model <- gam(Total_pesticides_pollen_over_MRL ~ s(Latitude, Longitude, k = 50),
                    data = combined_bee_data, family = poisson(link = "log"))
summary(simple_model)

model1<-gam(Total_pesticides_pollen_over_MRL ~ s(Latitude, Longitude, k=50) +
              alr_Bare.Soil.and.Disturbed.Ground,
            data = combined_bee_data, family = poisson(link = "log"))

summary(model1)

# residuals vs fitted
plot(model1, residuals = TRUE, pch = 16, main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# QQ Plot
qqnorm(residuals(model1), main = "Normal Q-Q Plot")
qqline(residuals(model1), col = "red")

# Scale-Location Plot
plot(model1, scale = TRUE, main = "Scale-Location Plot")

# Cook's Distance Plot
plot(model1, which = 4, main = "Cook's Distance Plot")



#Response: Total_pesticides_honey_over_LOQ
simple_model <- gam(Total_pesticides_honey_over_LOQ ~ s(Latitude, Longitude, k = 50),
                    data = combined_bee_data, family = poisson(link = "log"))
summary(simple_model)

model <- gam(Total_pesticides_honey_over_LOQ ~ s(Latitude, Longitude, k = 50) +
               Prop_Fert_Improved.Grassland,
             data = combined_bee_data, family = poisson(link = "log"))
summary(model)

full_gam_model <- gam(Total_pesticides_pollen_over_MRL ~ s(Latitude) + s(Longitude) + 
                        Days_in_minus_20_freezer + No._colonies_sampled + 
                        s(Min_Dist_Water_metre) + s(Min_Dist_Coast_metre) +
                        Total_Rainfall_mm + Mean_Dry_Bulb_Temperature_degC +
                        Min_Dist_Urban_metre + Min_Dist_Road_metre +
                        factor(County) + factor(Month_sampled) + factor(Soil_drainage) + factor(Soil_texture),
                      data = combined_bee_data, family = poisson(link = "log"), method = "REML")



#Response: Total_pesticides_honey_over_MRL

simple_model <- gam(Total_pesticides_honey_over_MRL ~ s(Latitude, Longitude, k = 50),
                    data = combined_bee_data, family = poisson(link = "log"))
summary(simple_model)

model <- gam(Total_pesticides_honey_over_MRL ~ s(Latitude, Longitude, k = 50) +
               Total_Rainfall_mm,
             data = combined_bee_data, family = poisson(link = "log"))
summary(model)

#Response: Pathogens
simple_model <- gam(no._pathogens_workers ~ s(Latitude, Longitude, k = 50),
                    data = combined_bee_data, family = poisson(link = "log"))
summary(simple_model)

model1<-gam(no._pathogens_workers ~ s(Latitude, Longitude, k=50) +
              alr_Cultivated.Land + Min_Dist_Road_metre,
            data = combined_bee_data, family = poisson(link = "log"))
summary(model1)

# residuals vs fitted
plot(model1, residuals = TRUE, pch = 16, main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# QQ Plot
qqnorm(residuals(model1), main = "Normal Q-Q Plot")
qqline(residuals(model1), col = "red")

# Scale-Location Plot
plot(model1, scale = TRUE, main = "Scale-Location Plot")

# Cook's Distance Plot
plot(model1, which = 4, main = "Cook's Distance Plot")


#summary stats

# List of all landscape variables
landscape_vars <- c("alr_Amenity.Grassland", "alr_Broadleaved.Forest.and.Woodland", "alr_Buildings", 
                    "alr_Dry.Grassland", "alr_Dry.Heath", "alr_Hedgerows", "alr_Improved.Grassland", 
                    "alr_Lakes.and.Ponds", "alr_Mixed.Forest", "alr_Other.Artificial.Surfaces", 
                    "alr_Rivers.and.Streams", "alr_Scrub", "alr_Treelines", "alr_Ways", 
                    "alr_Wet.Grassland", "alr_Artificial.Waterbodies", "alr_Cultivated.Land", 
                    "alr_Coniferous.Forest", "alr_Transitional.Forest", "alr_Bare.Soil.and.Disturbed.Ground", 
                    "alr_Blanket.Bog", "alr_Cutover.Bog", "alr_Exposed.Rock.and.Sediments", "alr_Fens", 
                    "alr_Wet.Heath", "alr_Bracken", "alr_Swamp", "alr_Bare.Peat", "alr_Raised.Bog", 
                    "alr_Marine.Water", "alr_Mudflats", "alr_Transitional.Waterbodies", "alr_Saltmarsh", 
                    "alr_Coastal.Sediments", "alr_Sand.Dunes")

# Calculate summary statistics for each landscape variable
landscape_summary <- sapply(combined_bee_data[landscape_vars], function(x) {
  c(Average = mean(x, na.rm = TRUE), 
    Maximum = max(x, na.rm = TRUE),
    Minimum = min(x, na.rm = TRUE))
})

print(landscape_summary)
