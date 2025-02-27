################Predict Risk
# Load required libraries
library(compositions) # for compositional data analysis
library(MASS)  # for stepAIC
library(dplyr)  # for data manipulation
library(glmmTMB) #for GLMMs
library(pscl) #zero-inflated and hurdle models 
library(dplyr) # data manipulation
library(glm2) #GLMs
library(gridExtra)
library(ggplot2)

#Housekeeping
setwd("/Users/rosiemangan/Desktop/MSc/Semester 2/Dissertation/R script")
list.files()

#Level1VA
bee_data <- read.csv("L1VA_Dateset_Final.csv")
View(bee_data)
colnames(bee_data)

# check for zeros and replace
adjusted_bee_data <- as.data.frame(lapply(bee_data[, c("ARTIFICIAL.SURFACES", "GRASSLAND..SALTMARSH.and.SWAMP", 
                                                       "HEATH.and.BRACKEN", "WATERBODIES", "CULTIVATED.LAND", "EXPOSED.SURFACES", 
                                                       "PEATLAND", "FOREST..WOODLAND.AND.SCRUB")], function(x) ifelse(x == 0, 1e-5, x)))
View(adjusted_bee_data)
comp_data <- acomp(adjusted_bee_data)  # convert to Aitchison composition
View(comp_data)
print(colnames(comp_data))


# Apply ALR transformation using FOREST..WOODLAND.&SCRUB as denominator
alr_data <- alr(comp_data, denominator = 7) 
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

#ALR MODEL

#CULTIVATED.LAND
#levels of interest for land cover proportions
land_cover_levels <- c(0.1, 0.25, 0.5, 0.75, 1.0)

# create flexible range for Long and Lat to cover Ireland
longitude_range <- seq(-10, -6, by = 0.5)
latitude_range <- seq(51, 55, by = 0.5) 

new_data <- expand.grid(
  Longitude = longitude_range,
  Latitude = latitude_range,
  alr_CULTIVATED.LAND = land_cover_levels,
  alr_GRASSLAND..SALTMARSH.and.SWAMP = 0, 
  alr_PEATLAND = 0,
  alr_WATERBODIES = 0,
  alr_EXPOSED.SURFACES = 0,
  alr_ARTIFICIAL.SURFACES = 0,
  alr_HEATH.and.BRACKEN = 0
)

# predict using the ZIP model

#ALR
zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude + alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND +
                        alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES + alr_HEATH.and.BRACKEN | 1
                      ,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data, na.action = na.omit)
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



predicted_pesticides <- predict(zip_model, newdata = new_data, type = "response")


# add predictions to the new_data df
new_data$Predicted_Pesticides <- predicted_pesticides
scenarios_summary <- new_data %>%
  group_by(alr_CULTIVATED.LAND) %>%
  summarize(Mean_Pesticides = mean(Predicted_Pesticides), .groups = "drop")

print(scenarios_summary)


plot_cultivated<-ggplot(new_data, aes(x = alr_CULTIVATED.LAND, y = Predicted_Pesticides)) +
  geom_smooth(se = TRUE, color = "darkgreen", size = 1, fill = "lightgreen") +
  labs(
    title = "Cultivated land",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),  # Center the title, increase size
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 18),  # Increase axis numbers size
    legend.position = "right") 
  

#ARTIFICIAL.SURFACES 

#levels of interest for land cover proportions
land_cover_levels <- c(0.1, 0.25, 0.5, 0.75, 1.0)

# create flexible range for Long and Lat to cover Ireland
longitude_range <- seq(-10, -6, by = 0.5)
latitude_range <- seq(51, 55, by = 0.5) 

new_data <- expand.grid(
  Longitude = longitude_range,
  Latitude = latitude_range,
  alr_CULTIVATED.LAND = 0,
  alr_GRASSLAND..SALTMARSH.and.SWAMP = 0, 
  alr_PEATLAND = 0,
  alr_WATERBODIES = 0,
  alr_EXPOSED.SURFACES = 0,
  alr_ARTIFICIAL.SURFACES = land_cover_levels,
  alr_HEATH.and.BRACKEN = 0
)

# predict using the ALR ZIP model

#ALR
zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude + alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND +
                        alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES + alr_HEATH.and.BRACKEN | 1
                      ,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data, na.action = na.omit)


summary(zip_model)


predicted_pesticides <- predict(zip_model, newdata = new_data, type = "response")


# add predictions to the new_data df
new_data$Predicted_Pesticides <- predicted_pesticides
scenarios_summary <- new_data %>%
  group_by(alr_ARTIFICIAL.SURFACES) %>%
  summarize(Mean_Pesticides = mean(Predicted_Pesticides), .groups = "drop")

print(scenarios_summary)



plot_artificial<-ggplot(new_data, aes(x = alr_ARTIFICIAL.SURFACES, y = Predicted_Pesticides)) +
  geom_smooth(se = TRUE, color = "black", size = 1, fill = "grey") +
  labs(
    title = "Artifical surfaces",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),  # Center the title, increase size
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 18),  # Increase axis numbers size
    legend.position = "right"
  )

#PEATLAND

#levels of interest for land cover proportions
land_cover_levels <- c(0.1, 0.25, 0.5, 0.75, 1.0)

# create flexible range for Long and Lat to cover Ireland
longitude_range <- seq(-10, -6, by = 0.5)
latitude_range <- seq(51, 55, by = 0.5) 

new_data <- expand.grid(
  Longitude = longitude_range,
  Latitude = latitude_range,
  alr_CULTIVATED.LAND = 0,
  alr_GRASSLAND..SALTMARSH.and.SWAMP = 0, 
  alr_PEATLAND = land_cover_levels,
  alr_WATERBODIES = 0,
  alr_EXPOSED.SURFACES = 0,
  alr_ARTIFICIAL.SURFACES = 0,
  alr_HEATH.and.BRACKEN = 0
)

# predict using the ZIP model

#ALR
zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude + alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND +
                        alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES + alr_HEATH.and.BRACKEN | 1
                      ,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data, na.action = na.omit)
summary(zip_model)


predicted_pesticides <- predict(zip_model, newdata = new_data, type = "response")


# add predictions to the new_data df
new_data$Predicted_Pesticides <- predicted_pesticides
scenarios_summary <- new_data %>%
  group_by(alr_PEATLAND) %>%
  summarize(Mean_Pesticides = mean(Predicted_Pesticides), .groups = "drop")

print(scenarios_summary)

plot_peatland<- ggplot(new_data, aes(x = alr_PEATLAND, y = Predicted_Pesticides)) +
  geom_smooth(se = TRUE, color = "brown", size = 1, fill = "tan") +
  labs(
    title = "Peatland",
    x = "",
    y = ""
  )+
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),  # Center the title, increase size
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 18),  # Increase axis numbers size
    legend.position = "right"
  )


#GRASSLAND..SALTMARSH.and.SWAMP

#levels of interest for land cover proportions
land_cover_levels <- c(0.1, 0.25, 0.5, 0.75, 1.0)

# create flexible range for Long and Lat to cover Ireland
longitude_range <- seq(-10, -6, by = 0.5)
latitude_range <- seq(51, 55, by = 0.5) 

new_data <- expand.grid(
  Longitude = longitude_range,
  Latitude = latitude_range,
  alr_CULTIVATED.LAND = 0,
  alr_GRASSLAND..SALTMARSH.and.SWAMP = land_cover_levels, 
  alr_PEATLAND = 0,
  alr_WATERBODIES = 0,
  alr_EXPOSED.SURFACES = 0,
  alr_ARTIFICIAL.SURFACES = 0,
  alr_HEATH.and.BRACKEN = 0
)

# predict using the ZIP model

#ALR
zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude + alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND +
                        alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES + alr_HEATH.and.BRACKEN | 1
                      ,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data, na.action = na.omit)
summary(zip_model)


predicted_pesticides <- predict(zip_model, newdata = new_data, type = "response")


# add predictions to the new_data df
new_data$Predicted_Pesticides <- predicted_pesticides
scenarios_summary <- new_data %>%
  group_by(alr_GRASSLAND..SALTMARSH.and.SWAMP) %>%
  summarize(Mean_Pesticides = mean(Predicted_Pesticides), .groups = "drop")

print(scenarios_summary)

plot_grassland <-ggplot(new_data, aes(x = alr_GRASSLAND..SALTMARSH.and.SWAMP, y = Predicted_Pesticides)) +
  geom_smooth(se = TRUE, color = "#99CC00", size = 1, fill = "lightgreen") +
  labs(
    title = "Grassland",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),  # Center the title, increase size
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 18),  # Increase axis numbers size
    legend.position = "right"
  )




#EXPOSED.SURFACES

#levels of interest for land cover proportions
land_cover_levels <- c(0.1, 0.25, 0.5, 0.75, 1.0)

# create flexible range for Long and Lat to cover Ireland
longitude_range <- seq(-10, -6, by = 0.5)
latitude_range <- seq(51, 55, by = 0.5) 

new_data <- expand.grid(
  Longitude = longitude_range,
  Latitude = latitude_range,
  alr_CULTIVATED.LAND = 0,
  alr_GRASSLAND..SALTMARSH.and.SWAMP = 0, 
  alr_PEATLAND = 0,
  alr_WATERBODIES = 0,
  alr_EXPOSED.SURFACES = land_cover_levels,
  alr_ARTIFICIAL.SURFACES = 0,
  alr_HEATH.and.BRACKEN = 0
)

# predict using the ZIP model

#ALR
zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude + alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND +
                        alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES + alr_HEATH.and.BRACKEN | 1
                      ,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data, na.action = na.omit)
summary(zip_model)


predicted_pesticides <- predict(zip_model, newdata = new_data, type = "response")


# add predictions to the new_data df
new_data$Predicted_Pesticides <- predicted_pesticides
scenarios_summary <- new_data %>%
  group_by(alr_EXPOSED.SURFACES) %>%
  summarize(Mean_Pesticides = mean(Predicted_Pesticides), .groups = "drop")

print(scenarios_summary)

plot_exposed <-ggplot(new_data, aes(x = alr_EXPOSED.SURFACES, y = Predicted_Pesticides)) +
  geom_smooth(se = TRUE, color = "magenta", size = 1, fill = "lightpink") +
  
  labs(
    title = "Exposed surfaces",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),  # Center the title, increase size
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 18),  # Increase axis numbers size
    legend.position = "right"
  )

#WATERBODIES

#levels of interest for land cover proportions
land_cover_levels <- c(0.1, 0.25, 0.5, 0.75, 1.0)

# create flexible range for Long and Lat to cover Ireland
longitude_range <- seq(-10, -6, by = 0.5)
latitude_range <- seq(51, 55, by = 0.5) 

new_data <- expand.grid(
  Longitude = longitude_range,
  Latitude = latitude_range,
  alr_CULTIVATED.LAND = 0,
  alr_GRASSLAND..SALTMARSH.and.SWAMP = 0, 
  alr_PEATLAND = 0,
  alr_WATERBODIES = land_cover_levels,
  alr_EXPOSED.SURFACES = 0,
  alr_ARTIFICIAL.SURFACES = 0,
  alr_HEATH.and.BRACKEN = 0
)

# predict using the ZIP model

#ALR
zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude + alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND +
                        alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES + alr_HEATH.and.BRACKEN | 1
                      ,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data, na.action = na.omit)
summary(zip_model)


predicted_pesticides <- predict(zip_model, newdata = new_data, type = "response")


# add predictions to the new_data df
new_data$Predicted_Pesticides <- predicted_pesticides
scenarios_summary <- new_data %>%
  group_by(alr_WATERBODIES) %>%
  summarize(Mean_Pesticides = mean(Predicted_Pesticides), .groups = "drop")

print(scenarios_summary)

plot_waterbodies <- ggplot(new_data, aes(x = alr_WATERBODIES, y = Predicted_Pesticides)) +
  geom_smooth(se = TRUE, color = "#0066CC", size = 1, fill = "lightblue") +
  labs(
    title = "Waterbodies",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),  # Center the title, increase size
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 18),  # Increase axis numbers size
    legend.position = "right"
  )

#HEATH.and.BRACKEN   

#levels of interest for land cover proportions
land_cover_levels <- c(0.1, 0.25, 0.5, 0.75, 1.0)

# create flexible range for Long and Lat to cover Ireland
longitude_range <- seq(-10, -6, by = 0.5)
latitude_range <- seq(51, 55, by = 0.5) 

new_data <- expand.grid(
  Longitude = longitude_range,
  Latitude = latitude_range,
  alr_CULTIVATED.LAND = 0,
  alr_GRASSLAND..SALTMARSH.and.SWAMP = 0, 
  alr_PEATLAND = 0,
  alr_WATERBODIES = 0,
  alr_EXPOSED.SURFACES = 0,
  alr_ARTIFICIAL.SURFACES = 0,
  alr_HEATH.and.BRACKEN = land_cover_levels
)

# predict using the ALR ZIP model

#ALR
zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude + alr_CULTIVATED.LAND + alr_GRASSLAND..SALTMARSH.and.SWAMP + alr_PEATLAND +
                        alr_WATERBODIES + alr_EXPOSED.SURFACES + alr_ARTIFICIAL.SURFACES + alr_HEATH.and.BRACKEN | 1
                      ,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data, na.action = na.omit)
summary(zip_model)


predicted_pesticides <- predict(zip_model, newdata = new_data, type = "response")


# add predictions to the new_data df
new_data$Predicted_Pesticides <- predicted_pesticides
scenarios_summary <- new_data %>%
  group_by(alr_HEATH.and.BRACKEN) %>%
  summarize(Mean_Pesticides = mean(Predicted_Pesticides), .groups = "drop")

print(scenarios_summary)

plot_heath <- ggplot(new_data, aes(x = alr_HEATH.and.BRACKEN, y = Predicted_Pesticides)) +
  geom_smooth(se = TRUE, color = "purple", size = 1, fill = "mediumpurple") +
  labs(title = "Heath & bracken",
       x = "",
       y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),  # Center the title, increase size
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 18),  # Increase axis numbers size
    legend.position = "right") +
  scale_y_continuous(limits = c(2, 10))




# Arrange the plots in a 2x4 grid
grid.arrange( 
  plot_cultivated, plot_artificial, plot_peatland, plot_grassland,
  plot_exposed, plot_waterbodies, plot_heath,
  ncol = 4
)

grid.arrange(
  plot_cultivated + theme(plot.margin = margin(5, 10, 5, 10)),
  plot_artificial + theme(plot.margin = margin(5, 10, 5, 10)),
  plot_grassland + theme(plot.margin = margin(5, 10, 5, 10)),
  plot_exposed + theme(plot.margin = margin(5, 10, 5, 10)),
  plot_waterbodies + theme(plot.margin = margin(5, 10, 5, 10)),
  plot_heath + theme(plot.margin = margin(5, 10, 5, 10)),
  plot_peatland + theme(plot.margin = margin(5, 10, 5, 10)),
  ncol = 4
)



##############CLR MODEL

#CULTIVATED.LAND
#levels of interest for land cover proportions
land_cover_levels <- c(0.1, 0.25, 0.5, 0.75, 1.0)

# create flexible range for Long and Lat to cover Ireland
longitude_range <- seq(-10, -6, by = 0.5)
latitude_range <- seq(51, 55, by = 0.5) 

new_data <- expand.grid(
  Longitude = longitude_range,
  Latitude = latitude_range,
  clr_CULTIVATED.LAND = land_cover_levels,
  clr_GRASSLAND..SALTMARSH.and.SWAMP = 0, 
  clr_WATERBODIES = 0,
  clr_EXPOSED.SURFACES = 0,
  clr_ARTIFICIAL.SURFACES = 0,
  clr_HEATH.and.BRACKEN = 0,
  clr_FOREST..WOODLAND.AND.SCRUB =0 )


# predict using the ZIP model

#CLR
zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude   +
                        clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES   | 1,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data,  na.action = na.omit)

summary(zip_model)
predicted_pesticides <- predict(zip_model, newdata = new_data, type = "response")


# add predictions to the new_data df
new_data$Predicted_Pesticides <- predicted_pesticides
scenarios_summary <- new_data %>%
  group_by(clr_CULTIVATED.LAND) %>%
  summarize(Mean_Pesticides = mean(Predicted_Pesticides), .groups = "drop")

print(scenarios_summary)

plot_cultivated<-ggplot(new_data, aes(x = clr_CULTIVATED.LAND, y = Predicted_Pesticides)) +
  geom_smooth(se = TRUE, color = "darkgreen", size = 1, fill = "lightgreen") +
  labs(
    title = "Cultivated land",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),  # Center the title, increase size
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 18),  # Increase axis numbers size
    legend.position = "right"
  )

#ARTIFICIAL.SURFACES 

#levels of interest for land cover proportions
land_cover_levels <- c(0.1, 0.25, 0.5, 0.75, 1.0)

# create flexible range for Long and Lat to cover Ireland
longitude_range <- seq(-10, -6, by = 0.5)
latitude_range <- seq(51, 55, by = 0.5) 

new_data <- expand.grid(
  Longitude = longitude_range,
  Latitude = latitude_range,
  clr_CULTIVATED.LAND = 0,
  clr_GRASSLAND..SALTMARSH.and.SWAMP = 0, 
  clr_WATERBODIES = 0,
  clr_EXPOSED.SURFACES = 0,
  clr_ARTIFICIAL.SURFACES = land_cover_levels,
  clr_HEATH.and.BRACKEN = 0,
  clr_FOREST..WOODLAND.AND.SCRUB =0 )

# predict using the ZIP model

#CLR
zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude   +
                        clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES   | 1,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data,  na.action = na.omit)

summary(zip_model)


predicted_pesticides <- predict(zip_model, newdata = new_data, type = "response")


# add predictions to the new_data df
new_data$Predicted_Pesticides <- predicted_pesticides
scenarios_summary <- new_data %>%
  group_by(clr_ARTIFICIAL.SURFACES) %>%
  summarize(Mean_Pesticides = mean(Predicted_Pesticides), .groups = "drop")

print(scenarios_summary)

plot_artificial<-ggplot(new_data, aes(x = clr_ARTIFICIAL.SURFACES, y = Predicted_Pesticides)) +
  geom_smooth(se = TRUE, color = "black", size = 1, fill = "grey") +
  labs(
    title = "Artifical surfaces",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),  # Center the title, increase size
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 18),  # Increase axis numbers size
    legend.position = "right"
  )


#GRASSLAND..SALTMARSH.and.SWAMP

#levels of interest for land cover proportions
land_cover_levels <- c(0.1, 0.25, 0.5, 0.75, 1.0)

# create flexible range for Long and Lat to cover Ireland
longitude_range <- seq(-10, -6, by = 0.5)
latitude_range <- seq(51, 55, by = 0.5) 

new_data <- expand.grid(
  Longitude = longitude_range,
  Latitude = latitude_range,
  clr_CULTIVATED.LAND = 0,
  clr_GRASSLAND..SALTMARSH.and.SWAMP = land_cover_levels, 
  clr_WATERBODIES = 0,
  clr_EXPOSED.SURFACES = 0,
  clr_ARTIFICIAL.SURFACES = 0,
  clr_HEATH.and.BRACKEN = 0,
  clr_FOREST..WOODLAND.AND.SCRUB =0 )


# predict using the CLR ZIP model

zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude   +
                        clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES   | 1,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data,  na.action = na.omit)

summary(zip_model)


predicted_pesticides <- predict(zip_model, newdata = new_data, type = "response")


# add predictions to the new_data df
new_data$Predicted_Pesticides <- predicted_pesticides
scenarios_summary <- new_data %>%
  group_by(clr_GRASSLAND..SALTMARSH.and.SWAMP) %>%
  summarize(Mean_Pesticides = mean(Predicted_Pesticides), .groups = "drop")

print(scenarios_summary)

plot_grassland <-ggplot(new_data, aes(x = clr_GRASSLAND..SALTMARSH.and.SWAMP, y = Predicted_Pesticides)) +
    geom_smooth(se = TRUE, color = "#99CC00", size = 1, fill = "lightgreen") +
    labs(
      title = "Grassland",
      x = "",
      y = ""
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 22),  # Center the title, increase size
      axis.title = element_text(size = 14),  # Increase axis title size
      axis.text = element_text(size = 18),  # Increase axis numbers size
      legend.position = "right"
    )  
  
  
  

#EXPOSED.SURFACES

#levels of interest for land cover proportions
land_cover_levels <- c(0.1, 0.25, 0.5, 0.75, 1.0)

# create flexible range for Long and Lat to cover Ireland
longitude_range <- seq(-10, -6, by = 0.5)
latitude_range <- seq(51, 55, by = 0.5) 

new_data <- expand.grid(
  Longitude = longitude_range,
  Latitude = latitude_range,
  clr_CULTIVATED.LAND = 0,
  clr_GRASSLAND..SALTMARSH.and.SWAMP = 0, 
  clr_WATERBODIES = 0,
  clr_EXPOSED.SURFACES = land_cover_levels,
  clr_ARTIFICIAL.SURFACES = 0,
  clr_HEATH.and.BRACKEN = 0,
  clr_FOREST..WOODLAND.AND.SCRUB =0 )

# predict using the ZIP model

zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude   +
                        clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES   | 1,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data,  na.action = na.omit)

summary(zip_model)




predicted_pesticides <- predict(zip_model, newdata = new_data, type = "response")


# add predictions to the new_data df
new_data$Predicted_Pesticides <- predicted_pesticides
scenarios_summary <- new_data %>%
  group_by(clr_EXPOSED.SURFACES) %>%
  summarize(Mean_Pesticides = mean(Predicted_Pesticides), .groups = "drop")

print(scenarios_summary)

plot_exposed <-ggplot(new_data, aes(x = clr_EXPOSED.SURFACES, y = Predicted_Pesticides)) +
  geom_smooth(se = TRUE, color = "magenta", size = 1, fill = "lightpink") +
  
  labs(
    title = "Exposed surfaces",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),  # Center the title, increase size
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 18),  # Increase axis numbers size
    legend.position = "right"
  )

#WATERBODIES

#levels of interest for land cover proportions
land_cover_levels <- c(0.1, 0.25, 0.5, 0.75, 1.0)

# create flexible range for Long and Lat to cover Ireland
longitude_range <- seq(-10, -6, by = 0.5)
latitude_range <- seq(51, 55, by = 0.5) 

new_data <- expand.grid(
  Longitude = longitude_range,
  Latitude = latitude_range,
  clr_CULTIVATED.LAND = 0,
  clr_GRASSLAND..SALTMARSH.and.SWAMP = 0, 
  clr_WATERBODIES = land_cover_levels,
  clr_EXPOSED.SURFACES = 0,
  clr_ARTIFICIAL.SURFACES = 0,
  clr_HEATH.and.BRACKEN = 0,
  clr_FOREST..WOODLAND.AND.SCRUB =0 )

# predict using the ZIP model

zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude   +
                        clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES   | 1,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data,  na.action = na.omit)

summary(zip_model)


predicted_pesticides <- predict(zip_model, newdata = new_data, type = "response")


# add predictions to the new_data df
new_data$Predicted_Pesticides <- predicted_pesticides
scenarios_summary <- new_data %>%
  group_by(clr_WATERBODIES) %>%
  summarize(Mean_Pesticides = mean(Predicted_Pesticides), .groups = "drop")

print(scenarios_summary)

plot_waterbodies<-ggplot(new_data, aes(x = clr_WATERBODIES, y = Predicted_Pesticides)) +
  geom_smooth(se = TRUE, color = "#0066CC", size = 1, fill="lightblue") +
  labs(
    title = "Waterbodies",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),  # Center the title, increase size
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 18),  # Increase axis numbers size
    legend.position = "right"
  )

#HEATH.and.BRACKEN   

#levels of interest for land cover proportions
land_cover_levels <- c(0.1, 0.25, 0.5, 0.75, 1.0)

# create flexible range for Long and Lat to cover Ireland
longitude_range <- seq(-10, -6, by = 0.5)
latitude_range <- seq(51, 55, by = 0.5) 

new_data <- expand.grid(
  Longitude = longitude_range,
  Latitude = latitude_range,
  clr_CULTIVATED.LAND = 0,
  clr_GRASSLAND..SALTMARSH.and.SWAMP = 0, 
  clr_WATERBODIES = 0,
  clr_EXPOSED.SURFACES = 0,
  clr_ARTIFICIAL.SURFACES = 0,
  clr_HEATH.and.BRACKEN = land_cover_levels,
  clr_FOREST..WOODLAND.AND.SCRUB =0 )

# predict using the ZIP model

zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude   +
                        clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES   | 1,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data,  na.action = na.omit)

summary(zip_model)


predicted_pesticides <- predict(zip_model, newdata = new_data, type = "response")


# add predictions to the new_data df
new_data$Predicted_Pesticides <- predicted_pesticides
scenarios_summary <- new_data %>%
  group_by(clr_HEATH.and.BRACKEN) %>%
  summarize(Mean_Pesticides = mean(Predicted_Pesticides), .groups = "drop")

print(scenarios_summary)

plot_heath<-ggplot(new_data, aes(x = clr_HEATH.and.BRACKEN, y = Predicted_Pesticides)) +
  geom_smooth(se = TRUE, color = "purple", size = 1, fill= "mediumpurple") +
  labs(title = "Heath & bracken",
       x = "",
       y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),  # Center the title, increase size
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 18),  # Increase axis numbers size
    legend.position = "right"
  )



#FOREST..WOODLAND.AND.SCRUB   

#levels of interest for land cover proportions
land_cover_levels <- c(0.1, 0.25, 0.5, 0.75, 1.0)

# create flexible range for Long and Lat to cover Ireland
longitude_range <- seq(-10, -6, by = 0.5)
latitude_range <- seq(51, 55, by = 0.5) 

new_data <- expand.grid(
  Longitude = longitude_range,
  Latitude = latitude_range,
  clr_CULTIVATED.LAND = 0,
  clr_GRASSLAND..SALTMARSH.and.SWAMP = 0, 
  clr_WATERBODIES = 0,
  clr_EXPOSED.SURFACES = 0,
  clr_ARTIFICIAL.SURFACES = 0,
  clr_HEATH.and.BRACKEN = 0,
  clr_FOREST..WOODLAND.AND.SCRUB =land_cover_levels )

# predict using the ZIP model

zip_model <- zeroinfl(Total_pesticides_pollen_over_LOQ ~ Longitude + Latitude   +
                        clr_CULTIVATED.LAND + clr_GRASSLAND..SALTMARSH.and.SWAMP + clr_FOREST..WOODLAND.AND.SCRUB + clr_HEATH.and.BRACKEN + clr_WATERBODIES + clr_EXPOSED.SURFACES + clr_ARTIFICIAL.SURFACES   | 1,
                      dist = "poisson", link = "logit",
                      data = combined_bee_data,  na.action = na.omit)

summary(zip_model)


predicted_pesticides <- predict(zip_model, newdata = new_data, type = "response")


# add predictions to the new_data df
new_data$Predicted_Pesticides <- predicted_pesticides
scenarios_summary <- new_data %>%
  group_by(clr_FOREST..WOODLAND.AND.SCRUB) %>%
  summarize(Mean_Pesticides = mean(Predicted_Pesticides), .groups = "drop")

print(scenarios_summary)

plot_forest<-ggplot(new_data, aes(x = clr_FOREST..WOODLAND.AND.SCRUB, y = Predicted_Pesticides)) +
  geom_smooth(se = TRUE, color = "yellow", size = 1, fill= "lightyellow") +
  labs(title = "Forest",
       x = "",
       y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),  # Center the title, increase size
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 18),  # Increase axis numbers size
    legend.position = "right"
  )


#All CLR predictions together
# Arrange the plots in a 2x4 grid
grid.arrange( 
  plot_cultivated, plot_artificial, plot_grassland,
  plot_exposed, plot_waterbodies, plot_heath, plot_forest, 
  ncol = 4
)

grid.arrange(
  plot_cultivated + theme(plot.margin = margin(5, 10, 5, 10)),
  plot_artificial + theme(plot.margin = margin(5, 10, 5, 10)),
  plot_grassland + theme(plot.margin = margin(5, 10, 5, 10)),
  plot_exposed + theme(plot.margin = margin(5, 10, 5, 10)),
  plot_waterbodies + theme(plot.margin = margin(5, 10, 5, 10)),
  plot_heath + theme(plot.margin = margin(5, 10, 5, 10)),
  plot_forest + theme(plot.margin = margin(5, 10, 5, 10)),
  ncol = 4
)





#Assigning Risk
CLR_predicted_pesticides<-read.csv("CLR_PredictedPesticides.csv")
ALR_predicted_pesticides<-read.csv("ALR_PredictedPesticides.csv")


# divide it into approx equal intervals
quintiles <- quantile(ALR_predicted_pesticides$Predicted.pesticides, probs = c(0.20, 0.40, 0.60, 0.80))
print(quintiles)

quintiles <- quantile(CLR_predicted_pesticides$Predicted.pesticides, probs = c(0.20, 0.40, 0.60, 0.80))
print(quintiles)


#ALR risk (Range: 2.52-7.26)
#Low Risk: Predicted pesticides < 4.1
#Neutral Risk: Predicted pesticides between 4.1 and 5.3
#High Risk: Predicted pesticides > 5.3

#Assigning CLR risk (Range: 3.15-9.08)
#Low Risk: Predicted pesticides < 5.1
#Neutral Risk: Predicted pesticides between 5.1 and 6.5
#High Risk: Predicted pesticides > 6.5

clr_100 <- subset(CLR_predicted_pesticides, Proportion == 1.0)
alr_100 <- subset(ALR_predicted_pesticides, Proportion == 1.0)

clr_75 <- subset(CLR_predicted_pesticides, Proportion == 0.75)
alr_75 <- subset(ALR_predicted_pesticides, Proportion == 0.75)

clr_50 <- subset(CLR_predicted_pesticides, Proportion == 0.50)
alr_50 <- subset(ALR_predicted_pesticides, Proportion == 0.50)



# Function to assign ALR risk categories
assign_risk_alr <- function(predicted_pesticides) {
  if (predicted_pesticides < 4.1) {
    return("Low")
  } else if (predicted_pesticides >= 4.1 & predicted_pesticides <= 5.3) {
    return("Neutral")
  } else {
    return("High")
  }
}


# Function to assign CLR risk categories
assign_risk_clr <- function(predicted_pesticides) {
  if (predicted_pesticides < 5.1) {
    return("Low")
  } else if (predicted_pesticides >= 5.1 & predicted_pesticides <= 6.5) {
    return("Neutral")
  } else {
    return("High")
  }
}

clr_100$Risk <- sapply(clr_100$Predicted.pesticides, assign_risk_clr)
alr_100$Risk <- sapply(alr_100$Predicted.pesticides, assign_risk_alr)

clr_75$Risk <- sapply(clr_75$Predicted.pesticides, assign_risk_clr)
alr_75$Risk <- sapply(alr_75$Predicted.pesticides, assign_risk_alr)

clr_50$Risk <- sapply(clr_50$Predicted.pesticides, assign_risk_clr)
alr_50$Risk <- sapply(alr_50$Predicted.pesticides, assign_risk_alr)

clr_risk<-print(clr_100)
alr_risk<-print(alr_100)

clr_risk<-print(clr_75)
alr_risk<-print(alr_75)

clr_risk<-print(clr_50)
alr_risk<-print(alr_50)

# Add proportion column for clarity
clr_100$Proportion <- "100%"
alr_100$Proportion <- "100%"
clr_75$Proportion <- "75%"
alr_75$Proportion <- "75%"
clr_50$Proportion <- "50%"
alr_50$Proportion <- "50%"

clr_100$Model <- "CLR"
alr_100$Model <- "ALR"
clr_75$Model <- "CLR"
alr_75$Model <- "ALR"
clr_50$Model <- "CLR"
alr_50$Model <- "ALR"

combined_100 <- merge(clr_100, alr_100, all = TRUE, suffixes = c("_CLR", "_ALR"))
combined_75 <- merge(clr_75, alr_75, all = TRUE, suffixes = c("_CLR", "_ALR"))
combined_50 <- merge(clr_50, alr_50, all = TRUE, suffixes = c("_CLR", "_ALR"))

combined_risk_df <- rbind(combined_100, combined_75, combined_50)
