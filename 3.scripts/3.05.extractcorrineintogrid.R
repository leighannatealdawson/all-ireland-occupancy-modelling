# Title: Extract Data to Grid 
# Purpose: extract the varialbes already processed into the grid 
#
# Author: Leighanna Teal Dawson 
# Date: 20 Feb 2025
# 
##### Contence of the script:#####
# 0. Admin (load libraries, set working directory, load data)

#

rm(list = ls())

#set working directory
setwd("C:/VScode/all-ireland-occupancy-modelling")

# Check if the directory is set correctly
getwd()

# Load required libraries
library(sf)          # For handling spatial vector data
library(tidyverse)   # For data manipulation and visualization  
library(ggplot2)     # For plotting
library(leaflet)
library(mapview)
library(mapview)


#import data 
# Read the CSV file containing the habitat information
exampledata <- read.csv("6.provided-scripts/habitatinfo201520182020sitesonly.csv")
View(exampledata)
#import corine data 
corine <- st_read("1.data/1.2.processed/corine.gpkg", layer = "corine")
plot(st_geometry(corine))

#import irish grid datasets 
irishgrid1km <- st_read("1.data/1.2.processed/1.2.1.irish_grid/1kmirishgrid.shp")
irishgrid10km <- st_read("1.data/1.2.processed/1.2.1.irish_grid/10kmirishgrid.shp")
irishgrid100km <- st_read("1.data/1.2.processed/1.2.1.irish_grid/100kmirishgrid.shp")

#plot the grid maps 
#mapview(irishgrid1km, color = "lightblue", legend = TRUE)
#mapview(irishgrid10km, color = "lightblue", legend = TRUE)
mapview(irishgrid100km, color = "lightblue", legend = TRUE)


#########################################################################################################################
# 1. Change CRS of the Corine data to match the Irish grid
# Check CRS of your Irish grid and Corine data
st_crs(irishgrid100km)  # CRS of your Irish grid
st_crs(corine)  # CRS of your Corine data

# Transform corine CRS from 3035 to # Transform Corine data CRS to match the grid CRS
corine <- st_transform(corine, st_crs(irishgrid100km))

# check this worked as expected
st_crs(irishgrid100km)  
st_crs(corine)

#NOTE>>> this will need repating for each grid size

#########################################################################################################################
# 2. 
# Perform a spatial join to assign each Corine polygon to a grid cell
corine_in_grid <- st_join(corine, irishgrid100km)

# Check the result to confirm the join was successful
head(corine_in_grid)
colnames(corine_in_grid)
nrow(corine_in_grid)

#########################################################################################################################
# test corine data with only 312 (coniferous forest) for speed 
 # Filter the corine_in_grid for rows where CLC_CODE is 312 (Coniferous forests)
coniferous_data <- corine_in_grid %>%
  filter(CLC_CODE == 312)

# View the resulting filtered data
head(coniferous_data)
nrow(coniferous_data)
class(coniferous_data)
colnames(coniferous_data)
# Check if the 'geometry' column is present and the data is an 'sf' object
coniferous_data <- st_as_sf(coniferous_data)  # Ensure it's an 'sf' object

# Calculate the total area (in km²) of CLC_CODE 312 for each grid cell
coniferous_area_by_grid <- coniferous_data %>%
  group_by(gridid) %>% 
  summarize(total_area_km2 = sum(st_area(geom)) / 1e6,  # Convert area from m² to km²
            .groups = "drop")

# View the result
head(coniferous_area_by_grid)
nrow(coniferous_area_by_grid)
view(coniferous_area_by_grid)
#########################################################################################################################

#########################################################################################################################
# repeate above for all CLC codes
 
# Step 1: Extract unique CLC_CODEs
CLC_CODES_char <- unique(corine_in_grid$CLC_CODE)
CLC_CODES_char

# Step 2: Initialize an empty data frame to store results
total_area_by_grid_df <- data.frame()

# Initialize an empty data frame with the correct column names
total_area_by_grid_df <- data.frame(gridid = character(),
                                    total_area_km2 = numeric(),
                                    CLC_CODE = character(),
                                    stringsAsFactors = FALSE)

# Now, loop through each unique CLC_CODE as before
for (code in CLC_CODES_char) {
  # Filter the data for the current CLC_CODE (use the correct column name)
  code_data <- corine_in_grid %>% filter(CLC_CODE == code)
  
  # Convert to 'sf' if it's not already
  code_data <- st_as_sf(code_data)
  
  # Calculate the total area (in km²) for each grid cell
  area_by_grid <- code_data %>%
    group_by(gridid) %>%
    summarize(total_area_km2 = sum(st_area(geom)) / 1e6,  # Convert area from m² to km²
              .groups = "drop")
  
  # Convert 'total_area_km2' to numeric to avoid unit type mismatch
  area_by_grid$total_area_km2 <- as.numeric(area_by_grid$total_area_km2)
  
  # Add a new column to store the CLC_CODE
  area_by_grid$CLC_CODE <- code
  
  # Remove the 'geom' column, as it's not needed for binding
  area_by_grid <- area_by_grid %>% select(gridid, total_area_km2, CLC_CODE)
  
  # Bind the result to the main data frame
  total_area_by_grid_df <- bind_rows(total_area_by_grid_df, area_by_grid)
}

#########################################################################################################################
# Calculate the area of each LABEL1 within each grid cell
corine_area_by_label <- corine_in_grid %>%
  group_by(gridid, LABEL1) %>%
  summarize(area_label = sum(st_area(st_geometry(.))), .groups = "drop")

# Check the result
head(corine_area_by_label)

# Pivot the data into a wide format
corine_area_wide <- corine_area_by_label %>%
  select(gridid, LABEL1, area_label) %>%
  pivot_wider(names_from = LABEL1, values_from = area_label)

# View the resulting table
head(corine_area_wide)

# Remove the 'geom' column and pivot the data
corine_area_wide <- corine_area_by_label %>%
  select(gridid, LABEL1, area_label) %>%
  pivot_wider(names_from = LABEL1, values_from = area_label)

# View the resulting table
head(corine_area_wide)





# Perform spatial intersection to extract Corine data within each grid cell
corine_in_grid <- st_intersection(irishgrid100km, corine)
head(corine_in_grid)#ade6b0#42bde6#add8e6#ADD8E6#3A6F81#119FCE#8CC1D3#A2D38C

# You can now aggregate the data per grid cell
corine_summary <- corine_in_grid %>%
  group_by(gridid) %>%  # Group by the grid cell (you may need to adjust the variable name)
  summarize(
    total_area = st_area(geometry),  # Example: total area of Corine land cover per grid cell
    count = n(),  # Number of features (polygons) intersecting in each grid cell
    .groups = "drop"
  )


view(corine_summary)
#########################################################################################################################
# Calculate the area for each individual polygon first (this works at the feature level)
corine_in_grid <- corine_in_grid %>%
  mutate(area = st_area(geometry))  # Add a new column 'area' with the area of each feature

# Now, aggregate by grid cell
corine_summary <- corine_in_grid %>%
  group_by(gridid) %>%  # Group by grid cell
  reframe(
    total_area = sum(area, na.rm = TRUE),  # Sum up the area for each grid cell
    count = n(),  # Count the number of intersecting Corine polygons in each grid cell
    .groups = "drop"
  )

view(corine_summary)
head(corine_summary)



# Remove units from total_area column and convert to numeric
corine_summary$total_area <- as.numeric(corine_summary$total_area)

# Merge the grid geometries with the summary data
corine_summary_sf <- irishgrid100km %>%
  left_join(corine_summary, by = c("gridid" = "gridid"))

# Plotting the grid cells with a color scale based on the total area
ggplot(corine_summary_sf) +
  geom_sf(aes(fill = total_area), color = "black") +  # Color by total_area
  scale_fill_viridis_c(option = "C") +  # You can choose other color scales like 'plasma' or 'magma'
  theme_minimal() +
  labs(
    title = "Total Area of Corine Land Cover per Grid Cell",
    fill = "Total Area (m²)"
  )
#########################################################################################################################
#extract the proportion of each CLC_CODE in corine for each grid cell 
# Step 1: Calculate the area of each Corine feature (by CLC_CODE)
corine_area_by_code <- corine_in_grid %>%
  mutate(area = st_area(geometry)) %>%
  group_by(gridid, CLC_CODE) %>%
  summarize(total_area_code = sum(area), .groups = "drop")

corine_area_by_code

# Step 2: Calculate the total area of each grid cell
corine_total_area <- corine_in_grid %>%
  mutate(area = st_area(geometry)) %>%
  group_by(gridid) %>%
  summarize(total_area_grid = sum(area), .groups = "drop")

head(corine_total_area)
view(corine_total_area)



#########################################################################################################################
# Step 1: Calculate the area of each Corine feature (by CLC_CODE)
corine_area_by_code <- corine_in_grid %>%
  mutate(area = st_area(geometry)) %>%
  group_by(gridid, CLC_CODE) %>%
  summarize(total_area_code = sum(area), .groups = "drop")

# Step 2: Calculate the total area of each grid cell (using st_area)
# Since `irishgrid100km` contains the grid geometries, we can calculate their area
grid_area <- irishgrid100km %>%
  mutate(total_area_grid = st_area(geometry))

# Step 3: Perform a spatial join to combine grid geometries with the CLC_CODE area
# This assumes that both the `corine_in_grid` and `irishgrid100km` data are in the same coordinate system
corine_area_with_proportions <- st_join(corine_area_by_code, grid_area) %>%
  mutate(proportion = total_area_code / total_area_grid)  # Calculate proportion

# Now `corine_area_with_proportions` contains the proportion of each CLC_CODE in each grid cell
head(corine_area_with_proportions)


# Assuming 'irishgrid' is your existing grid dataset and 'corine_area_with_proportions' contains the proportions
# Merge the two datasets based on 'gridid'
irishgrid_updated <- irishgrid %>%
  left_join(corine_area_with_proportions, by = "gridid")

# View the updated dataset
head(irishgrid_updated)

# If you want to export the updated dataset
write.csv(irishgrid_updated, "irishgrid_with_corine_proportions.csv")


#########################################################################################################################
# Import the CSV file containing the 10km Irish grid by CLC data processed in arcgispro 
irishgrid_by_CLC_10km <- read.csv("C:/VScode/all-ireland-occupancy-modelling/1.data/1.3.processedinarc/10kmirishgridbyCLC.csv")

# View the imported data
head(irishgrid_by_CLC_10km)

library(tidyverse)
library(tidyr)
colnames(irishgrid_by_CLC_10km)

# Step 1: Aggregate the data if there are duplicates
irishgrid_by_CLC_10km <- irishgrid_by_CLC_10km %>%
  group_by(gridid, Code_18) %>%
  summarise(PercentArea = sum(PercentArea, na.rm = TRUE), .groups = "drop")

# Step 2: Pivot the data to wide format
wide_data_10km <- irishgrid_by_CLC %>%
  pivot_wider(
    names_from = Code_18,           # Use CLC type (Code_18) as new column names
    values_from = PercentArea,      # Use PercentArea as values
    values_fill = list(PercentArea = 0)  # Fill missing values with 0
  )

# View the wide format data
head(wide_data_10km)
View(wide_data_10km)
# Compute the total row sum for columns starting with "1", "2", "3", "4", "5"
wide_data_10km <- wide_data_10km %>%
  mutate(total = rowSums(across(starts_with(c("1", "2", "3", "4", "5"))), na.rm = TRUE))

# View the result
head(wide_data_10km)


#save this as a csvfile to processed folder
write.csv(wide_data_10km, "1.data/1.3.processedinarc/10kmwidecorine.csv", row.names = FALSE)


##########################################################################################################
#repeate this for 1km grid 

# Import the CSV files containing the 1km Irish grid by CLC data processed in arcgispro 
irishgrid_by_CLC_1km_B_G <- read.csv("1.data/1.3.processedinarc/1kmirishgridbyCLC_B_G.csv")
irishgrid_by_CLC_1km_H_M <- read.csv("1.data/1.3.processedinarc/1kmirishgridbyCLC_H_M.csv")
irishgrid_by_CLC_1km_N_R <- read.csv("1.data/1.3.processedinarc/1kmirishgridbyCLC_N_R.csv")
irishgrid_by_CLC_1km_S_X <- read.csv("1.data/1.3.processedinarc/1kmirishgridbyCLC_S_X.csv")

#check formatting of each df 
head(irishgrid_by_CLC_1km_B_G)
head(irishgrid_by_CLC_1km_H_M)
head(irishgrid_by_CLC_1km_N_R)
head(irishgrid_by_CLC_1km_S_X)


# merge all 4 irishgrid by CLC 1km dfs 
irishgrid_by_CLC_1km <- bind_rows(irishgrid_by_CLC_1km_B_G, 
                        irishgrid_by_CLC_1km_H_M, 
                        irishgrid_by_CLC_1km_N_R, 
                        irishgrid_by_CLC_1km_S_X)

# View merged data 
head(irishgrid_by_CLC_1km)
View(irishgrid_by_CLC_1km)

# count nubmer of rows
nrow(irishgrid_by_CLC_1km)

#check for duplicates 
irishgrid_by_CLC_1km <- irishgrid_by_CLC_1km %>%
  group_by(gridid, Code_18) %>%
  summarise(PercentArea = sum(PercentArea, na.rm = TRUE), .groups = "drop")

# count nubmer of rows
nrow(irishgrid_by_CLC_1km)

#check for duplicated rows 
sum(duplicated(irishgrid_by_CLC_1km))

# Pivot the data to wide format
wide_data_1km <- irishgrid_by_CLC_1km %>%
  pivot_wider(
    names_from = Code_18,           # Use CLC type (Code_18) as new column names
    values_from = PercentArea, 
    values_fill = list(PercentArea = 0)     # Use PercentArea as values
  )

head(wide_data_1km)
View(wide_data_1km)
wide_data <- irishgrid_by_CLC %>%
  pivot_wider(
    names_from = Code_18,           # Use CLC type (Code_18) as new column names
    values_from = PercentArea,      # Use PercentArea as values
    values_fill = list(PercentArea = 0)  # Fill missing values with 0
  )

# Compute the total row sum for columns starting with "1", "2", "3", "4", "5"
wide_data_1km <- wide_data_1km %>%
  mutate(total = rowSums(across(starts_with(c("1", "2", "3", "4", "5"))), na.rm = TRUE))

# View the result
View(wide_data_1km)

#save this as a csvfile to processed folder
write.csv(wide_data_1km, "1.data/1.3.processedinarc/1kmwidecorine.csv", row.names = FALSE)

