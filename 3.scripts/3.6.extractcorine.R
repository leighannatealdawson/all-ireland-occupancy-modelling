# Title: Extract Corine into Irish Grid 

#
# Author: Leighanna Teal Dawson 
# Date: 28 Jan 2025
# 
##### Contence of the script:#####
# 0. Admin (load libraries, set working directory, load data)


#

rm(list = ls())

#set working directory
setwd("C:/VScode/all-ireland-occupancy-modelling")

# Check if the directory is set correctly
getwd()

#
# Load required libraries
library(sf)          # For handling spatial vector data
library(raster)      # For handling raster data
library(tidyverse)   # For data manipulation and visualization (# Tidyverse packages:
# ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats, lubridate, hms)
library(terra)       # For advanced raster and spatial data processing
library(igr)         # For working in Irish Grid Reference system (https://cran.r-project.org/web/packages/igr/vignettes/igr.html#:~:text=The%20igr%20package%20is%20designed,R%20packages%20can%20work%20with.)
library(rnaturalearth) # For downloading country bounties 
library(rnaturalearthdata) # For downloading country bounties data 
<<<<<<< Updated upstream
library(exactextractr)   # For extracting raster data by polygon

=======
print("Libraries loaded successfully.")

# Required Libraries
library(sf)
library(raster)
library(exactextractr)
library(dplyr)
library(ggplot2)
>>>>>>> Stashed changes

# Load the CORINE RData file
load("1.data/1.2.processed/corine.RData")

# Check the object loaded from the RData file
print(ls())  # List all objects in the environment to identify the CORINE raster
# Assume the CORINE raster is named "corine_raster" after loading

# Load the 1km Irish Grid shapefile
igrpoly_1km <- st_read("1.data/1.2.processed/1.2.1.irish_grid/1kmirishgrid.shp")

# Ensure CRS matches between grid and CORINE raster
igrpoly_1km <- st_transform(igrpoly_1km, crs = st_crs(corine_raster))

# Extract CORINE data for each grid cell
corine_values <- exact_extract(corine_raster, igrpoly_1km, fun = "mode", progress = TRUE)

# Add extracted CORINE values to the grid
igrpoly_1km$corine_mode <- corine_values

# Save the updated grid with CORINE data as a shapefile
st_write(igrpoly_1km, "1.data/1.2.processed/1.2.1.irish_grid/1kmirishgrid_with_corine.shp", delete_layer = TRUE)

# Optional: Save as CSV for summary analysis
corine_summary <- igrpoly_1km %>%
  st_drop_geometry() %>%
  select(gridid, corine_mode)  # Select only relevant columns for analysis

write.csv(corine_summary, "1.data/1.2.processed/1.2.1.irish_grid/1kmirishgrid_with_corine.csv", row.names = FALSE)

# Plot grid with CORINE data for visualization
ggplot() +
  geom_sf(data = igrpoly_1km, aes(fill = as.factor(corine_mode)), color = NA) +
  scale_fill_viridis_d(name = "CORINE Class") +
  labs(title = "CORINE Land Cover Data by 1km Irish Grid", subtitle = "Mode Value per Grid Square") +
  theme_minimal()

