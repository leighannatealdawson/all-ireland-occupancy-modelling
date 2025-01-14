# Title: Processing CORINE Land Cover Data for Ireland
# Purpose: Pocesses CORINE Land Cover data for all of Ireland
#          to prepare it for use in a multi-species occupancy model. 
#
# Author: Leighanna
# Date: [Insert Date]
# 
##### Contence of the script:#####
# 0. Admin (load libraries, set working directory, load data)
# 1. Load CORINE Land Cover data for Ireland.
# 2. Load the study area boundary for Ireland.
# 3. Load the Irish Grid Reference system.  
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
print("Libraries loaded successfully.")



# Scope and Workflow:
# This script processes CORINE Land Cover data to provide spatially explicit covariates, 
# focusing on habitat type and landscape features relevant to mesopredator species such 
# as pine martens and foxes. These data will be used in a multi-species occupancy model 
# to explore ecological relationships and human-wildlife conflict in Ireland.
#
# Workflow:
# 1. Load CORINE land cover raster data. 
# 2. Load the study area boundary for Ireland. 
# 2. Crop and reproject the data to the study area boundary.
# 3. Reclassify land cover categories based on their ecological relevance.
# 4. Generate summary statistics for land cover classes.
# 5. Export the processed data for modeling.

# Define study area and file paths
# Example paths - adjust these to your data locations


######################################################################
# Load CORINE Land Cover data for Ireland from 2018 # 
# Data source: https://land.copernicus.eu/en/products/corine-land-cover/clc2018

# Define the path to the .gdb file
gdb_path <- "1.data/1.1.raw/CORINELandCover2018/U2018_CLC2018_V2020_20u1.gdb"

# Read the specific layer from the .gdb file
corinedataraw <- st_read(gdb_path, layer = "U2018_CLC2018_V2020_20u1")

# Check the structure of the loaded data 
str(corinedataraw)
head(corinedataraw)
colnames(corinedataraw)
dim(corinedataraw)

# Plot the geometries from the layer (this may take some time)
# plot(st_geometry(corinedataraw), main = "Corine Land Cover 2018")

# View the CORINE data 
#View(corinedataraw)

###########################################################################
# Load the CLC Code from folder legend
CLClegend <- readxl::read_excel("1.data/1.1.raw/CORINELandCover2018/U2018_CLC2018_V2020_20u1_doc/Info/Legend/Vector/clc_legend.xls")

# Check the structure of the loaded data
head(CLClegend)
View(CLClegend)

##########################################################################
# Add CORINE Land Cover legend labels to the data 

colnames(corinedataraw)
colnames(CLClegend)

# Rename the column 'CODE_18' to 'CLC_CODE' in the corinedataraw data frame
corinedataraw <- corinedataraw %>% rename("CLC_CODE" = "Code_18")

# Check column names of both data frames match 
colnames(corinedataraw)
colnames(CLClegend)

# Check the class of the merge column in both data frames
class(corinedataraw$CLC_CODE)
class(CLClegend$CLC_CODE)

# Merge the CORINE data with the CLC legend data based on the 'CLC_CODE' column
corine <- merge(corinedataraw, CLClegend, by = "CLC_CODE", all.x = TRUE)

colnames(corine)


# Tidy up -Remove everything except 'corine' from the environment
rm(list = setdiff(ls(), "corine"))


##########################################################################
# Get country boundry for Ireland
# Load the map of UK and Ireland and Northern Ireland boundaries
ireland_uk <- ne_countries(scale = "large", returnclass = "sf")[ne_countries(scale = "large", returnclass = "sf")$name %in% c("Ireland", "United Kingdom"), ]
northern_ireland <- ne_states(geounit = "northern ireland", returnclass = "sf")
ireland <- ne_states(geounit = "ireland", returnclass = "sf")

# Merge Northern Ireland and Republic of Ireland boundaries
ireland_map <- rbind(ireland, northern_ireland)

# Simplify the map to just the outline by dissolving the geometry
ireland_outline <- st_union(ireland_map)


# Plot the outline on top of the counties (showing just the boundary)
ggplot() +
  geom_sf(data = ireland_map, aes(fill = name)) + 
  geom_sf(data = ireland_outline, fill = NA, color = "black", size = 1) + 
  labs(title = "Island of Ireland with Outline") +
  theme_minimal() +
  theme(legend.position = "none")  # Optional: Remove the legend

# Plot just the outline of Ireland (without counties)
ggplot() +
  geom_sf(data = ireland_outline, fill = NA, color = "black") + 
  labs(title = "Outline of Ireland") +
  theme_minimal()

# Save the Ireland outline as a shapefile
st_write(ireland_outline, "1.data/1.2.processed/ireland_outline.shp")


###########################################################################
