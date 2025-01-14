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

# Get world shapefile
world <- ne_countries(scale = "large", returnclass = "sf")

# Filter for the Republic of Ireland
republic_ireland <- world[world$name == "Ireland", ]

# Base R plot
plot(st_geometry(republic_ireland), main = "Republic of Ireland Map")




# Download both Republic of Ireland and Northern Ireland (as part of the UK)
republic_ireland <- ne_countries(scale = "large", returnclass = "sf", country = "Ireland")
plot(st_geometry(republic_ireland), main = "Republic of Ireland Map")
UK <- ne_countries(scale = "large", returnclass = "sf", country = "United Kingdom")
plot(st_geometry(UK), main = "UK Map")

# Filter for Northern Ireland from the United Kingdom (for detailed administrative regions)
northern_ireland <- UK[UK$subregion == "Northern Ireland", ]
plot(st_geometry(northern_ireland), main = "Northern Ireland Map")


# Combine Republic of Ireland and Northern Ireland into one map
all_ireland <- rbind(republic_ireland, northern_ireland)

# Plot the map of Ireland (both NI and Republic of Ireland)
plot(st_geometry(all_ireland), main = "Map of Ireland (Republic and Northern Ireland)")

# Combine Republic of Ireland and Northern Ireland into one map
all_ireland <- rbind(republic_ireland, northern_ireland)

# Plot the map of Ireland (both NI and Republic of Ireland)
plot(st_geometry(all_ireland), main = "Map of Ireland (Republic and Northern Ireland)")





# Download the Republic of Ireland (RoI) boundaries using rnaturalearth
republic_ireland <- ne_countries(scale = "large", returnclass = "sf", country = "Ireland")
plot(st_geometry(republic_ireland), main = "Republic of Ireland Map")
# Check the plot of the Republic of Ireland
print("Republic of Ireland plot displayed.")

# Download the United Kingdom (UK) boundaries (it includes Northern Ireland)
UK <- ne_countries(scale = "large", returnclass = "sf", country = "United Kingdom")
plot(st_geometry(UK), main = "UK Map")
# Check the plot of the UK map
print("UK map displayed.")

# Filter for Northern Ireland from the United Kingdom (since it's part of the UK)
northern_ireland <- UK[UK$subregion == "Northern Ireland", ]
plot(st_geometry(northern_ireland), main = "Northern Ireland Map")
# Check the plot of Northern Ireland
print("Northern Ireland plot displayed.")

# Combine Republic of Ireland and Northern Ireland into one map
all_ireland <- rbind(republic_ireland, northern_ireland)

# Plot the combined map of Ireland (both RoI and NI)
plot(st_geometry(all_ireland), main = "Map of Ireland (Republic and Northern Ireland)")
# Check the final plot of all of Ireland
print("Combined map of Ireland displayed.")

# Install necessary packages if not already installed
install.packages("rnaturalearth")
install.packages("sf")
library(rnaturalearth)
library(sf)

# Download the Republic of Ireland (RoI) boundaries using rnaturalearth
republic_ireland <- ne_countries(scale = "large", returnclass = "sf", country = "Ireland")
plot(st_geometry(republic_ireland), main = "Republic of Ireland Map")
# Check the plot of the Republic of Ireland
print("Republic of Ireland plot displayed.")

# Download the United Kingdom (UK) boundaries (it includes Northern Ireland)
UK <- ne_countries(scale = "large", returnclass = "sf", country = "United Kingdom")
plot(st_geometry(UK), main = "UK Map")
# Check the plot of the UK map
print("UK map displayed.")

# For Northern Ireland, use bounding box or known coordinates, as it is part of the UK.
# Spatially filter using bounding box or known geography
northern_ireland <- UK[UK$admin == "United Kingdom" & 
                       st_intersects(UK, st_as_sfc(st_bbox(c(xmin = -7.5, xmax = -5.5, ymin = 54, ymax = 55.8)))), ]
plot(st_geometry(northern_ireland), main = "Northern Ireland Map")
# Check the plot of Northern Ireland
print("Northern Ireland plot displayed.")

# Combine Republic of Ireland and Northern Ireland into one map
all_ireland <- rbind(republic_ireland, northern_ireland)

# Plot the combined map of Ireland (both RoI and NI)
plot(st_geometry(all_ireland), main = "Map of Ireland (Republic and Northern Ireland)")
# Check the final plot of all of Ireland
print("Combined map of Ireland displayed.")
##############################################################################
#try a different package 
library(dplyr)
library(tidyverse)
library(sf) #for reading in shapefiles

#




# Output goals:
# - Reclassified raster dataset
# - Summary statistics of land cover distribution
# - Visualizations for data validation

C:\VScode\all-ireland-occupancy-modelling\1.data\1.1.raw\CORINELandCover2018\U2018_CLC2018_V2020_20u1_doc\Info\Metadata\U2018_CLC2018_V2020_20u1.xml

# Import the CSV file
file_path <- "C:/VScode/all-ireland-occupancy-modelling/1.data/1.1.raw/CORINELandCover2018/U2018_CLC2018_V2020_20u1_doc/Info/Legend/Vector/CLC_legend.csv"
data <- read.csv(file_path)

# Display the first few rows of the data
head(data)
