# Title: Irish Mapping 
# Purpose: Process the Irish boundary and using irish grid reference system
#
# Author: Leighanna Teal Dawson 
# Date: 20 Jan 2025
# 
##### Contence of the script:#####
# 0. Admin (load libraries, set working directory, load data)
# 1. Produce country boundry for Ireland (NI and ROI)
# 2. 
# 3.  
#

#### >>> Last note 20/01/25 - more high res map needed, this is only to 10m scale 

# clear environment
rm(list = ls())
#### 0. Admin (load libraries, set working directory, load data) ####
#set working directory
setwd("C:/VScode/all-ireland-occupancy-modelling")

# Check if the directory is set correctly
getwd()


# Load required libraries
library(sf)          # For handling spatial vector data
library(raster)      # For handling raster data
library(tidyverse)   # For data manipulation and visualization (# Tidyverse packages:
# ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats, lubridate, hms)
library(leaflet)     # For interactive maps
library(ggplot2)     # For plotting
library(rnaturalearth) # For downloading country bounties 
library(rnaturalearthdata) # For downloading country bounties data 
library(igr)        # For working in Irish Grid Reference system (https://cran.r-project.org/web/packages/igr/vignettes/igr.html#:~:text=The%20igr%20package%20is%20designed,R%20packages%20can%20work%20with.)
library(maps)        # For mapping
library(units) 
library(tmap) 
print("Libraries loaded successfully.")


##########################################################################
#### 1.Get country boundry for Ireland ####


# Load the map of UK, Ireland, and Northern Ireland boundaries
ireland_uk <- ne_countries(scale = 10, returnclass = "sf")[ne_countries(scale = "large", returnclass = "sf")$name %in% c("Ireland", "United Kingdom"), ]

# Retrieve boundary for Ireland (1:10m scale)
ireland <- ne_countries(country = "Ireland", scale = 10, returnclass = "sf")

# Plot the  boundary of Ireland
ggplot() +
  geom_sf(data = ireland, fill = "lightblue", color = "black") +
  labs(title = "High-Resolution Boundary of Ireland (1:10m scale)") +
  theme_minimal()

# Retrieve Northern Ireland boundary
northern_ireland <- ne_states(geounit = "northern ireland", returnclass = "sf")

# Plot the boundary of Northern Ireland
ggplot() +
  geom_sf(data = northern_ireland, fill = "lightblue", color = "black") +
  labs(title = "Boundary of Northern Ireland") +
  theme_minimal()


# Merge Northern Ireland and Republic of Ireland boundaries
ireland_map <- bind_rows(ireland, northern_ireland)

# Simplify the map to just the outline by dissolving the geometry
ireland_outline <- st_union(ireland_map)

# Plot just the outline of Ireland (without counties)
ggplot() +
  geom_sf(data = ireland_outline, fill = NA, color = "black") + 
  labs(title = "Outline of Ireland") +
  theme_minimal()

# Save the Ireland outline as a shapefile
st_write(ireland_outline, "1.data/1.2.processed/ireland_outline.shp",delete_layer = TRUE)


# Convert from sf object to GeoJSON for leaflet compatibility
ireland_geojson <- st_transform(ireland_outline, crs = 4326)  # Convert to WGS 84 (long-lat) for Leaflet

# Create the Leaflet map
leaflet(data = ireland_geojson) %>%
  addTiles() %>%  # Add OpenStreetMap tiles
  addPolygons(
    color = "blue",   # Boundary color
    weight = 2,       # Boundary line weight
    opacity = 1,      # Boundary line opacity
    fillOpacity = 0,  # No fill color (only border)
    popup = "Ireland Boundary" # Optional popup when clicking on the boundary
  ) %>%
  setView(lng = -7.5, lat = 53.5, zoom = 7) %>%  # Set the initial map view (centered on Ireland)
  addLegend("bottomright", colors = "blue", labels = "Ireland Boundary", title = "Legend") # Optional legend

#NOTE: This is only to 10m scale, more high res map needed but a different package/dataset is needed for this.
 
##########################################################################################################
#### 2. ####

############################################################################################################
#### 3. Create a grid using irish grid reference system for 100km squares ####
##### 3.1. Generate all grid references for 100km grid squares #####

# Create a list of all the valid prefixes (A to Y, excluding I, A, E, K, P, U, Y)
prefixes <- setdiff(LETTERS[1:25], c("I", "A", "E", "K", "P", "U", "Y"))

# Generate grid references for 100km grid squares (just the prefixes)
irishgrid100km <- data.frame(gridid = prefixes)

# Save the grid references for 100km to a CSV file
write.csv(irishgrid100km, file = "1.data/1.2.processed/1.2.1.irish_grid/100kmirishgrid.csv", row.names = FALSE)

##### 3.2. Converting to an sf object of POLYGON features and plotting #####
# Converting to an sf object of POLYGON features
igrpoly_100km <- st_igr_as_sf(irishgrid100km, "gridid", polygon = TRUE)

# Save igrpoly 
st_write(igrpoly_100km, "1.data/1.2.processed/1.2.1.irish_grid/100kmirishgrid.shp", delete_layer = TRUE)

# Plot igrpoly to check the grid
# Convert `igrpoly` to EPSG:4326 (required by leaflet)
igrpoly_100km_wgs84 <- sf::st_transform(igrpoly_100km, 4326)

# Create the leaflet map
leaflet(data = igrpoly_100km_wgs84) %>% 
  addTiles() %>%  # Add the default OpenStreetMap tiles
  addPolygons(color = "blue", weight = 1, opacity = 0.8, 
              fillOpacity = 0.5, label = ~gridid) %>% 
  addScaleBar(position = "bottomright")  # Add a scale bar for context

###########################################################################################
#### 4. Create a grid using irish grid reference system for 10km squares ####
##### 4.1. Generate all grid references for 10km grid squares #####

# Create a list of all the 4-digit numeric parts (00 to 99) for 10km grid cells
numbers <- sprintf("%02d", 0:99)  # This will generate a vector of "0000", "0001", ..., "9999"

# Generate all combinations of prefix and number
irishgrid10km <- expand.grid(prefix = prefixes, number = numbers, stringsAsFactors = FALSE)

# Combine the prefix and number to create the full Irish Grid reference
irishgrid10km$gridid <- paste0(irishgrid10km$prefix, irishgrid10km$number)

# Save the grid references 10km to a CSV file
write.csv(irishgrid10km, file = "1.data/1.2.processed/1.2.1.irish_grid/10kmirishgrid.csv", row.names = FALSE)

##### 4.2. Converting to an sf object of POLYGON features and plotting #####
# Converting to an sf object of POLYGON features
igrpoly_10km <- st_igr_as_sf(irishgrid10km, "gridid", polygon = TRUE)

# Save igrpoly 
st_write(igrpoly_10km, "1.data/1.2.processed/1.2.1.irish_grid/10kmirishgrid.shp", delete_layer = TRUE)

# Plot igrpoly to check the grid
# Convert `igrpoly` to EPSG:4326 (required by leaflet)
igrpoly_10km_wgs84 <- sf::st_transform(igrpoly_10km, 4326)

# Create the leaflet map
leaflet(data = igrpoly_10km_wgs84) %>%
  addTiles() %>%  # Add the default OpenStreetMap tiles
  addPolygons(color = "blue", weight = 1, opacity = 0.8, 
              fillOpacity = 0.5, label = ~gridid) %>%
  addScaleBar(position = "bottomright")  # Add a scale bar for context

###########################################################################################
#### 5. Create a grid using irish grid reference system for 1km squares ####
##### 5.1. Generate all grid references for 1km grid squares #####

# Create a list of all the 2-digit numeric parts (00 to 99) for 1km grid cells (for each 10km grid square, you now need 100 1km cells)
numbers <- sprintf("%04d", 0:9999)  # This will generate a vector of "00", "01", ..., "99" (for each 1km square)

# Generate all combinations of prefix and number for the 1km grid cells
irishgrid1km <- expand.grid(prefix = prefixes, number = numbers, stringsAsFactors = FALSE)

# Combine the prefix and number to create the full Irish Grid reference for 1km squares
irishgrid1km$gridid <- paste0(irishgrid1km$prefix, irishgrid1km$number)

# Save the grid references for 1km to a CSV file
write.csv(irishgrid1km, file = "1.data/1.2.processed/1.2.1.irish_grid/1kmirishgrid.csv", row.names = FALSE)

##### 5.2. Converting to an sf object of POLYGON features and plotting #####
# Converting to an sf object of POLYGON features
igrpoly_1km <- st_igr_as_sf(irishgrid1km, "gridid", polygon = TRUE)

# Save the grid polygons for 1km squares
st_write(igrpoly_1km, "1.data/1.2.processed/1.2.1.irish_grid/1kmirishgrid.shp", delete_layer = TRUE)

# Plot igrpoly_1km to check the grid
# Convert `igrpoly_1km` to EPSG:4326 (required by leaflet)
igrpoly_1km_wgs84 <- sf::st_transform(igrpoly_1km, 4326)

# Create the leaflet map for 1km grid squares
leaflet(data = igrpoly_1km_wgs84) %>%
  addTiles() %>%  # Add the default OpenStreetMap tiles
  addPolygons(color = "blue", weight = 1, opacity = 0.8, 
              fillOpacity = 0.5, label = ~gridid) %>%  # Show the gridid as the label for each square
  addScaleBar(position = "bottomright")  # Add a scale bar for context

print ("Irish Grid Reference System created successfully.")



