rm(ls( ))

# extract road data 

library(purrr)
library(tidyr)
library(dplyr)
library(sf)
library(terra)
library(stars)
library(ggplot2)
library(ggspatial)
library(leaflet)
library(prettymapr)
library(maptiles)
library(raster)
library(landscapemetrics)
library(tidyverse)
library(terra)



# Import road data
roads <- st_read(  "1.data/1.1.raw/roads/OSM IReland data MAy 25/gis_osm_roads_free_1.shp")

# Check CRS of road data
st_crs(roads)

# Load site data
cams <- read.csv("1.data/1.2.processed/paandhabitat.csv")

# Convert Site Data to Spatial Points to match road data 
cams.sp <- st_as_sf(cams, coords = c("long", "lat"), crs = 4326) 

# Calculate 1km buffer around each camera trap site
buffer_km <- 1
buffer_m <- buffer_km *10^6
radius_buffer <- sqrt(buffer_m / pi) # radius in m
radius_buffer

buffers_1km2 <- st_buffer(cams.sp, dist = radius_buffer,nQuadSegs = 100)

leaflet() %>%
    addTiles() %>%
    addPolygons(data = buffers_1km2, color = "blue", weight = 1, fillOpacity = 0.5)

View(buffers_1km2)

### adapt from here 
# Intersect roads with buffers to get segments inside
roads_in_buffers <- st_intersection(roads, buffers_1km2)

# Calculate road lengths
roads_in_buffers$length_m <- st_length(roads_in_buffers)
print("done")
View(roads_in_buffers)
# Sum road length per buffer
road_lengths_by_site <- roads_in_buffers |>
  group_by(buffer_id = buffers_1km2$your_id_column[match(st_within(roads_in_buffers, buffers_1km2, sparse = FALSE), TRUE)]) |>
  summarise(total_road_m = sum(length_m))

# Join this back to your site data
buffers_with_road <- left_join(buffers_1km2, road_lengths_by_site, by = c("your_id_column" = "buffer_id"))