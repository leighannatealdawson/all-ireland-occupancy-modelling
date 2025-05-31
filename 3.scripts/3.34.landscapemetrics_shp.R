# Load Required Libraries
library(raster)
library(landscapemetrics)
library(sp)
library(plyr)
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


LCM_3035 <- raster("1.data/1.1.raw/corine_raster/DATA/U2018_CLC2018_V2020_20u1.tif") #original raster downloaded from copernicus in epsg 3035 
plot(LCM_3035)
crs(LCM_3035)

