

# Load CORINE raster
IrelandLCM <- raster("1.data/1.1.raw/U2018_CLC2018_V2020_20u1_all_Ireland.tif")

# Load camera site data
cams <- read.csv("1.data/1.2.processed/paandhabitat.csv")

# Convert site data to sf points
cams.sf <- st_as_sf(cams, coords = c("long", "lat"), crs = 4326)  # WGS84

# Reproject cams to match raster (assume raster is in EPSG:2157 - Irish Transverse Mercator)
cams.sf <- st_transform(cams.sf, crs = 2157)

# Convert raster to dataframe for ggplot
IrelandLCM_df <- as.data.frame(IrelandLCM, xy = TRUE, na.rm = TRUE)
colnames(IrelandLCM_df) <- c("landcover", "x", "y")




library(tmap)

# tmap is like leaflet but can do static too
tmap_mode("plot")

tm_shape(cams.sf) +
  tm_basemap(server = "OpenStreetMap") + # or "Esri.WorldImagery", "CartoDB.Positron"
  tm_dots(col = "red", size = 0.1) +
  tm_layout(title = "Camera Traps with OSM Basemap")


library(tmap)

tmap_mode("view")  # use interactive mode

tm_shape(cams.sf) +
  tm_basemap(server = "OpenStreetMap") +
  tm_dots(col = "red", size = 0.1)


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(data = st_transform(cams.sp, 4326),
                   radius = 4, color = "red") %>%
  addScaleBar(position = "bottomleft") 
  
  %>%
  addTitle("Camera Traps Across Ireland")

















#'# Using Landcovermetrics to calculate landcover metrics for each camera trap site within a bufer 

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
####################################################################################
# Import Data
## Import CORINE land cover data in GeoTIFF format
IrelandLCM <- raster("1.data/1.1.raw/U2018_CLC2018_V2020_20u1_all_Ireland.tif")
crs(IrelandLCM)
print(IrelandLCM)
plot(IrelandLCM)
####################################################################################
## Load site data
cams <- read.csv("1.data/1.2.processed/paandhabitat.csv")

####################################################################################
# plot overlapping to check projections 

# Convert Site Data to Spatial Points
cams.sp <- st_as_sf(cams, coords = c("long", "lat"), crs = 4326) # dont worry change to ITM later 

# Convert raster to dataframe
IrelandLCM_df <- as.data.frame(IrelandLCM, xy = TRUE, na.rm = TRUE)
#class(IrelandLCM_df)
cams.sp_df <- as.data.frame(cams.sp)

# plot NOTE: when plotting like this points may not voerlap with raster due to mixing raster and sf?? 
plot(IrelandLCM)
plot(cams.sp, add=TRUE, col = "#000000", pch = 20, cex = 1.5)

# reproject to 2157 to match raster later as itll need to be in m not degrees 
cams.sp <- st_transform(cams.sp, crs = 2157)

# Rename the layer column as tif layer doesnt keep correct col names 
colnames(IrelandLCM_df) <- c("landcover", "x", "y")
head(IrelandLCM_df)

# plot both layers as csv using ggplot2 to check overlap as expected 
ggplot() +
  geom_raster(data = IrelandLCM_df, aes(x = x, y = y, fill = landcover)) +
  geom_point(data = cams, aes(x = long, y = lat), color = "black", size = 2) +
  coord_sf(xlim = c(-11, -5), ylim = c(51, 56)) +
  theme_minimal() +
  labs(title = "Camera Points over CORINE Land Cover")

# this plots as expected with no reporjection needed. i think??? 
##############################################################################
# Landscape metrics require:
# 1. projection in metres 
# 2. Raster encodes landscape sclasses as intergers (e.g., 1, 2, 3, ...)
# 3. Landscape metrics describe categorical landscapes, that means that your landscape needs to be classified 
## Warning if over 30 classes (i have 44? )
# Check Landscape Requirements
check_landscape(IrelandLCM)
#            layer        crs       units   class n_classes OK
# i have      1 geographic degrees  integer        35       ✖ 
# i need      1 projected   m       integer        n       ✔
# n_classes over 30 will get warning 

# reprojection require intergers without missing values, this orignal data for ireland has missing valeus due to lack of some clc categoreis therefore temparay renaming of interger is needed to avoid issues 

# Define the mapping from orig_code to temp_code
orig_to_temp <- matrix(c(
  1, 1001,
  2, 1002,
  3, 1003,
  4, 1004,
  5, 1005,
  6, 1006,
  7, 1007,
  8, 1008,
  9, 1009,
  10, 1010,
  11, 1011,
  12, 1012,
  16, 1013,
  18, 1014,
  20, 1015,
  21, 1016,
  23, 1017,
  24, 1018,
  25, 1019,
  26, 1020,
  27, 1021,
  29, 1022,
  30, 1023,
  31, 1024,
  32, 1025,
  33, 1026,
  35, 1027,
  36, 1028,
  37, 1029,
  39, 1030,
  40, 1031,
  41, 1032,
  42, 1033,
  43, 1034,
  44, 1035
), byrow = TRUE, ncol = 2)

nrow(orig_to_temp) # 35 rows in matrix
# Rename raster landcover classes 
IrelandLCM_temp <- reclassify(IrelandLCM, orig_to_temp)

#check renaming 
# make table of count of unique classes in raster 
# Calculate the frequency of each unique value in the raster
unique_values <- values(IrelandLCM_temp)
# Create a frequency table (data frame) of unique values and their counts
value_counts <- table(unique_values)
# Convert the table to a data frame for easier viewing and handling
value_counts_df <- as.data.frame(value_counts)
# Rename columns for clarity
colnames(value_counts_df) <- c("LandCoverClass", "Count")
# Display the result
View(value_counts_df)

# Reproject Raster to Metres (ITM75)
IrelandLCMepsg2157 <- projectRaster(IrelandLCM_temp, crs = CRS("+init=epsg:2157"))
#ensure landcover is still as interger 
IrelandLCMepsg2157 <- as.integer(IrelandLCMepsg2157)

# Recheck Landscape Metrics Requirements
check_landscape(IrelandLCMepsg2157)
# this is okay 
# Note there should be 1 layer, projected in metres,
# as interger with with nclasses = 
#######################################################################
# Plot Reprojected Raster and Points to double check projection
## Downsample Raster for Testing
IrelandLCM_small <- aggregate(IrelandLCMepsg2157, fact = 10)

## Plot with Leaflet
leaflet() %>%
  addTiles() %>%
  addRasterImage(IrelandLCM_small, colors = terrain.colors(100), opacity = 0.6) %>%
  addCircleMarkers(data = cams.sp, color = "red", radius = 3, label = ~as.character(Site)) %>%
  setView(lng = mean(cams$long), lat = mean(cams$lat), zoom = 7)
# its as expected 

#################################################################################################################
# rename landcover type from temp_code to clc_code 

# Define the mapping from temp_code to clc_code
temp_to_clc <- matrix(c(
  1001, 111,
  1002, 112,
  1003, 121,
  1004, 122,
  1005, 123,
  1006, 124,
  1007, 131,
  1008, 132,
  1009, 133,
  1010, 141,
  1011, 142,
  1012, 211,
  1013, 222,
  1014, 231,
  1015, 242,
  1016, 243,
  1017, 311,
  1018, 312,
  1019, 313,
  1020, 321,
  1021, 322,
  1022, 324,
  1023, 331,
  1024, 332,
  1025, 333,
  1026, 334,
  1027, 411,
  1028, 412,
  1029, 421,
  1030, 423,
  1031, 511,
  1032, 512,
  1033, 521,
  1034, 522,
  1035, 523
), byrow = TRUE, ncol = 2)

# count rows in matrix 
nrow(temp_to_clc) # 35 rows in matrix
# Rename raster landcover classes 
IrelandLCM_reclassed_CLC <- reclassify(IrelandLCMepsg2157, temp_to_clc)

# Calculate the frequency of each unique value in the raster
unique_values <- values(IrelandLCM_reclassed_CLC)
# Create a frequency table (data frame) of unique values and their counts
value_counts <- table(unique_values)
# Convert the table to a data frame for easier viewing and handling
value_counts_df <- as.data.frame(value_counts)
# Rename columns for clarity
colnames(value_counts_df) <- c("LandCoverClass", "Count")
# Display the result
View(value_counts_df)

check_landscape(IrelandLCM_reclassed_CLC)

st_crs(cams.sp)
crs(IrelandLCMepsg2157)

plot(IrelandLCM_reclassed_CLC)
points(cams.sp, col = "red", pch = 20, cex = 1.5)


st_crs(cams.sp)
crs(IrelandLCM_reclassed_CLC)
extent(cams.sp)
extent(IrelandLCM_reclassed_CLC)

check for nas in raster
nas <- is.na(IrelandLCM_reclassed_CLC)

IrelandLCM_df <- as.data.frame(IrelandLCM_reclassed_CLC, xy = TRUE, na.rm = TRUE)
colnames(IrelandLCM_df) <- c("x", "y", "landcover")
cams.sp_df <- as.data.frame(st_coordinates(cams.sp))
colnames(cams.sp_df) <- c("long", "lat")
colnames
ggplot() +
  geom_raster(data = IrelandLCM_df, aes(x = x, y = y, fill = landcover)) +
  geom_point(data = cams.sp_df, aes(x = long, y = lat), color = "black", size = 2) +
  coord_sf(xlim = c(-11, -5), ylim = c(51, 56)) +
  theme_minimal() +
  labs(title = "Camera Points over CORINE Land Cover")

