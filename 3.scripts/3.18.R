rm(list = ls()) # Clear the workspace
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
View(cams)
# Convert Site Data to Spatial Points
#cams.sp <- cams
#coordinates(cams.sp) <- ~long+lat
#proj4string(cams.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

####################################################################################
# plot overlapping to check projections 


# Convert Site Data to Spatial Points
cams.sp <- st_as_sf(cams, coords = c("long", "lat"), crs = 4326)


# Convert raster to dataframe
IrelandLCM_df <- as.data.frame(IrelandLCM, xy = TRUE, na.rm = TRUE)
#class(IrelandLCM_df)
cams.sp_df <- as.data.frame(cams.sp)

# plot NOTE: when plotting like this points may not voerlap with raster due to mixing raster and sf?? 
plot(IrelandLCM)
plot(cams.sp, add=TRUE, col = "#000000", pch = 20, cex = 1.5)

# reproject to 2157 
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

# Get Unique Values from Raster
#unique_values <- unique(values(IrelandLCM))
#sorted_unique_values <- sort(unique_values)
#print(sorted_unique_values)

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

#############################################################################################################
# Check and Transform CRS of Points
#crs(IrelandLCM_reclassed)
#crs(cams.sp)
#cams.sp <- spTransform(cams.sp, CRS(proj4string(IrelandLCM_reclassed)))
#crs(cams.sp)

#################################################################################################################
# rename landcover type from temp_code to clc_code 

# rename landcover type -exclusing covertypes not present in ireland 

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
check 

#unique_values <- unique(values(IrelandLCM_reclassed_CLC))
#sorted_unique_values <- sort(unique_values)
#print(sorted_unique_values)
# make table of count of unique classes in raster 
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
####################################################################################################
# Sample Landscape Metrics
LCM_5kmbuffer_allsites <- sample_lsm(IrelandLCM_reclassed_CLC, cams.sp, size = 150, level = "patch", metric = "area")
print("done")
View(LCM_5kmbuffer_allsites)

#crs(IrelandLCM_reclassed_CLC)
#crs(cams.sp)


# Process Sampled Data
df <- LCM_5kmbuffer_allsites %>%
  group_by(plot_id, class) %>%
  summarise(area = sum(value)) %>%
  spread(class, area)
View(df)

# Calculate Total Cover per Row
df$total <- rowSums(df[ , !(names(df) %in% c("plot_id"))], na.rm = TRUE)
View(df)

# Calculate Proportion of Each Class
df[is.na(df)] <- 0
df[ , !(names(df) %in% c("plot_id", "total"))] <- df[ , !(names(df) %in% c("plot_id", "total"))] / df$total
df[,"112"] <- df[,"112"] / df[,"total"]

# Merge with Site Data
cams <- rename(cams, plot_id = observation_id)
lcm_df <- merge(df, cams, by = "plot_id")
View(lcm_df)

# Visualize Data
range(df$"311", na.rm = TRUE)
range(df$"312", na.rm = TRUE)
hist(df$"312")

##########################################################################################



