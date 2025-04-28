# retrying landscape metrics using rater in epsg:3035 ETRS89-extended / LAEA Europe- and converting to sf 
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
 

# Crop the raster to the extent of Ireland
ireland_extent <- extent(2500000, 3750000, 2500000, 4500000) # Define the extent of Ireland in EPSG:3035
crop_LCM_3035 <- crop(LCM_3035, ireland_extent) # Crop the raster to the extent of Ireland 
plot(crop_LCM_3035, main = "Cropped Raster of Ireland")
####################################################################################
## Load site data
cams <- read.csv("1.data/1.2.processed/paandhabitat.csv")

# Convert Site Data to Spatial Points
cams.sp <- st_as_sf(cams, coords = c("long", "lat"), crs = 4326) 

# reproject to 3035 to match raster later as itll need to be in m not degrees
cams.sp_3035 <- st_transform(cams.sp, crs = 3035)

# reproject to the same as raster file directly
cams.sp_reprojected <- st_transform(cams.sp_3035, crs = crs(crop_LCM_3035))


# Simplified Plot using leaflet
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = cams.sp_3035, color = "red", radius = 5) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))

print("done")
plot(st_geometry(cams.sp_3035), add = TRUE, col = "red", pch = 20)

#check sf proejction with basemap 
basemap <- get_tiles(cams.sp_3035, provider = "CartoDB.Positron", crop = TRUE, zoom = 8)

ggplot() +
  layer_spatial(basemap) +
  geom_sf(data = cams.sp_3035, color = "red", size = 2) +
  coord_sf(crs = st_crs(cams.sp_3035)) +
  theme_minimal()


# Extract the WKT CRS string from the terra raster
rast_crs <- st_crs(crop_LCM_3035)

# Reproject the sf object to the raster's CRS
reproject_cams.sp <- st_transform(cams.sp_3035, crs = rast_crs)

st_crs(reproject_cams.sp) == st_crs(crop_LCM_3035)



############################################################################################################################
#'# 5km buffer 
# Calculate 5km buffer around each camera trap site
buffer_km <- 5 
buffer_m <- buffer_km *10^6 
radius_buffer <- sqrt(buffer_m / pi) # radius in m
radius_buffer

# Sample Landscape Metrics
LCM_5kmbuffer_allsites <- sample_lsm(crop_LCM_3035, reproject_cams.sp, size = radius_buffer, level = "patch", metric = "area")
View(LCM_5kmbuffer_allsites)

# Process Sampled Data
df <- LCM_5kmbuffer_allsites %>%
  group_by(plot_id, class) %>%
  summarise(area = sum(value)) %>%
  spread(class, area)


# Calculate Total Cover per Row
df$total <- rowSums(df[ , !(names(df) %in% c("plot_id"))], na.rm = TRUE)

# Calculate Proportion of Each Class
df[is.na(df)] <- 0
df[ , !(names(df) %in% c("plot_id", "total"))] <- df[ , !(names(df) %in% c("plot_id", "total"))] / df$total

# merge with cam data 
lcm_df <- merge(df, cams, by = "plot_id")


View(lcm_df)
colnames(lcm_df)


##################################################################################
# rename columns as needed 
create renaming matrix 
CLC_matrix <- matrix(c(
  1, 111,
  2, 112,
  3, 121,
  4, 122,
  5, 123,
  6, 124,
  7, 131,
  8, 132,
  9, 133,
  10, 141,
  11, 142,
  12, 211,
  16, 222,
  18, 231,
  20, 242,
  21, 243,
  23, 311,
  24, 312,
  25, 313,
  26, 321,
  27, 322,
  29, 324,
  30, 331,
  31, 332,
  32, 333,
  33, 334,
  35, 411,
  36, 412,
  37, 421,
  39, 423,
  40, 511,
  41, 512,
  42, 521,
  43, 522,
  44, 523
), ncol = 2, byrow = TRUE)

# Rename lcm_df columns according to CLC matrix
rename_map <- setNames(paste0("CLC_", CLC_matrix[, 2]), CLC_matrix[, 1])
colnames(lcm_df) <- ifelse(colnames(lcm_df) %in% names(rename_map), rename_map[colnames(lcm_df)], colnames(lcm_df))
View(lcm_df)

#save 5km buffer data
write.csv(lcm_df, "1.data/1.2.processed/lcm_df_5km_buffer_3035.csv")

##############################################################################################################################
# repeate for 10km buffer 
# Calculate 10km buffer around each camera trap site
buffer_km <- 10
buffer_m <- buffer_km *10^6
radius_buffer <- sqrt(buffer_m / pi) # radius in m
radius_buffer

# Sample Landscape Metrics
LCM_10kmbuffer_allsites <- sample_lsm(crop_LCM_3035, reproject_cams.sp, size = radius_buffer, level = "patch", metric = "area")
View(LCM_10kmbuffer_allsites)

# Process Sampled Data
df <- LCM_10kmbuffer_allsites %>%
  group_by(plot_id, class) %>%
  summarise(area = sum(value)) %>%
  spread(class, area)

colnames(df)
# Calculate Total Cover per Row
df$total <- rowSums(df[ , !(names(df) %in% c("plot_id"))], na.rm = TRUE)

# Calculate Proportion of Each Class
df[is.na(df)] <- 0
df[ , !(names(df) %in% c("plot_id", "total"))] <- df[ , !(names(df) %in% c("plot_id", "total"))] / df$total

# Merge with cam Data
lcm_df <- merge(df, cams, by = "plot_id")

# rename using matrix as before 
colnames(lcm_df) <- ifelse(colnames(lcm_df) %in% names(rename_map), rename_map[colnames(lcm_df)], colnames(lcm_df))

# save 10km buffer data 
write.csv(lcm_df, "1.data/1.2.processed/lcm_df_10km_buffer_3035.csv")

############################################################################################################################
# repeate for 1km buffer 
# Calculate 1km buffer around each camera trap site
buffer_km <- 1
buffer_m <- buffer_km *10^6
radius_buffer <- sqrt(buffer_m / pi) # radius in m
radius_buffer

# Sample Landscape Metrics
LCM_1kmbuffer_allsites <- sample_lsm(crop_LCM_3035, reproject_cams.sp, size = radius_buffer, level = "patch", metric = "area")
View(LCM_1kmbuffer_allsites)

# Process Sampled Data
df <- LCM_1kmbuffer_allsites %>%
  group_by(plot_id, class) %>%
  summarise(area = sum(value)) %>%
  spread(class, area)

colnames(df)
# Calculate Total Cover per Row
df$total <- rowSums(df[ , !(names(df) %in% c("plot_id"))], na.rm = TRUE)

# Calculate Proportion of Each Class
df[is.na(df)] <- 0
df[ , !(names(df) %in% c("plot_id", "total"))] <- df[ , !(names(df) %in% c("plot_id", "total"))] / df$total

# Merge with cam Data
lcm_df <- merge(df, cams, by = "plot_id")

# rename using matrix as before 
colnames(lcm_df) <- ifelse(colnames(lcm_df) %in% names(rename_map), rename_map[colnames(lcm_df)], colnames(lcm_df))

# save 10km buffer data 
write.csv(lcm_df, "1.data/1.2.processed/lcm_df_1km_buffer_3035.csv")

View(lcm_df)
