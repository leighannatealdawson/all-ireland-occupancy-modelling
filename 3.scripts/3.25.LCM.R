clear working directory 
rm(list = ls(all = TRUE))

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

# lets extract the corine raster data for each of the 1/5/10km grid cells using landscapemetrics

#import raster
ireland_CLC <- raster("1.data/1.1.raw/corine_raster/DATA/U2018_CLC2018_V2020_20u1.tif") #original raster downloaded from copernicus in epsg 3035 
plot(ireland_CLC)
crs(ireland_CLC)

# import 1km grid shapefile
grid_1km <- st_read("1.data/1.3.processedinarc/1kmgridireland/1kmgridallireland.shp")
class(grid_1km)

# check the CRS of the grid
st_crs(grid_1km)
View(grid_1km)
plot(grid_1km)

# transform to same CRS at Ireland raster (see crs(Ireland) above)
grid_1km <- st_transform(grid_1km, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

# check crs match 
st_crs(grid_1km) == st_crs(ireland_CLC)

grid_1km_top10 <- grid_1km %>%
  slice_head(n = 10)
View(grid_1km_top10)

#area of each class for each 1km2 grid cell
grid_1km_lsm <- sample_lsm(ireland_CLC, y = grid_1km, what = "lsm_p_area")

View(grid_1km_lsm)
print("done")

write.csv(grid_1km_lsm, "1.data/1.2.processed/1km_lcm_ireland_grid22_05_2025_pregrouping.csv", row.names = FALSE)

read this infer from the pregrouping file 
grid_1km_lsm <- read.csv("1.data/1.2.processed/1km_lcm_ireland_grid22_05_2025_pregrouping.csv")

#sum the different classes
df <- grid_1km_lsm %>%
  group_by(plot_id, class) %>%
  summarise(area = sum(value)) %>%
  spread(class, area)

head(df)




write.csv(df, "1.data/1.2.processed/1km_lcm_ireland_grid22_05_2025_postgrouping.csv", row.names = FALSE)



#################################################################################################
#################################################################################################
# rename corine classes 







# plot to verify alignment
plot(ireland_CLC)
plot(grid_1km, add = TRUE, border = "blue", lwd = 0.5)
# ... existing code ...
#keep only the columns we need
irishgrid_1km <- irishgrid_1km[, c("gridid", "x", "y")]

# Convert to spatial object
irishgrid_1km_sf <- st_as_sf(irishgrid_1km, coords = c("x", "y"), crs = 4326)

# plot using raster to project the grid points
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%  # clean basemap
  addCircleMarkers(data = irishgrid_1km_sf,
                   radius = 3,
                   stroke = FALSE,
                   fillOpacity = 0.7,
                   color = "blue") %>%
  addScaleBar(position = "bottomleft") %>%
  addMiniMap(toggleDisplay = TRUE) %>%
  setView(lng = mean(irishgrid_1km$x), lat = mean(irishgrid_1km$y), zoom = 7)

# reproject to match raster CRS 
# check raster crs 
crs(ireland_CLC)
# transform to same CRS at Ireland raster (see crs(Ireland) above)
irishgrid_1km_sf <- st_transform(irishgrid_1km_sf, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")


# check they now match 
plot(ireland_CLC)
points(irishgrid_1km_sf) # THEY DONT!! :(

# check crs of both match 
st_crs(ireland_CLC) == st_crs(irishgrid_1km_sf)

# check input data 
check_landscape(ireland_CLC)
colnames(irishgrid_1km_sf)

# sample raster for data for each grid cell 
#all_ireland_lcm_1km <- sample_lsm(ireland_CLC,
                                 y = irishgrid_1km_sf,
                                 size = 2500, #half of the side-length for squares in map units
                                 level = "patch",
                                 metric= "area",  
                                 shape = "square", 
                                 progress = TRUE  )

View(all_ireland_lcm_1km)
str(all_ireland_lcm_1km) 

#### save because it takes a while to run
#turn into df 
all_ireland_lcm_1km <- as.data.frame(all_ireland_lcm_1km)

write.csv(all_ireland_lcm_1km, "1.data/1.2.processed/1km_lcm_ireland.csv", row.names = FALSE)

all_ireland_lcm_1km <- read.csv("1.data/1.2.processed/1km_lcm_ireland.csv")



df <- all_ireland_lcm_1km %>%
  group_by(plot_id, class) %>%
  summarise(area = sum(value), .groups = "drop")     %>%
  pivot_wider(names_from = 'class', values_from = area, values_fill = 0)

View(df)

#rename corine classes 
#create renaming matrix 
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

View(df)

#ensure using correct df 
# Rename all_ireland_lcm_1km$class according to CLC matrix and add "CLC_" prefix
rename_map <- setNames(paste0("CLC_", CLC_matrix[, 2]), CLC_matrix[, 1])
df$class <- as.character(df$class)
all_ireland_lcm_1km$class <- ifelse(df$class %in% names(rename_map),
    rename_map[df$class],
    df$class)

# Rename lcm_df columns according to CLC matrix
rename_map <- setNames(paste0("CLC_", CLC_matrix[, 2]), CLC_matrix[, 1])
colnames(grid_5km) <- ifelse(colnames(grid_5km) %in% names(rename_map), rename_map[colnames(grid_5km)], colnames(grid_5km))
View(grid_5km)


# calculate proportion of each class in each grid cell 


























# issue is above 
  colnames(all_ireland_lcm_1km)

###############################################################################################
# Plot to verify alignment
plot(ireland)
plot(irishgrid_1km_sf, add = TRUE, col = "blue", pch = 20)

library(dplyr)




   









# import 5km grid but in points 
grid_5km_points <- st_read("1.data/1.3.processedinarc/DD5kmgridallirelandpoints.shp")


grid_5km_points <- st_transform(grid_5km_points,
  crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
)

# Compare them
st_crs(grid_5km_points) == st_crs(LCM_3035)

# Plot the raster and the 5km grid points
plot(ireland)
plot(grid_5km_points, add = TRUE, col = "red", pch = 20)

# add plot id 
nrow(grid_5km_points)
grid_5km_points$plot_id <- c(1:3981)


allireland_lcm_5km <- sample_lsm(ireland,y= grid_5km_points,  size = 2.5, level = "patch",
        metric= "area",  shape = "square", progress = TRUE  )
print("done")

View(allireland_lcm_5km)

# sum the different classes for each grid cell

df<- allireland_lcm_5km %>%
  group_by(plot_id)  %>%
  summarise(area = sum(value)) %>%
  spread(class, area) # area not correcT?? 


# calculate the percentage of each class in each grid cell
df$total <- rowSums(df[,!names(df)    
], na.rm = TRUE)

?landscapemetrics::sample_lsm


###########################################################################################################################
# read in 10km grid 
grid_5km <- st_read("1.data/1.3.processedinarc/5kmgridallireland.shp")
tenkm_grid<- st_read(dsn = ".", layer = "5kmgridallireland")
grid_5km <- st_read("C:/Users/B00997413/OneDrive - Ulster University/Documents/ArcGIS/Projects/gridtest2-4/gridtest2-4.gdb", layer = "x5kmgridgeopack")
#save to processin arc folder 
st_write(grid_5km, "1.data/1.3.processedinarc/5kmgridallireland_processed.gdb", delete_layer = TRUE)


plot(grid_5km)
st_layers("1.data/1.3.processedinarc/5kmgridallireland.shp")
st_crs(ireland)
st_crs(grid_5km)
# reproject grid to match raster
grid_5km <- st_transform(grid_5km, crs = st_crs(ireland))
plot(ireland)
plot(grid_5km, add=TRUE, border = "blue")

# add plot ids to each grid
nrow(grid_5km)
grid_5km$plot_id <- c(1:3804)
head(grid_5km)


unique(ireland)
##########################################################################################################
#rename corine classes 
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
colnames(grid_5km) <- ifelse(colnames(grid_5km) %in% names(rename_map), rename_map[colnames(grid_5km)], colnames(grid_5km))
View(grid_5km)

##########################################################################################################

corrine_5km_grid_ireland <- sample_lsm(ireland, grid_5km, level = "patch", metric= "area")
View(corrine_5km_grid_ireland)

View(grid_5km)

?landscapemetrics::sample_lsm
##### try from points and extract for a square for 5km 

# import 1km grid shapefile
grid_1km <- st_read("1.data/1.3.processedinarc/1kmgridireland/1kmgridallireland.shp")

# check the CRS of the grid
st_crs(grid_1km)

# reproject to match the CORINE raster CRS if needed
grid_1km <- st_transform(grid_1km, crs = st_crs(ireland_CLC))

# add plot IDs to each grid cell
grid_1km$plot_id <- 1:nrow(grid_1km)

# plot to verify alignment
plot(ireland_CLC)
plot(grid_1km, add = TRUE, border = "blue", lwd = 0.5)