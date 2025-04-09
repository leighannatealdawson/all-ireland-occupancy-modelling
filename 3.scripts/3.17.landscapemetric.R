
# finally using landscape metrics! 
library(raster)
library(landscapemetrics)
#library(rgdal) #package retired 
#library(maptools) #package retired 
library(sp)
#library(rgeos) #package retired 
library(plyr) 
library(purrr)
library(tidyr)
library(dplyr)

#other packages i might be able to use?? 
library(sf)
library(terra)
library(stars)

library(raster)
library(ggplot2)
library(ggspatial)

# Import the CORINE land cover data in GeoTIFF format
IrelandLCM <- raster("1.data/1.1.raw/U2018_CLC2018_V2020_20u1_all_Ireland.tif")
crs(IrelandLCM)
IrelandLCM2 <- raster("1.data/1.2.processed/allirelandlcm.tif") 
# Laod site data
cams <- read.csv("1.data/1.2.processed/paandhabitat.csv")
head(cams)
###########################################################################
PLOT 
PLOT
# Load required packages
library(raster)
library(sp)
library(leaflet)

# Load CORINE raster
IrelandLCM <- raster("1.data/1.1.raw/U2018_CLC2018_V2020_20u1_all_Ireland.tif")

# Check and assign CRS if not correct
crs(IrelandLCM)  


# Load camera trap points
cams <- read.csv("1.data/1.2.processed/paandhabitat.csv")

# Convert to spatial points
cams.sp <- cams
coordinates(cams.sp) <- ~long+lat
proj4string(cams.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")




# Load raster (already in WGS 84)
IrelandLCM <- raster("1.data/1.1.raw/U2018_CLC2018_V2020_20u1_all_Ireland.tif")

# Load camera points
cams <- read.csv("1.data/1.2.processed/paandhabitat.csv")
cams.sp <- cams
coordinates(cams.sp) <- ~long+lat
proj4string(cams.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")


# check my tif for correct requirements  
check_landscape(IrelandLCM)

# Get unique values from the raster
unique_values <- unique(values(IrelandLCM))
# Sort the unique values in ascending order
sorted_unique_values <- sort(unique_values)
# Print the sorted unique values
print(sorted_unique_values)
# ensure CLC classes are integers (1, 2, 3, ...) 
IrelandLCM <- as.integer(IrelandLCM)

# Landscape metrics require:
# 1. projection in metres 
# 2. Raster encodes landscape sclasses as intergers (e.g., 1, 2, 3, ...)
# 3. Landscape metrics describe categorical landscapes, that means that your landscape needs to be classified 
## Warning if over 30 classes (i have 44? )

#change crs to metres (ITM75  )
IrelandLCMepsg2157 <- projectRaster(IrelandLCM, crs = CRS("+init=epsg:2157")) # ITM
IrelandLCMepsg2157 <- as.integer(IrelandLCMepsg2157) # Ensure integer values
view class in raster 
# Get unique classes in the raster
unique_classes <- sort(unique(values(IrelandLCM_reclassed)))
print(unique_classes)
count<- length(unique_classes) # Count the number of unique classes
print(count)
rename the classes now as interger 
library(raster)

# Define the mapping as a matrix: from → to
CLC_legend <- matrix(c(
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
  13, 212,
  14, 213,
  15, 221,
  16, 222,
  17, 223,
  18, 231,
  19, 241,
  20, 242,
  21, 243,
  22, 244,
  23, 311,
  24, 312,
  25, 313,
  26, 321,
  27, 322,
  28, 323,
  29, 324,
  30, 331,
  31, 332,
  32, 333,
  33, 334,
  34, 335,
  35, 411,
  36, 412,
  37, 421,
  38, 422,
  39, 423,
  40, 511,
  41, 512,
  42, 521,
  43, 522,
  44, 523
), byrow = TRUE, ncol = 2)


# Reclassify raster
IrelandLCM_reclassed <- reclassify(IrelandLCMepsg2157, CLC_legend)


check_landscape(IrelandLCM_reclassed)

# Now plot the to check reprojection

# Downsample raster to reduce resolution for testing only 
IrelandLCM_small <- aggregate(IrelandLCMepsg2157, fact = 10)  # Increase 'fact' if still too big

leaflet() %>%
  addTiles() %>%
  addRasterImage(IrelandLCM_small, colors = terrain.colors(100), opacity = 0.6) %>%
  addCircleMarkers(data = cams.sp, color = "red", radius = 3, label = ~as.character(Site)) %>%
  setView(lng = mean(cams$long), lat = mean(cams$lat), zoom = 7)

crs(IrelandLCM_reclassed) # Check the CRS of the raster
crs(cams.sp) # Check the CRS of the points
# Reproject the points to match the raster CRS (ITM)
cams.sp <- spTransform(cams.sp, CRS(proj4string(IrelandLCM_reclassed)))

# Verify the CRS after transformation
crs(cams.sp)

LCM_5kmbuffer_allsites <- sample_lsm(IrelandLCM_reclassed, cams.sp, size = 150, level = "patch", metric = "area")
print("done")
View(LCM_5kmbuffer_allsites)

df <- LCM_5kmbuffer_allsites %>%
  group_by(plot_id, class) %>%              # group by point (ID) and lc class               # count the number of occurences of each class
  summarise(area = sum(value))  %>% 
  spread(class, area)
View(df)
# calcuate total for propotion 
# First, calculate total cover per row
df$total <- rowSums(df[ , !(names(df) %in% c("plot_id"))], na.rm = TRUE)
View(df)
## find proportion of each class 
df[is.na(df)] <- 0
df[ , !(names(df) %in% c("plot_id", "total"))] <- 
  df[ , !(names(df) %in% c("plot_id", "total"))] / df$total
df[,"112"]<-df[,"112"]/df[,"total"]

#merge with cam data
cams <- rename(cams,  plot_id = observation_id)
lcm_df <- merge(df, cams, by = "plot_id")
View(lcm_df)

range(df$"311", na.rm = TRUE)

range(df$"312", na.rm = TRUE)
hist(df$"312")


#####################################################################################




View(df)
ncol(df)
colnames(df)
colnames(df)[colnames(df) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
 "16", "18", "20", "21", "23", "24", "25", "26", "27", "29", "30", "31", "32", "33", "35", 
 "36", "37", "39", "40", "41", "42", "43", "44")] <- c("111", "112", "121", "122", "123", "124",
  "131", "132", "133", "141", "142", "211", "222", "231", "242", "243", "311", "312", "313", 
  "321", "322", "324", "331", "332", "333", "334", "411", "412", "421", "423", "511", "512", "521", "522", "523")
colnames(df)
#create total sum column
df$total <- rowSums(df[,c("111", "112", "121", "122", "123", "124",
  "131", "132", "133", "141", "142", "211", "222", "231", "242", "243", "311", "312", "313", 
  "321", "322", "324", "331", "332", "333", "334", "411", "412", "421", "423", "511", "512", "521", "522", "523")], na.rm = TRUE)


# Check the result
head(land_cover_data)




##################################################################################################
# Downsample raster to reduce resolution for testing only 
IrelandLCM_small <- aggregate(IrelandLCM, fact = 10)  # Increase 'fact' if still too big

# Now plot the smaller version
leaflet() %>%
  addTiles() %>%
  addRasterImage(IrelandLCM_small, colors = terrain.colors(100), opacity = 0.6) %>%
  addCircleMarkers(data = cams.sp, color = "red", radius = 3, label = ~as.character(Site)) %>%
  setView(lng = mean(cams$long), lat = mean(cams$lat), zoom = 7)

check_landscape(IrelandLCM)
LCM_5kmbuffer_allsites <- sample_lsm(IrelandLCM_small, cams.sp, size = 1, level = "patch", metric = "area")
print("done")
View(LCM_5kmbuffer_allsites)

df <- LCM_5kmbuffer_allsites %>%
  group_by(plot_id, class) %>%              # group by point (ID) and lc class               # count the number of occurences of each class
  summarise(area = sum(value))  %>% 
  spread(class, area)






df <- LCM_5kmbuffer_allsites %>%
  group_by(plot_id, class) %>%              # group by point (ID) and lc class               # count the number of occurences of each class
  summarise(area = sum(value))  %>% 
  spread(class, area)

library(raster)

# Load the raster
IrelandLCM <- raster("1.data/1.1.raw/U2018_CLC2018_V2020_20u1_all_Ireland.tif")

library(dplyr)
library(tidyr)

df <- LCM_5kmbuffer_allsites %>%
  group_by(plot_id, class) %>%                # Group by site and land cover class
  summarise(area = sum(value, na.rm = TRUE),  # Sum area (e.g., in pixels or ha)
            .groups = "drop") %>%
  pivot_wider(names_from = class,            # Make one column per land cover class
              values_from = area,
              values_fill = 0)               # Fill missing class values with 0







# Plot raster
plot(IrelandLCM_3035, main = "Camera Trap Points on Land Cover Map", col = terrain.colors(100))

# Overlay camera points
plot(cams.sp, add = TRUE, col = "red", pch = 20)

# Load raster (example placeholder, replace with your own file if needed)
# IrelandLCM <- raster("path_to_your_raster.tif")

# Reproject raster to WGS84 (EPSG:4326) if needed
if (is.na(crs(IrelandLCM)) || crs(IrelandLCM) != CRS("+proj=longlat +datum=WGS84 +no_defs")) {
  IrelandLCM <- projectRaster(IrelandLCM, crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))
}
# Define WGS84 CRS
wgs84_crs <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Reproject only if CRS is different
if (!identical(crs(IrelandLCM), wgs84_crs)) {
  IrelandLCM <- projectRaster(IrelandLCM, crs = wgs84_crs)
}


# Create SpatialPointsDataFrame from cams and assign CRS
cams.sp <- cams
coordinates(cams.sp) <- ~long + lat
crs(cams.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Plot raster
plot(IrelandLCM, main = "Camera Trap Points on Land Cover Map", col = terrain.colors(100))

# Overlay camera points
plot(cams.sp, add = TRUE, col = "red", pch = 20)

############################################################################

IrelandLCM <- raster("1.data/1.1.raw/U2018_CLC2018_V2020_20u1_all_Ireland.tif")

crs(IrelandLCM)<- "+init=epsg:3035"

# Check CRS of raster
crs(IrelandLCM)

# Check CRS of points
crs(cams.sp)

# Reassign CRS to the points explicitly (just to ensure proper alignment)
crs(cams.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Plot raster
plot(IrelandLCM, main = "Camera Trap Points on Land Cover Map", col = terrain.colors(100))

# Overlay camera points
plot(cams.sp, add = TRUE, col = "red", pch = 20)


# Create spatial points data frame
cams.sp <- cams
coordinates(cams.sp) <- ~long+lat


plot(IrelandLCM)
plot(cams.sp, add=TRUE)


# check CRS 
crs(IrelandLCM)

# Reproject using raster package
IrelandLCM_reprojected <- projectRaster(IrelandLCM, crs = 4326)

crs(IrelandLCM_reprojected)

# Convert to sf object and get CRS
cams_sf <- st_as_sf(cams, coords = c("long", "lat"), crs = 4326)
crs(cams_sf)

crs(camos.sp <- CRS("+proj=longlat +datum=WGS84 +no_defs"))




# Plot the reprojected raster
plot(IrelandLCM, main = "Reprojected Raster with Camera Locations")

# Add the cams points
plot(cams.sp, add = TRUE)







# Plot raster and spatial points
plot(IrelandLCM)
plot(cams.sp, add=TRUE, col="red", pch=19, cex=0.5)



head(cams)

cams.sp <- cams

coordinates(cams.dem) <- ~long+lat


plot(IrelandLCM)
plot(cams.dem,add=TRUE, col="red", pch=19, cex=0.5)



df <- LCM_5kmbuffer_allsites %>%
  group_by(plot_id, class) %>%              # group by point (ID) and lc class               # count the number of occurences of each class
  summarise(area = sum(value))  %>% 
  spread(class, area)