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

Larger_IrelandLCM <- raster("1.data/1.3.processedinarc/CLC_Raster_10m_ITM/Raster_10m_ITM.tif") #this is in ITM 
crs(IrelandLCM)
print(IrelandLCM)
plot(IrelandLCM)
plot(Larger_IrelandLCM)
crs(Larger_IrelandLCM) # this is in ITM
# ;ets use the higherr resolution raster for the analysis 
 IrelandLCM <- Larger_IrelandLCM 
## Load site data
cams <- read.csv("1.data/1.2.processed/paandhabitat.csv")
plot(cams$long, cams$lat, pch = 20, cex = 1.5, col = "black", xlab = "Longitude", ylab = "Latitude")
####################################################################################
# plot overlapping to check projections 


# Convert Site Data to Spatial Points
cams.sp <- st_as_sf(cams, coords = c("long", "lat"), crs = 4326) # dont worry change to ITM later 

# Convert raster to dataframe
#IrelandLCM_df <- as.data.frame(IrelandLCM, xy = TRUE, na.rm = TRUE)
#class(IrelandLCM_df)
cams.sp_df <- as.data.frame(cams.sp)

# plot NOTE: when plotting like this points may not voerlap with raster due to mixing raster and sf?? 
#plot(IrelandLCM)
#plot(cams.sp, add=TRUE, col = "#000000", pch = 20, cex = 1.5)

# reproject to 2157 to match raster later as itll need to be in m not degrees 
cams.sp <- st_transform(cams.sp, crs = 2157)


plot(IrelandLCM)
plot(cams.sp, add=TRUE, col = "#000000", pch = 20, cex = 1.5)
# Rename the layer column as tif layer doesnt keep correct col names 
#colnames(IrelandLCM_df) <- c("landcover", "x", "y")
#head(IrelandLCM_df)

# plot both layers as csv using ggplot2 to check overlap as expected 
#ggplot() +
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

# looks good 
#######################################################################
# Plot Reprojected Raster and Points to double check projection
## Downsample Raster for Testing
IrelandLCM_small <- aggregate(IrelandLCM, fact = 10)

## Plot with Leaflet
leaflet() %>%
  addTiles() %>%
  addRasterImage(IrelandLCM_small, colors = terrain.colors(100), opacity = 0.6) %>%
  addCircleMarkers(data = cams.sp, color = "red", radius = 3, label = ~as.character(Site)) %>%
  setView(lng = mean(cams$long), lat = mean(cams$lat), zoom = 7)
# its as expected 

#################################################################################################################

####################################################################################################
# Sample Landscape Metrics
LCM_5kmbuffer_allsites <- sample_lsm(IrelandLCM, cams.sp, size = 1261.57, level = "patch", metric = "area")
View(LCM_5kmbuffer_allsites)

# Process Sampled Data
df <- LCM_5kmbuffer_allsites %>%
  group_by(plot_id, class) %>%
  summarise(area = sum(value)) %>%
  spread(class, area)
View(df)


# Calculate Total Cover per Row
df$total <- rowSums(df[ , !(names(df) %in% c("plot_id"))], na.rm = TRUE)

# Calculate Proportion of Each Class
df[is.na(df)] <- 0
df[ , !(names(df) %in% c("plot_id", "total"))] <- df[ , !(names(df) %in% c("plot_id", "total"))] / df$total


# Merge with Site Data
cams <- rename(cams, plot_id = observation_id)
lcm_df <- merge(df, cams, by = "plot_id")
 View(lcm_df)
colnames(lcm_df)

# rename columns to CLC Code 
# Update column names using CLC
colnames(lcm_df)[colnames(lcm_df) %in% c("0", "1", "2", "3", "6", "7", "8", "9", 
                                          "10", "11", "12", "13", "14", "15", "16", "17", "18", 
                                          "19", "20", "21", "22", "24", "25", "27", 
                                          "28", "30", "31", "32", "34")] <- c(
  "111", "112", "121", "122", "131", "132", "133", "141", 
  "142", "211", "222", "231", "242", "243", "311", "312", "313", "321", 
  "322", "324", "331", "333", "334", "412", "421", 
  "511", "512", "521", "523"
)

View(lcm_df)

# Save Data
write.csv(lcm_df, "1.data/1.3.processedinarc/lcm_5kmbuffer_allsites_largertif.csv", row.names = FALSE)


####################################################################################################
# Sample Landscape Metrics
LCM_10kmbuffer_allsites <- sample_lsm(IrelandLCM, cams.sp, size = 1261.57, level = "patch", metric = "area")
View(LCM_10kmbuffer_allsites)

# Process Sampled Data
df <- LCM_10kmbuffer_allsites %>%
  group_by(plot_id, class) %>%
  summarise(area = sum(value)) %>%
  spread(class, area)
View(df)


# Calculate Total Cover per Row
df$total <- rowSums(df[ , !(names(df) %in% c("plot_id"))], na.rm = TRUE)

# Calculate Proportion of Each Class
df[is.na(df)] <- 0
df[ , !(names(df) %in% c("plot_id", "total"))] <- df[ , !(names(df) %in% c("plot_id", "total"))] / df$total


# Merge with Site Data
cams <- rename(cams, plot_id = observation_id)
lcm_df <- merge(df, cams, by = "plot_id")
 View(lcm_df)
colnames(lcm_df)

# rename columns to CLC Code 
# Update column names using CLC
colnames(lcm_df)[colnames(lcm_df) %in% c("0", "1", "2", "3", "6", "7", "8", "9", 
                                          "10", "11", "12", "13", "14", "15", "16", "17", "18", 
                                          "19", "20", "21", "22", "24", "25", "27", 
                                          "28", "30", "31", "32", "34")] <- c(
  "111", "112", "121", "122", "131", "132", "133", "141", 
  "142", "211", "222", "231", "242", "243", "311", "312", "313", "321", 
  "322", "324", "331", "333", "334", "412", "421", 
  "511", "512", "521", "523"
)

View(lcm_df)

# Save Data
write.csv(lcm_df, "1.data/1.3.processedinarc/lcm_10kmbuffer_allsites_largertif.csv", row.names = FALSE)



