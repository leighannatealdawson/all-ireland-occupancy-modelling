clear working directory 
rm(list = ls(all = TRUE))
 
# data check for sanity in arc
# using shapefile to lcm extraction 
# creates csv of lcm for each 1km grid cell in file called lcm_1km_grid_27_05.csv

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
grid_1km <- st_read("1.data/1.3.processedinarc/1kmgridireland/1kmgridallireland.shp") #mac 
grid_1km <- st_read("1.data/1.3.processedinarc/1kmgridallireland.shp") # laptop 
View(grid_1km)

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

#area of each class for each 1km2 grid cell #THIS TAKES A WHILE TO RUN SO DONT RUN ON LAPTOP 
# grid_1km_lsm <- sample_lsm(ireland_CLC, y = grid_1km, what = "lsm_p_area")

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

#import the postgrouping file
df <- read.csv("1.data/1.2.processed/1km_lcm_ireland_grid22_05_2025_postgrouping.csv")

View(df)

#replace na with 0 
df[is.na(df)] <- 0
# rename corine classes

# Create the renaming matrix with character labels
CLC_matrix <- matrix(c(
  1, "111",
  2, "112",
  3, "121",
  4, "122",
  5, "123",
  6, "124",
  7, "131",
  8, "132",
  9, "133",
  10, "141",
  11, "142",
  12, "211",
  16, "222",
  18, "231",
  20, "242",
  21, "243",
  23, "311",
  24, "312",
  25, "313",
  26, "321",
  27, "322",
  29, "324",
  30, "331",
  31, "332",
  32, "333",
  33, "334",
  35, "411",
  36, "412",
  37, "421", 
  39, "423",
  40, "511",
  41, "512",
  42, "521",
  43, "522",
  44, "523"
), ncol = 2, byrow = TRUE)

#rename cols based on renaming matrix 
# Create map
rename_map <- setNames(paste0("CLC_", CLC_matrix[, 2]), paste0("X", CLC_matrix[, 1]))

# Rename columns
colnames(df) <- ifelse(
  colnames(df) %in% names(rename_map),
  rename_map[colnames(df)],
  colnames(df)
)

colnames(df)

# create a total column 
df$total <- rowSums(df[, !names(df) %in% c("plot_id")], na.rm = TRUE)

head(df)
range(df$total)
#calculate proportions of each class 
for (i in 2:ncol(df)){
  df[,i]<-df[,i]/df[,"total"]
  
}

View(df)


# merge with 1km 
#import 1km grid shapefile 
grid_1km <- st_read("1.data/1.3.processedinarc/1kmgridallireland.shp")

# add plot_id 
grid_1km$plot_id <- 1:nrow(grid_1km)

head(grid_1km)

# make it a df 
grid_1km <- as.data.frame(grid_1km)

class(grid_1km)
# Merge by plot_id and keep gridid
df <- merge(df, grid_1km[, c("plot_id", "gridid")], by = "plot_id", all.x = TRUE)

View(df)

#make into df to save 
df <- as.data.frame(df)

head(df)  # preview the names

check for issues before saving  
# Check for NA values in the data frame
na_count <- sapply(df, function(x) sum(is.na(x)))
na_count



# Save df as lcm_1km_grid_27_05.csv
write.csv(df, "1.data/1.2.processed/lcm_1km_grid_27_05.csv", row.names = FALSE)

# Import the saved lcm_1km_grid_27_05.csv file
lcm_1km_grid_27_05 <- read.csv("1.data/1.2.processed/lcm_1km_grid_27_05.csv")
head(lcm_1km_grid_27_05)

#########################################################################################################
# what happens if we use the proportions for each grid size being the max 

#import the postgrouping file
df <- read.csv("1.data/1.2.processed/1km_lcm_ireland_grid22_05_2025_postgrouping.csv")

View(df)

#replace na with 0 
df[is.na(df)] <- 0
# rename corine classes

# Create the renaming matrix with character labels
CLC_matrix <- matrix(c(
  1, "111",
  2, "112",
  3, "121",
  4, "122",
  5, "123",
  6, "124",
  7, "131",
  8, "132",
  9, "133",
  10, "141",
  11, "142",
  12, "211",
  16, "222",
  18, "231",
  20, "242",
  21, "243",
  23, "311",
  24, "312",
  25, "313",
  26, "321",
  27, "322",
  29, "324",
  30, "331",
  31, "332",
  32, "333",
  33, "334",
  35, "411",
  36, "412",
  37, "421", 
  39, "423",
  40, "511",
  41, "512",
  42, "521",
  43, "522",
  44, "523"
), ncol = 2, byrow = TRUE)

#rename cols based on renaming matrix 
# Create map
rename_map <- setNames(paste0("CLC_", CLC_matrix[, 2]), paste0("X", CLC_matrix[, 1]))


# Rename columns
colnames(df) <- ifelse(
  colnames(df) %in% names(rename_map),
  rename_map[colnames(df)],
  colnames(df)
)

colnames(df)

# create a total column 
df$total <- rowSums(df[, !names(df) %in% c("plot_id")], na.rm = TRUE)

# find max of this col to use 
max_total <- max(df$total, na.rm = TRUE)
max_total
#what is the average of this col 
average_total <- mean(df$total, na.rm = TRUE)
average_total
#this looks about right 

# recalculate proportions for each class based on the max total
for (i in 2:ncol(df)) {
  df[, i] <- df[, i] / max_total
}


recalculate propotions for eeach 

# merge with 1km 
#import 1km grid shapefile 
grid_1km <- st_read("1.data/1.3.processedinarc/1kmgridallireland.shp") # laptop 

# add plot_id 
grid_1km$plot_id <- 1:nrow(grid_1km)

head(grid_1km)

# make it a df 
grid_1km <- as.data.frame(grid_1km)

class(grid_1km)
# Merge by plot_id and keep gridid
df <- merge(df, grid_1km[, c("plot_id", "gridid")], by = "plot_id", all.x = TRUE)

View(df)

#make into df to save 
df <- as.data.frame(df)

head(df)  # preview the names

check for issues before saving  
# Check for NA values in the data frame
na_count <- sapply(df, function(x) sum(is.na(x)))
na_count



# Save df as lcm_1km_grid_27_05.csv
write.csv(df, "1.data/1.2.processed/lcm_1km_grid_27_05_using_total_prop.csv", row.names = FALSE)
