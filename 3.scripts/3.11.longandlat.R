# create new cols of long and lat for each centroid of each 1km square 
# Load necessary libraries
library(sp)
library(rgdal)

# Read in the Irish grid 1km and 10km shapefiles
irish_grid_1km <- readOGR(dsn = "/path/to/shapefiles", layer = "irish_grid_1km")
irish_grid_10km <- readOGR(dsn = "/path/to/shapefiles", layer = "irish_grid_10km")
# Import the 1km Irish grid CSV file
irish_grid1km_csv <- read.csv("1.data/1.2.processed/1.2.1.irish_grid/1kmirishgrid.csv")
irish_grid10km_csv <- read.csv("1.data/1.2.processed/1.2.1.irish_grid/10kmirishgrid.csv")

View(irish_grid1km_csv)
View(irish_grid10km_csv)
# Calculate centroids for each 1km square
centroids_1km <- coordinates(irish_grid_1km)

# Create new columns for longitude and latitude
irish_grid_1km$longitude <- centroids_1km[, 1]
irish_grid_1km$latitude <- centroids_1km[, 2]

# Print the first few rows to verify
head(irish_grid_1km)