import geopandas as gpd
import os

# Define the path to the shapefile
shapefile_path = os.path.join('1.data', '1.3.processedinarc', '1km_grid_all_ireland.shp')

# Read the shapefile
grid = gpd.read_file(shapefile_path)

# Print basic information about the dataset
print("Dataset Info:")
print(f"Number of features: {len(grid)}")
print("\nColumns:")
print(grid.columns.tolist())
print("\nFirst few rows:")
print(grid.head())

# Print the coordinate reference system
print("\nCoordinate Reference System:")
print(grid.crs) 