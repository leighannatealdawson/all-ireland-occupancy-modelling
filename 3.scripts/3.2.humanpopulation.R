#Title: Processing human population data for Ireland (Northern Ireland and Republic of Ireland)
# Purpose: 
# Author: Leighanna Teal Dawson 
# Date: 21 Jan 2025
# 
##### Contence of the script:#####
# 0. Admin (load libraries, set working directory, load data)
# 1.
# 2. 
# 3.  
#

# Data sources 
# Northern Ireland: Central Statistics Office (CSO) https://www.nisra.gov.uk
# grid https://www.nisra.gov.uk/publications/census-2021-grid-square-product-for-northern-ireland
# Republic of Ireland: Central Statistics Office (CSO) https://www.cso.ie
#years required 2015, 2018, 2020 

# irish small aboundies https://data-osi.opendata.arcgis.com/datasets/osi::cso-small-areas-national-statistical-boundaries-2022-ungeneralised/explore?location=53.244900%2C-8.129148%2C11.16



#### 0. Admin (load libraries, set working directory, load data) ####
# clear environment if required
rm(list = ls())

#set working directory
setwd("C:/VScode/all-ireland-occupancy-modelling")

# Check if the directory is set correctly
getwd()

# Load required libraries
library(readxl)
library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(leaflet)
library(sf)
library(igr)
library(stringr)

#### 1. Northern Ireland Population Data ####
# Import data in grid 
# Read the CSV file
# Read the CSV file
humanpop_ni_1kmgrid <- read.csv("1.data/1.1.raw/humanpopulationdata/NorthernIrishCensusGrid/census-2021-grid-square-product-1km-full-dataset.csv")


# Keep only the necessary columns (Geography, HOUSEHOLDS, and PERSONS) as original data is huge 
humanpop_ni_1kmgrid <- humanpop_ni_1kmgrid[, c("Geography", "HOUSEHOLDS", "PERSONS")]

# Rename columns for ease 
colnames(humanpop_ni_1kmgrid) <- c("igr", "households", "persons")
head(humanpop_ni_1kmgrid)
nrow(humanpop_ni_1kmgrid)
# Convert the Irish Grid references  to a Simple Features object
humanpop_ni_1kmgrid_sf <- st_igr_as_sf(humanpop_ni_1kmgrid, "igr", crs = 4326) 
head(humanpop_ni_1kmgrid_sf)
summary(humanpop_ni_1kmgrid_sf$households)

# Check if households and persons column is numeric, and if not, convert it
humanpop_ni_1kmgrid_sf$households <- as.numeric(humanpop_ni_1kmgrid_sf$households)
humanpop_ni_1kmgrid_sf$persons <- as.numeric(humanpop_ni_1kmgrid_sf$persons)

# Create a color palette for household data
household_palette <- colorNumeric(
  palette = "YlOrRd", # Yellow-Orange-Red color palette
  domain = humanpop_ni_1kmgrid_sf$households, 
  na.color = "transparent"
)

# Plot the data on a leaflet map
leaflet(data = humanpop_ni_1kmgrid_sf) %>%
  addTiles() %>% # Add base map tiles
  addCircleMarkers(
    radius = 5, # Circle marker size
    color = ~household_palette(households), # Color based on household count
    stroke = TRUE, # Add border
    fillOpacity = 0.8, # Opacity of fill color
    popup = ~paste0(
      "<b>Grid Reference:</b> ", igr, "<br>",
      "<b>Households:</b> ", households, "<br>",
      "<b>Persons:</b> ", persons
    ) # Add popup details
  ) %>%
  addLegend(
    "bottomright", 
    pal = household_palette, 
    values = ~households, 
    title = "Households",
    opacity = 0.8
  )

head(humanpop_ni_1kmgrid_sf)



class(humanpop_ni_1kmgrid_sf)


# Define the output Shapefile path
shp_path <- "1.data/1.2.processed/processed_humanpop_ni_1kmgrid.shp"
1.data\1.2.processed
# Write the sf object to a Shapefile
st_write(humanpop_ni_1kmgrid_sf, dsn = shp_path, driver = "ESRI Shapefile")



################################################################################################
#### 2. Republic of Ireland Population Data ####
# Import data 
# Import data
humanpop_ie_1kmgrid2011_2016change <- read_excel("1.data/1.1.raw/humanpopulationdata/Censusireland2011/1_Km_dataset_population_change_2011-2016IE.xlsx")
# Import the shapefile
combined_grid_square_2021 <- st_read("1.data/1.1.raw/humanpopulationdata/Grid_Square_2021_shapefiles/Combined_Grid_Square_2021.shp")

# Check the structure of the imported shapefile
str(combined_grid_square_2021)

# View the first few rows of the shapefile
View(combined_grid_square_2021)




humanpop_ie_1kmgrid2011 <- read.csv("1.data/1.1.raw/humanpopulationdata/Censusireland2011/COP2011_Grid_ITM_IE_1Km.csv")
humanpop_ie_1kmgrid2016 <- read_excel("1.data/1.1.raw/humanpopulationdata/Censusireland2011/1_Km_dataset_population_change_2011-2016IE.xlsx")
humanpop_ie_1kmgrid_2022 <- read_excel(("1.data/1.1.raw/humanpopulationdata/IrishCensusGrid/grid1km_population_2022.xlsx"))
# clear df removing unneded rows 
# keep rows OBJECTID, ITM1KM, CENT_EAST, CENT_NORTH, TOTAL_P, POP_2011, POP_2016, and CHANGE_11_16
humanpop_ie_1kmgrid2011 <- humanpop_ie_1kmgrid2011[, c("OBJECTID", "ITM1KM", "CENT_EAST", "CENT_NORTH", "TOT_P")]
################################################################################################
View(humanpop_ie_1kmgrid2011)
colnames(humanpop_ie_1kmgrid2011)
head(humanpop_ie_1kmgrid2016)
head(humanpop_ie_1kmgrid2011)
### not the data doesnt make sense and 2011 gives 2 valvues per 1km sq this way!!!!!!!!!!!!!!!!!!!!!!
# Merge dfs by ITM1km 
humanpop_ie_1kmgrid <- merge(humanpop_ie_1kmgrid2011, humanpop_ie_1kmgrid2016, by = "ITM1KM")

view(humanpop_ie_1kmgrid)

average_difference <- mean(humanpop_ie_1kmgrid$TOT_P - humanpop_ie_1kmgrid$POP_2016, na.rm = TRUE)
print(average_difference)

biggest_difference <- max(abs(humanpop_ie_1kmgrid$TOT_P - humanpop_ie_1kmgrid$POP_2016), na.rm = TRUE)
print(biggest_difference)

# Calculate the difference
humanpop_ie_1kmgrid$difference <- abs(humanpop_ie_1kmgrid$TOT_P - humanpop_ie_1kmgrid$POP_2016)

# Select the row with the biggest difference
row_with_biggest_difference <- humanpop_ie_1kmgrid[which.max(humanpop_ie_1kmgrid$difference), ]

# View the row
print(row_with_biggest_difference)
################################################################################################
#data finding issue so going to test with older data set until fixed 

humanpop_ie_1kmgrid2011
head(humanpop_ie_1kmgrid2011)
# Convert the Irish Grid references  to a Simple Features object

# Create sf object with ITM CRS (EPSG:2157)
humanpop_ie_sf <- st_as_sf(humanpop_ie_1kmgrid2011, coords = c("CENT_EAST", "CENT_NORTH"), crs = 2157) 

# Transform to Irish Grid (EPSG:29903)
humanpop_ie_sf_irishgrid <- st_transform(humanpop_ie_sf, crs = 29903)
#check crs
st_crs(humanpop_ie_sf_irishgrid)

# Add Irish Grid coordinates to the data frame
humanpop_ie_1kmgrid2011$Irish_East <- st_coordinates(humanpop_ie_sf_irishgrid)[, 1]
humanpop_ie_1kmgrid2011$Irish_North <- st_coordinates(humanpop_ie_sf_irishgrid)[, 2]

view(humanpop_ie_1kmgrid2011)

# Assuming you already have 'humanpop_ie_1kmgrid2011' data with CENT_EAST and CENT_NORTH columns
# Convert the ITM coordinates to Irish Grid codes
humanpop_ie_1kmgrid2011$Irish_Grid_Code <- itm_to_irishgrid(humanpop_ie_1kmgrid2011$CENT_EAST, humanpop_ie_1kmgrid2011$CENT_NORTH)

# View the updated data frame with Irish Grid Code
head(humanpop_ie_1kmgrid2011)

################################################################################################
# work on irish census 2022 grid data humanpop_ie_1kmgrid_2022
humanpop_ie_1kmgrid_2022 <- read_excel(("1.data/1.1.raw/humanpopulationdata/IrishCensusGrid/grid1km_population_2022.xlsx"))
# clear df removing unneded rows 
head(humanpop_ie_1kmgrid_2022)
st_crs(humanpop_ie_1kmgrid_2022)

# Extract Northing (N) and Easting (E)
humanpop_ie_1kmgrid_2022_igr <- humanpop_ie_1kmgrid_2022 %>%
  mutate(
    Northing = as.numeric(str_extract(GRD_ID, "N\\d+") %>% str_remove("N")),
    Easting = as.numeric(str_extract(GRD_ID, "E\\d+") %>% str_remove("E")),
    Irish_Grid = paste0("N", Northing, " E", Easting)
  )


# Convert the dataframe to an `sf` object
humanpop_ie_1kmgrid_2022_sf <- st_as_sf(humanpop_ie_1kmgrid_2022_igr, 
                         coords = c("Easting", "Northing"), 
                         crs = 3035)  # Set the current CRS to EPSG:3035



st_crs(humanpop_ie_1kmgrid_2022_sf)
class(humanpop_ie_1kmgrid_2022_sf)
colnames(humanpop_ie_1kmgrid_2022_sf)

# Reproject to Irish Grid (EPSG:29903)
humanpop_ie_1kmgrid_2022_sf <- st_transform(humanpop_ie_1kmgrid_2022_sf, crs = 29903)

# Check the CRS again
st_crs(humanpop_ie_1kmgrid_2022_sf)
head(humanpop_ie_1kmgrid_2022_sf)


###################
# Convert the sf object to a data.frame
humanpop_ie_1kmgrid_2022_df <- as.data.frame(st_drop_geometry(humanpop_ie_1kmgrid_2022_sf))

# Extract Easting and Northing
humanpop_ie_1kmgrid_2022_df$Easting <- st_coordinates(humanpop_ie_1kmgrid_2022_sf)[, 1]
humanpop_ie_1kmgrid_2022_df$Northing <- st_coordinates(humanpop_ie_1kmgrid_2022_sf)[, 2]

# Function to generate Irish Grid reference from Easting and Northing
generate_irish_grid_ref <- function(easting, northing) {
  grid_ref <- paste0("N", floor(northing / 1000), " E", floor(easting / 1000))
  return(grid_ref)
}

# Apply the function to create the IRISH_GRID column
humanpop_ie_1kmgrid_2022_df$IRISH_GRID <- mapply(generate_irish_grid_ref, 
                                                   humanpop_ie_1kmgrid_2022_df$Easting, 
                                                   humanpop_ie_1kmgrid_2022_df$Northing)

# Check the new column
head(humanpop_ie_1kmgrid_2022_df$IRISH_GRID)
view(humanpop_ie_1kmgrid_2022_df)
# Optionally, convert the data.frame back to sf if you need spatial functionality
humanpop_ie_1kmgrid_2022_sf_updated <- st_as_sf(humanpop_ie_1kmgrid_2022_df, 
                                                 coords = c("Easting", "Northing"), 
                                                 crs = 29903)

humanpop_ie_1kmgrid_2022_sf$IGR <- st_irishgridrefs(humanpop_ie_1kmgrid_2022_sf)
head(humanpop_ie_1kmgrid_2022_sf)
view(humanpop_ie_1kmgrid_2022_sf)
################################################################################################
####  WorldPop data ####

worldpop_gbr_2015 <- read.csv("1.data/1.1.raw/humanpopulationdata/wolrdpop/gbr_pd_2015_1km_UNadj_ASCII_XYZ.csv")
worldpop_gbr_2018 <- read.csv("1.data/1.1.raw/humanpopulationdata/wolrdpop/gbr_pd_2018_1km_UNadj_ASCII_XYZ.csv")
worldpop_gbr_2020 <- read.csv("1.data/1.1.raw/humanpopulationdata/wolrdpop/gbr_pd_2020_1km_UNadj_ASCII_XYZ.csv")

worldpop_irl_2015 <- read.csv("1.data/1.1.raw/humanpopulationdata/wolrdpop/irl_pd_2015_1km_UNadj_ASCII_XYZ.csv")
worldpop_irl_2018 <- read.csv("1.data/1.1.raw/humanpopulationdata/wolrdpop/irl_pd_2018_1km_UNadj_ASCII_XYZ.csv")
worldpop_irl_2020 <- read.csv("1.data/1.1.raw/humanpopulationdata/wolrdpop/irl_pd_2020_1km_UNadj_ASCII_XYZ.csv")

head(worldpop_gbr_2015)
head(worldpop_irl_2015)
################################################################################################
#### Irish census data 2022 ####
# Import data
census_ireland_2022_female <- read_excel("1.data/1.1.raw/humanpopulationdata/Censusireland2022/Censusbysmallarea_female.xlsx", sheet = "Unpivoted")
census_ireland_2022_male <- read_excel("1.data/1.1.raw/humanpopulationdata/Censusireland2022/Censusbysmallarea_male.xlsx", sheet = "Unpivoted")

# Filter only rows where Age is "Total"
census_ireland_2022_female_total <- census_ireland_2022_female %>% filter(Age == "Total")
census_ireland_2022_male_total <- census_ireland_2022_male %>% filter(Age == "Total")

# Merge the male and female dfs by "CSO Small Areas 2022"
census_ireland_2022 <- census_ireland_2022_female_total %>%
  inner_join(census_ireland_2022_male_total, by = "CSO Small Areas 2022")

# Convert the values columns to numeric and rename  
census_ireland_2022$populationmale <- as.numeric(census_ireland_2022$VALUE.y)
census_ireland_2022$populationfemale <- as.numeric(census_ireland_2022$VALUE.x)

# Create the population column by adding female and male populations
census_ireland_2022$population <- census_ireland_2022$populationfemale + census_ireland_2022$populationmale

# Check work 
View(census_ireland_2022)

# Keep only cols needed
census_ireland_2022 <- census_ireland_2022[, c("CSO Small Areas 2022", "population")]

head(census_ireland_2022)
################################################################################################
#### Merge by small area boundaries ####
# data source: https://data-osi.opendata.arcgis.com/datasets/osi::cso-small-areas-national-statistical-boundaries-2022-ungeneralised/explore?location=53.244582%2C-8.129148%2C11.16
smallareanboundaries <- read_excel("1.data/1.1.raw/humanpopulationdata/Censusireland2022/Small_Area_National_Statistical_Boundaries_2022_Ungeneralised_view_1521351576885344945.xlsx")

View(smallareanboundaries)


# Create new columns for merging (smallareacode2022)
census_ireland_2022$smallareacode2022 <- census_ireland_2022$`CSO Small Areas 2022`

# Ensure column in smallareanboundaries is correctly named
smallareanboundaries$smallareacode2022 <- smallareanboundaries$`SA_PUB2022`

# Perform the merge using left_join
final_merged_df <- census_ireland_2022 %>%
  left_join(smallareanboundaries, by = "smallareacode2022")

# Optionally, rename the population column if necessary
final_merged_df <- final_merged_df %>%
  rename(SA_PUB2022 = population)

# View the merged data
View(final_merged_df)
